mod display;
mod keyboard;

use winit::{
    dpi::LogicalSize,
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};

use crate::keyboard::convert_keycode;
use pixels::{Pixels, SurfaceTexture};
use std::collections::VecDeque;
use std::time::{Duration, Instant};
use vasm::assemble_snippet;
use vcore::cpu::CPU;
use vcore::memory::{Memory, PeekPoke};
use vcore::word::Word;
use winit::event::{DeviceEvent, ElementState};
use winit::window::Window;

fn main() {
    let event_loop = EventLoop::new();

    let window = {
        let size = LogicalSize::new(640, 480);
        WindowBuilder::new()
            .with_title("Vulcan")
            .with_inner_size(size)
            .with_min_inner_size(size)
            .build(&event_loop)
            .unwrap()
    };

    let pixels = {
        let winit::dpi::PhysicalSize { width, height } = window.inner_size();
        let surface_texture = SurfaceTexture::new(width, height, &window);
        Pixels::new(640, 480, surface_texture).unwrap()
    };

    let rng = rand::thread_rng();

    let memory = Memory::from(rng);
    let mut cpu = CPU::new(memory);
    display::reset(&mut cpu);
    let code = assemble_snippet(include_str!("typewriter.asm").lines().map(String::from)).expect("Assemble error");
    println!("ROM size: {} bytes", code.len());
    cpu.poke_slice(0x400.into(), code.as_slice());
    cpu.start();
    window_loop(event_loop, window, pixels, cpu)
}

fn window_loop(event_loop: EventLoop<()>, window: Window, mut pixels: Pixels, mut cpu: CPU) -> ! {
    let mut interrupt_events: VecDeque<(usize, Option<Word>)> = VecDeque::new();
    let mut focused = true;

    event_loop.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Poll;

        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                window_id,
            } if window_id == window.id() => *control_flow = ControlFlow::Exit,

            Event::WindowEvent {
                event: WindowEvent::Resized(new_size),
                window_id,
            } if window_id == window.id() => {
                pixels.resize_surface(new_size.width, new_size.height);
            }

            // These are sent regardless of whether the window is focused or not, and are the same regardless of keyboard
            // layout, so, we have to separately track focus and this will only work as expected for normal US Sholes
            // keyboards.
            // If we use the WindowEvent equivalent of this, however, shifted non-letter characters will send no vkeys
            // at all on Linux. Which is actually worse: this way, someone with a "normal" keyboard can use the app on
            // any OS, and there are probably more Linux users than Dvorak users.
            // Also, this won't actually affect me, even on Linux, because the KMAC handles the dvorak mapping in the
            // keyboard itself, rather than an input map; it generates scancodes as though it were a Sholes board.
            // See this bug: https://github.com/rust-windowing/winit/issues/1443
            Event::DeviceEvent {
                event: DeviceEvent::Key(input),
                device_id: _device_id,
            } => {
                if let (Some(vk), state, true) = (input.virtual_keycode, input.state, focused) {
                    let byte = convert_keycode(vk);
                    let word = Word::from_bytes([
                        byte,
                        if state == ElementState::Pressed { 1 } else { 0 },
                        0,
                    ]);
                    interrupt_events.push_back((5, Some(word)))
                }
            }

            Event::WindowEvent {
                event: WindowEvent::Focused(new_focus),
                window_id,
            } if window_id == window.id() => focused = new_focus,

            Event::MainEventsCleared => {
                let start = Instant::now();
                draw(pixels.get_frame(), &mut cpu);
                pixels.render().expect("Problem displaying framebuffer");
                loop { // TODO: this will run the CPU at 100%, need to not spin while halted
                    if let Some((int, arg)) = interrupt_events.pop_front() {
                        cpu.interrupt(int, arg)
                    }
                    if cpu.running() {
                        for _ in 1..1000 {
                            cpu.tick()
                        }
                    }
                    if Instant::now() > start + Duration::from_millis(25) {
                        break;
                    }
                }
            }
            _ => {}
        }
    })
}

fn draw(frame: &mut [u8], cpu: &mut CPU) {
    assert_eq!(frame.len(), 640 * 480 * 4);

    display::draw(cpu, frame);
}
