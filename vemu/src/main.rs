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
use winit::event::ElementState;
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
    let code = assemble_snippet(include_str!("typewriter.asm").lines()).expect("Assemble error");
    println!("ROM size: {} bytes", code.len());
    cpu.poke_slice(0x400.into(), code.as_slice());
    cpu.start();
    window_loop(event_loop, window, pixels, cpu)
}

fn window_loop(event_loop: EventLoop<()>, window: Window, mut pixels: Pixels, mut cpu: CPU) -> ! {
    let mut interrupt_events: VecDeque<(usize, Option<Word>)> = VecDeque::new();

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

            Event::WindowEvent {
                event: WindowEvent::KeyboardInput { input, .. },
                window_id,
            } if window_id == window.id() => {
                if let (Some(vk), state) = (input.virtual_keycode, input.state) {
                    let byte = convert_keycode(vk);
                    let word = Word::from_bytes([
                        byte,
                        if state == ElementState::Pressed { 1 } else { 0 },
                        0,
                    ]);
                    interrupt_events.push_back((5, Some(word)))
                }
            }

            Event::MainEventsCleared => {
                let start = Instant::now();
                draw(pixels.get_frame(), &mut cpu);
                pixels.render().expect("Problem displaying framebuffer");
                loop {
                    if let Some((int, arg)) = interrupt_events.pop_front() {
                        cpu.interrupt(int, arg)
                    }
                    for _ in 1..1000 {
                        cpu.tick()
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
