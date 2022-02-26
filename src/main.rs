mod bus;
mod cpu;
mod display;
mod memory;
mod opcodes;
mod word;

use winit::{
    dpi::LogicalSize,
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};

use crate::cpu::CPU;
use crate::memory::{Memory, PeekPoke};
use crate::word::Word;
use pixels::{Pixels, SurfaceTexture};
use std::time::{Instant, Duration};
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
        let surface_texture = SurfaceTexture::new(640, 480, &window);
        Pixels::new(640, 480, surface_texture).unwrap()
    };

    let rng = rand::thread_rng();

    let memory = Memory::from(rng);
    let mut cpu = CPU::new(memory);
    display::reset(&mut cpu);
    // for n in 0..128 {
    //     cpu.poke(Word::from(0x10000 + 128 * n), 0b11100000);
    //     cpu.poke(Word::from(0x10000 + n), 0xff);
    //     cpu.poke(Word::from(0x10000 + 128 * n + 127), 0b00000011);
    //     cpu.poke(Word::from(0x10000 + 128 * 127 + n), 0b00011100);
    // }
    cpu.poke_slice(Word::from(0x400), include_bytes!("gfx_test.rom"));
    cpu.start();
    window_loop(event_loop, window, pixels, cpu)
}

fn window_loop(event_loop: EventLoop<()>, window: Window, mut pixels: Pixels, mut cpu: CPU) -> ! {
    event_loop.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Poll;

        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                window_id,
            } if window_id == window.id() => *control_flow = ControlFlow::Exit,

            Event::WindowEvent {
                event: WindowEvent::Resized(newSize),
                window_id
            } if window_id == window.id() => {
                pixels.resize_surface(newSize.width, newSize.height);
            }

            Event::MainEventsCleared => {
                let start = Instant::now();
                draw(pixels.get_frame(), &mut cpu);
                pixels.render().expect("Problem displaying framebuffer");
                while Instant::now() < start + Duration::from_millis(25) {
                    for _ in 1..1000 {
                        cpu.tick()
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
