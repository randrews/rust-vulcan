mod keyboard;

use winit::{
    dpi::LogicalSize,
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
};

use crate::keyboard::convert_keycode;
use pixels::{Pixels, SurfaceTexture};
use std::collections::VecDeque;
use std::sync::Arc;
use std::time::{Duration, Instant};
use winit::application::ApplicationHandler;
use vasm_core::assemble_snippet;
use vcore::cpu::CPU;
use vcore::memory::{Memory, PeekPoke};
use vcore::word::Word;
use winit::event::{DeviceEvent, DeviceId, ElementState, StartCause};
use winit::event_loop::ActiveEventLoop;
use winit::keyboard::PhysicalKey;
use winit::window::{Window, WindowId};
use vgfx::display;

struct App<'a> {
    cpu: CPU,
    pixels: Option<Pixels<'a>>,
    window: Option<Arc<Window>>,
    interrupt_events: VecDeque<(usize, Option<Word>)>,
    focused: bool
}

impl<'a> App<'a> {
    fn new() -> Self {
        let rng = rand::thread_rng();
        let memory = Memory::from(rng);

        Self {
            cpu: CPU::new(memory),
            pixels: None,
            window: None,
            interrupt_events: VecDeque::new(),
            focused: false
        }
    }
}
impl<'a> ApplicationHandler for App<'a> {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let size = LogicalSize::new(640, 480);

        let attrs = Window::default_attributes()
            .with_title("Vulcan")
            .with_inner_size(size)
            .with_min_inner_size(size);

        self.window = Some(event_loop.create_window(attrs).expect("Failed to create window").into());

        display::init_gfx(&mut self.cpu);

        let code = assemble_snippet(include_str!("typewriter.asm").lines().map(String::from))
            .expect("Assemble error");

        // let code = assemble_file("init.asm").expect("Assemble error");
        // let mut file = File::open("4th.rom").expect("ROM not found");
        // let mut code = Vec::new();
        // file.read_to_end(&mut code).expect("Couldn't read ROM");
        println!("ROM size: {} bytes", code.len());
        self.cpu.poke_slice(0x400.into(), code.as_slice());
        self.cpu.start();

        let winit::dpi::PhysicalSize { width, height } = self.window.as_ref().unwrap().inner_size();
        let surface_texture = SurfaceTexture::new(width, height, self.window.as_ref().unwrap().clone());

        self.pixels = Some(Pixels::new(640, 480, surface_texture).unwrap());
        event_loop.set_control_flow(ControlFlow::WaitUntil(Instant::now() + Duration::from_millis(15)))
    }

    fn new_events(&mut self, event_loop: &ActiveEventLoop, cause: StartCause) {
        if let StartCause::ResumeTimeReached { .. } = cause {
            event_loop.set_control_flow(ControlFlow::WaitUntil(Instant::now() + Duration::from_millis(15)));
            let start = Instant::now();

            draw(self.pixels.as_mut().unwrap().frame_mut(), &mut self.cpu);
            self.pixels.as_mut().unwrap().render().expect("Problem displaying framebuffer");

            loop {
                // TODO: this will run the CPU at 100%, need to not spin while halted
                if let Some((int, arg)) = self.interrupt_events.pop_front() {
                    self.cpu.interrupt(int, arg)
                }
                if self.cpu.running() {
                    for _ in 1..1000 {
                        self.cpu.tick()
                    }
                } else {
                    break
                }
                if Instant::now() > start + Duration::from_millis(14) {
                    break
                }
            }
        }
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, window_id: WindowId, event: WindowEvent) {
        match event {
            WindowEvent::CloseRequested => event_loop.exit(),

            WindowEvent::Resized(new_size) => {
                self.pixels.as_mut().unwrap().resize_surface(new_size.width, new_size.height).expect("Resize");
            }

            WindowEvent::Focused(new_focus) => self.focused = new_focus,

            WindowEvent::KeyboardInput { event, .. } => {
                if let PhysicalKey::Code(key_code) = event.physical_key {
                    let byte = convert_keycode(key_code);
                    let word = Word::from_bytes([
                        byte,
                        if event.state == ElementState::Pressed { 1 } else { 0 },
                        0,
                    ]);
                    self.interrupt_events.push_back((5, Some(word)))
                }
            }
            _ => {}
        }
    }

    // fn device_event(&mut self, event_loop: &ActiveEventLoop, device_id: DeviceId, event: DeviceEvent) {
    //     match event {
    //         // These are sent regardless of whether the window is focused or not, and are the same regardless of keyboard
    //         // layout, so, we have to separately track focus and this will only work as expected for normal US Sholes
    //         // keyboards.
    //         // If we use the WindowEvent equivalent of this, however, shifted non-letter characters will send no vkeys
    //         // at all on Linux. Which is actually worse: this way, someone with a "normal" keyboard can use the app on
    //         // any OS, and there are probably more Linux users than Dvorak users.
    //         // Also, this won't actually affect me, even on Linux, because the KMAC handles the dvorak mapping in the
    //         // keyboard itself, rather than an input map; it generates scancodes as though it were a Sholes board.
    //         // See this bug: https://github.com/rust-windowing/winit/issues/1443
    //         DeviceEvent::Key(input) => {
    //             println!("foo");
    //
    //         }
    //         _ => {}
    //     }
    // }
}

fn main() {
    let event_loop = EventLoop::new().expect("Failed to create event loop");
    let mut app = App::new();
    if let Err(e) = event_loop.run_app(&mut app) {
        println!("Event loop error: {}", e.to_string())
    }
}

fn draw(frame: &mut [u8], cpu: &mut CPU) {
    assert_eq!(frame.len(), 640 * 480 * 4);

    display::draw(cpu, frame);
}
