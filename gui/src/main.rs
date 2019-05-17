extern crate winit;

use winit::EventsLoop;
use winit::Window;

#[derive(Debug)]
pub struct WinitState {
  pub events_loop: EventsLoop,
  pub window: Window,
}

fn open_window() -> Result<WinitState, winit::CreationError> {
    let events_loop = EventsLoop::new();

    winit::WindowBuilder::new()
        .with_title("roc")
        .build(&events_loop)
        .map(|window| {
            WinitState {
                events_loop: events_loop,
                window: window
            }
        })

}

fn main() {
    let mut wopen_state = open_window().unwrap();

    wopen_state.events_loop.run_forever(|event| {
        match event {
            winit::Event::WindowEvent {
                event: winit::WindowEvent::CloseRequested,
                ..
            } => winit::ControlFlow::Break,
            _ => winit::ControlFlow::Continue,
        }
    });
}
