use crate::window::WindowState;
use winit::{
    dpi::LogicalSize,
    event::{Event, WindowEvent},
    event_loop::ControlFlow,
};

pub fn run() {
    let mut window_state = WindowState::new(
        "roc",
        LogicalSize {
            width: 800,
            height: 600,
        },
    )
    .expect("TODO gracefuly handle error initializing window");

    window_state.run_event_loop(|event, _, control_flow| match event {
        Event::WindowEvent {
            event: WindowEvent::CloseRequested,
            ..
        } => {
            println!("✈️ Thank you for flying Roc Airlines!");
            *control_flow = ControlFlow::Exit
        }
        _ => (),
    });
}
