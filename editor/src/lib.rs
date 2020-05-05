use std::io;
use std::path::Path;

/// The editor is actually launched from the CLI if you pass it zero arguments,
/// or if you provide it 1 or more files or directories to open on launch.
pub fn launch(_filepaths: &[&Path]) -> io::Result<()> {
    // TODO do any initialization here

    run_event_loop();

    Ok(())
}

use winit::{
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};

fn run_event_loop() {
    let event_loop = EventLoop::new();
    let window = WindowBuilder::new().build(&event_loop).unwrap();

    event_loop.run(move |event, _, control_flow| {
        // TODO try ControlFlow::Poll and see if it affects input latency.
        // Otherwise, this seems like a better default for minimizing idle
        // CPU usage and battry drain. (Might want to switch to Poll whenever
        // there are animations in progress though.)
        *control_flow = ControlFlow::Wait;

        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => {
                println!("✈️ Thank you for flying Roc Airlines!");
                *control_flow = ControlFlow::Exit
            }
            Event::MainEventsCleared => window.request_redraw(),
            Event::RedrawRequested(_) => {
                // TODO render the editor
            }
            _ => (),
        }
    });
}
