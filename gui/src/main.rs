extern crate winit;

fn main() {
    let mut events_loop = winit::EventsLoop::new();

    winit::WindowBuilder::new()
        .with_title("roc")
        .build(&events_loop)
        .unwrap();

    events_loop.run_forever(|event| {
        match event {
            winit::Event::WindowEvent {
                event: winit::WindowEvent::CloseRequested,
                ..
            } => winit::ControlFlow::Break,
            _ => winit::ControlFlow::Continue,
        }
    });
}
