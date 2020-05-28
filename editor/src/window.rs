use winit::{
    dpi::LogicalSize,
    error::OsError,
    event::Event,
    event_loop::{ControlFlow, EventLoop, EventLoopWindowTarget},
    window::{Window, WindowBuilder},
};

#[derive(Debug)]
pub struct WindowState {
    event_loop: EventLoop<()>,
    pub window: Window,
}

impl WindowState {
    pub fn new<T: Into<String>>(title: T, size: LogicalSize<u32>) -> Result<Self, OsError> {
        let event_loop = EventLoop::new();
        let output = WindowBuilder::new()
            .with_title(title)
            .with_inner_size(size)
            .with_maximized(true)
            .build(&event_loop);

        output.map(|window| Self { event_loop, window })
    }

    pub fn run_event_loop<F>(self, event_handler: F) -> !
    where
        F: 'static + FnMut(Event<()>, &EventLoopWindowTarget<()>, &mut ControlFlow),
    {
        self.event_loop.run(event_handler)
    }
}
