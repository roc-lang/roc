use winit::{
    dpi::{LogicalSize, PhysicalSize},
    error::OsError,
    event::Event,
    event_loop::{ControlFlow, EventLoop, EventLoopWindowTarget},
    window::{Window, WindowBuilder},
};

#[derive(Debug)]
pub struct WindowState {
    event_loop: EventLoop<()>,
    logical_size: LogicalSize<u32>,
    pub window: Window,
}

impl WindowState {
    pub fn new<T: Into<String>>(title: T, logical_size: LogicalSize<u32>) -> Result<Self, OsError> {
        let event_loop = EventLoop::new();
        let output = WindowBuilder::new()
            .with_title(title)
            .with_inner_size(logical_size)
            .with_maximized(true)
            .build(&event_loop);

        output.map(|window| Self {
            event_loop,
            window,
            logical_size,
        })
    }

    pub fn physical_window_size(&self) -> PhysicalSize<u32> {
        let dpi = self.event_loop.primary_monitor().scale_factor();
        let physical: PhysicalSize<u32> = self.logical_size.to_physical(dpi);

        physical
    }

    pub fn run_event_loop<F>(self, event_handler: F) -> !
    where
        F: 'static + FnMut(Event<()>, &EventLoopWindowTarget<()>, &mut ControlFlow),
    {
        self.event_loop.run(event_handler)
    }
}
