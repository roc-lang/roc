use gfx_hal::{
    device::Device,
    window::{Extent2D, PresentationSurface, Surface},
    Instance,
};
use glsl_to_spirv::ShaderType;
use std::io;
use std::mem::ManuallyDrop;
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
    // TODO do a better window size
    const WINDOW_SIZE: [u32; 2] = [512, 512];

    let event_loop = EventLoop::new();

    let (logical_window_size, physical_window_size) = {
        use winit::dpi::{LogicalSize, PhysicalSize};

        let dpi = event_loop.primary_monitor().scale_factor();
        let logical: LogicalSize<u32> = WINDOW_SIZE.into();
        let physical: PhysicalSize<u32> = logical.to_physical(dpi);

        (logical, physical)
    };

    let mut surface_extent = Extent2D {
        width: physical_window_size.width,
        height: physical_window_size.height,
    };

    let window = WindowBuilder::new()
        .with_title("roc")
        .with_inner_size(logical_window_size)
        .build(&event_loop)
        .unwrap();

    let mut should_configure_swapchain = true;

    event_loop.run(move |event, _, control_flow| {
        use winit::event::{Event, WindowEvent};
        use winit::event_loop::ControlFlow;

        // TODO try ControlFlow::Poll and see if it affects input latency.
        // Otherwise, this seems like a better default for minimizing idle
        // CPU usage and battry drain. (Might want to switch to Poll whenever
        // there are animations in progress though.)
        *control_flow = ControlFlow::Wait;

        match event {
            Event::WindowEvent {
                event: window_event,
                ..
            } => match window_event {
                WindowEvent::CloseRequested => {
                    println!("✈️ Thank you for flying Roc Airlines!");
                    *control_flow = ControlFlow::Exit
                }
                WindowEvent::Resized(dims) => {
                    surface_extent = Extent2D {
                        width: dims.width,
                        height: dims.height,
                    };
                    should_configure_swapchain = true;
                }
                WindowEvent::ScaleFactorChanged { new_inner_size, .. } => {
                    surface_extent = Extent2D {
                        width: new_inner_size.width,
                        height: new_inner_size.height,
                    };
                    should_configure_swapchain = true;
                }
                _ => (),
            },
            Event::MainEventsCleared => window.request_redraw(),
            Event::RedrawRequested(_) => {
                // TODO render the editor
            }
            _ => (),
        }
    });
}
