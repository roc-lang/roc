use crate::{gfx::GfxState, text_state::handle_text_input, window::WindowState};
use gfx_hal::window::Extent2D;
use winit::{
    dpi::{LogicalSize, PhysicalSize},
    event::{Event, ModifiersState, WindowEvent},
    event_loop::ControlFlow,
};

pub fn run() {
    let logical_window_size = LogicalSize {
        width: 800,
        height: 600,
    };
    let mut window_state = WindowState::new("roc", logical_window_size)
        .expect("TODO gracefuly handle error initializing window");

    let window = window_state.window;
    let physical_window_size = window_state.physical_window_size();
    let mut surface_extent = Extent2D {
        width: physical_window_size.width,
        height: physical_window_size.height,
    };
    let mut gfx_state = GfxState::new(&window, surface_extent);
    let mut editor_state = EditorState::default();
    let is_animating = true;
    let mut text_state = String::new();
    let mut keyboard_modifiers = ModifiersState::empty();
    let mut should_configure_swapchain = false;

    window_state.run_event_loop(|event, _, control_flow| {
        // TODO dynamically switch this on/off depending on whether any
        // animations are running. Should conserve CPU usage and battery life!
        if is_animating {
            *control_flow = ControlFlow::Poll;
        } else {
            *control_flow = ControlFlow::Wait;
        }

        match event {
            Event::WindowEvent {
                event: window_event,
                ..
            } => match window_event {
                WindowEvent::CloseRequested => {
                    println!("✈️ Thank you for flying Roc Airlines!");
                    *control_flow = ControlFlow::Exit
                }
                WindowEvent::KeyboardInput { input, .. } => {
                    if let Some(virtual_keycode) = input.virtual_keycode {
                        handle_text_input(
                            &mut text_state,
                            input.state,
                            virtual_keycode,
                            keyboard_modifiers,
                        );
                    }
                }
                WindowEvent::ModifiersChanged(modifiers) => {
                    keyboard_modifiers = modifiers;
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
            },
            Event::MainEventsCleared => window.request_redraw(),
            Event::RedrawRequested(_) => {
                if let Err(err) = do_the_render(&mut gfx_state, &editor_state) {
                    todo!("if should_configure_swapchain ...");
                    panic!("TODO gracefully handle rendering Error: {:?}", err);
                }
            }
            _ => (),
        }
    });
}
