use std::error::Error;
use std::io;
use std::path::Path;
use wgpu_glyph::{ab_glyph, GlyphBrushBuilder, Section, Text};
use winit::event::{ElementState, ModifiersState, VirtualKeyCode};
use winit::event_loop::ControlFlow;

pub mod ast;
pub mod text_state;

/// The editor is actually launched from the CLI if you pass it zero arguments,
/// or if you provide it 1 or more files or directories to open on launch.
pub fn launch(_filepaths: &[&Path]) -> io::Result<()> {
    // TODO do any initialization here

    run_event_loop().expect("Error running event loop");

    Ok(())
}

fn run_event_loop() -> Result<(), Box<dyn Error>> {
    env_logger::init();

    // Open window and create a surface
    let event_loop = winit::event_loop::EventLoop::new();

    let window = winit::window::WindowBuilder::new()
        .build(&event_loop)
        .unwrap();

    let surface = wgpu::Surface::create(&window);

    // Initialize GPU
    let (device, queue) = futures::executor::block_on(async {
        let adapter = wgpu::Adapter::request(
            &wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::HighPerformance,
                compatible_surface: Some(&surface),
            },
            wgpu::BackendBit::all(),
        )
        .await
        .expect("Request adapter");

        adapter
            .request_device(&wgpu::DeviceDescriptor {
                extensions: wgpu::Extensions {
                    anisotropic_filtering: false,
                },
                limits: wgpu::Limits { max_bind_groups: 1 },
            })
            .await
    });

    // Prepare swap chain
    let render_format = wgpu::TextureFormat::Bgra8UnormSrgb;
    let mut size = window.inner_size();

    let mut swap_chain = device.create_swap_chain(
        &surface,
        &wgpu::SwapChainDescriptor {
            usage: wgpu::TextureUsage::OUTPUT_ATTACHMENT,
            format: render_format,
            width: size.width,
            height: size.height,
            present_mode: wgpu::PresentMode::Immediate,
        },
    );

    // Prepare glyph_brush
    let inconsolata =
        ab_glyph::FontArc::try_from_slice(include_bytes!("../Inconsolata-Regular.ttf"))?;

    let mut glyph_brush = GlyphBrushBuilder::using_font(inconsolata).build(&device, render_format);

    let is_animating = true;
    let mut text_state = String::new();
    let mut keyboard_modifiers = ModifiersState::empty();

    // Render loop
    window.request_redraw();

    event_loop.run(move |event, _, control_flow| {
        // TODO dynamically switch this on/off depending on whether any
        // animations are running. Should conserve CPU usage and battery life!
        if is_animating {
            *control_flow = ControlFlow::Poll;
        } else {
            *control_flow = ControlFlow::Wait;
        }

        match event {
            winit::event::Event::WindowEvent {
                event: winit::event::WindowEvent::CloseRequested,
                ..
            } => *control_flow = winit::event_loop::ControlFlow::Exit,
            winit::event::Event::WindowEvent {
                event: winit::event::WindowEvent::Resized(new_size),
                ..
            } => {
                size = new_size;

                swap_chain = device.create_swap_chain(
                    &surface,
                    &wgpu::SwapChainDescriptor {
                        usage: wgpu::TextureUsage::OUTPUT_ATTACHMENT,
                        format: render_format,
                        width: size.width,
                        height: size.height,
                        present_mode: wgpu::PresentMode::Immediate,
                    },
                );
            }
            winit::event::Event::WindowEvent {
                event: winit::event::WindowEvent::ReceivedCharacter(ch),
                ..
            } => {
                match ch {
                    '\u{8}' => {
                        // In Linux, we get one of these when you press
                        // backspace, but in macOS we don't. In both, we
                        // get a Back keydown event. Therefore, we use the
                        // Back keydown event and ignore this, resulting
                        // in a system that works in both Linux and macOS.
                    }
                    '\u{e000}'..='\u{f8ff}'
                    | '\u{f0000}'..='\u{ffffd}'
                    | '\u{100000}'..='\u{10fffd}' => {
                        // These are private use characters; ignore them.
                        // See http://www.unicode.org/faq/private_use.html
                    }
                    _ => {
                        text_state.push(ch);
                    }
                }
            }
            winit::event::Event::WindowEvent {
                event: winit::event::WindowEvent::KeyboardInput { input, .. },
                ..
            } => {
                if let Some(virtual_keycode) = input.virtual_keycode {
                    handle_keydown(
                        &mut text_state,
                        input.state,
                        virtual_keycode,
                        keyboard_modifiers,
                    );
                }
            }
            winit::event::Event::WindowEvent {
                event: winit::event::WindowEvent::ModifiersChanged(modifiers),
                ..
            } => {
                keyboard_modifiers = modifiers;
            }
            winit::event::Event::MainEventsCleared => window.request_redraw(),
            winit::event::Event::RedrawRequested { .. } => {
                // Get a command encoder for the current frame
                let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                    label: Some("Redraw"),
                });

                // Get the next frame
                let frame = swap_chain.get_next_texture().expect("Get next frame");

                // Clear frame
                {
                    let _ = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                        color_attachments: &[wgpu::RenderPassColorAttachmentDescriptor {
                            attachment: &frame.view,
                            resolve_target: None,
                            load_op: wgpu::LoadOp::Clear,
                            store_op: wgpu::StoreOp::Store,
                            clear_color: wgpu::Color {
                                r: 0.007,
                                g: 0.007,
                                b: 0.007,
                                a: 1.0,
                            },
                        }],
                        depth_stencil_attachment: None,
                    });
                }

                glyph_brush.queue(Section {
                    screen_position: (30.0, 30.0),
                    bounds: (size.width as f32, size.height as f32),
                    text: vec![Text::new("Enter some text:")
                        .with_color([0.4666, 0.2, 1.0, 1.0])
                        .with_scale(40.0)],
                    ..Section::default()
                });

                glyph_brush.queue(Section {
                    screen_position: (30.0, 90.0),
                    bounds: (size.width as f32, size.height as f32),
                    text: vec![Text::new(format!("{}|", text_state).as_str())
                        .with_color([1.0, 1.0, 1.0, 1.0])
                        .with_scale(40.0)],
                    ..Section::default()
                });

                // Draw the text!
                glyph_brush
                    .draw_queued(&device, &mut encoder, &frame.view, size.width, size.height)
                    .expect("Draw queued");

                queue.submit(&[encoder.finish()]);
            }
            _ => {
                *control_flow = winit::event_loop::ControlFlow::Wait;
            }
        }
    })
}

fn handle_keydown(
    text_state: &mut String,
    elem_state: ElementState,
    virtual_keycode: VirtualKeyCode,
    _modifiers: ModifiersState,
) {
    use winit::event::VirtualKeyCode::*;

    if let ElementState::Released = elem_state {
        return;
    }

    match virtual_keycode {
        Back => {
            // Backspace deletes a character.
            // In Linux, we get a Unicode character for backspace events
            // (which is handled elsewhere), but on macOS we only get one of these.
            text_state.pop();
        }
        Copy => {
            todo!("copy");
        }
        Paste => {
            todo!("paste");
        }
        Cut => {
            todo!("cut");
        }
        _ => {}
    }
}
