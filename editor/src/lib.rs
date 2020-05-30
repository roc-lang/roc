use std::error::Error;
use std::io;
use std::path::Path;
use wgpu_glyph::{ab_glyph, GlyphBrushBuilder, Section, Text};
use winit::event::{ElementState, ModifiersState, VirtualKeyCode};
use winit::event_loop::ControlFlow;

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
        .with_maximized(true)
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
                event: winit::event::WindowEvent::KeyboardInput { input, .. },
                ..
            } => {
                if let Some(virtual_keycode) = input.virtual_keycode {
                    handle_text_input(
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
                                r: 0.4,
                                g: 0.4,
                                b: 0.4,
                                a: 1.0,
                            },
                        }],
                        depth_stencil_attachment: None,
                    });
                }

                glyph_brush.queue(Section {
                    screen_position: (30.0, 30.0),
                    bounds: (size.width as f32, size.height as f32),
                    text: vec![Text::new(text_state.as_str())
                        .with_color([0.0, 0.0, 0.0, 1.0])
                        .with_scale(40.0)],
                    ..Section::default()
                });

                glyph_brush.queue(Section {
                    screen_position: (30.0, 90.0),
                    bounds: (size.width as f32, size.height as f32),
                    text: vec![Text::new("Hello wgpu_glyph!")
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

fn handle_text_input(
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
        Key1 | Numpad1 => text_state.push_str("1"),
        Key2 | Numpad2 => text_state.push_str("2"),
        Key3 | Numpad3 => text_state.push_str("3"),
        Key4 | Numpad4 => text_state.push_str("4"),
        Key5 | Numpad5 => text_state.push_str("5"),
        Key6 | Numpad6 => text_state.push_str("6"),
        Key7 | Numpad7 => text_state.push_str("7"),
        Key8 | Numpad8 => text_state.push_str("8"),
        Key9 | Numpad9 => text_state.push_str("9"),
        Key0 | Numpad0 => text_state.push_str("0"),
        A => text_state.push_str("a"),
        B => text_state.push_str("b"),
        C => text_state.push_str("c"),
        D => text_state.push_str("d"),
        E => text_state.push_str("e"),
        F => text_state.push_str("f"),
        G => text_state.push_str("g"),
        H => text_state.push_str("h"),
        I => text_state.push_str("i"),
        J => text_state.push_str("j"),
        K => text_state.push_str("k"),
        L => text_state.push_str("l"),
        M => text_state.push_str("m"),
        N => text_state.push_str("n"),
        O => text_state.push_str("o"),
        P => text_state.push_str("p"),
        Q => text_state.push_str("q"),
        R => text_state.push_str("r"),
        S => text_state.push_str("s"),
        T => text_state.push_str("t"),
        U => text_state.push_str("u"),
        V => text_state.push_str("v"),
        W => text_state.push_str("w"),
        X => text_state.push_str("x"),
        Y => text_state.push_str("y"),
        Z => text_state.push_str("z"),
        Escape | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12 | F13 | F14 | F15
        | F16 | F17 | F18 | F19 | F20 | F21 | F22 | F23 | F24 | Snapshot | Scroll | Pause
        | Insert | Home | Delete | End | PageDown | PageUp | Left | Up | Right | Down | Compose
        | Caret | Numlock | AbntC1 | AbntC2 | Ax | Calculator | Capital | Convert | Kana
        | Kanji | LAlt | LBracket | LControl | LShift | LWin | Mail | MediaSelect | PlayPause
        | Power | PrevTrack | MediaStop | Mute | MyComputer | NavigateForward
        | NavigateBackward | NextTrack | NoConvert | OEM102 | RAlt | Sysrq | RBracket
        | RControl | RShift | RWin | Sleep | Stop | Unlabeled | VolumeDown | VolumeUp | Wake
        | WebBack | WebFavorites | WebForward | WebHome | WebRefresh | WebSearch | Apps | Tab
        | WebStop => {
            // TODO handle
            dbg!(virtual_keycode);
        }
        Back => {
            text_state.pop();
        }
        Return | NumpadEnter => {
            text_state.push_str("\n");
        }
        Space => {
            text_state.push_str(" ");
        }
        Comma | NumpadComma => {
            text_state.push_str(",");
        }
        Add => {
            text_state.push_str("+");
        }
        Apostrophe => {
            text_state.push_str("'");
        }
        At => {
            text_state.push_str("@");
        }
        Backslash => {
            text_state.push_str("\\");
        }
        Colon => {
            text_state.push_str(":");
        }
        Period | Decimal => {
            text_state.push_str(".");
        }
        Equals | NumpadEquals => {
            text_state.push_str("=");
        }
        Grave => {
            text_state.push_str("`");
        }
        Minus | Subtract => {
            text_state.push_str("-");
        }
        Multiply => {
            text_state.push_str("*");
        }
        Semicolon => {
            text_state.push_str(";");
        }
        Slash | Divide => {
            text_state.push_str("/");
        }
        Underline => {
            text_state.push_str("_");
        }
        Yen => {
            text_state.push_str("¥");
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
    }
}
