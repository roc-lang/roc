use super::keyboard_input;
use super::style::CODE_TXT_XY;
use crate::editor::ed_error::print_ui_err;
use crate::editor::mvc::ed_view;
use crate::editor::mvc::ed_view::RenderedWgpu;
use crate::editor::resources::strings::NOTHING_OPENED;
use crate::editor::{
    config::Config,
    ed_error::print_err,
    mvc::{app_model::AppModel, app_update, ed_model},
    theme::EdTheme,
};
use crate::graphics::{
    colors::to_wgpu_color,
    lowlevel::buffer::create_rect_buffers,
    lowlevel::ortho::update_ortho_buffer,
    lowlevel::pipelines,
    primitives::rect::Rect,
    primitives::text::{build_glyph_brush, example_code_glyph_rect, queue_text_draw, Text},
};
use crate::lang::expr::Env;
use crate::lang::pool::Pool;
use crate::ui::ui_error::UIError::FileOpenFailed;
use crate::ui::util::slice_get;
use bumpalo::collections::String as BumpString;
use bumpalo::Bump;
use cgmath::Vector2;
use pipelines::RectResources;
use roc_module::symbol::{IdentIds, ModuleIds};
use roc_types::subs::VarStore;
use std::{error::Error, io, path::Path};
use wgpu::{CommandEncoder, RenderPass, TextureView};
use wgpu_glyph::GlyphBrush;
use winit::{
    dpi::PhysicalSize,
    event,
    event::{Event, ModifiersState},
    event_loop::ControlFlow,
    platform::run_return::EventLoopExtRunReturn,
};

// Inspired by:
// https://github.com/sotrh/learn-wgpu by Benjamin Hansen, licensed under the MIT license
// https://github.com/cloudhead/rgx by Alexis Sellier, licensed under the MIT license

// See this link to learn wgpu: https://sotrh.github.io/learn-wgpu/

/// The editor is actually launched from the CLI if you pass it zero arguments,
/// or if you provide it 1 or more files or directories to open on launch.
pub fn launch(filepaths: &[&Path]) -> io::Result<()> {
    //TODO support using multiple filepaths
    let first_path_opt = if !filepaths.is_empty() {
        match slice_get(0, filepaths) {
            Ok(path_ref_ref) => Some(*path_ref_ref),
            Err(e) => {
                eprintln!("{}", e);
                None
            }
        }
    } else {
        None
    };

    run_event_loop(first_path_opt).expect("Error running event loop");

    Ok(())
}

fn run_event_loop(file_path_opt: Option<&Path>) -> Result<(), Box<dyn Error>> {
    env_logger::init();

    // Open window and create a surface
    let mut event_loop = winit::event_loop::EventLoop::new();

    let window = winit::window::WindowBuilder::new()
        .with_inner_size(PhysicalSize::new(1200.0, 1000.0))
        .build(&event_loop)
        .unwrap();

    let instance = wgpu::Instance::new(wgpu::BackendBit::all());

    let surface = unsafe { instance.create_surface(&window) };

    // Initialize GPU
    let (gpu_device, cmd_queue) = futures::executor::block_on(async {
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::HighPerformance,
                compatible_surface: Some(&surface),
            })
            .await
            .expect("Request adapter");

        adapter
            .request_device(
                &wgpu::DeviceDescriptor {
                    label: None,
                    features: wgpu::Features::empty(),
                    limits: wgpu::Limits::default(),
                },
                None,
            )
            .await
            .expect("Request device")
    });

    // Create staging belt and a local pool
    let mut staging_belt = wgpu::util::StagingBelt::new(1024);
    let mut local_pool = futures::executor::LocalPool::new();
    let local_spawner = local_pool.spawner();

    // Prepare swap chain
    let render_format = wgpu::TextureFormat::Bgra8Unorm;
    let mut size = window.inner_size();

    let swap_chain_descr = wgpu::SwapChainDescriptor {
        usage: wgpu::TextureUsage::RENDER_ATTACHMENT,
        format: render_format,
        width: size.width,
        height: size.height,
        // TODO go back to Immediate
        present_mode: wgpu::PresentMode::Fifo,
    };

    let mut swap_chain = gpu_device.create_swap_chain(&surface, &swap_chain_descr);

    let rect_resources = pipelines::make_rect_pipeline(&gpu_device, &swap_chain_descr);

    let mut glyph_brush = build_glyph_brush(&gpu_device, render_format)?;

    let is_animating = true;

    let mut env_pool = Pool::with_capacity(1024);
    let env_arena = Bump::new();
    let code_arena = Bump::new();

    let mut var_store = VarStore::default();
    let dep_idents = IdentIds::exposed_builtins(8);

    let exposed_ident_ids = IdentIds::default();
    let mut module_ids = ModuleIds::default();
    let mod_id = module_ids.get_or_insert(&"ModId123".into());

    let env = Env::new(
        mod_id,
        &env_arena,
        &mut env_pool,
        &mut var_store,
        dep_idents,
        &module_ids,
        exposed_ident_ids,
    );

    let mut code_str = BumpString::from_str_in("", &code_arena);

    let file_path = if let Some(file_path) = file_path_opt {
        match std::fs::read_to_string(file_path) {
            Ok(file_as_str) => {
                code_str = BumpString::from_str_in(&file_as_str, &code_arena);
                file_path
            }

            Err(e) => {
                print_ui_err(&FileOpenFailed {
                    path_str: file_path.to_string_lossy().to_string(),
                    err_msg: e.to_string(),
                });
                Path::new("")
            }
        }
    } else {
        Path::new("")
    };

    let ed_model_opt = {
        let ed_model_res = ed_model::init_model(&code_str, file_path, env, &code_arena);

        match ed_model_res {
            Ok(mut ed_model) => {
                ed_model.glyph_dim_rect_opt = Some(example_code_glyph_rect(&mut glyph_brush));

                Some(ed_model)
            }
            Err(e) => {
                print_err(&e);
                None
            }
        }
    };

    let mut rendered_wgpu_opt: Option<RenderedWgpu> = None;

    let mut app_model = AppModel::init(ed_model_opt);

    let mut keyboard_modifiers = ModifiersState::empty();

    let config: Config = Config::default(); //confy::load("roc_editor", None)?;
    let ed_theme = EdTheme::default();

    // Render loop
    window.request_redraw();

    event_loop.run_return(|event, _, control_flow| {
        // TODO dynamically switch this on/off depending on whether any
        // animations are running. Should conserve CPU usage and battery life!
        if is_animating {
            *control_flow = ControlFlow::Poll;
        } else {
            *control_flow = ControlFlow::Wait;
        }

        match event {
            //Close
            Event::WindowEvent {
                event: event::WindowEvent::CloseRequested,
                ..
            } => *control_flow = ControlFlow::Exit,
            //Resize
            Event::WindowEvent {
                event: event::WindowEvent::Resized(new_size),
                ..
            } => {
                size = new_size;

                swap_chain = gpu_device.create_swap_chain(
                    &surface,
                    &wgpu::SwapChainDescriptor {
                        usage: wgpu::TextureUsage::RENDER_ATTACHMENT,
                        format: render_format,
                        width: size.width,
                        height: size.height,
                        // TODO go back to Immediate
                        present_mode: wgpu::PresentMode::Fifo,
                    },
                );

                update_ortho_buffer(
                    size.width,
                    size.height,
                    &gpu_device,
                    &rect_resources.ortho.buffer,
                    &cmd_queue,
                );
            }
            //Received Character
            Event::WindowEvent {
                event: event::WindowEvent::ReceivedCharacter(ch),
                ..
            } => {
                if let Err(e) = app_update::handle_new_char(&ch, &mut app_model) {
                    print_err(&e)
                }
            }
            //Keyboard Input
            Event::WindowEvent {
                event: event::WindowEvent::KeyboardInput { input, .. },
                ..
            } => {
                if let Some(virtual_keycode) = input.virtual_keycode {
                    if let Some(ref mut ed_model) = app_model.ed_model_opt {
                        if ed_model.has_focus {
                            let keydown_res = keyboard_input::handle_keydown(
                                input.state,
                                virtual_keycode,
                                keyboard_modifiers,
                                &mut app_model,
                            );

                            if let Err(e) = keydown_res {
                                print_err(&e)
                            }
                        }
                    }
                }
            }
            //Modifiers Changed
            Event::WindowEvent {
                event: event::WindowEvent::ModifiersChanged(modifiers),
                ..
            } => {
                keyboard_modifiers = modifiers;
            }
            Event::MainEventsCleared => window.request_redraw(),
            Event::RedrawRequested { .. } => {
                // Get a command encoder for the current frame
                let mut encoder =
                    gpu_device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                        label: Some("Redraw"),
                    });

                let frame = swap_chain
                    .get_current_frame()
                    .expect("Failed to acquire next SwapChainFrame")
                    .output;

                if let Some(ref mut ed_model) = app_model.ed_model_opt {
                    if rendered_wgpu_opt.is_none() || ed_model.dirty {
                        let rendered_wgpu_res =
                            ed_view::model_to_wgpu(ed_model, &size, CODE_TXT_XY.into(), &config);

                        match rendered_wgpu_res {
                            Ok(rendered_wgpu) => rendered_wgpu_opt = Some(rendered_wgpu),
                            Err(e) => print_err(&e),
                        }

                        ed_model.dirty = false;
                    }

                    if let Some(ref rendered_wgpu) = rendered_wgpu_opt {
                        let borrowed_text = rendered_wgpu.text.to_borrowed();

                        glyph_brush.queue(borrowed_text);

                        draw_all_rects(
                            &rendered_wgpu.rects,
                            &mut encoder,
                            &frame.view,
                            &gpu_device,
                            &rect_resources,
                            &ed_theme,
                        )
                    }
                } else {
                    queue_no_file_text(
                        &size,
                        NOTHING_OPENED,
                        CODE_TXT_XY.into(),
                        &config,
                        &mut glyph_brush,
                    );
                }

                // draw all text
                glyph_brush
                    .draw_queued(
                        &gpu_device,
                        &mut staging_belt,
                        &mut encoder,
                        &frame.view,
                        size.width,
                        size.height,
                    )
                    .expect("Draw queued");

                staging_belt.finish();
                cmd_queue.submit(Some(encoder.finish()));

                // Recall unused staging buffers
                use futures::task::SpawnExt;

                local_spawner
                    .spawn(staging_belt.recall())
                    .expect("Recall staging belt");

                local_pool.run_until_stalled();
            }
            _ => {
                *control_flow = winit::event_loop::ControlFlow::Wait;
            }
        }
    });

    Ok(())
}

fn draw_all_rects(
    all_rects: &[Rect],
    encoder: &mut CommandEncoder,
    texture_view: &TextureView,
    gpu_device: &wgpu::Device,
    rect_resources: &RectResources,
    ed_theme: &EdTheme,
) {
    let rect_buffers = create_rect_buffers(gpu_device, encoder, all_rects);

    let mut render_pass = begin_render_pass(encoder, texture_view, ed_theme);

    render_pass.set_pipeline(&rect_resources.pipeline);
    render_pass.set_bind_group(0, &rect_resources.ortho.bind_group, &[]);
    render_pass.set_vertex_buffer(0, rect_buffers.vertex_buffer.slice(..));
    render_pass.set_index_buffer(
        rect_buffers.index_buffer.slice(..),
        wgpu::IndexFormat::Uint32,
    );
    render_pass.draw_indexed(0..rect_buffers.num_rects, 0, 0..1);
}

fn begin_render_pass<'a>(
    encoder: &'a mut CommandEncoder,
    texture_view: &'a TextureView,
    ed_theme: &EdTheme,
) -> RenderPass<'a> {
    let bg_color = to_wgpu_color(ed_theme.background);

    encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
        color_attachments: &[wgpu::RenderPassColorAttachmentDescriptor {
            attachment: texture_view,
            resolve_target: None,
            ops: wgpu::Operations {
                load: wgpu::LoadOp::Clear(bg_color),
                store: true,
            },
        }],
        depth_stencil_attachment: None,
        label: None,
    })
}

fn queue_no_file_text(
    size: &PhysicalSize<u32>,
    text: &str,
    text_coords: Vector2<f32>,
    config: &Config,
    glyph_brush: &mut GlyphBrush<()>,
) {
    let area_bounds = (size.width as f32, size.height as f32).into();

    let code_text = Text {
        position: text_coords,
        area_bounds,
        color: config.ed_theme.ui_theme.text,
        text,
        size: config.code_font_size,
        ..Default::default()
    };

    queue_text_draw(&code_text, glyph_brush);
}
