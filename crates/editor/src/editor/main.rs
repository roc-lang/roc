use super::keyboard_input;
use super::resources::strings::PLATFORM_DIR_NAME;
use crate::editor::mvc::ed_view;
use crate::editor::mvc::ed_view::RenderedWgpu;
use crate::editor::resources::strings::{HELLO_WORLD, NOTHING_OPENED};
use crate::editor::{
    config::Config,
    ed_error::print_err,
    mvc::{app_model::AppModel, app_update, app_update::InputOutcome, ed_model},
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
use crate::ui::text::caret_w_select::CaretPos;
use bumpalo::Bump;
use cgmath::Vector2;
use fs_extra::dir::{copy, ls, CopyOptions, DirEntryAttr, DirEntryValue};
use futures::TryFutureExt;
use pipelines::RectResources;
use roc_ast::lang::env::Env;
use roc_ast::mem_pool::pool::Pool;
use roc_ast::module::load_module;
use roc_load::Threading;
use roc_module::symbol::IdentIds;
use roc_packaging::cache::{self, RocCacheDir};
use roc_types::subs::VarStore;
use std::collections::HashSet;
use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;
use std::{error::Error, io, path::Path};
use wgpu::{CommandEncoder, LoadOp, RenderPass, TextureView};
use wgpu_glyph::GlyphBrush;
use winit::{
    dpi::PhysicalSize,
    event,
    event::{Event, ModifiersState},
    event_loop::ControlFlow,
    platform::run_return::EventLoopExtRunReturn,
};

// Inspired by:
// https://github.com/sotrh/learn-wgpu by Benjamin Hansen, which is licensed under the MIT license
// https://github.com/cloudhead/rgx by Alexis Sellier, which is licensed under the MIT license
//
// See this link to learn wgpu: https://sotrh.github.io/learn-wgpu/

/// The editor is actually launched from the CLI if you pass it zero arguments,
/// or if you provide it 1 or more files or directories to open on launch.
pub fn launch(project_dir_path_opt: Option<&Path>) -> io::Result<()> {
    run_event_loop(project_dir_path_opt).expect("Error running event loop");

    Ok(())
}

fn run_event_loop(project_dir_path_opt: Option<&Path>) -> Result<(), Box<dyn Error>> {
    // Open window and create a surface
    let mut event_loop = winit::event_loop::EventLoop::new();

    let window = winit::window::WindowBuilder::new()
        .with_inner_size(PhysicalSize::new(1900.0, 1000.0))
        .with_title("The Roc Editor - Work In Progress")
        .build(&event_loop)
        .unwrap();

    let instance = wgpu::Instance::new(wgpu::Backends::all());

    let surface = unsafe { instance.create_surface(&window) };

    // Initialize GPU
    let (gpu_device, cmd_queue, color_format) = futures::executor::block_on(async {
        create_device(
            &instance,
            &surface,
            wgpu::PowerPreference::HighPerformance,
            false,
        )
        .or_else(|_| create_device(&instance, &surface, wgpu::PowerPreference::LowPower, false))
        .or_else(|_| {
            create_device(
                &instance,
                &surface,
                wgpu::PowerPreference::HighPerformance,
                true,
            )
        })
        .unwrap_or_else(|err| {
            panic!("Failed to request device: `{}`", err);
        })
        .await
    });

    // Create staging belt and a local pool
    let mut staging_belt = wgpu::util::StagingBelt::new(1024);
    let mut local_pool = futures::executor::LocalPool::new();
    let local_spawner = local_pool.spawner();

    let mut size = window.inner_size();

    let surface_config = wgpu::SurfaceConfiguration {
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
        format: color_format,
        width: size.width,
        height: size.height,
        present_mode: wgpu::PresentMode::Mailbox,
    };

    surface.configure(&gpu_device, &surface_config);

    let rect_resources = pipelines::make_rect_pipeline(&gpu_device, &surface_config);

    let mut glyph_brush = build_glyph_brush(&gpu_device, color_format)?;

    let is_animating = true;

    let mut env_pool = Pool::with_capacity(1024);
    let env_arena = Bump::new();
    let code_arena = Bump::new();

    let (file_path_str, code_str) = read_main_roc_file(project_dir_path_opt);
    println!("Loading file {:?}...", file_path_str);

    let file_path = Path::new(&file_path_str);
    let loaded_module = load_module(
        file_path,
        RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
        Threading::AllAvailable,
    );

    let mut var_store = VarStore::default();
    let dep_idents = IdentIds::exposed_builtins(8);
    let exposed_ident_ids = IdentIds::default();
    let module_ids = loaded_module.interns.module_ids.clone();

    let env = Env::new(
        loaded_module.module_id,
        &env_arena,
        &mut env_pool,
        &mut var_store,
        dep_idents,
        &module_ids,
        exposed_ident_ids,
    );

    let config: Config = Config::default(); //confy::load("roc_editor", None)?;
    let ed_model_opt = {
        let ed_model_res = ed_model::init_model(
            &code_str,
            file_path,
            env,
            loaded_module,
            &code_arena,
            CaretPos::End,
        );

        match ed_model_res {
            Ok(mut ed_model) => {
                ed_model.glyph_dim_rect_opt = Some(example_code_glyph_rect(
                    &mut glyph_brush,
                    config.code_font_size,
                ));

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

                surface.configure(
                    &gpu_device,
                    &wgpu::SurfaceConfiguration {
                        usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
                        format: color_format,
                        width: size.width,
                        height: size.height,
                        present_mode: wgpu::PresentMode::Mailbox,
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
                let input_outcome_res =
                    app_update::handle_new_char(&ch, &mut app_model, keyboard_modifiers);
                if let Err(e) = input_outcome_res {
                    print_err(&e)
                } else if let Ok(InputOutcome::Ignored) = input_outcome_res {
                    println!("\nInput '{}' ignored!", ch);
                } else {
                    window.request_redraw()
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

                            window.request_redraw()
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
            Event::RedrawRequested { .. } => {
                // Get a command encoder for the current frame
                let mut encoder =
                    gpu_device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                        label: Some("Redraw"),
                    });

                let surface_texture = surface
                    .get_current_texture()
                    .expect("Failed to acquire next SwapChainTexture");

                let view = surface_texture
                    .texture
                    .create_view(&wgpu::TextureViewDescriptor::default());

                if let Some(ref mut ed_model) = app_model.ed_model_opt {
                    if rendered_wgpu_opt.is_none() || ed_model.dirty {
                        let rendered_wgpu_res = ed_view::model_to_wgpu(
                            ed_model,
                            &size,
                            config.make_code_txt_xy().into(),
                            &config,
                        );

                        match rendered_wgpu_res {
                            Ok(rendered_wgpu) => rendered_wgpu_opt = Some(rendered_wgpu),
                            Err(e) => print_err(&e),
                        }

                        ed_model.dirty = false;
                    }

                    if let Some(ref rendered_wgpu) = rendered_wgpu_opt {
                        draw_rects(
                            &rendered_wgpu.rects_behind,
                            &mut encoder,
                            &view,
                            &gpu_device,
                            &rect_resources,
                            wgpu::LoadOp::Clear(to_wgpu_color(ed_theme.background)),
                        );

                        for text_section in &rendered_wgpu.text_sections_behind {
                            let borrowed_text = text_section.to_borrowed();

                            glyph_brush.queue(borrowed_text);
                        }

                        // draw first layer of text
                        glyph_brush
                            .draw_queued(
                                &gpu_device,
                                &mut staging_belt,
                                &mut encoder,
                                &view,
                                size.width,
                                size.height,
                            )
                            .expect("Failed to draw first layer of text.");

                        // draw rects on top of first text layer
                        draw_rects(
                            &rendered_wgpu.rects_front,
                            &mut encoder,
                            &view,
                            &gpu_device,
                            &rect_resources,
                            wgpu::LoadOp::Load,
                        );

                        for text_section in &rendered_wgpu.text_sections_front {
                            let borrowed_text = text_section.to_borrowed();

                            glyph_brush.queue(borrowed_text);
                        }
                    }
                } else {
                    begin_render_pass(
                        &mut encoder,
                        &view,
                        wgpu::LoadOp::Clear(to_wgpu_color(ed_theme.background)),
                    );

                    queue_no_file_text(
                        &size,
                        NOTHING_OPENED,
                        config.make_code_txt_xy().into(),
                        &config,
                        &mut glyph_brush,
                    );
                }

                // draw text
                glyph_brush
                    .draw_queued(
                        &gpu_device,
                        &mut staging_belt,
                        &mut encoder,
                        &view,
                        size.width,
                        size.height,
                    )
                    .expect("Failed to draw queued text.");

                staging_belt.finish();
                cmd_queue.submit(Some(encoder.finish()));
                surface_texture.present();

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

async fn create_device(
    instance: &wgpu::Instance,
    surface: &wgpu::Surface,
    power_preference: wgpu::PowerPreference,
    force_fallback_adapter: bool,
) -> Result<(wgpu::Device, wgpu::Queue, wgpu::TextureFormat), wgpu::RequestDeviceError> {
    if force_fallback_adapter {
        log::error!("Falling back to software renderer. GPU acceleration has been disabled.");
    }

    let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference,
                compatible_surface: Some(surface),
                force_fallback_adapter,
            })
            .await
            .expect(r#"Request adapter
            If you're running this from inside nix, follow the instructions here to resolve this: https://github.com/roc-lang/roc/blob/main/BUILDING_FROM_SOURCE.md#editor
            "#);

    let color_format = surface.get_preferred_format(&adapter).unwrap();

    if color_format != wgpu::TextureFormat::Bgra8UnormSrgb {
        log::warn!("Your preferred TextureFormat {:?} is different than expected. Colors may look different, please report this issue on github and tag @Anton-4.", color_format);
    }

    let request_res = adapter
        .request_device(
            &wgpu::DeviceDescriptor {
                label: None,
                features: wgpu::Features::empty(),
                limits: wgpu::Limits::default(),
            },
            None,
        )
        .await;

    match request_res {
        Ok((device, queue)) => Ok((device, queue, color_format)),
        Err(err) => Err(err),
    }
}

fn draw_rects(
    all_rects: &[Rect],
    encoder: &mut CommandEncoder,
    texture_view: &TextureView,
    gpu_device: &wgpu::Device,
    rect_resources: &RectResources,
    load_op: LoadOp<wgpu::Color>,
) {
    let rect_buffers = create_rect_buffers(gpu_device, encoder, all_rects);

    let mut render_pass = begin_render_pass(encoder, texture_view, load_op);

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
    load_op: LoadOp<wgpu::Color>,
) -> RenderPass<'a> {
    encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
        color_attachments: &[wgpu::RenderPassColorAttachment {
            view: texture_view,
            resolve_target: None,
            ops: wgpu::Operations {
                load: load_op,
                store: true,
            },
        }],
        depth_stencil_attachment: None,
        label: None,
    })
}

const ROC_PROJECTS_FOLDER: &str = "roc-projects";
const ROC_NEW_PROJECT_FOLDER: &str = "new-roc-project-1";

fn read_main_roc_file(project_dir_path_opt: Option<&Path>) -> (PathBuf, String) {
    if let Some(project_dir_path) = project_dir_path_opt {
        let mut ls_config = HashSet::new();
        ls_config.insert(DirEntryAttr::FullName);

        let dir_items = ls(project_dir_path, &ls_config)
            .unwrap_or_else(|err| panic!("Failed to list items in project directory: {:?}", err))
            .items;

        let file_names = dir_items.iter().flat_map(|info_hash_map| {
            info_hash_map
                .values()
                .filter_map(|dir_entry_value| {
                    if let DirEntryValue::String(file_name) = dir_entry_value {
                        Some(file_name)
                    } else {
                        None
                    }
                })
                .collect::<Vec<&String>>()
        });

        let roc_file_names: Vec<&String> = file_names
            .filter(|file_name| file_name.contains(".roc"))
            .collect();

        if let Some(&roc_file_name) = roc_file_names.first() {
            let full_roc_file_path = project_dir_path.join(roc_file_name);
            let file_as_str = std::fs::read_to_string(Path::new(&full_roc_file_path))
                .unwrap_or_else(|err| panic!("In the provided project {:?}, I found the roc file {:?}, but I failed to read it: {}", &project_dir_path, full_roc_file_path, err));

            (full_roc_file_path, file_as_str)
        } else {
            init_new_roc_project(project_dir_path)
        }
    } else {
        init_new_roc_project(&Path::new(ROC_PROJECTS_FOLDER).join(ROC_NEW_PROJECT_FOLDER))
    }
}

// returns path and content of app file
fn init_new_roc_project(project_dir_path: &Path) -> (PathBuf, String) {
    let orig_platform_path = Path::new("examples")
        .join("platform-switching")
        .join(PLATFORM_DIR_NAME);

    let roc_file_path = Path::new(project_dir_path).join("main.roc");

    let project_platform_path = project_dir_path.join(PLATFORM_DIR_NAME);

    if !project_dir_path.exists() {
        fs::create_dir_all(project_dir_path).expect("Failed to create dir for roc project.");
    }

    copy_roc_platform_if_not_exists(
        &orig_platform_path,
        &project_platform_path,
        project_dir_path,
    );

    let code_str = create_roc_file_if_not_exists(project_dir_path, &roc_file_path);

    (roc_file_path, code_str)
}

// returns contents of file
fn create_roc_file_if_not_exists(project_dir_path: &Path, roc_file_path: &Path) -> String {
    if !roc_file_path.exists() {
        let mut roc_file = File::create(roc_file_path).unwrap_or_else(|err| {
            panic!("No roc file path was passed to the editor, so I wanted to create a new roc project with the file {:?}, but it failed: {}", roc_file_path, err)
        });

        write!(roc_file, "{}", HELLO_WORLD).unwrap_or_else(|err| {
            panic!(
                r#"No roc file path was passed to the editor, so I created a new roc project with the file {:?}
                I wanted to write roc hello world to that file, but it failed: {:?}"#,
                roc_file_path,
                err
            )
        });

        HELLO_WORLD.to_string()
    } else {
        std::fs::read_to_string(roc_file_path).unwrap_or_else(|err| {
            panic!(
                "I detected an existing {:?} inside {:?}, but I failed to read from it: {}",
                roc_file_path, project_dir_path, err
            )
        })
    }
}

fn copy_roc_platform_if_not_exists(
    orig_platform_path: &Path,
    project_platform_path: &Path,
    project_dir_path: &Path,
) {
    if !orig_platform_path.exists() && !project_platform_path.exists() {
        panic!(
            r#"No roc file path was passed to the editor, so I wanted to create a new roc project but I could not find the platform at {:?}.
            Are you at the root of the roc repository?
            My current directory is: {:?}"#,
            orig_platform_path,
            env::current_dir()
        );
    } else if !project_platform_path.exists() {
        copy(orig_platform_path, project_dir_path, &CopyOptions::new()).unwrap_or_else(|err|{
            panic!(r#"No roc file path was passed to the editor, so I wanted to create a new roc project and roc projects require a platform,
            I tried to copy the platform at {:?} to {:?} but it failed: {}"#,
            orig_platform_path,
            project_platform_path,
            err
        )
        });
    }
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
