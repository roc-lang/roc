#![warn(clippy::all, clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

// Inspired by:
// https://github.com/sotrh/learn-wgpu by Benjamin Hansen, licensed under the MIT license
// https://github.com/cloudhead/rgx by Alexis Sellier, licensed under the MIT license

// See this link to learn wgpu: https://sotrh.github.io/learn-wgpu/

extern crate pest;
#[cfg(test)]
#[macro_use]
extern crate pest_derive;

use crate::error::{print_err, EdResult};
use crate::graphics::colors::{CODE_COLOR, TXT_COLOR};
use crate::graphics::lowlevel::buffer::create_rect_buffers;
use crate::graphics::lowlevel::ortho::update_ortho_buffer;
use crate::graphics::lowlevel::pipelines;
use crate::graphics::primitives::text::{
    build_glyph_brush, example_code_glyph_rect, queue_text_draw, queue_code_text_draw, Text,
};
use crate::graphics::style::CODE_FONT_SIZE;
use crate::graphics::style::CODE_TXT_XY;
use crate::mvc::app_model::AppModel;
use crate::mvc::ed_model::EdModel;
use crate::mvc::{ed_model, ed_view, update};
use crate::resources::strings::NOTHING_OPENED;
use crate::vec_result::get_res;
use bumpalo::Bump;
use cgmath::Vector2;
use ed_model::Position;
use pipelines::RectResources;
use std::error::Error;
use std::io;
use std::path::Path;
use wgpu::{CommandEncoder, RenderPass, TextureView};
use wgpu_glyph::GlyphBrush;
use winit::dpi::PhysicalSize;
use winit::event;
use winit::event::{Event, ModifiersState};
use winit::event_loop::ControlFlow;

pub mod error;
pub mod graphics;
mod keyboard_input;
pub mod lang;
mod mvc;
mod resources;
mod selection;
mod text_buffer;
mod util;
mod vec_result;

/// The editor is actually launched from the CLI if you pass it zero arguments,
/// or if you provide it 1 or more files or directories to open on launch.
pub fn launch(filepaths: &[&Path]) -> io::Result<()> {
    //TODO support using multiple filepaths
    let first_path_opt = if !filepaths.is_empty() {
        match get_res(0, filepaths) {
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
    let event_loop = winit::event_loop::EventLoop::new();

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
                    features: wgpu::Features::empty(),
                    limits: wgpu::Limits::default(),
                    shader_validation: false,
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
        usage: wgpu::TextureUsage::OUTPUT_ATTACHMENT,
        format: render_format,
        width: size.width,
        height: size.height,
        //Immediate may cause tearing, change present_mode if this becomes a problem
        present_mode: wgpu::PresentMode::Immediate,
    };

    let mut swap_chain = gpu_device.create_swap_chain(&surface, &swap_chain_descr);

    let rect_resources = pipelines::make_rect_pipeline(&gpu_device, &swap_chain_descr);

    let mut glyph_brush = build_glyph_brush(&gpu_device, render_format)?;

    let is_animating = true;
    let ed_model_opt = if let Some(file_path) = file_path_opt {
        let ed_model_res = ed_model::init_model(file_path);

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
    } else {
        None
    };

    let mut app_model = AppModel { ed_model_opt };

    let mut keyboard_modifiers = ModifiersState::empty();

    let arena = Bump::new();

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
                        usage: wgpu::TextureUsage::OUTPUT_ATTACHMENT,
                        format: render_format,
                        width: size.width,
                        height: size.height,
                        //Immediate may cause tearing, change present_mode if this becomes a problem
                        present_mode: wgpu::PresentMode::Immediate,
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
                if let Err(e) = update::handle_new_char(&mut app_model, &ch) {
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
                            keyboard_input::handle_keydown(
                                input.state,
                                virtual_keycode,
                                keyboard_modifiers,
                                ed_model,
                            );
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

                if let Some(ed_model) = &app_model.ed_model_opt {
                    //TODO don't pass invisible lines
                    queue_editor_text(
                        &size,
                        &ed_model.text_buf.all_lines(&arena),
                        ed_model.caret_pos,
                        CODE_TXT_XY.into(),
                        &mut glyph_brush,
                    );
                } else {
                    queue_no_file_text(&size, NOTHING_OPENED, CODE_TXT_XY.into(), &mut glyph_brush);
                }

                match draw_all_rects(
                    &app_model.ed_model_opt,
                    &arena,
                    &mut encoder,
                    &frame.view,
                    &gpu_device,
                    &rect_resources,
                ) {
                    Ok(()) => (),
                    Err(e) => {
                        print_err(&e);
                        begin_render_pass(&mut encoder, &frame.view);
                    }
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
    })
}

fn draw_all_rects(
    ed_model_opt: &Option<EdModel>,
    arena: &Bump,
    encoder: &mut CommandEncoder,
    texture_view: &TextureView,
    gpu_device: &wgpu::Device,
    rect_resources: &RectResources,
) -> EdResult<()> {
    if let Some(ed_model) = ed_model_opt {
        let all_rects = ed_view::create_ed_rects(ed_model, arena)?;

        let rect_buffers = create_rect_buffers(gpu_device, encoder, &all_rects);

        let mut render_pass = begin_render_pass(encoder, texture_view);

        render_pass.set_pipeline(&rect_resources.pipeline);
        render_pass.set_bind_group(0, &rect_resources.ortho.bind_group, &[]);
        render_pass.set_vertex_buffer(0, rect_buffers.vertex_buffer.slice(..));
        render_pass.set_index_buffer(rect_buffers.index_buffer.slice(..));
        render_pass.draw_indexed(0..rect_buffers.num_rects, 0, 0..1);
    } else {
        // need to begin render pass to clear screen
        begin_render_pass(encoder, texture_view);
    }

    Ok(())
}

fn begin_render_pass<'a>(
    encoder: &'a mut CommandEncoder,
    texture_view: &'a TextureView,
) -> RenderPass<'a> {
    encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
        color_attachments: &[wgpu::RenderPassColorAttachmentDescriptor {
            attachment: texture_view,
            resolve_target: None,
            ops: wgpu::Operations {
                load: wgpu::LoadOp::Clear(wgpu::Color {
                    r: 0.1,
                    g: 0.2,
                    b: 0.3,
                    a: 1.0,
                }),
                store: true,
            },
        }],
        depth_stencil_attachment: None,
    })
}

// returns bounding boxes for every glyph
fn queue_editor_text(
    size: &PhysicalSize<u32>,
    editor_lines: &str,
    caret_pos: Position,
    code_coords: Vector2<f32>,
    glyph_brush: &mut GlyphBrush<()>,
) {
    let area_bounds = (size.width as f32, size.height as f32).into();

    let code_text = Text {
        position: code_coords,
        area_bounds,
        color: CODE_COLOR.into(),
        text: editor_lines.to_owned(),
        size: CODE_FONT_SIZE,
        ..Default::default()
    };

    let caret_pos_label = Text {
        position: ((size.width as f32) - 150.0, (size.height as f32) - 40.0).into(),
        area_bounds,
        color: TXT_COLOR.into(),
        text: format!("Ln {}, Col {}", caret_pos.line, caret_pos.column),
        size: 25.0,
        ..Default::default()
    };

    queue_text_draw(&caret_pos_label, glyph_brush);

    queue_code_text_draw(&code_text, glyph_brush);
}

fn queue_no_file_text(
    size: &PhysicalSize<u32>,
    text: &str,
    text_coords: Vector2<f32>,
    glyph_brush: &mut GlyphBrush<()>,
) {
    let area_bounds = (size.width as f32, size.height as f32).into();

    let code_text = Text {
        position: text_coords,
        area_bounds,
        color: CODE_COLOR.into(),
        text: text.to_owned(),
        size: CODE_FONT_SIZE,
        ..Default::default()
    };

    queue_text_draw(&code_text, glyph_brush);
}
