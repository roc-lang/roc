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

use crate::error::EdError::MissingGlyphDims;
use crate::error::{print_err, EdResult};
use crate::graphics::colors::{CARET_COLOR, CODE_COLOR, TXT_COLOR};
use crate::graphics::lowlevel::buffer::create_rect_buffers;
use crate::graphics::lowlevel::ortho::update_ortho_buffer;
use crate::graphics::lowlevel::pipelines;
use crate::graphics::primitives::rect::Rect;
use crate::graphics::primitives::text::{
    build_glyph_brush, example_code_glyph_rect, queue_text_draw, Text,
};
use crate::graphics::style::CODE_FONT_SIZE;
use crate::graphics::style::CODE_TXT_XY;
use crate::selection::create_selection_rects;
use crate::tea::ed_model::EdModel;
use crate::tea::{ed_model, update};
use bumpalo::collections::Vec as BumpVec;
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
mod selection;
mod tea;
mod util;
mod vec_result;

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
    let render_format = wgpu::TextureFormat::Bgra8UnormSrgb;
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
    let mut ed_model = ed_model::init_model();
    ed_model.glyph_dim_rect_opt = Some(example_code_glyph_rect(&mut glyph_brush));
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
                update::update_text_state(&mut ed_model, &ch);
            }
            //Keyboard Input
            Event::WindowEvent {
                event: event::WindowEvent::KeyboardInput { input, .. },
                ..
            } => {
                if let Some(virtual_keycode) = input.virtual_keycode {
                    keyboard_input::handle_keydown(
                        input.state,
                        virtual_keycode,
                        keyboard_modifiers,
                        &mut ed_model,
                    );
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

                queue_all_text(
                    &size,
                    &ed_model.lines,
                    ed_model.caret_pos,
                    CODE_TXT_XY.into(),
                    &mut glyph_brush,
                );

                match draw_all_rects(
                    &ed_model,
                    &ed_model.glyph_dim_rect_opt,
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
    ed_model: &EdModel,
    glyph_dim_rect_opt: &Option<Rect>,
    arena: &Bump,
    encoder: &mut CommandEncoder,
    texture_view: &TextureView,
    gpu_device: &wgpu::Device,
    rect_resources: &RectResources,
) -> EdResult<()> {
    let mut all_rects: BumpVec<Rect> = BumpVec::new_in(arena);

    let glyph_rect = if let Some(glyph_dim_rect) = glyph_dim_rect_opt {
        glyph_dim_rect
    } else {
        return Err(MissingGlyphDims {});
    };

    if let Some(selection) = ed_model.selection_opt {
        let mut selection_rects =
            create_selection_rects(selection, &ed_model.lines, glyph_rect, &arena)?;

        all_rects.append(&mut selection_rects);
    }

    all_rects.push(make_caret_rect(ed_model.caret_pos, glyph_rect)?);

    let rect_buffers = create_rect_buffers(gpu_device, encoder, &all_rects);

    let mut render_pass = begin_render_pass(encoder, texture_view);

    render_pass.set_pipeline(&rect_resources.pipeline);
    render_pass.set_bind_group(0, &rect_resources.ortho.bind_group, &[]);
    render_pass.set_vertex_buffer(0, rect_buffers.vertex_buffer.slice(..));
    render_pass.set_index_buffer(rect_buffers.index_buffer.slice(..));
    render_pass.draw_indexed(0..rect_buffers.num_rects, 0, 0..1);

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
                    r: 0.0,
                    g: 0.0,
                    b: 0.0,
                    a: 1.0,
                }),
                store: true,
            },
        }],
        depth_stencil_attachment: None,
    })
}

fn make_caret_rect(caret_pos: Position, glyph_dim_rect: &Rect) -> EdResult<Rect> {
    let caret_y =
        glyph_dim_rect.top_left_coords.y + (caret_pos.line as f32) * glyph_dim_rect.height;

    let caret_x =
        glyph_dim_rect.top_left_coords.x + glyph_dim_rect.width * (caret_pos.column as f32);

    Ok(Rect {
        top_left_coords: (caret_x, caret_y).into(),
        height: glyph_dim_rect.height,
        width: 2.0,
        color: CARET_COLOR,
    })
}

// returns bounding boxes for every glyph
fn queue_all_text(
    size: &PhysicalSize<u32>,
    lines: &[String],
    caret_pos: Position,
    code_coords: Vector2<f32>,
    glyph_brush: &mut GlyphBrush<()>,
) {
    let area_bounds = (size.width as f32, size.height as f32).into();

    let main_label = Text {
        position: (30.0, 30.0).into(),
        area_bounds,
        color: TXT_COLOR.into(),
        text: String::from("Enter some text:"),
        size: CODE_FONT_SIZE,
        ..Default::default()
    };

    let code_text = Text {
        position: code_coords,
        area_bounds,
        color: CODE_COLOR.into(),
        text: lines.join(""),
        size: CODE_FONT_SIZE,
        ..Default::default()
    };

    let caret_pos_label = Text {
        position: (30.0, (size.height as f32) - 45.0).into(),
        area_bounds,
        color: TXT_COLOR.into(),
        text: format!("Ln {}, Col {}", caret_pos.line, caret_pos.column),
        size: 30.0,
        ..Default::default()
    };

    queue_text_draw(&main_label, glyph_brush);

    queue_text_draw(&caret_pos_label, glyph_brush);

    queue_text_draw(&code_text, glyph_brush);
}
