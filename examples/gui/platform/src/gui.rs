use crate::{
    graphics::{
        colors::Rgba,
        lowlevel::buffer::create_rect_buffers,
        lowlevel::{buffer::MAX_QUADS, ortho::update_ortho_buffer},
        lowlevel::{buffer::QUAD_INDICES, pipelines},
        primitives::{
            rect::{Rect, RectElt},
            text::build_glyph_brush,
        },
    },
    roc::{RocElem, RocElemTag},
};
use cgmath::{Vector2, Vector4};
use glyph_brush::OwnedSection;
use pipelines::RectResources;
use roc_std::RocStr;
use std::error::Error;
use wgpu::{CommandEncoder, LoadOp, RenderPass, TextureView};
use wgpu_glyph::{GlyphBrush, GlyphCruncher};
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

fn run_event_loop(title: &str, root: RocElem) -> Result<(), Box<dyn Error>> {
    // Open window and create a surface
    let mut event_loop = winit::event_loop::EventLoop::new();

    let window = winit::window::WindowBuilder::new()
        .with_inner_size(PhysicalSize::new(1900.0, 1000.0))
        .with_title(title)
        .build(&event_loop)
        .unwrap();

    let instance = wgpu::Instance::new(wgpu::Backends::all());

    let surface = unsafe { instance.create_surface(&window) };

    // Initialize GPU
    let (gpu_device, cmd_queue) = futures::executor::block_on(async {
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::HighPerformance,
                compatible_surface: Some(&surface),
                force_fallback_adapter: false,
            })
            .await
            .expect(r#"Request adapter
            If you're running this from inside nix, follow the instructions here to resolve this: https://github.com/rtfeldman/roc/blob/trunk/BUILDING_FROM_SOURCE.md#editor
            "#);

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

    let surface_config = wgpu::SurfaceConfiguration {
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
        format: render_format,
        width: size.width,
        height: size.height,
        present_mode: wgpu::PresentMode::Mailbox,
    };

    surface.configure(&gpu_device, &surface_config);

    let rect_resources = pipelines::make_rect_pipeline(&gpu_device, &surface_config);

    let mut glyph_brush = build_glyph_brush(&gpu_device, render_format)?;

    let is_animating = true;

    let mut keyboard_modifiers = ModifiersState::empty();

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
                        format: render_format,
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
                event: event::WindowEvent::ReceivedCharacter(_ch),
                ..
            } => {
                // let input_outcome_res =
                //     app_update::handle_new_char(&ch, &mut app_model, keyboard_modifiers);
                // if let Err(e) = input_outcome_res {
                //     print_err(&e)
                // } else if let Ok(InputOutcome::Ignored) = input_outcome_res {
                //     println!("Input '{}' ignored!", ch);
                // }
                todo!("TODO handle character input");
            }
            //Keyboard Input
            Event::WindowEvent {
                event: event::WindowEvent::KeyboardInput { input: _, .. },
                ..
            } => {
                // if let Some(virtual_keycode) = input.virtual_keycode {
                //     if let Some(ref mut ed_model) = app_model.ed_model_opt {
                //         if ed_model.has_focus {
                //             let keydown_res = keyboard_input::handle_keydown(
                //                 input.state,
                //                 virtual_keycode,
                //                 keyboard_modifiers,
                //                 &mut app_model,
                //             );

                //             if let Err(e) = keydown_res {
                //                 print_err(&e)
                //             }
                //         }
                //     }
                // }
                // TODO todo!("TODO handle keyboard input");
            }
            //Modifiers Changed
            Event::WindowEvent {
                event: event::WindowEvent::ModifiersChanged(modifiers),
                ..
            } => {
                keyboard_modifiers = modifiers;
            }
            Event::RedrawRequested { .. } => {
                // Get a command cmd_encoder for the current frame
                let mut cmd_encoder =
                    gpu_device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                        label: Some("Redraw"),
                    });

                let surface_texture = surface
                    .get_current_texture()
                    .expect("Failed to acquire next SwapChainTexture");

                let view = surface_texture
                    .texture
                    .create_view(&wgpu::TextureViewDescriptor::default());

                // for text_section in &rects_and_texts.text_sections_behind {
                //     let borrowed_text = text_section.to_borrowed();

                //     glyph_brush.queue(borrowed_text);
                // }

                // draw first layer of text
                // glyph_brush
                //     .draw_queued(
                //         &gpu_device,
                //         &mut staging_belt,
                //         &mut cmd_encoder,
                //         &view,
                //         size.width,
                //         size.height,
                //     )
                //     .expect("Failed to draw first layer of text.");

                // draw rects on top of first text layer
                // draw_rects(
                //     &rects_and_texts.rects_front,
                //     &mut cmd_encoder,
                //     &view,
                //     &gpu_device,
                //     &rect_resources,
                //     wgpu::LoadOp::Load,
                // );

                // TODO use with_capacity based on some heuristic
                let (_bounds, drawable) = to_drawable(
                    &root,
                    Bounds {
                        width: size.width as f32,
                        height: size.height as f32,
                    },
                    &mut glyph_brush,
                );

                process_drawable(
                    drawable,
                    &mut staging_belt,
                    &mut glyph_brush,
                    &mut cmd_encoder,
                    &view,
                    &gpu_device,
                    &rect_resources,
                    wgpu::LoadOp::Load,
                    Bounds {
                        width: size.width as f32,
                        height: size.height as f32,
                    },
                );

                // for text_section in &rects_and_texts.text_sections_front {
                //     let borrowed_text = text_section.to_borrowed();

                //     glyph_brush.queue(borrowed_text);
                // }

                // draw text
                // glyph_brush
                //     .draw_queued(
                //         &gpu_device,
                //         &mut staging_belt,
                //         &mut cmd_encoder,
                //         &view,
                //         size.width,
                //         size.height,
                //     )
                //     .expect("Failed to draw queued text.");

                staging_belt.finish();
                cmd_queue.submit(Some(cmd_encoder.finish()));
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

fn draw_rects(
    all_rects: &[RectElt],
    cmd_encoder: &mut CommandEncoder,
    texture_view: &TextureView,
    gpu_device: &wgpu::Device,
    rect_resources: &RectResources,
    load_op: LoadOp<wgpu::Color>,
) {
    let rect_buffers = create_rect_buffers(gpu_device, cmd_encoder, all_rects);

    let mut render_pass = begin_render_pass(cmd_encoder, texture_view, load_op);

    render_pass.set_pipeline(&rect_resources.pipeline);
    render_pass.set_bind_group(0, &rect_resources.ortho.bind_group, &[]);

    render_pass.set_vertex_buffer(0, rect_buffers.vertex_buffer.slice(..));
    render_pass.set_vertex_buffer(1, rect_buffers.quad_buffer.slice(..));

    render_pass.set_index_buffer(
        rect_buffers.index_buffer.slice(..),
        wgpu::IndexFormat::Uint16,
    );

    render_pass.draw_indexed(0..QUAD_INDICES.len() as u32, 0, 0..MAX_QUADS as u32);
}

fn begin_render_pass<'a>(
    cmd_encoder: &'a mut CommandEncoder,
    texture_view: &'a TextureView,
    load_op: LoadOp<wgpu::Color>,
) -> RenderPass<'a> {
    cmd_encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
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

pub fn render(title: RocStr, root: RocElem) {
    run_event_loop(title.as_str(), root).expect("Error running event loop");
}

#[derive(Copy, Clone, Debug, Default)]
struct Bounds {
    width: f32,
    height: f32,
}

#[derive(Clone, Debug)]
struct Drawable {
    bounds: Bounds,
    content: DrawableContent,
}

#[derive(Clone, Debug)]
enum DrawableContent {
    /// This stores an actual Section because an earlier step needs to know the bounds of
    /// the text, and making a Section is a convenient way to compute those bounds.
    Text(OwnedSection, Vector2<f32>),
    FillRect {
        color: Rgba,
        border_width: f32,
        border_color: Rgba,
    },
    Multi(Vec<Drawable>),
    Offset(Vec<(Vector2<f32>, Drawable)>),
}

fn process_drawable(
    drawable: Drawable,
    staging_belt: &mut wgpu::util::StagingBelt,
    glyph_brush: &mut GlyphBrush<()>,
    cmd_encoder: &mut CommandEncoder,
    texture_view: &TextureView,
    gpu_device: &wgpu::Device,
    rect_resources: &RectResources,
    load_op: LoadOp<wgpu::Color>,
    texture_size: Bounds,
) {
    // TODO iterate through drawables,
    // calculating a pos using offset,
    // calling draw and updating bounding boxes
    let pos: Vector2<f32> = (0.0, 0.0).into();

    draw(
        drawable.bounds,
        drawable.content,
        pos,
        staging_belt,
        glyph_brush,
        cmd_encoder,
        texture_view,
        gpu_device,
        rect_resources,
        load_op,
        texture_size,
    );
}

fn draw(
    bounds: Bounds,
    content: DrawableContent,
    pos: Vector2<f32>,
    staging_belt: &mut wgpu::util::StagingBelt,
    glyph_brush: &mut GlyphBrush<()>,
    cmd_encoder: &mut CommandEncoder,
    texture_view: &TextureView,
    gpu_device: &wgpu::Device,
    rect_resources: &RectResources,
    load_op: LoadOp<wgpu::Color>,
    texture_size: Bounds,
) {
    use DrawableContent::*;

    match content {
        Text(section, offset) => {
            glyph_brush.queue(section.with_screen_position(pos + offset).to_borrowed());

            glyph_brush
                .draw_queued(
                    gpu_device,
                    staging_belt,
                    cmd_encoder,
                    texture_view,
                    texture_size.width as u32, // TODO why do we make these be u32 and then cast to f32 in orthorgraphic_projection?
                    texture_size.height as u32,
                )
                .expect("Failed to draw text element");
        }
        FillRect {
            color,
            border_width,
            border_color,
        } => {
            // TODO store all these colors and things in FillRect
            let rect_elt = RectElt {
                rect: Rect {
                    pos,
                    width: bounds.width,
                    height: bounds.height,
                },
                color,
                border_width,
                border_color,
            };

            // TODO inline draw_rects into here!
            draw_rects(
                &[rect_elt],
                cmd_encoder,
                texture_view,
                gpu_device,
                rect_resources,
                load_op,
            );
        }
        Offset(children) => {
            for (offset, child) in children.into_iter() {
                draw(
                    child.bounds,
                    child.content,
                    pos + offset,
                    staging_belt,
                    glyph_brush,
                    cmd_encoder,
                    texture_view,
                    gpu_device,
                    rect_resources,
                    load_op,
                    texture_size,
                );
            }
        }
        Multi(children) => {
            for child in children.into_iter() {
                draw(
                    child.bounds,
                    child.content,
                    pos,
                    staging_belt,
                    glyph_brush,
                    cmd_encoder,
                    texture_view,
                    gpu_device,
                    rect_resources,
                    load_op,
                    texture_size,
                );
            }
        }
    }
}

fn to_drawable(
    elem: &RocElem,
    bounds: Bounds,
    glyph_brush: &mut GlyphBrush<()>,
) -> (Bounds, Drawable) {
    use RocElemTag::*;

    match elem.tag() {
        Button => {
            let button = unsafe { &elem.entry().button };
            let styles = button.styles;
            let (child_bounds, child_drawable) = to_drawable(&*button.child, bounds, glyph_brush);

            let button_drawable = Drawable {
                bounds: child_bounds,
                content: DrawableContent::FillRect {
                    color: styles.bg_color,
                    border_width: styles.border_width,
                    border_color: styles.border_color,
                },
            };

            let drawable = Drawable {
                bounds: child_bounds,
                content: DrawableContent::Multi(vec![button_drawable, child_drawable]),
            };

            (child_bounds, drawable)
        }
        Text => {
            // TODO let text color and font settings inherit from parent
            let text = unsafe { &elem.entry().text };
            let is_centered = true; // TODO don't hardcode this
            let layout = wgpu_glyph::Layout::default().h_align(if is_centered {
                wgpu_glyph::HorizontalAlign::Center
            } else {
                wgpu_glyph::HorizontalAlign::Left
            });

            let section = owned_section_from_str(text.as_str(), bounds, layout);

            // Calculate the bounds and offset by measuring glyphs
            let text_bounds;
            let offset;

            match glyph_brush.glyph_bounds(section.to_borrowed()) {
                Some(glyph_bounds) => {
                    text_bounds = Bounds {
                        width: glyph_bounds.max.x - glyph_bounds.min.x,
                        height: glyph_bounds.max.y - glyph_bounds.min.y,
                    };

                    offset = (-glyph_bounds.min.x, -glyph_bounds.min.y).into();
                }
                None => {
                    text_bounds = Bounds {
                        width: 0.0,
                        height: 0.0,
                    };

                    offset = (0.0, 0.0).into();
                }
            }

            let drawable = Drawable {
                bounds: text_bounds,
                content: DrawableContent::Text(section, offset),
            };

            (text_bounds, drawable)
        }
        Row => {
            let row = unsafe { &elem.entry().row_or_col };
            let mut final_bounds = Bounds::default();
            let mut offset: Vector2<f32> = (0.0, 0.0).into();
            let mut offset_entries = Vec::with_capacity(row.children.len());

            for child in row.children.as_slice().iter() {
                let (child_bounds, child_drawable) = to_drawable(&child, bounds, glyph_brush);

                offset_entries.push((offset, child_drawable));

                // Make sure the final height is enough to fit this child
                final_bounds.height = final_bounds.height.max(child_bounds.height);

                // Add the child's width to the final width
                final_bounds.width = final_bounds.width + child_bounds.width;

                // Offset the next child to make sure it appears after this one.
                offset.x += child_bounds.width;
            }

            (
                final_bounds,
                Drawable {
                    bounds: final_bounds,
                    content: DrawableContent::Offset(offset_entries),
                },
            )
        }
        Col => {
            let col = unsafe { &elem.entry().row_or_col };
            let mut final_bounds = Bounds::default();
            let mut offset: Vector2<f32> = (0.0, 0.0).into();
            let mut offset_entries = Vec::with_capacity(col.children.len());

            for child in col.children.as_slice().iter() {
                let (child_bounds, child_drawable) = to_drawable(&child, bounds, glyph_brush);

                offset_entries.push((offset, child_drawable));

                // Make sure the final width is enough to fit this child
                final_bounds.width = final_bounds.width.max(child_bounds.width);

                // Add the child's height to the final height
                final_bounds.height = final_bounds.height + child_bounds.height;

                // Offset the next child to make sure it appears after this one.
                offset.y += child_bounds.height;
            }

            (
                final_bounds,
                Drawable {
                    bounds: final_bounds,
                    content: DrawableContent::Offset(offset_entries),
                },
            )
        }
    }
}

fn owned_section_from_str(
    string: &str,
    bounds: Bounds,
    layout: wgpu_glyph::Layout<wgpu_glyph::BuiltInLineBreaker>,
) -> OwnedSection {
    // TODO don't hardcode any of this!
    let color = Rgba::WHITE;
    let size: f32 = 40.0;

    OwnedSection {
        bounds: (bounds.width, bounds.height),
        layout,
        ..OwnedSection::default()
    }
    .add_text(
        glyph_brush::OwnedText::new(string)
            .with_color(Vector4::from(color))
            .with_scale(size),
    )
}
