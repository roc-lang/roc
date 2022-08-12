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
    roc::{self, Bounds, RocElem, RocElemTag, RocEvent},
};
use cgmath::{Vector2, Vector4};
use glyph_brush::{GlyphCruncher, OwnedSection};
use pipelines::RectResources;
use std::{
    error::Error,
    time::{Duration, Instant},
};
use wgpu::{CommandEncoder, LoadOp, RenderPass, TextureView};
use wgpu_glyph::GlyphBrush;
use winit::{
    dpi::PhysicalSize,
    event,
    event::{ElementState, Event, ModifiersState, StartCause},
    event_loop::ControlFlow,
    platform::run_return::EventLoopExtRunReturn,
};

// Inspired by:
// https://github.com/sotrh/learn-wgpu by Benjamin Hansen, which is licensed under the MIT license
// https://github.com/cloudhead/rgx by Alexis Sellier, which is licensed under the MIT license
//
// See this link to learn wgpu: https://sotrh.github.io/learn-wgpu/

const TIME_BETWEEN_TICKS: Duration = Duration::new(0, 1000 / 60);

pub fn run_event_loop(title: &str, window_bounds: Bounds) -> Result<(), Box<dyn Error>> {
    let (mut model, mut elems) = roc::init_and_render(window_bounds);

    // Open window and create a surface
    let mut event_loop = winit::event_loop::EventLoop::new();

    let window = winit::window::WindowBuilder::new()
        .with_inner_size(PhysicalSize::new(window_bounds.width, window_bounds.height))
        .with_title(title)
        .build(&event_loop)
        .unwrap();

    macro_rules! update_and_rerender {
        ($event:expr) => {
            // TODO use (model, elems) =  ... once we've upgraded rust versions
            let pair = roc::update_and_render(model, $event);

            model = pair.0;
            elems = pair.1;

            window.request_redraw();
        };
    }

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
            If you're running this from inside nix, follow the instructions here to resolve this: https://github.com/roc-lang/roc/blob/main/BUILDING_FROM_SOURCE.md#editor
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
    let mut keyboard_modifiers = ModifiersState::empty();

    // Render loop
    let app_start_time = Instant::now();
    let mut next_tick = app_start_time + TIME_BETWEEN_TICKS;

    window.request_redraw();

    event_loop.run_return(|event, _, control_flow| {
        match event {
            // Close
            Event::WindowEvent {
                event: event::WindowEvent::CloseRequested,
                ..
            } => *control_flow = ControlFlow::Exit,
            // Resize
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

                update_and_rerender!(RocEvent::Resize(Bounds {
                    height: size.height as f32,
                    width: size.width as f32,
                }));
            }
            // Keyboard input
            Event::WindowEvent {
                event:
                    event::WindowEvent::KeyboardInput {
                        input:
                            event::KeyboardInput {
                                virtual_keycode: Some(keycode),
                                state: input_state,
                                ..
                            },
                        ..
                    },
                ..
            } => {
                let roc_event = match input_state {
                    ElementState::Pressed => RocEvent::KeyDown(keycode.into()),
                    ElementState::Released => RocEvent::KeyUp(keycode.into()),
                };

                model = roc::update(model, roc_event);
            }
            // Modifiers Changed
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

                for elem in elems.iter() {
                    let (_bounds, drawable) = to_drawable(
                        elem,
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
                }

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
            Event::NewEvents(StartCause::ResumeTimeReached {
                requested_resume, ..
            }) => {
                // Only run this logic if this is the tick we originally requested.
                if requested_resume == next_tick {
                    let now = Instant::now();

                    // Set a new next_tick *before* running update and rerender,
                    // so their runtime isn't factored into when we want to render next.
                    next_tick = now + TIME_BETWEEN_TICKS;

                    let tick = now.saturating_duration_since(app_start_time);

                    update_and_rerender!(RocEvent::Tick(tick));

                    *control_flow = winit::event_loop::ControlFlow::WaitUntil(next_tick);
                }
            }
            _ => {
                // Keep waiting until the next tick.
                *control_flow = winit::event_loop::ControlFlow::WaitUntil(next_tick);
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

#[derive(Clone, Debug)]
struct Drawable {
    pos: Vector2<f32>,
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
    draw(
        drawable.bounds,
        drawable.content,
        drawable.pos,
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
    }
}

/// focused_elem is the currently-focused element (or NULL if nothing has the focus)
fn to_drawable(
    elem: &RocElem,
    bounds: Bounds,
    glyph_brush: &mut GlyphBrush<()>,
) -> (Bounds, Drawable) {
    use RocElemTag::*;

    match elem.tag() {
        Rect => {
            let rect = unsafe { &elem.entry().rect };

            let bounds = Bounds {
                width: rect.width,
                height: rect.height,
            };

            let drawable = Drawable {
                pos: (rect.left, rect.top).into(),
                bounds,
                content: DrawableContent::FillRect {
                    color: rect.color,
                    border_width: 1.0,
                    border_color: rect.color,
                },
            };

            (bounds, drawable)
        }
        Text => {
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
                pos: (0.0, 0.0).into(), // TODO store the pos in Text and read it here
                bounds: text_bounds,
                content: DrawableContent::Text(section, offset),
            };

            (text_bounds, drawable)
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
