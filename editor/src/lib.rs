#![warn(clippy::all, clippy::dbg_macro)]
// I'm skeptical that clippy:large_enum_variant is a good lint to have globally enabled.
//
// It warns about a performance problem where the only quick remediation is
// to allocate more on the heap, which has lots of tradeoffs - including making it
// long-term unclear which allocations *need* to happen for compilation's sake
// (e.g. recursive structures) versus those which were only added to appease clippy.
//
// Effectively optimizing data structure memory layout isn't a quick fix,
// and encouraging shortcuts here creates bad incentives. I would rather temporarily
// re-enable this when working on performance optimizations than have it block PRs.
#![allow(clippy::large_enum_variant)]

// Inspired by https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen, licensed under the MIT license

// See this link to learn wgpu: https://sotrh.github.io/learn-wgpu/

use crate::buffer::QuadBufferBuilder;
use crate::rect::Rect;
use crate::vertex::Vertex;
use std::error::Error;
use std::io;
use std::path::Path;
use wgpu_glyph::{ab_glyph, GlyphBrushBuilder, Section, Text};
use winit::event;
use winit::event::{ElementState, Event, ModifiersState, VirtualKeyCode};
use winit::event_loop::ControlFlow;

pub mod ast;
pub mod bucket;
mod buffer;
pub mod file;
mod rect;
pub mod text_state;
mod util;
mod vertex;

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

    let rect_pipeline = make_rect_pipeline(&gpu_device, &swap_chain_descr);

    // Prepare glyph_brush
    let inconsolata =
        ab_glyph::FontArc::try_from_slice(include_bytes!("../Inconsolata-Regular.ttf"))?;

    let mut glyph_brush =
        GlyphBrushBuilder::using_font(inconsolata).build(&gpu_device, render_format);

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
            Event::WindowEvent {
                event: event::WindowEvent::CloseRequested,
                ..
            } => *control_flow = winit::event_loop::ControlFlow::Exit,
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
            }
            Event::WindowEvent {
                event: event::WindowEvent::ReceivedCharacter(ch),
                ..
            } => {
                update_text_state(&mut text_state, &ch);
            }
            Event::WindowEvent {
                event: event::WindowEvent::KeyboardInput { input, .. },
                ..
            } => {
                if let Some(virtual_keycode) = input.virtual_keycode {
                    handle_keydown(input.state, virtual_keycode, keyboard_modifiers);
                }
            }
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

                let rect_buffers = create_rect_buffers(&gpu_device, &mut encoder);

                let frame = swap_chain
                    .get_current_frame()
                    .expect("Failed to acquire next SwapChainFrame")
                    .output;

                let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                    color_attachments: &[wgpu::RenderPassColorAttachmentDescriptor {
                        attachment: &frame.view,
                        resolve_target: None,
                        ops: wgpu::Operations::default(),
                    }],
                    depth_stencil_attachment: None,
                });

                if rect_buffers.num_rects > 0 {
                    render_pass.set_vertex_buffer(0, rect_buffers.vertex_buffer.slice(..));
                    render_pass.set_index_buffer(rect_buffers.index_buffer.slice(..));
                    render_pass.set_pipeline(&rect_pipeline);
                    render_pass.draw_indexed(0..rect_buffers.num_rects, 0, 0..1);
                }

                drop(render_pass);

                draw_text(
                    &gpu_device,
                    &mut staging_belt,
                    &mut encoder,
                    &frame,
                    &size,
                    &text_state,
                    &mut glyph_brush,
                );

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

fn make_rect_pipeline(
    gpu_device: &wgpu::Device,
    swap_chain_descr: &wgpu::SwapChainDescriptor,
) -> wgpu::RenderPipeline {
    let pipeline_layout = gpu_device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        bind_group_layouts: &[],
        push_constant_ranges: &[],
        label: Some("Pipeline Layout"),
    });
    let pipeline = create_render_pipeline(
        &gpu_device,
        &pipeline_layout,
        swap_chain_descr.format,
        &[Vertex::DESC],
        wgpu::include_spirv!("shaders/rect.vert.spv"),
        wgpu::include_spirv!("shaders/rect.frag.spv"),
    );

    pipeline
}

fn create_render_pipeline(
    device: &wgpu::Device,
    layout: &wgpu::PipelineLayout,
    color_format: wgpu::TextureFormat,
    vertex_descs: &[wgpu::VertexBufferDescriptor],
    vs_src: wgpu::ShaderModuleSource,
    fs_src: wgpu::ShaderModuleSource,
) -> wgpu::RenderPipeline {
    let vs_module = device.create_shader_module(vs_src);
    let fs_module = device.create_shader_module(fs_src);

    device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: Some("Render Pipeline"),
        layout: Some(&layout),
        vertex_stage: wgpu::ProgrammableStageDescriptor {
            module: &vs_module,
            entry_point: "main",
        },
        fragment_stage: Some(wgpu::ProgrammableStageDescriptor {
            module: &fs_module,
            entry_point: "main",
        }),
        rasterization_state: None,
        primitive_topology: wgpu::PrimitiveTopology::TriangleList,
        color_states: &[wgpu::ColorStateDescriptor {
            format: color_format,
            color_blend: wgpu::BlendDescriptor::REPLACE,
            alpha_blend: wgpu::BlendDescriptor::REPLACE,
            write_mask: wgpu::ColorWrite::ALL,
        }],
        depth_stencil_state: None,
        sample_count: 1,
        sample_mask: !0,
        alpha_to_coverage_enabled: false,
        vertex_state: wgpu::VertexStateDescriptor {
            index_format: wgpu::IndexFormat::Uint32,
            vertex_buffers: vertex_descs,
        },
    })
}

struct RectBuffers {
    vertex_buffer: wgpu::Buffer,
    index_buffer: wgpu::Buffer,
    num_rects: u32,
}

fn create_rect_buffers(
    gpu_device: &wgpu::Device,
    encoder: &mut wgpu::CommandEncoder,
) -> RectBuffers {
    // Test Rectangles
    let test_rect_1 = Rect {
        top_left_coords: (-0.2, 0.6).into(),
        width: 0.1,
        height: 0.5,
        color: [0.0, 0.0, 1.0],
    };
    let test_rect_2 = Rect {
        top_left_coords: (-0.5, 0.0).into(),
        width: 0.5,
        height: 0.5,
        color: [0.0, 1.0, 0.0],
    };
    let test_rect_3 = Rect {
        top_left_coords: (0.3, 0.3).into(),
        width: 0.6,
        height: 0.1,
        color: [0.0, 0.0, 1.0],
    };

    let vertex_buffer = gpu_device.create_buffer(&wgpu::BufferDescriptor {
        label: None,
        size: Vertex::SIZE * 4 * 3,
        usage: wgpu::BufferUsage::VERTEX | wgpu::BufferUsage::COPY_DST,
        mapped_at_creation: false,
    });

    let u32_size = std::mem::size_of::<u32>() as wgpu::BufferAddress;

    let index_buffer = gpu_device.create_buffer(&wgpu::BufferDescriptor {
        label: None,
        size: u32_size * 6 * 3,
        usage: wgpu::BufferUsage::INDEX | wgpu::BufferUsage::COPY_DST,
        mapped_at_creation: false,
    });

    let num_rects = {
        let (stg_vertex, stg_index, num_indices) = QuadBufferBuilder::new()
            .push_rect(&test_rect_1)
            .push_rect(&test_rect_2)
            .push_rect(&test_rect_3)
            .build(&gpu_device);

        stg_vertex.copy_to_buffer(encoder, &vertex_buffer);
        stg_index.copy_to_buffer(encoder, &index_buffer);
        num_indices
    };

    RectBuffers {
        vertex_buffer,
        index_buffer,
        num_rects,
    }
}

fn draw_text(
    gpu_device: &wgpu::Device,
    staging_belt: &mut wgpu::util::StagingBelt,
    encoder: &mut wgpu::CommandEncoder,
    frame: &wgpu::SwapChainTexture,
    size: &winit::dpi::PhysicalSize<u32>,
    text_state: &str,
    glyph_brush: &mut wgpu_glyph::GlyphBrush<()>,
) {
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
        .draw_queued(
            gpu_device,
            staging_belt,
            encoder,
            &frame.view,
            size.width,
            size.height,
        )
        .expect("Draw queued");
}

fn update_text_state(text_state: &mut String, received_char: &char) {
    match received_char {
        '\u{8}' | '\u{7f}' => {
            // In Linux, we get a '\u{8}' when you press backspace,
            // but in macOS we get '\u{7f}'.
            text_state.pop();
        }
        '\u{e000}'..='\u{f8ff}' | '\u{f0000}'..='\u{ffffd}' | '\u{100000}'..='\u{10fffd}' => {
            // These are private use characters; ignore them.
            // See http://www.unicode.org/faq/private_use.html
        }
        _ => {
            text_state.push(*received_char);
        }
    }
}

fn handle_keydown(
    elem_state: ElementState,
    virtual_keycode: VirtualKeyCode,
    _modifiers: ModifiersState,
) {
    use winit::event::VirtualKeyCode::*;

    if let ElementState::Released = elem_state {
        return;
    }

    match virtual_keycode {
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
