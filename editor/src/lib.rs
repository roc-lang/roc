#![warn(clippy::all, clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

// Inspired by:
// https://github.com/sotrh/learn-wgpu by Benjamin Hansen, licensed under the MIT license
// https://github.com/cloudhead/rgx by Alexis Sellier, licensed under the MIT license

// See this link to learn wgpu: https://sotrh.github.io/learn-wgpu/

use crate::buffer::create_rect_buffers;
use crate::text::{build_glyph_brush, Text};
use crate::vertex::Vertex;
use crate::rect::{Rect};
use crate::error::{EdResult, print_err};
use ortho::{init_ortho, update_ortho_buffer, OrthoResources};
use std::error::Error;
use std::io;
use std::path::Path;
use winit::event;
use winit::event::{Event, ModifiersState};
use winit::event_loop::ControlFlow;
use vec_result::{get_res};

pub mod ast;
mod buffer;
pub mod expr;
pub mod file;
mod keyboard_input;
mod ortho;
mod pattern;
pub mod pool;
mod rect;
mod scope;
pub mod text;
mod types;
mod util;
mod vertex;
mod colors;
pub mod error;
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

    let (rect_pipeline, ortho) = make_rect_pipeline(&gpu_device, &swap_chain_descr);

    let mut glyph_brush = build_glyph_brush(&gpu_device, render_format)?;

    let is_animating = true;
    let mut text_state = "aaaaaaaaa\nbbbbbbbbbb\ncccccccccc\ndddddddddd\neeeeeee\nffffffff\ngggggggg".to_owned();//String::new();
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
            //Close
            Event::WindowEvent {
                event: event::WindowEvent::CloseRequested,
                ..
            } => *control_flow = winit::event_loop::ControlFlow::Exit,
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
                    &ortho.buffer,
                    &cmd_queue,
                );
            }
            //Received Character
            Event::WindowEvent {
                event: event::WindowEvent::ReceivedCharacter(ch),
                ..
            } => {
                update_text_state(&mut text_state, &ch);
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

                let glyph_bounds_rects = queue_all_text(
                    &size,
                    &text_state,
                    &mut glyph_brush,
                );

                let selection_rects_res = create_selection_rects(1, 5, 4, 2, &glyph_bounds_rects);

                match selection_rects_res {
                    Ok(selection_rects) => 
                        if !selection_rects.is_empty() {
                            let rect_buffers = create_rect_buffers(
                                &gpu_device,
                                &mut encoder, 
                                &selection_rects,
                            );
        
                            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                                color_attachments: &[wgpu::RenderPassColorAttachmentDescriptor {
                                    attachment: &frame.view,
                                    resolve_target: None,
                                    ops: wgpu::Operations::default(),
                                }],
                                depth_stencil_attachment: None,
                            });            
        
                            render_pass.set_pipeline(&rect_pipeline);
                            render_pass.set_bind_group(0, &ortho.bind_group, &[]);
                            render_pass.set_vertex_buffer(0, rect_buffers.vertex_buffer.slice(..));
                            render_pass.set_index_buffer(rect_buffers.index_buffer.slice(..));
                            render_pass.draw_indexed(0..rect_buffers.num_rects, 0, 0..1);
        
                            drop(render_pass);
                        },
                    Err(e) => print_err(&e) //TODO draw error text on screen
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


fn create_selection_rects(
    start_line_indx: usize,
    pos_in_start_line: usize,
    stop_line_indx: usize,
    pos_in_stop_line: usize,
    glyph_bound_rects: &Vec<Vec<Rect>>
)  -> EdResult<Vec<Rect>> {
    //TODO assert start_line <= stop_line, if start_line == stop_line => pos_in_start_line <= pos_in_stop_line

    let mut all_rects = Vec::new();

    if start_line_indx == stop_line_indx {
        let start_glyph_rect = 
            get_res(
                pos_in_start_line,
                get_res(start_line_indx, glyph_bound_rects)?,
            )?;

        let stop_glyph_rect = 
            get_res(
                pos_in_stop_line,
                get_res(stop_line_indx, glyph_bound_rects)?
            )?;

        let top_left_coords =
            start_glyph_rect.top_left_coords;

        let height = start_glyph_rect.height;
        let width = (stop_glyph_rect.top_left_coords.x - start_glyph_rect.top_left_coords.x) + stop_glyph_rect.width;

        all_rects.push(
            Rect {
                top_left_coords,
                width,
                height,
                color: colors::WHITE
            }
        );

        Ok(all_rects)
    } else {
        // first line
        let start_line = get_res(start_line_indx, glyph_bound_rects)?;

        let start_glyph_rect = 
            get_res(
                pos_in_start_line,
                start_line
            )?;

        let start_line_last_glyph_rect = 
            get_res(
                start_line.len() - 1,
                start_line
            )?;

        let top_left_coords =
            start_glyph_rect.top_left_coords;

        let height = start_glyph_rect.height;
        let width = (start_line_last_glyph_rect.top_left_coords.x - start_glyph_rect.top_left_coords.x) + start_line_last_glyph_rect.width;

        all_rects.push(
            Rect {
                top_left_coords,
                width,
                height,
                color: colors::WHITE
            }
        );

        //middle lines
        let nr_mid_lines = (stop_line_indx - start_line_indx) - 1;
        let first_mid_line = start_line_indx + 1;

        for i in first_mid_line..(first_mid_line + nr_mid_lines)  {
            let mid_line = get_res(i, glyph_bound_rects)?;

            let mid_line_first_glyph_rect = 
                get_res(
                    0,
                    mid_line
                )?;

            let mid_line_last_glyph_rect = 
                get_res(
                    mid_line.len() - 1,
                    mid_line
                )?;

            let top_left_coords =
                mid_line_first_glyph_rect.top_left_coords;

            let height = mid_line_first_glyph_rect.height;
            let width = (mid_line_last_glyph_rect.top_left_coords.x - mid_line_first_glyph_rect.top_left_coords.x) + mid_line_last_glyph_rect.width;

            all_rects.push(
                Rect {
                    top_left_coords,
                    width,
                    height,
                    color: colors::WHITE
                }
            );
        }

        //last line
        let stop_line = get_res(stop_line_indx, glyph_bound_rects)?;

        let stop_line_first_glyph_rect = 
        get_res(
            0,
            stop_line
        )?;

        let stop_glyph_rect = 
            get_res(
                pos_in_stop_line,
                stop_line
            )?;

        let top_left_coords =
            stop_line_first_glyph_rect.top_left_coords;

        let height = stop_glyph_rect.height;
        let width = (stop_glyph_rect.top_left_coords.x - stop_line_first_glyph_rect.top_left_coords.x) + stop_glyph_rect.width;

        all_rects.push(
            Rect {
                top_left_coords,
                width,
                height,
                color: colors::WHITE
            }
        );
        
        Ok(all_rects)
    }
}

fn make_rect_pipeline(
    gpu_device: &wgpu::Device,
    swap_chain_descr: &wgpu::SwapChainDescriptor,
) -> (wgpu::RenderPipeline, OrthoResources) {
    let ortho = init_ortho(swap_chain_descr.width, swap_chain_descr.height, gpu_device);

    let pipeline_layout = gpu_device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        bind_group_layouts: &[&ortho.bind_group_layout],
        push_constant_ranges: &[],
        label: Some("Rectangle pipeline layout"),
    });
    let pipeline = create_render_pipeline(
        &gpu_device,
        &pipeline_layout,
        swap_chain_descr.format,
        &[Vertex::DESC],
        wgpu::include_spirv!("shaders/rect.vert.spv"),
        wgpu::include_spirv!("shaders/rect.frag.spv"),
    );

    (pipeline, ortho)
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
        label: Some("Render pipeline"),
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

// returns bounding boxes for every glyph
fn queue_all_text(
    size: &winit::dpi::PhysicalSize<u32>,
    text_state: &str,
    glyph_brush: &mut wgpu_glyph::GlyphBrush<()>,
) -> Vec<Vec<Rect>> {
    let area_bounds = (size.width as f32, size.height as f32).into();

    let main_label = Text {
        position: (30.0, 30.0).into(),
        area_bounds,
        color: (0.4666, 0.2, 1.0, 1.0).into(),
        text: String::from("Enter some text:"),
        size: 40.0,
        ..Default::default()
    };

    let code_text = Text {
        position: (30.0, 90.0).into(),
        area_bounds,
        color: (0.0, 0.05, 0.46, 1.0).into(),
        text: String::from(text_state),
        size: 40.0,
        ..Default::default()
    };

    text::queue_text_draw(&main_label, glyph_brush);

    text::queue_text_draw(&code_text, glyph_brush)
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
