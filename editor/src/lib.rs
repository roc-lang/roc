use crate::text_state::handle_text_input;
use core::ptr::read;
use gfx_hal::{
    adapter::{Adapter, PhysicalDevice},
    buffer::Usage as BufferUsage,
    command::{ClearColor, ClearValue, CommandBuffer, CommandBufferFlags, Level, SubpassContents},
    device::Device,
    format::{Aspects, ChannelType, Format},
    image::{Extent, Layout, SubresourceRange},
    memory::{Properties, Requirements, Segment},
    pass::{Attachment, AttachmentLoadOp, AttachmentOps, AttachmentStoreOp, Subpass, SubpassDesc},
    pool::{CommandPool, CommandPoolCreateFlags},
    pso::{
        BlendState, ColorBlendDesc, ColorMask, EntryPoint, Face, GraphicsPipelineDesc,
        GraphicsShaderSet, PipelineStage, Primitive, Rasterizer, ShaderStageFlags, Specialization,
        Viewport,
    },
    queue::{CommandQueue, QueueFamily, Submission},
    window::{Extent2D, PresentMode, PresentationSurface, Surface, SwapchainConfig},
    Backend, Instance, MemoryTypeId,
};
use glsl_to_spirv::ShaderType;
use glyph_brush::ab_glyph::{point, Rect};
use std::borrow::Borrow;
use std::io;
use std::marker::PhantomData;
use std::mem::{size_of, ManuallyDrop};
use std::path::Path;
use winit::event::ModifiersState;

pub mod text_state;

/// The editor is actually launched from the CLI if you pass it zero arguments,
/// or if you provide it 1 or more files or directories to open on launch.
pub fn launch(_filepaths: &[&Path]) -> io::Result<()> {
    // TODO do any initialization here

    run_event_loop().expect("Error running event loop");

    Ok(())
}

use winit::{event_loop::EventLoop, window::WindowBuilder};

struct Resources<B: Backend> {
    instance: B::Instance,
    surface: B::Surface,
    device: B::Device,
    render_passes: Vec<B::RenderPass>,
    pipeline_layouts: Vec<B::PipelineLayout>,
    pipelines: Vec<B::GraphicsPipeline>,
    command_pool: B::CommandPool,
    submission_complete_fence: B::Fence,
    rendering_complete_semaphore: B::Semaphore,
    vertices: BufferBundle<B, B::Device>,
    indexes: BufferBundle<B, B::Device>,
}

struct ResourceHolder<B: gfx_hal::Backend>(ManuallyDrop<Resources<B>>);

impl<B: gfx_hal::Backend> Drop for ResourceHolder<B> {
    fn drop(&mut self) {
        unsafe {
            let Resources {
                instance,
                mut surface,
                device,
                command_pool,
                render_passes,
                pipeline_layouts,
                pipelines,
                indexes,
                vertices,
                submission_complete_fence,
                rendering_complete_semaphore,
            } = ManuallyDrop::take(&mut self.0);

            device.destroy_semaphore(rendering_complete_semaphore);
            device.destroy_fence(submission_complete_fence);
            for pipeline in pipelines {
                device.destroy_graphics_pipeline(pipeline);
            }
            for pipeline_layout in pipeline_layouts {
                device.destroy_pipeline_layout(pipeline_layout);
            }
            for render_pass in render_passes {
                device.destroy_render_pass(render_pass);
            }
            device.destroy_command_pool(command_pool);
            surface.unconfigure_swapchain(&device);
            instance.destroy_surface(surface);
        }
    }
}

pub const VERTEX_SOURCE: &str = "#version 450
layout (location = 0) in vec2 position;
layout (location = 1) in vec3 color;
layout (location = 2) in vec2 vert_uv;
layout (location = 0) out gl_PerVertex {
  vec4 gl_Position;
};
layout (location = 1) out vec3 frag_color;
layout (location = 2) out vec2 frag_uv;
void main()
{
  gl_Position = vec4(position, 0.0, 1.0);
  frag_color = color;
  frag_uv = vert_uv;
}";

pub const FRAGMENT_SOURCE: &str = "#version 450
layout (push_constant) uniform PushConsts {
  float time;
} push;
layout(set = 0, binding = 0) uniform texture2D tex;
layout(set = 0, binding = 1) uniform sampler samp;
layout (location = 1) in vec3 frag_color;
layout (location = 2) in vec2 frag_uv;
layout (location = 0) out vec4 color;
void main()
{
  float time01 = -0.9 * abs(sin(push.time * 0.7)) + 0.9;
  vec4 tex_color = texture(sampler2D(tex, samp), frag_uv);
  color = mix(tex_color, vec4(frag_color, 1.0), time01);
}";

pub static CREATURE_BYTES: &[u8] = include_bytes!("../creature.png");

#[derive(Debug, Clone, Copy)]
pub struct Quad {
    pub x: f32,
    pub y: f32,
    pub w: f32,
    pub h: f32,
}
impl Quad {
    pub fn vertex_attributes(self) -> [f32; 4 * (2 + 3 + 2)] {
        let x = self.x;
        let y = self.y;
        let w = self.w;
        let h = self.h;
        #[cfg_attr(rustfmt, rustfmt_skip)]
    [
    // X    Y    R    G    B                  U    V
      x  , y+h, 1.0, 0.0, 0.0, /* red     */ 0.0, 1.0, /* bottom left */
      x  , y  , 0.0, 1.0, 0.0, /* green   */ 0.0, 0.0, /* top left */
      x+w, y  , 0.0, 0.0, 1.0, /* blue    */ 1.0, 0.0, /* bottom right */
      x+w, y+h, 1.0, 0.0, 1.0, /* magenta */ 1.0, 1.0, /* top right */
    ]
    }
}

pub struct BufferBundle<B: Backend, D: Device<B>> {
    pub buffer: ManuallyDrop<B::Buffer>,
    pub requirements: Requirements,
    pub memory: ManuallyDrop<B::Memory>,
    pub phantom: PhantomData<D>,
}
impl<B: Backend, D: Device<B>> BufferBundle<B, D> {
    pub fn new(
        adapter: &Adapter<B>,
        device: &D,
        size: usize,
        usage: BufferUsage,
    ) -> Result<Self, &'static str> {
        unsafe {
            let mut buffer = device
                .create_buffer(size as u64, usage)
                .map_err(|_| "Couldn't create a buffer!")?;
            let requirements = device.get_buffer_requirements(&buffer);
            let memory_type_id = adapter
                .physical_device
                .memory_properties()
                .memory_types
                .iter()
                .enumerate()
                .find(|&(id, memory_type)| {
                    requirements.type_mask & (1 << id) != 0
                        && memory_type.properties.contains(Properties::CPU_VISIBLE)
                })
                .map(|(id, _)| MemoryTypeId(id))
                .ok_or("Couldn't find a memory type to support the buffer!")?;
            let memory = device
                .allocate_memory(memory_type_id, requirements.size)
                .map_err(|_| "Couldn't allocate buffer memory!")?;
            device
                .bind_buffer_memory(&memory, 0, &mut buffer)
                .map_err(|_| "Couldn't bind the buffer memory!")?;
            Ok(Self {
                buffer: ManuallyDrop::new(buffer),
                requirements,
                memory: ManuallyDrop::new(memory),
                phantom: PhantomData,
            })
        }
    }

    pub unsafe fn manually_drop(&self, device: &D) {
        device.destroy_buffer(ManuallyDrop::into_inner(read(&self.buffer)));
        device.free_memory(ManuallyDrop::into_inner(read(&self.memory)));
    }
}

pub struct LoadedImage<B: Backend> {
    pub image: ManuallyDrop<B::Image>,
    pub requirements: Requirements,
    pub memory: ManuallyDrop<B::Memory>,
    pub image_view: ManuallyDrop<B::ImageView>,
    pub sampler: ManuallyDrop<B::Sampler>,
    pub phantom: PhantomData<B::Device>,
}

impl<B: Backend> LoadedImage<B> {
    pub fn new(
        adapter: &Adapter<B>,
        device: &B::Device,
        command_pool: &mut B::CommandPool,
        command_queue: &mut B::CommandQueue,
        img: image::RgbaImage,
    ) -> Result<Self, &'static str> {
        unsafe {
            // 0. First we compute some memory related values.
            let pixel_size = size_of::<image::Rgba<u8>>();
            let row_size = pixel_size * (img.width() as usize);
            let limits = adapter.physical_device.limits();
            let row_alignment_mask = limits.optimal_buffer_copy_pitch_alignment as u32 - 1;
            let row_pitch = ((row_size as u32 + row_alignment_mask) & !row_alignment_mask) as usize;
            debug_assert!(row_pitch as usize >= row_size);

            // 1. make a staging buffer with enough memory for the image, and a
            //    transfer_src usage
            let required_bytes = row_pitch * img.height() as usize;
            let staging_bundle =
                BufferBundle::new(&adapter, device, required_bytes, BufferUsage::TRANSFER_SRC)?;

            // 2. use mapping writer to put the image data into that buffer
            unsafe {
                let mapping = device
                    .map_memory(&staging_bundle.memory, Segment::ALL)
                    .unwrap();

                for y in 0..img.height() as usize {
                    let row = &(*img)[y * row_size..(y + 1) * row_size];
                    let dest_base = y * row_pitch;

                    mapping[dest_base..dest_base + row.len()].copy_from_slice(row);
                }

                device.unmap_memory(&staging_bundle.memory);
            }

            // 3. Make an image with transfer_dst and SAMPLED usage
            let mut the_image = device
                .create_image(
                    gfx_hal::image::Kind::D2(img.width(), img.height(), 1, 1),
                    1,
                    Format::Rgba8Srgb,
                    gfx_hal::image::Tiling::Optimal,
                    gfx_hal::image::Usage::TRANSFER_DST | gfx_hal::image::Usage::SAMPLED,
                    gfx_hal::image::ViewCapabilities::empty(),
                )
                .map_err(|_| "Couldn't create the image!")?;

            // 4. allocate memory for the image and bind it
            let requirements = device.get_image_requirements(&the_image);
            let memory_type_id = adapter
                .physical_device
                .memory_properties()
                .memory_types
                .iter()
                .enumerate()
                .find(|&(id, memory_type)| {
                    // BIG NOTE: THIS IS DEVICE LOCAL NOT CPU VISIBLE
                    requirements.type_mask & (1 << id) != 0
                        && memory_type.properties.contains(Properties::DEVICE_LOCAL)
                })
                .map(|(id, _)| MemoryTypeId(id))
                .ok_or("Couldn't find a memory type to support the image!")?;
            let memory = device
                .allocate_memory(memory_type_id, requirements.size)
                .map_err(|_| "Couldn't allocate image memory!")?;
            device
                .bind_image_memory(&memory, 0, &mut the_image)
                .map_err(|_| "Couldn't bind the image memory!")?;

            // 5. create image view and sampler
            let image_view = device
                .create_image_view(
                    &the_image,
                    gfx_hal::image::ViewKind::D2,
                    Format::Rgba8Srgb,
                    gfx_hal::format::Swizzle::NO,
                    SubresourceRange {
                        aspects: Aspects::COLOR,
                        levels: 0..1,
                        layers: 0..1,
                    },
                )
                .map_err(|_| "Couldn't create the image view!")?;
            let sampler = device
                .create_sampler(gfx_hal::image::SamplerInfo::new(
                    gfx_hal::image::Filter::Nearest,
                    gfx_hal::image::WrapMode::Tile,
                ))
                .map_err(|_| "Couldn't create the sampler!")?;

            // 6. create a command buffer
            let mut cmd_buffer = command_pool.allocate_one(Level::Primary);
            cmd_buffer.begin_primary(CommandBufferFlags::ONE_TIME_SUBMIT);

            // 7. Use a pipeline barrier to transition the image from empty/undefined
            //    to TRANSFER_WRITE/TransferDstOptimal
            let image_barrier = gfx_hal::memory::Barrier::Image {
                states: (gfx_hal::image::Access::empty(), Layout::Undefined)
                    ..(
                        gfx_hal::image::Access::TRANSFER_WRITE,
                        Layout::TransferDstOptimal,
                    ),
                target: &the_image,
                families: None,
                range: SubresourceRange {
                    aspects: Aspects::COLOR,
                    levels: 0..1,
                    layers: 0..1,
                },
            };
            cmd_buffer.pipeline_barrier(
                PipelineStage::TOP_OF_PIPE..PipelineStage::TRANSFER,
                gfx_hal::memory::Dependencies::empty(),
                &[image_barrier],
            );

            // 8. perform copy from staging buffer to image
            cmd_buffer.copy_buffer_to_image(
                &staging_bundle.buffer,
                &the_image,
                Layout::TransferDstOptimal,
                &[gfx_hal::command::BufferImageCopy {
                    buffer_offset: 0,
                    buffer_width: (row_pitch / pixel_size) as u32,
                    buffer_height: img.height(),
                    image_layers: gfx_hal::image::SubresourceLayers {
                        aspects: Aspects::COLOR,
                        level: 0,
                        layers: 0..1,
                    },
                    image_offset: gfx_hal::image::Offset { x: 0, y: 0, z: 0 },
                    image_extent: gfx_hal::image::Extent {
                        width: img.width(),
                        height: img.height(),
                        depth: 1,
                    },
                }],
            );

            // 9. use pipeline barrier to transition the image to SHADER_READ access/
            //    ShaderReadOnlyOptimal layout
            let image_barrier = gfx_hal::memory::Barrier::Image {
                states: (
                    gfx_hal::image::Access::TRANSFER_WRITE,
                    Layout::TransferDstOptimal,
                )
                    ..(
                        gfx_hal::image::Access::SHADER_READ,
                        Layout::ShaderReadOnlyOptimal,
                    ),
                target: &the_image,
                families: None,
                range: SubresourceRange {
                    aspects: Aspects::COLOR,
                    levels: 0..1,
                    layers: 0..1,
                },
            };
            cmd_buffer.pipeline_barrier(
                PipelineStage::TRANSFER..PipelineStage::FRAGMENT_SHADER,
                gfx_hal::memory::Dependencies::empty(),
                &[image_barrier],
            );

            // 10. Submit the cmd buffer to queue and wait for it
            cmd_buffer.finish();
            let upload_fence = device
                .create_fence(false)
                .map_err(|_| "Couldn't create an upload fence!")?;
            command_queue.submit_without_semaphores(Some(&cmd_buffer), Some(&upload_fence));
            device
                .wait_for_fence(&upload_fence, core::u64::MAX)
                .map_err(|_| "Couldn't wait for the fence!")?;
            device.destroy_fence(upload_fence);

            // 11. Destroy the staging bundle and one shot buffer now that we're done
            staging_bundle.manually_drop(device);
            command_pool.free(Some(cmd_buffer));

            Ok(Self {
                image: ManuallyDrop::new(the_image),
                requirements,
                memory: ManuallyDrop::new(memory),
                image_view: ManuallyDrop::new(image_view),
                sampler: ManuallyDrop::new(sampler),
                phantom: PhantomData,
            })
        }
    }

    pub unsafe fn manually_drop(&self, device: &B::Device) {
        device.destroy_sampler(ManuallyDrop::into_inner(read(&self.sampler)));
        device.destroy_image_view(ManuallyDrop::into_inner(read(&self.image_view)));
        device.destroy_image(ManuallyDrop::into_inner(read(&self.image)));
        device.free_memory(ManuallyDrop::into_inner(read(&self.memory)));
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct PushConstants {
    color: [f32; 4],
    pos: [f32; 2],
    scale: [f32; 2],
}

pub type Vertex = [f32; 13];

#[inline]
pub fn to_vertex(
    glyph_brush::GlyphVertex {
        mut tex_coords,
        pixel_coords,
        bounds,
        extra,
    }: glyph_brush::GlyphVertex,
) -> Vertex {
    let gl_bounds = bounds;

    let mut gl_rect = Rect {
        min: point(pixel_coords.min.x as f32, pixel_coords.min.y as f32),
        max: point(pixel_coords.max.x as f32, pixel_coords.max.y as f32),
    };

    // handle overlapping bounds, modify uv_rect to preserve texture aspect
    if gl_rect.max.x > gl_bounds.max.x {
        let old_width = gl_rect.width();
        gl_rect.max.x = gl_bounds.max.x;
        tex_coords.max.x = tex_coords.min.x + tex_coords.width() * gl_rect.width() / old_width;
    }
    if gl_rect.min.x < gl_bounds.min.x {
        let old_width = gl_rect.width();
        gl_rect.min.x = gl_bounds.min.x;
        tex_coords.min.x = tex_coords.max.x - tex_coords.width() * gl_rect.width() / old_width;
    }
    if gl_rect.max.y > gl_bounds.max.y {
        let old_height = gl_rect.height();
        gl_rect.max.y = gl_bounds.max.y;
        tex_coords.max.y = tex_coords.min.y + tex_coords.height() * gl_rect.height() / old_height;
    }
    if gl_rect.min.y < gl_bounds.min.y {
        let old_height = gl_rect.height();
        gl_rect.min.y = gl_bounds.min.y;
        tex_coords.min.y = tex_coords.max.y - tex_coords.height() * gl_rect.height() / old_height;
    }

    [
        gl_rect.min.x,
        gl_rect.max.y,
        extra.z,
        gl_rect.max.x,
        gl_rect.min.y,
        tex_coords.min.x,
        tex_coords.max.y,
        tex_coords.max.x,
        tex_coords.min.y,
        extra.color[0],
        extra.color[1],
        extra.color[2],
        extra.color[3],
    ]
}

fn run_event_loop() -> Result<(), &'static str> {
    // TODO do a better window size
    const WINDOW_SIZE: [u32; 2] = [512, 512];

    let event_loop = EventLoop::new();
    let (logical_window_size, physical_window_size) = {
        use winit::dpi::{LogicalSize, PhysicalSize};

        let dpi = event_loop.primary_monitor().scale_factor();
        let logical: LogicalSize<u32> = WINDOW_SIZE.into();
        let physical: PhysicalSize<u32> = logical.to_physical(dpi);

        (logical, physical)
    };

    let mut surface_extent = Extent2D {
        width: physical_window_size.width,
        height: physical_window_size.height,
    };

    let window = WindowBuilder::new()
        .with_title("roc")
        .with_inner_size(logical_window_size)
        .build(&event_loop)
        .unwrap();

    let mut should_configure_swapchain = true;

    let (instance, surface, adapter) = {
        let instance = backend::Instance::create("roc_editor", 1).expect("Backend not supported");

        let surface = unsafe {
            instance
                .create_surface(&window)
                .expect("Failed to create surface for window")
        };

        let adapter = instance.enumerate_adapters().remove(0);

        (instance, surface, adapter)
    };

    let (device, mut queue_group) = {
        let queue_family = adapter
            .queue_families
            .iter()
            .find(|family| {
                surface.supports_queue_family(family) && family.queue_type().supports_graphics()
            })
            .expect("No compatible queue family found");

        let mut gpu = unsafe {
            adapter
                .physical_device
                .open(&[(queue_family, &[1.0])], gfx_hal::Features::empty())
                .expect("Failed to open device")
        };

        (gpu.device, gpu.queue_groups.pop().unwrap())
    };

    let (command_pool, mut command_buffer) = unsafe {
        let mut command_pool = device
            .create_command_pool(queue_group.family, CommandPoolCreateFlags::empty())
            .expect("Out of memory");

        let command_buffer = command_pool.allocate_one(Level::Primary);

        (command_pool, command_buffer)
    };

    let surface_color_format = {
        let supported_formats = surface
            .supported_formats(&adapter.physical_device)
            .unwrap_or_else(|| vec![]);

        let default_format = *supported_formats.get(0).unwrap_or(&Format::Rgba8Srgb);

        supported_formats
            .into_iter()
            .find(|format| format.base_format().1 == ChannelType::Srgb)
            .unwrap_or(default_format)
    };

    let render_pass = {
        let color_attachment = Attachment {
            format: Some(surface_color_format),
            samples: 1,
            ops: AttachmentOps::new(AttachmentLoadOp::Clear, AttachmentStoreOp::Store),
            stencil_ops: AttachmentOps::DONT_CARE,
            layouts: Layout::Undefined..Layout::Present,
        };

        let subpass = SubpassDesc {
            colors: &[(0, Layout::ColorAttachmentOptimal)],
            depth_stencil: None,
            inputs: &[],
            resolves: &[],
            preserves: &[],
        };

        unsafe {
            device
                .create_render_pass(&[color_attachment], &[subpass], &[])
                .expect("Out of memory")
        }
    };

    let pipeline_layout = unsafe {
        let push_constant_bytes = std::mem::size_of::<PushConstants>() as u32;

        device
            .create_pipeline_layout(&[], &[(ShaderStageFlags::VERTEX, 0..push_constant_bytes)])
            .expect("Out of memory")
    };

    let vertex_shader = include_str!("../shaders/triangle.vert");
    let fragment_shader = include_str!("../shaders/triangle.frag");

    /// Create a pipeline with the given layout and shaders.
    unsafe fn make_pipeline<B: gfx_hal::Backend>(
        device: &B::Device,
        render_pass: &B::RenderPass,
        pipeline_layout: &B::PipelineLayout,
        vertex_shader: &str,
        fragment_shader: &str,
    ) -> Result<B::GraphicsPipeline, &'static str> {
        let vertex_shader_module = device
            .create_shader_module(&compile_shader(vertex_shader, ShaderType::Vertex))
            .expect("Failed to create vertex shader module");

        let fragment_shader_module = device
            .create_shader_module(&compile_shader(fragment_shader, ShaderType::Fragment))
            .expect("Failed to create fragment shader module");

        let (vs_entry, fs_entry) = (
            EntryPoint {
                entry: "main",
                module: &vertex_shader_module,
                specialization: Specialization::default(),
            },
            EntryPoint {
                entry: "main",
                module: &fragment_shader_module,
                specialization: Specialization::default(),
            },
        );

        let shader_entries = GraphicsShaderSet {
            vertex: vs_entry,
            hull: None,
            domain: None,
            geometry: None,
            fragment: Some(fs_entry),
        };

        let mut pipeline_desc = GraphicsPipelineDesc::new(
            shader_entries,
            Primitive::TriangleList,
            Rasterizer {
                cull_face: Face::BACK,
                ..Rasterizer::FILL
            },
            pipeline_layout,
            Subpass {
                index: 0,
                main_pass: render_pass,
            },
        );

        pipeline_desc.blender.targets.push(ColorBlendDesc {
            mask: ColorMask::ALL,
            blend: Some(BlendState::ALPHA),
        });

        let pipeline = device
            .create_graphics_pipeline(&pipeline_desc, None)
            .expect("Failed to create graphics pipeline");

        device.destroy_shader_module(vertex_shader_module);
        device.destroy_shader_module(fragment_shader_module);

        Ok(pipeline)
    };

    let pipeline = unsafe {
        make_pipeline::<backend::Backend>(
            &device,
            &render_pass,
            &pipeline_layout,
            vertex_shader,
            fragment_shader,
        )
    }?;

    let submission_complete_fence = device.create_fence(true).expect("Out of memory");
    let rendering_complete_semaphore = device.create_semaphore().expect("Out of memory");

    const F32_XY_RGB_UV_QUAD: usize = size_of::<f32>() * (2 + 3 + 2) * 4;
    let vertices = BufferBundle::new(&adapter, &device, F32_XY_RGB_UV_QUAD, BufferUsage::VERTEX)?;

    const U16_QUAD_INDICES: usize = size_of::<u16>() * 2 * 3;
    let indexes = BufferBundle::new(&adapter, &device, U16_QUAD_INDICES, BufferUsage::INDEX)?;
    let mut resource_holder: ResourceHolder<backend::Backend> =
        ResourceHolder(ManuallyDrop::new(Resources {
            instance,
            surface,
            device,
            command_pool,
            render_passes: vec![render_pass],
            pipeline_layouts: vec![pipeline_layout],
            pipelines: vec![pipeline],
            submission_complete_fence,
            rendering_complete_semaphore,
            vertices,
            indexes,
        }));
    let is_animating = false;
    let mut text_state = String::new();
    let mut keyboard_modifiers = ModifiersState::empty();

    event_loop.run(move |event, _, control_flow| {
        use winit::event::{Event, WindowEvent};
        use winit::event_loop::ControlFlow;

        // TODO dynamically switch is_animating on/off depending on whether any
        // animations are running. Should conserve CPU usage and battery life!
        if is_animating {
            *control_flow = ControlFlow::Poll;
        } else {
            *control_flow = ControlFlow::Wait;
        }

        match event {
            Event::WindowEvent {
                event: window_event,
                ..
            } => match window_event {
                WindowEvent::CloseRequested => {
                    println!("✈️ Thank you for flying Roc Airlines!");
                    *control_flow = ControlFlow::Exit
                }
                WindowEvent::Resized(dims) => {
                    surface_extent = Extent2D {
                        width: dims.width,
                        height: dims.height,
                    };
                    should_configure_swapchain = true;
                }
                WindowEvent::KeyboardInput { input, .. } => {
                    if let Some(virtual_keycode) = input.virtual_keycode {
                        handle_text_input(
                            &mut text_state,
                            input.state,
                            virtual_keycode,
                            keyboard_modifiers,
                        );
                    }
                }
                WindowEvent::ScaleFactorChanged { new_inner_size, .. } => {
                    surface_extent = Extent2D {
                        width: new_inner_size.width,
                        height: new_inner_size.height,
                    };
                    should_configure_swapchain = true;
                }
                WindowEvent::ModifiersChanged(modifiers) => {
                    keyboard_modifiers = modifiers;
                }
                _ => (),
            },
            Event::MainEventsCleared => window.request_redraw(),
            Event::RedrawRequested(_) => {
                let res: &mut Resources<_> = &mut resource_holder.0;
                let render_pass = &res.render_passes[0];
                let pipeline_layout = &res.pipeline_layouts[0];
                let pipeline = &res.pipelines[0];

                unsafe {
                    // We refuse to wait more than a second, to avoid hanging.
                    let render_timeout_ns = 1_000_000_000;

                    res.device
                        .wait_for_fence(&res.submission_complete_fence, render_timeout_ns)
                        .expect("Out of memory or device lost");

                    res.device
                        .reset_fence(&res.submission_complete_fence)
                        .expect("Out of memory");

                    res.command_pool.reset(false);
                }

                if should_configure_swapchain {
                    let caps = res.surface.capabilities(&adapter.physical_device);

                    let mut swapchain_config =
                        SwapchainConfig::from_caps(&caps, surface_color_format, surface_extent)
                            .with_present_mode(PresentMode::IMMEDIATE);

                    // This seems to fix some fullscreen slowdown on macOS.
                    if caps.image_count.contains(&3) {
                        swapchain_config.image_count = 3;
                    }

                    surface_extent = swapchain_config.extent;

                    unsafe {
                        res.surface
                            .configure_swapchain(&res.device, swapchain_config)
                            .expect("Failed to configure swapchain");
                    };

                    should_configure_swapchain = false;
                }

                let surface_image = unsafe {
                    // We refuse to wait more than a second, to avoid hanging.
                    let acquire_timeout_ns = 1_000_000_000;

                    match res.surface.acquire_image(acquire_timeout_ns) {
                        Ok((image, _)) => image,
                        Err(_) => {
                            should_configure_swapchain = true;
                            return;
                        }
                    }
                };

                let framebuffer = unsafe {
                    res.device
                        .create_framebuffer(
                            render_pass,
                            vec![surface_image.borrow()],
                            Extent {
                                width: surface_extent.width,
                                height: surface_extent.height,
                                depth: 1,
                            },
                        )
                        .unwrap()
                };

                let viewport = {
                    Viewport {
                        rect: gfx_hal::pso::Rect {
                            x: 0,
                            y: 0,
                            w: surface_extent.width as i16,
                            h: surface_extent.height as i16,
                        },
                        depth: 0.0..1.0,
                    }
                };

                let triangles = text_state.chars().enumerate().flat_map(|(index, char)| {
                    if char == ' ' {
                        // Don't render spaces
                        None
                    } else {
                        Some(PushConstants {
                            color: [0.77254902, 0.658823529, 1.0, 0.5],
                            pos: [0.06 * index as f32, 0.0],
                            scale: [0.05, 0.05],
                        })
                    }
                });

                unsafe {
                    command_buffer.begin_primary(CommandBufferFlags::ONE_TIME_SUBMIT);

                    command_buffer.set_viewports(0, &[viewport.clone()]);
                    command_buffer.set_scissors(0, &[viewport.rect]);
                    command_buffer.begin_render_pass(
                        render_pass,
                        &framebuffer,
                        viewport.rect,
                        &[ClearValue {
                            color: ClearColor {
                                float32: [0.01, 0.01, 0.01, 1.0],
                            },
                        }],
                        SubpassContents::Inline,
                    );
                    command_buffer.bind_graphics_pipeline(pipeline);

                    for triangle in triangles {
                        command_buffer.push_graphics_constants(
                            pipeline_layout,
                            ShaderStageFlags::VERTEX,
                            0,
                            push_constant_bytes(&triangle),
                        );

                        command_buffer.draw(0..3, 0..1);
                    }

                    command_buffer.end_render_pass();
                    command_buffer.finish();
                }

                unsafe {
                    let submission = Submission {
                        command_buffers: vec![&command_buffer],
                        wait_semaphores: None,
                        signal_semaphores: vec![&res.rendering_complete_semaphore],
                    };

                    queue_group.queues[0].submit(submission, Some(&res.submission_complete_fence));
                    let result = queue_group.queues[0].present_surface(
                        &mut res.surface,
                        surface_image,
                        Some(&res.rendering_complete_semaphore),
                    );

                    should_configure_swapchain |= result.is_err();

                    res.device.destroy_framebuffer(framebuffer);
                }
            }
            _ => (),
        }
    });
}

/// Compile some GLSL shader source to SPIR-V.
/// TODO do this at build time - possibly in CI only
fn compile_shader(glsl: &str, shader_type: ShaderType) -> Vec<u32> {
    use std::io::{Cursor, Read};

    let mut compiled_file =
        glsl_to_spirv::compile(glsl, shader_type).expect("Failed to compile shader");

    let mut spirv_bytes = vec![];
    compiled_file.read_to_end(&mut spirv_bytes).unwrap();

    gfx_hal::pso::read_spirv(Cursor::new(&spirv_bytes)).expect("Invalid SPIR-V")
}

/// Returns a view of a struct as a slice of `u32`s.
unsafe fn push_constant_bytes<T>(push_constants: &T) -> &[u32] {
    let size_in_bytes = std::mem::size_of::<T>();
    let size_in_u32s = size_in_bytes / std::mem::size_of::<u32>();
    let start_ptr = push_constants as *const T as *const u32;
    std::slice::from_raw_parts(start_ptr, size_in_u32s)
}

// TEXT

pub fn compile_shader(src: &str, ty: GLenum) -> Res<GLuint> {
    let shader;
    unsafe {
        shader = gl::CreateShader(ty);
        // Attempt to compile the shader
        let c_str = CString::new(src.as_bytes())?;
        gl::ShaderSource(shader, 1, &c_str.as_ptr(), ptr::null());
        gl::CompileShader(shader);

        // Get the compile status
        let mut status = GLint::from(gl::FALSE);
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut status);

        // Fail on error
        if status != GLint::from(gl::TRUE) {
            let mut len = 0;
            gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut len);
            let mut buf = Vec::with_capacity(len as usize);
            buf.set_len((len as usize) - 1); // subtract 1 to skip the trailing null character
            gl::GetShaderInfoLog(
                shader,
                len,
                ptr::null_mut(),
                buf.as_mut_ptr() as *mut GLchar,
            );
            return Err(str::from_utf8(&buf)?.into());
        }
    }
    Ok(shader)
}

pub struct GlGlyphTexture {
    pub name: usize,
}

impl GlGlyphTexture {
    pub fn new((width, height): (u32, u32)) -> Self {
        let mut name = 0;
        unsafe {
            // Create a texture for the glyphs
            // The texture holds 1 byte per pixel as alpha data
            gl::PixelStorei(gl::UNPACK_ALIGNMENT, 1);
            gl::GenTextures(1, &mut name);
            gl::BindTexture(gl::TEXTURE_2D, name);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as _);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as _);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as _);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as _);
            gl::TexImage2D(
                gl::TEXTURE_2D,
                0,
                gl::RED as _,
                width as _,
                height as _,
                0,
                gl::RED,
                gl::UNSIGNED_BYTE,
                ptr::null(),
            );
            gl_assert_ok!();

            Self { name }
        }
    }

    pub fn clear(&self) {
        unsafe {
            gl::BindTexture(gl::TEXTURE_2D, self.name);
            gl::ClearTexImage(
                self.name,
                0,
                gl::RED,
                gl::UNSIGNED_BYTE,
                [12_u8].as_ptr() as _,
            );
            gl_assert_ok!();
        }
    }
}

impl Drop for GlGlyphTexture {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteTextures(1, &self.name);
        }
    }
}

pub struct GlTextPipe {
    shaders: [GLuint; 2],
    program: GLuint,
    vao: GLuint,
    vbo: GLuint,
    transform_uniform: GLint,
    vertex_count: usize,
    vertex_buffer_len: usize,
}

impl GlTextPipe {
    pub fn new(window_size: glutin::dpi::PhysicalSize<u32>) -> Res<Self> {
        let (w, h) = (window_size.width as f32, window_size.height as f32);

        let vs = compile_shader(include_str!("shader/text.vs"), gl::VERTEX_SHADER)?;
        let fs = compile_shader(include_str!("shader/text.fs"), gl::FRAGMENT_SHADER)?;
        let program = link_program(vs, fs)?;

        let mut vao = 0;
        let mut vbo = 0;

        let transform_uniform = unsafe {
            // Create Vertex Array Object
            gl::GenVertexArrays(1, &mut vao);
            gl::BindVertexArray(vao);

            // Create a Vertex Buffer Object
            gl::GenBuffers(1, &mut vbo);
            gl::BindBuffer(gl::ARRAY_BUFFER, vbo);

            // Use shader program
            gl::UseProgram(program);
            gl::BindFragDataLocation(program, 0, CString::new("out_color")?.as_ptr());

            // Specify the layout of the vertex data
            let uniform = gl::GetUniformLocation(program, CString::new("transform")?.as_ptr());
            if uniform < 0 {
                return Err(format!("GetUniformLocation(\"transform\") -> {}", uniform).into());
            }
            let transform = ortho(0.0, w, 0.0, h, 1.0, -1.0);
            gl::UniformMatrix4fv(uniform, 1, 0, transform.as_ptr());

            let mut offset = 0;
            for (v_field, float_count) in &[
                ("left_top", 3),
                ("right_bottom", 2),
                ("tex_left_top", 2),
                ("tex_right_bottom", 2),
                ("color", 4),
            ] {
                let attr = gl::GetAttribLocation(program, CString::new(*v_field)?.as_ptr());
                if attr < 0 {
                    return Err(format!("{} GetAttribLocation -> {}", v_field, attr).into());
                }
                gl::VertexAttribPointer(
                    attr as _,
                    *float_count,
                    gl::FLOAT,
                    gl::FALSE as _,
                    mem::size_of::<Vertex>() as _,
                    offset as _,
                );
                gl::EnableVertexAttribArray(attr as _);
                gl::VertexAttribDivisor(attr as _, 1);

                offset += float_count * 4;
            }

            // Enabled alpha blending
            gl::Enable(gl::BLEND);
            gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
            // Use srgb for consistency with other examples
            gl::Enable(gl::FRAMEBUFFER_SRGB);
            gl::ClearColor(0.02, 0.02, 0.02, 1.0);
            gl_assert_ok!();

            uniform
        };

        Ok(Self {
            shaders: [vs, fs],
            program,
            vao,
            vbo,
            transform_uniform,
            vertex_count: 0,
            vertex_buffer_len: 0,
        })
    }

    pub fn upload_vertices(&mut self, vertices: &[Vertex]) {
        // Draw new vertices
        self.vertex_count = vertices.len();

        unsafe {
            gl::BindVertexArray(self.vao);
            gl::BindBuffer(gl::ARRAY_BUFFER, self.vbo);
            if self.vertex_buffer_len < self.vertex_count {
                gl::BufferData(
                    gl::ARRAY_BUFFER,
                    (self.vertex_count * mem::size_of::<Vertex>()) as GLsizeiptr,
                    vertices.as_ptr() as _,
                    gl::DYNAMIC_DRAW,
                );
                self.vertex_buffer_len = self.vertex_count;
            } else {
                gl::BufferSubData(
                    gl::ARRAY_BUFFER,
                    0,
                    (self.vertex_count * mem::size_of::<Vertex>()) as GLsizeiptr,
                    vertices.as_ptr() as _,
                );
            }
            gl_assert_ok!();
        }
    }

    pub fn update_geometry(&self, window_size: glutin::dpi::PhysicalSize<u32>) {
        let (w, h) = (window_size.width as f32, window_size.height as f32);
        let transform = ortho(0.0, w, 0.0, h, 1.0, -1.0);

        unsafe {
            gl::UseProgram(self.program);
            gl::UniformMatrix4fv(self.transform_uniform, 1, 0, transform.as_ptr());
            gl_assert_ok!();
        }
    }

    pub fn draw(&self) {
        unsafe {
            gl::UseProgram(self.program);
            gl::BindVertexArray(self.vao);
            gl::DrawArraysInstanced(gl::TRIANGLE_STRIP, 0, 4, self.vertex_count as _);
            gl_assert_ok!();
        }
    }
}

impl Drop for GlTextPipe {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteProgram(self.program);
            self.shaders.iter().for_each(|s| gl::DeleteShader(*s));
            gl::DeleteBuffers(1, &self.vbo);
            gl::DeleteVertexArrays(1, &self.vao);
        }
    }
}
