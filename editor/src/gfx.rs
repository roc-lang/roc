use arrayvec::ArrayVec;
use core::mem::ManuallyDrop;
use gfx_hal::{
    adapter::{Adapter, Gpu, PhysicalDevice},
    command::{ClearColor, ClearValue, CommandBuffer},
    device::Device,
    format::{Aspects, ChannelType, Format, Swizzle},
    image::{Extent, Layout, SubresourceRange, Usage, ViewKind},
    pass::{Attachment, AttachmentLoadOp, AttachmentOps, AttachmentStoreOp, SubpassDesc},
    pool::{CommandPool, CommandPoolCreateFlags},
    pso::{PipelineStage, Rect},
    queue::{
        family::{QueueFamily, QueueGroup},
        Submission,
    },
    window::{CompositeAlphaMode, Extent2D, PresentMode, Surface, Swapchain, SwapchainConfig},
    Backend, Instance,
};
use winit::window::Window;

pub struct GfxState<B: CommandBuffer<gfx_backend::Backend>, P: CommandPool<gfx_backend::Backend>> {
    current_frame: usize,
    frames_in_flight: usize,
    in_flight_fences: Vec<<gfx_backend::Backend as Backend>::Fence>,
    render_finished_semaphores: Vec<<gfx_backend::Backend as Backend>::Semaphore>,
    image_available_semaphores: Vec<<gfx_backend::Backend as Backend>::Semaphore>,
    command_buffers: Vec<B>,
    command_pool: ManuallyDrop<P>,
    framebuffers: Vec<<gfx_backend::Backend as Backend>::Framebuffer>,
    image_views: Vec<<gfx_backend::Backend as Backend>::ImageView>,
    render_pass: ManuallyDrop<<gfx_backend::Backend as Backend>::RenderPass>,
    render_area: Rect,
    queue_group: QueueGroup<gfx_backend::Backend>,
    swapchain: ManuallyDrop<<gfx_backend::Backend as Backend>::Swapchain>,
    device: ManuallyDrop<gfx_backend::Device>,
    adapter: Adapter<gfx_backend::Backend>,
    surface: <gfx_backend::Backend as Backend>::Surface,
    surface_extent: Extent2D,
    instance: ManuallyDrop<gfx_backend::Instance>,
}

impl<B: CommandBuffer<gfx_backend::Backend>, P: CommandPool<gfx_backend::Backend>> GfxState<B, P> {
    pub fn new(window: &Window, surface_extent: Extent2D) -> Result<Self, &'static str> {
        // Create an instance
        let instance = gfx_backend::Instance::create("roc", 1).expect("Backend not supported");

        // Create a surface
        let mut surface = unsafe {
            instance
                .create_surface(window)
                .expect("Failed to create surface for window")
        };

        // Select an adapter
        let adapter = instance
            .enumerate_adapters()
            .into_iter()
            .find(|adapter| {
                adapter.queue_families.iter().any(|family| {
                    surface.supports_queue_family(family) && family.queue_type().supports_graphics()
                })
            })
            .ok_or("Couldn't find a graphical Adapter!")?;

        // Open A Device and take out a QueueGroup
        let (device, queue_group) = {
            let queue_family = adapter
                .queue_families
                .iter()
                .find(|family| {
                    surface.supports_queue_family(family) && family.queue_type().supports_graphics()
                })
                .ok_or("Couldn't find a QueueFamily with graphics!")?;
            let Gpu {
                device,
                mut queue_groups,
            } = unsafe {
                adapter
                    .physical_device
                    .open(&[(queue_family, &[1.0])], gfx_hal::Features::empty())
                    .expect("Failed to open device")
            };
            let queue_group = queue_groups
                .pop()
                .ok_or("Couldn't take ownership of the QueueGroup!")?;

            if queue_group.queues.len() > 0 {
                Ok(())
            } else {
                Err("The QueueGroup did not have any CommandQueues available!")
            }?;

            (device, queue_group)
        };

        let (swapchain_config, format) = configure_swapchain(adapter, surface, surface_extent)?;

        let (swapchain, backbuffer) = unsafe {
            device
                .create_swapchain(&mut surface, swapchain_config, None)
                .map_err(|_| "Failed to create the swapchain!")?
        };

        let mut answer = Self {
            instance: ManuallyDrop::new(instance),
            surface,
            adapter,
            device: ManuallyDrop::new(device),
            queue_group,
            swapchain: ManuallyDrop::new(swapchain),
            render_area: extent.to_extent().rect(),
            render_pass: ManuallyDrop::new(render_pass),
            image_views,
            framebuffers,
            command_pool: ManuallyDrop::new(command_pool),
            command_buffers,
            image_available_semaphores,
            render_finished_semaphores,
            in_flight_fences,
            frames_in_flight,
            surface_extent,
            current_frame: 0,
        };

        // Create Our Sync Primitives
        let (image_available_semaphores, render_finished_semaphores, in_flight_fences) = {
            let mut image_available_semaphores: Vec<<gfx_backend::Backend as Backend>::Semaphore> =
                vec![];
            let mut render_finished_semaphores: Vec<<gfx_backend::Backend as Backend>::Semaphore> =
                vec![];
            let mut in_flight_fences: Vec<<gfx_backend::Backend as Backend>::Fence> = vec![];
            for _ in 0..frames_in_flight {
                in_flight_fences.push(
                    device
                        .create_fence(true)
                        .map_err(|_| "Could not create a fence!")?,
                );
                image_available_semaphores.push(
                    device
                        .create_semaphore()
                        .map_err(|_| "Could not create a semaphore!")?,
                );
                render_finished_semaphores.push(
                    device
                        .create_semaphore()
                        .map_err(|_| "Could not create a semaphore!")?,
                );
            }
            (
                image_available_semaphores,
                render_finished_semaphores,
                in_flight_fences,
            )
        };

        // Define A RenderPass
        let render_pass = {
            let color_attachment = Attachment {
                format: Some(format),
                samples: 1,
                ops: AttachmentOps {
                    load: AttachmentLoadOp::Clear,
                    store: AttachmentStoreOp::Store,
                },
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
                    .map_err(|_| "Couldn't create a render pass!")?
            }
        };

        // Create The ImageViews
        let image_views: Vec<_> = match backbuffer {
            gfx_backend::Backend::Images(images) => images
                .into_iter()
                .map(|image| unsafe {
                    device
                        .create_image_view(
                            &image,
                            ViewKind::D2,
                            format,
                            Swizzle::NO,
                            SubresourceRange {
                                aspects: Aspects::COLOR,
                                levels: 0..1,
                                layers: 0..1,
                            },
                        )
                        .map_err(|_| "Couldn't create the image_view for the image!")
                })
                .collect::<Result<Vec<_>, &str>>()?,
            gfx_backend::Backend::Framebuffer(_) => {
                unimplemented!("Can't handle framebuffer backbuffer!")
            }
        };

        // Create Our FrameBuffers
        let framebuffers: Vec<<gfx_backend::Backend as Backend>::Framebuffer> = {
            image_views
                .iter()
                .map(|image_view| unsafe {
                    device
                        .create_framebuffer(
                            &render_pass,
                            vec![image_view],
                            Extent {
                                width: extent.width as u32,
                                height: extent.height as u32,
                                depth: 1,
                            },
                        )
                        .map_err(|_| "Failed to create a framebuffer!")
                })
                .collect::<Result<Vec<_>, &str>>()?
        };

        // Create Our CommandPool
        let mut command_pool = unsafe {
            device
                .create_command_pool_typed(&queue_group, CommandPoolCreateFlags::RESET_INDIVIDUAL)
                .map_err(|_| "Could not create the raw command pool!")?
        };

        // Create Our CommandBuffers
        let command_buffers: Vec<_> = framebuffers
            .iter()
            .map(|_| command_pool.acquire_command_buffer())
            .collect();

        Ok(answer)
    }

    /// Draw a frame that's just cleared to the color specified.
    pub fn draw_clear_frame(&mut self, color: [f32; 4]) -> Result<(), &'static str> {
        // SETUP FOR THIS FRAME
        let image_available = &self.image_available_semaphores[self.current_frame];
        let render_finished = &self.render_finished_semaphores[self.current_frame];
        // Advance the frame _before_ we start using the `?` operator
        self.current_frame = (self.current_frame + 1) % self.frames_in_flight;

        let (i_u32, i_usize) = unsafe {
            let image_index = self
                .swapchain
                .acquire_image(core::u64::MAX)
                .map_err(|_| "Couldn't acquire an image from the swapchain!")?;
            (image_index, image_index as usize)
        };

        let flight_fence = &self.in_flight_fences[i_usize];
        unsafe {
            self.device
                .wait_for_fence(flight_fence, core::u64::MAX)
                .map_err(|_| "Failed to wait on the fence!")?;
            self.device
                .reset_fence(flight_fence)
                .map_err(|_| "Couldn't reset the fence!")?;
        }

        // RECORD COMMANDS
        unsafe {
            let buffer = &mut self.command_buffers[i_usize];
            let clear_values = [ClearValue::Color(ClearColor::Float(color))];
            buffer.begin(false);
            buffer.begin_render_pass_inline(
                &self.render_pass,
                &self.framebuffers[i_usize],
                self.render_area,
                clear_values.iter(),
            );
            buffer.finish();
        }

        // SUBMISSION AND PRESENT
        let command_buffers = &self.command_buffers[i_usize..=i_usize];
        let wait_semaphores: ArrayVec<[_; 1]> =
            [(image_available, PipelineStage::COLOR_ATTACHMENT_OUTPUT)].into();
        let signal_semaphores: ArrayVec<[_; 1]> = [render_finished].into();
        // yes, you have to write it twice like this. yes, it's silly.
        let present_wait_semaphores: ArrayVec<[_; 1]> = [render_finished].into();
        let submission = Submission {
            command_buffers,
            wait_semaphores,
            signal_semaphores,
        };
        let the_command_queue = &mut self.queue_group.queues[0];
        unsafe {
            the_command_queue.submit(submission, Some(flight_fence));
            self.swapchain
                .present(the_command_queue, i_u32, present_wait_semaphores)
                .map_err(|_| "Failed to present into the swapchain!")
        }
    }

    pub fn configure_swapchain(&mut self) -> Result<(), &'static str> {
        let (mut swapchain_config, _format) =
            configure_swapchain(self.adapter, self.surface, self.surface_extent)?;

        self.surface_extent = swapchain_config.extent;

        unsafe {
            self.surface
                .configure_swapchain(&self.device, swapchain_config)
                .expect("Failed to configure swapchain");
        };

        Ok(())
    }
}

impl<B: CommandBuffer<gfx_backend::Backend>, P: CommandPool<gfx_backend::Backend>> core::ops::Drop
    for GfxState<B, P>
{
    /// We have to clean up "leaf" elements before "root" elements. Basically, we
    /// clean up in reverse of the order that we created things.
    fn drop(&mut self) {
        let _ = self.device.wait_idle();
        unsafe {
            for fence in self.in_flight_fences.drain(..) {
                self.device.destroy_fence(fence)
            }
            for semaphore in self.render_finished_semaphores.drain(..) {
                self.device.destroy_semaphore(semaphore)
            }
            for semaphore in self.image_available_semaphores.drain(..) {
                self.device.destroy_semaphore(semaphore)
            }
            for framebuffer in self.framebuffers.drain(..) {
                self.device.destroy_framebuffer(framebuffer);
            }
            for image_view in self.image_views.drain(..) {
                self.device.destroy_image_view(image_view);
            }
            // LAST RESORT STYLE CODE, NOT TO BE IMITATED LIGHTLY
            use core::ptr::read;
            self.device
                .destroy_command_pool(ManuallyDrop::into_inner(read(&self.command_pool)));
            self.device
                .destroy_render_pass(ManuallyDrop::into_inner(read(&self.render_pass)));
            self.device
                .destroy_swapchain(ManuallyDrop::into_inner(read(&self.swapchain)));
            ManuallyDrop::drop(&mut self.device);
            ManuallyDrop::drop(&mut self.instance);
        }
    }
}

fn configure_swapchain<B: Backend, S: Surface<B>>(
    adapter: Adapter<B>,
    surface: S,
    surface_extent: Extent2D,
) -> Result<(SwapchainConfig, Format), &'static str> {
    let caps = surface.capabilities(&adapter.physical_device);
    let present_modes = caps.present_modes;
    let present_mode = *[
        PresentMode::MAILBOX,
        PresentMode::FIFO,
        PresentMode::RELAXED,
        PresentMode::IMMEDIATE,
    ]
    .iter()
    .find(|pm| present_modes.contains(**pm))
    .ok_or("No PresentMode values specified!")?;

    let composite_alphas = caps.composite_alpha_modes;
    let composite_alpha = *[
        CompositeAlphaMode::OPAQUE,
        CompositeAlphaMode::INHERIT,
        CompositeAlphaMode::PREMULTIPLIED,
        CompositeAlphaMode::POSTMULTIPLIED,
    ]
    .iter()
    .find(|ca| composite_alphas.contains(**ca))
    .ok_or("No CompositeAlpha values specified!")?;

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

    let extent = caps.extents.end();
    let image_count = if present_mode == PresentMode::MAILBOX {
        (caps.image_count.end() - 1).min((*caps.image_count.start()).max(3))
    } else {
        (caps.image_count.end() - 1).min((*caps.image_count.start()).max(2))
    };

    let image_layers = 1;
    let image_usage = if caps.usage.contains(Usage::COLOR_ATTACHMENT) {
        Usage::COLOR_ATTACHMENT
    } else {
        Err("The Surface isn't capable of supporting color!")?
    };

    let mut swapchain_config =
        SwapchainConfig::from_caps(&caps, surface_color_format, surface_extent);

    // This seems to fix some fullscreen slowdown on macOS, according to:
    // https://www.falseidolfactory.com/2020/04/01/intro-to-gfx-hal-part-1-drawing-a-triangle.html
    if caps.image_count.contains(&3) {
        swapchain_config.image_count = 3;
    }

    Ok((swapchain_config, surface_color_format))
}
