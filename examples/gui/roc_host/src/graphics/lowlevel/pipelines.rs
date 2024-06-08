use super::ortho::{init_ortho, OrthoResources};
use super::quad::Quad;
use super::vertex::Vertex;
use std::borrow::Cow;

pub struct RectResources {
    pub pipeline: wgpu::RenderPipeline,
    pub ortho: OrthoResources,
}

pub fn make_rect_pipeline(
    gpu_device: &wgpu::Device,
    surface_config: &wgpu::SurfaceConfiguration,
) -> RectResources {
    let ortho = init_ortho(surface_config.width, surface_config.height, gpu_device);

    let pipeline_layout = gpu_device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: None,
        bind_group_layouts: &[&ortho.bind_group_layout],
        push_constant_ranges: &[],
    });
    let pipeline = create_render_pipeline(
        gpu_device,
        &pipeline_layout,
        surface_config.format,
        &wgpu::ShaderModuleDescriptor {
            label: None,
            source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(include_str!("../shaders/quad.wgsl"))),
        },
    );

    RectResources { pipeline, ortho }
}

pub fn create_render_pipeline(
    device: &wgpu::Device,
    layout: &wgpu::PipelineLayout,
    color_format: wgpu::TextureFormat,
    shader_module_desc: &wgpu::ShaderModuleDescriptor,
) -> wgpu::RenderPipeline {
    let shader = device.create_shader_module(shader_module_desc);

    device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: Some("Render pipeline"),
        layout: Some(layout),
        vertex: wgpu::VertexState {
            module: &shader,
            entry_point: "vs_main",
            buffers: &[Vertex::DESC, Quad::DESC],
        },
        fragment: Some(wgpu::FragmentState {
            module: &shader,
            entry_point: "fs_main",
            targets: &[wgpu::ColorTargetState {
                format: color_format,
                blend: Some(wgpu::BlendState {
                    color: wgpu::BlendComponent {
                        operation: wgpu::BlendOperation::Add,
                        src_factor: wgpu::BlendFactor::SrcAlpha,
                        dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
                    },
                    alpha: wgpu::BlendComponent::REPLACE,
                }),
                write_mask: wgpu::ColorWrites::ALL,
            }],
        }),
        primitive: wgpu::PrimitiveState::default(),
        depth_stencil: None,
        multisample: wgpu::MultisampleState::default(),
        multiview: None,
    })
}
