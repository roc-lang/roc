use super::ortho::{init_ortho, OrthoResources};
use super::vertex::Vertex;

pub struct RectResources {
    pub pipeline: wgpu::RenderPipeline,
    pub ortho: OrthoResources,
}

pub fn make_rect_pipeline(
    gpu_device: &wgpu::Device,
    swap_chain_descr: &wgpu::SwapChainDescriptor,
) -> RectResources {
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
        wgpu::include_spirv!("../shaders/rect.vert.spv"),
        wgpu::include_spirv!("../shaders/rect.frag.spv"),
    );

    RectResources { pipeline, ortho }
}

pub fn create_render_pipeline(
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
