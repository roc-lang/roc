use cgmath::{Matrix4, Ortho};
use wgpu::util::DeviceExt;
use wgpu::{
    BindGroup, BindGroupLayout, BindGroupLayoutDescriptor, BindGroupLayoutEntry, Buffer,
    ShaderStages,
};

// orthographic projection is used to transform pixel coords to the coordinate system used by wgpu

#[repr(C)]
#[derive(Debug, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
struct Uniforms {
    // We can't use cgmath with bytemuck directly so we'll have
    // to convert the Matrix4 into a 4x4 f32 array
    ortho: [[f32; 4]; 4],
}

impl Uniforms {
    fn new(w: u32, h: u32) -> Self {
        let ortho: Matrix4<f32> = Ortho::<f32> {
            left: 0.0,
            right: w as f32,
            bottom: h as f32,
            top: 0.0,
            near: -1.0,
            far: 1.0,
        }
        .into();
        Self {
            ortho: ortho.into(),
        }
    }
}

// update orthographic buffer according to new window size
pub fn update_ortho_buffer(
    inner_width: u32,
    inner_height: u32,
    gpu_device: &wgpu::Device,
    ortho_buffer: &Buffer,
    cmd_queue: &wgpu::Queue,
) {
    let new_uniforms = Uniforms::new(inner_width, inner_height);

    let new_ortho_buffer = gpu_device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: Some("Ortho uniform buffer"),
        contents: bytemuck::cast_slice(&[new_uniforms]),
        usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_SRC,
    });

    // get a command encoder for the current frame
    let mut encoder = gpu_device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
        label: Some("Resize"),
    });

    // overwrite the new buffer over the old one
    encoder.copy_buffer_to_buffer(
        &new_ortho_buffer,
        0,
        ortho_buffer,
        0,
        (std::mem::size_of::<Uniforms>() * vec![new_uniforms].as_slice().len())
            as wgpu::BufferAddress,
    );

    cmd_queue.submit(Some(encoder.finish()));
}

#[derive(Debug)]
pub struct OrthoResources {
    pub buffer: Buffer,
    pub bind_group_layout: BindGroupLayout,
    pub bind_group: BindGroup,
}

pub fn init_ortho(
    inner_width: u32,
    inner_height: u32,
    gpu_device: &wgpu::Device,
) -> OrthoResources {
    let uniforms = Uniforms::new(inner_width, inner_height);

    let ortho_buffer = gpu_device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: Some("Ortho uniform buffer"),
        contents: bytemuck::cast_slice(&[uniforms]),
        usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
    });

    // bind groups consist of extra resources that are provided to the shaders
    let ortho_bind_group_layout = gpu_device.create_bind_group_layout(&BindGroupLayoutDescriptor {
        entries: &[BindGroupLayoutEntry {
            binding: 0,
            visibility: ShaderStages::VERTEX,
            ty: wgpu::BindingType::Buffer {
                ty: wgpu::BufferBindingType::Uniform,
                has_dynamic_offset: false,
                min_binding_size: None,
            },
            count: None,
        }],
        label: Some("Ortho bind group layout"),
    });

    let ortho_bind_group = gpu_device.create_bind_group(&wgpu::BindGroupDescriptor {
        layout: &ortho_bind_group_layout,
        entries: &[wgpu::BindGroupEntry {
            binding: 0,
            resource: ortho_buffer.as_entire_binding(),
        }],
        label: Some("Ortho bind group"),
    });

    OrthoResources {
        buffer: ortho_buffer,
        bind_group_layout: ortho_bind_group_layout,
        bind_group: ortho_bind_group,
    }
}
