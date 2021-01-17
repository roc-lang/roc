// Taken from https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen, licensed under the MIT license
use cgmath::Vector2;

#[derive(Copy, Clone)]
pub struct Vertex {
    #[allow(dead_code)]
    pub position: Vector2<f32>,
    pub color: [f32; 4],
}

unsafe impl bytemuck::Pod for Vertex {}
unsafe impl bytemuck::Zeroable for Vertex {}

impl Vertex {
    pub const SIZE: wgpu::BufferAddress = std::mem::size_of::<Self>() as wgpu::BufferAddress;
    pub const DESC: wgpu::VertexBufferDescriptor<'static> = wgpu::VertexBufferDescriptor {
        stride: Self::SIZE,
        step_mode: wgpu::InputStepMode::Vertex,
        attributes: &[
            // position
            wgpu::VertexAttributeDescriptor {
                offset: 0,
                shader_location: 0,
                format: wgpu::VertexFormat::Float2,
            },
            // color
            wgpu::VertexAttributeDescriptor {
                offset: std::mem::size_of::<[f32; 2]>() as wgpu::BufferAddress,
                shader_location: 1,
                format: wgpu::VertexFormat::Float4,
            },
        ],
    };
}
