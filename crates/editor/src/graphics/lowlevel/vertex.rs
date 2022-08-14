// Taken from https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen - license information can be found in the LEGAL_DETAILS
// file in the root directory of this distribution.
//
// Thank you, Benjamin!
use cgmath::Vector2;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Vertex {
    pub position: Vector2<f32>,
    pub color: [f32; 4],
}

// Safety: As defined, there is no padding
// the type is repr(C), and Vector2 is repr(C)
unsafe impl bytemuck::Pod for Vertex {}
unsafe impl bytemuck::Zeroable for Vertex {}

impl Vertex {
    pub const SIZE: wgpu::BufferAddress = std::mem::size_of::<Self>() as wgpu::BufferAddress;
    pub const DESC: wgpu::VertexBufferLayout<'static> = wgpu::VertexBufferLayout {
        array_stride: Self::SIZE,
        step_mode: wgpu::VertexStepMode::Vertex,
        attributes: &[
            // position
            wgpu::VertexAttribute {
                offset: 0,
                shader_location: 0,
                format: wgpu::VertexFormat::Float32x2,
            },
            // color
            wgpu::VertexAttribute {
                offset: std::mem::size_of::<[f32; 2]>() as wgpu::BufferAddress,
                shader_location: 1,
                format: wgpu::VertexFormat::Float32x4,
            },
        ],
    };
}
