// Inspired by https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen - license information can be found in the LEGAL_DETAILS
// file in the root directory of this distribution.
//
// Thank you, Benjamin!

// Inspired by https://github.com/iced-rs/iced/blob/adce9e04213803bd775538efddf6e7908d1c605e/wgpu/src/shader/quad.wgsl
// By Héctor Ramón, Iced contributors Licensed under the MIT license.
// The license is included in the LEGAL_DETAILS file in the root directory of this distribution.

// Thank you Héctor Ramón and Iced contributors!
use bytemuck::{Pod, Zeroable};


#[repr(C)]
#[derive(Copy, Clone, Zeroable, Pod)]
pub struct Vertex {
    pub _position: [f32; 2],
}

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
        ],
    };
}
