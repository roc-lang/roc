// Contains parts of https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen - license information can be found in the LEGAL_DETAILS
// file in the root directory of this distribution.
//
// Thank you, Benjamin!


// Contains parts of https://github.com/iced-rs/iced/blob/adce9e04213803bd775538efddf6e7908d1c605e/wgpu/src/shader/quad.wgsl
// By Héctor Ramón, Iced contributors Licensed under the MIT license.
// The license is included in the LEGAL_DETAILS file in the root directory of this distribution.

// Thank you Héctor Ramón and Iced contributors!

use std::mem;

use super::{vertex::Vertex, quad::Quad};
use crate::graphics::{colors::to_slice, primitives::rect::RectElt};
use wgpu::util::{ DeviceExt};

pub struct RectBuffers {
    pub vertex_buffer: wgpu::Buffer,
    pub index_buffer: wgpu::Buffer,
    pub quad_buffer: wgpu::Buffer,
}

pub const QUAD_INDICES: [u16; 6] = [0, 1, 2, 0, 2, 3];

const QUAD_VERTS: [Vertex; 4] = [
    Vertex {
        _position: [0.0, 0.0],
    },
    Vertex {
        _position: [1.0, 0.0],
    },
    Vertex {
        _position: [1.0, 1.0],
    },
    Vertex {
        _position: [0.0, 1.0],
    },
];

pub const MAX_QUADS: usize = 100_000;

pub fn create_rect_buffers(
    gpu_device: &wgpu::Device,
    cmd_encoder: &mut wgpu::CommandEncoder,
    rects: &[RectElt],
) -> RectBuffers {

    let vertex_buffer = gpu_device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: None,
        contents: bytemuck::cast_slice(&QUAD_VERTS),
        usage: wgpu::BufferUsages::VERTEX,
    });

    let index_buffer = gpu_device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: None,
        contents: bytemuck::cast_slice(&QUAD_INDICES),
        usage: wgpu::BufferUsages::INDEX,
    });

    let quad_buffer = gpu_device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("iced_wgpu::quad instance buffer"),
        size: mem::size_of::<Quad>() as u64 * MAX_QUADS as u64,
        usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
        mapped_at_creation: false,
    });

    
    let quads: Vec<Quad> = rects.iter().map(|rect| {to_quad(rect)}).collect();

    let buffer_size = (quads.len() as u64 ) * Quad::SIZE;

    let staging_buffer = gpu_device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: None,
        contents: bytemuck::cast_slice(&quads),
        usage: wgpu::BufferUsages::COPY_SRC,
    });

    cmd_encoder.copy_buffer_to_buffer(&staging_buffer, 0, &quad_buffer, 0, buffer_size);
    

    RectBuffers {
        vertex_buffer,
        index_buffer,
        quad_buffer,
    }
}

pub fn to_quad(rect_elt: &RectElt) -> Quad {
    Quad {
        pos: rect_elt.rect.top_left_coords.into(),
        width: rect_elt.rect.width,
        height: rect_elt.rect.height,
        color: to_slice(rect_elt.color),
        border_color: to_slice(rect_elt.border_color),
        border_width: rect_elt.border_width,
    }
}
