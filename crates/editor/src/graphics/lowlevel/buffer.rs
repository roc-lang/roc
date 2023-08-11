// Adapted from https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen - license information can be found in the LEGAL_DETAILS
// file in the root directory of this distribution.
//
// Thank you, Benjamin!
use super::vertex::Vertex;
use crate::graphics::colors::to_slice;
use crate::graphics::primitives::rect::Rect;
use wgpu::util::{BufferInitDescriptor, DeviceExt};

pub struct QuadBufferBuilder {
    vertex_data: Vec<Vertex>,
    index_data: Vec<u32>,
    current_quad: u32,
}

impl QuadBufferBuilder {
    pub fn new() -> Self {
        Self {
            vertex_data: Vec::new(),
            index_data: Vec::new(),
            current_quad: 0,
        }
    }

    pub fn push_rect(self, rect: &Rect) -> Self {
        let coords = rect.top_left_coords;
        self.push_quad(
            coords.x,
            coords.y,
            coords.x + rect.width,
            coords.y + rect.height,
            to_slice(rect.color),
        )
    }

    pub fn push_quad(
        mut self,
        min_x: f32,
        min_y: f32,
        max_x: f32,
        max_y: f32,
        color: [f32; 4],
    ) -> Self {
        self.vertex_data.extend([
            Vertex {
                position: (min_x, min_y).into(),
                color,
            },
            Vertex {
                position: (max_x, min_y).into(),
                color,
            },
            Vertex {
                position: (max_x, max_y).into(),
                color,
            },
            Vertex {
                position: (min_x, max_y).into(),
                color,
            },
        ]);
        self.index_data.extend([
            self.current_quad * 4,
            self.current_quad * 4 + 1,
            self.current_quad * 4 + 2,
            self.current_quad * 4,
            self.current_quad * 4 + 2,
            self.current_quad * 4 + 3,
        ]);
        self.current_quad += 1;
        self
    }

    pub fn build(self, device: &wgpu::Device) -> (StagingBuffer, StagingBuffer, u32) {
        (
            StagingBuffer::new(device, &self.vertex_data),
            StagingBuffer::new(device, &self.index_data),
            self.index_data.len() as u32,
        )
    }
}

impl Default for QuadBufferBuilder {
    fn default() -> Self {
        Self::new()
    }
}

pub struct RectBuffers {
    pub vertex_buffer: wgpu::Buffer,
    pub index_buffer: wgpu::Buffer,
    pub num_rects: u32,
}

pub fn create_rect_buffers(
    gpu_device: &wgpu::Device,
    encoder: &mut wgpu::CommandEncoder,
    rects: &[Rect],
) -> RectBuffers {
    let nr_of_rects = rects.len() as u64;

    let vertex_buffer = gpu_device.create_buffer(&wgpu::BufferDescriptor {
        label: None,
        size: Vertex::SIZE * 4 * nr_of_rects,
        usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
        mapped_at_creation: false,
    });

    let u32_size = std::mem::size_of::<u32>() as wgpu::BufferAddress;

    let index_buffer = gpu_device.create_buffer(&wgpu::BufferDescriptor {
        label: None,
        size: u32_size * 6 * nr_of_rects,
        usage: wgpu::BufferUsages::INDEX | wgpu::BufferUsages::COPY_DST,
        mapped_at_creation: false,
    });

    let num_rects = {
        let mut quad_buffer_builder = QuadBufferBuilder::new();
        for rect in rects {
            quad_buffer_builder = quad_buffer_builder.push_rect(rect);
        }

        let (stg_vertex, stg_index, num_indices) = quad_buffer_builder.build(gpu_device);

        stg_vertex.copy_to_buffer(encoder, &vertex_buffer);
        stg_index.copy_to_buffer(encoder, &index_buffer);
        num_indices
    };

    RectBuffers {
        vertex_buffer,
        index_buffer,
        num_rects,
    }
}

pub struct StagingBuffer {
    buffer: wgpu::Buffer,
    size: wgpu::BufferAddress,
}

impl StagingBuffer {
    pub fn new<T: bytemuck::Pod + Sized>(device: &wgpu::Device, data: &[T]) -> StagingBuffer {
        StagingBuffer {
            buffer: device.create_buffer_init(&BufferInitDescriptor {
                contents: bytemuck::cast_slice(data),
                usage: wgpu::BufferUsages::COPY_SRC,
                label: Some("Staging Buffer"),
            }),
            size: size_of_slice(data) as wgpu::BufferAddress,
        }
    }

    pub fn copy_to_buffer(&self, encoder: &mut wgpu::CommandEncoder, other: &wgpu::Buffer) {
        encoder.copy_buffer_to_buffer(&self.buffer, 0, other, 0, self.size)
    }
}

// Taken from https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen - license information can be found in the LEGAL_DETAILS
// file in the root directory of this distribution.
//
// Thank you, Benjamin!
pub fn size_of_slice<T: Sized>(slice: &[T]) -> usize {
    std::mem::size_of_val(slice)
}
