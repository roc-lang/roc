

/// A polygon with 4 corners
#[derive(Copy, Clone)]
#[repr(C)]
pub struct Quad {
    pub pos: [f32; 2],
    pub width: f32,
    pub height: f32,
    pub color: [f32; 4],
    pub border_color: [f32; 4],
    pub border_width: f32,
}

// Safety: Pod's contract says the type must
// not have any padding, and must be repr(C).
// As currrently defined, Quad does not have
// any padding.
unsafe impl bytemuck::Pod for Quad {}
unsafe impl bytemuck::Zeroable for Quad {}

impl Quad {
    pub const SIZE: wgpu::BufferAddress = std::mem::size_of::<Self>() as wgpu::BufferAddress;
    pub const DESC: wgpu::VertexBufferLayout<'static> = wgpu::VertexBufferLayout {
        array_stride: Self::SIZE,
        step_mode: wgpu::VertexStepMode::Instance,
        attributes: &wgpu::vertex_attr_array!(
            1 => Float32x2,
            2 => Float32,
            3 => Float32,
            4 => Float32x4,
            5 => Float32x4,
            6 => Float32,
        ),
    };
}
