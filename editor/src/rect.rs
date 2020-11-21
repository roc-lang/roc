use crate::vertex::Vertex;

pub struct Rect {
    pub top: f32,
    pub left: f32,
    pub width: f32,
    pub height: f32,
    pub color: [f32; 3],
}

impl Rect {
    pub fn as_array(&self) -> [Vertex; 4] {
        [
            Vertex {
                position: [self.left, self.top, 0.0],
                color: self.color,
            },
            Vertex {
                position: [self.left + self.width, self.top, 0.0],
                color: self.color,
            },
            Vertex {
                position: [self.left + self.width, self.top - self.height, 0.0],
                color: self.color,
            },
            Vertex {
                position: [self.left, self.top - self.height, 0.0],
                color: self.color,
            },
        ]
    }

    // Currently broken - needs to be offset when additional rectangles are appended
    pub const INDEX_BUFFER: [u16; 6] = [0, 1, 3, 1, 2, 3];
}
