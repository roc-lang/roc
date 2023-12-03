use cgmath::Vector4;
use palette::{FromColor, Hsv, Srgb};

/// This order is optimized for what Roc will send
#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Default)]
pub struct Rgba {
    a: f32,
    b: f32,
    g: f32,
    r: f32,
}

impl Rgba {
    pub const WHITE: Self = Self::new(1.0, 1.0, 1.0, 1.0);

    pub const fn new(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self { r, g, b, a }
    }

    pub const fn to_array(self) -> [f32; 4] {
        [self.r, self.g, self.b, self.a]
    }

    pub fn from_hsb(hue: usize, saturation: usize, brightness: usize) -> Self {
        Self::from_hsba(hue, saturation, brightness, 1.0)
    }

    pub fn from_hsba(hue: usize, saturation: usize, brightness: usize, alpha: f32) -> Self {
        let rgb = Srgb::from_color(Hsv::new(
            hue as f32,
            (saturation as f32) / 100.0,
            (brightness as f32) / 100.0,
        ));

        Self::new(rgb.red, rgb.green, rgb.blue, alpha)
    }
}

impl From<Rgba> for [f32; 4] {
    fn from(rgba: Rgba) -> Self {
        rgba.to_array()
    }
}

impl From<Rgba> for Vector4<f32> {
    fn from(rgba: Rgba) -> Self {
        Vector4::new(rgba.r, rgba.b, rgba.g, rgba.a)
    }
}
