use palette::{FromColor, Hsv, Srgb};

pub type RgbaTup = (f32, f32, f32, f32);
pub const WHITE: RgbaTup = (1.0, 1.0, 1.0, 1.0);

pub fn to_slice((r, g, b, a): RgbaTup) -> [f32; 4] {
    [r, g, b, a]
}

pub fn from_hsb(hue: usize, saturation: usize, brightness: usize) -> RgbaTup {
    from_hsba(hue, saturation, brightness, 1.0)
}

pub fn from_hsba(hue: usize, saturation: usize, brightness: usize, alpha: f32) -> RgbaTup {
    let rgb = Srgb::from_color(Hsv::new(
        hue as f32,
        (saturation as f32) / 100.0,
        (brightness as f32) / 100.0,
    ));

    (rgb.red, rgb.green, rgb.blue, alpha)
}
