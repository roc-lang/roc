pub type ColorTup = (f32, f32, f32, f32);
pub const WHITE: ColorTup = (1.0, 1.0, 1.0, 1.0);
pub const BLACK: ColorTup = (0.0, 0.0, 0.0, 1.0);
pub const TXT_COLOR: ColorTup = (1.0, 1.0, 1.0, 1.0);
pub const CODE_COLOR: ColorTup = (0.21, 0.55, 0.83, 1.0);
pub const CARET_COLOR: ColorTup = WHITE;
pub const SELECT_COLOR: ColorTup = (0.45, 0.61, 1.0, 1.0);
pub const BG_COLOR: ColorTup = (0.11, 0.11, 0.13, 1.0);

pub fn to_wgpu_color((r, g, b, a): ColorTup) -> wgpu::Color {
    wgpu::Color {
        r: r as f64,
        g: g as f64,
        b: b as f64,
        a: a as f64,
    }
}

pub fn to_slice((r, g, b, a): ColorTup) -> [f32; 4] {
    [r, g, b, a]
}
