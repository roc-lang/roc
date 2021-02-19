pub type ColorTup = (f32, f32, f32, f32);
pub const WHITE: ColorTup = (1.0, 1.0, 1.0, 1.0);

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
