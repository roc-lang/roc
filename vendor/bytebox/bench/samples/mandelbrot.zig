const std = @import("std");
const complex = std.math.complex;
const Complex = complex.Complex(f32);

const Color = struct {
    R: u8,
    G: u8,
    B: u8,
};

const COLOR_BLACK = Color{ .R = 0, .G = 0, .B = 0 };
const COLOR_WHITE = Color{ .R = 255, .G = 255, .B = 255 };

const WIDTH = 256;
const HEIGHT = 256;

fn mandelbrot(c: Complex) Color {
    var z = Complex.init(0, 0);
    for (0..8) |_| {
        z = z.mul(z).add(c);
        if (2.0 <= complex.abs(z)) {
            return COLOR_WHITE;
        }
    }

    return COLOR_BLACK;
}

export fn run(resolution_s: i32) i64 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    const resolution: usize = @intCast(@max(resolution_s, 4));
    var pixels: []volatile Color = allocator.alloc(Color, resolution * resolution) catch @panic("OOM");

    for (0..resolution) |y| {
        for (0..resolution) |x| {
            const c = Complex.init(@as(f32, @floatFromInt(x)), @as(f32, @floatFromInt(y)));
            const color: Color = mandelbrot(c);
            pixels[y * resolution + x] = color;
        }
    }

    return 0;
}
