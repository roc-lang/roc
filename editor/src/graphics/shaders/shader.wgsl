
[[location(0)]]
var<in> in_position: vec2<f32>;
[[location(1)]]
var<in> in_color: vec4<f32>;
[[location(0)]]
var<out> out_color: vec4<f32>;
[[builtin(position)]]
var<out> out_pos: vec4<f32>;

[[block]]
struct Globals {
    ortho: mat4x4<f32>;
};

[[group(0), binding(0)]]
var<uniform> u_globals: Globals;

[[stage(vertex)]]
fn vs_main() {
    out_pos = u_globals.ortho * vec4<f32>(in_position, 0.0, 1.0);
    out_color = in_color;
}

[[location(0)]]
var<in> in_color: vec4<f32>;
[[location(0)]]
var<out> out_color: vec4<f32>;

[[stage(fragment)]]
fn fs_main() {
    out_color = in_color;
}