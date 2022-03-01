struct VertexOutput {
    [[location(0)]] color: vec4<f32>;
    [[builtin(position)]] position: vec4<f32>;
};

[[block]]
struct Globals {
    ortho: mat4x4<f32>;
};

[[group(0), binding(0)]]
var<uniform> u_globals: Globals;

[[stage(vertex)]]
fn vs_main(
    [[location(0)]] in_position: vec2<f32>,
    [[location(1)]] in_color: vec4<f32>,
) -> VertexOutput {
    var out: VertexOutput;

    out.position = u_globals.ortho * vec4<f32>(in_position, 0.0, 1.0);
    out.color = in_color;

    return out;
}

[[stage(fragment)]]
fn fs_main(in: VertexOutput) -> [[location(0)]] vec4<f32> {
    return in.color;
}