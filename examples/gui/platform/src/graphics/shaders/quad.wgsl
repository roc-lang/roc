

struct Globals {
    ortho: mat4x4<f32>;
};

@group(0)
@binding(0)
var<uniform> globals: Globals;

struct VertexInput {
    @location(0) position: vec2<f32>;
};

struct Quad {
    @location(1) pos: vec2<f32>; // can't use the name "position" twice for compatibility with metal on MacOS
    @location(2) width: f32;
    @location(3) height: f32;
    @location(4) color: vec4<f32>;
    @location(5) border_color: vec4<f32>;
    @location(6) border_width: f32;
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>;
    @location(0) color: vec4<f32>;
    @location(1) border_color: vec4<f32>;
    @location(2) border_width: f32;
};

@stage(vertex)
fn vs_main(
    input: VertexInput,
    quad: Quad
) -> VertexOutput {

    var transform: mat4x4<f32> = mat4x4<f32>(
        vec4<f32>(quad.width, 0.0, 0.0, 0.0),
        vec4<f32>(0.0, quad.height, 0.0, 0.0),
        vec4<f32>(0.0, 0.0, 1.0, 0.0),
        vec4<f32>(quad.pos, 0.0, 1.0)
    );

    var out: VertexOutput;
    
    out.position = globals.ortho * transform * vec4<f32>(input.position, 0.0, 1.0);;
    out.color = quad.color;
    out.border_color = quad.border_color;
    out.border_width = quad.border_width;

    return out;
}


@stage(fragment)
fn fs_main(
    input: VertexOutput
) -> @location(0) vec4<f32> {
    return input.color;
}
