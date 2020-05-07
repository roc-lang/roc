#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(push_constant) uniform PushConstants {
    vec4 color;
    vec2 pos;
    vec2 scale;
} push_constants;

layout(location = 0) out vec4 vertex_color;

void main() {
    vec2 position;
    if (gl_VertexIndex == 0) {
        position = vec2(0.0, -0.5);
    } else if (gl_VertexIndex == 1) {
        position = vec2(-0.5, 0.5);
    } else if (gl_VertexIndex == 2) {
        position = vec2(0.5, 0.5);
    }

    vec2 pos = position * push_constants.scale;
    vertex_color = push_constants.color;
    gl_Position = vec4((pos + push_constants.pos), 0.0, 1.0);
}
