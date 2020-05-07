#version 450
#extension GL_ARB_separate_shader_objects : enable

void main() {
    vec2 position;
    if (gl_VertexIndex == 0) {
        position = vec2(0.0, -0.5);
    } else if (gl_VertexIndex == 1) {
        position = vec2(-0.5, 0.5);
    } else if (gl_VertexIndex == 2) {
        position = vec2(0.5, 0.5);
    }

    gl_Position = vec4(position, 0.0, 1.0);
}
