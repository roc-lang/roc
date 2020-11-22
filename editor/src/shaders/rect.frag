#version 450

// The fragment shader's "in" values come from the "out" values of the vertex shader.
layout(location=0) in vec3 color;

// The actual color that is rendered to the screen based on the vertex.
layout(location=0) out vec4 f_color;

void main() {
    f_color = vec4(color, 1.0);
}
