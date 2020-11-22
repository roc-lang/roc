#version 450

// Layout value labelled "in" acquire data from the vertex buffer,
// as defined in the buffer descriptor for this shader.
layout(location=0) in vec3 position;
layout(location=1) in vec3 color;

// Layout values labelled "out" send their data to the fragment shader.
layout(location=0) out vec3 v_color;

void main() {
    v_color = color;
    gl_Position = vec4(position, 1.0);
}
