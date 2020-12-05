#version 450
// Taken from https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen, licensed under the MIT license

layout(location=0) in vec3 color;

layout(location=0) out vec4 fColor;

void main() {
    fColor = vec4(color, 1.0);
}