#version 450
// Taken from https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen, licensed under the MIT license

// Check build_shaders.rs on how to recompile shaders if you have made changes to this file

layout(location=0) in vec3 color;

layout(location=0) out vec4 fColor;

void main() {
    fColor = vec4(color, 1.0);
}