#version 450
// Taken from https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen, licensed under the MIT license

layout(location=0) in vec2 aPosition;
layout(location=1) in vec3 aColor;

layout(location=0) out vec3 vColor;

void main() {
    gl_Position = vec4(aPosition, 0, 1);
    vColor = aColor;
}