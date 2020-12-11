#version 450
// Adapted from: 
// https://github.com/sotrh/learn-wgpu by Benjamin Hansen, licensed under the MIT license
// https://github.com/cloudhead/rgx by Alexis Sellier, licensed under the MIT license

layout(set = 0, binding = 0) uniform Globals {
	mat4 ortho;
} global;

layout(location=0) in vec2 aPosition;
layout(location=1) in vec3 aColor;

layout(location=0) out vec3 vColor;

void main() {
    gl_Position = global.ortho * vec4(aPosition, 0, 1);
    vColor = aColor;
}