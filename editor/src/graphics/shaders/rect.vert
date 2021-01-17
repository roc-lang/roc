#version 450
// Adapted from: 
// https://github.com/sotrh/learn-wgpu by Benjamin Hansen, licensed under the MIT license
// https://github.com/cloudhead/rgx by Alexis Sellier, licensed under the MIT license

// Check build_shaders.rs on how to recompile shaders if you have made changes to this file

layout(set = 0, binding = 0) uniform Globals {
    // orthographic projection is used to transform pixel coords to the coordinate system used by wgpu 
	mat4 ortho;
} global;

layout(location=0) in vec2 aPosition;
layout(location=1) in vec4 aColor;

layout(location=0) out vec4 vColor;

void main() {
    gl_Position = global.ortho * vec4(aPosition, 0, 1);
    vColor = aColor;
}