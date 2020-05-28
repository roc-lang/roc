#version 450
#extension GL_ARB_separate_shader_objects : enable

uniform sampler2D font_tex;

layout(location = 0) in vec2 f_tex_pos;
layout(location = 1) in vec4 f_color;

layout(location = 0) out vec4 out_color;

void main() {
    float alpha = texture(font_tex, f_tex_pos).r;
    if (alpha <= 0.0) {
        discard;
    }
    out_color = f_color * vec4(1.0, 1.0, 1.0, alpha);
}
