platform ""
    requires { [Model : model] for main : { init! : () => model, update! : model => model } }
    exposes [W4, Sprite, Host]
    packages {}
    provides { "init_for_host": init_for_host!, "update_for_host": update_for_host! }
    hosted {
        "host_blit": Host.blit!,
        "host_blit_sub": Host.blit_sub!,
        "host_disk_read": Host.disk_read!,
        "host_disk_write": Host.disk_write!,
        "host_get_draw_colors": Host.get_draw_colors!,
        "host_get_gamepad": Host.get_gamepad!,
        "host_get_mouse_buttons": Host.get_mouse_buttons!,
        "host_get_mouse_x": Host.get_mouse_x!,
        "host_get_mouse_y": Host.get_mouse_y!,
        "host_get_netplay": Host.get_netplay!,
        "host_get_palette_color": Host.get_palette_color!,
        "host_get_pixel": Host.get_pixel!,
        "host_hline": Host.hline!,
        "host_line": Host.line!,
        "host_oval": Host.oval!,
        "host_rand": Host.rand!,
        "host_rand_range_less_than": Host.rand_range_less_than!,
        "host_rect": Host.rect!,
        "host_seed_rand": Host.seed_rand!,
        "host_set_draw_colors": Host.set_draw_colors!,
        "host_set_hide_gamepad_overlay": Host.set_hide_gamepad_overlay!,
        "host_set_palette": Host.set_palette!,
        "host_set_pixel": Host.set_pixel!,
        "host_set_preserve_frame_buffer": Host.set_preserve_frame_buffer!,
        "host_text": Host.text!,
        "host_tone": Host.tone!,
        "host_trace": Host.trace!,
        "host_vline": Host.vline!,
    }
    targets: {
        inputs: "targets/",
        wasm32: {
            inputs: ["host.wasm", app],
            output: Shared,
            import_memory: Zeroed,
            minimum_memory: 65536,
            maximum_memory: 65536,
            initial_stack_size: 14752,
            global_base: wasm4_program_memory_base,
        },
    }

# WASM-4 owns 0x0000..0x199f for registers and the framebuffer.
# Program data starts after that reserved range, rounded up to 32-byte alignment.
wasm4_reserved_memory_end = 0x19a0
wasm4_program_memory_base = 0x19c0

import W4
import Sprite
import Host

init_for_host! : () => Box(Model)
init_for_host! = || {
    init_fn! = main.init!
    Box.box(init_fn!())
}

update_for_host! : Box(Model) => Box(Model)
update_for_host! = |boxed| {
    update_fn! = main.update!
    Box.box(update_fn!(Box.unbox(boxed)))
}
