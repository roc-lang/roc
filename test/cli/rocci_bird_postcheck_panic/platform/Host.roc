## Internal module exposing raw WASM-4 hosted effects.
##
## End users should import `W4` for the high-level, ergonomic API.
## These functions correspond 1:1 with the WASM-4 runtime imports.
##
## Hosted functions are sorted alphabetically below because the Roc ABI
## dispatches hosted calls positionally by alphabetical index.
Host := [].{
    blit! : List(U8), I32, I32, U32, U32, U32 => {}
    blit_sub! : List(U8), I32, I32, U32, U32, U32, U32, U32, U32 => {}
    disk_read! : () => List(U8)
    disk_write! : List(U8) => Bool
    get_draw_colors! : () => U16
    get_gamepad! : U8 => U8
    get_mouse_buttons! : () => U8
    get_mouse_x! : () => I16
    get_mouse_y! : () => I16
    get_netplay! : () => U8
    get_palette_color! : U8 => U32
    get_pixel! : U8, U8 => U8
    hline! : I32, I32, U32 => {}
    line! : I32, I32, I32, I32 => {}
    oval! : I32, I32, U32, U32 => {}
    rand! : () => I32
    rand_range_less_than! : I32, I32 => I32
    rect! : I32, I32, U32, U32 => {}
    seed_rand! : U64 => {}
    set_draw_colors! : U16 => {}
    set_hide_gamepad_overlay! : Bool => {}
    set_palette! : U32, U32, U32, U32 => {}
    set_pixel! : U8, U8, U8 => {}
    set_preserve_frame_buffer! : Bool => {}
    text! : Str, I32, I32 => {}
    tone! : U32, U32, U16, U8 => {}
    trace! : Str => {}
    vline! : I32, I32, U32 => {}
}
