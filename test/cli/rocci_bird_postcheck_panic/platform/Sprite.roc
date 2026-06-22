## Sprites for drawing to the WASM-4 framebuffer.
##
## A [Sprite] is simply a list of bytes encoded with either 1 bit per pixel (`BPP1`)
## or 2 bits per pixel (`BPP2`), along with information about how to read the
## pixel data.
##
## [Refer to the WASM-4 docs for more information](https://wasm4.org/docs/guides/sprites)
import Host

## Represents a [sprite](https://en.wikipedia.org/wiki/Sprite_(computer_graphics))
## for drawing to the screen.
Sprite := {
    data : List(U8),
    bpp : [BPP1, BPP2],
    stride : U32,
    region : { src_x : U32, src_y : U32, width : U32, height : U32 },
}.{
    ## A subregion of a [Sprite].
    SubRegion : { src_x : U32, src_y : U32, width : U32, height : U32 }

    ## Create a [Sprite] to be drawn or [blit](https://en.wikipedia.org/wiki/Bit_blit)
    ## to the screen.
    ##
    ## ```
    ## fruit_sprite = Sprite.new({
    ##     data: [0x00, 0xa0, 0x02, 0x00, 0x0e, 0xf0, 0x36, 0x5c, 0xd6, 0x57, 0xd5, 0x57, 0x35, 0x5c, 0x0f, 0xf0],
    ##     bpp: BPP2,
    ##     width: 8,
    ##     height: 8,
    ## })
    ## ```
    new : { data : List(U8), bpp : [BPP1, BPP2], width : U32, height : U32 } -> Sprite
    new = |{ data, bpp, width, height }| {
        data,
        bpp,
        stride: width,
        region: {
            src_x: 0,
            src_y: 0,
            width,
            height,
        },
    }

    ## Draw a [Sprite] to the framebuffer.
    ##
    ## ```
    ## Sprite.blit!(fruit_sprite, { x: 0, y: 0, flags: [FlipX, Rotate] })
    ## ```
    ##
    ## [Refer to the WASM-4 docs for more information](https://wasm4.org/docs/reference/functions#blit-spriteptr-x-y-width-height-flags)
    blit! : Sprite, { x : I32, y : I32, flags : List([FlipX, FlipY, Rotate]) } => {}
    blit! = |sprite, { x, y, flags }| {
        { src_x, src_y, width, height } = sprite.region

        format = match sprite.bpp {
            BPP1 => 0
            BPP2 => 1
        }

        # Each flag occupies a distinct bit, so we can sum non-duplicate contributions.
        flip_x_bit = if flags.contains(FlipX) { 2 } else { 0 }
        flip_y_bit = if flags.contains(FlipY) { 4 } else { 0 }
        rotate_bit = if flags.contains(Rotate) { 8 } else { 0 }
        combined = format + flip_x_bit + flip_y_bit + rotate_bit

        Host.blit_sub!(sprite.data, x, y, width, height, src_x, src_y, sprite.stride, combined)
    }

    ## Creates a [Sprite] referencing a subregion of the current [Sprite].
    ## This will return an error if the subregion does not fit in the current [Sprite].
    ##
    ## ```
    ## sub_sprite_result = Sprite.sub(sprite, { src_x: 20, src_y: 0, width: 20, height: 20 })
    ## ```
    ##
    ## Note: If your program should never generate an invalid subregion,
    ## [sub_or_crash] enables avoiding the result and simpler code.
    sub : Sprite, SubRegion -> Try(Sprite, [OutOfBounds])
    sub = |sprite, sub_region| {
        current_region = sprite.region

        out_of_bound_x = sub_region.src_x + sub_region.width > current_region.width
        out_of_bound_y = sub_region.src_y + sub_region.height > current_region.height

        if out_of_bound_x or out_of_bound_y {
            Err(OutOfBounds)
        } else {
            new_region = {
                src_x: current_region.src_x + sub_region.src_x,
                src_y: current_region.src_y + sub_region.src_y,
                width: sub_region.width,
                height: sub_region.height,
            }
            Ok({ ..sprite, region: new_region })
        }
    }

    ## Equivalent to the [sub] function, but will crash on error.
    ## This is really useful for static sprite sheet data that needs subSprites extracted.
    ##
    ## ```
    ## sub_sprite = Sprite.sub_or_crash(sprite, { src_x: 20, src_y: 0, width: 20, height: 20 })
    ## ```
    ##
    ## Warning: Will crash if the subregion is not contained within the sprite.
    sub_or_crash : Sprite, SubRegion -> Sprite
    sub_or_crash = |sprite, sub_region|
        match Sprite.sub(sprite, sub_region) {
            Ok(s) => s
            Err(OutOfBounds) => { crash "out of bounds subregion when generating subsprite" }
        }
}
