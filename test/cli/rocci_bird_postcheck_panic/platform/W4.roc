## # roc-wasm4
##
## Build [WASM-4](https://wasm4.org) games using Roc.
##
## This module provides the high-level, ergonomic API for the WASM-4 platform.
## Internally it wraps the low-level effects exposed by `Host`.
import Host

## The `Palette` consists of four colors. There is also `None` which is used to
## represent a transparent or no change color. Each pixel on the screen will be
## drawn using one of these colors.
##
## You may find it helpful to create an alias for your game.
##
## ```
## red = Color2
## green = Color3
##
## W4.set_text_colors!({ fg: red, bg: green })
## ```
Palette : [None, Color1, Color2, Color3, Color4]

## Represents the four colors in the `Palette`.
DrawColors : {
    primary : Palette,
    secondary : Palette,
    tertiary : Palette,
    quaternary : Palette,
}

## Represents the current state of a [Player] gamepad.
Gamepad : {
    button1 : Bool,
    button2 : Bool,
    left : Bool,
    right : Bool,
    up : Bool,
    down : Bool,
}

## Represents the current state of the mouse.
Mouse : {
    x : I16,
    y : I16,
    left : Bool,
    right : Bool,
    middle : Bool,
}

## Represents the current state of [Netplay](https://wasm4.org/docs/guides/multiplayer#netplay).
##
## Netplay connects gamepad inputs over the Internet using WebRTC.
Netplay : [
    Enabled(Player),
    Disabled,
]

## Represents a player.
##
## [WASM-4 supports realtime multiplayer](https://wasm4.org/docs/guides/multiplayer) of
## up to 4 players, either locally or online.
Player : [Player1, Player2, Player3, Player4]

## Represents a fragment shader for raw operations with the framebuffer.
##
## A shader takes the pixel `(x, y)` and the current `Palette` color at that
## position, and returns the new color to draw.
Shader : U8, U8, Palette -> Palette

W4 :: [].{

    ## Width of the WASM-4 screen in pixels.
    screen_width : () -> I32
    screen_width = || 160

    ## Height of the WASM-4 screen in pixels.
    screen_height : () -> I32
    screen_height = || 160

    ## Set the color `Palette` for your game.
    ##
    ## ```
    ## W4.set_palette!({
    ##     color1: 0xffffff,
    ##     color2: 0xff0000,
    ##     color3: 0x00ff00,
    ##     color4: 0x0000ff,
    ## })
    ## ```
    ##
    ## Warning: this will overwrite the existing `Palette`, changing all colors on the screen.
    set_palette! : { color1 : U32, color2 : U32, color3 : U32, color4 : U32 } => {}
    set_palette! = |{ color1, color2, color3, color4 }|
        Host.set_palette!(color1, color2, color3, color4)

    ## Get the color `Palette` for your game.
    ##
    ## ```
    ## { color1, color2, color3, color4 } = W4.get_palette!()
    ## ```
    get_palette! : () => { color1 : U32, color2 : U32, color3 : U32, color4 : U32 }
    get_palette! = || {
        color1: Host.get_palette_color!(0),
        color2: Host.get_palette_color!(1),
        color3: Host.get_palette_color!(2),
        color4: Host.get_palette_color!(3),
    }

    ## Set the draw colors for the next draw command.
    ##
    ## ```
    ## blue = Color1
    ## white = Color4
    ## W4.set_draw_colors!({
    ##     primary: blue,
    ##     secondary: white,
    ##     tertiary: None,
    ##     quaternary: None,
    ## })
    ## ```
    ##
    ## Warning: this will overwrite any existing draw colors that are set.
    set_draw_colors! : DrawColors => {}
    set_draw_colors! = |colors|
        Host.set_draw_colors!(W4.to_color_flags(colors))

    ## Get the currently set draw colors.
    ##
    ## ```
    ## { primary, secondary, tertiary, quaternary } = W4.get_draw_colors!()
    ## ```
    get_draw_colors! : () => DrawColors
    get_draw_colors! = || W4.from_color_flags(Host.get_draw_colors!())

    ## Helper for setting the primary drawing color.
    ##
    ## ```
    ## blue = Color1
    ## W4.set_primary_color!(blue)
    ## ```
    ##
    ## Warning: this will overwrite any existing draw colors, and sets the
    ## secondary, tertiary and quaternary values to `None`.
    set_primary_color! : Palette => {}
    set_primary_color! = |primary|
        W4.set_draw_colors!({
            primary,
            secondary: None,
            tertiary: None,
            quaternary: None,
        })

    ## Helper for setting the draw colors for text.
    ##
    ## ```
    ## blue = Color1
    ## white = Color4
    ## W4.set_text_colors!({ fg: blue, bg: white })
    ## ```
    ##
    ## Warning: this will overwrite any existing draw colors, and sets the
    ## tertiary and quaternary values to `None`.
    set_text_colors! : { fg : Palette, bg : Palette } => {}
    set_text_colors! = |{ fg, bg }|
        W4.set_draw_colors!({
            primary: fg,
            secondary: bg,
            tertiary: None,
            quaternary: None,
        })

    ## Helper for colors when drawing a shape.
    ##
    ## ```
    ## blue = Color1
    ## white = Color4
    ## W4.set_shape_colors!({ border: blue, fill: white })
    ## ```
    ##
    ## Warning: this will overwrite any existing draw colors, and sets the
    ## tertiary and quaternary values to `None`.
    set_shape_colors! : { border : Palette, fill : Palette } => {}
    set_shape_colors! = |{ border, fill }|
        W4.set_draw_colors!({
            primary: fill,
            secondary: border,
            tertiary: None,
            quaternary: None,
        })

    ## Draw text to the screen.
    ##
    ## ```
    ## W4.text!("Hello, World", { x: 0, y: 0 })
    ## ```
    ##
    ## Text color is the Primary draw color.
    ## Background color is the Secondary draw color.
    ##
    ## [Refer to the WASM-4 docs for more information](https://wasm4.org/docs/guides/text)
    text! : Str, { x : I32, y : I32 } => {}
    text! = |str, { x, y }| Host.text!(str, x, y)

    ## Draw a rectangle to the screen.
    ##
    ## ```
    ## W4.rect!({ x: 0, y: 10, width: 40, height: 60 })
    ## ```
    ##
    ## Fill color is the Primary draw color.
    ## Border color is the Secondary draw color.
    ##
    ## [Refer to the WASM-4 docs for more information](https://wasm4.org/docs/reference/functions#rect-x-y-width-height)
    rect! : { x : I32, y : I32, width : U32, height : U32 } => {}
    rect! = |{ x, y, width, height }| Host.rect!(x, y, width, height)

    ## Draw an oval to the screen.
    ##
    ## ```
    ## W4.oval!({ x: 10, y: 20, width: 20, height: 30 })
    ## ```
    ##
    ## Fill color is the Primary draw color.
    ## Border color is the Secondary draw color.
    ##
    ## [Refer to the WASM-4 docs for more information](https://wasm4.org/docs/reference/functions#oval-x-y-width-height)
    oval! : { x : I32, y : I32, width : U32, height : U32 } => {}
    oval! = |{ x, y, width, height }| Host.oval!(x, y, width, height)

    ## Draw a line between two points to the screen.
    ##
    ## ```
    ## W4.line!({ x: 0, y: 0 }, { x: 10, y: 10 })
    ## ```
    ##
    ## Line color is the Primary draw color.
    ##
    ## [Refer to the WASM-4 docs for more information](https://wasm4.org/docs/reference/functions#line-x1-y1-x2-y2)
    line! : { x : I32, y : I32 }, { x : I32, y : I32 } => {}
    line! = |{ x: x1, y: y1 }, { x: x2, y: y2 }| Host.line!(x1, y1, x2, y2)

    ## Draw a horizontal line starting at (x, y) with len to the screen.
    ##
    ## ```
    ## W4.hline!({ x: 10, y: 20, len: 30 })
    ## ```
    ##
    ## Line color is the Primary draw color.
    hline! : { x : I32, y : I32, len : U32 } => {}
    hline! = |{ x, y, len }| Host.hline!(x, y, len)

    ## Draw a vertical line starting at (x, y) with len to the screen.
    ##
    ## ```
    ## W4.vline!({ x: 10, y: 20, len: 30 })
    ## ```
    ##
    ## Line color is the Primary draw color.
    vline! : { x : I32, y : I32, len : U32 } => {}
    vline! = |{ x, y, len }| Host.vline!(x, y, len)

    ## Get the controls for a `Gamepad`.
    ##
    ## ```
    ## { button1, button2, left, right, up, down } = W4.get_gamepad!(Player1)
    ## ```
    get_gamepad! : Player => Gamepad
    get_gamepad! = |player| {
        gamepad_number = match player {
            Player1 => 1
            Player2 => 2
            Player3 => 3
            Player4 => 4
        }

        flags = Host.get_gamepad!(gamepad_number)

        {
            # 1 BUTTON_1
            button1: W4.bit_is_set(flags, 0),
            # 2 BUTTON_2
            button2: W4.bit_is_set(flags, 1),
            # 16 BUTTON_LEFT
            left: W4.bit_is_set(flags, 4),
            # 32 BUTTON_RIGHT
            right: W4.bit_is_set(flags, 5),
            # 64 BUTTON_UP
            up: W4.bit_is_set(flags, 6),
            # 128 BUTTON_DOWN
            down: W4.bit_is_set(flags, 7),
        }
    }

    ## Get the current `Mouse` position and button state.
    ##
    ## ```
    ## { x, y, left, right, middle } = W4.get_mouse!()
    ## ```
    get_mouse! : () => Mouse
    get_mouse! = || {
        x = Host.get_mouse_x!()
        y = Host.get_mouse_y!()
        buttons = Host.get_mouse_buttons!()

        {
            x,
            y,
            # 1 MOUSE_LEFT
            left: W4.bit_is_set(buttons, 0),
            # 2 MOUSE_RIGHT
            right: W4.bit_is_set(buttons, 1),
            # 4 MOUSE_MIDDLE
            middle: W4.bit_is_set(buttons, 2),
        }
    }

    ## Get the `Netplay` status.
    ##
    ## ```
    ## netplay = W4.get_netplay!()
    ## match netplay {
    ##     Enabled(Player1) => # ..
    ##     Enabled(Player2) => # ..
    ##     Enabled(Player3) => # ..
    ##     Enabled(Player4) => # ..
    ##     Disabled => # ..
    ## }
    ## ```
    ##
    ## Note: All WASM-4 games that support local multiplayer automatically support netplay.
    ##
    ## [Refer to the WASM-4 docs for more information](https://wasm4.org/docs/guides/multiplayer)
    get_netplay! : () => Netplay
    get_netplay! = || {
        flags = Host.get_netplay!()
        enabled = W4.bit_is_set(flags, 2)
        if enabled {
            # low 2 bits select the player
            player = match flags % 4 {
                0 => Player1
                1 => Player2
                2 => Player3
                3 => Player4
                _ => { crash "got invalid netplay player from the host" }
            }
            Enabled(player)
        } else {
            Disabled
        }
    }

    ## Seeds the global pseudo-random number generator.
    ##
    ## ```
    ## W4.seed_rand!(frames_since_start)
    ## ```
    ##
    ## Wasm4 exposes no way to seed a random number generator.
    ## To work around this, it is suggested to count the number of frames the user
    ## is on the title screen before starting the game and use that to seed the prng.
    seed_rand! : U64 => {}
    seed_rand! = |s| Host.seed_rand!(s)

    ## Generate a pseudo-random number between `Num.min_i32` and `Num.max_i32` (inclusive).
    ##
    ## ```
    ## i = W4.rand!()
    ## ```
    rand! : () => I32
    rand! = || Host.rand!()

    ## Generate a pseudo-random number in the range `[start, before)`.
    ##
    ## ```
    ## # random number in the range 0-99
    ## i = W4.rand_between!({ start: 0, before: 100 })
    ## ```
    rand_between! : { start : I32, before : I32 } => I32
    rand_between! = |{ start, before }| Host.rand_range_less_than!(start, before)

    ## Prints a message to the debug console.
    ##
    ## ```
    ## W4.trace!("Hello, World")
    ## ```
    ##
    ## [Refer to the WASM-4 docs for more information](https://wasm4.org/docs/guides/trace)
    trace! : Str => {}
    trace! = |str| Host.trace!(str)

    ## Saves data to persistent storage. Any previously saved data on the disk is replaced.
    ##
    ## Returns `Err(SaveFailed)` on failure.
    ##
    ## ```
    ## result = W4.save_to_disk!([0x10])
    ## match result {
    ##     Ok({}) => # success
    ##     Err(SaveFailed) => # handle failure
    ## }
    ## ```
    ##
    ## Games can persist up to 1024 bytes of data.
    ##
    ## [Refer to the WASM-4 docs for more information](https://wasm4.org/docs/guides/diskw)
    save_to_disk! : List(U8) => Try({}, [SaveFailed])
    save_to_disk! = |data|
        if Host.disk_write!(data) {
            Ok({})
        } else {
            Err(SaveFailed)
        }

    ## Gets all saved data from persistent storage.
    ##
    ## ```
    ## data = W4.load_from_disk!()
    ## ```
    ##
    ## Games can persist up to 1024 bytes of data.
    ##
    ## [Refer to the WASM-4 docs for more information](https://wasm4.org/docs/guides/diskw)
    load_from_disk! : () => List(U8)
    load_from_disk! = || Host.disk_read!()

    ## Set a flag to keep the framebuffer between frames.
    ##
    ## This can be helpful if you only want to update part of the screen.
    preserve_frame_buffer! : () => {}
    preserve_frame_buffer! = || Host.set_preserve_frame_buffer!(Bool.True)

    ## Set a flag to clear the framebuffer between frames.
    clear_frame_buffer_each_update! : () => {}
    clear_frame_buffer_each_update! = || Host.set_preserve_frame_buffer!(Bool.False)

    ## Set a flag to hide the gamepad overlay.
    hide_gamepad_overlay! : () => {}
    hide_gamepad_overlay! = || Host.set_hide_gamepad_overlay!(Bool.True)

    ## Set a flag to show the gamepad overlay.
    show_gamepad_overlay! : () => {}
    show_gamepad_overlay! = || Host.set_hide_gamepad_overlay!(Bool.False)

    ## Get the color for an individual pixel in the framebuffer.
    get_pixel! : { x : U8, y : U8 } => Palette
    get_pixel! = |{ x, y }| W4.extract_color(Host.get_pixel!(x, y))

    ## Set the color for an individual pixel in the framebuffer.
    set_pixel! : { x : U8, y : U8 }, Palette => {}
    set_pixel! = |{ x, y }, color| {
        bits = match color {
            None => 0x0
            Color1 => 0x1
            Color2 => 0x2
            Color3 => 0x3
            Color4 => 0x4
        }
        Host.set_pixel!(x, y, bits)
    }

    ## Run a fragment `Shader` on the raw framebuffer.
    ##
    ## The shader is invoked for every pixel `(x, y)` on the screen, and its
    ## return value is written back to the framebuffer.
    run_shader! : Shader => {}
    run_shader! = |shader| {
        var $y = 0.U8
        while $y < 160 {
            var $x = 0.U8
            while $x < 160 {
                color = W4.get_pixel!({ x: $x, y: $y })
                new_color = shader($x, $y, color)
                W4.set_pixel!({ x: $x, y: $y }, new_color)
                $x = $x + 1
            }
            $y = $y + 1
        }
    }

    ## Plays a tone sound.
    ##
    ## Please refer to the [WASM-4 audio docs](https://wasm4.org/docs/guides/audio/).
    ##
    ## The sound.roc example app along with the
    ## [WASM-4 sound tools](https://wasm4.org/docs/guides/audio/#sound-tool) can be
    ## quite helpful to play with.
    tone! :
        {
            start_freq : U16,
            end_freq : U16,
            channel : [
                Pulse1([Eighth, Quarter, Half, ThreeQuarters]),
                Pulse2([Eighth, Quarter, Half, ThreeQuarters]),
                Triangle,
                Noise,
            ],
            pan : [Center, Left, Right],
            sustain_time : U8,
            release_time : U8,
            decay_time : U8,
            attack_time : U8,
            volume : U8,
            peak_volume : U8,
        }
        => {}
    tone! = |{ start_freq, end_freq, channel, pan, sustain_time, release_time, decay_time, attack_time, volume, peak_volume }| {
        # Each component occupies a disjoint byte/range, so OR is equivalent to addition.
        freq = U32.shift_left_by(U16.to_u32(end_freq), 16) + U16.to_u32(start_freq)

        duration =
            U32.shift_left_by(U8.to_u32(attack_time), 24)
            + U32.shift_left_by(U8.to_u32(decay_time), 16)
            + U32.shift_left_by(U8.to_u32(release_time), 8)
            + U8.to_u32(sustain_time)

        volume_bits = U16.shift_left_by(U8.to_u16(peak_volume), 8) + U8.to_u16(volume)

        pan_bits : U8
        pan_bits = match pan {
            Center => 0
            # pub const TONE_PAN_LEFT: u32 = 16;
            Left => 16
            # pub const TONE_PAN_RIGHT: u32 = 32;
            Right => 32
        }

        (channel_bits, mode_bits) = match channel {
            # pub const TONE_PULSE1: u32 = 0;
            Pulse1(mode) => (0.U8, W4.convert_mode(mode))
            # pub const TONE_PULSE2: u32 = 1;
            Pulse2(mode) => (1.U8, W4.convert_mode(mode))
            # pub const TONE_TRIANGLE: u32 = 2;
            Triangle => (2.U8, 0.U8)
            # pub const TONE_NOISE: u32 = 3;
            Noise => (3.U8, 0.U8)
        }

        # channel_bits is in bits 0-1, mode_bits is in bits 2-3, pan_bits is in bits 4-5.
        flags = pan_bits + mode_bits + channel_bits

        Host.tone!(freq, duration, volume_bits, flags)
    }

    # HELPERS ------

    ## Returns Bool.True if bit `n` (0-indexed from LSB) is set in `byte`.
    bit_is_set : U8, U8 -> Bool
    bit_is_set = |byte, n| U8.shift_right_zf_by(byte, n) % 2 == 1

    convert_mode : [Eighth, Quarter, Half, ThreeQuarters] -> U8
    convert_mode = |m| match m {
        # pub const TONE_MODE1: u32 = 0;
        Eighth => 0
        # pub const TONE_MODE2: u32 = 4;
        Quarter => 4
        # pub const TONE_MODE3: u32 = 8;
        Half => 8
        # pub const TONE_MODE4: u32 = 12;
        ThreeQuarters => 12
    }

    to_color_flags : DrawColors -> U16
    to_color_flags = |{ primary, secondary, tertiary, quaternary }| {
        pos1 = match primary {
            None => 0x0
            Color1 => 0x1
            Color2 => 0x2
            Color3 => 0x3
            Color4 => 0x4
        }

        pos2 = match secondary {
            None => 0x00
            Color1 => 0x10
            Color2 => 0x20
            Color3 => 0x30
            Color4 => 0x40
        }

        pos3 = match tertiary {
            None => 0x000
            Color1 => 0x100
            Color2 => 0x200
            Color3 => 0x300
            Color4 => 0x400
        }

        pos4 = match quaternary {
            None => 0x0000
            Color1 => 0x1000
            Color2 => 0x2000
            Color3 => 0x3000
            Color4 => 0x4000
        }

        # The four positions occupy disjoint nibbles, so OR is equivalent to addition.
        pos1 + pos2 + pos3 + pos4
    }

    extract_color : U8 -> Palette
    extract_color = |pos| match pos {
        0x0 => None
        0x1 => Color1
        0x2 => Color2
        0x3 => Color3
        0x4 => Color4
        _ => { crash "got invalid draw color from the host" }
    }

    from_color_flags : U16 -> DrawColors
    from_color_flags = |flags| {
        # Each draw color occupies one nibble (4 bits); extract via shift + mod.
        pos1 = U16.to_u8_wrap(flags % 16)
        pos2 = U16.to_u8_wrap(U16.shift_right_zf_by(flags, 4) % 16)
        pos3 = U16.to_u8_wrap(U16.shift_right_zf_by(flags, 8) % 16)
        pos4 = U16.to_u8_wrap(U16.shift_right_zf_by(flags, 12) % 16)

        {
            primary: W4.extract_color(pos1),
            secondary: W4.extract_color(pos2),
            tertiary: W4.extract_color(pos3),
            quaternary: W4.extract_color(pos4),
        }
    }
}

expect W4.to_color_flags({ primary: Color2, secondary: Color4, tertiary: None, quaternary: None }) == 0x0042
expect W4.to_color_flags({ primary: Color1, secondary: Color2, tertiary: Color3, quaternary: Color4 }) == 0x4321

# NOTE: the following expects round-trip from_color_flags, but currently trip a
# Roc compiler bug: `expect` calling a function that returns a closed tag-union
# type alias produces an "infinite type" error. Once Roc fixes this, they can be re-enabled.
# expect W4.from_color_flags(0x0042) == { primary: Color2, secondary: Color4, tertiary: None, quaternary: None }
# expect W4.from_color_flags(0x4321) == { primary: Color1, secondary: Color2, tertiary: Color3, quaternary: Color4 }
