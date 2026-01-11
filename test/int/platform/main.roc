platform ""
    requires {
        [Model : model] for main : {
            init : {} -> model,
            update : model, I64 -> model,
            render : model -> Simple(model)
        }
    }
    exposes [Simple]
    packages {}
    provides { init_for_host: "init", update_for_host: "update", render_for_host: "render", test_mixed_args_for_host: "test_mixed_args", test_struct_arg_for_host: "test_struct_arg" }
    targets: {
        files: "targets/",
        exe: {
            x64mac: ["libhost.a", app],
            arm64mac: ["libhost.a", app],
            x64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            arm64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            x64glibc: ["Scrt1.o", "crti.o", "libhost.a", app, "crtn.o", "libc.so"],
            arm64glibc: ["Scrt1.o", "crti.o", "libhost.a", app, "crtn.o", "libc.so"],
            x64win: ["host.lib", app],
            arm64win: ["host.lib", app],
        }
    }

import Simple exposing [Simple]

# Explicit type annotations for host-facing functions
# Note: Use uppercase Model here - it's a type alias introduced by the for-clause [Model : model]
# that gets unified with the app's concrete type during type checking.
init_for_host : {} -> Box(Model)
init_for_host = |{}| {
    init_fn = main.init
    record = init_fn({})
    Box.box(record)
}

update_for_host : Box(Model), I64 -> Box(Model)
update_for_host = |boxed_model, value| {
    m = Box.unbox(boxed_model)
    update_fn = main.update
    Box.box(update_fn(m, value))
}

# This now returns Simple(Model) - an opaque type from an imported module
render_for_host : Box(Model) -> Simple(Model)
render_for_host = |boxed_model| {
    m = Box.unbox(boxed_model)
    render_fn = main.render
    render_fn(m)
}

# Test function for issue 8991: verify mixed-alignment arguments are received correctly
# Takes Bool (1-byte alignment) and I64 (8-byte alignment) - these have different alignments
# and must be laid out correctly when passed from the host.
# Returns a tuple of both values so the host can verify they were received correctly.
test_mixed_args_for_host : Bool, I64 -> (Bool, I64)
test_mixed_args_for_host = |flag, value| (flag, value)

# Test struct similar to bug report: mixed alignments with multiple fields
# Expected memory layout sorted by alignment descending, then field name alphabetically:
# - frame_count: U64 (8 bytes, offset 0)
# - mouse_wheel: F32 (4 bytes, offset 8)
# - mouse_x: F32 (4 bytes, offset 12)
# - mouse_y: F32 (4 bytes, offset 16)
# - mouse_left: Bool (1 byte, offset 20)
# - mouse_middle: Bool (1 byte, offset 21)
# - mouse_right: Bool (1 byte, offset 22)
# Total size: 24 bytes (with padding to 8-byte alignment)
FrameInput : {
    frame_count : U64,
    mouse_x : F32,
    mouse_y : F32,
    mouse_left : Bool,
    mouse_middle : Bool,
    mouse_right : Bool,
    mouse_wheel : F32,
}

# Entry point that takes FrameInput struct and returns it
# This tests that a single struct argument with mixed alignments is handled correctly
test_struct_arg_for_host : FrameInput -> FrameInput
test_struct_arg_for_host = |input| input
