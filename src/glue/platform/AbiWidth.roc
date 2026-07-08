## Host pointer width: the only ABI axis glue may branch on.
##
## This is not an OS, architecture, or concrete Roc build target. `roc glue`
## emits every layout fact for both widths (`size32`/`size64`,
## `offset32`/`offset64`, ...), and generated bindings contain both shapes;
## the host compiler resolves the actual width
## (`#if UINTPTR_MAX == UINT64_MAX` in C, `if (@sizeOf(usize) == 4)` in Zig,
## `#[cfg(target_pointer_width = "32")]` in Rust).
AbiWidth := [
	Pointer32,
	Pointer64,
]
