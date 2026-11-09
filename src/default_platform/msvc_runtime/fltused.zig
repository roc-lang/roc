//! `_fltused`, which code that uses floating point references, for an
//! MSVC-ABI image without vcruntime. See `memcpy.zig` for why each of these
//! routines is its own strong archive member.

export var _fltused: c_int = 1;
