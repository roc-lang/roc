//! REPL evaluation module that processes expressions and maintains state

const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("../base.zig");
const parse = @import("../check/parse.zig");
const canonicalize = @import("../check/canonicalize.zig");
const check_types = @import("../check/check_types.zig");
const types = @import("../types.zig");
const types_store = @import("../types/store.zig");
const layout_store = @import("../layout/store.zig");
const layout = @import("../layout/layout.zig");
const eval = @import("../eval/interpreter.zig");
const stack = @import("../eval/stack.zig");
const CIR = canonicalize.CIR;
const target = @import("../base/target.zig");
const writers = @import("../types/writers.zig");
const repl_state = @import("repl_full.zig");

/// Read-Eval-Print Loop implementation for interactive Roc expression evaluation
pub const Repl = repl_state.Repl;
