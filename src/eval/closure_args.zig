//! Closure argument handling utilities for the interpreter
//! Provides functions to push closure arguments onto the interpreter stack

const std = @import("std");
const base = @import("base");
const types = @import("types");
const compile = @import("compile");
const builtins = @import("builtins");
const safe_memory = @import("../base/safe_memory.zig");

const eval = @import("interpreter.zig");
const layout_store = @import("../layout/store.zig");
const layout = @import("../layout/layout.zig");

const ModuleEnv = compile.ModuleEnv;
const RocOps = builtins.host_abi.RocOps;

/// Maximum number of closure parameters supported
pub const MAX_CLOSURE_PARAMS = 8;

/// Buffer size for formatting results
pub const RESULT_BUFFER_SIZE = 1024;

/// Validation and closure argument handling errors
pub const ClosureArgError = error{
    ParameterCountExceeded,
    StackPushFailed,
    LayoutError,
    MemoryError,
};

/// Closure argument pusher - handles pushing arguments onto the interpreter stack
pub const ClosureArgumentPusher = struct {
    interpreter: *eval.Interpreter,
    layout_cache: *layout_store.Store,

    pub fn init(interpreter: *eval.Interpreter, layout_cache: *layout_store.Store) ClosureArgumentPusher {
        return ClosureArgumentPusher{
            .interpreter = interpreter,
            .layout_cache = layout_cache,
        };
    }

    /// Push closure arguments onto the interpreter stack
    pub fn pushArguments(
        self: *ClosureArgumentPusher,
        param_patterns: []const ModuleEnv.Pattern.Idx,
        arg_ptr: ?*anyopaque,
    ) ClosureArgError!void {
        const param_count = param_patterns.len;

        // Validate parameter count
        try validateParameterCount(param_count);

        if (param_count == 0) {
            return; // No arguments to push
        }

        if (param_count == 1) {
            try self.pushSingleArgument(param_patterns[0], arg_ptr);
        } else {
            try self.pushMultipleArguments(param_patterns, arg_ptr);
        }
    }

    /// Push a single argument for closure calls
    fn pushSingleArgument(
        self: *ClosureArgumentPusher,
        param_pattern: ModuleEnv.Pattern.Idx,
        arg_ptr: ?*anyopaque,
    ) ClosureArgError!void {
        const param_var: types.Var = @enumFromInt(@intFromEnum(param_pattern));
        const param_layout_idx = self.layout_cache.addTypeVar(param_var) catch {
            return error.LayoutError;
        };
        const param_layout = self.layout_cache.getLayout(param_layout_idx);
        const size_bytes = self.layout_cache.layoutSize(param_layout);

        const dest_ptr = self.interpreter.pushStackValue(param_layout) catch {
            return error.StackPushFailed;
        };

        // Use safe copy with bounds checking for single parameter
        safe_memory.safeCopyArgument(arg_ptr, dest_ptr, 0, size_bytes, size_bytes) catch {
            return error.MemoryError;
        };
    }

    /// Push multiple arguments for closure calls
    fn pushMultipleArguments(
        self: *ClosureArgumentPusher,
        param_patterns: []const ModuleEnv.Pattern.Idx,
        arg_ptr: ?*anyopaque,
    ) ClosureArgError!void {
        const param_count = param_patterns.len;

        // Convert patterns to type variables
        var param_vars_buf: [MAX_CLOSURE_PARAMS]types.Var = undefined;
        for (param_patterns, 0..) |pat_idx, idx| {
            param_vars_buf[idx] = @enumFromInt(@intFromEnum(pat_idx));
        }

        // Compute element layouts
        var elem_layouts: [MAX_CLOSURE_PARAMS]layout.Layout = undefined;
        for (0..param_count) |i| {
            const v = param_vars_buf[i];
            const idx_v = self.layout_cache.addTypeVar(v) catch {
                return error.LayoutError;
            };
            elem_layouts[i] = self.layout_cache.getLayout(idx_v);
        }

        // Compute offsets using native target alignment rules
        const layout_info = try self.computeLayoutInfo(elem_layouts[0..param_count]);

        // Copy each element from arg_ptr + computed offset to the interpreter stack
        for (0..param_count) |i| {
            const elem_layout = elem_layouts[i];
            const elem_size = self.layout_cache.layoutSize(elem_layout);
            const elem_offset = layout_info.offsets[i];

            const dest_ptr = self.interpreter.pushStackValue(elem_layout) catch {
                return error.StackPushFailed;
            };

            // Use safe copy with bounds checking
            safe_memory.safeCopyArgument(arg_ptr, dest_ptr, elem_offset, elem_size, layout_info.total_size) catch {
                return error.MemoryError;
            };
        }
    }

    /// Layout information for multiple arguments
    const LayoutInfo = struct {
        offsets: [MAX_CLOSURE_PARAMS]usize,
        total_size: usize,
    };

    /// Compute layout offsets and total size for multiple arguments
    fn computeLayoutInfo(self: *ClosureArgumentPusher, layouts: []const layout.Layout) ClosureArgError!LayoutInfo {
        var offsets: [MAX_CLOSURE_PARAMS]usize = undefined;
        var running_offset: usize = 0;

        for (layouts, 0..) |elem_layout, i| {
            const elem_align = elem_layout.alignment(base.target.Target.native.target_usize);
            const mask = elem_align.toByteUnits() - 1;

            // Apply alignment
            if ((running_offset & mask) != 0) {
                running_offset = (running_offset + mask) & ~mask;
            }

            offsets[i] = running_offset;
            running_offset += self.layout_cache.layoutSize(elem_layout);
        }

        return LayoutInfo{
            .offsets = offsets,
            .total_size = running_offset,
        };
    }
};

/// Validate parameter count is within reasonable bounds
pub fn validateParameterCount(param_count: usize) ClosureArgError!void {
    if (param_count > MAX_CLOSURE_PARAMS) {
        std.log.err("Parameter count {} exceeds maximum {}", .{ param_count, MAX_CLOSURE_PARAMS });
        return error.ParameterCountExceeded;
    }
}

/// Push closure arguments onto the interpreter stack (convenience function)
pub fn pushClosureArguments(
    interpreter: *eval.Interpreter,
    layout_cache: *layout_store.Store,
    param_patterns: []const ModuleEnv.Pattern.Idx,
    arg_ptr: ?*anyopaque,
) ClosureArgError!void {
    var pusher = ClosureArgumentPusher.init(interpreter, layout_cache);
    try pusher.pushArguments(param_patterns, arg_ptr);
}
