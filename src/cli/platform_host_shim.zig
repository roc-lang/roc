//! Helpers for using Zig's LLVM Builder API to generate a shim library for the
//! Roc interpreter that translates from the platform host API.

const std = @import("std");
const Builder = std.zig.llvm.Builder;
const WipFunction = Builder.WipFunction;
const builtin = @import("builtin");

/// Represents a single entrypoint that a Roc platform host expects to call.
/// Each entrypoint corresponds to a specific function the host can invoke,
/// such as "init", "render", "update", etc.
pub const EntryPoint = struct {
    /// The name of the entrypoint function (without the "roc__" prefix).
    /// This will be used to generate the exported function name.
    /// For example, "init" becomes "roc__init".
    name: []const u8,

    /// The unique index for this entrypoint that gets passed to roc_entrypoint.
    /// This allows the Roc runtime to dispatch to the correct implementation
    /// based on which exported function was called by the host.
    idx: u32,
};

/// Adds the extern declaration for `roc_entrypoint` to the LLVM module.
///
/// This function creates the declaration for the single entry point that all
/// Roc platform functions will delegate to. The Roc interpreter provides
/// the actual implementation of this function, which acts as a dispatcher
/// based on the entry_idx parameter.
fn addRocEntrypoint(builder: *Builder) !Builder.Function.Index {
    // Create pointer type for generic pointers (i8* in LLVM)
    const ptr_type = try builder.ptrType(.default);

    // Create the roc_entrypoint function type:
    // void roc_entrypoint(u32 entry_idx, RocOps* ops, void* ret_ptr, void* arg_ptr)
    const entrypoint_params = [_]Builder.Type{ .i32, ptr_type, ptr_type, ptr_type };
    const entrypoint_type = try builder.fnType(.void, &entrypoint_params, .normal);

    // Create function name with platform-specific prefix
    const base_name = "roc_entrypoint";
    const fn_name_str = if (builtin.target.os.tag == .macos)
        try std.fmt.allocPrint(builder.gpa, "_{s}", .{base_name})
    else
        try builder.gpa.dupe(u8, base_name);
    defer builder.gpa.free(fn_name_str);
    const fn_name = try builder.strtabString(fn_name_str);

    // Add the extern function declaration (no body)
    const entrypoint_fn = try builder.addFunction(entrypoint_type, fn_name, .default);
    entrypoint_fn.setLinkage(.external, builder);

    return entrypoint_fn;
}

/// Generates a single exported platform function that delegates to roc_entrypoint.
///
/// This creates the "glue" functions that a Roc platform host expects to find when
/// linking against a Roc application. Each generated function follows the exact
/// Roc Host ABI specification and simply forwards the call to the interpreter's `roc_entrypoint`
/// with the appropriate index.
///
/// For example, if name="render" and entry_idx=1, this generates:
/// ```llvm
/// define void @roc__render(ptr %ops, ptr %ret_ptr, ptr %arg_ptr) {
///   call void @roc_entrypoint(i32 1, ptr %ops, ptr %ret_ptr, ptr %arg_ptr)
///   ret void
/// }
/// ```
///
/// This delegation pattern allows:
/// 1. The host to call specific named functions (roc__init, roc__render, etc.)
/// 2. The pre-built Roc interpreter to handle all calls through a single dispatch mechanism
/// 3. Efficient code generation since each wrapper is just a simple function call
/// 4. Easy addition/removal of platform functions without changing the pre-built interpreter binary which is embedded in the roc cli exectuable.
fn addRocExportedFunction(builder: *Builder, entrypoint_fn: Builder.Function.Index, name: []const u8, entry_idx: u32) !Builder.Function.Index {
    // Create pointer type for generic pointers
    const ptr_type = try builder.ptrType(.default);

    // Create the Roc function type following the ABI:
    // void roc_function(RocOps* ops, void* ret_ptr, void* arg_ptr)
    const roc_fn_params = [_]Builder.Type{ ptr_type, ptr_type, ptr_type };
    const roc_fn_type = try builder.fnType(.void, &roc_fn_params, .normal);

    // Create function name with roc__ prefix and platform-specific prefix
    const base_name = try std.fmt.allocPrint(builder.gpa, "roc__{s}", .{name});
    defer builder.gpa.free(base_name);
    const full_name = if (builtin.target.os.tag == .macos)
        try std.fmt.allocPrint(builder.gpa, "_{s}", .{base_name})
    else
        try builder.gpa.dupe(u8, base_name);
    defer builder.gpa.free(full_name);
    const fn_name = try builder.strtabString(full_name);

    // Add the function to the module with external linkage
    const roc_fn = try builder.addFunction(roc_fn_type, fn_name, .default);
    roc_fn.setLinkage(.external, builder);

    // Create a work-in-progress function to add instructions
    var wip = try WipFunction.init(builder, .{
        .function = roc_fn,
        .strip = false,
    });
    defer wip.deinit();

    // Create the entry basic block
    const entry_block = try wip.block(0, "entry");
    wip.cursor = .{ .block = entry_block };

    // Get the function parameters
    const ops_ptr = wip.arg(0); // RocOps pointer
    const ret_ptr = wip.arg(1); // Return value pointer
    const arg_ptr = wip.arg(2); // Arguments pointer

    // Create constant for entry_idx
    const idx_const = try builder.intConst(.i32, entry_idx);

    // Call roc_entrypoint(entry_idx, ops, ret_ptr, arg_ptr)
    const call_args = [_]Builder.Value{ idx_const.toValue(), ops_ptr, ret_ptr, arg_ptr };
    _ = try wip.call(.normal, .ccc, .none, entrypoint_fn.typeOf(builder), entrypoint_fn.toValue(builder), &call_args, "");

    // Return void
    _ = try wip.retVoid();

    // Finish building the function
    try wip.finish();

    return roc_fn;
}

/// Creates a complete Roc platform library with all necessary entrypoints.
///
/// This generates a shim that translates between the pre-built roc interpreter
/// which has a single `roc_entrypoint`, and the API defined by the platform with the
/// specific entrypoints the host expects to link with.
///
/// The generated library structure follows this pattern:
/// ```llvm
/// ; External function that provided by the pre-built roc interpreter
/// declare void @roc_entrypoint(i32 %entry_idx, ptr %ops, ptr %ret_ptr, ptr %arg_ptr)
///
/// ; Platform functions that the host expects to be linked with
/// define void @roc__init(ptr %ops, ptr %ret_ptr, ptr %arg_ptr) {
///   call void @roc_entrypoint(i32 0, ptr %ops, ptr %ret_ptr, ptr %arg_ptr)
///   ret void
/// }
///
/// define void @roc__render(ptr %ops, ptr %ret_ptr, ptr %arg_ptr) {
///   call void @roc_entrypoint(i32 1, ptr %ops, ptr %ret_ptr, ptr %arg_ptr)
///   ret void
/// }
/// ; ... etc for each entrypoint
/// ```
///
/// The generated library is then compiled using LLVM to an object file and linked with
/// both the host and the Roc interpreter to create a dev build executable.
pub fn createInterpreterShim(builder: *Builder, entrypoints: []const EntryPoint) !void {
    // Add the extern roc_entrypoint declaration
    const entrypoint_fn = try addRocEntrypoint(builder);

    // Add each exported entrypoint function
    for (entrypoints) |entry| {
        _ = try addRocExportedFunction(builder, entrypoint_fn, entry.name, entry.idx);
    }
}
