//! Generates app stub libraries for cross-compilation
//! These stubs provide the Roc app entrypoints that the platform host expects to call

const std = @import("std");
const builtin = @import("builtin");
const target_mod = @import("target.zig");
const builder = @import("builder.zig");

const RocTarget = target_mod.RocTarget;
const Allocator = std.mem.Allocator;

// Check if LLVM is available at compile time
const llvm_available = if (@import("builtin").is_test) false else @import("config").llvm;

/// Platform entrypoint information
pub const PlatformEntrypoint = struct {
    name: []const u8, // Function name like "addInts", "processString"
};

/// Generate an app stub object file containing implementations for platform-expected entrypoints
pub fn generateAppStubObject(
    allocator: Allocator,
    output_dir: []const u8,
    entrypoints: []const PlatformEntrypoint,
    target: RocTarget,
) ![]const u8 {
    // Check if LLVM is available
    if (!llvm_available) {
        return error.LLVMNotAvailable;
    }

    const std_zig_llvm = @import("std").zig.llvm;
    const Builder = std_zig_llvm.Builder;

    // Create LLVM Builder
    var llvm_builder = try Builder.init(.{
        .allocator = allocator,
        .name = "roc_app_stub",
    });
    defer llvm_builder.deinit();

    // Generate the app stub functions
    try createAppStubs(&llvm_builder, entrypoints, target);

    // Generate paths for temporary files
    const bitcode_path = try std.fs.path.join(allocator, &.{ output_dir, "app_stub.bc" });
    defer allocator.free(bitcode_path);

    const object_filename = try std.fmt.allocPrint(allocator, "app_stub_{s}.o", .{@tagName(target)});
    const object_path = try std.fs.path.join(allocator, &.{ output_dir, object_filename });
    // Don't defer free object_path since we return it

    // Generate bitcode
    const producer = Builder.Producer{
        .name = "Roc App Stub Generator",
        .version = .{ .major = 1, .minor = 0, .patch = 0 },
    };

    const bitcode = try llvm_builder.toBitcode(allocator, producer);
    defer allocator.free(bitcode);

    // Write bitcode to file
    const bc_file = try std.fs.cwd().createFile(bitcode_path, .{});
    defer bc_file.close();

    // Convert u32 array to bytes for writing
    const bytes = std.mem.sliceAsBytes(bitcode);
    try bc_file.writeAll(bytes);
    std.log.debug("Wrote bitcode file: {s} ({} bytes)", .{ bitcode_path, bytes.len });

    // Compile bitcode to object file using LLVM
    // For native compilation, use empty CPU to let LLVM choose the default
    // For cross-compilation, use "generic" for maximum compatibility
    const detected_native = target_mod.RocTarget.detectNative();
    const is_native = target == detected_native;
    const cpu_name = if (is_native) "" else "generic";

    std.log.debug("Native target: {}, Request target: {}, Is native: {}", .{ detected_native, target, is_native });
    std.log.debug("Using CPU: '{s}'", .{cpu_name});

    const compile_config = builder.CompileConfig{
        .input_path = bitcode_path,
        .output_path = object_path,
        .optimization = .size,
        .target = target,
        .cpu = cpu_name,
        .features = "",
    };

    std.log.debug("About to call compileBitcodeToObject...", .{});

    const success = builder.compileBitcodeToObject(allocator, compile_config) catch |err| {
        std.log.err("Failed to compile bitcode to object: {}", .{err});
        allocator.free(object_path);
        return err;
    };

    std.log.debug("compileBitcodeToObject returned: {}", .{success});

    if (!success) {
        std.log.err("Bitcode compilation returned false without error", .{});
        allocator.free(object_path);
        return error.CompilationFailed;
    }

    std.log.debug("Generated app stub object: {s}", .{object_path});
    return object_path;
}

/// Creates app stub functions in LLVM IR
fn createAppStubs(llvm_builder: *std.zig.llvm.Builder, entrypoints: []const PlatformEntrypoint, target: RocTarget) !void {

    // Create pointer type
    const ptr_type = try llvm_builder.ptrType(.default);

    // Add stub for each platform entrypoint
    for (entrypoints) |entrypoint| {
        try addRocCallAbiStub(llvm_builder, ptr_type, entrypoint.name, target);
    }
}

/// Add an app entrypoint stub that follows the RocCall ABI
/// RocCall ABI: void roc__<name>(ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.C) void;
fn addRocCallAbiStub(
    llvm_builder: *std.zig.llvm.Builder,
    ptr_type: std.zig.llvm.Builder.Type,
    name: []const u8,
    target: RocTarget,
) !void {
    const Builder = std.zig.llvm.Builder;
    const WipFunction = Builder.WipFunction;

    // RocCall ABI signature: void roc__<name>(ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.C) void
    const params = [_]Builder.Type{ ptr_type, ptr_type, ptr_type };
    const fn_type = try llvm_builder.fnType(.void, &params, .normal);

    // Build the function name with roc__ prefix
    const base_name = try std.fmt.allocPrint(llvm_builder.gpa, "roc__{s}", .{name});
    defer llvm_builder.gpa.free(base_name);

    // Add platform-specific prefix if needed (e.g., underscore for macOS)
    const full_name = if (target.isMacOS())
        try std.fmt.allocPrint(llvm_builder.gpa, "_{s}", .{base_name})
    else
        try llvm_builder.gpa.dupe(u8, base_name);
    defer llvm_builder.gpa.free(full_name);

    const fn_name = try llvm_builder.strtabString(full_name);
    const func = try llvm_builder.addFunction(fn_type, fn_name, .default);

    // Use external linkage so the symbol is visible to the linker
    func.setLinkage(.external, llvm_builder);

    var wip = try WipFunction.init(llvm_builder, .{
        .function = func,
        .strip = false,
    });
    defer wip.deinit();

    const entry = try wip.block(0, "entry");
    wip.cursor = .{ .block = entry };

    // Generate actual implementation based on function name
    if (std.mem.eql(u8, name, "addInts")) {
        try addIntsImplementation(&wip, llvm_builder);
    } else if (std.mem.eql(u8, name, "multiplyInts")) {
        try multiplyIntsImplementation(&wip, llvm_builder);
    } else if (std.mem.eql(u8, name, "processString")) {
        // processString not supported in cross-compilation stubs - only int platform supported
        _ = try wip.retVoid();
    } else {
        // Default: just return void for unknown functions
        _ = try wip.retVoid();
    }

    try wip.finish();
}

/// Get the expected app entrypoints for known test platforms based on host.zig files
pub fn getTestPlatformEntrypoints(allocator: Allocator, platform_type: []const u8) ![]PlatformEntrypoint {
    if (std.mem.eql(u8, platform_type, "int")) {
        // Based on test/int/platform/host.zig:
        // extern fn roc__addInts(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.C) void;
        // extern fn roc__multiplyInts(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.C) void;
        const entrypoints = try allocator.alloc(PlatformEntrypoint, 2);
        entrypoints[0] = PlatformEntrypoint{ .name = "addInts" };
        entrypoints[1] = PlatformEntrypoint{ .name = "multiplyInts" };
        return entrypoints;
    }

    // Only int platform supported for cross-compilation
    return error.PlatformNotSupported;
}

/// Detect platform type from file path
pub fn detectPlatformType(platform_path: []const u8) []const u8 {
    // Use cross-platform path checking
    var iter = std.fs.path.componentIterator(platform_path) catch return "unknown";
    while (iter.next()) |component| {
        if (std.mem.eql(u8, component.name, "int")) {
            return "int";
        } else if (std.mem.eql(u8, component.name, "str")) {
            return "str";
        }
    }
    return "unknown";
}

/// Generate implementation for addInts: loads two i64s from arg_ptr, adds them, stores result to ret_ptr
fn addIntsImplementation(wip: *std.zig.llvm.Builder.WipFunction, llvm_builder: *std.zig.llvm.Builder) !void {
    // Get function parameters: ops, ret_ptr, arg_ptr
    const ret_ptr = wip.arg(1); // ret_ptr: *anyopaque -> where to store the i64 result
    const arg_ptr = wip.arg(2); // arg_ptr: *anyopaque -> points to struct { a: i64, b: i64 }

    // Cast arg_ptr to pointer to struct { i64, i64 }
    const i64_type = .i64;
    const args_struct_type = try llvm_builder.structType(.normal, &[_]std.zig.llvm.Builder.Type{ i64_type, i64_type });
    const args_ptr_type = try llvm_builder.ptrType(.default);
    const args_ptr = try wip.cast(.bitcast, arg_ptr, args_ptr_type, "args_ptr");

    // Load the two i64 values from the args struct
    const zero = try llvm_builder.intConst(.i32, 0);
    const one = try llvm_builder.intConst(.i32, 1);

    const a_ptr = try wip.gep(.inbounds, args_struct_type, args_ptr, &[_]std.zig.llvm.Builder.Value{ zero.toValue(), zero.toValue() }, "a_ptr");
    const b_ptr = try wip.gep(.inbounds, args_struct_type, args_ptr, &[_]std.zig.llvm.Builder.Value{ zero.toValue(), one.toValue() }, "b_ptr");

    const a = try wip.load(.normal, i64_type, a_ptr, .default, "a");
    const b = try wip.load(.normal, i64_type, b_ptr, .default, "b");

    // Add the two values
    const result = try wip.bin(.add, a, b, "result");

    // Cast ret_ptr and store the result
    const ret_i64_ptr = try wip.cast(.bitcast, ret_ptr, args_ptr_type, "ret_i64_ptr");
    _ = try wip.store(.normal, result, ret_i64_ptr, .default);

    // Return void
    _ = try wip.retVoid();
}

/// Generate implementation for multiplyInts: loads two i64s from arg_ptr, multiplies them, stores result to ret_ptr
fn multiplyIntsImplementation(wip: *std.zig.llvm.Builder.WipFunction, llvm_builder: *std.zig.llvm.Builder) !void {
    // Get function parameters: ops, ret_ptr, arg_ptr
    const ret_ptr = wip.arg(1); // ret_ptr: *anyopaque -> where to store the i64 result
    const arg_ptr = wip.arg(2); // arg_ptr: *anyopaque -> points to struct { a: i64, b: i64 }

    // Cast arg_ptr to pointer to struct { i64, i64 }
    const i64_type = .i64;
    const args_struct_type = try llvm_builder.structType(.normal, &[_]std.zig.llvm.Builder.Type{ i64_type, i64_type });
    const args_ptr_type = try llvm_builder.ptrType(.default);
    const args_ptr = try wip.cast(.bitcast, arg_ptr, args_ptr_type, "args_ptr");

    // Load the two i64 values from the args struct
    const zero = try llvm_builder.intConst(.i32, 0);
    const one = try llvm_builder.intConst(.i32, 1);

    const a_ptr = try wip.gep(.inbounds, args_struct_type, args_ptr, &[_]std.zig.llvm.Builder.Value{ zero.toValue(), zero.toValue() }, "a_ptr");
    const b_ptr = try wip.gep(.inbounds, args_struct_type, args_ptr, &[_]std.zig.llvm.Builder.Value{ zero.toValue(), one.toValue() }, "b_ptr");

    const a = try wip.load(.normal, i64_type, a_ptr, .default, "a");
    const b = try wip.load(.normal, i64_type, b_ptr, .default, "b");

    // Multiply the two values
    const result = try wip.bin(.mul, a, b, "result");

    // Cast ret_ptr and store the result
    const ret_i64_ptr = try wip.cast(.bitcast, ret_ptr, args_ptr_type, "ret_i64_ptr");
    _ = try wip.store(.normal, result, ret_i64_ptr, .default);

    // Return void
    _ = try wip.retVoid();
}
