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

    // Compile bitcode to object file using LLVM
    const compile_config = builder.CompileConfig{
        .input_path = bitcode_path,
        .output_path = object_path,
        .optimization = .size,
        .target = target,
    };

    const success = try builder.compileBitcodeToObject(allocator, compile_config);
    if (!success) {
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
    
    // For stub purposes, just return immediately
    // In a real app, this would contain the actual compiled Roc code
    _ = try wip.retVoid();
    
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
    } else if (std.mem.eql(u8, platform_type, "str")) {
        // Based on test/str/platform/host.zig:
        // extern fn roc__processString(ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.C) void;
        const entrypoints = try allocator.alloc(PlatformEntrypoint, 1);
        entrypoints[0] = PlatformEntrypoint{ .name = "processString" };
        return entrypoints;
    }
    
    // Default/unknown platforms - return empty array
    return try allocator.alloc(PlatformEntrypoint, 0);
}

/// Detect platform type from file path
pub fn detectPlatformType(platform_path: []const u8) []const u8 {
    if (std.mem.indexOf(u8, platform_path, "/int/") != null) {
        return "int";
    } else if (std.mem.indexOf(u8, platform_path, "/str/") != null) {
        return "str";
    }
    return "unknown";
}