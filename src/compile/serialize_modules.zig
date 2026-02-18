//! Module serialization for embedded builds.
//!
//! This module provides the `serializeModules()` function that takes compiled
//! modules from BuildEnv and serializes them into a binary format suitable
//! for embedding in executables.
//!
//! The serialization format is defined in `collections.serialization`.

const std = @import("std");
const can = @import("can");
const collections = @import("collections");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CompactWriter = collections.CompactWriter;
const SerializedHeader = collections.SerializedHeader;
const SerializedModuleInfo = collections.SerializedModuleInfo;
const SERIALIZED_FORMAT_MAGIC = collections.SERIALIZED_FORMAT_MAGIC;

const compile_build = @import("compile_build.zig");
const CompiledModuleInfo = compile_build.BuildEnv.CompiledModuleInfo;

/// Result of serializing modules.
pub const SerializedModulesResult = struct {
    /// Serialized bytes (owned by provided allocator)
    bytes: []align(16) u8,
    /// Entry point definition indices
    entry_def_indices: []const u32,
    /// Number of compilation errors encountered
    error_count: usize,
    /// Number of compilation warnings encountered
    warning_count: usize,
};

/// Serialize compiled modules into a binary format for embedding.
///
/// This function:
/// 1. Reassigns hosted lambda indices globally across all platform modules
/// 2. Serializes all modules using CompactWriter
/// 3. Returns the serialized bytes and entry point information
///
/// Parameters:
/// - allocator: Allocator for result buffers
/// - modules: Compiled modules in serialization order (from getModulesInSerializationOrder)
/// - primary_module_idx: Index of the primary module (platform main or app)
/// - app_module_idx: Index of the app module
///
/// Returns: SerializedModulesResult with serialized bytes
pub fn serializeModules(
    allocator: Allocator,
    modules: []const CompiledModuleInfo,
    primary_module_idx: usize,
    app_module_idx: usize,
) !SerializedModulesResult {
    // Phase 1: Reassign hosted lambda indices globally
    try reassignHostedLambdaIndices(allocator, modules, primary_module_idx, app_module_idx);

    // Phase 2: Serialize everything using CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(allocator);

    const module_count: u32 = @intCast(modules.len);

    // Get entry points from primary environment
    const primary_env = modules[primary_module_idx].env;
    const entry_defs = primary_env.exports;
    const entry_count: u32 = entry_defs.span.len;

    // Build entry def indices
    const entry_def_indices = try allocator.alloc(u32, entry_count);
    const defs_slice = primary_env.store.sliceDefs(entry_defs);
    for (defs_slice, 0..) |def_idx, i| {
        entry_def_indices[i] = @intFromEnum(def_idx);
    }

    // 1. Allocate and fill header
    if (@import("build_options").trace_build) {
        std.debug.print("[SERIALIZE] module_count={} entry_count={}\n", .{ module_count, entry_count });
        for (modules) |mod| {
            std.debug.print("[SERIALIZE]   module: {s}\n", .{mod.name});
        }
    }
    const header = try writer.appendAlloc(allocator, SerializedHeader);
    header.magic = SERIALIZED_FORMAT_MAGIC;
    header.format_version = 1;
    header.module_count = module_count;
    header.entry_count = entry_count;
    header.primary_env_index = @intCast(primary_module_idx);
    header.app_env_index = @intCast(app_module_idx);
    // def_indices_offset and module_infos_offset will be set later

    // 2. Allocate module info array
    try writer.padToAlignment(allocator, @alignOf(SerializedModuleInfo));
    header.module_infos_offset = writer.total_bytes;
    const module_infos = try allocator.alloc(SerializedModuleInfo, module_count);
    defer allocator.free(module_infos);

    // Add module infos to writer
    try writer.iovecs.append(allocator, .{
        .iov_base = @ptrCast(module_infos.ptr),
        .iov_len = module_count * @sizeOf(SerializedModuleInfo),
    });
    writer.total_bytes += module_count * @sizeOf(SerializedModuleInfo);

    // 3. Serialize source bytes and module names for each module
    for (modules, 0..) |mod, i| {
        // Source bytes
        try writer.padToAlignment(allocator, 1);
        module_infos[i].source_offset = writer.total_bytes;
        module_infos[i].source_len = mod.source.len;
        if (mod.source.len > 0) {
            try writer.iovecs.append(allocator, .{
                .iov_base = @constCast(mod.source.ptr),
                .iov_len = mod.source.len,
            });
            writer.total_bytes += mod.source.len;
        }

        // Module name
        try writer.padToAlignment(allocator, 1);
        module_infos[i].module_name_offset = writer.total_bytes;
        module_infos[i].module_name_len = mod.name.len;
        if (mod.name.len > 0) {
            try writer.iovecs.append(allocator, .{
                .iov_base = @constCast(mod.name.ptr),
                .iov_len = mod.name.len,
            });
            writer.total_bytes += mod.name.len;
        }
    }

    // 4. Serialize each ModuleEnv
    for (modules, 0..) |mod, i| {
        // Ensure 8-byte alignment for ModuleEnv.Serialized (it contains u64/i64 fields)
        try writer.padToAlignment(allocator, 8);

        // Record the offset before allocating
        const env_offset_before = writer.total_bytes;
        const serialized_env = try writer.appendAlloc(allocator, ModuleEnv.Serialized);
        module_infos[i].env_serialized_offset = env_offset_before;

        try serialized_env.serialize(mod.env, allocator, &writer);
    }

    // 5. Serialize entry point def indices
    try writer.padToAlignment(allocator, @alignOf(u32));
    header.def_indices_offset = writer.total_bytes;
    if (entry_count > 0) {
        try writer.iovecs.append(allocator, .{
            .iov_base = @ptrCast(entry_def_indices.ptr),
            .iov_len = entry_count * @sizeOf(u32),
        });
        writer.total_bytes += entry_count * @sizeOf(u32);
    }

    // 6. Write all to buffer
    const buffer = try allocator.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    _ = try writer.writeToBuffer(buffer);

    return SerializedModulesResult{
        .bytes = buffer,
        .entry_def_indices = entry_def_indices,
        .error_count = 0, // Errors are tracked separately by BuildEnv
        .warning_count = 0,
    };
}

/// Reassign hosted lambda indices globally across all platform modules.
///
/// This ensures that hosted function indices are consistent across all modules,
/// sorted alphabetically by their qualified names (e.g., "Stdout.line!").
///
/// Only processes platform sibling modules (not app, not platform main.roc).
fn reassignHostedLambdaIndices(
    allocator: Allocator,
    modules: []const CompiledModuleInfo,
    primary_module_idx: usize,
    app_module_idx: usize,
) !void {
    const HostedCompiler = can.HostedCompiler;

    var all_hosted_fns = std.ArrayList(HostedCompiler.HostedFunctionInfo).empty;
    defer {
        for (all_hosted_fns.items) |fn_info| {
            allocator.free(fn_info.name_text);
        }
        all_hosted_fns.deinit(allocator);
    }

    // Collect from platform sibling modules only (not app, not platform main.roc)
    for (modules, 0..) |mod, i| {
        // Skip app module and platform main.roc
        if (i == app_module_idx or i == primary_module_idx) continue;
        // Only process platform siblings
        if (!mod.is_platform_sibling) continue;

        var module_fns = try HostedCompiler.collectAndSortHostedFunctions(mod.env);
        defer {
            // Free the name_text strings allocated by collectAndSortHostedFunctions
            for (module_fns.items) |fn_info| {
                mod.env.gpa.free(fn_info.name_text);
            }
            module_fns.deinit(mod.env.gpa);
        }

        for (module_fns.items) |fn_info| {
            try all_hosted_fns.append(allocator, .{
                .name_text = try allocator.dupe(u8, fn_info.name_text),
                .symbol_name = fn_info.symbol_name,
                .expr_idx = fn_info.expr_idx,
            });
        }
    }

    // Sort globally by name
    const SortContext = struct {
        pub fn lessThan(_: void, a: HostedCompiler.HostedFunctionInfo, b: HostedCompiler.HostedFunctionInfo) bool {
            return std.mem.order(u8, a.name_text, b.name_text) == .lt;
        }
    };
    std.mem.sort(HostedCompiler.HostedFunctionInfo, all_hosted_fns.items, {}, SortContext.lessThan);

    // Deduplicate
    var write_idx: usize = 0;
    for (all_hosted_fns.items, 0..) |fn_info, read_idx| {
        if (write_idx == 0 or !std.mem.eql(u8, all_hosted_fns.items[write_idx - 1].name_text, fn_info.name_text)) {
            if (write_idx != read_idx) {
                all_hosted_fns.items[write_idx] = fn_info;
            }
            write_idx += 1;
        } else {
            allocator.free(fn_info.name_text);
        }
    }
    all_hosted_fns.shrinkRetainingCapacity(write_idx);

    // Reassign global indices for platform sibling modules only
    for (modules, 0..) |mod, module_idx| {
        // Skip app module and platform main.roc
        if (module_idx == app_module_idx or module_idx == primary_module_idx) continue;
        // Only process platform siblings
        if (!mod.is_platform_sibling) continue;

        const platform_env = mod.env;
        const all_defs = platform_env.store.sliceDefs(platform_env.all_defs);

        for (all_defs) |def_idx| {
            const def = platform_env.store.getDef(def_idx);
            const expr = platform_env.store.getExpr(def.expr);

            if (expr == .e_hosted_lambda) {
                const hosted = expr.e_hosted_lambda;
                const local_name = platform_env.getIdent(hosted.symbol_name);

                var plat_module_name = platform_env.module_name;
                if (std.mem.endsWith(u8, plat_module_name, ".roc")) {
                    plat_module_name = plat_module_name[0 .. plat_module_name.len - 4];
                }
                const qualified_name = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ plat_module_name, local_name });
                defer allocator.free(qualified_name);

                const stripped_name = if (std.mem.endsWith(u8, qualified_name, "!"))
                    qualified_name[0 .. qualified_name.len - 1]
                else
                    qualified_name;

                for (all_hosted_fns.items, 0..) |fn_info, idx| {
                    if (std.mem.eql(u8, fn_info.name_text, stripped_name)) {
                        const expr_node_idx = @as(@TypeOf(platform_env.store.nodes).Idx, @enumFromInt(@intFromEnum(def.expr)));
                        var expr_node = platform_env.store.nodes.get(expr_node_idx);
                        var payload = expr_node.getPayload().expr_hosted_lambda;
                        payload.index = @intCast(idx);
                        expr_node.setPayload(.{ .expr_hosted_lambda = payload });
                        platform_env.store.nodes.set(expr_node_idx, expr_node);
                        break;
                    }
                }
            }
        }
    }
}
