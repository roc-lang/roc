//! Compile-time evaluation finalization.
//!
//! This module is intentionally only the checking finalizer boundary.
//! Compile-time values must be stored through the checked `ConstStore` path
//! produced by the post-check pipeline.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const lir = @import("lir");

const Allocator = std.mem.Allocator;
const checked = check.CheckedArtifact;
const CompilerHost = @import("compiler_host.zig");
const ConstStoreWriter = @import("const_store_writer.zig");
const Interpreter = @import("interpreter.zig").Interpreter;

pub fn finalizer() checked.CompileTimeFinalizer {
    return .{ .finalize = finalize };
}

fn finalize(
    _: ?*anyopaque,
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    imports: []const checked.PublishImportArtifact,
    available_modules: []const checked.ImportedModuleView,
    relation_modules: []const checked.ImportedModuleView,
) anyerror!void {
    _ = imports;

    const requests = try compileTimeRootRequests(allocator, module);
    defer allocator.free(requests);

    if (requests.len != 0) {
        var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
            allocator,
            .{
                .root = checked.loweringViewWithRelations(module, relation_modules),
                .imports = available_modules,
            },
            .{
                .requests = requests,
                .purpose = .compile_time,
                .compile_time_module_sink = module,
            },
            .{
                .target_usize = base.target.TargetUsize.native,
                .checked_state = .checking_finalization,
            },
        );
        defer lowered.deinit();

        var host = CompilerHost.init(allocator);
        defer host.deinit();

        var interpreter = try Interpreter.init(
            allocator,
            &lowered.lir_result.store,
            &lowered.lir_result.layouts,
            host.ops(),
        );
        defer interpreter.deinit();

        var writer = ConstStoreWriter.Writer.init(allocator, module, &lowered.lir_result);
        defer writer.deinit();

        for (lowered.lir_result.const_roots.items) |root| {
            const payload = blk: {
                const eval_result = try interpreter.eval(.{
                    .proc_id = root.proc,
                    .ret_layout = root.ret_layout,
                });
                defer interpreter.dropValue(eval_result.value, root.ret_layout);
                break :blk try writer.storeRoot(root, eval_result.value);
            };

            const root_id = compileTimeRootForRequest(module, root.request);
            module.compile_time_roots.fillPayload(root_id, payload);
            publishConstRoot(module, module.compile_time_roots.root(root_id), payload);
        }
    }

    module.const_store.verifyPublished();
}

fn compileTimeRootRequests(
    allocator: Allocator,
    module: *const checked.CheckedModuleArtifact,
) Allocator.Error![]checked.RootRequest {
    var requests = std.ArrayList(checked.RootRequest).empty;
    errdefer requests.deinit(allocator);

    for (module.root_requests.requests) |request| {
        if (request.abi != .compile_time) continue;
        try requests.append(allocator, request);
    }

    return try requests.toOwnedSlice(allocator);
}

fn compileTimeRootForRequest(
    module: *const checked.CheckedModuleArtifact,
    request: checked.RootRequest,
) checked.ComptimeRootId {
    const expected_kind: checked.CompileTimeRootKind = switch (request.kind) {
        .compile_time_constant => .constant,
        .compile_time_callable => .callable_binding,
        else => finalizationInvariant("non compile-time request reached compile-time root lookup"),
    };

    for (module.compile_time_roots.roots) |root| {
        if (root.kind == expected_kind and rootSourceEql(root.source, request.source)) return root.id;
    }

    finalizationInvariant("compile-time root request did not match a checked root");
}

fn publishConstRoot(
    module: *checked.CheckedModuleArtifact,
    root: checked.CompileTimeRoot,
    payload: checked.CompileTimeRootPayload,
) void {
    if (root.kind != .constant) return;
    const node = switch (payload) {
        .const_node => |id| id,
        else => finalizationInvariant("constant root finalized with non-constant payload"),
    };
    const pattern = root.pattern orelse finalizationInvariant("constant root had no checked pattern");
    const top_level = module.top_level_values.lookupByPattern(pattern) orelse
        finalizationInvariant("constant root had no top-level value");
    const const_ref = switch (top_level.value) {
        .const_ref => |ref| ref,
        .procedure_binding => finalizationInvariant("constant root top-level value was not a constant"),
    };
    const stored = checked.StoredConstTemplate{ .node = node };
    module.const_templates.fillStoredConst(const_ref, stored);
    module.exported_const_templates.fillStoredConst(const_ref, stored);
}

fn rootSourceEql(a: checked.RootSource, b: checked.RootSource) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .def => |left| left == b.def,
        .expr => |left| left == b.expr,
        .statement => |left| left == b.statement,
        .required_binding => |left| left == b.required_binding,
    };
}

fn finalizationInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("compile-time finalization invariant violated: {s}", .{message});
    }
    unreachable;
}

test "compile-time finalization declarations are referenced" {
    std.testing.refAllDecls(@This());
}
