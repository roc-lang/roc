//! Focused eval tests for the cor-style lowering pipeline.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const backend = @import("backend");
const builtins = @import("builtins");
const collections = @import("collections");
const layout = @import("layout");
const types_mod = @import("types");
const monotype = @import("monotype");
const monotype_lifted = @import("monotype_lifted");
const lambdasolved = @import("lambdasolved");
const lambdamono = @import("lambdamono");
const ir = @import("ir");
const symbol_mod = @import("symbol");
const Interpreter = @import("../interpreter.zig").Interpreter;
const RuntimeHostEnv = @import("RuntimeHostEnv.zig");
const helpers = @import("helpers.zig");

const testing = std.testing;
const HostLirCodeGen = backend.HostLirCodeGen;
const ExecutableMemory = backend.ExecutableMemory;

fn mainProcArgLayouts(
    allocator: std.mem.Allocator,
    compiled: *const helpers.CompiledProgram,
) ![]layout.Idx {
    const proc = compiled.lowered.lir_result.store.getProcSpec(compiled.lowered.main_proc);
    const arg_locals = compiled.lowered.lir_result.store.getLocalSpan(proc.args);
    const arg_layouts = try allocator.alloc(layout.Idx, arg_locals.len);
    for (arg_locals, 0..) |local_id, i| {
        arg_layouts[i] = compiled.lowered.lir_result.store.getLocal(local_id).layout_idx;
    }
    return arg_layouts;
}

fn zeroedEntrypointArgBuffer(
    allocator: std.mem.Allocator,
    compiled: *const helpers.CompiledProgram,
    arg_layouts: []const layout.Idx,
) !?[]u8 {
    const EntrypointArgOrder = struct {
        index: usize,
        alignment: u32,
        size: u32,
    };

    const arg_offsets = try allocator.alloc(u32, arg_layouts.len);
    defer allocator.free(arg_offsets);
    if (arg_layouts.len != 0) {
        const ordered = try allocator.alloc(EntrypointArgOrder, arg_layouts.len);
        defer allocator.free(ordered);

        for (arg_layouts, 0..) |arg_layout, i| {
            const size_align = compiled.lowered.lir_result.layouts.layoutSizeAlign(
                compiled.lowered.lir_result.layouts.getLayout(arg_layout),
            );
            const slot_size = entrypointParamSlotSize(compiled, arg_layout);
            ordered[i] = .{
                .index = i,
                .alignment = @intCast(size_align.alignment.toByteUnits()),
                .size = slot_size,
            };
        }

        const SortCtx = struct {
            fn lessThan(_: void, lhs: EntrypointArgOrder, rhs: EntrypointArgOrder) bool {
                if (lhs.alignment != rhs.alignment) return lhs.alignment > rhs.alignment;
                return lhs.index < rhs.index;
            }
        };

        std.mem.sort(EntrypointArgOrder, ordered, {}, SortCtx.lessThan);

        var current_offset: u32 = 0;
        for (ordered) |arg| {
            current_offset = std.mem.alignForward(u32, current_offset, arg.alignment);
            arg_offsets[arg.index] = current_offset;
            current_offset += arg.size;
        }
    }

    var total_size: usize = 0;
    for (arg_layouts, 0..) |arg_layout, i| {
        total_size = @max(total_size, @as(usize, arg_offsets[i]) + entrypointParamSlotSize(compiled, arg_layout));
    }

    if (total_size == 0) return null;

    const buffer = try allocator.alignedAlloc(u8, collections.max_roc_alignment, @max(total_size, 1));
    @memset(buffer, 0);
    return buffer;
}

fn entrypointParamSlotSize(compiled: *const helpers.CompiledProgram, layout_idx: layout.Idx) u32 {
    const layouts = &compiled.lowered.lir_result.layouts;
    const runtime_layout_idx = layouts.runtimeRepresentationLayoutIdx(layout_idx);
    if (runtime_layout_idx == .str) return 24;
    if (runtime_layout_idx == .i128 or runtime_layout_idx == .u128 or runtime_layout_idx == .dec) return 16;

    if (@intFromEnum(runtime_layout_idx) < layouts.layouts.len()) {
        const layout_val = layouts.getLayout(runtime_layout_idx);
        const size = layouts.layoutSizeAlign(layout_val).size;
        if (layout_val.tag == .zst or size == 0) return 0;
        if (layout_val.tag == .list or layout_val.tag == .list_of_zst) return 24;
        if (layout_val.tag == .struct_ or layout_val.tag == .tag_union) {
            if (size > 8) return @intCast(std.mem.alignForward(u32, size, 8));
        }
    }

    const size = layouts.layoutSizeAlign(layouts.getLayout(layout_idx)).size;
    return if (size == 0) 0 else 8;
}

fn expectInspect(comptime source: []const u8, expected: []const u8) !void {
    var compiled = try helpers.compileInspectedExpr(testing.allocator, source);
    defer compiled.deinit(testing.allocator);
    const actual = try helpers.lirInterpreterInspectedStr(testing.allocator, &compiled.lowered);
    defer testing.allocator.free(actual);
    try testing.expectEqualStrings(expected, actual);
}

fn expectInspectProgram(
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource,
    expected: []const u8,
) !void {
    var compiled = try helpers.compileInspectedProgram(testing.allocator, source_kind, source, imports);
    defer compiled.deinit(testing.allocator);
    const actual = try helpers.lirInterpreterInspectedStr(testing.allocator, &compiled.lowered);
    defer testing.allocator.free(actual);
    try testing.expectEqualStrings(expected, actual);
}

fn expectInspectProgramWithArena(
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource,
    expected: []const u8,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var compiled = try helpers.compileInspectedProgram(arena_allocator, source_kind, source, imports);
    const actual = try helpers.lirInterpreterInspectedStr(arena_allocator, &compiled.lowered);
    try testing.expectEqualStrings(expected, actual);
}

fn countLowLevelOp(compiled: *const helpers.CompiledInspectedExpr, op: base.LowLevel) usize {
    var count: usize = 0;
    for (compiled.lowered.lir_result.store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_low_level => |assign| {
                if (assign.op == op) count += 1;
            },
            else => {},
        }
    }
    return count;
}

fn countDirectCalls(compiled: *const helpers.CompiledProgram) usize {
    var count: usize = 0;
    for (compiled.lowered.lir_result.store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_call => count += 1,
            else => {},
        }
    }
    return count;
}

fn countErasedCalls(compiled: *const helpers.CompiledProgram) usize {
    var count: usize = 0;
    for (compiled.lowered.lir_result.store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_call_erased => count += 1,
            else => {},
        }
    }
    return count;
}

const CompiledExecutableProgram = struct {
    resources: helpers.ParsedResources,
    executable: lambdamono.Lower.Result,

    pub fn deinit(self: *CompiledExecutableProgram, allocator: std.mem.Allocator) void {
        self.executable.deinit();
        helpers.cleanupParseAndCanonical(allocator, self.resources);
    }
};

fn compileMonotypeFromParsedResources(
    allocator: std.mem.Allocator,
    resources: *helpers.ParsedResources,
) !monotype.Lower.Result {
    var mono_lowerer = try monotype.Lower.Lowerer.init(allocator, &resources.typed_cir_modules, 1, null);
    defer mono_lowerer.deinit();
    return mono_lowerer.run(0);
}

fn compileEntrySpecializedMonotypeFromParsedResources(
    allocator: std.mem.Allocator,
    resources: *helpers.ParsedResources,
) !monotype.Lower.Result {
    const defs = resources.module_env.store.sliceDefs(resources.module_env.all_defs);
    var entry_def: ?can.CIR.Def.Idx = null;
    for (defs) |def_idx| {
        const def = resources.module_env.store.getDef(def_idx);
        if (def.expr == resources.expr_idx) {
            entry_def = def_idx;
            break;
        }
    }
    const entry_def_idx = entry_def orelse return error.NoRootDefinition;

    var mono_lowerer = try monotype.Lower.Lowerer.init(allocator, &resources.typed_cir_modules, 1, null);
    defer mono_lowerer.deinit();
    _ = try mono_lowerer.specializeTopLevelDef(0, entry_def_idx);
    return mono_lowerer.run(0);
}

fn compileExecutableFromParsedResources(
    allocator: std.mem.Allocator,
    resources: *helpers.ParsedResources,
) !lambdamono.Lower.Result {
    var mono = try compileMonotypeFromParsedResources(allocator, resources);
    defer mono.deinit();
    var lifted = try monotype_lifted.Lower.run(allocator, &mono);
    defer lifted.deinit();
    var solved = try lambdasolved.Lower.run(allocator, &lifted);
    defer solved.deinit();
    return lambdamono.Lower.run(allocator, &solved);
}

const CompiledIrProgram = struct {
    resources: helpers.ParsedResources,
    ir_result: ir.Lower.Result,

    pub fn deinit(self: *CompiledIrProgram, allocator: std.mem.Allocator) void {
        self.ir_result.deinit();
        helpers.cleanupParseAndCanonical(allocator, self.resources);
    }
};

fn compileExecutableProgram(
    allocator: std.mem.Allocator,
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource,
) !CompiledExecutableProgram {
    var resources = try helpers.parseAndCanonicalizeProgram(allocator, source_kind, source, imports);
    errdefer helpers.cleanupParseAndCanonical(allocator, resources);

    var mono_lowerer = try monotype.Lower.Lowerer.init(allocator, &resources.typed_cir_modules, 1, null);
    defer mono_lowerer.deinit();
    var mono = try mono_lowerer.run(0);
    defer mono.deinit();
    var lifted = try monotype_lifted.Lower.run(allocator, &mono);
    defer lifted.deinit();
    var solved = try lambdasolved.Lower.run(allocator, &lifted);
    defer solved.deinit();
    const executable = try lambdamono.Lower.run(allocator, &solved);

    return .{
        .resources = resources,
        .executable = executable,
    };
}

fn compileIrProgram(
    allocator: std.mem.Allocator,
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource,
) !CompiledIrProgram {
    var resources = try helpers.parseAndCanonicalizeProgram(allocator, source_kind, source, imports);
    errdefer helpers.cleanupParseAndCanonical(allocator, resources);

    var mono_lowerer = try monotype.Lower.Lowerer.init(allocator, &resources.typed_cir_modules, 1, null);
    defer mono_lowerer.deinit();
    var mono = try mono_lowerer.run(0);
    defer mono.deinit();
    var lifted = try monotype_lifted.Lower.run(allocator, &mono);
    defer lifted.deinit();
    var solved = try lambdasolved.Lower.run(allocator, &lifted);
    defer solved.deinit();
    var executable = try lambdamono.Lower.run(allocator, &solved);
    defer executable.deinit();
    const ir_result = try ir.Lower.run(allocator, &executable);

    return .{
        .resources = resources,
        .ir_result = ir_result,
    };
}

fn countExecutableDirectCalls(executable: *const lambdamono.Lower.Result) usize {
    var count: usize = 0;
    for (executable.store.exprs.items) |expr| {
        switch (expr.data) {
            .call => count += 1,
            else => {},
        }
    }
    return count;
}

fn countExecutableErasedCalls(executable: *const lambdamono.Lower.Result) usize {
    var count: usize = 0;
    for (executable.store.exprs.items) |expr| {
        switch (expr.data) {
            .call_erased => count += 1,
            else => {},
        }
    }
    return count;
}

fn countExecutablePackedFns(executable: *const lambdamono.Lower.Result) usize {
    var count: usize = 0;
    for (executable.store.exprs.items) |expr| {
        switch (expr.data) {
            .packed_fn => count += 1,
            else => {},
        }
    }
    return count;
}

fn countExecutableErasedFnTypes(executable: *const lambdamono.Lower.Result) usize {
    var count: usize = 0;
    for (executable.types.types.items) |content| {
        switch (content) {
            .erased_fn => count += 1,
            else => {},
        }
    }
    return count;
}

fn executableTypeContainsReferencedErasedPrimitiveRec(
    executable: *const lambdamono.Lower.Result,
    ty: lambdamono.Type.TypeId,
    visited: *std.AutoHashMap(lambdamono.Type.TypeId, void),
) std.mem.Allocator.Error!bool {
    const gop = try visited.getOrPut(ty);
    if (gop.found_existing) return false;

    return switch (executable.types.getTypePreservingNominal(ty)) {
        .placeholder, .unbd => false,
        .link => unreachable,
        .primitive => |prim| prim == .erased,
        .nominal => |nominal| blk: {
            for (nominal.args) |arg| {
                if (try executableTypeContainsReferencedErasedPrimitiveRec(executable, arg, visited)) {
                    break :blk true;
                }
            }
            break :blk try executableTypeContainsReferencedErasedPrimitiveRec(executable, nominal.backing, visited);
        },
        .list => |elem| try executableTypeContainsReferencedErasedPrimitiveRec(executable, elem, visited),
        .box => |elem| try executableTypeContainsReferencedErasedPrimitiveRec(executable, elem, visited),
        .erased_fn => |erased_fn| blk: {
            if (erased_fn.capture) |capture| {
                if (try executableTypeContainsReferencedErasedPrimitiveRec(executable, capture, visited)) {
                    break :blk true;
                }
            }
            for (erased_fn.call.args) |arg| {
                if (try executableTypeContainsReferencedErasedPrimitiveRec(executable, arg, visited)) {
                    break :blk true;
                }
            }
            break :blk try executableTypeContainsReferencedErasedPrimitiveRec(executable, erased_fn.call.ret, visited);
        },
        .tuple => |elems| blk: {
            for (elems) |elem| {
                if (try executableTypeContainsReferencedErasedPrimitiveRec(executable, elem, visited)) {
                    break :blk true;
                }
            }
            break :blk false;
        },
        .record => |record| blk: {
            for (record.fields) |field| {
                if (try executableTypeContainsReferencedErasedPrimitiveRec(executable, field.ty, visited)) {
                    break :blk true;
                }
            }
            break :blk false;
        },
        .tag_union => |tag_union| blk: {
            for (tag_union.tags) |tag| {
                for (tag.args) |arg| {
                    if (try executableTypeContainsReferencedErasedPrimitiveRec(executable, arg, visited)) {
                        break :blk true;
                    }
                }
            }
            if (tag_union.call) |call| {
                for (call.args) |arg| {
                    if (try executableTypeContainsReferencedErasedPrimitiveRec(executable, arg, visited)) {
                        break :blk true;
                    }
                }
                if (try executableTypeContainsReferencedErasedPrimitiveRec(executable, call.ret, visited)) {
                    break :blk true;
                }
            }
            break :blk false;
        },
    };
}

fn executableTypeContainsReferencedErasedPrimitive(
    allocator: std.mem.Allocator,
    executable: *const lambdamono.Lower.Result,
    ty: lambdamono.Type.TypeId,
) std.mem.Allocator.Error!bool {
    var visited = std.AutoHashMap(lambdamono.Type.TypeId, void).init(allocator);
    defer visited.deinit();
    return try executableTypeContainsReferencedErasedPrimitiveRec(executable, ty, &visited);
}

fn countExecutableReferencedErasedPrimitiveTypes(
    allocator: std.mem.Allocator,
    executable: *const lambdamono.Lower.Result,
) std.mem.Allocator.Error!usize {
    var count: usize = 0;

    for (executable.store.exprs.items) |expr| {
        if (try executableTypeContainsReferencedErasedPrimitive(allocator, executable, expr.ty)) {
            count += 1;
        }
    }

    for (executable.store.pats.items) |pat| {
        if (try executableTypeContainsReferencedErasedPrimitive(allocator, executable, pat.ty)) {
            count += 1;
        }
    }

    for (executable.store.defsSlice()) |def| {
        if (def.result_ty) |result_ty| {
            if (try executableTypeContainsReferencedErasedPrimitive(allocator, executable, result_ty)) {
                count += 1;
            }
        }
        switch (def.value) {
            .fn_ => |fn_def| {
                for (executable.store.sliceTypedSymbolSpan(fn_def.args)) |arg| {
                    if (try executableTypeContainsReferencedErasedPrimitive(allocator, executable, arg.ty)) {
                        count += 1;
                    }
                }
            },
            .hosted_fn => |hosted_fn| {
                for (executable.store.sliceTypedSymbolSpan(hosted_fn.args)) |arg| {
                    if (try executableTypeContainsReferencedErasedPrimitive(allocator, executable, arg.ty)) {
                        count += 1;
                    }
                }
            },
            .val, .run => {},
        }
    }

    return count;
}

fn countExecutableBridgeNodes(executable: *const lambdamono.Lower.Result) usize {
    var count: usize = 0;
    for (executable.store.exprs.items) |expr| {
        switch (expr.data) {
            .bridge => count += 1,
            else => {},
        }
    }
    return count;
}

fn countUniqueDirectCallTargetsNamed(
    allocator: std.mem.Allocator,
    executable: *const lambdamono.Lower.Result,
) !usize {
    var seen = std.AutoHashMap(u32, void).init(allocator);
    defer seen.deinit();

    for (executable.store.exprs.items) |expr| {
        switch (expr.data) {
            .call => |call| {
                const entry = executable.symbols.get(call.proc);
                switch (entry.origin) {
                    .specialized_local_fn => {},
                    else => continue,
                }
                try seen.put(call.proc.raw(), {});
            },
            else => {},
        }
    }

    return seen.count();
}

fn countIrBridgeNodes(ir_result: *const ir.Lower.Result) usize {
    var count: usize = 0;
    for (ir_result.store.exprs.items) |expr| {
        switch (expr) {
            .bridge => count += 1,
            else => {},
        }
    }
    return count;
}

fn debugPrintExecutableType(
    executable: *const lambdamono.Lower.Result,
    ty: lambdamono.Type.TypeId,
) void {
    const content = executable.types.getTypePreservingNominal(ty);
    std.debug.print("{s}#{d}", .{ @tagName(content), @intFromEnum(ty) });
    switch (content) {
        .primitive => |prim| std.debug.print("({s})", .{@tagName(prim)}),
        .list => |elem| std.debug.print("(elem=#{d})", .{@intFromEnum(elem)}),
        .box => |elem| std.debug.print("(elem=#{d})", .{@intFromEnum(elem)}),
        .erased_fn => |erased_fn| {
            if (erased_fn.capture) |capture| {
                std.debug.print("(capture=#{d})", .{@intFromEnum(capture)});
            } else {
                std.debug.print("(capture=none)", .{});
            }
        },
        .tuple => |elems| {
            std.debug.print("(len={d}", .{elems.len});
            for (elems) |elem| {
                std.debug.print(" #{d}", .{@intFromEnum(elem)});
            }
            std.debug.print(")", .{});
        },
        .nominal => |nominal| {
            const backing = executable.types.getTypePreservingNominal(nominal.backing);
            std.debug.print("(module={d} ident={any} backing={s}#{d}", .{
                nominal.module_idx,
                nominal.ident,
                @tagName(backing),
                @intFromEnum(nominal.backing),
            });
            for (nominal.args) |arg| {
                std.debug.print(" arg=#{d}", .{@intFromEnum(arg)});
            }
            switch (backing) {
                .tag_union => |tag_union| {
                    for (tag_union.tags, 0..) |tag, i| {
                        std.debug.print(" [{d}:{s}", .{ i, @tagName(tag.name) });
                        switch (tag.name) {
                            .ctor => |name| std.debug.print(" ctor={any}", .{name}),
                            .lambda => |lambda| std.debug.print(" lambda={d}", .{lambda.raw()}),
                        }
                        for (tag.args) |arg| {
                            std.debug.print(" arg=#{d}", .{@intFromEnum(arg)});
                        }
                        std.debug.print("]", .{});
                    }
                },
                else => {},
            }
            std.debug.print(")", .{});
        },
        .record => |record| {
            std.debug.print("(fields={d}", .{record.fields.len});
            for (record.fields) |field| {
                std.debug.print(" {any}:#{d}", .{
                    field.name,
                    @intFromEnum(field.ty),
                });
            }
            std.debug.print(")", .{});
        },
        .tag_union => |tag_union| {
            std.debug.print("(tags={d}", .{tag_union.tags.len});
            for (tag_union.tags, 0..) |tag, i| {
                std.debug.print(" [{d}:{s}", .{ i, @tagName(tag.name) });
                switch (tag.name) {
                    .ctor => |name| std.debug.print(" ctor={any}", .{name}),
                    .lambda => |lambda| std.debug.print(" lambda={d}", .{lambda.raw()}),
                }
                for (tag.args) |arg| {
                    std.debug.print(" arg=#{d}", .{@intFromEnum(arg)});
                }
                std.debug.print("]", .{});
            }
            std.debug.print(")", .{});
        },
        .placeholder, .unbd, .link => {},
    }
}

fn debugPrintExecutableExprs(executable: *const lambdamono.Lower.Result) void {
    std.debug.print("\n== executable exprs ==\n", .{});
    for (executable.store.exprs.items, 0..) |expr, i| {
        std.debug.print("expr[{d}] tag={s} ty=", .{ i, @tagName(std.meta.activeTag(expr.data)) });
        debugPrintExecutableType(executable, expr.ty);
        switch (expr.data) {
            .bridge => |source| {
                const source_expr = executable.store.getExpr(source);
                std.debug.print(" source={d} source_tag={s} source_ty=", .{
                    @intFromEnum(source),
                    @tagName(std.meta.activeTag(source_expr.data)),
                });
                debugPrintExecutableType(executable, source_expr.ty);
            },
            .call => |call| {
                std.debug.print(" proc={d} args_len={d}", .{ call.proc.raw(), call.args.len });
            },
            .tag => |tag| {
                std.debug.print(" discr={d} args_len={d}", .{ tag.discriminant, tag.args.len });
            },
            .tag_payload => |payload| {
                std.debug.print(" union={d} discr={d} payload={d}", .{
                    @intFromEnum(payload.tag_union),
                    payload.tag_discriminant,
                    payload.payload_index,
                });
            },
            .when => |when_expr| {
                std.debug.print(" cond={d} branches={d}", .{
                    @intFromEnum(when_expr.cond),
                    when_expr.branches.len,
                });
            },
            .let_ => |let_expr| {
                std.debug.print(" bind_sym={d} bind_ty=", .{let_expr.bind.symbol.raw()});
                debugPrintExecutableType(executable, let_expr.bind.ty);
                std.debug.print(" body={d} rest={d}", .{
                    @intFromEnum(let_expr.body),
                    @intFromEnum(let_expr.rest),
                });
            },
            .var_ => |symbol| {
                std.debug.print(" symbol={d}", .{symbol.raw()});
            },
            else => {},
        }
        std.debug.print("\n", .{});
    }
}

fn debugPrintIrLayoutRef(graph: *const layout.Graph, ref: ir.Ast.LayoutRef) void {
    switch (ref) {
        .canonical => |idx| std.debug.print("canonical#{d}", .{@intFromEnum(idx)}),
        .local => |node| {
            const layout_node = graph.getNode(node);
            std.debug.print("{s}#{d}", .{
                @tagName(layout_node),
                @intFromEnum(node),
            });
            switch (layout_node) {
                .nominal => |backing| {
                    std.debug.print("(backing=", .{});
                    debugPrintIrLayoutRef(graph, backing);
                    std.debug.print(")", .{});
                },
                .box => |child| {
                    std.debug.print("(child=", .{});
                    debugPrintIrLayoutRef(graph, child);
                    std.debug.print(")", .{});
                },
                .list => |child| {
                    std.debug.print("(elem=", .{});
                    debugPrintIrLayoutRef(graph, child);
                    std.debug.print(")", .{});
                },
                .closure => |child| {
                    std.debug.print("(capture=", .{});
                    debugPrintIrLayoutRef(graph, child);
                    std.debug.print(")", .{});
                },
                .struct_ => |fields| {
                    std.debug.print("(fields={d}", .{graph.getFields(fields).len});
                    for (graph.getFields(fields)) |field| {
                        std.debug.print(" {d}=", .{field.index});
                        debugPrintIrLayoutRef(graph, field.child);
                    }
                    std.debug.print(")", .{});
                },
                .tag_union => |refs| {
                    std.debug.print("(variants={d}", .{graph.getRefs(refs).len});
                    for (graph.getRefs(refs), 0..) |child, i| {
                        std.debug.print(" [{d}]=", .{i});
                        debugPrintIrLayoutRef(graph, child);
                    }
                    std.debug.print(")", .{});
                },
                .pending => {},
            }
        },
    }
}

fn debugPrintIrBridges(ir_result: *const ir.Lower.Result) void {
    std.debug.print("\n== ir bridge lets ==\n", .{});
    for (ir_result.store.stmts.items, 0..) |stmt, stmt_idx| {
        switch (stmt) {
            .let_ => |let_stmt| {
                const expr = ir_result.store.getExpr(let_stmt.expr);
                switch (expr) {
                    .bridge => |source| {
                        std.debug.print("stmt[{d}] bind_sym={d} bind_layout=", .{
                            stmt_idx,
                            let_stmt.bind.symbol.raw(),
                        });
                        debugPrintIrLayoutRef(&ir_result.layouts, let_stmt.bind.layout);
                        std.debug.print(" source_sym={d} source_layout=", .{source.value.symbol.raw()});
                        debugPrintIrLayoutRef(&ir_result.layouts, source.value.layout);
                        std.debug.print("\n", .{});
                    },
                    else => {},
                }
            },
            else => {},
        }
    }
}

fn debugPrintBridgeShapesForSource(
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, source_kind, source, imports);
    defer executable.deinit(arena_allocator);
    debugPrintExecutableExprs(&executable.executable);
    std.debug.print("executable bridge count={d}\n", .{countExecutableBridgeNodes(&executable.executable)});

    var ir_program = try compileIrProgram(arena_allocator, source_kind, source, imports);
    defer ir_program.deinit(arena_allocator);
    debugPrintIrBridges(&ir_program.ir_result);
    std.debug.print("ir bridge count={d}\n", .{countIrBridgeNodes(&ir_program.ir_result)});
}

fn countSpecializedLocalFnDefs(executable: *const lambdamono.Lower.Result) usize {
    var count: usize = 0;
    for (executable.store.defsSlice()) |def| {
        const origin = executable.symbols.get(def.bind).origin;
        switch (origin) {
            .specialized_local_fn => count += 1,
            else => {},
        }
    }
    return count;
}

fn collectSpecializedLocalFnResultTypes(
    allocator: std.mem.Allocator,
    executable: *const lambdamono.Lower.Result,
) ![]const lambdamono.Type.TypeId {
    var out = std.ArrayList(lambdamono.Type.TypeId).empty;
    errdefer out.deinit(allocator);

    for (executable.store.defsSlice()) |def| {
        const origin = executable.symbols.get(def.bind).origin;
        switch (origin) {
            .specialized_local_fn => {
                const result_ty = def.result_ty orelse
                    @panic("cor pipeline test expected specialized def result type");
                try out.append(allocator, result_ty);
            },
            else => {},
        }
    }

    return try out.toOwnedSlice(allocator);
}

fn collectSpecializedLocalFnFirstArgTypes(
    allocator: std.mem.Allocator,
    executable: *const lambdamono.Lower.Result,
) ![]const lambdamono.Type.TypeId {
    var out = std.ArrayList(lambdamono.Type.TypeId).empty;
    errdefer out.deinit(allocator);

    for (executable.store.defsSlice()) |def| {
        const origin = executable.symbols.get(def.bind).origin;
        switch (origin) {
            .specialized_local_fn => switch (def.value) {
                .fn_ => |fn_def| {
                    const args = executable.store.sliceTypedSymbolSpan(fn_def.args);
                    if (args.len == 0) {
                        @panic("cor pipeline test expected specialized local fn to have at least one arg");
                    }
                    try out.append(allocator, args[0].ty);
                },
                else => @panic("cor pipeline test expected specialized local fn def"),
            },
            else => {},
        }
    }

    return try out.toOwnedSlice(allocator);
}

fn expectDirectOnlyLoweringProgram(
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource,
    expected_executable_direct_calls: usize,
    expected_lir_direct_calls: usize,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, source_kind, source, imports);
    defer executable.deinit(arena_allocator);

    const executable_direct = countExecutableDirectCalls(&executable.executable);
    const executable_erased_calls = countExecutableErasedCalls(&executable.executable);
    const executable_packed = countExecutablePackedFns(&executable.executable);
    const executable_erased = countExecutableErasedFnTypes(&executable.executable);

    var compiled = try helpers.compileProgram(arena_allocator, source_kind, source, imports);
    defer compiled.deinit(arena_allocator);

    const lir_direct = countDirectCalls(&compiled);
    const lir_erased_calls = countErasedCalls(&compiled);
    try testing.expectEqual(expected_executable_direct_calls, executable_direct);
    try testing.expectEqual(@as(usize, 0), executable_erased_calls);
    try testing.expectEqual(@as(usize, 0), executable_packed);
    try testing.expectEqual(@as(usize, 0), executable_erased);
    try testing.expectEqual(expected_lir_direct_calls, lir_direct);
    try testing.expectEqual(@as(usize, 0), lir_erased_calls);
}

fn expectErasedLoweringProgram(
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource,
    expected_executable_erased_calls: usize,
    expected_executable_packed_fns: usize,
    expected_executable_erased_fn_types: usize,
    expected_lir_erased_calls: usize,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, source_kind, source, imports);
    defer executable.deinit(arena_allocator);

    const executable_erased_calls = countExecutableErasedCalls(&executable.executable);
    const executable_packed = countExecutablePackedFns(&executable.executable);
    const executable_erased = countExecutableErasedFnTypes(&executable.executable);

    var compiled = try helpers.compileProgram(arena_allocator, source_kind, source, imports);
    defer compiled.deinit(arena_allocator);

    const lir_erased_calls = countErasedCalls(&compiled);
    try testing.expectEqual(expected_executable_erased_calls, executable_erased_calls);
    try testing.expectEqual(expected_executable_packed_fns, executable_packed);
    try testing.expectEqual(expected_executable_erased_fn_types, executable_erased);
    try testing.expectEqual(expected_lir_erased_calls, lir_erased_calls);
}

fn expectNoErasedLoweringProgram(
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource,
    expected: []const u8,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, source_kind, source, imports);
    defer executable.deinit(arena_allocator);

    const executable_erased_calls = countExecutableErasedCalls(&executable.executable);
    const executable_packed = countExecutablePackedFns(&executable.executable);
    const executable_erased = countExecutableErasedFnTypes(&executable.executable);

    var compiled = if (source_kind == .expr and imports.len == 0)
        try helpers.compileInspectedExpr(arena_allocator, source)
    else
        try helpers.compileProgram(arena_allocator, source_kind, source, imports);
    defer compiled.deinit(arena_allocator);

    const actual = try helpers.lirInterpreterInspectedStr(arena_allocator, &compiled.lowered);
    const lir_erased_calls = countErasedCalls(&compiled);

    try testing.expectEqualStrings(expected, actual);
    try testing.expectEqual(@as(usize, 0), executable_erased_calls);
    try testing.expectEqual(@as(usize, 0), executable_packed);
    try testing.expectEqual(@as(usize, 0), executable_erased);
    try testing.expectEqual(@as(usize, 0), lir_erased_calls);
}

const DirectCallCase = struct {
    name: []const u8,
    source_kind: helpers.SourceKind = .module,
    source: []const u8,
    imports: []const helpers.ModuleSource = &.{},
    expected: []const u8,
    executable_direct_calls: usize,
    lir_direct_calls: usize,
};

const annotated_callback_param_source =
    \\apply : (I64 -> I64), I64 -> I64
    \\apply = |f, x| f(x)
    \\
    \\main = apply(|n| n + 1.I64, 41.I64)
;

const annotated_return_source =
    \\make_adder : I64 -> (I64 -> I64)
    \\make_adder = |n| |x| x + n
    \\
    \\main = make_adder(1.I64)(41.I64)
;

const abstract_apply_source =
    \\main =
    \\    {
    \\        apply = |f, x| f(x)
    \\        apply(|n| n + 1.I64, 41.I64)
    \\    }
;

const nested_polymorphic_helper_source =
    \\main = ((|f| (|x| f(x)))(|n| n + 1.I64))(41.I64)
;

const apply_twice_source =
    \\main =
    \\    {
    \\        twice = |f, x| f(f(x))
    \\        twice(|n| n + 1.I64, 40.I64)
    \\    }
;

const annotated_local_binding_source =
    \\main =
    \\    {
    \\        f : I64 -> I64
    \\        f = |x| x + 1.I64
    \\        f(41.I64)
    \\    }
;

const captured_callback_param_source =
    \\main =
    \\    {
    \\        apply = |f, x| f(x)
    \\        y = 10.I64
    \\        apply(|x| x + y, 32.I64)
    \\    }
;

const cross_module_captured_callback_param_source =
    \\import Helpers
    \\
    \\main =
    \\    {
    \\        y = 10.I64
    \\        Helpers.apply(|x| x + y, 32.I64)
    \\    }
;

const cross_module_captured_callback_param_imports = [_]helpers.ModuleSource{
    .{
        .name = "Helpers",
        .source =
        \\module [apply]
        \\
        \\apply = |f, x| f(x)
        ,
    },
};

const two_callback_params_source =
    \\main =
    \\    {
    \\        combine = |f, g, x| f(x) + g(x)
    \\        a = 10.I64
    \\        b = 20.I64
    \\        combine(|x| x + a, |x| x + b, 6.I64)
    \\    }
;

const record_field_closure_extraction_source =
    \\main =
    \\    {
    \\        a = 10.I64
    \\        b = 20.I64
    \\        rec = { add_a: |x| x + a, add_b: |x| x + b }
    \\        add_a = rec.add_a
    \\        add_a(32.I64)
    \\    }
;

const nonboxed_polymorphic_closure_round_trip_source =
    \\{
    \\    make_const = |value| |_| value
    \\    num_f = make_const(41)
    \\    str_f = make_const("ok")
    \\    { n: num_f({}), s: str_f({}) }
    \\}
;

const nonboxed_polymorphic_closure_helper_round_trip_source =
    \\{
    \\    make_const = |value| |_| value
    \\    keep = |f| f
    \\    num_f = keep(make_const(41))
    \\    str_f = keep(make_const("ok"))
    \\    { n: num_f({}), s: str_f({}) }
    \\}
;

const boxed_lambda_helper_chain_source =
    \\{
    \\    make = |n| |x| x + n
    \\    wrap = |boxed| { value: boxed }
    \\    unwrap = |record| record.value
    \\    f = Box.unbox(unwrap(wrap(Box.box(make(5)))))
    \\    f(1) + f(2)
    \\}
;

const opaque_function_field_lookup_source =
    \\W(a) := { f : {} -> [V(a)] }.{
    \\    run : W(a) -> [V(a)]
    \\    run = |w| (w.f)({})
    \\
    \\    mk : a -> W(a)
    \\    mk = |val| { f: |_| V(val) }
    \\}
    \\
    \\main = W.run(W.mk("x")) == V("x")
;

const additional_specialization_via_list_append_source =
    \\main =
    \\    {
    \\        append_one = |acc, x| List.append(acc, x)
    \\        _first_len = List.fold([1.I64, 2.I64], List.with_capacity(1), append_one).len()
    \\        List.fold([[1.I64, 2.I64], [3.I64, 4.I64]], List.with_capacity(1), append_one).len()
    \\    }
;

const polymorphic_two_list_types_source =
    \\main =
    \\    {
    \\        my_len = |list| list.len()
    \\        a : List(I64)
    \\        a = [1, 2, 3]
    \\        b : List(Str)
    \\        b = ["x", "y"]
    \\        my_len(a) + my_len(b)
    \\    }
;

const simple_direct_call_source =
    \\main =
    \\    {
    \\        add_one = |x| x + 1.I64
    \\        add_one(41.I64)
    \\    }
;

const typed_nested_captures_source =
    \\(|y| (|x| (|z| x + y + z)(3.I64))(2.I64))(1.I64)
;

const cross_module_annotated_callback_param_source =
    \\import Helpers
    \\
    \\main = Helpers.apply(|n| n + 1.I64, 41.I64)
;

const cross_module_annotated_callback_param_imports = [_]helpers.ModuleSource{
    .{
        .name = "Helpers",
        .source =
        \\module [apply]
        \\
        \\apply : (I64 -> I64), I64 -> I64
        \\apply = |f, x| f(x)
        ,
    },
};

const cross_module_annotated_return_source =
    \\import Makers
    \\
    \\main = Makers.make_adder(1.I64)(41.I64)
;

const cross_module_annotated_return_imports = [_]helpers.ModuleSource{
    .{
        .name = "Makers",
        .source =
        \\module [make_adder]
        \\
        \\make_adder : I64 -> (I64 -> I64)
        \\make_adder = |n| |x| x + n
        ,
    },
};

const cross_module_abstract_apply_source =
    \\import Helpers
    \\
    \\main = Helpers.apply(|n| n + 1.I64, 41.I64)
;

const cross_module_abstract_apply_imports = [_]helpers.ModuleSource{
    .{
        .name = "Helpers",
        .source =
        \\module [apply]
        \\
        \\apply = |f, x| f(x)
        ,
    },
};

const cross_module_nested_bridge_source =
    \\import Helpers
    \\
    \\main = Helpers.bridge(|n| n + 1.I64)(41.I64)
;

const cross_module_nested_bridge_imports = [_]helpers.ModuleSource{
    .{
        .name = "Helpers",
        .source =
        \\module [bridge]
        \\
        \\bridge = |f| (|x| f(x))
        ,
    },
};

const cross_module_apply_twice_source =
    \\import Helpers
    \\
    \\main = Helpers.twice(|n| n + 1.I64, 40.I64)
;

const cross_module_apply_twice_imports = [_]helpers.ModuleSource{
    .{
        .name = "Helpers",
        .source =
        \\module [twice]
        \\
        \\twice = |f, x| f(f(x))
        ,
    },
};

const direct_call_cases = [_]DirectCallCase{
    .{
        .name = "annotated callback parameter slot",
        .source = annotated_callback_param_source,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "annotated function return slot",
        .source = annotated_return_source,
        .expected = "42",
        .executable_direct_calls = 2,
        .lir_direct_calls = 9,
    },
    .{
        .name = "abstract higher-order apply",
        .source = abstract_apply_source,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "nested polymorphic helper",
        .source = nested_polymorphic_helper_source,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "apply twice",
        .source = apply_twice_source,
        .expected = "42",
        .executable_direct_calls = 6,
        .lir_direct_calls = 15,
    },
    .{
        .name = "annotated local binding",
        .source = annotated_local_binding_source,
        .expected = "42",
        .executable_direct_calls = 1,
        .lir_direct_calls = 6,
    },
    .{
        .name = "passing captured closure to callback parameter",
        .source = captured_callback_param_source,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "annotated return of concrete lambda",
        .source = annotated_return_source,
        .expected = "42",
        .executable_direct_calls = 2,
        .lir_direct_calls = 9,
    },
    .{
        .name = "passing concrete lambda to annotated callback parameter",
        .source = annotated_callback_param_source,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "cross-module annotated callback parameter slot",
        .source = cross_module_annotated_callback_param_source,
        .imports = &cross_module_annotated_callback_param_imports,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "cross-module annotated function return slot",
        .source = cross_module_annotated_return_source,
        .imports = &cross_module_annotated_return_imports,
        .expected = "42",
        .executable_direct_calls = 2,
        .lir_direct_calls = 9,
    },
    .{
        .name = "cross-module abstract higher-order apply",
        .source = cross_module_abstract_apply_source,
        .imports = &cross_module_abstract_apply_imports,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "cross-module nested higher-order bridge",
        .source = cross_module_nested_bridge_source,
        .imports = &cross_module_nested_bridge_imports,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "cross-module apply twice",
        .source = cross_module_apply_twice_source,
        .imports = &cross_module_apply_twice_imports,
        .expected = "42",
        .executable_direct_calls = 6,
        .lir_direct_calls = 15,
    },
    .{
        .name = "cross-module passing captured closure to callback parameter",
        .source = cross_module_captured_callback_param_source,
        .imports = &cross_module_captured_callback_param_imports,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "two callback params with different captures",
        .source = two_callback_params_source,
        .expected = "42",
        .executable_direct_calls = 7,
        .lir_direct_calls = 18,
    },
    .{
        .name = "record field closure extraction then call",
        .source = record_field_closure_extraction_source,
        .expected = "42",
        .executable_direct_calls = 1,
        .lir_direct_calls = 6,
    },
};

const ErasedCallCase = struct {
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource = &.{},
    expected_executable_erased_calls: usize,
    expected_executable_packed_fns: usize,
    expected_executable_erased_fn_types: usize,
    expected_lir_erased_calls: usize,
};

const boxed_lambda_round_trip_erased_case = ErasedCallCase{
    .source_kind = .expr,
    .source =
    \\{
    \\    wrap = |boxed| { value: boxed }
    \\    unwrap = |record| record.value
    \\    f = Box.unbox(unwrap(wrap(Box.box(|x| x + 1.I64))))
    \\    f(41.I64)
    \\}
    ,
    .expected_executable_erased_calls = 2,
    .expected_executable_packed_fns = 1,
    .expected_executable_erased_fn_types = 1,
    .expected_lir_erased_calls = 3,
};

fn countHostedProcSpecs(compiled: *const helpers.CompiledProgram) usize {
    var count: usize = 0;
    for (compiled.lowered.lir_result.store.getProcSpecs()) |proc| {
        if (proc.hosted != null) count += 1;
    }
    return count;
}

fn countHostedProcSpecsWithoutBody(compiled: *const helpers.CompiledProgram) usize {
    var count: usize = 0;
    for (compiled.lowered.lir_result.store.getProcSpecs()) |proc| {
        if (proc.hosted != null and proc.body == null) count += 1;
    }
    return count;
}

fn echoHostedFn(ops_raw: *anyopaque, _: *anyopaque, args_raw: *anyopaque) callconv(.c) void {
    const ops: *builtins.host_abi.RocOps = @ptrCast(@alignCast(ops_raw));
    const roc_str: *const builtins.str.RocStr = @ptrCast(@alignCast(args_raw));
    ops.dbg(roc_str.asSlice());
}

fn tickHostedFn(ops_raw: *anyopaque, _: *anyopaque, _: *anyopaque) callconv(.c) void {
    const ops: *builtins.host_abi.RocOps = @ptrCast(@alignCast(ops_raw));
    ops.dbg("tick");
}

const PairArgs = extern struct {
    first: builtins.str.RocStr,
    second: builtins.str.RocStr,
};

fn pairHostedFn(ops_raw: *anyopaque, _: *anyopaque, args_raw: *anyopaque) callconv(.c) void {
    const ops: *builtins.host_abi.RocOps = @ptrCast(@alignCast(ops_raw));
    const args: *const PairArgs = @ptrCast(@alignCast(args_raw));
    ops.dbg(args.first.asSlice());
    ops.dbg(args.second.asSlice());
}

fn attachHostedFns(runtime_env: *RuntimeHostEnv, hosted_fns: []const builtins.host_abi.HostedFn) void {
    const ops = runtime_env.get_ops();
    ops.hosted_fns = .{
        .count = @intCast(hosted_fns.len),
        .fns = @ptrCast(@constCast(hosted_fns.ptr)),
    };
}

fn runModuleWithInterpreter(
    allocator: std.mem.Allocator,
    compiled: *const helpers.CompiledProgram,
    hosted_fns: []const builtins.host_abi.HostedFn,
) !RuntimeHostEnv.RecordedRun {
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    attachHostedFns(&runtime_env, hosted_fns);

    var interp = try Interpreter.init(
        allocator,
        &compiled.lowered.lir_result.store,
        &compiled.lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interp.deinit();

    const arg_layouts = try mainProcArgLayouts(allocator, compiled);
    defer allocator.free(arg_layouts);
    _ = try interp.eval(.{
        .proc_id = compiled.lowered.main_proc,
        .arg_layouts = arg_layouts,
    });
    return try runtime_env.snapshot(allocator);
}

fn runModuleWithDevBackend(
    allocator: std.mem.Allocator,
    compiled: *const helpers.CompiledProgram,
    hosted_fns: []const builtins.host_abi.HostedFn,
) !RuntimeHostEnv.RecordedRun {
    var codegen = try HostLirCodeGen.init(
        allocator,
        &compiled.lowered.lir_result.store,
        &compiled.lowered.lir_result.layouts,
        null,
    );
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(compiled.lowered.lir_result.store.getProcSpecs());

    const proc = compiled.lowered.lir_result.store.getProcSpec(compiled.lowered.main_proc);
    const arg_layouts = try mainProcArgLayouts(allocator, compiled);
    defer allocator.free(arg_layouts);
    const entrypoint = try codegen.generateEntrypointWrapper(
        "roc_eval_hosted_test_main",
        compiled.lowered.main_proc,
        arg_layouts,
        proc.ret_layout,
    );
    var exec_mem = try ExecutableMemory.initWithEntryOffset(
        codegen.getGeneratedCode(),
        entrypoint.offset,
    );
    defer exec_mem.deinit();

    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    attachHostedFns(&runtime_env, hosted_fns);

    const ret_layout = proc.ret_layout;
    const size_align = compiled.lowered.lir_result.layouts.layoutSizeAlign(
        compiled.lowered.lir_result.layouts.getLayout(ret_layout),
    );
    const alloc_len = @max(size_align.size, 1);
    const ret_buf = try allocator.alignedAlloc(u8, collections.max_roc_alignment, alloc_len);
    defer allocator.free(ret_buf);
    @memset(ret_buf, 0);

    const arg_buf = try zeroedEntrypointArgBuffer(allocator, compiled, arg_layouts);
    defer if (arg_buf) |buf| allocator.free(buf);

    exec_mem.callRocABI(
        @ptrCast(runtime_env.get_ops()),
        @ptrCast(ret_buf.ptr),
        if (arg_buf) |buf| @ptrCast(buf.ptr) else null,
    );
    return try runtime_env.snapshot(allocator);
}

test "cor pipeline - recursive lambda factorial" {
    try expectInspect(
        \\{
        \\    factorial = |n| if (n <= 1.I64) 1.I64 else n * factorial(n - 1.I64)
        \\    factorial(5.I64)
        \\}
    ,
        "120",
    );
}

test "cor pipeline - mutual recursion in local lambdas" {
    try expectInspect(
        \\{
        \\    is_even = |n| if (n == 0.I64) True else is_odd(n - 1.I64)
        \\    is_odd = |n| if (n == 0.I64) False else is_even(n - 1.I64)
        \\    is_even(6.I64)
        \\}
    ,
        "True",
    );
}

test "cor pipeline - for loop sums list" {
    try expectInspect(
        \\{
        \\    var $sum = 0.I64
        \\    for item in [10.I64, 20.I64, 30.I64] {
        \\        $sum = $sum + item
        \\    }
        \\    $sum
        \\}
    ,
        "60",
    );
}

test "cor pipeline - for loop in lambda body" {
    try expectInspect(
        \\{
        \\    sum = |xs| {
        \\        var $sum = 0.I64
        \\        for item in xs {
        \\            $sum = $sum + item
        \\        }
        \\        $sum
        \\    }
        \\    sum([1.I64, 2.I64, 3.I64, 4.I64])
        \\}
    ,
        "10",
    );
}

test "cor pipeline - generic local attached method specialization on nominal" {
    try expectInspectProgram(
        .module,
        \\Counter := [Counter(U64)].{
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Counter.Counter(5)), read(Counter.Counter(8)))
    ,
        &.{},
        "(5, 8)",
    );
}

test "cor pipeline - generic local attached method specialization on nominal with arena allocator" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    var compiled = try helpers.compileInspectedProgram(
        allocator,
        .module,
        \\Counter := [Counter(U64)].{
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Counter.Counter(5)), read(Counter.Counter(8)))
    ,
        &.{},
    );
    defer compiled.deinit(allocator);

    const actual = try helpers.lirInterpreterInspectedStr(allocator, &compiled.lowered);
    try testing.expectEqualStrings("(5, 8)", actual);
}

test "cor pipeline - generic local attached method specialization on nominal dev backend" {
    var compiled = try helpers.compileInspectedProgram(
        testing.allocator,
        .module,
        \\Counter := [Counter(U64)].{
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Counter.Counter(5)), read(Counter.Counter(8)))
    ,
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    const actual = try helpers.devEvaluatorInspectedStr(testing.allocator, &compiled.lowered);
    defer testing.allocator.free(actual);

    try testing.expectEqualStrings("(5, 8)", actual);
}

test "cor pipeline - generic local attached method specialization on nominal wasm backend" {
    var compiled = try helpers.compileInspectedProgram(
        testing.allocator,
        .module,
        \\Counter := [Counter(U64)].{
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Counter.Counter(5)), read(Counter.Counter(8)))
    ,
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    const actual = try helpers.wasmEvaluatorInspectedStr(testing.allocator, &compiled.wasm_lowered);
    defer testing.allocator.free(actual);

    try testing.expectEqualStrings("(5, 8)", actual);
}

test "cor pipeline - checked nominal method call rewrites to dispatch_call" {
    var resources = try helpers.parseAndCanonicalizeProgram(
        testing.allocator,
        .module,
        \\Counter := [Counter(U64)].{
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
        \\
        \\main = Counter.Counter(5).get()
    ,
        &.{},
    );
    defer helpers.cleanupParseAndCanonical(testing.allocator, resources);

    switch (resources.module_env.store.getExpr(resources.expr_idx)) {
        .e_dispatch_call => |call| {
            try testing.expectEqual(@as(usize, 0), resources.module_env.store.sliceExpr(call.args).len);
        },
        else => return error.UnexpectedCheckedDispatchShape,
    }
}

test "cor pipeline - checked record equality stays structural_eq" {
    var resources = try helpers.parseAndCanonicalizeProgram(
        testing.allocator,
        .expr,
        "({ x: 1.U64 } == { x: 1.U64 })",
        &.{},
    );
    defer helpers.cleanupParseAndCanonical(testing.allocator, resources);

    switch (resources.module_env.store.getExpr(resources.expr_idx)) {
        .e_structural_eq => {},
        else => return error.UnexpectedCheckedStructuralEqShape,
    }
}

test "cor pipeline - generic local attached method specialization stays non-erased" {
    var compiled = try compileExecutableProgram(
        testing.allocator,
        .module,
        \\Counter := [Counter(U64)].{
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Counter.Counter(5)), read(Counter.Counter(8)))
    ,
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 0), countExecutableErasedCalls(&compiled.executable));
    try testing.expect(countExecutableDirectCalls(&compiled.executable) >= 2);
}

test "cor pipeline - generic local attached method specialization picks different nominal targets" {
    try expectInspectProgram(
        .module,
        \\Box := [Box(U64)].{
        \\  get : Box -> U64
        \\  get = |Box.Box(n)| n
        \\}
        \\
        \\Count := [Count(U64)].{
        \\  get : Count -> U64
        \\  get = |Count.Count(n)| n + 100
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Box.Box(5)), read(Count.Count(8)))
    ,
        &.{},
        "(5, 108)",
    );
}

test "cor pipeline - cross-module attached method specialization on imported nominal" {
    try expectInspectProgram(
        .module,
        \\import CounterMod
        \\
        \\main = CounterMod.Counter(41).get()
    ,
        &.{.{
            .name = "CounterMod",
            .source =
            \\Counter := [Counter(U64)].{
            \\  get : Counter -> U64
            \\  get = |Counter.Counter(n)| n
            \\}
            ,
        }},
        "41",
    );
}

test "cor pipeline - cross-module polymorphic attached method specialization from helper module" {
    try expectInspectProgram(
        .module,
        \\import BoxMod
        \\import CountMod
        \\import Helpers
        \\
        \\main = (Helpers.read(BoxMod.Box(5)), Helpers.read(CountMod.Count(8)))
    ,
        &.{
            .{
                .name = "BoxMod",
                .source =
                \\Box := [Box(U64)].{
                \\  get : Box -> U64
                \\  get = |Box.Box(n)| n
                \\}
                ,
            },
            .{
                .name = "CountMod",
                .source =
                \\Count := [Count(U64)].{
                \\  get : Count -> U64
                \\  get = |Count.Count(n)| n + 100
                \\}
                ,
            },
            .{
                .name = "Helpers",
                .source =
                \\module [read]
                \\
                \\read = |value| value.get()
                ,
            },
        },
        "(5, 108)",
    );
}

test "cor pipeline - record field access remains separate from method calls" {
    try expectInspectProgram(
        .expr,
        \\{
        \\    record = { get: |n| n + 1, value: 41 }
        \\    getter = record.get
        \\    getter(record.value)
        \\}
    ,
        &.{},
        "42.0",
    );
}

test "cor pipeline - recursive tag payload match issue 8754" {
    try expectInspectProgram(
        .module,
        \\Tree := [Node(Str, List(Tree)), Text(Str), Wrapper(Tree)]
        \\
        \\inner : Tree
        \\inner = Text("hello")
        \\
        \\wrapped : Tree
        \\wrapped = Wrapper(inner)
        \\
        \\main = match wrapped {
        \\    Wrapper(inner_tree) =>
        \\        match inner_tree {
        \\            Text(_) => 1
        \\            Node(_, _) => 2
        \\            Wrapper(_) => 3
        \\        }
        \\    _ => 0
        \\}
    ,
        &.{},
        "1.0",
    );
}

test "cor pipeline - recursive lambda with record" {
    try expectInspect(
        \\{
        \\    f = |n|
        \\        if n <= 0.I64
        \\            0.I64
        \\        else
        \\            { a: n, b: n * 2.I64, c: n * 3.I64, d: n * 4.I64 }.a + f(n - 1.I64)
        \\    f(100.I64)
        \\}
    ,
        "5050",
    );
}

test "cor pipeline - for loop early return" {
    try expectInspect(
        \\{
        \\    f = |list| {
        \\        for _item in list {
        \\            if True { return True }
        \\        }
        \\        False
        \\    }
        \\    f([1.I64, 2.I64, 3.I64])
        \\}
    ,
        "True",
    );
}

test "cor pipeline - for loop closure early return" {
    try expectInspect(
        \\{
        \\    f = |list, pred| {
        \\        for item in list {
        \\            if pred(item) { return True }
        \\        }
        \\        False
        \\    }
        \\    f([1.I64, 2.I64, 3.I64], |_x| True)
        \\}
    ,
        "True",
    );
}

test "cor pipeline - inspect recursive nominal arithmetic expr" {
    try expectInspectProgramWithArena(
        .module,
        \\Arith := [Lit(I64), Add(Arith, Arith), Mul(Arith, Arith), Neg(Arith)]
        \\
        \\main = Arith.Mul(
        \\    Arith.Add(Arith.Lit(2), Arith.Lit(3)),
        \\    Arith.Neg(Arith.Lit(4))
        \\)
    ,
        &.{},
        "Mul(Add(Lit(2), Lit(3)), Neg(Lit(4)))",
    );
}

test "cor pipeline - inspect recursive nominal arithmetic expr dev backend" {
    var compiled = try helpers.compileInspectedProgram(
        testing.allocator,
        .module,
        \\Arith := [Lit(I64), Add(Arith, Arith), Mul(Arith, Arith), Neg(Arith)]
        \\
        \\main = Arith.Mul(
        \\    Arith.Add(Arith.Lit(2), Arith.Lit(3)),
        \\    Arith.Neg(Arith.Lit(4))
        \\)
    ,
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    const actual = try helpers.devEvaluatorInspectedStr(testing.allocator, &compiled.lowered);
    defer testing.allocator.free(actual);

    try testing.expectEqualStrings("Mul(Add(Lit(2), Lit(3)), Neg(Lit(4)))", actual);
}

test "cor pipeline - inspect recursive nominal arithmetic expr wasm backend" {
    var compiled = try helpers.compileInspectedProgram(
        testing.allocator,
        .module,
        \\Arith := [Lit(I64), Add(Arith, Arith), Mul(Arith, Arith), Neg(Arith)]
        \\
        \\main = Arith.Mul(
        \\    Arith.Add(Arith.Lit(2), Arith.Lit(3)),
        \\    Arith.Neg(Arith.Lit(4))
        \\)
    ,
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    const actual = try helpers.wasmEvaluatorInspectedStr(testing.allocator, &compiled.wasm_lowered);
    defer testing.allocator.free(actual);

    try testing.expectEqualStrings("Mul(Add(Lit(2), Lit(3)), Neg(Lit(4)))", actual);
}

test "cor pipeline - inspect recursive nominal dom element with text child" {
    var compiled = try helpers.compileInspectedProgram(
        testing.allocator,
        .module,
        \\Node := [Text(Str), Element(Str, List(Node))]
        \\
        \\main = Node.Element("p", [Node.Text("hello")])
    ,
        &.{},
    );
    defer compiled.deinit(testing.allocator);
}

test "cor pipeline - eval direct-only higher-order call annotated callback parameter slot" {
    const case = direct_call_cases[0];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call annotated callback parameter slot" {
    const case = direct_call_cases[0];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call annotated function return slot" {
    const case = direct_call_cases[1];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call annotated function return slot" {
    const case = direct_call_cases[1];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call abstract higher-order apply" {
    const case = direct_call_cases[2];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call abstract higher-order apply" {
    const case = direct_call_cases[2];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call nested polymorphic helper" {
    const case = direct_call_cases[3];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call nested polymorphic helper" {
    const case = direct_call_cases[3];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call apply twice" {
    const case = direct_call_cases[4];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call apply twice" {
    const case = direct_call_cases[4];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call annotated local binding" {
    const case = direct_call_cases[5];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call annotated local binding" {
    const case = direct_call_cases[5];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call passing captured closure to callback parameter" {
    const case = direct_call_cases[6];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call passing captured closure to callback parameter" {
    const case = direct_call_cases[6];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call annotated return of concrete lambda" {
    const case = direct_call_cases[7];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call annotated return of concrete lambda" {
    const case = direct_call_cases[7];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call passing concrete lambda to annotated callback parameter" {
    const case = direct_call_cases[8];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call passing concrete lambda to annotated callback parameter" {
    const case = direct_call_cases[8];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call cross-module annotated callback parameter slot" {
    const case = direct_call_cases[9];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call cross-module annotated callback parameter slot" {
    const case = direct_call_cases[9];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call cross-module annotated function return slot" {
    const case = direct_call_cases[10];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call cross-module annotated function return slot" {
    const case = direct_call_cases[10];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call cross-module abstract higher-order apply" {
    const case = direct_call_cases[11];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call cross-module abstract higher-order apply" {
    const case = direct_call_cases[11];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call cross-module nested higher-order bridge" {
    const case = direct_call_cases[12];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call cross-module nested higher-order bridge" {
    const case = direct_call_cases[12];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call cross-module apply twice" {
    const case = direct_call_cases[13];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call cross-module apply twice" {
    const case = direct_call_cases[13];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call cross-module passing captured closure to callback parameter" {
    const case = direct_call_cases[14];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call cross-module passing captured closure to callback parameter" {
    const case = direct_call_cases[14];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call two callback params with different captures" {
    const case = direct_call_cases[15];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call two callback params with different captures" {
    const case = direct_call_cases[15];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call record field closure extraction then call" {
    const case = direct_call_cases[16];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call record field closure extraction then call" {
    const case = direct_call_cases[16];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - lowering opaque function field lookup issue 9262 has no erased callables" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, .module, opaque_function_field_lookup_source, &.{});
    defer executable.deinit(arena_allocator);

    try testing.expectEqual(@as(usize, 0), countExecutableErasedCalls(&executable.executable));
    try testing.expectEqual(@as(usize, 0), countExecutablePackedFns(&executable.executable));
    try testing.expectEqual(@as(usize, 0), countExecutableErasedFnTypes(&executable.executable));

    var compiled = try helpers.compileProgram(arena_allocator, .module, opaque_function_field_lookup_source, &.{});
    defer compiled.deinit(arena_allocator);

    try testing.expectEqual(@as(usize, 0), countErasedCalls(&compiled));
}

test "cor pipeline - lowering local callback specialization uses two distinct concrete defs" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, .module, additional_specialization_via_list_append_source, &.{});
    defer executable.deinit(arena_allocator);

    try testing.expectEqual(@as(usize, 2), countSpecializedLocalFnDefs(&executable.executable));

    const result_tys = try collectSpecializedLocalFnResultTypes(
        arena_allocator,
        &executable.executable,
    );
    try testing.expectEqual(@as(usize, 2), result_tys.len);
    try testing.expect(!executable.executable.types.equalIds(result_tys[0], result_tys[1]));
    try testing.expect(!executable.executable.types.containsAbstractLeaf(result_tys[0]));
    try testing.expect(!executable.executable.types.containsAbstractLeaf(result_tys[1]));
}

test "cor pipeline - lowering polymorphic two-list helper uses two distinct concrete defs" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, .module, polymorphic_two_list_types_source, &.{});
    defer executable.deinit(arena_allocator);

    try testing.expectEqual(@as(usize, 2), countSpecializedLocalFnDefs(&executable.executable));
    try testing.expectEqual(@as(usize, 2), try countUniqueDirectCallTargetsNamed(
        arena_allocator,
        &executable.executable,
    ));

    const arg_tys = try collectSpecializedLocalFnFirstArgTypes(
        arena_allocator,
        &executable.executable,
    );
    try testing.expectEqual(@as(usize, 2), arg_tys.len);
    try testing.expect(!executable.executable.types.equalIds(arg_tys[0], arg_tys[1]));
    try testing.expect(!executable.executable.types.containsAbstractLeaf(arg_tys[0]));
    try testing.expect(!executable.executable.types.containsAbstractLeaf(arg_tys[1]));

    const result_tys = try collectSpecializedLocalFnResultTypes(
        arena_allocator,
        &executable.executable,
    );
    try testing.expectEqual(@as(usize, 2), result_tys.len);
    try testing.expect(!executable.executable.types.containsAbstractLeaf(result_tys[0]));
    try testing.expect(!executable.executable.types.containsAbstractLeaf(result_tys[1]));
    try testing.expectEqual(@as(usize, 0), countExecutableBridgeNodes(&executable.executable));

    var ir_program = try compileIrProgram(arena_allocator, .module, polymorphic_two_list_types_source, &.{});
    defer ir_program.deinit(arena_allocator);
    try testing.expectEqual(@as(usize, 0), countIrBridgeNodes(&ir_program.ir_result));
}

test "cor pipeline - lowering direct-only higher-order call has no executable or ir bridges" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, .module, simple_direct_call_source, &.{});
    defer executable.deinit(arena_allocator);
    try testing.expectEqual(@as(usize, 0), countExecutableBridgeNodes(&executable.executable));

    var ir_program = try compileIrProgram(arena_allocator, .module, simple_direct_call_source, &.{});
    defer ir_program.deinit(arena_allocator);
    try testing.expectEqual(@as(usize, 0), countIrBridgeNodes(&ir_program.ir_result));
}

test "cor pipeline - typed nested captures" {
    try expectInspectProgram(.expr, typed_nested_captures_source, &.{}, "6");
}

test "cor pipeline - lowering emits explicit bridges only at real representation boundaries" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, .module, annotated_callback_param_source, &.{});
    defer executable.deinit(arena_allocator);
    const executable_bridges = countExecutableBridgeNodes(&executable.executable);
    try testing.expect(executable_bridges >= 1);

    var ir_program = try compileIrProgram(arena_allocator, .module, annotated_callback_param_source, &.{});
    defer ir_program.deinit(arena_allocator);
    try testing.expectEqual(executable_bridges, countIrBridgeNodes(&ir_program.ir_result));
}

test "cor pipeline - debug bridge shapes recursive tag payload issue 8754" {
    try debugPrintBridgeShapesForSource(
        .module,
        \\Tree := [Node(Str, List(Tree)), Text(Str), Wrapper(Tree)]
        \\
        \\inner : Tree
        \\inner = Text("hello")
        \\
        \\wrapped : Tree
        \\wrapped = Wrapper(inner)
        \\
        \\main = match wrapped {
        \\    Wrapper(inner_tree) =>
        \\        match inner_tree {
        \\            Text(_) => 1
        \\            Node(_, _) => 2
        \\            Wrapper(_) => 3
        \\        }
        \\    _ => 0
        \\}
    ,
        &.{},
    );
}

test "cor pipeline - debug bridge shapes boxed lambda stored in tag union" {
    try debugPrintBridgeShapesForSource(
        .expr,
        \\{
        \\make = |n| |x| x + n
        \\boxed = Box.box(make(6))
        \\tagged = if Bool.True Ok(boxed) else Err(boxed)
        \\f = Box.unbox(match tagged {
        \\    Ok(value) => value
        \\    Err(value) => value
        \\})
        \\f(1) + f(2)
        \\}
    ,
        &.{},
    );
}

test "cor pipeline - boxed polymorphic const closure round trip keeps erased captures" {
    const source =
        \\{
        \\    make_const = |value| |_| value
        \\    num_f = Box.unbox(Box.box(make_const(41)))
        \\    str_f = Box.unbox(Box.box(make_const("ok")))
        \\    { n: num_f({}), s: str_f({}) }
        \\}
    ;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, .expr, source, &.{});
    defer executable.deinit(arena_allocator);
    try testing.expectEqual(@as(usize, 0), countExecutableBridgeNodes(&executable.executable));

    var ir_program = try compileIrProgram(arena_allocator, .expr, source, &.{});
    defer ir_program.deinit(arena_allocator);
    try testing.expectEqual(@as(usize, 0), countIrBridgeNodes(&ir_program.ir_result));

    var compiled = try helpers.compileInspectedExpr(arena_allocator, source);
    defer compiled.deinit(arena_allocator);
    const actual = try helpers.lirInterpreterInspectedStr(arena_allocator, &compiled.lowered);
    try testing.expectEqualStrings("{ n: 41.0, s: \"ok\" }", actual);
}

test "cor pipeline - debug bridge shapes list concat utf8 issue 8618" {
    try debugPrintBridgeShapesForSource(
        .expr,
        \\{
        \\test = |line| {
        \\    bytes = line.to_utf8()
        \\    List.concat([0], bytes)
        \\}
        \\
        \\x = test("abc")
        \\x
        \\}
    ,
        &.{},
    );
}

test "cor pipeline - boxed lambda helper chain keeps erased captures" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var compiled = try helpers.compileInspectedExpr(arena_allocator, boxed_lambda_helper_chain_source);
    defer compiled.deinit(arena_allocator);

    const actual = try helpers.lirInterpreterInspectedStr(arena_allocator, &compiled.lowered);
    try testing.expectEqualStrings("13.0", actual);
    try testing.expect(countErasedCalls(&compiled) >= 1);
    try testing.expect(countLowLevelOp(&compiled, .box_box) >= 1);
    try testing.expect(countLowLevelOp(&compiled, .box_unbox) >= 1);
}

test "cor pipeline - boxed lambda lowering uses erased-call path" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var compiled = try helpers.compileInspectedExpr(
        arena_allocator,
        \\{
        \\    wrap = |boxed| { value: boxed }
        \\    unwrap = |record| record.value
        \\    f = Box.unbox(unwrap(wrap(Box.box(|x| x + 1.I64))))
        \\    f(41.I64)
        \\}
        ,
    );
    defer compiled.deinit(arena_allocator);

    const actual = try helpers.lirInterpreterInspectedStr(arena_allocator, &compiled.lowered);
    try testing.expectEqualStrings("42", actual);

    try testing.expect(countErasedCalls(&compiled) >= 1);
    try testing.expect(countLowLevelOp(&compiled, .box_box) >= 1);
    try testing.expect(countLowLevelOp(&compiled, .box_unbox) >= 1);
    try expectErasedLoweringProgram(
        boxed_lambda_round_trip_erased_case.source_kind,
        boxed_lambda_round_trip_erased_case.source,
        boxed_lambda_round_trip_erased_case.imports,
        boxed_lambda_round_trip_erased_case.expected_executable_erased_calls,
        boxed_lambda_round_trip_erased_case.expected_executable_packed_fns,
        boxed_lambda_round_trip_erased_case.expected_executable_erased_fn_types,
        boxed_lambda_round_trip_erased_case.expected_lir_erased_calls,
    );
}

test "cor pipeline - non-boxed polymorphic closure round trip does not erase" {
    try expectNoErasedLoweringProgram(
        .expr,
        nonboxed_polymorphic_closure_round_trip_source,
        &.{},
        "{ n: 41.0, s: \"ok\" }",
    );
}

test "cor pipeline - non-boxed polymorphic closure helper round trip does not erase" {
    try expectNoErasedLoweringProgram(
        .expr,
        nonboxed_polymorphic_closure_helper_round_trip_source,
        &.{},
        "{ n: 41.0, s: \"ok\" }",
    );
}

test "cor pipeline - canonical hosted lambda fact has no fake body field" {
    try testing.expect(!@hasField(@FieldType(can.CIR.Expr, "e_hosted_lambda"), "body"));
}

test "cor pipeline - echo hosted proc metadata reaches lir" {
    var compiled = try helpers.compileProgram(
        testing.allocator,
        .module,
        \\main! = |_args| {
        \\    echo!("Hello from hosted")
        \\}
    ,
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    try testing.expect(countHostedProcSpecs(&compiled) >= 1);
    try testing.expectEqual(countHostedProcSpecs(&compiled), countHostedProcSpecsWithoutBody(&compiled));

    var saw_echo = false;
    for (compiled.lowered.lir_result.store.getProcSpecs()) |proc| {
        if (proc.hosted) |hosted| {
            try testing.expectEqual(@as(u32, 0), hosted.index);
            try testing.expectEqualStrings("echo!", compiled.resources.module_env.getIdent(hosted.symbol_name));
            try testing.expect(proc.body == null);
            saw_echo = true;
        }
    }
    try testing.expect(saw_echo);
}

test "cor pipeline - echo hosted proc call reaches interpreter and dev backend" {
    const hosted_fns = [_]builtins.host_abi.HostedFn{
        builtins.host_abi.hostedFn(&echoHostedFn),
    };
    var compiled = try helpers.compileProgram(
        testing.allocator,
        .module,
        \\main! = |_args| {
        \\    echo!("Hello from hosted")
        \\    echo!("Again")
        \\}
    ,
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    var interp_run = try runModuleWithInterpreter(testing.allocator, &compiled, &hosted_fns);
    defer interp_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, interp_run.termination);
    try testing.expectEqual(@as(usize, 2), interp_run.events.len);
    try testing.expectEqualStrings("Hello from hosted", interp_run.events[0].bytes());
    try testing.expectEqualStrings("Again", interp_run.events[1].bytes());

    var dev_run = try runModuleWithDevBackend(testing.allocator, &compiled, &hosted_fns);
    defer dev_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, dev_run.termination);
    try testing.expectEqual(@as(usize, 2), dev_run.events.len);
    try testing.expectEqualStrings("Hello from hosted", dev_run.events[0].bytes());
    try testing.expectEqualStrings("Again", dev_run.events[1].bytes());
}

test "cor pipeline - hosted function can flow as a first-class argument" {
    const hosted_fns = [_]builtins.host_abi.HostedFn{
        builtins.host_abi.hostedFn(&echoHostedFn),
    };
    var compiled = try helpers.compileProgram(
        testing.allocator,
        .module,
        \\import Platform
        \\
        \\main! = || {
        \\    ["hello"].for_each!(Platform.line!)
        \\}
    ,
        &.{.{
            .name = "Platform",
            .source =
            \\module [line!]
            \\
            \\line! : Str => {}
            ,
        }},
    );
    defer compiled.deinit(testing.allocator);

    var interp_run = try runModuleWithInterpreter(testing.allocator, &compiled, &hosted_fns);
    defer interp_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, interp_run.termination);
    try testing.expectEqual(@as(usize, 1), interp_run.events.len);
    try testing.expectEqualStrings("hello", interp_run.events[0].bytes());

    var dev_run = try runModuleWithDevBackend(testing.allocator, &compiled, &hosted_fns);
    defer dev_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, dev_run.termination);
    try testing.expectEqual(@as(usize, 1), dev_run.events.len);
    try testing.expectEqualStrings("hello", dev_run.events[0].bytes());
}

test "cor pipeline - hosted polymorphic list signature does not implicitly erase element type" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(
        arena_allocator,
        .module,
        \\import Platform
        \\
        \\main = Platform.id([1.U8, 2.U8])
    ,
        &.{.{
            .name = "Platform",
            .source =
            \\module [id]
            \\
            \\id : List(elem) -> List(elem)
            ,
        }},
    );
    defer executable.deinit(arena_allocator);

    try testing.expectEqual(@as(usize, 0), countExecutableErasedCalls(&executable.executable));
    try testing.expectEqual(@as(usize, 0), countExecutableErasedFnTypes(&executable.executable));
    try testing.expectEqual(@as(usize, 0), try countExecutableReferencedErasedPrimitiveTypes(
        arena_allocator,
        &executable.executable,
    ));
}

test "cor pipeline - hosted polymorphic record signature does not implicitly erase field type" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(
        arena_allocator,
        .module,
        \\import Platform
        \\
        \\main = Platform.echo({ value: 1.U8 })
    ,
        &.{.{
            .name = "Platform",
            .source =
            \\module [echo]
            \\
            \\echo : { value : elem } -> { value : elem }
            ,
        }},
    );
    defer executable.deinit(arena_allocator);

    try testing.expectEqual(@as(usize, 0), countExecutableErasedCalls(&executable.executable));
    try testing.expectEqual(@as(usize, 0), countExecutableErasedFnTypes(&executable.executable));
    try testing.expectEqual(@as(usize, 0), try countExecutableReferencedErasedPrimitiveTypes(
        arena_allocator,
        &executable.executable,
    ));
}

test "cor pipeline - hosted function survives boxed erased-call round trip" {
    // Blocked by a pre-existing monotype_lifted placeholder invariant failure.
    return error.SkipZigTest;
}

test "cor pipeline - boxed lambda round trip through host boundary" {
    // Blocked by a pre-existing parse/check failure in the hosted helper module.
    return error.SkipZigTest;
}

test "cor pipeline - zero-arg hosted proc call reaches host abi" {
    const hosted_fns = [_]builtins.host_abi.HostedFn{
        builtins.host_abi.hostedFn(&tickHostedFn),
    };
    var compiled = try helpers.compileProgram(
        testing.allocator,
        .module,
        \\import Platform
        \\
        \\main! = || {
        \\    Platform.tick!()
        \\    Platform.tick!()
        \\}
    ,
        &.{.{
            .name = "Platform",
            .source =
            \\tick! : {} => {}
            ,
        }},
    );
    defer compiled.deinit(testing.allocator);

    var interp_run = try runModuleWithInterpreter(testing.allocator, &compiled, &hosted_fns);
    defer interp_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, interp_run.termination);
    try testing.expectEqual(@as(usize, 2), interp_run.events.len);
    try testing.expectEqualStrings("tick", interp_run.events[0].bytes());
    try testing.expectEqualStrings("tick", interp_run.events[1].bytes());

    var dev_run = try runModuleWithDevBackend(testing.allocator, &compiled, &hosted_fns);
    defer dev_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, dev_run.termination);
    try testing.expectEqual(@as(usize, 2), dev_run.events.len);
    try testing.expectEqualStrings("tick", dev_run.events[0].bytes());
    try testing.expectEqualStrings("tick", dev_run.events[1].bytes());
}

test "cor pipeline - multi-arg hosted proc call preserves argument marshaling" {
    const hosted_fns = [_]builtins.host_abi.HostedFn{
        builtins.host_abi.hostedFn(&pairHostedFn),
    };
    var compiled = try helpers.compileProgram(
        testing.allocator,
        .module,
        \\import Platform
        \\
        \\main! = || {
        \\    Platform.pair!("left", "right")
        \\}
    ,
        &.{.{
            .name = "Platform",
            .source =
            \\pair! : Str, Str => {}
            ,
        }},
    );
    defer compiled.deinit(testing.allocator);

    var interp_run = try runModuleWithInterpreter(testing.allocator, &compiled, &hosted_fns);
    defer interp_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, interp_run.termination);
    try testing.expectEqual(@as(usize, 2), interp_run.events.len);
    try testing.expectEqualStrings("left", interp_run.events[0].bytes());
    try testing.expectEqualStrings("right", interp_run.events[1].bytes());

    var dev_run = try runModuleWithDevBackend(testing.allocator, &compiled, &hosted_fns);
    defer dev_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, dev_run.termination);
    try testing.expectEqual(@as(usize, 2), dev_run.events.len);
    try testing.expectEqualStrings("left", dev_run.events[0].bytes());
    try testing.expectEqualStrings("right", dev_run.events[1].bytes());
}

test "cor pipeline - checked bool inequality is negated equality" {
    var resources = try helpers.parseAndCanonicalizeProgram(testing.allocator, .expr, "True != False", &.{});
    defer helpers.cleanupParseAndCanonical(testing.allocator, resources);

    switch (resources.module_env.store.getExpr(resources.expr_idx)) {
        .e_method_eq => |eq| try testing.expect(eq.negated),
        .e_structural_eq => |eq| try testing.expect(eq.negated),
        else => return error.UnexpectedCheckedEqualityShape,
    }
}

test "cor pipeline - inspect-wrapped bool inequality stays negated in checked cir" {
    var resources = try helpers.parseAndCanonicalizeInspectedExpr(testing.allocator, "True != False");
    defer helpers.cleanupParseAndCanonical(testing.allocator, resources);

    const inner_expr_idx = switch (resources.module_env.store.getExpr(resources.expr_idx)) {
        .e_call => |call| blk: {
            const args = resources.module_env.store.sliceExpr(call.args);
            try testing.expectEqual(@as(usize, 1), args.len);
            break :blk args[0];
        },
        else => return error.UnexpectedInspectWrapShape,
    };

    switch (resources.module_env.store.getExpr(inner_expr_idx)) {
        .e_method_eq => |eq| try testing.expect(eq.negated),
        .e_structural_eq => |eq| try testing.expect(eq.negated),
        else => return error.UnexpectedCheckedEqualityShape,
    }
}

test "cor pipeline - inspect-wrapped bool inequality keeps bool_not in executable" {
    var resources = try helpers.parseAndCanonicalizeInspectedExpr(testing.allocator, "True != False");
    defer helpers.cleanupParseAndCanonical(testing.allocator, resources);

    var executable = try compileExecutableFromParsedResources(testing.allocator, &resources);
    defer executable.deinit();

    try testing.expect(countExecutableLowLevelOp(&executable, .bool_not) >= 1);
}

fn rootMonotypeExprId(mono: *const monotype.Lower.Result) monotype.Ast.ExprId {
    const root_def_id = mono.program.root_defs.items[mono.program.root_defs.items.len - 1];
    const root_def = mono.program.store.getDef(root_def_id);
    return switch (root_def.value) {
        .val => |expr_id| expr_id,
        .run => |run_def| run_def.body,
        else => @panic("expected monotype root expr def"),
    };
}

fn monotypeRootValueExprId(mono: *const monotype.Lower.Result) monotype.Ast.ExprId {
    const root_expr = mono.program.store.getExpr(rootMonotypeExprId(mono));
    return switch (root_expr.data) {
        .inspect => |expr_id| expr_id,
        else => rootMonotypeExprId(mono),
    };
}

fn checkedTypeText(
    allocator: std.mem.Allocator,
    module: check.TypedCIR.Module,
    var_: types_mod.Var,
) ![]u8 {
    var type_writer = try types_mod.TypeWriter.initFromParts(
        allocator,
        module.typeStoreConst(),
        module.identStoreConst(),
        null,
    );
    defer type_writer.deinit();
    try type_writer.write(var_, .wrap);
    return try allocator.dupe(u8, type_writer.get());
}

fn typedCirPatternName(module: check.TypedCIR.Module, pattern_idx: can.CIR.Pattern.Idx) []const u8 {
    return switch (module.pattern(pattern_idx).data) {
        .assign => |assign| module.identStoreConst().getText(assign.ident),
        else => @panic("expected assign pattern"),
    };
}

fn typedCirBlockDeclExprByName(
    module: check.TypedCIR.Module,
    body_expr_idx: can.CIR.Expr.Idx,
    name: []const u8,
) !can.CIR.Expr.Idx {
    const body_expr = module.expr(body_expr_idx);
    const block = switch (body_expr.data) {
        .e_block => |block| block,
        else => return error.UnexpectedTypedCirBlockShape,
    };

    for (module.sliceStatements(block.stmts)) |stmt_idx| {
        switch (module.getStatement(stmt_idx)) {
            .s_decl => |decl| {
                if (std.mem.eql(u8, typedCirPatternName(module, decl.pattern), name)) {
                    return decl.expr;
                }
            },
            else => {},
        }
    }

    return error.MissingTypedCirLocalDecl;
}

fn typedCirTopLevelDefExprByName(module: check.TypedCIR.Module, name: []const u8) !can.CIR.Expr.Idx {
    for (module.allDefs()) |def_idx| {
        const def = module.def(def_idx);
        if (std.mem.eql(u8, typedCirPatternName(module, def.data.pattern), name)) {
            return def.data.expr;
        }
    }

    return error.MissingTypedCirTopLevelDef;
}

const TypedCirLambdaBody = struct {
    module: check.TypedCIR.Module,
    body: can.CIR.Expr.Idx,
};

fn typedCirLambdaBodyFromExpr(
    modules: *const check.TypedCIR.Modules,
    module: check.TypedCIR.Module,
    expr_idx: can.CIR.Expr.Idx,
) !TypedCirLambdaBody {
    const expr = module.expr(expr_idx);
    return switch (expr.data) {
        .e_lambda => |lambda| .{ .module = module, .body = lambda.body },
        .e_closure => |closure| try typedCirLambdaBodyFromExpr(modules, module, closure.lambda_idx),
        .e_call => |call| try typedCirLambdaBodyFromExpr(modules, module, call.func),
        .e_lookup_external => |lookup| {
            const target_module_idx = module.resolvedImportModule(lookup.module_idx) orelse
                return error.MissingTypedCirImportTarget;
            const target_module = modules.module(target_module_idx);
            return try typedCirLambdaBodyFromExpr(
                modules,
                target_module,
                target_module.def(@enumFromInt(lookup.target_node_idx)).data.expr,
            );
        },
        else => return error.UnexpectedTypedCirEntrypointLambdaShape,
    };
}

fn monotypeSymbolName(mono: *const monotype.Lower.Result, symbol: symbol_mod.Symbol) []const u8 {
    return mono.idents.getText(mono.symbols.get(symbol).name);
}

fn rootLiftedExprId(lifted: *const monotype_lifted.Lower.Result) monotype_lifted.Ast.ExprId {
    const root_def_id = lifted.root_defs.items[lifted.root_defs.items.len - 1];
    const root_def = lifted.store.getDef(root_def_id);
    return switch (root_def.value) {
        .val => |expr_id| expr_id,
        .run => |run_def| run_def.body,
        else => @panic("expected lifted root expr def"),
    };
}

fn liftedRootValueExprId(lifted: *const monotype_lifted.Lower.Result) monotype_lifted.Ast.ExprId {
    const root_expr = lifted.store.getExpr(rootLiftedExprId(lifted));
    return switch (root_expr.data) {
        .inspect => |expr_id| expr_id,
        else => rootLiftedExprId(lifted),
    };
}

fn monotypeBlockDeclByName(
    mono: *const monotype.Lower.Result,
    body_expr_id: monotype.Ast.ExprId,
    name: []const u8,
) !@FieldType(monotype.Ast.Stmt, "decl") {
    const body_expr = mono.program.store.getExpr(body_expr_id);
    const block = switch (body_expr.data) {
        .block => |block| block,
        else => return error.UnexpectedMonotypeFnBodyShape,
    };

    for (mono.program.store.sliceStmtSpan(block.stmts)) |stmt_id| {
        switch (mono.program.store.getStmt(stmt_id)) {
            .decl => |decl| {
                if (std.mem.eql(u8, monotypeSymbolName(mono, decl.bind.symbol), name)) {
                    return decl;
                }
            },
            else => {},
        }
    }

    return error.MissingMonotypeLocalDecl;
}

fn monotypeBlockLocalFnByName(
    mono: *const monotype.Lower.Result,
    body_expr_id: monotype.Ast.ExprId,
    name: []const u8,
) !@FieldType(monotype.Ast.Stmt, "local_fn") {
    const body_expr = mono.program.store.getExpr(body_expr_id);
    const block = switch (body_expr.data) {
        .block => |block| block,
        else => return error.UnexpectedMonotypeFnBodyShape,
    };

    for (mono.program.store.sliceStmtSpan(block.stmts)) |stmt_id| {
        switch (mono.program.store.getStmt(stmt_id)) {
            .local_fn => |let_fn| {
                if (std.mem.eql(u8, monotypeSymbolName(mono, let_fn.bind.symbol), name)) {
                    return let_fn;
                }
            },
            else => {},
        }
    }

    return error.MissingMonotypeLocalFn;
}

fn monotypeListElemPrim(types: *const monotype.Type.Store, ty: monotype.Type.TypeId) !monotype.Type.Prim {
    return switch (types.getType(ty)) {
        .list => |elem_ty| switch (types.getType(elem_ty)) {
            .primitive => |prim| prim,
            else => error.UnexpectedMonotypeListElemShape,
        },
        else => error.UnexpectedMonotypeListShape,
    };
}

fn monotypePrim(types: *const monotype.Type.Store, ty: monotype.Type.TypeId) !monotype.Type.Prim {
    return switch (types.getType(ty)) {
        .primitive => |prim| prim,
        else => error.UnexpectedMonotypePrimitiveShape,
    };
}

fn liftedSymbolName(lifted: *const monotype_lifted.Lower.Result, symbol: symbol_mod.Symbol) []const u8 {
    return lifted.idents.getText(lifted.symbols.get(symbol).name);
}

fn liftedBlockDeclByName(
    lifted: *const monotype_lifted.Lower.Result,
    body_expr_id: monotype_lifted.Ast.ExprId,
    name: []const u8,
) !@FieldType(monotype_lifted.Ast.Stmt, "decl") {
    const body_expr = lifted.store.getExpr(body_expr_id);
    const block = switch (body_expr.data) {
        .block => |block| block,
        else => return error.UnexpectedLiftedFnBodyShape,
    };

    for (lifted.store.sliceStmtSpan(block.stmts)) |stmt_id| {
        switch (lifted.store.getStmt(stmt_id)) {
            .decl => |decl| {
                if (std.mem.eql(u8, liftedSymbolName(lifted, decl.bind.symbol), name)) {
                    return decl;
                }
            },
            else => {},
        }
    }

    return error.MissingLiftedLocalDecl;
}

fn solvedSymbolName(solved: *const lambdasolved.Lower.Result, symbol: symbol_mod.Symbol) []const u8 {
    return solved.idents.getText(solved.symbols.get(symbol).name);
}

fn solvedBlockDeclByName(
    solved: *const lambdasolved.Lower.Result,
    body_expr_id: lambdasolved.Ast.ExprId,
    name: []const u8,
) !@FieldType(lambdasolved.Ast.Stmt, "decl") {
    const body_expr = solved.store.getExpr(body_expr_id);
    const block = switch (body_expr.data) {
        .block => |block| block,
        else => return error.UnexpectedSolvedFnBodyShape,
    };

    for (solved.store.sliceStmtSpan(block.stmts)) |stmt_id| {
        switch (solved.store.getStmt(stmt_id)) {
            .decl => |decl| {
                if (std.mem.eql(u8, solvedSymbolName(solved, decl.bind.symbol), name)) {
                    return decl;
                }
            },
            else => {},
        }
    }

    return error.MissingSolvedLocalDecl;
}

fn solvedResolvedNode(types: *const lambdasolved.Type.Store, ty: lambdasolved.Type.TypeVarId) lambdasolved.Type.Node {
    const root = types.unlinkPreservingNominalConst(ty);
    return switch (types.getNode(root)) {
        .nominal => |nominal| solvedResolvedNode(types, nominal.backing),
        else => |node| node,
    };
}

fn solvedListElemPrim(types: *const lambdasolved.Type.Store, ty: lambdasolved.Type.TypeVarId) !lambdasolved.Type.Prim {
    return switch (solvedResolvedNode(types, ty)) {
        .content => |content| switch (content) {
            .list => |elem_ty| switch (solvedResolvedNode(types, elem_ty)) {
                .content => |elem_content| switch (elem_content) {
                    .primitive => |prim| prim,
                    else => error.UnexpectedSolvedListElemShape,
                },
                else => error.UnexpectedSolvedListElemNode,
            },
            else => error.UnexpectedSolvedListShape,
        },
        else => error.UnexpectedSolvedListNode,
    };
}

fn solvedPrim(types: *const lambdasolved.Type.Store, ty: lambdasolved.Type.TypeVarId) !lambdasolved.Type.Prim {
    return switch (solvedResolvedNode(types, ty)) {
        .content => |content| switch (content) {
            .primitive => |prim| prim,
            else => error.UnexpectedSolvedPrimitiveShape,
        },
        else => error.UnexpectedSolvedPrimitiveNode,
    };
}

fn rootSolvedExprId(solved: *const lambdasolved.Lower.Result) lambdasolved.Ast.ExprId {
    const root_def_id = solved.root_defs.items[solved.root_defs.items.len - 1];
    const root_def = solved.store.getDef(root_def_id);
    return switch (root_def.value) {
        .val => |expr_id| expr_id,
        .run => |run_def| run_def.body,
        else => @panic("expected solved root expr def"),
    };
}

fn solvedRootValueExprId(solved: *const lambdasolved.Lower.Result) lambdasolved.Ast.ExprId {
    const root_expr = solved.store.getExpr(rootSolvedExprId(solved));
    return switch (root_expr.data) {
        .inspect => |expr_id| expr_id,
        else => rootSolvedExprId(solved),
    };
}

test "cor pipeline - inspect-wrapped bool inequality keeps bool_not in monotype" {
    var resources = try helpers.parseAndCanonicalizeInspectedExpr(testing.allocator, "True != False");
    defer helpers.cleanupParseAndCanonical(testing.allocator, resources);

    var mono = try compileMonotypeFromParsedResources(testing.allocator, &resources);
    defer mono.deinit();

    const root_expr = mono.program.store.getExpr(rootMonotypeExprId(&mono));
    const inspect_child = switch (root_expr.data) {
        .inspect => |expr_id| expr_id,
        else => return error.UnexpectedMonotypeInspectShape,
    };
    const child = mono.program.store.getExpr(inspect_child);
    switch (child.data) {
        .low_level => |ll| try testing.expectEqual(base.LowLevel.bool_not, ll.op),
        else => return error.UnexpectedMonotypeInspectChildShape,
    }
}

test "cor pipeline - to_utf8 local binding stays list u8 before lambdamono" {
    var resources = try helpers.parseAndCanonicalizeInspectedExpr(testing.allocator,
        \\{
        \\test = |line| {
        \\    bytes = line.to_utf8()
        \\    List.concat([0], bytes)
        \\}
        \\
        \\x = test("abc")
        \\x
        \\}
    );
    defer helpers.cleanupParseAndCanonical(testing.allocator, resources);

    const typed = resources.typed_cir_modules.module(0);
    const typed_root_expr = switch (typed.expr(resources.expr_idx).data) {
        .e_call => |call| typed.sliceExpr(call.args)[0],
        else => resources.expr_idx,
    };
    const typed_test_expr = try typedCirBlockDeclExprByName(typed, typed_root_expr, "test");
    const typed_test_text = try checkedTypeText(testing.allocator, typed, typed.expr(typed_test_expr).ty());
    defer testing.allocator.free(typed_test_text);
    const typed_test_body = try typedCirLambdaBodyFromExpr(&resources.typed_cir_modules, typed, typed_test_expr);
    const typed_bytes_expr = try typedCirBlockDeclExprByName(typed_test_body.module, typed_test_body.body, "bytes");
    const typed_bytes_text = try checkedTypeText(testing.allocator, typed_test_body.module, typed_test_body.module.expr(typed_bytes_expr).ty());
    defer testing.allocator.free(typed_bytes_text);
    try testing.expectEqualStrings(
        \\a -> List(item)
        \\  where [
        \\    a.to_utf8 : a -> List(item),
        \\    item.from_numeral : Numeral -> Try(item, [InvalidNumeral(Str)]),
        \\  ]
    ,
        typed_test_text,
    );
    try testing.expect(std.mem.indexOf(u8, typed_bytes_text, "List(item)") != null);
    try testing.expect(std.mem.indexOf(u8, typed_bytes_text, "item.from_numeral") != null);
    try testing.expect(std.mem.indexOf(u8, typed_bytes_text, "Dec") == null);

    var mono = try compileMonotypeFromParsedResources(testing.allocator, &resources);
    defer mono.deinit();

    const mono_root_body = monotypeRootValueExprId(&mono);
    const mono_test_fn = try monotypeBlockLocalFnByName(&mono, mono_root_body, "test");
    const mono_test_body = mono_test_fn.body;
    const mono_bytes_decl = try monotypeBlockDeclByName(&mono, mono_test_body, "bytes");
    const mono_bytes_expr = mono.program.store.getExpr(mono_bytes_decl.body);
    switch (mono_bytes_expr.data) {
        .dispatch_call => {},
        else => return error.UnexpectedMonotypeDispatchShape,
    }

    var lifted = try monotype_lifted.Lower.run(testing.allocator, &mono);
    defer lifted.deinit();
    var solved = try lambdasolved.Lower.run(testing.allocator, &lifted);
    defer solved.deinit();
}

test "cor pipeline - List.fold builtin sum keeps numeric facts before lambdamono" {
    var resources = try helpers.parseAndCanonicalizeInspectedExpr(
        testing.allocator,
        "List.fold([1, 2, 3], 0, |acc, item| acc + item)",
    );
    defer helpers.cleanupParseAndCanonical(testing.allocator, resources);

    const typed = resources.typed_cir_modules.module(0);
    const typed_root_expr = switch (typed.expr(resources.expr_idx).data) {
        .e_call => |call| typed.sliceExpr(call.args)[0],
        else => resources.expr_idx,
    };
    const typed_root_call = switch (typed.expr(typed_root_expr).data) {
        .e_call => |call| call,
        else => return error.UnexpectedTypedCirCallShape,
    };
    const typed_args = typed.sliceExpr(typed_root_call.args);
    const typed_root_text = try checkedTypeText(testing.allocator, typed, typed.expr(typed_root_expr).ty());
    defer testing.allocator.free(typed_root_text);
    const typed_list_text = try checkedTypeText(testing.allocator, typed, typed.expr(typed_args[0]).ty());
    defer testing.allocator.free(typed_list_text);
    const typed_seed_text = try checkedTypeText(testing.allocator, typed, typed.expr(typed_args[1]).ty());
    defer testing.allocator.free(typed_seed_text);
    const typed_root_resolved = typed.typeStoreConst().resolveVar(typed.expr(typed_root_expr).ty());
    const typed_seed_resolved = typed.typeStoreConst().resolveVar(typed.expr(typed_args[1]).ty());
    try testing.expectEqualStrings("Dec", typed_root_text);
    try testing.expectEqualStrings("List(Dec)", typed_list_text);
    try testing.expectEqualStrings("Dec", typed_seed_text);
    try testing.expectEqual(types_mod.Rank.outermost, typed_root_resolved.desc.rank);
    try testing.expectEqual(types_mod.Rank.outermost, typed_seed_resolved.desc.rank);

    var mono = try compileMonotypeFromParsedResources(testing.allocator, &resources);
    defer mono.deinit();

    const mono_root = mono.program.store.getExpr(monotypeRootValueExprId(&mono));
    const mono_call = switch (mono_root.data) {
        .call => |call| call,
        else => return error.UnexpectedMonotypeCallShape,
    };
    const mono_args = mono.program.store.sliceExprSpan(mono_call.args);
    try testing.expectEqual(@as(usize, 3), mono_args.len);
    try testing.expectEqual(monotype.Type.Prim.dec, try monotypeListElemPrim(&mono.types, mono.program.store.getExpr(mono_args[0]).ty));
    try testing.expectEqual(monotype.Type.Prim.dec, try monotypePrim(&mono.types, mono.program.store.getExpr(mono_args[1]).ty));

    var lifted = try monotype_lifted.Lower.run(testing.allocator, &mono);
    defer lifted.deinit();
    var solved = try lambdasolved.Lower.run(testing.allocator, &lifted);
    defer solved.deinit();

    const solved_root = solved.store.getExpr(solvedRootValueExprId(&solved));
    const solved_call = switch (solved_root.data) {
        .call => |call| call,
        else => return error.UnexpectedSolvedCallShape,
    };
    const solved_args = solved.store.sliceExprSpan(solved_call.args);
    try testing.expectEqual(@as(usize, 3), solved_args.len);
    try testing.expectEqual(lambdasolved.Type.Prim.dec, try solvedListElemPrim(&solved.types, solved.store.getExpr(solved_args[0]).ty));
    try testing.expectEqual(lambdasolved.Type.Prim.dec, try solvedPrim(&solved.types, solved.store.getExpr(solved_args[1]).ty));
}

test "cor pipeline - List.fold method syntax keeps accumulator facts before lambdamono" {
    var resources = try helpers.parseAndCanonicalizeInspectedExpr(testing.allocator,
        \\{
        \\    append_one = |acc, x| List.append(acc, x)
        \\    xs = [1.I64, 2.I64]
        \\    xs.fold(List.with_capacity(1), append_one)
        \\}
    );
    defer helpers.cleanupParseAndCanonical(testing.allocator, resources);

    const typed = resources.typed_cir_modules.module(0);
    const typed_root_expr = switch (typed.expr(resources.expr_idx).data) {
        .e_call => |call| typed.sliceExpr(call.args)[0],
        else => resources.expr_idx,
    };
    const typed_root = typed.expr(typed_root_expr);
    const typed_dispatch = switch (typed_root.data) {
        .e_dispatch_call => |call| call,
        else => return error.UnexpectedTypedCirDispatchShape,
    };
    const typed_args = typed.sliceExpr(typed_dispatch.args);
    try testing.expectEqual(@as(usize, 2), typed_args.len);
    const typed_receiver_text = try checkedTypeText(testing.allocator, typed, typed.expr(typed_dispatch.receiver).ty());
    defer testing.allocator.free(typed_receiver_text);
    const typed_seed_text = try checkedTypeText(testing.allocator, typed, typed.expr(typed_args[0]).ty());
    defer testing.allocator.free(typed_seed_text);
    const typed_callback_text = try checkedTypeText(testing.allocator, typed, typed.expr(typed_args[1]).ty());
    defer testing.allocator.free(typed_callback_text);
    try testing.expectEqualStrings("List(I64)", typed_receiver_text);
    try testing.expectEqualStrings("List(I64)", typed_seed_text);
    try testing.expect(std.mem.indexOf(u8, typed_callback_text, "I64") != null);

    var mono = try compileMonotypeFromParsedResources(testing.allocator, &resources);
    defer mono.deinit();

    const mono_root = mono.program.store.getExpr(monotypeRootValueExprId(&mono));
    const mono_dispatch = switch (mono_root.data) {
        .dispatch_call => |call| call,
        else => return error.UnexpectedMonotypeDispatchShape,
    };
    const mono_args = mono.program.store.sliceExprSpan(mono_dispatch.args);
    try testing.expectEqual(@as(usize, 2), mono_args.len);
    try testing.expectEqual(monotype.Type.Prim.i64, try monotypeListElemPrim(&mono.types, mono.program.store.getExpr(mono_dispatch.receiver).ty));
    try testing.expectEqual(monotype.Type.Prim.i64, try monotypeListElemPrim(&mono.types, mono.program.store.getExpr(mono_args[0]).ty));

    var lifted = try monotype_lifted.Lower.run(testing.allocator, &mono);
    defer lifted.deinit();
    var solved = try lambdasolved.Lower.run(testing.allocator, &lifted);
    defer solved.deinit();

    const solved_root = solved.store.getExpr(solvedRootValueExprId(&solved));
    const solved_dispatch = switch (solved_root.data) {
        .dispatch_call => |call| call,
        else => return error.UnexpectedSolvedDispatchShape,
    };
    const solved_args = solved.store.sliceExprSpan(solved_dispatch.args);
    try testing.expectEqual(@as(usize, 2), solved_args.len);
    try testing.expectEqual(lambdasolved.Type.Prim.i64, try solvedListElemPrim(&solved.types, solved.store.getExpr(solved_dispatch.receiver).ty));
    try testing.expectEqual(lambdasolved.Type.Prim.i64, try solvedListElemPrim(&solved.types, solved.store.getExpr(solved_args[0]).ty));
}

test "cor pipeline - polymorphic additional specialization via List.append keeps len result facts before lambdamono" {
    var resources = try helpers.parseAndCanonicalizeInspectedExpr(testing.allocator,
        \\{
        \\    append_one = |acc, x| List.append(acc, x)
        \\    clone_via_fold = |xs| xs.fold(List.with_capacity(1), append_one)
        \\    _first_len = clone_via_fold([1.I64, 2.I64]).len()
        \\    clone_via_fold([[1.I64, 2.I64], [3.I64, 4.I64]]).len()
        \\}
    );
    defer helpers.cleanupParseAndCanonical(testing.allocator, resources);

    const typed = resources.typed_cir_modules.module(0);
    const typed_root_expr = switch (typed.expr(resources.expr_idx).data) {
        .e_call => |call| typed.sliceExpr(call.args)[0],
        else => resources.expr_idx,
    };
    const typed_first_len_expr = try typedCirBlockDeclExprByName(typed, typed_root_expr, "_first_len");
    const typed_root_text = try checkedTypeText(testing.allocator, typed, typed.expr(typed_root_expr).ty());
    defer testing.allocator.free(typed_root_text);
    const typed_first_len_text = try checkedTypeText(testing.allocator, typed, typed.expr(typed_first_len_expr).ty());
    defer testing.allocator.free(typed_first_len_text);
    try testing.expectEqualStrings("U64", typed_root_text);
    try testing.expectEqualStrings("U64", typed_first_len_text);

    var mono = try compileMonotypeFromParsedResources(testing.allocator, &resources);
    defer mono.deinit();
    try testing.expectEqual(monotype.Type.Prim.u64, try monotypePrim(&mono.types, mono.program.store.getExpr(monotypeRootValueExprId(&mono)).ty));
    const mono_first_len_decl = try monotypeBlockDeclByName(&mono, monotypeRootValueExprId(&mono), "_first_len");
    try testing.expectEqual(monotype.Type.Prim.u64, try monotypePrim(&mono.types, mono.program.store.getExpr(mono_first_len_decl.body).ty));

    var lifted = try monotype_lifted.Lower.run(testing.allocator, &mono);
    defer lifted.deinit();
    var solved = try lambdasolved.Lower.run(testing.allocator, &lifted);
    defer solved.deinit();
    try testing.expectEqual(lambdasolved.Type.Prim.u64, try solvedPrim(&solved.types, solved.store.getExpr(solvedRootValueExprId(&solved)).ty));
    const solved_first_len_decl = try solvedBlockDeclByName(&solved, solvedRootValueExprId(&solved), "_first_len");
    try testing.expectEqual(lambdasolved.Type.Prim.u64, try solvedPrim(&solved.types, solved.store.getExpr(solved_first_len_decl.body).ty));
}

test "cor pipeline - polymorphic return function call keeps numeric facts before lambdamono" {
    var resources = try helpers.parseAndCanonicalizeInspectedExpr(
        testing.allocator,
        "(|_| (|x| x))(0)(42)",
    );
    defer helpers.cleanupParseAndCanonical(testing.allocator, resources);

    const typed = resources.typed_cir_modules.module(0);
    const typed_root_expr = switch (typed.expr(resources.expr_idx).data) {
        .e_call => |call| typed.sliceExpr(call.args)[0],
        else => resources.expr_idx,
    };
    const typed_final_call = switch (typed.expr(typed_root_expr).data) {
        .e_call => |call| call,
        else => return error.UnexpectedTypedCirCallShape,
    };
    const typed_first_call_expr = typed_final_call.func;
    const typed_first_call = switch (typed.expr(typed_first_call_expr).data) {
        .e_call => |call| call,
        else => return error.UnexpectedTypedCirCallShape,
    };
    const typed_first_args = typed.sliceExpr(typed_first_call.args);
    const typed_final_args = typed.sliceExpr(typed_final_call.args);

    const typed_root_text = try checkedTypeText(testing.allocator, typed, typed.expr(typed_root_expr).ty());
    defer testing.allocator.free(typed_root_text);
    const typed_first_arg_text = try checkedTypeText(testing.allocator, typed, typed.expr(typed_first_args[0]).ty());
    defer testing.allocator.free(typed_first_arg_text);
    const typed_final_arg_text = try checkedTypeText(testing.allocator, typed, typed.expr(typed_final_args[0]).ty());
    defer testing.allocator.free(typed_final_arg_text);
    const typed_root_resolved = typed.typeStoreConst().resolveVar(typed.expr(typed_root_expr).ty());
    const typed_first_arg_resolved = typed.typeStoreConst().resolveVar(typed.expr(typed_first_args[0]).ty());
    const typed_final_arg_resolved = typed.typeStoreConst().resolveVar(typed.expr(typed_final_args[0]).ty());

    try testing.expectEqualStrings("Dec", typed_root_text);
    try testing.expectEqualStrings("Dec", typed_first_arg_text);
    try testing.expectEqualStrings("Dec", typed_final_arg_text);
    try testing.expectEqual(types_mod.Rank.outermost, typed_root_resolved.desc.rank);
    try testing.expectEqual(types_mod.Rank.outermost, typed_first_arg_resolved.desc.rank);
    try testing.expectEqual(types_mod.Rank.outermost, typed_final_arg_resolved.desc.rank);
}

test "cor pipeline - dbg literal in polymorphic function body keeps numeric facts before lambdamono" {
    var resources = try helpers.parseAndCanonicalizeProgram(
        testing.allocator,
        .module,
        \\debug = |v| {
        \\    dbg 42
        \\    v
        \\}
        \\xs = [1, 2, 3]
        \\main = xs->debug()->List.fold(0, |acc, x| acc + x)
    ,
        &.{},
    );
    defer helpers.cleanupParseAndCanonical(testing.allocator, resources);

    const typed = resources.typed_cir_modules.module(0);
    const debug_expr = try typedCirTopLevelDefExprByName(typed, "debug");
    const debug_body = try typedCirLambdaBodyFromExpr(&resources.typed_cir_modules, typed, debug_expr);
    const body_expr = debug_body.module.expr(debug_body.body);
    const block = switch (body_expr.data) {
        .e_block => |block| block,
        else => return error.UnexpectedTypedCirBlockShape,
    };
    const body_stmts = debug_body.module.sliceStatements(block.stmts);
    try testing.expect(body_stmts.len >= 1);
    const dbg_lit_expr = switch (debug_body.module.getStatement(body_stmts[0])) {
        .s_dbg => |dbg_stmt| dbg_stmt.expr,
        else => return error.UnexpectedTypedCirStmtShape,
    };

    const dbg_lit_text = try checkedTypeText(testing.allocator, debug_body.module, debug_body.module.expr(dbg_lit_expr).ty());
    defer testing.allocator.free(dbg_lit_text);
    const dbg_lit_resolved = debug_body.module.typeStoreConst().resolveVar(debug_body.module.expr(dbg_lit_expr).ty());

    try testing.expectEqualStrings("Dec", dbg_lit_text);
    try testing.expectEqual(types_mod.Rank.outermost, dbg_lit_resolved.desc.rank);
}

test "cor pipeline - entry-specialized to_utf8 local binding stays list u8 before lambdamono" {
    var resources = try helpers.parseAndCanonicalizeInspectedExpr(testing.allocator,
        \\{
        \\test = |line| {
        \\    bytes = line.to_utf8()
        \\    List.concat([0], bytes)
        \\}
        \\
        \\x = test("abc")
        \\x
        \\}
    );
    defer helpers.cleanupParseAndCanonical(testing.allocator, resources);

    const typed = resources.typed_cir_modules.module(0);
    const typed_root_expr = switch (typed.expr(resources.expr_idx).data) {
        .e_call => |call| typed.sliceExpr(call.args)[0],
        else => resources.expr_idx,
    };
    const typed_x_expr = try typedCirBlockDeclExprByName(typed, typed_root_expr, "x");
    const typed_x_call = switch (typed.expr(typed_x_expr).data) {
        .e_call => |call| call,
        else => return error.UnexpectedTypedCirCallShape,
    };
    const typed_lookup_text = try checkedTypeText(testing.allocator, typed, typed.expr(typed_x_call.func).ty());
    defer testing.allocator.free(typed_lookup_text);
    const typed_x_text = try checkedTypeText(testing.allocator, typed, typed.expr(typed_x_expr).ty());
    defer testing.allocator.free(typed_x_text);
    try testing.expectEqualStrings("Str -> List(U8)", typed_lookup_text);
    try testing.expectEqualStrings("List(U8)", typed_x_text);

    var mono = try compileEntrySpecializedMonotypeFromParsedResources(testing.allocator, &resources);
    defer mono.deinit();

    const mono_root_body = monotypeRootValueExprId(&mono);
    const mono_x_decl = try monotypeBlockDeclByName(&mono, mono_root_body, "x");
    try testing.expectEqual(monotype.Type.Prim.u8, try monotypeListElemPrim(&mono.types, mono_x_decl.bind.ty));
    try testing.expectEqual(monotype.Type.Prim.u8, try monotypeListElemPrim(&mono.types, mono.program.store.getExpr(mono_x_decl.body).ty));

    var lifted = try monotype_lifted.Lower.run(testing.allocator, &mono);
    defer lifted.deinit();

    const lifted_x_decl = try liftedBlockDeclByName(&lifted, liftedRootValueExprId(&lifted), "x");
    try testing.expectEqual(monotype.Type.Prim.u8, try monotypeListElemPrim(&lifted.types, lifted_x_decl.bind.ty));
    try testing.expectEqual(monotype.Type.Prim.u8, try monotypeListElemPrim(&lifted.types, lifted.store.getExpr(lifted_x_decl.body).ty));

    var solved = try lambdasolved.Lower.run(testing.allocator, &lifted);
    defer solved.deinit();

    const solved_x_decl = try solvedBlockDeclByName(&solved, solvedRootValueExprId(&solved), "x");
    try testing.expectEqual(lambdasolved.Type.Prim.u8, try solvedListElemPrim(&solved.types, solved_x_decl.bind.ty));
    try testing.expectEqual(lambdasolved.Type.Prim.u8, try solvedListElemPrim(&solved.types, solved.store.getExpr(solved_x_decl.body).ty));
}

fn countExecutableLowLevelOp(executable: *const lambdamono.Lower.Result, op: base.LowLevel) usize {
    var count: usize = 0;
    for (executable.store.exprs.items) |expr| {
        switch (expr.data) {
            .low_level => |low_level| {
                if (low_level.op == op) count += 1;
            },
            else => {},
        }
    }
    return count;
}

test "cor pipeline - bool inequality lowers to bool_not over equality" {
    var compiled = try helpers.compileInspectedExpr(testing.allocator, "True != False");
    defer compiled.deinit(testing.allocator);

    try testing.expect(countLowLevelOp(&compiled, .bool_not) >= 1);
    try testing.expect(countLowLevelOp(&compiled, .num_is_eq) >= 1);

    const actual = try helpers.lirInterpreterInspectedStr(testing.allocator, &compiled.lowered);
    defer testing.allocator.free(actual);
    try testing.expectEqualStrings("True", actual);
}
