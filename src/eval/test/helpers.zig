//! Shared eval test helpers built on the cor-style lowering pipeline.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const parse = @import("parse");
const builtins = @import("builtins");
const compiled_builtins = @import("compiled_builtins");
const monotype = @import("monotype");
const monotype_lifted = @import("monotype_lifted");
const lambdasolved = @import("lambdasolved");
const lambdamono = @import("lambdamono");
const ir = @import("ir");
const lir = @import("lir");
const backend = @import("backend");
const collections = @import("collections");

const builtin_loading = @import("../builtin_loading.zig");
const Interpreter = @import("../interpreter.zig").Interpreter;
const RuntimeEnv = @import("TestEnv.zig");

const Can = can.Can;
const Check = check.Check;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Allocators = base.Allocators;
const RocStr = builtins.str.RocStr;
const HostLirCodeGen = backend.HostLirCodeGen;
const ExecutableMemory = backend.ExecutableMemory;
const FromIr = lir.FromIr;
const LayoutStore = @import("layout").Store;
const LayoutIdx = @import("layout").Idx;

pub const interpreter_allocator = std.heap.page_allocator;

pub const ParsedResources = struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    checker: *Check,
    expr_idx: CIR.Expr.Idx,
    builtin_module: builtin_loading.LoadedModule,
    builtin_indices: CIR.BuiltinIndices,
    imported_envs: []*const ModuleEnv,
    owned_source: ?[]u8 = null,
    parse_ns: u64 = 0,
    canonicalize_ns: u64 = 0,
    typecheck_ns: u64 = 0,
};

const LoweredProgram = struct {
    lir_result: FromIr.Result,
    main_proc: lir.LIR.LirProcSpecId,

    fn deinit(self: *LoweredProgram) void {
        self.lir_result.deinit();
    }
};

pub fn allocInspectedExprSource(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    return std.fmt.allocPrint(allocator, "Str.inspect(({s}))", .{source});
}

pub fn parseAndCanonicalizeExpr(allocator: std.mem.Allocator, source: []const u8) !ParsedResources {
    var parse_timer = try std.time.Timer.start();

    const wrapped_source = try std.fmt.allocPrint(allocator, "main = {s}", .{source});
    errdefer allocator.free(wrapped_source);

    const module_env = try allocator.create(ModuleEnv);
    errdefer allocator.destroy(module_env);
    module_env.* = try ModuleEnv.init(allocator, wrapped_source);
    errdefer module_env.deinit();
    module_env.common.source = wrapped_source;
    module_env.module_name = "Test";
    try module_env.common.calcLineStarts(module_env.gpa);

    var allocators: Allocators = undefined;
    allocators.initInPlace(allocator);
    errdefer allocators.deinit();

    const parse_ast = try parse.parse(&allocators, &module_env.common);
    errdefer {
        parse_ast.deinit();
        allocators.deinit();
    }
    parse_ast.store.emptyScratch();
    if (parse_ast.tokenize_diagnostics.items.len > 0 or parse_ast.parse_diagnostics.items.len > 0) {
        return error.ParseError;
    }
    const parse_elapsed = parse_timer.read();

    var can_timer = try std.time.Timer.start();
    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);
    var builtin_module = try builtin_loading.loadCompiledModule(
        allocator,
        compiled_builtins.builtin_bin,
        "Builtin",
        compiled_builtins.builtin_source,
    );
    errdefer builtin_module.deinit();

    try module_env.initCIRFields("test");
    const builtin_ctx: Check.BuiltinContext = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .bool_stmt = builtin_indices.bool_type,
        .try_stmt = builtin_indices.try_type,
        .str_stmt = builtin_indices.str_type,
        .builtin_module = builtin_module.env,
        .builtin_indices = builtin_indices,
    };

    const czer = try allocator.create(Can);
    errdefer allocator.destroy(czer);
    czer.* = try Can.initModule(&allocators, module_env, parse_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_module.env,
            .builtin_indices = builtin_indices,
        },
    });
    errdefer czer.deinit();

    try czer.canonicalizeFile();
    const can_elapsed = can_timer.read();

    var check_timer = try std.time.Timer.start();
    const imported_envs = try allocator.alloc(*const ModuleEnv, 1);
    errdefer allocator.free(imported_envs);
    imported_envs[0] = builtin_module.env;
    module_env.imports.resolveImports(module_env, imported_envs);

    const checker = try allocator.create(Check);
    errdefer allocator.destroy(checker);
    checker.* = try Check.init(
        allocator,
        &module_env.types,
        module_env,
        imported_envs,
        null,
        &module_env.store.regions,
        builtin_ctx,
    );
    errdefer checker.deinit();
    try checker.checkFile();
    const check_elapsed = check_timer.read();

    const defs = module_env.store.sliceDefs(module_env.all_defs);
    if (defs.len == 0) return error.NoRootDefinition;
    const expr_idx = module_env.store.getDef(defs[defs.len - 1]).expr;

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = czer,
        .checker = checker,
        .expr_idx = expr_idx,
        .builtin_module = builtin_module,
        .builtin_indices = builtin_indices,
        .imported_envs = imported_envs,
        .owned_source = wrapped_source,
        .parse_ns = parse_elapsed,
        .canonicalize_ns = can_elapsed,
        .typecheck_ns = check_elapsed,
    };
}

pub fn parseAndCanonicalizeInspectedExpr(allocator: std.mem.Allocator, source: []const u8) !ParsedResources {
    const inspected_source = try allocInspectedExprSource(allocator, source);
    defer allocator.free(inspected_source);
    return parseAndCanonicalizeExpr(allocator, inspected_source);
}

pub fn cleanupParseAndCanonical(allocator: std.mem.Allocator, resources: ParsedResources) void {
    resources.checker.deinit();
    resources.can.deinit();
    resources.parse_ast.deinit();
    resources.module_env.deinit();
    allocator.free(resources.imported_envs);
    var builtin_module = resources.builtin_module;
    builtin_module.deinit();
    if (resources.owned_source) |source| allocator.free(source);
    allocator.destroy(resources.checker);
    allocator.destroy(resources.can);
    allocator.destroy(resources.module_env);
}

pub fn lirInterpreterInspectedStr(
    allocator: std.mem.Allocator,
    module_env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    builtin_module_env: *const ModuleEnv,
) ![]u8 {
    _ = expr_idx;
    var lowered = try lowerToLir(allocator, module_env, builtin_module_env);
    defer lowered.deinit();

    var runtime_env = RuntimeEnv.init(allocator);
    defer runtime_env.deinit();

    var interp = try Interpreter.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interp.deinit();

    const result = try interp.eval(.{ .proc_id = lowered.main_proc });
    const ret_layout = lowered.lir_result.store.getProcSpec(lowered.main_proc).ret_layout;
    return copyReturnedRocStr(
        allocator,
        &lowered.lir_result.layouts,
        ret_layout,
        result.value.ptr,
        null,
    );
}

pub fn devEvaluatorInspectedStr(
    allocator: std.mem.Allocator,
    module_env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    builtin_module_env: *const ModuleEnv,
) ![]u8 {
    _ = expr_idx;
    var lowered = try lowerToLir(allocator, module_env, builtin_module_env);
    defer lowered.deinit();

    var codegen = try HostLirCodeGen.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        null,
    );
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(lowered.lir_result.store.getProcSpecs());

    const proc = lowered.lir_result.store.getProcSpec(lowered.main_proc);
    const code_result = try codegen.generateCode(lowered.main_proc, proc.ret_layout, 0);
    var exec_mem = try ExecutableMemory.initWithEntryOffset(code_result.code, code_result.entry_offset);
    defer exec_mem.deinit();

    var runtime_env = RuntimeEnv.init(allocator);
    defer runtime_env.deinit();

    const ret_layout = proc.ret_layout;
    const size_align = lowered.lir_result.layouts.layoutSizeAlign(lowered.lir_result.layouts.getLayout(ret_layout));
    const alloc_len = @max(size_align.size, 1);
    const ret_buf = try allocator.alignedAlloc(u8, collections.max_roc_alignment, alloc_len);
    defer allocator.free(ret_buf);
    @memset(ret_buf, 0);

    exec_mem.callWithResultPtrAndRocOps(@ptrCast(ret_buf.ptr), @ptrCast(runtime_env.get_ops()));

    return copyReturnedRocStr(
        allocator,
        &lowered.lir_result.layouts,
        ret_layout,
        ret_buf.ptr,
        runtime_env.get_ops(),
    );
}

pub fn wasmEvaluatorInspectedStr(
    allocator: std.mem.Allocator,
    module_env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    builtin_module_env: *const ModuleEnv,
) ![]u8 {
    _ = allocator;
    _ = module_env;
    _ = expr_idx;
    _ = builtin_module_env;
    @panic("TODO: wasm eval backend not implemented for the cor-style inspect-only pipeline");
}

fn lowerToLir(
    allocator: std.mem.Allocator,
    module_env: *ModuleEnv,
    builtin_module_env: *const ModuleEnv,
) !LoweredProgram {
    const all_module_envs = [_]*const ModuleEnv{
        module_env,
        builtin_module_env,
    };
    module_env.imports.resolveImports(module_env, &all_module_envs);

    var mono_lowerer = monotype.Lower.Lowerer.init(allocator, &all_module_envs, 1);
    defer mono_lowerer.deinit();
    const mono = try mono_lowerer.run(0);
    const lifted = try monotype_lifted.Lower.run(allocator, mono);
    const solved = try lambdasolved.Lower.run(allocator, lifted);
    const executable = try lambdamono.Lower.run(allocator, solved);
    const lowered_ir = try ir.Lower.run(allocator, executable);

    var lowered_lir = try FromIr.run(
        allocator,
        &all_module_envs,
        null,
        base.target.TargetUsize.native,
        lowered_ir,
    );
    errdefer lowered_lir.deinit();
    if (lowered_lir.root_procs.items.len == 0) return error.NoRootProc;

    return .{
        .lir_result = lowered_lir,
        .main_proc = lowered_lir.root_procs.items[lowered_lir.root_procs.items.len - 1],
    };
}

fn copyReturnedRocStr(
    allocator: std.mem.Allocator,
    layout_store: *const LayoutStore,
    ret_layout: LayoutIdx,
    value_ptr: [*]u8,
    roc_ops: ?*builtins.host_abi.RocOps,
) ![]u8 {
    const layout_val = layout_store.getLayout(ret_layout);
    const is_str =
        ret_layout == .str or
        (layout_val.tag == .scalar and layout_val.data.scalar.tag == .str);

    if (!is_str) {
        std.debug.panic(
            "eval inspect invariant violated: expected Str return layout, found {s}",
            .{@tagName(layout_val.tag)},
        );
    }

    const roc_str = @as(*align(1) const RocStr, @ptrCast(value_ptr)).*;
    const copied = try allocator.dupe(u8, roc_str.asSlice());
    if (roc_ops) |ops| roc_str.decref(ops);
    return copied;
}
