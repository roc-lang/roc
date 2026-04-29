//! Source-blind IR to statement-only LIR lowering boundary.
//!
//! This stage consumes executable IR and committed logical layouts. It is not
//! allowed to inspect checked CIR, MIR builders, method names, source syntax, or
//! reference-counting policy. Reference counting is inserted later by `arc.zig`.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const ir = @import("ir");
const layout_mod = @import("layout");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;

pub const LowerResourceError = Allocator.Error;

pub const ProcMapEntry = struct {
    executable_proc: ir.Ast.ProcRef,
    lir_proc: LIR.LirProcSpecId,
};

pub const Result = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    root_procs: std.ArrayList(LIR.LirProcSpecId),
    proc_map: std.ArrayList(ProcMapEntry),

    pub fn deinit(self: *Result) void {
        self.proc_map.deinit(self.store.allocator);
        self.root_procs.deinit(self.store.allocator);
        self.layouts.deinit();
        self.store.deinit();
    }

    pub fn lirProcForExecutable(self: *const Result, proc: ir.Ast.ProcRef) ?LIR.LirProcSpecId {
        for (self.proc_map.items) |entry| {
            if (entry.executable_proc == proc) return entry.lir_proc;
        }
        return null;
    }
};

pub fn run(
    allocator: Allocator,
    all_module_envs: []const *const ModuleEnv,
    builtin_str_ident: ?base.Ident.Idx,
    target_usize: base.target.TargetUsize,
    input: ir.Lower.Program,
    explicit_roots: []const ir.Ast.ProcRef,
) LowerResourceError!Result {
    var owned_input = input;
    errdefer owned_input.deinit();

    var lowerer = try Lowerer.init(allocator, all_module_envs, builtin_str_ident, target_usize, &owned_input);
    errdefer lowerer.deinit();

    try lowerer.registerProcPlaceholders();
    try lowerer.lowerAllDefs();
    try lowerer.bindRoots(explicit_roots);

    owned_input.deinit();
    return lowerer.finish();
}

const Lowerer = struct {
    allocator: Allocator,
    input: *const ir.Lower.Program,
    store: LirStore,
    layouts: layout_mod.Store,
    root_procs: std.ArrayList(LIR.LirProcSpecId),
    proc_map: std.ArrayList(ProcMapEntry),

    fn init(
        allocator: Allocator,
        all_module_envs: []const *const ModuleEnv,
        builtin_str_ident: ?base.Ident.Idx,
        target_usize: base.target.TargetUsize,
        input: *const ir.Lower.Program,
    ) LowerResourceError!Lowerer {
        return .{
            .allocator = allocator,
            .input = input,
            .store = LirStore.init(allocator),
            .layouts = try layout_mod.Store.init(all_module_envs, builtin_str_ident, allocator, target_usize),
            .root_procs = .empty,
            .proc_map = .empty,
        };
    }

    fn deinit(self: *Lowerer) void {
        self.proc_map.deinit(self.allocator);
        self.root_procs.deinit(self.allocator);
        self.layouts.deinit();
        self.store.deinit();
    }

    fn finish(self: *Lowerer) Result {
        const result = Result{
            .store = self.store,
            .layouts = self.layouts,
            .root_procs = self.root_procs,
            .proc_map = self.proc_map,
        };
        self.store = LirStore.init(self.allocator);
        self.layouts = undefined;
        self.root_procs = .empty;
        self.proc_map = .empty;
        return result;
    }

    fn registerProcPlaceholders(self: *Lowerer) LowerResourceError!void {
        for (self.input.store.defsSlice()) |def| {
            const args = self.input.store.sliceVarSpan(def.args);
            const arg_locals = try self.allocator.alloc(LIR.LocalId, args.len);
            defer self.allocator.free(arg_locals);

            for (args, 0..) |arg, i| {
                arg_locals[i] = try self.store.addLocal(.{
                    .layout_idx = try self.lowerLayoutRef(arg.layout),
                });
            }

            const proc_id = try self.store.addProcSpec(.{
                .name = self.store.freshSyntheticSymbol(),
                .args = try self.store.addLocalSpan(arg_locals),
                .body = null,
                .ret_layout = try self.lowerLayoutRef(def.ret_layout),
                .hosted = if (def.hosted) |hosted| .{
                    .symbol_name = hosted.symbol_name,
                    .index = hosted.index,
                } else null,
            });

            try self.proc_map.append(self.allocator, .{
                .executable_proc = def.proc,
                .lir_proc = proc_id,
            });
        }
    }

    fn lowerAllDefs(self: *Lowerer) LowerResourceError!void {
        for (self.input.store.defsSlice()) |def| {
            if (def.hosted != null) continue;
            if (def.body == null) {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic("lir.lower_ir invariant violated: non-hosted IR def has no body", .{});
                }
                unreachable;
            }
            try self.lowerDef(def);
        }
    }

    fn lowerDef(self: *Lowerer, def: ir.Ast.Def) LowerResourceError!void {
        const lir_proc = self.lirProcForExecutable(def.proc) orelse {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic("lir.lower_ir invariant violated: missing proc placeholder", .{});
            }
            unreachable;
        };

        const proc = self.store.getProcSpecPtr(lir_proc);
        proc.body = try self.lowerBlock(def.body.?);
    }

    fn bindRoots(self: *Lowerer, explicit_roots: []const ir.Ast.ProcRef) LowerResourceError!void {
        try self.root_procs.ensureTotalCapacity(self.allocator, explicit_roots.len);
        for (explicit_roots) |root| {
            const proc = self.lirProcForExecutable(root) orelse {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic("lir.lower_ir invariant violated: explicit root has no LIR proc", .{});
                }
                unreachable;
            };
            self.root_procs.appendAssumeCapacity(proc);
        }
    }

    fn lowerBlock(self: *Lowerer, block_id: ir.Ast.BlockId) LowerResourceError!LIR.CFStmtId {
        const block = self.input.store.getBlock(block_id);
        return switch (block.term) {
            .return_ => |ret| try self.store.addCFStmt(.{ .ret = .{ .value = try self.lowerVar(ret) } }),
            .value => |value| try self.store.addCFStmt(.{ .ret = .{ .value = try self.lowerVar(value) } }),
            .crash => |msg| try self.store.addCFStmt(.{ .crash = .{ .msg = msg } }),
            .runtime_error, .@"unreachable" => try self.store.addCFStmt(.{ .runtime_error = {} }),
        };
    }

    fn lowerVar(self: *Lowerer, var_: ir.Ast.Var) LowerResourceError!LIR.LocalId {
        return try self.store.addLocal(.{
            .layout_idx = try self.lowerLayoutRef(var_.layout),
        });
    }

    fn lowerLayoutRef(self: *Lowerer, ref: ir.Layout.Ref) LowerResourceError!layout_mod.Idx {
        return switch (ref) {
            .canonical => |idx| idx,
            .local => blk: {
                var commit = try self.layouts.commitGraph(&self.input.layouts, ref);
                defer commit.deinit(self.allocator);
                break :blk commit.root_idx;
            },
        };
    }

    fn lirProcForExecutable(self: *const Lowerer, proc: ir.Ast.ProcRef) ?LIR.LirProcSpecId {
        for (self.proc_map.items) |entry| {
            if (entry.executable_proc == proc) return entry.lir_proc;
        }
        return null;
    }
};

test "IR to LIR lowering exposes only resource errors" {
    std.testing.refAllDecls(@This());
}
