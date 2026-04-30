//! Lambda lifting state for row-finalized mono MIR.

const std = @import("std");
const check = @import("check");
const symbol_mod = @import("symbol");
const MonoRow = @import("../mono_row/mod.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const canonical = check.CanonicalNames;
const Symbol = symbol_mod.Symbol;

pub const ProcOrderKey = struct {
    ordinal: u32,
};

pub const LiftedGroupMember = struct {
    source_symbol: Symbol,
    lifted_proc: canonical.ProcedureValueRef,
    order_key: ProcOrderKey,
    args: Ast.Span(Ast.TypedSymbol),
    capture_slots: Ast.Span(Ast.CaptureSlot),
};

pub const CaptureValueEdge = struct {
    from_proc: canonical.ProcedureValueRef,
    source_symbol: Symbol,
    source_ty: Type.TypeId,
};

pub const CaptureProcValueEdge = struct {
    from_proc: canonical.ProcedureValueRef,
    referenced_proc: canonical.ProcedureValueRef,
};

pub const LiftedCaptureGraph = struct {
    members: []const LiftedGroupMember = &.{},
    value_edges: []const CaptureValueEdge = &.{},
    proc_value_edges: []const CaptureProcValueEdge = &.{},
};

pub const Proc = struct {
    proc: canonical.MonoSpecializedProcRef,
    order_key: ProcOrderKey,
    body: ?Ast.DefId = null,
};

pub const Program = struct {
    allocator: Allocator,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    root_procs: std.ArrayList(canonical.MonoSpecializedProcRef),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
            .root_procs = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.root_procs.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.* = Program.init(self.allocator);
    }
};

pub fn run(allocator: Allocator, row_result: MonoRow.Result) Allocator.Error!Program {
    var input = row_result;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();
    program.types = input.program.types;
    input.program.types = Type.Store.init(allocator);

    try program.procs.ensureTotalCapacity(allocator, input.program.procs.items.len);
    for (input.program.procs.items, 0..) |proc, i| {
        program.procs.appendAssumeCapacity(.{
            .proc = proc.proc,
            .order_key = .{ .ordinal = @intCast(i) },
            .body = null,
        });
    }
    try program.root_procs.appendSlice(allocator, input.program.root_procs.items);

    input.deinit();
    return program;
}

test "lifted capture graph has explicit edge records" {
    std.testing.refAllDecls(@This());
}
