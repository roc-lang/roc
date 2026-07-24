//! Subprocess harness for guarded-list accesses that must fail in Debug.

const std = @import("std");
const collections = @import("collections");
const check = @import("check");
const layout = @import("layout");
const lir = @import("lir");
const postcheck = @import("postcheck");

const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;
const ViolationError = Allocator.Error || error{
    ExpectedGuardedListPanic,
    MissingCaseName,
    UnknownCaseName,
};
const LIR = lir.LIR;
const Mono = postcheck.Monotype;
const Lifted = postcheck.MonotypeLifted;
const LambdaMono = postcheck.LambdaMono;

const MoveAllocator = struct {
    const vtable = Allocator.VTable{
        .alloc = alloc,
        .resize = resize,
        .remap = remap,
        .free = free,
    };

    fn allocator(self: *MoveAllocator) Allocator {
        return .{ .ptr = self, .vtable = &vtable };
    }

    fn alloc(_: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
        return std.heap.page_allocator.rawAlloc(len, alignment, ret_addr);
    }

    fn resize(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) bool {
        return false;
    }

    fn remap(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) ?[*]u8 {
        return null;
    }

    fn free(_: *anyopaque, memory: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
        std.heap.page_allocator.rawFree(memory, alignment, ret_addr);
    }
};

const TestList = GuardedList.List(u32, "guarded_list_violation_test.values");

/// Runs one guarded-list expected-failure case selected by command-line name.
pub fn main(init: std.process.Init) ViolationError!void {
    var args = try std.process.Args.Iterator.initAllocator(init.minimal.args, init.gpa);
    defer args.deinit();

    _ = args.next();
    const case_name = args.next() orelse return error.MissingCaseName;

    if (std.mem.eql(u8, case_name, "span_append_move")) return spanAppendMove();
    if (std.mem.eql(u8, case_name, "ptr_append_move")) return ptrAppendMove();
    if (std.mem.eql(u8, case_name, "span_ensure_move")) return spanEnsureMove();
    if (std.mem.eql(u8, case_name, "span_append_slice_move")) return spanAppendSliceMove();
    if (std.mem.eql(u8, case_name, "span_restore_below_range")) return spanRestoreBelowRange();
    if (std.mem.eql(u8, case_name, "ptr_restore_below_index")) return ptrRestoreBelowIndex();
    if (std.mem.eql(u8, case_name, "span_clear")) return spanClear();
    if (std.mem.eql(u8, case_name, "span_ownership_transfer")) return spanOwnershipTransfer();
    if (std.mem.eql(u8, case_name, "lir_proc_specs")) return lirProcSpecs();
    if (std.mem.eql(u8, case_name, "lir_local_span")) return lirLocalSpan();
    if (std.mem.eql(u8, case_name, "lifted_fns")) return liftedFns();
    if (std.mem.eql(u8, case_name, "lifted_expr_ids")) return liftedExprIds();
    if (std.mem.eql(u8, case_name, "mono_exprs")) return monoExprs();
    if (std.mem.eql(u8, case_name, "mono_type_spans")) return monoTypeSpans();
    if (std.mem.eql(u8, case_name, "mono_type_fields")) return monoTypeFields();
    if (std.mem.eql(u8, case_name, "lambda_mono_expr_ids")) return lambdaMonoExprIds();
    if (std.mem.eql(u8, case_name, "lambda_mono_type_spans")) return lambdaMonoTypeSpans();

    return error.UnknownCaseName;
}

fn spanAppendMove() ViolationError!void {
    var move_allocator = MoveAllocator{};
    const allocator = move_allocator.allocator();
    var list = try TestList.initCapacity(allocator, 1);
    defer list.deinit(allocator);

    try list.append(allocator, 1);
    const borrow = list.borrowSpan(0, 1);
    try list.append(allocator, 2);
    _ = GuardedList.at(borrow, 0);
    return error.ExpectedGuardedListPanic;
}

fn ptrAppendMove() ViolationError!void {
    var move_allocator = MoveAllocator{};
    const allocator = move_allocator.allocator();
    var list = try TestList.initCapacity(allocator, 1);
    defer list.deinit(allocator);

    try list.append(allocator, 1);
    const borrow = list.borrowPtr(0);
    try list.append(allocator, 2);
    _ = GuardedList.ptrGet(borrow);
    return error.ExpectedGuardedListPanic;
}

fn spanEnsureMove() ViolationError!void {
    var move_allocator = MoveAllocator{};
    const allocator = move_allocator.allocator();
    var list = try TestList.initCapacity(allocator, 1);
    defer list.deinit(allocator);

    try list.append(allocator, 1);
    const borrow = list.borrowSpan(0, 1);
    try list.ensureUnusedCapacity(allocator, 1);
    _ = GuardedList.at(borrow, 0);
    return error.ExpectedGuardedListPanic;
}

fn spanAppendSliceMove() ViolationError!void {
    var move_allocator = MoveAllocator{};
    const allocator = move_allocator.allocator();
    var list = try TestList.initCapacity(allocator, 1);
    defer list.deinit(allocator);

    try list.append(allocator, 1);
    const borrow = list.borrowSpan(0, 1);
    try list.appendSlice(allocator, &.{ 2, 3 });
    _ = GuardedList.at(borrow, 0);
    return error.ExpectedGuardedListPanic;
}

fn spanRestoreBelowRange() ViolationError!void {
    var list = try TestList.initCapacity(std.heap.page_allocator, 4);
    defer list.deinit(std.heap.page_allocator);

    try list.appendSlice(std.heap.page_allocator, &.{ 1, 2, 3, 4 });
    const borrow = list.borrowSpan(2, 2);
    list.restoreLen(2);
    _ = GuardedList.at(borrow, 0);
    return error.ExpectedGuardedListPanic;
}

fn ptrRestoreBelowIndex() ViolationError!void {
    var list = try TestList.initCapacity(std.heap.page_allocator, 4);
    defer list.deinit(std.heap.page_allocator);

    try list.appendSlice(std.heap.page_allocator, &.{ 1, 2, 3, 4 });
    const borrow = list.borrowPtr(3);
    list.restoreLen(3);
    _ = GuardedList.ptrGet(borrow);
    return error.ExpectedGuardedListPanic;
}

fn spanClear() ViolationError!void {
    var list = try TestList.initCapacity(std.heap.page_allocator, 4);
    defer list.deinit(std.heap.page_allocator);

    try list.appendSlice(std.heap.page_allocator, &.{ 1, 2, 3, 4 });
    const borrow = list.borrowSpan(0, 1);
    list.clearRetainingCapacity();
    _ = GuardedList.at(borrow, 0);
    return error.ExpectedGuardedListPanic;
}

fn spanOwnershipTransfer() ViolationError!void {
    var list = try TestList.initCapacity(std.heap.page_allocator, 4);

    try list.appendSlice(std.heap.page_allocator, &.{ 1, 2, 3, 4 });
    const borrow = list.borrowSpan(0, 1);
    var moved = list.takeArrayList();
    defer moved.deinit(std.heap.page_allocator);

    _ = GuardedList.at(borrow, 0);
    return error.ExpectedGuardedListPanic;
}

fn lirProcSpecs() ViolationError!void {
    var move_allocator = MoveAllocator{};
    const allocator = move_allocator.allocator();
    var store = lir.LirStore.init(allocator);
    defer store.deinit();

    try store.proc_specs.ensureTotalCapacityPrecise(allocator, 1);
    _ = try store.addProcSpec(dummyProcSpec(1));
    const borrow = store.proc_specs.borrowPtr(0);
    _ = try store.addProcSpec(dummyProcSpec(2));
    _ = GuardedList.ptrGet(borrow);
    return error.ExpectedGuardedListPanic;
}

fn lirLocalSpan() ViolationError!void {
    var move_allocator = MoveAllocator{};
    const allocator = move_allocator.allocator();
    var store = lir.LirStore.init(allocator);
    defer store.deinit();

    const first_local = try store.addLocal(.{ .layout_idx = layout.Idx.u8 });
    const second_local = try store.addLocal(.{ .layout_idx = layout.Idx.u16 });
    try store.local_ids.ensureTotalCapacityPrecise(allocator, 1);
    const span = try store.addLocalSpan(&.{first_local});
    const borrow = store.local_ids.borrowSpan(span.start, span.len);
    _ = try store.addLocalSpan(&.{second_local});
    _ = GuardedList.at(borrow, 0);
    return error.ExpectedGuardedListPanic;
}

fn liftedFns() ViolationError!void {
    var move_allocator = MoveAllocator{};
    const allocator = move_allocator.allocator();
    var program = emptyLiftedProgram(allocator);
    defer program.deinit();

    const ret = try program.types.add(.zst);
    try program.fns.ensureTotalCapacityPrecise(allocator, 1);
    _ = try program.addFn(dummyLiftedFn(1, ret));
    const borrow = program.fns.borrowPtr(0);
    _ = try program.addFn(dummyLiftedFn(2, ret));
    _ = GuardedList.ptrGet(borrow);
    return error.ExpectedGuardedListPanic;
}

fn liftedExprIds() ViolationError!void {
    var move_allocator = MoveAllocator{};
    const allocator = move_allocator.allocator();
    var program = emptyLiftedProgram(allocator);
    defer program.deinit();

    const ty = try program.types.add(.zst);
    const first_expr = try program.addExpr(.{ .ty = ty, .data = .unit });
    const second_expr = try program.addExpr(.{ .ty = ty, .data = .unit });
    try program.expr_ids.ensureTotalCapacityPrecise(allocator, 1);
    const span = try program.addExprSpan(&.{first_expr});
    const borrow = program.expr_ids.borrowSpan(span.start, span.len);
    _ = try program.addExprSpan(&.{second_expr});
    _ = GuardedList.at(borrow, 0);
    return error.ExpectedGuardedListPanic;
}

fn monoExprs() ViolationError!void {
    var move_allocator = MoveAllocator{};
    const allocator = move_allocator.allocator();
    var program = Mono.Ast.ProgramBuilder.init(allocator);
    defer program.deinit();

    const ty = try program.types.add(.zst);
    try program.exprs.ensureTotalCapacityPrecise(allocator, 1);
    _ = try program.addExpr(dummyMonoExpr(ty));
    const borrow = program.exprs.borrowPtr(0);
    _ = try program.addExpr(dummyMonoExpr(ty));
    _ = GuardedList.ptrGet(borrow);
    return error.ExpectedGuardedListPanic;
}

fn monoTypeSpans() ViolationError!void {
    var move_allocator = MoveAllocator{};
    const allocator = move_allocator.allocator();
    var store = Mono.Type.Store.init(allocator);
    defer store.deinit();

    const first_ty = try store.add(.zst);
    const second_ty = try store.add(.zst);
    try store.spans.ensureTotalCapacityPrecise(allocator, 1);
    const span = try store.addSpan(&.{first_ty});
    const borrow = store.spans.borrowSpan(span.start, span.len);
    _ = try store.addSpan(&.{second_ty});
    _ = GuardedList.at(borrow, 0);
    return error.ExpectedGuardedListPanic;
}

fn monoTypeFields() ViolationError!void {
    var move_allocator = MoveAllocator{};
    const allocator = move_allocator.allocator();
    var store = Mono.Type.Store.init(allocator);
    defer store.deinit();

    const ty = try store.add(.zst);
    try store.fields.ensureTotalCapacityPrecise(allocator, 1);
    const span = try store.addFields(&.{dummyMonoTypeField(1, ty)});
    const borrow = store.fields.borrowSpan(span.start, span.len);
    _ = try store.addFields(&.{dummyMonoTypeField(2, ty)});
    _ = GuardedList.at(borrow, 0);
    return error.ExpectedGuardedListPanic;
}

fn lambdaMonoExprIds() ViolationError!void {
    var move_allocator = MoveAllocator{};
    const allocator = move_allocator.allocator();
    var program = LambdaMono.Ast.Program.init(allocator, check.CheckedNames.NameStore.init(allocator), .empty);
    defer program.deinit();

    const ty = try program.types.add(.zst);
    const first_expr = try program.addExpr(.{ .ty = ty, .data = .unit });
    const second_expr = try program.addExpr(.{ .ty = ty, .data = .unit });
    try program.expr_ids.ensureTotalCapacityPrecise(allocator, 1);
    const span = try program.addExprSpan(&.{first_expr});
    const borrow = program.expr_ids.borrowSpan(span.start, span.len);
    _ = try program.addExprSpan(&.{second_expr});
    _ = GuardedList.at(borrow, 0);
    return error.ExpectedGuardedListPanic;
}

fn lambdaMonoTypeSpans() ViolationError!void {
    var move_allocator = MoveAllocator{};
    const allocator = move_allocator.allocator();
    var store = LambdaMono.Type.Store.init(allocator);
    defer store.deinit();

    const first_ty = try store.add(.zst);
    const second_ty = try store.add(.zst);
    try store.spans.ensureTotalCapacityPrecise(allocator, 1);
    const span = try store.addSpan(&.{first_ty});
    const borrow = store.spans.borrowSpan(span.start, span.len);
    _ = try store.addSpan(&.{second_ty});
    _ = GuardedList.at(borrow, 0);
    return error.ExpectedGuardedListPanic;
}

fn emptyLiftedProgram(allocator: Allocator) Lifted.Ast.Program {
    return Lifted.Ast.Program.init(
        allocator,
        check.CheckedNames.NameStore.init(allocator),
        Mono.Type.Store.init(allocator),
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        Mono.Ast.ProcDebugNameMap.init(allocator),
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        0,
    );
}

fn dummyProcSpec(raw: u64) LIR.LirProcSpec {
    return .{
        .name = LIR.Symbol.fromRaw(raw),
        .args = LIR.LocalSpan.empty(),
        .ret_layout = layout.Idx.u8,
    };
}

fn dummyLiftedFn(raw: u32, ret: Mono.Type.TypeId) Lifted.Ast.Fn {
    return .{
        .symbol = @enumFromInt(raw),
        .args = Lifted.Ast.Span(Lifted.Ast.TypedLocal).empty(),
        .captures = Lifted.Ast.Span(Lifted.Ast.TypedLocal).empty(),
        .body = .hosted,
        .ret = ret,
    };
}

fn dummyMonoExpr(ty: Mono.Type.TypeId) Mono.Ast.Expr {
    return .{
        .ty = ty,
        .data = .unit,
    };
}

fn dummyMonoTypeField(raw: u32, ty: Mono.Type.TypeId) Mono.Type.Field {
    return .{
        .name = @enumFromInt(raw),
        .ty = ty,
    };
}
