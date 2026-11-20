//! Give every lifted function capture slots of its own.
//!
//! Lifting names a closure's capture slots by the locals of the function
//! that defined it, so the closure's body reads the definer's locals. Lambda
//! Solved IR instead treats a capture slot like a parameter: each function
//! with captures gets a fresh local per slot (a copy carrying the slot
//! local's complete identity, so capture operands still match their slots by
//! `CaptureId`), and its body reads those locals. No function's types then
//! depend on a local another function binds, and a capture operand meets its
//! slot only through the reference that supplies it, exactly as an argument
//! meets its parameter (design.md "Lambda Set Generalization").

const std = @import("std");
const collections = @import("collections");

const Common = @import("../common.zig");
const Ast = @import("ast.zig");
const body_copy = @import("body_copy.zig");

const Allocator = std.mem.Allocator;
const GuardedList = collections.GuardedList;

/// Give each function of `program` with captures its own capture locals.
pub fn run(program: *Ast.Program) Allocator.Error!void {
    const allocator = program.allocator;
    var symbols: Common.SymbolGen = .{ .next = program.next_symbol };
    defer program.next_symbol = symbols.next;
    var renames: body_copy.Renames = .empty;
    defer renames.deinit(allocator);
    var slots: std.ArrayList(Ast.TypedLocal) = .empty;
    defer slots.deinit(allocator);

    for (0..program.fnCount()) |index| {
        var fn_ = program.getFnAt(index);
        if (fn_.captures.len == 0) continue;
        renames.clearRetainingCapacity();
        slots.clearRetainingCapacity();
        const captures = program.typedLocalSpan(fn_.captures);
        try slots.ensureTotalCapacity(allocator, captures.len);
        for (0..captures.len) |slot_index| slots.appendAssumeCapacity(GuardedList.at(captures, slot_index));
        for (slots.items) |*slot| {
            const own = try program.addLocalCopy(slot.local, symbols.fresh());
            try renames.put(allocator, slot.local, own);
            slot.local = own;
        }
        fn_.captures = try program.addTypedLocalSpan(slots.items);
        fn_.body = switch (fn_.body) {
            .roc => |body| .{ .roc = try body_copy.copyRenamingReads(program, body, &renames) },
            .hosted => Common.invariant("a hosted function had captures"),
        };
        program.setFnAt(index, fn_);
    }
}
