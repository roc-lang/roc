//! The erased callables completed compile-time values hold.
//!
//! An erased callable type's member set names every function the whole
//! program may put in a value of that type, but a program compiles only the
//! members something it runs can create (design.md "Dependency-Ordered
//! Compile-Time Evaluation"). A value a later program reads from an earlier
//! stage's frozen image may hold a member that program never creates: the
//! stage that evaluated the value created it. The program still needs that
//! member's code, because the transcoded value points at it, so each such
//! member is named here by the frozen identity transcoding matches it by.
//! A member here was compiled by the stage that created it, so every
//! compile-time value it reads was complete when that stage finished.
const std = @import("std");
const core = @import("lir_core");
const Common = @import("common.zig");

const Program = core.Program;
const Allocator = std.mem.Allocator;

/// The set of erased callables held by completed frozen values.
pub const CompletedErasedMembers = struct {
    members: std.AutoHashMapUnmanaged(Key, void) = .empty,

    /// The identity frozen-value transcoding pairs a source callable with
    /// its target entry by.
    const Key = struct {
        frozen_fn: ?u32,
        frozen_worker: ?[96]u8,
        frozen_context: ?Program.FrozenCallableContext,
    };

    fn keyOf(template: Program.FnTemplate) Key {
        return .{ .frozen_fn = template.frozen_fn, .frozen_worker = template.frozen_worker, .frozen_context = template.frozen_context };
    }

    /// Records every erased callable `program`'s frozen image holds. Members
    /// that share one procedure have the same code, so each of them counts
    /// as held.
    pub fn add(self: *CompletedErasedMembers, allocator: Allocator, program: *const Program.Result, frozen: *const Program.FrozenStaticData) Allocator.Error!void {
        for (frozen.exports) |item| for (item.relocations) |relocation| {
            if (relocation.callable_capture_offset == null) continue;
            const proc = relocation.procedure orelse Common.invariant("frozen erased callable lacked its procedure");
            var found = false;
            for (program.erased_fns.items) |set| for (set.entries) |entry| {
                if (entry.entry != proc) continue;
                const template = entry.template orelse Common.invariant("frozen erased callable's entry lacked its template");
                try self.members.put(allocator, keyOf(template), {});
                found = true;
            };
            if (!found) Common.invariant("frozen erased callable named a procedure no erased entry has");
        };
    }

    /// Whether a completed value holds the erased callable `template` names.
    pub fn contains(self: *const CompletedErasedMembers, template: Program.FnTemplate) bool {
        return self.members.contains(keyOf(template));
    }

    pub fn deinit(self: *CompletedErasedMembers, allocator: Allocator) void {
        self.members.deinit(allocator);
        self.* = undefined;
    }
};
