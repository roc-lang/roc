//! Shared input and small ids for the post-check pipeline.

const std = @import("std");
const base = @import("base");
const check = @import("check");

const checked = check.CheckedModule;

pub const LowerError = std.mem.Allocator.Error;

pub const CheckedModules = struct {
    root: checked.LoweringModuleView,
    imports: []const checked.ImportedModuleView = &.{},
};

pub const RootRequests = struct {
    requests: []const checked.RootRequest = &.{},
};

pub const Target = struct {
    target_usize: base.target.TargetUsize = base.target.TargetUsize.native,
    checked_state: CheckedState = .published,
};

pub const CheckedState = enum {
    published,
    checking_finalization,
};

pub const Symbol = enum(u32) { _ };
pub const ConstNodeId = enum(u32) { _ };
pub const FnSetId = enum(u32) { _ };
pub const ErasedFnsId = enum(u32) { _ };
pub const CaptureSlotId = enum(u32) { _ };

pub fn invariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("postcheck invariant violated: {s}", .{message});
    }
    unreachable;
}

pub const SymbolGen = struct {
    next: u32 = 0,

    pub fn fresh(self: *SymbolGen) Symbol {
        const symbol: Symbol = @enumFromInt(self.next);
        self.next += 1;
        return symbol;
    }
};

test "common declarations are referenced" {
    std.testing.refAllDecls(@This());
}
