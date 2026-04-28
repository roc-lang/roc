//! Lambda-solved MIR construction state.

const std = @import("std");
const check = @import("check");
const Lifted = @import("../lifted/mod.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");
const repr = @import("representation.zig");

const Allocator = std.mem.Allocator;
const canonical = check.CanonicalNames;

pub const Proc = struct {
    proc: canonical.ProcedureValueRef,
    body: ?Ast.DefId = null,
    representation_instance: repr.ProcRepresentationInstanceId,
};

pub const Program = struct {
    allocator: Allocator,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    solve_sessions: std.ArrayList(repr.RepresentationSolveSession),
    proc_instances: std.ArrayList(repr.ProcRepresentationInstance),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
            .solve_sessions = .empty,
            .proc_instances = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.proc_instances.deinit(self.allocator);
        self.solve_sessions.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.* = Program.init(self.allocator);
    }
};

pub fn run(allocator: Allocator, lifted: Lifted.Lift.Program) Allocator.Error!Program {
    var input = lifted;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();

    try program.procs.ensureTotalCapacity(allocator, input.procs.items.len);
    for (input.procs.items, 0..) |proc, i| {
        const instance: repr.ProcRepresentationInstanceId = @enumFromInt(@as(u32, @intCast(i)));
        program.procs.appendAssumeCapacity(.{
            .proc = proc.proc,
            .body = null,
            .representation_instance = instance,
        });
    }

    input.deinit();
    return program;
}

test "lambda-solved program owns representation tables" {
    std.testing.refAllDecls(@This());
}
