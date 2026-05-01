//! Lambda-solved MIR construction state.

const std = @import("std");
const check = @import("check");
const Lifted = @import("../lifted/mod.zig");
const ids = @import("../ids.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");
const repr = @import("representation.zig");

const Allocator = std.mem.Allocator;
const canonical = check.CanonicalNames;

pub const Proc = struct {
    proc: canonical.MonoSpecializedProcRef,
    body: ?Ast.DefId = null,
    representation_instance: repr.ProcRepresentationInstanceId,
};

pub const Program = struct {
    allocator: Allocator,
    literal_pool: ids.ProgramLiteralPool,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    root_procs: std.ArrayList(canonical.MonoSpecializedProcRef),
    solve_sessions: std.ArrayList(repr.RepresentationSolveSession),
    proc_instances: std.ArrayList(repr.ProcRepresentationInstance),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .literal_pool = ids.ProgramLiteralPool.init(allocator),
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
            .root_procs = .empty,
            .solve_sessions = .empty,
            .proc_instances = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.proc_instances.deinit(self.allocator);
        self.solve_sessions.deinit(self.allocator);
        self.root_procs.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.literal_pool.deinit();
        self.* = Program.init(self.allocator);
    }
};

pub fn run(allocator: Allocator, lifted: Lifted.Lift.Program) Allocator.Error!Program {
    var input = lifted;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();
    program.literal_pool = input.literal_pool;
    input.literal_pool = ids.ProgramLiteralPool.init(allocator);

    try program.procs.ensureTotalCapacity(allocator, input.procs.items.len);
    for (input.procs.items, 0..) |proc, i| {
        const instance: repr.ProcRepresentationInstanceId = @enumFromInt(@as(u32, @intCast(i)));
        program.procs.appendAssumeCapacity(.{
            .proc = proc.proc,
            .body = null,
            .representation_instance = instance,
        });
    }
    try program.root_procs.appendSlice(allocator, input.root_procs.items);

    input.deinit();
    return program;
}

test "lambda-solved program owns representation tables" {
    std.testing.refAllDecls(@This());
}
