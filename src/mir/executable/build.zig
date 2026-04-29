//! Executable MIR construction state.

const std = @import("std");
const check = @import("check");
const LambdaSolved = @import("../lambda_solved/mod.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");
const Layouts = @import("layouts.zig");

const Allocator = std.mem.Allocator;
const canonical = check.CanonicalNames;

pub const Proc = struct {
    executable_proc: Ast.ExecutableProcId,
    source_proc: canonical.ProcedureValueRef,
    body: ?Ast.DefId = null,
};

pub const Program = struct {
    allocator: Allocator,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    root_procs: std.ArrayList(Ast.ExecutableProcId),
    layouts: ?Layouts.Layouts = null,

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
        if (self.layouts) |*layouts| layouts.deinit();
        self.root_procs.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.* = Program.init(self.allocator);
    }
};

pub fn run(allocator: Allocator, solved: LambdaSolved.Solve.Program) Allocator.Error!Program {
    var input = solved;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();

    try program.procs.ensureTotalCapacity(allocator, input.procs.items.len);
    for (input.procs.items, 0..) |proc, i| {
        program.procs.appendAssumeCapacity(.{
            .executable_proc = @enumFromInt(@as(u32, @intCast(i))),
            .source_proc = proc.proc,
            .body = null,
        });
    }

    for (input.root_procs.items) |root| {
        const executable_root = executableProcForSource(&program, root) orelse {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic("executable build invariant violated: root source proc has no executable proc", .{});
            }
            unreachable;
        };
        try program.root_procs.append(allocator, executable_root);
    }

    input.deinit();
    return program;
}

fn executableProcForSource(program: *const Program, source_proc: canonical.ProcedureValueRef) ?Ast.ExecutableProcId {
    for (program.procs.items) |proc| {
        if (std.mem.eql(u8, &proc.source_proc.artifact.bytes, &source_proc.artifact.bytes) and
            proc.source_proc.proc_base == source_proc.proc_base)
        {
            return proc.executable_proc;
        }
    }
    return null;
}

test "executable build owns final program state" {
    std.testing.refAllDecls(@This());
}
