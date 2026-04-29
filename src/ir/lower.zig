//! Executable MIR to IR lowering boundary.

const std = @import("std");
const mir = @import("mir");

const Ast = @import("ast.zig");
const Layout = @import("layout.zig");

const Allocator = std.mem.Allocator;

pub const LowerResourceError = Allocator.Error;

pub const Program = struct {
    allocator: Allocator,
    store: Ast.Store,
    layouts: Layout.Graph,
    root_procs: std.ArrayList(Ast.ProcRef),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .store = Ast.Store.init(allocator),
            .layouts = Layout.Graph.init(allocator),
            .root_procs = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.root_procs.deinit(self.allocator);
        self.layouts.deinit();
        self.store.deinit();
        self.* = Program.init(self.allocator);
    }
};

pub fn fromExecutable(allocator: Allocator, executable: mir.Executable.Build.Program) LowerResourceError!Program {
    var input = executable;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();

    try program.root_procs.appendSlice(allocator, input.root_procs.items);

    input.deinit();
    return program;
}

test "IR lowering consumes executable MIR only" {
    std.testing.refAllDecls(@This());
}
