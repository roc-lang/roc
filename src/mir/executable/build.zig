//! Executable MIR construction state.

const std = @import("std");
const LambdaSolved = @import("../lambda_solved/mod.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");
const Layouts = @import("layouts.zig");

const Allocator = std.mem.Allocator;

pub const Program = struct {
    allocator: Allocator,
    types: Type.Store,
    ast: Ast.Store,
    layouts: ?Layouts.Layouts = null,

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
        };
    }

    pub fn deinit(self: *Program) void {
        if (self.layouts) |*layouts| layouts.deinit();
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

    input.deinit();
    return program;
}

test "executable build owns final program state" {
    std.testing.refAllDecls(@This());
}
