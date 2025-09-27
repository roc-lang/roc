const std = @import("std");
const Random = std.Random;
const Prng = Random.DefaultPrng;
const Allocator = std.mem.Allocator;
const StringHashMap = std.hash_map.StringHashMap;

const Type = union(enum) {
    str: void,
    int: void,
    func: Type.Function,

    const Function = struct {
        args: []Type,
        ret: *Type,
    };

    pub fn eql(self: Type, other: Type) bool {
        if (@as(std.meta.Tag(Type), self) != @as(std.meta.Tag(Type), other)) {
            return false;
        }

        switch (self) {
            .str, .int => return true,
            .func => |self_func| {
                const other_func = other.func;
                if (self_func.args.len != other_func.args.len) {
                    return false;
                }
                for (0..self_func.args.len) |i| {
                    if (!self_func.args[i].eql(other_func.args[i])) {
                        return false;
                    }
                }
                return self_func.ret.eql(other_func.ret.*);
            },
        }
    }
};

const Lookup = struct {
    name: []const u8,
    type: Type,
};

const NamedFnCall = struct {
    name: []const u8,
    args: []Node,
};

// const Lambda = struct {
//     /// key is the arg name, type is the type of that arg
//     scope: StringHashMap(Type),
//     body: Node,
// };

const Node = union(enum) {
    str_literal: []const u8,
    int_literal: i128,
    // lambda: Lambda,
    call_named_fn: NamedFnCall,
    lookup: []const u8,

    pub fn getType(self: Node, scope: StringHashMap(Node)) Type {
        return switch (self) {
            .str_literal => Type.str,
            .int_literal => Type.int,
            .call_named_fn => |call| {
                const node = scope.get(call.name) orelse @panic("Lookup was not found");

                switch (node.getType(scope)) {
                    .func => |func| {
                        std.debug.assert(func.args.len == call.args.len);

                        for (0..func.args.len) |idx| {
                            // TODO give a nicer error
                            std.debug.assert(func.args[idx].eql(call.args[idx].getType(scope)));
                        }

                        return func.ret.*;
                    },
                    else => @panic("Tried to call a non-function"),
                }
            },
            .lookup => |name| {
                const node = scope.get(name) orelse @panic("Lookup was not found");
                return node.getType(scope);
            },
        };
    }

    pub fn render(self: Node, _: Allocator, writer: std.ArrayList(u8).Writer) Allocator.Error!void {
        switch (self) {
            .str_literal => |bytes| {
                _ = try writer.print("\"{s}\"", .{bytes});
            },
            .int_literal => |int| {
                _ = try writer.print("{}", .{int});
            },
            // .lambda => |lambda| {
            //     _ = try writer.write("|");

            //     var iter = lambda.scope.keyIterator();

            //     while (iter.next()) |arg_name| {
            //         _ = try writer.print("{}, ", .{arg_name});
            //     }

            //     _ = try writer.write("| ");

            //     try lambda.body.render(gpa, writer);
            // },
            .call_named_fn => |call| {
                _ = try writer.print("{s}(", .{call.name});
                for (call.args) |arg| {
                    _ = try writer.print(", {}", .{arg});
                }
                _ = try writer.write(")");
            },
            .lookup => |name| {
                _ = try writer.print("{s}", .{name});
            },
        }
    }

    pub fn random(
        gpa: Allocator,
        rand: *Random,
        nodes: *std.ArrayList(Node),
        scope: StringHashMap(Node),
        sub_scopes: *std.ArrayList(Scope),
    ) Allocator.Error!Node {
        switch (rand.intRangeAtMost(u8, 0, @typeInfo(Node).@"union".fields.len - 1)) {
            0 => return .{ .str_literal = try randomString(rand, gpa, 10) },
            1 => return .{ .int_literal = rand.int(i128) },
            // 2 => return .{ .lambda = blk: {
            //     const sub_scope = Scope.init(gpa);
            //     _ = try sub_scopes.append(sub_scope);
            //     defer {
            //         sub_scope.pop();
            //         sub_scope.deinit();
            //     }

            //     const arg = try randomString(&rand, gpa, rand.intRangeAtMost(u8, 1, 6));

            //     break :blk Lambda{
            //         .args =
            //         .body = .{.lookup = arg }
            //     };
            // } },
            2 => return .{
                .call_named_fn = blk: {
                    if (scope.count() > 0) {
                        // Capture the current nodes count before adding new nodes
                        const top_nodes = nodes.items.len;

                        // Get a random ident in the scope
                        const ident = getRandomInScope(scope, rand);
                        const ident_type = scope.get(ident).?.getType(scope);

                        // Make sure the random ident is a function
                        if (ident_type == .func) {
                            const call_fn = ident_type.func;

                            // Randomly generate type compatible args
                            var cur_param: usize = 0;
                            while (cur_param < call_fn.args.len) {
                                const cur_param_type = call_fn.args[cur_param];
                                const rand_param = try Node.random(gpa, rand, nodes, scope, sub_scopes);
                                if (rand_param.getType(scope).eql(cur_param_type)) {
                                    try nodes.append(rand_param);
                                    cur_param += 1;
                                } else {
                                    // if this random param wasn't the right type, continue looping
                                }
                            }

                            // Return the generated func node
                            break :blk .{
                                .name = ident,
                                .args = nodes.items[top_nodes..],
                            };
                        } else {
                            // Could make this guaranteed to succeed but re-rolling should be fine in practice.
                            return random(gpa, rand, nodes, scope, sub_scopes);
                        }
                    } else {
                        // Could make this guaranteed to succeed but re-rolling should be fine in practice.
                        return random(gpa, rand, nodes, scope, sub_scopes);
                    }
                },
            },
            3 => return .{
                .lookup = blk: {
                    if (scope.count() > 0) {
                        break :blk getRandomInScope(scope, rand);
                    } else {
                        // Could make this guaranteed to succeed but re-rolling should be fine in practice.
                        return random(gpa, rand, nodes, scope, sub_scopes);
                    }
                },
            },
            else => unreachable,
        }
    }
};

const Scope = StringHashMap(Node);

pub fn getRandomInScope(scope: Scope, rand: *Random) []const u8 {
    // Randomly choose a val from the scope
    var count = rand.int(u8) % @as(u8, @intCast(scope.count()));

    // Iter until we find it
    var iter = scope.keyIterator();
    while (iter.next()) |key| {
        if (count == 0) {
            return key.*;
        }
        count -= 1;
    }

    // Since the count we generated is based on the size of the scope, it should be impossible that we didn't find a node.
    unreachable;
}

pub fn genScope(gpa: Allocator, seed: u64, nodes: *std.ArrayList(Node), sub_scopes: *std.ArrayList(Scope)) Allocator.Error!Scope {
    var top_level = StringHashMap(Node).init(gpa);

    var prng = Prng.init(seed);

    // Randomly generate a number of nodes to add to scope
    var rand = prng.random();
    var nodes_to_add = rand.intRangeAtMost(u8, 1, 32);

    while (nodes_to_add > 0) {
        nodes_to_add -= 1;

        // Generate a randome node
        const node = try Node.random(gpa, &rand, nodes, top_level, sub_scopes);
        try nodes.append(node);

        // Add to the top-level scope
        const name = try randomString(&rand, gpa, rand.intRangeAtMost(u8, 1, 6));
        try top_level.put(name, node);
    }

    return top_level;
}

fn randomString(rand: *Random, gpa: Allocator, len: usize) Allocator.Error![]const u8 {
    const str = try gpa.alloc(u8, len);
    for (str) |*c| {
        c.* = rand.intRangeAtMost(u8, 'a', 'z');
    }
    return str;
}

pub fn renderScope(gpa: Allocator, scope: Scope, writer: anytype) !void {
    var iter = scope.iterator();
    while (iter.next()) |entry| {
        try renderAssignment(gpa, entry.key_ptr.*, entry.value_ptr.*, writer);
        _ = try writer.write("\n");
    }
}

fn renderAssignment(
    gpa: Allocator,
    key: []const u8,
    node: Node,
    writer: std.ArrayList(u8).Writer,
) Allocator.Error!void {
    _ = try writer.write(key);
    _ = try writer.write(" = ");
    try node.render(gpa, writer);
}

pub fn generateProgram(gpa: std.mem.Allocator, seed: u64, writer: std.ArrayList(u8).Writer) !void {
    var sub_scopes = try std.ArrayList(Scope).initCapacity(gpa, 12);
    defer sub_scopes.deinit();

    var nodes = std.ArrayList(Node).init(gpa);
    defer nodes.deinit();

    const top_level = try genScope(gpa, seed, &nodes, &sub_scopes);
    try renderScope(gpa, top_level, writer);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer _ = arena.deinit();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const seed = if (args.len < 2) blk: {
        std.debug.print("Usage: {s} <seed>\n", .{args[0]});
        std.debug.print("Using default seed: 12345\n", .{});
        break :blk @as(u64, 12345);
    } else try std.fmt.parseInt(u64, args[1], 10);

    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();
    const writer = buf.writer();

    try generateProgram(allocator, seed, writer);

    std.debug.print("{s}\n", .{buf.items});
}
