const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const tokenize = parse.tokenize;
const AST2 = parse.AST2;
const Parser2 = parse.Parser2;

const Section = enum {
    META,
    SOURCE,
    TOKENS,
    PARSE_AST2,
};

const TestType = enum {
    file,
    expr,
};

const Snapshot = struct {
    description: []const u8,
    test_type: TestType,
    source: []const u8,
    tokens_output: []const u8,
    parse_output: []const u8,
    allocator: std.mem.Allocator,

    fn deinit(self: *Snapshot) void {
        self.allocator.free(self.description);
        self.allocator.free(self.source);
        self.allocator.free(self.tokens_output);
        self.allocator.free(self.parse_output);
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // If no arguments given, process all .md files in test/snapshot2
    if (args.len < 2) {
        const snapshot2_dir = "test/snapshot2";
        var dir = try std.fs.cwd().openDir(snapshot2_dir, .{ .iterate = true });
        defer dir.close();

        var walker = try dir.walk(allocator);
        defer walker.deinit();

        var count: usize = 0;
        while (try walker.next()) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.basename, ".md")) {
                const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ snapshot2_dir, entry.path });
                defer allocator.free(full_path);

                try processSnapshot(allocator, full_path);
                count += 1;
            }
        }
    } else {
        // Process specific file(s) given as arguments
        for (args[1..]) |snapshot_path| {
            try processSnapshot(allocator, snapshot_path);
        }
    }
}

fn processSnapshot(allocator: std.mem.Allocator, path: []const u8) !void {
    const file_content = try std.fs.cwd().readFileAlloc(allocator, path, 1024 * 1024);
    defer allocator.free(file_content);

    var snapshot = try parseSnapshot(allocator, file_content);
    defer snapshot.deinit();

    // Generate new outputs
    const tokens_output = try generateTokensOutput(allocator, snapshot.source);
    defer allocator.free(tokens_output);

    const parse_output = try generateParseOutput(allocator, snapshot.source, snapshot.test_type);
    defer allocator.free(parse_output);

    // Write updated snapshot
    try writeSnapshot(allocator, path, snapshot, tokens_output, parse_output);
}

fn parseSnapshot(allocator: std.mem.Allocator, content: []const u8) !Snapshot {
    var snapshot = Snapshot{
        .description = "",
        .test_type = .file,
        .source = "",
        .tokens_output = "",
        .parse_output = "",
        .allocator = allocator,
    };

    var current_section: ?Section = null;
    var section_content = std.ArrayList(u8).init(allocator);
    defer section_content.deinit();

    var lines = std.mem.tokenizeScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (std.mem.startsWith(u8, line, "# META")) {
            current_section = .META;
            section_content.clearRetainingCapacity();
        } else if (std.mem.startsWith(u8, line, "# SOURCE")) {
            if (current_section == .META) {
                try parseMeta(&snapshot, section_content.items);
            }
            current_section = .SOURCE;
            section_content.clearRetainingCapacity();
        } else if (std.mem.startsWith(u8, line, "# TOKENS")) {
            if (current_section == .SOURCE) {
                snapshot.source = try allocator.dupe(u8, std.mem.trim(u8, section_content.items, " \n"));
            }
            current_section = .TOKENS;
            section_content.clearRetainingCapacity();
        } else if (std.mem.startsWith(u8, line, "# PARSE_AST2")) {
            if (current_section == .TOKENS) {
                snapshot.tokens_output = try allocator.dupe(u8, section_content.items);
            }
            current_section = .PARSE_AST2;
            section_content.clearRetainingCapacity();
        } else if (current_section != null and !std.mem.startsWith(u8, line, "~~~")) {
            if (section_content.items.len > 0) {
                try section_content.append('\n');
            }
            try section_content.appendSlice(line);
        }
    }

    // Save last section
    if (current_section) |section| {
        switch (section) {
            .SOURCE => snapshot.source = try allocator.dupe(u8, std.mem.trim(u8, section_content.items, " \n")),
            .TOKENS => snapshot.tokens_output = try allocator.dupe(u8, section_content.items),
            .PARSE_AST2 => snapshot.parse_output = try allocator.dupe(u8, section_content.items),
            else => {},
        }
    }

    return snapshot;
}

fn parseMeta(snapshot: *Snapshot, content: []const u8) !void {
    var lines = std.mem.tokenizeScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (std.mem.indexOf(u8, line, "description=")) |idx| {
            snapshot.description = try snapshot.allocator.dupe(u8, line[idx + "description=".len ..]);
        } else if (std.mem.indexOf(u8, line, "type=")) |idx| {
            const type_str = line[idx + "type=".len ..];
            if (std.mem.eql(u8, type_str, "expr")) {
                snapshot.test_type = .expr;
            } else {
                snapshot.test_type = .file;
            }
        }
    }
}

fn getSimplifiedTagName(tag: AST2.Node.Tag) []const u8 {
    return switch (tag) {
        .binop_equals => "=",
        .binop_double_equals => "==",
        .binop_not_equals => "!=",
        .binop_plus => "+",
        .binop_minus => "-",
        .binop_star => "*",
        .binop_slash => "/",
        .binop_double_slash => "//",
        .binop_gt => ">",
        .binop_gte => ">=",
        .binop_lt => "<",
        .binop_lte => "<=",
        .binop_and => "and",
        .binop_or => "or",
        .binop_pipe => "|>",
        .binop_colon => ":",
        .binop_thick_arrow => "=>",
        .binop_thin_arrow => "->",
        .binop_double_question => "??",
        .num_literal_i32, .num_literal_big => "num",
        .int_literal_i32, .int_literal_big => "int",
        .frac_literal_small, .frac_literal_big => "frac",
        .str_literal_small, .str_literal_big => "str",
        .list_literal => "list",
        .underscore => "_",
        .import => "import",
        else => @tagName(tag),
    };
}

fn generateTokensOutput(allocator: std.mem.Allocator, source: []const u8) ![]const u8 {
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    var messages: [128]tokenize.Diagnostic = undefined;
    var byte_slices = base.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);
    var tokenizer = try tokenize.Tokenizer.init(&env, allocator, source, messages[0..], &byte_slices);
    try tokenizer.tokenize(allocator);
    var result = tokenizer.finishAndDeinit(allocator);
    defer result.tokens.deinit(allocator);

    var output = std.ArrayList(u8).init(allocator);
    errdefer output.deinit();

    // Generate token output
    const token_count = result.tokens.tokens.len;
    for (0..token_count) |i| {
        if (i > 0) try output.appendSlice(",");

        const tag = result.tokens.tokens.items(.tag)[i];
        const region = result.tokens.tokens.items(.region)[i];

        try output.writer().print("{s}({d}-{d})", .{
            @tagName(tag),
            region.start.offset,
            region.end.offset,
        });
    }

    return output.toOwnedSlice();
}

fn generateParseOutput(allocator: std.mem.Allocator, source: []const u8, test_type: TestType) ![]const u8 {
    var ast = try AST2.initCapacity(allocator, 100);
    defer ast.deinit(allocator);

    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    // Tokenize
    var messages: [128]tokenize.Diagnostic = undefined;
    var byte_slices = base.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);
    var tokenizer = try tokenize.Tokenizer.init(&env, allocator, source, messages[0..], &byte_slices);
    try tokenizer.tokenize(allocator);
    var result = tokenizer.finishAndDeinit(allocator);
    defer result.tokens.deinit(allocator);

    // Parse
    var parser = try Parser2.init(result.tokens, allocator, &ast, &byte_slices);
    defer {
        // Clean up parser diagnostics
        parser.diagnostics.deinit(allocator);
        parser.deinit();
    }

    var expr_root: ?AST2.Node.Idx = null;
    if (test_type == .file) {
        _ = try parser.parseFile();
    } else {
        expr_root = try parser.parseExpr();
    }

    // Generate S-expression output
    var output = std.ArrayList(u8).init(allocator);
    errdefer output.deinit();

    if (test_type == .file) {
        try output.appendSlice("(file\n");

        // Output header if present
        if (ast.header) |header| {
            try writeHeader(&output, &ast, &env, header, 1);
        }

        // Check if there's a block node (contains the statements)
        // The block should be the last node if there are any statements
        if (ast.nodes.len() > 0) {
            const last_idx = @as(AST2.Node.Idx, @enumFromInt(ast.nodes.len() - 1));
            if (ast.tag(last_idx) == .block) {
                // Output the statements
                // Output the statements directly without wrapper
                var iter = ast.node_slices.nodes(ast.payload(last_idx).nodes);
                while (iter.next()) |stmt_idx| {
                    try writeNode(&output, &ast, &env, stmt_idx, 1);
                }
            }
        }

        try output.appendSlice(")\n");
    } else {
        // For expressions, output the root node returned by parseExpr
        if (expr_root) |root_idx| {
            try writeNodeInline(&output, &ast, &env, root_idx);
            try output.appendSlice("\n");
        }
    }

    return output.toOwnedSlice();
}

fn writeHeader(output: *std.ArrayList(u8), ast: *const AST2, env: *const base.CommonEnv, header: AST2.Header, indent_level: usize) !void {
    const indent = "  " ** 10;
    const indent_str = indent[0 .. indent_level * 2];

    switch (header) {
        .app => |app| {
            try output.writer().print("{s}(app\n", .{indent_str});
            try output.writer().print("{s}  (provides ", .{indent_str});
            try writeNodeSlice(output, ast, env, app.provides, indent_level + 1);
            try output.appendSlice(")\n");
            try output.writer().print("{s}  (platform ", .{indent_str});
            try writeNodeInline(output, ast, env, app.platform_idx);
            try output.appendSlice(")\n");
            try output.writer().print("{s}  (packages ", .{indent_str});
            try writeNodeSlice(output, ast, env, app.packages, indent_level + 1);
            try output.appendSlice(")\n");
            try output.writer().print("{s})\n", .{indent_str});
        },
        .module => |mod| {
            try output.writer().print("{s}(module\n", .{indent_str});
            try output.writer().print("{s}  (exposes", .{indent_str});
            // Check if there are any exposes
            var iter = ast.node_slices.nodes(mod.exposes);
            if (iter.next() != null) {
                // Reset the iterator since we consumed one
                iter = ast.node_slices.nodes(mod.exposes);
                try output.appendSlice(" ");
                try writeNodeSlice(output, ast, env, mod.exposes, indent_level + 1);
            }
            try output.appendSlice(")\n");
            try output.writer().print("{s})\n", .{indent_str});
        },
        .package => |pkg| {
            try output.writer().print("{s}(package\n", .{indent_str});
            try output.writer().print("{s}  (exposes ", .{indent_str});
            try writeNodeSlice(output, ast, env, pkg.exposes, indent_level + 1);
            try output.appendSlice(")\n");
            try output.writer().print("{s}  (packages ", .{indent_str});
            try writeNodeSlice(output, ast, env, pkg.packages, indent_level + 1);
            try output.appendSlice(")\n");
            try output.writer().print("{s})\n", .{indent_str});
        },
        .platform => |plat| {
            try output.writer().print("{s}(platform\n", .{indent_str});
            try output.writer().print("{s}  (name ", .{indent_str});
            try writeNodeInline(output, ast, env, plat.name);
            try output.appendSlice(")\n");
            try output.writer().print("{s}  (requires-rigids ", .{indent_str});
            try writeNodeSlice(output, ast, env, plat.requires_rigids, indent_level + 1);
            try output.appendSlice(")\n");
            try output.writer().print("{s}  (requires-signatures ", .{indent_str});
            try writeNodeInline(output, ast, env, plat.requires_signatures);
            try output.appendSlice(")\n");
            try output.writer().print("{s}  (exposes ", .{indent_str});
            try writeNodeSlice(output, ast, env, plat.exposes, indent_level + 1);
            try output.appendSlice(")\n");
            try output.writer().print("{s}  (packages ", .{indent_str});
            try writeNodeSlice(output, ast, env, plat.packages, indent_level + 1);
            try output.appendSlice(")\n");
            try output.writer().print("{s}  (provides ", .{indent_str});
            try writeNodeSlice(output, ast, env, plat.provides, indent_level + 1);
            try output.appendSlice(")\n");
            try output.writer().print("{s})\n", .{indent_str});
        },
        .hosted => |host| {
            try output.writer().print("{s}(hosted\n", .{indent_str});
            try output.writer().print("{s}  (exposes ", .{indent_str});
            try writeNodeSlice(output, ast, env, host.exposes, indent_level + 1);
            try output.appendSlice(")\n");
            try output.writer().print("{s})\n", .{indent_str});
        },
        .interface => |iface| {
            _ = iface;
            try output.writer().print("{s}(interface-header)\n", .{indent_str});
        },
        .malformed => |mal| {
            _ = mal;
            try output.writer().print("{s}(malformed-header)\n", .{indent_str});
        },
    }
}

fn writeNodeSlice(output: *std.ArrayList(u8), ast: *const AST2, env: *const base.CommonEnv, slice_idx: AST2.NodeSlices.Idx, _: usize) !void {
    var iter = ast.node_slices.nodes(slice_idx);
    var i: usize = 0;
    while (iter.next()) |node_idx| {
        if (i > 0) try output.appendSlice(", ");
        try writeNodeInline(output, ast, env, node_idx);
        i += 1;
    }
}

fn writeNodeInline(output: *std.ArrayList(u8), ast: *const AST2, env: *const base.CommonEnv, idx: AST2.Node.Idx) !void {
    // For inline nodes, we write them compactly without indentation
    // but still with full structure
    const tag = ast.tag(idx);
    const start_pos = ast.start(idx);

    // Use simplified tag names for better readability
    const simplified_name = getSimplifiedTagName(tag);
    try output.writer().print("({s}", .{simplified_name});

    // Write node-specific content based on tag
    switch (tag) {
        .uc, .lc, .lc_dot_ucs, .uc_dot_ucs, .var_lc, .neg_lc, .not_lc, .dot_lc, .double_dot_lc => {
            const ident_idx = ast.payload(idx).ident;
            const ident_text = env.getIdent(ident_idx);
            try output.writer().print(" \"{s}\"", .{ident_text});
        },
        .num_literal_i32 => {
            const value = ast.payload(idx).num_literal_i32;
            try output.writer().print(" {d}", .{value});
        },
        .str_literal_small => {
            const bytes = ast.payload(idx).str_literal_small;
            try output.writer().print(" \"", .{});
            for (bytes) |b| {
                if (b == 0) break;
                try output.append(b);
            }
            try output.appendSlice("\"");
        },
        .str_literal_big => {
            const bytes_idx = ast.payload(idx).str_literal_big;
            const string_bytes = ast.byte_slices.slice(bytes_idx);
            try output.writer().print(" \"", .{});
            for (string_bytes) |b| {
                try output.append(b);
            }
            try output.writer().print("\"", .{});
        },
        .list_literal => {
            // List literal stores the count, not a slice index
            // The elements are the preceding nodes in the AST
            const elem_count = ast.payload(idx).list_elems;
            const list_idx_num = @intFromEnum(idx);

            // The elements are the nodes before this list_literal node
            const start_idx = list_idx_num - @as(i32, @intCast(elem_count));

            for (0..elem_count) |i| {
                try output.appendSlice(" (");

                const elem_idx = @as(AST2.Node.Idx, @enumFromInt(start_idx + @as(i32, @intCast(i))));
                const node_tag = ast.tag(elem_idx);
                const node_pos = ast.start(elem_idx);

                // Write simplified node type and value inline
                switch (node_tag) {
                    .num_literal_i32 => {
                        const value = ast.payload(elem_idx).num_literal_i32;
                        try output.writer().print("num {d} @{d}", .{ value, node_pos.offset });
                    },
                    else => {
                        // For other types, use the full inline representation
                        try writeNodeInline(output, ast, env, elem_idx);
                    },
                }
                try output.appendSlice(")");
            }
        },
        .tuple_literal, .record_literal, .block => {
            const nodes_idx = ast.payload(idx).nodes;
            var iter = ast.node_slices.nodes(nodes_idx);
            try output.appendSlice(" (");
            var i: usize = 0;
            while (iter.next()) |node_idx| {
                if (i > 0) try output.appendSlice(", ");
                try writeNodeInline(output, ast, env, node_idx);
                i += 1;
            }
            try output.appendSlice(")");
        },
        // TODO: .apply tag is missing from AST2.Node.Tag enum - needs to be rewritten for iterator
        // .apply => {
        //     const nodes_idx = ast.payload(idx).block_nodes;
        //     var iter = ast.node_slices.nodes(nodes_idx);
        //     // TODO: Rewrite to use iterator
        //     @panic("TODO: .apply handling needs to be rewritten for iterator");
        // },
        .apply_lc, .apply_uc, .apply_anon => {
            // TODO: apply cases need implementation for iterator
            @panic("TODO: apply cases need to be implemented with iterator");
        },
        .underscore => {
            // Underscore has no additional content
        },
        .import => {
            const nodes_idx = ast.payload(idx).import_nodes;
            var iter = ast.node_slices.nodes(nodes_idx);
            var i: usize = 0;
            while (iter.next()) |node_idx| {
                try output.appendSlice(" ");
                if (i > 0) try output.appendSlice(" ");
                try writeNodeInline(output, ast, env, node_idx);
                i += 1;
            }
        },
        .binop_equals, .binop_double_equals, .binop_not_equals, .binop_colon, .binop_colon_equals, .binop_dot, .binop_as, .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_double_slash, .binop_double_question, .binop_gt, .binop_gte, .binop_lt, .binop_lte, .binop_thick_arrow, .binop_thin_arrow, .binop_and, .binop_or, .binop_pipe => {
            const binop = ast.binOp(idx);
            try output.appendSlice(" ");
            try writeNodeInline(output, ast, env, binop.lhs);
            try output.appendSlice(" ");
            try writeNodeInline(output, ast, env, binop.rhs);
        },
        .lambda => {
            const lambda = ast.lambda(idx);
            try output.appendSlice(" [");
            var args_iter = ast.lambdaArgs(lambda);
            var i: usize = 0;
            while (args_iter.next()) |arg_idx| {
                if (i > 0) try output.appendSlice(", ");
                try writeNodeInline(output, ast, env, arg_idx);
                i += 1;
            }
            try output.appendSlice("] ");
            try writeNodeInline(output, ast, env, lambda.body);
        },
        .if_else, .if_without_else => {
            const branches = ast.payload(idx).if_branches;
            const branches_idx = @as(AST2.NodeSlices.Idx, @enumFromInt(branches));
            var iter = ast.node_slices.nodes(branches_idx);
            // if-else has pairs of (condition, body), potentially with final else
            var i: usize = 0;
            while (iter.next()) |node_idx| {
                if (i > 0) try output.appendSlice(" ");
                try writeNodeInline(output, ast, env, node_idx);
                i += 1;
            }
        },
        .match => {
            const branches = ast.payload(idx).match_branches;
            const branches_idx = @as(AST2.NodeSlices.Idx, @enumFromInt(branches));
            // TODO: match needs to be rewritten for iterator
            _ = branches_idx;
            @panic("TODO: match handling needs to be rewritten for iterator");
        },
        .unary_not, .unary_neg, .unary_double_dot, .ret, .crash => {
            // These operators store their operand as the preceding node
            const child_idx = @as(AST2.Node.Idx, @enumFromInt(@intFromEnum(idx) - 1));
            try output.appendSlice(" ");
            try writeNodeInline(output, ast, env, child_idx);
        },
        .for_loop => {
            const nodes_idx = ast.payload(idx).nodes;
            var iter = ast.node_slices.nodes(nodes_idx);

            // for x in y { ... }
            if (iter.next()) |pattern| {
                try output.appendSlice(" ");
                try writeNodeInline(output, ast, env, pattern);

                if (iter.next()) |iterable| {
                    try output.appendSlice(" ");
                    try writeNodeInline(output, ast, env, iterable);

                    // Check if there are body nodes
                    if (iter.next()) |first_body| {
                        try output.appendSlice(" [");
                        try writeNodeInline(output, ast, env, first_body);

                        // Write remaining body nodes
                        while (iter.next()) |body_node| {
                            try output.appendSlice(", ");
                            try writeNodeInline(output, ast, env, body_node);
                        }
                        try output.appendSlice("]");
                    }
                }
            }
        },
        .while_loop => {
            const nodes_idx = ast.payload(idx).nodes;
            var iter = ast.node_slices.nodes(nodes_idx);

            // while x { ... }
            if (iter.next()) |condition| {
                try output.appendSlice(" ");
                try writeNodeInline(output, ast, env, condition);

                // Check if there are body nodes
                if (iter.next()) |first_body| {
                    try output.appendSlice(" [");
                    try writeNodeInline(output, ast, env, first_body);

                    // Write remaining body nodes
                    while (iter.next()) |body_node| {
                        try output.appendSlice(", ");
                        try writeNodeInline(output, ast, env, body_node);
                    }
                    try output.appendSlice("]");
                }
            }
        },
        // Note: AST2 doesn't have separate empty_record, empty_tuple, empty_list tags
        // Empty collections are just list_literal/record_literal/tuple_literal with no nodes
        .dot_num => {
            const value = ast.payload(idx).num_literal_i32;
            try output.writer().print(" .{d}", .{value});
        },
        .num_literal_big, .int_literal_big, .frac_literal_big => {
            try output.appendSlice(" <big-num>");
        },
        .int_literal_i32 => {
            const value = ast.payload(idx).num_literal_i32;
            try output.writer().print(" 0x{x}", .{value});
        },
        .frac_literal_small => {
            const value = ast.payload(idx).num_literal_i32;
            try output.writer().print(" {d}", .{value});
        },
        .str_interpolation => {
            const nodes_idx = ast.payload(idx).nodes;
            var iter = ast.node_slices.nodes(nodes_idx);
            try output.appendSlice(" [");
            var i: usize = 0;
            while (iter.next()) |node_idx| {
                if (i > 0) try output.appendSlice(", ");
                try writeNodeInline(output, ast, env, node_idx);
                i += 1;
            }
            try output.appendSlice("]");
        },
        .lambda_no_args => {
            const body_idx = ast.payload(idx).nodes;
            var nodes_iter = ast.node_slices.nodes(body_idx);
            if (nodes_iter.next()) |body| {
                try output.appendSlice(" || ");
                try writeNodeInline(output, ast, env, body);
            }
        },
        .malformed => {
            const diag = ast.payload(idx).malformed;
            try output.writer().print(" error:{s}", .{@tagName(diag)});
        },
    }

    try output.writer().print(" @{d})", .{start_pos.offset});
}

fn writeNode(output: *std.ArrayList(u8), ast: *const AST2, env: *const base.CommonEnv, idx: AST2.Node.Idx, indent_level: usize) !void {
    const indent = "  " ** 10;
    const indent_str = indent[0 .. indent_level * 2];

    const tag = ast.tag(idx);
    const start_pos = ast.start(idx);
    const simplified_name = getSimplifiedTagName(tag);

    try output.writer().print("{s}({s}", .{
        indent_str,
        simplified_name,
    });

    // Write node-specific content based on tag
    switch (tag) {
        .uc, .lc, .lc_dot_ucs, .uc_dot_ucs, .var_lc, .neg_lc, .not_lc, .dot_lc, .double_dot_lc => {
            const ident_idx = ast.payload(idx).ident;
            const ident_text = env.getIdent(ident_idx);
            try output.writer().print(" \"{s}\" @{d})\n", .{ ident_text, start_pos.offset });
        },
        .num_literal_i32 => {
            const value = ast.payload(idx).num_literal_i32;
            try output.writer().print(" {d} @{d})\n", .{ value, start_pos.offset });
        },
        .str_literal_small => {
            try output.writer().print(" \"", .{});
            const bytes = ast.payload(idx).str_literal_small;
            for (bytes) |b| {
                if (b == 0) break;
                try output.append(b);
            }
            try output.writer().print("\" @{d})\n", .{start_pos.offset});
        },
        .str_literal_big => {
            // Now that ByteSlices is fixed, we can display the actual content
            const bytes_idx = ast.payload(idx).str_literal_big;
            const string_bytes = ast.byte_slices.slice(bytes_idx);
            try output.writer().print(" \"", .{});
            for (string_bytes) |b| {
                try output.append(b);
            }
            try output.writer().print("\" @{d})\n", .{start_pos.offset});
        },
        .list_literal => {
            const nodes_idx = ast.payload(idx).nodes;
            var iter = ast.node_slices.nodes(nodes_idx);
            try output.appendSlice(" @");
            try output.writer().print("{d}\n", .{start_pos.offset});
            try output.writer().print("{s}  [", .{indent_str});
            var i: usize = 0;
            while (iter.next()) |node_idx| {
                if (i > 0) try output.appendSlice(", ");
                try writeNodeInline(output, ast, env, node_idx);
                i += 1;
            }
            try output.appendSlice("]\n");
        },
        .tuple_literal, .record_literal, .block => {
            const nodes_idx = ast.payload(idx).nodes;
            var iter = ast.node_slices.nodes(nodes_idx);
            try output.appendSlice(" @");
            try output.writer().print("{d}\n", .{start_pos.offset});
            try output.writer().print("{s}  (", .{indent_str});
            var i: usize = 0;
            while (iter.next()) |node_idx| {
                if (i > 0) try output.appendSlice(", ");
                try writeNodeInline(output, ast, env, node_idx);
                i += 1;
            }
            try output.appendSlice(")\n");
        },
        .lambda => {
            const lambda = ast.lambda(idx);
            try output.appendSlice(" @");
            try output.writer().print("{d}\n", .{start_pos.offset});
            try output.writer().print("{s}  args: [", .{indent_str});
            var args_iter = ast.lambdaArgs(lambda);
            var i: usize = 0;
            while (args_iter.next()) |arg_idx| {
                if (i > 0) try output.appendSlice(", ");
                try writeNodeInline(output, ast, env, arg_idx);
                i += 1;
            }
            try output.appendSlice("]\n");
            try output.writer().print("{s}  body: ", .{indent_str});
            try writeNodeInline(output, ast, env, lambda.body);
            try output.appendSlice("\n");
        },
        .apply_lc, .apply_uc, .apply_anon => {
            const nodes_idx = ast.payload(idx).nodes;
            var iter = ast.node_slices.nodes(nodes_idx);
            try output.appendSlice(" @");
            try output.writer().print("{d}\n", .{start_pos.offset});
            if (iter.next()) |func| {
                try output.writer().print("{s}  func: ", .{indent_str});
                try writeNodeInline(output, ast, env, func);
                try output.appendSlice("\n");

                // Check if there are args
                if (iter.next()) |first_arg| {
                    try output.writer().print("{s}  args: [", .{indent_str});
                    try writeNodeInline(output, ast, env, first_arg);

                    // Write remaining args
                    while (iter.next()) |arg_idx| {
                        try output.appendSlice(", ");
                        try writeNodeInline(output, ast, env, arg_idx);
                    }
                    try output.appendSlice("]\n");
                }
            }
        },
        .if_else, .if_without_else => {
            const branches = ast.payload(idx).if_branches;
            const branches_idx = @as(AST2.NodeSlices.Idx, @enumFromInt(branches));
            var iter = ast.node_slices.nodes(branches_idx);
            try output.appendSlice(" @");
            try output.writer().print("{d}\n", .{start_pos.offset});
            try output.writer().print("{s}  branches: [", .{indent_str});
            var i: usize = 0;
            while (iter.next()) |node_idx| {
                if (i > 0) try output.appendSlice(", ");
                try writeNodeInline(output, ast, env, node_idx);
                i += 1;
            }
            try output.appendSlice("]\n");
        },
        .match => {
            const branches = ast.payload(idx).match_branches;
            const branches_idx = @as(AST2.NodeSlices.Idx, @enumFromInt(branches));
            var iter = ast.node_slices.nodes(branches_idx);
            try output.appendSlice(" @");
            try output.writer().print("{d}\n", .{start_pos.offset});
            if (iter.next()) |cond| {
                try output.writer().print("{s}  cond: ", .{indent_str});
                try writeNodeInline(output, ast, env, cond);
                try output.appendSlice("\n");

                // Check if there are branches
                if (iter.next()) |first_branch| {
                    try output.writer().print("{s}  branches: [", .{indent_str});
                    try writeNodeInline(output, ast, env, first_branch);

                    // Write remaining branches
                    while (iter.next()) |branch_idx| {
                        try output.appendSlice(", ");
                        try writeNodeInline(output, ast, env, branch_idx);
                    }
                    try output.appendSlice("]\n");
                }
            }
        },
        .underscore => {
            try output.writer().print(" @{d})\n", .{start_pos.offset});
        },
        .import => {
            const nodes_idx = ast.payload(idx).import_nodes;
            var iter = ast.node_slices.nodes(nodes_idx);
            while (iter.next()) |node_idx| {
                try output.appendSlice(" ");
                try writeNodeInline(output, ast, env, node_idx);
            }
            try output.writer().print(" @{d})\n", .{start_pos.offset});
        },
        .unary_not, .unary_neg, .unary_double_dot, .ret, .crash => {
            // These operators store their operand as the preceding node
            const child_idx = @as(AST2.Node.Idx, @enumFromInt(@intFromEnum(idx) - 1));
            try output.appendSlice(" ");
            try writeNodeInline(output, ast, env, child_idx);
            try output.writer().print(" @{d})\n", .{start_pos.offset});
        },
        .lambda_no_args => {
            const body_idx = ast.payload(idx).nodes;
            var nodes_iter = ast.node_slices.nodes(body_idx);
            if (nodes_iter.next()) |body| {
                try output.appendSlice(" || ");
                try writeNodeInline(output, ast, env, body);
            }
            try output.writer().print(" @{d})\n", .{start_pos.offset});
        },
        .malformed => {
            const diag = ast.payload(idx).malformed;
            try output.writer().print(" error:{s} @{d})\n", .{ @tagName(diag), start_pos.offset });
        },
        else => {
            // Check if it's a binary operator by trying to get binOp
            if (@hasField(AST2.Node.Payload, "binop")) {
                switch (tag) {
                    .binop_equals, .binop_double_equals, .binop_not_equals, .binop_colon, .binop_colon_equals, .binop_dot, .binop_as, .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_double_slash, .binop_double_question, .binop_gt, .binop_gte, .binop_lt, .binop_lte, .binop_thick_arrow, .binop_thin_arrow, .binop_and, .binop_or, .binop_pipe => {
                        const binop = ast.binOp(idx);
                        try output.appendSlice(" ");
                        try writeNodeInline(output, ast, env, binop.lhs);
                        try output.appendSlice(" ");
                        try writeNodeInline(output, ast, env, binop.rhs);
                        try output.writer().print(" @{d})\n", .{start_pos.offset});
                    },
                    else => {
                        try output.writer().print(" unhandled @{d})\n", .{start_pos.offset});
                    },
                }
            } else {
                try output.writer().print(" unhandled @{d})\n", .{start_pos.offset});
            }
        },
    }
}

fn writeSnapshot(allocator: std.mem.Allocator, path: []const u8, snapshot: Snapshot, tokens: []const u8, parse_output: []const u8) !void {
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    // Write META section
    try output.appendSlice("# META\n~~~ini\n");
    try output.writer().print("description={s}\n", .{snapshot.description});
    try output.writer().print("type={s}\n", .{@tagName(snapshot.test_type)});
    try output.appendSlice("~~~\n\n");

    // Write SOURCE section
    try output.appendSlice("# SOURCE\n~~~roc\n");
    try output.appendSlice(snapshot.source);
    try output.appendSlice("\n~~~\n\n");

    // Write TOKENS section
    try output.appendSlice("# TOKENS\n~~~zig\n");
    try output.appendSlice(tokens);
    try output.appendSlice("\n~~~\n\n");

    // Write PARSE_AST2 section
    try output.appendSlice("# PARSE_AST2\n~~~clojure\n");
    try output.appendSlice(parse_output);
    try output.appendSlice("\n~~~\n");

    // Write to file
    try std.fs.cwd().writeFile(.{ .sub_path = path, .data = output.items });
}
