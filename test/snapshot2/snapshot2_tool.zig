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
                const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{snapshot2_dir, entry.path});
                defer allocator.free(full_path);
                
                std.debug.print("Processing {s}...\n", .{full_path});
                try processSnapshot(allocator, full_path);
                count += 1;
            }
        }
        
        if (count == 0) {
            std.debug.print("No .md files found in {s}\n", .{snapshot2_dir});
        } else {
            std.debug.print("Processed {d} snapshot files.\n", .{count});
        }
    } else {
        // Process specific file(s) given as arguments
        for (args[1..]) |snapshot_path| {
            std.debug.print("Processing {s}...\n", .{snapshot_path});
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

fn generateTokensOutput(allocator: std.mem.Allocator, source: []const u8) ![]const u8 {
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    var messages: [128]tokenize.Diagnostic = undefined;
    var tokenizer = try tokenize.Tokenizer.init(&env, allocator, source, messages[0..]);
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
    var tokenizer = try tokenize.Tokenizer.init(&env, allocator, source, messages[0..]);
    try tokenizer.tokenize(allocator);
    var result = tokenizer.finishAndDeinit(allocator);
    defer result.tokens.deinit(allocator);

    // Parse
    var parser = try Parser2.init(result.tokens, allocator, &ast);
    defer parser.deinit();
    
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
                try output.appendSlice("  (statements\n");
                const block_nodes = ast.node_slices.slice(ast.payload(last_idx).block_nodes);
                std.debug.print("Block has {d} statements\n", .{block_nodes.len});
                std.debug.print("Total nodes in AST: {d}\n", .{ast.nodes.len()});
                for (0..ast.nodes.len()) |i| {
                    const node_idx = @as(AST2.Node.Idx, @enumFromInt(i));
                    std.debug.print("  Node {d}: {s} @{d}\n", .{i, @tagName(ast.tag(node_idx)), ast.start(node_idx).offset});
                }
                for (block_nodes, 0..) |stmt_idx, i| {
                    std.debug.print("  Statement {d}: idx={d}, tag={s}\n", .{i, @intFromEnum(stmt_idx), @tagName(ast.tag(stmt_idx))});
                    try writeNode(&output, &ast, &env, stmt_idx, 2);
                }
                try output.appendSlice("  )\n");
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
    const indent_str = indent[0..indent_level * 2];
    
    switch (header) {
        .app => |app| {
            try output.writer().print("{s}(app-header\n", .{indent_str});
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
            try output.writer().print("{s}(module-header\n", .{indent_str});
            try output.writer().print("{s}  (exposes ", .{indent_str});
            try writeNodeSlice(output, ast, env, mod.exposes, indent_level + 1);
            try output.appendSlice(")\n");
            try output.writer().print("{s})\n", .{indent_str});
        },
        .package => |pkg| {
            try output.writer().print("{s}(package-header\n", .{indent_str});
            try output.writer().print("{s}  (exposes ", .{indent_str});
            try writeNodeSlice(output, ast, env, pkg.exposes, indent_level + 1);
            try output.appendSlice(")\n");
            try output.writer().print("{s}  (packages ", .{indent_str});
            try writeNodeSlice(output, ast, env, pkg.packages, indent_level + 1);
            try output.appendSlice(")\n");
            try output.writer().print("{s})\n", .{indent_str});
        },
        .platform => |plat| {
            try output.writer().print("{s}(platform-header\n", .{indent_str});
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
            try output.writer().print("{s}(hosted-header\n", .{indent_str});
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
    const slice = ast.node_slices.slice(slice_idx);
    for (slice, 0..) |node_idx, i| {
        if (i > 0) try output.appendSlice(", ");
        try writeNodeInline(output, ast, env, node_idx);
    }
}

fn writeNodeInline(output: *std.ArrayList(u8), ast: *const AST2, env: *const base.CommonEnv, idx: AST2.Node.Idx) !void {
    // For inline nodes, we write them compactly without indentation
    // but still with full structure
    const tag = ast.tag(idx);
    const start_pos = ast.start(idx);
    
    // Simplify some tag names for better readability
    const tag_name = switch (tag) {
        .list_literal => "list",
        else => @tagName(tag),
    };
    try output.writer().print("({s}", .{tag_name});
    
    // Write node-specific content based on tag
    switch (tag) {
        .uc, .lc, .var_lc, .neg_lc, .not_lc, .dot_lc, .double_dot_lc => {
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
            try output.writer().print(" \"<big>\"", .{});
        },
        .list_literal => {
            // List literal stores the count, not a slice index
            // The elements are the preceding nodes in the AST
            const elem_count = ast.payload(idx).list_elems;
            const list_idx_num = @intFromEnum(idx);
            
            // The elements are the nodes before this list_literal node
            const start_idx = list_idx_num - elem_count;
            
            for (0..elem_count) |i| {
                try output.appendSlice(" (");
                
                const elem_idx = @as(AST2.Node.Idx, @enumFromInt(start_idx + i));
                const node_tag = ast.tag(elem_idx);
                const node_pos = ast.start(elem_idx);
                
                // Write simplified node type and value inline
                switch (node_tag) {
                    .num_literal_i32 => {
                        const value = ast.payload(elem_idx).num_literal_i32;
                        try output.writer().print("i32 {d} @{d}", .{value, node_pos.offset});
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
            const nodes_idx = ast.payload(idx).block_nodes;
            const nodes = ast.node_slices.slice(nodes_idx);
            try output.appendSlice(" (");
            for (nodes, 0..) |node_idx, i| {
                if (i > 0) try output.appendSlice(", ");
                try writeNodeInline(output, ast, env, node_idx);
            }
            try output.appendSlice(")");
        },
        .apply => {
            const nodes_idx = ast.payload(idx).block_nodes;
            const nodes = ast.node_slices.slice(nodes_idx);
            if (nodes.len > 0) {
                try output.appendSlice(" ");
                try writeNodeInline(output, ast, env, nodes[0]);
                if (nodes.len > 1) {
                    try output.appendSlice(" [");
                    for (nodes[1..], 0..) |arg_idx, i| {
                        if (i > 0) try output.appendSlice(", ");
                        try writeNodeInline(output, ast, env, arg_idx);
                    }
                    try output.appendSlice("]");
                }
            }
        },
        .binop_equals, .binop_double_equals, .binop_not_equals,
        .binop_colon, .binop_plus, .binop_minus, .binop_star,
        .binop_slash, .binop_double_slash, .binop_double_question,
        .binop_gt, .binop_gte, .binop_lt, .binop_lte,
        .binop_thick_arrow, .binop_thin_arrow,
        .binop_and, .binop_or, .binop_pipe => {
            const binop = ast.binOp(idx);
            std.debug.print("  binop at idx={d}: lhs={d}, rhs={d}\n", .{@intFromEnum(idx), @intFromEnum(binop.lhs), @intFromEnum(binop.rhs)});
            try output.appendSlice(" ");
            try writeNodeInline(output, ast, env, binop.lhs);
            try output.appendSlice(" ");
            try writeNodeInline(output, ast, env, binop.rhs);
        },
        .lambda => {
            const lambda = ast.lambda(idx);
            try output.appendSlice(" [");
            for (lambda.args, 0..) |arg_idx, i| {
                if (i > 0) try output.appendSlice(", ");
                try writeNodeInline(output, ast, env, arg_idx);
            }
            try output.appendSlice("] ");
            try writeNodeInline(output, ast, env, lambda.body);
        },
        .if_else, .if_without_else => {
            const branches = ast.payload(idx).if_branches;
            const branches_idx = @as(AST2.NodeSlices.Idx, @enumFromInt(branches));
            const branch_nodes = ast.node_slices.slice(branches_idx);
            // if-else has pairs of (condition, body), potentially with final else
            for (branch_nodes, 0..) |node_idx, i| {
                if (i > 0) try output.appendSlice(" ");
                try writeNodeInline(output, ast, env, node_idx);
            }
        },
        .match => {
            const branches = ast.payload(idx).match_branches;  
            const branches_idx = @as(AST2.NodeSlices.Idx, @enumFromInt(branches));
            const branch_nodes = ast.node_slices.slice(branches_idx);
            // match has condition followed by pattern-body pairs
            if (branch_nodes.len > 0) {
                try output.appendSlice(" ");
                try writeNodeInline(output, ast, env, branch_nodes[0]); // condition
                if (branch_nodes.len > 1) {
                    try output.appendSlice(" [");
                    for (branch_nodes[1..], 0..) |branch_idx, i| {
                        if (i > 0) try output.appendSlice(", ");
                        try writeNodeInline(output, ast, env, branch_idx);
                    }
                    try output.appendSlice("]");
                }
            }
        },
        .unary_not, .unary_neg, .unary_double_dot => {
            // Unary operators store their operand as the preceding node
            const child_idx = @as(AST2.Node.Idx, @enumFromInt(@intFromEnum(idx) - 1));
            try output.appendSlice(" ");
            try writeNodeInline(output, ast, env, child_idx);
        },
        .ret, .crash => {
            // ret and crash probably store their expressions inline somehow
            // For now just mark them as having no operand
            try output.appendSlice(" <operand>");
        },
        .for_loop => {
            const nodes_idx = ast.payload(idx).block_nodes;
            const nodes = ast.node_slices.slice(nodes_idx);
            // for x in y { ... }
            if (nodes.len >= 2) {
                try output.appendSlice(" ");
                try writeNodeInline(output, ast, env, nodes[0]); // pattern
                try output.appendSlice(" ");
                try writeNodeInline(output, ast, env, nodes[1]); // iterable
                if (nodes.len > 2) {
                    try output.appendSlice(" [");
                    for (nodes[2..], 0..) |body_idx, i| {
                        if (i > 0) try output.appendSlice(", ");
                        try writeNodeInline(output, ast, env, body_idx);
                    }
                    try output.appendSlice("]");
                }
            }
        },
        .while_loop => {
            const nodes_idx = ast.payload(idx).block_nodes;
            const nodes = ast.node_slices.slice(nodes_idx);
            // while x { ... }
            if (nodes.len >= 1) {
                try output.appendSlice(" ");
                try writeNodeInline(output, ast, env, nodes[0]); // condition
                if (nodes.len > 1) {
                    try output.appendSlice(" [");
                    for (nodes[1..], 0..) |body_idx, i| {
                        if (i > 0) try output.appendSlice(", ");
                        try writeNodeInline(output, ast, env, body_idx);
                    }
                    try output.appendSlice("]");
                }
            }
        },
        .empty_record => {
            // No inner content for empty record
        },
        .empty_list => {
            // No inner content for empty list
        },
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
            const nodes_idx = ast.payload(idx).block_nodes;
            const nodes = ast.node_slices.slice(nodes_idx);
            try output.appendSlice(" [");
            for (nodes, 0..) |node_idx, i| {
                if (i > 0) try output.appendSlice(", ");
                try writeNodeInline(output, ast, env, node_idx);
            }
            try output.appendSlice("]");
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
    const indent_str = indent[0..indent_level * 2];
    
    const tag = ast.tag(idx);
    const start_pos = ast.start(idx);
    
    try output.writer().print("{s}({s}", .{
        indent_str,
        @tagName(tag),
    });
    
    // Write node-specific content based on tag
    switch (tag) {
        .uc, .lc, .var_lc, .neg_lc, .not_lc, .dot_lc, .double_dot_lc => {
            const ident_idx = ast.payload(idx).ident;
            const ident_text = env.getIdent(ident_idx);
            try output.writer().print(" \"{s}\" @{d})\n", .{ident_text, start_pos.offset});
        },
        .num_literal_i32 => {
            const value = ast.payload(idx).num_literal_i32;
            try output.writer().print(" {d} @{d})\n", .{value, start_pos.offset});
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
            // TODO: Fix ByteSlices.slice alignment issue
            try output.writer().print(" \"<big>\" @{d})\n", .{start_pos.offset});
        },
        .list_literal => {
            const nodes_idx = ast.payload(idx).block_nodes;
            const nodes = ast.node_slices.slice(nodes_idx);
            try output.appendSlice(" @");
            try output.writer().print("{d}\n", .{start_pos.offset});
            try output.writer().print("{s}  [", .{indent_str});
            for (nodes, 0..) |node_idx, i| {
                if (i > 0) try output.appendSlice(", ");
                try writeNodeInline(output, ast, env, node_idx);
            }
            try output.appendSlice("]\n");
        },
        .tuple_literal, .record_literal, .block => {
            const nodes_idx = ast.payload(idx).block_nodes;
            const nodes = ast.node_slices.slice(nodes_idx);
            try output.appendSlice(" @");
            try output.writer().print("{d}\n", .{start_pos.offset});
            try output.writer().print("{s}  (", .{indent_str});
            for (nodes, 0..) |node_idx, i| {
                if (i > 0) try output.appendSlice(", ");
                try writeNodeInline(output, ast, env, node_idx);
            }
            try output.appendSlice(")\n");
        },
        .lambda => {
            const lambda = ast.lambda(idx);
            try output.appendSlice(" @");
            try output.writer().print("{d}\n", .{start_pos.offset});
            try output.writer().print("{s}  args: [", .{indent_str});
            for (lambda.args, 0..) |arg_idx, i| {
                if (i > 0) try output.appendSlice(", ");
                try writeNodeInline(output, ast, env, arg_idx);
            }
            try output.appendSlice("]\n");
            try output.writer().print("{s}  body: ", .{indent_str});
            try writeNodeInline(output, ast, env, lambda.body);
            try output.appendSlice("\n");
        },
        .apply => {
            const nodes_idx = ast.payload(idx).block_nodes;
            const nodes = ast.node_slices.slice(nodes_idx);
            try output.appendSlice(" @");
            try output.writer().print("{d}\n", .{start_pos.offset});
            if (nodes.len > 0) {
                try output.writer().print("{s}  func: ", .{indent_str});
                try writeNodeInline(output, ast, env, nodes[0]);
                try output.appendSlice("\n");
                if (nodes.len > 1) {
                    try output.writer().print("{s}  args: [", .{indent_str});
                    for (nodes[1..], 0..) |arg_idx, i| {
                        if (i > 0) try output.appendSlice(", ");
                        try writeNodeInline(output, ast, env, arg_idx);
                    }
                    try output.appendSlice("]\n");
                }
            }
        },
        .if_else, .if_without_else => {
            const branches = ast.payload(idx).if_branches;
            const branches_idx = @as(AST2.NodeSlices.Idx, @enumFromInt(branches));
            const branch_nodes = ast.node_slices.slice(branches_idx);
            try output.appendSlice(" @");
            try output.writer().print("{d}\n", .{start_pos.offset});
            try output.writer().print("{s}  branches: [", .{indent_str});
            for (branch_nodes, 0..) |node_idx, i| {
                if (i > 0) try output.appendSlice(", ");
                try writeNodeInline(output, ast, env, node_idx);
            }
            try output.appendSlice("]\n");
        },
        .match => {
            const branches = ast.payload(idx).match_branches;
            const branches_idx = @as(AST2.NodeSlices.Idx, @enumFromInt(branches));
            const branch_nodes = ast.node_slices.slice(branches_idx);
            try output.appendSlice(" @");
            try output.writer().print("{d}\n", .{start_pos.offset});
            if (branch_nodes.len > 0) {
                try output.writer().print("{s}  cond: ", .{indent_str});
                try writeNodeInline(output, ast, env, branch_nodes[0]);
                try output.appendSlice("\n");
                if (branch_nodes.len > 1) {
                    try output.writer().print("{s}  branches: [", .{indent_str});
                    for (branch_nodes[1..], 0..) |branch_idx, i| {
                        if (i > 0) try output.appendSlice(", ");
                        try writeNodeInline(output, ast, env, branch_idx);
                    }
                    try output.appendSlice("]\n");
                }
            }
        },
        .empty_list, .empty_record => {
            try output.writer().print(" @{d})\n", .{start_pos.offset});
        },
        .unary_not, .unary_neg, .unary_double_dot => {
            // Unary operators store their operand as the preceding node
            const child_idx = @as(AST2.Node.Idx, @enumFromInt(@intFromEnum(idx) - 1));
            try output.appendSlice(" ");
            try writeNodeInline(output, ast, env, child_idx);
            try output.writer().print(" @{d})\n", .{start_pos.offset});
        },
        .ret, .crash => {
            // ret and crash probably store their expressions inline somehow
            try output.writer().print(" <operand> @{d})\n", .{start_pos.offset});
        },
        .malformed => {
            const diag = ast.payload(idx).malformed;
            try output.writer().print(" error:{s} @{d})\n", .{@tagName(diag), start_pos.offset});
        },
        else => {
            // Check if it's a binary operator by trying to get binOp
            if (@hasField(AST2.Node.Payload, "binop")) {
                switch (tag) {
                    .binop_equals, .binop_double_equals, .binop_not_equals,
                    .binop_colon, .binop_plus, .binop_minus, .binop_star,
                    .binop_slash, .binop_double_slash, .binop_double_question,
                    .binop_gt, .binop_gte, .binop_lt, .binop_lte,
                    .binop_thick_arrow, .binop_thin_arrow,
                    .binop_and, .binop_or, .binop_pipe => {
                        const binop = ast.binOp(idx);
                        std.debug.print("writeNode binop at idx={d}: lhs={d}, rhs={d}\n", .{@intFromEnum(idx), @intFromEnum(binop.lhs), @intFromEnum(binop.rhs)});
                        try output.appendSlice(" @");
                        try output.writer().print("{d}\n", .{start_pos.offset});
                        try output.writer().print("{s}  lhs: ", .{indent_str});
                        try writeNodeInline(output, ast, env, binop.lhs);
                        try output.appendSlice("\n");
                        try output.writer().print("{s}  rhs: ", .{indent_str});
                        try writeNodeInline(output, ast, env, binop.rhs);
                        try output.appendSlice("\n");
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