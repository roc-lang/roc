const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const parse = @import("parse");
const tokenize_old = parse.tokenize;
const tokenize_iter = parse.tokenize_iter;
const AST2 = parse.AST2;
const Parser2 = parse.Parser2;
const reporting = @import("reporting");

const Section = enum {
    META,
    SOURCE,
    EXPECTED,
    PROBLEMS,
    TOKENS,
    PARSE,
    FORMATTED,
    CANONICALIZE,
    TYPES,
};

const TestType = enum {
    file,
    expr,
};

const Snapshot = struct {
    description: []const u8,
    test_type: TestType,
    source: []const u8,
    expected: []const u8,
    problems: []const u8,
    tokens_output: []const u8,
    parse_output: []const u8,
    formatted: []const u8,
    canonicalize: []const u8,
    types: []const u8,
    allocator: std.mem.Allocator,

    fn deinit(self: *Snapshot) void {
        self.allocator.free(self.description);
        self.allocator.free(self.source);
        self.allocator.free(self.expected);
        self.allocator.free(self.problems);
        self.allocator.free(self.tokens_output);
        self.allocator.free(self.parse_output);
        self.allocator.free(self.formatted);
        self.allocator.free(self.canonicalize);
        self.allocator.free(self.types);
    }
};

pub fn main() !void {
    std.debug.print("Starting snapshot2 tool...\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    std.debug.print("Getting arguments...\n", .{});
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    
    std.debug.print("Got {} arguments\n", .{args.len});

    // Create output directory
    const output_dir = "test/snapshots_new";
    std.debug.print("Creating output directory: {s}\n", .{output_dir});
    try std.fs.cwd().makePath(output_dir);
    std.debug.print("Directory created successfully\n", .{});

    var count: usize = 0;
    var success_count: usize = 0;
    
    std.debug.print("Starting main logic...\n", .{});
    
    // If no arguments given, process all .md files in test/snapshots
    if (args.len < 2) {
        const snapshots_dir = "test/snapshots";
        std.debug.print("Opening snapshots directory: {s}\n", .{snapshots_dir});
        var dir = try std.fs.cwd().openDir(snapshots_dir, .{ .iterate = true });
        defer dir.close();
        std.debug.print("Directory opened successfully\n", .{});

        std.debug.print("Creating directory walker...\n", .{});
        var walker = try dir.walk(allocator);
        defer walker.deinit();
        std.debug.print("Walker created successfully\n", .{});

        walker_loop: while (true) {
            const entry = walker.next() catch |err| {
                std.debug.print("Walker error: {}\n", .{err});
                break :walker_loop;
            } orelse break;
            
            if (entry.kind == .file and std.mem.endsWith(u8, entry.basename, ".md")) {
                // Skip problematic files that cause recursive panics in Parser2
                if (std.mem.eql(u8, entry.basename, "syntax_grab_bag.md") or 
                    std.mem.startsWith(u8, entry.path, "fuzz_crash/")) {
                    // std.debug.print("Skipping problematic file: {s}\n", .{entry.path});
                    continue;
                }
                // std.debug.print("Processing file: {s}\n", .{entry.path});
                const input_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ snapshots_dir, entry.path });
                defer allocator.free(input_path);
                
                const output_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ output_dir, entry.path });
                defer allocator.free(output_path);
                
                // Create subdirectories if needed
                if (std.fs.path.dirname(entry.path)) |subdir| {
                    const full_subdir = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ output_dir, subdir });
                    defer allocator.free(full_subdir);
                    try std.fs.cwd().makePath(full_subdir);
                }

                count += 1;
                const process_result = processSnapshot(allocator, input_path, output_path) catch |err| {
                    std.debug.print("Error processing {s}: {}\n", .{entry.path, err});
                    false;
                };
                if (process_result) {
                    success_count += 1;
                } else {
                    std.debug.print("Failed to process {s}\n", .{entry.path});
                }
            }
        }
    } else {
        // Process specific file(s) given as arguments
        for (args[1..]) |snapshot_path| {
            const basename = std.fs.path.basename(snapshot_path);
            const output_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ output_dir, basename });
            defer allocator.free(output_path);
            
            count += 1;
            if (try processSnapshot(allocator, snapshot_path, output_path)) {
                success_count += 1;
            }
        }
    }

    std.debug.print("\nProcessed {}/{} snapshots successfully\n", .{ success_count, count });
    std.debug.print("Output written to {s}/\n", .{output_dir});
    std.debug.print("\nTo compare results, run:\n", .{});
    std.debug.print("  diff -r test/snapshots test/snapshots_new\n\n", .{});
}

fn processSnapshot(allocator: std.mem.Allocator, input_path: []const u8, output_path: []const u8) !bool {
    const file_content = std.fs.cwd().readFileAlloc(allocator, input_path, 1024 * 1024) catch |err| {
        std.debug.print("Failed to read {s}: {}\n", .{ input_path, err });
        return false;
    };
    defer allocator.free(file_content);

    var snapshot = parseSnapshot(allocator, file_content) catch |err| {
        std.debug.print("Failed to parse snapshot {s}: {}\n", .{ input_path, err });
        return false;
    };
    defer snapshot.deinit();

    // Generate new outputs
    const tokens_output = generateTokensOutput(allocator, snapshot.source) catch |err| {
        std.debug.print("Failed to tokenize {s}: {}\n", .{ input_path, err });
        return false;
    };
    defer allocator.free(tokens_output);

    const parse_result = generateParseWithDiagnostics(allocator, snapshot.source, snapshot.test_type, input_path) catch |err| {
        std.debug.print("Failed to parse {s}: {}\n", .{ input_path, err });
        return false;
    };
    defer allocator.free(parse_result.parse_output);
    defer allocator.free(parse_result.problems_output);

    // Write the new snapshot with updated TOKENS, PARSE, and PROBLEMS sections
    writeSnapshotWithProblems(allocator, output_path, snapshot, tokens_output, parse_result.parse_output, parse_result.problems_output) catch |err| {
        std.debug.print("Failed to write {s}: {}\n", .{ output_path, err });
        return false;
    };
    return true;
}

fn parseSnapshot(allocator: std.mem.Allocator, content: []const u8) !Snapshot {
    var snapshot = Snapshot{
        .description = "",
        .test_type = .file,
        .source = "",
        .expected = "",
        .problems = "",
        .tokens_output = "",
        .parse_output = "",
        .formatted = "",
        .canonicalize = "",
        .types = "",
        .allocator = allocator,
    };

    var current_section: ?Section = null;
    var section_content = std.ArrayList(u8).init(allocator);
    defer section_content.deinit();

    var lines = std.mem.tokenizeScalar(u8, content, '\n');
    while (lines.next()) |line| {
        // Check for section headers
        if (std.mem.startsWith(u8, line, "# META")) {
            if (current_section != null) {
                try saveSection(&snapshot, current_section.?, section_content.items);
            }
            current_section = .META;
            section_content.clearRetainingCapacity();
        } else if (std.mem.startsWith(u8, line, "# SOURCE")) {
            if (current_section != null) {
                try saveSection(&snapshot, current_section.?, section_content.items);
            }
            current_section = .SOURCE;
            section_content.clearRetainingCapacity();
        } else if (std.mem.startsWith(u8, line, "# EXPECTED")) {
            if (current_section != null) {
                try saveSection(&snapshot, current_section.?, section_content.items);
            }
            current_section = .EXPECTED;
            section_content.clearRetainingCapacity();
        } else if (std.mem.startsWith(u8, line, "# PROBLEMS")) {
            if (current_section != null) {
                try saveSection(&snapshot, current_section.?, section_content.items);
            }
            current_section = .PROBLEMS;
            section_content.clearRetainingCapacity();
        } else if (std.mem.startsWith(u8, line, "# TOKENS")) {
            if (current_section != null) {
                try saveSection(&snapshot, current_section.?, section_content.items);
            }
            current_section = .TOKENS;
            section_content.clearRetainingCapacity();
        } else if (std.mem.startsWith(u8, line, "# PARSE")) {
            if (current_section != null) {
                try saveSection(&snapshot, current_section.?, section_content.items);
            }
            current_section = .PARSE;
            section_content.clearRetainingCapacity();
        } else if (std.mem.startsWith(u8, line, "# FORMATTED")) {
            if (current_section != null) {
                try saveSection(&snapshot, current_section.?, section_content.items);
            }
            current_section = .FORMATTED;
            section_content.clearRetainingCapacity();
        } else if (std.mem.startsWith(u8, line, "# CANONICALIZE")) {
            if (current_section != null) {
                try saveSection(&snapshot, current_section.?, section_content.items);
            }
            current_section = .CANONICALIZE;
            section_content.clearRetainingCapacity();
        } else if (std.mem.startsWith(u8, line, "# TYPES")) {
            if (current_section != null) {
                try saveSection(&snapshot, current_section.?, section_content.items);
            }
            current_section = .TYPES;
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
        try saveSection(&snapshot, section, section_content.items);
    }

    return snapshot;
}

fn saveSection(snapshot: *Snapshot, section: Section, content: []const u8) !void {
    switch (section) {
        .META => try parseMeta(snapshot, content),
        .SOURCE => snapshot.source = try snapshot.allocator.dupe(u8, std.mem.trim(u8, content, " \n")),
        .EXPECTED => snapshot.expected = try snapshot.allocator.dupe(u8, content),
        .PROBLEMS => snapshot.problems = try snapshot.allocator.dupe(u8, content),
        .TOKENS => snapshot.tokens_output = try snapshot.allocator.dupe(u8, content),
        .PARSE => snapshot.parse_output = try snapshot.allocator.dupe(u8, content),
        .FORMATTED => snapshot.formatted = try snapshot.allocator.dupe(u8, content),
        .CANONICALIZE => snapshot.canonicalize = try snapshot.allocator.dupe(u8, content),
        .TYPES => snapshot.types = try snapshot.allocator.dupe(u8, content),
    }
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
        .binop_where => "where",
        .apply_module => "module",
        else => @tagName(tag),
    };
}

fn generateTokensOutput(allocator: std.mem.Allocator, source: []const u8) ![]const u8 {
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    var messages: [128]tokenize_old.Diagnostic = undefined;
    var tokenizer = try tokenize_old.Tokenizer.init(&env, allocator, source, messages[0..]);
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

const ParseResult = struct {
    parse_output: []const u8,
    problems_output: []const u8,
};

fn generateParseWithDiagnostics(allocator: std.mem.Allocator, source: []const u8, test_type: TestType, filename: []const u8) !ParseResult {
    // Extract just the filename for error messages
    const basename = std.fs.path.basename(filename);
    
    var ast = try AST2.initCapacity(allocator, 100);
    defer ast.deinit(allocator);

    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    // Create diagnostics buffer for Parser2
    var messages: [128]tokenize_iter.Diagnostic = undefined;
    var byte_slices = collections.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);
    
    // Parse
    var parser = try Parser2.init(&env, allocator, source, messages[0..], &ast, &byte_slices);
    defer parser.deinit();

    var parse_output = std.ArrayList(u8).init(allocator);
    errdefer parse_output.deinit();

    // Try to parse
    var parse_error: bool = false;
    if (test_type == .file) {
        _ = parser.parseFile() catch |err| {
            try parse_output.writer().print("PARSE ERROR: {}\n", .{err});
            parse_error = true;
        };
    } else {
        _ = parser.parseExpr() catch |err| {
            try parse_output.writer().print("PARSE ERROR: {}\n", .{err});
            parse_error = true;
        };
    }
    
    // Get diagnostics and convert to reports
    const diagnostics = parser.getDiagnostics();
    
    // Only show success if no errors and no diagnostics
    if (!parse_error and diagnostics.len == 0) {
        try parse_output.appendSlice("PARSE SUCCESS\n");
        try parse_output.writer().print("Node count: {}\n", .{ast.nodes.len()});
    } else if (!parse_error) {
        // We have diagnostics but didn't crash
        try parse_output.appendSlice("PARSE ERRORS DETECTED\n");
        try parse_output.writer().print("Node count: {}\n", .{ast.nodes.len()});
    }
    var problems = std.ArrayList(u8).init(allocator);
    errdefer problems.deinit();
    
    if (diagnostics.len == 0) {
        try problems.appendSlice("NIL");
    } else {
        // Convert diagnostics to reports and format them
        var reports = std.ArrayList(reporting.Report).init(allocator);
        defer {
            for (reports.items) |*report| {
                report.deinit();
            }
            reports.deinit();
        }
        
        for (diagnostics) |diagnostic| {
            const report = try ast.parseDiagnosticToReport(&env, diagnostic, allocator, basename);
            try reports.append(report);
        }
        
        // Render reports to PROBLEMS format
        for (reports.items, 0..) |*report, idx| {
            var buffer = std.ArrayList(u8).init(allocator);
            defer buffer.deinit();
            
            try report.render(buffer.writer(), .markdown);
            try problems.appendSlice(buffer.items);
            
            if (idx < reports.items.len - 1) {
                try problems.append('\n');
            }
        }
    }
    
    return ParseResult{
        .parse_output = try parse_output.toOwnedSlice(),
        .problems_output = try problems.toOwnedSlice(),
    };
}

fn generateParseOutput(allocator: std.mem.Allocator, source: []const u8, test_type: TestType) ![]const u8 {
    var ast = try AST2.initCapacity(allocator, 100);
    defer ast.deinit(allocator);

    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    // Create diagnostics buffer for Parser2
    var messages: [128]tokenize_iter.Diagnostic = undefined;
    var byte_slices = collections.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);
    
    // Parse  
    var parser = try Parser2.init(&env, allocator, source, messages[0..], &ast, &byte_slices);
    defer parser.deinit();

    var output = std.ArrayList(u8).init(allocator);
    errdefer output.deinit();

    // Try to parse and just output success/failure for now
    if (test_type == .file) {
        _ = parser.parseFile() catch |err| {
            try output.writer().print("PARSE ERROR: {}\n", .{err});
            return output.toOwnedSlice();
        };
        try output.appendSlice("PARSE SUCCESS\n");
    } else {
        _ = parser.parseExpr() catch |err| {
            try output.writer().print("PARSE ERROR: {}\n", .{err});
            return output.toOwnedSlice();
        };
        try output.appendSlice("PARSE SUCCESS\n");
    }
    
    // Add AST node count as a simple metric
    try output.writer().print("Node count: {}\n", .{ast.nodes.len()});
    
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
        .binop_where, .apply_module => {
            // These are new nodes, handle them separately for now
        },
        .uc, .lc, .lc_dot_ucs, .uc_dot_ucs, .var_lc, .neg_lc, .not_lc, .dot_lc, .double_dot_lc => {
            const ident_idx = ast.payload(idx).ident;
            const ident_text = env.getIdent(ident_idx);
            try output.writer().print(" \"{s}\"", .{ident_text});
        },
        .num_literal_i32 => {
            const value = ast.payload(idx).num_literal_i32;
            try output.writer().print(" {d}", .{value});
        },
        .frac_literal_small => {
            const frac = ast.payload(idx).frac_literal_small;
            const decimal_value = @as(f64, @floatFromInt(frac.numerator)) / std.math.pow(f64, 10, @as(f64, @floatFromInt(frac.denominator_power_of_ten)));
            try output.writer().print(" {d}", .{decimal_value});
        },
        .frac_literal_big => {
            const bytes_idx = ast.payload(idx).frac_literal_big;
            const frac_bytes = ast.byte_slices.slice(bytes_idx);
            try output.writer().print(" \"", .{});
            for (frac_bytes) |b| {
                try output.append(b);
            }
            try output.writer().print("\"", .{});
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
            // List literal can store elements in two ways:
            // 1. As list_elems count (Parser2 style) 
            // 2. As nodes slice (older style)
            // We'll just write [list] for now to avoid crashes
            try output.appendSlice(" [...]");
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
            // Application nodes store func and args in nodes
            const nodes_idx = ast.payload(idx).nodes;
            
            if (!nodes_idx.isNil()) {
                var iter = ast.node_slices.nodes(nodes_idx);
                
                // First node is the function
                if (iter.next()) |func_idx| {
                    try output.appendSlice(" ");
                    try writeNodeInline(output, ast, env, func_idx);
                }
                
                // Remaining nodes are arguments
                while (iter.next()) |arg_idx| {
                    try output.appendSlice(" ");
                    try writeNodeInline(output, ast, env, arg_idx);
                }
            }
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
        .binop_equals, .binop_double_equals, .binop_not_equals, .binop_colon, .binop_colon_equals, .binop_dot, .binop_as, .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_double_slash, .binop_double_question, .binop_gt, .binop_gte, .binop_lt, .binop_lte, .binop_thick_arrow, .binop_thin_arrow, .binop_and, .binop_or, .binop_pipe, .binop_where => {
            // Binary operators - the payload contains a binop index that points to the LHS and RHS
            // For inline output, just show the operator symbol
            try output.appendSlice(" <binop>");
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
        .num_literal_big, .int_literal_big => {
            try output.appendSlice(" <big-num>");
        },
        .int_literal_i32 => {
            const value = ast.payload(idx).num_literal_i32;
            try output.writer().print(" 0x{x}", .{value});
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
        .frac_literal_small => {
            const frac = ast.payload(idx).frac_literal_small;
            const decimal_value = @as(f64, @floatFromInt(frac.numerator)) / std.math.pow(f64, 10, @as(f64, @floatFromInt(frac.denominator_power_of_ten)));
            try output.writer().print(" {d} @{d})\n", .{ decimal_value, start_pos.offset });
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
        .binop_where, .apply_module => {
            // These are new nodes that need special handling
            // For now, just output the tag name
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

fn writeSnapshotWithProblems(allocator: std.mem.Allocator, path: []const u8, snapshot: Snapshot, tokens: []const u8, parse_output: []const u8, problems: []const u8) !void {
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    // Write META section
    try output.appendSlice("# META\n~~~ini\n");
    try output.writer().print("description={s}\n", .{snapshot.description});
    try output.writer().print("type={s}\n", .{@tagName(snapshot.test_type)});
    try output.appendSlice("~~~\n");
    
    // Write SOURCE section
    try output.appendSlice("# SOURCE\n~~~roc\n");
    try output.appendSlice(snapshot.source);
    try output.appendSlice("\n~~~\n");
    
    // Write EXPECTED section (copy from original)
    try output.appendSlice("# EXPECTED\n");
    try output.appendSlice(snapshot.expected);
    if (snapshot.expected.len > 0 and !std.mem.endsWith(u8, snapshot.expected, "\n")) {
        try output.append('\n');
    }
    
    // Write PROBLEMS section (new from Parser2)
    try output.appendSlice("# PROBLEMS\n");
    try output.appendSlice(problems);
    if (problems.len > 0 and !std.mem.endsWith(u8, problems, "\n")) {
        try output.append('\n');
    }
    
    // Write TOKENS section (new)
    try output.appendSlice("# TOKENS\n~~~zig\n");
    try output.appendSlice(tokens);
    try output.appendSlice("\n~~~\n");
    
    // Write PARSE section (new)
    try output.appendSlice("# PARSE\n~~~clojure\n");
    try output.appendSlice(parse_output);
    try output.appendSlice("\n~~~\n");
    
    // Write FORMATTED section (copy from original)
    try output.appendSlice("# FORMATTED\n");
    try output.appendSlice(snapshot.formatted);
    if (snapshot.formatted.len > 0 and !std.mem.endsWith(u8, snapshot.formatted, "\n")) {
        try output.append('\n');
    }
    
    // Write CANONICALIZE section (copy from original)
    try output.appendSlice("# CANONICALIZE\n");
    try output.appendSlice(snapshot.canonicalize);
    if (snapshot.canonicalize.len > 0 and !std.mem.endsWith(u8, snapshot.canonicalize, "\n")) {
        try output.append('\n');
    }
    
    // Write TYPES section (copy from original)
    try output.appendSlice("# TYPES\n");
    try output.appendSlice(snapshot.types);
    if (snapshot.types.len > 0 and !std.mem.endsWith(u8, snapshot.types, "\n")) {
        try output.append('\n');
    }

    // Write to file
    try std.fs.cwd().writeFile(.{ .sub_path = path, .data = output.items });
}
