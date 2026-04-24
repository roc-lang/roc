//! HTML documentation renderer for Roc modules.
//!
//! Generates a standalone HTML documentation site from `PackageDocs`.
//! Each module gets its own page with linked type signatures, and an
//! index page lists all modules in the package.

const std = @import("std");
const Allocator = std.mem.Allocator;
const DocModel = @import("DocModel.zig");
const DocType = DocModel.DocType;

// Static assets embedded at compile time
const embedded_css = @embedFile("static/styles.css");
const embedded_js = @embedFile("static/search.js");

const Writer = *std.Io.Writer;

/// Tree node for sidebar/content hierarchy
const SidebarNode = struct {
    name: []const u8, // The name component at this level
    full_path: []const u8, // Full qualified name (allocated)
    is_type: bool, // Is this a type definition?
    is_leaf: bool, // Is this a leaf entry?
    entry: ?*const DocModel.DocEntry, // Reference to the actual entry (if leaf)
    children: std.ArrayList(*SidebarNode),
    owns_full_path: bool, // Whether we own the full_path allocation

    fn init(gpa: Allocator, name: []const u8, full_path: []const u8, owns_full_path: bool) !*SidebarNode {
        const node = try gpa.create(SidebarNode);
        const children = try std.ArrayList(*SidebarNode).initCapacity(gpa, 0);
        node.* = .{
            .name = name,
            .full_path = full_path,
            .is_type = false,
            .is_leaf = false,
            .entry = null,
            .children = children,
            .owns_full_path = owns_full_path,
        };
        return node;
    }

    fn deinit(self: *SidebarNode, gpa: Allocator) void {
        for (self.children.items) |child| {
            child.deinit(gpa);
        }
        self.children.deinit(gpa);
        if (self.owns_full_path) {
            gpa.free(self.full_path);
        }
        gpa.destroy(self);
    }
};

/// Context for rendering, shared across all pages.
const RenderContext = struct {
    package_docs: *const DocModel.PackageDocs,
    known_modules: std.StringHashMapUnmanaged(void),
    current_module: ?[]const u8,
    current_module_entries: ?[]const DocModel.DocEntry = null,
    /// When true, the single module's page is rendered at the root (index.html),
    /// so links to the module should point to the root instead of a subdirectory.
    single_module_at_root: bool = false,
    /// When true, renderDocTypeHtml emits plain <span> instead of <a> for type
    /// references. Used inside search entries to avoid invalid nested <a> tags.
    suppress_type_links: bool = false,

    fn init(package_docs: *const DocModel.PackageDocs, gpa: Allocator) RenderContext {
        var known = std.StringHashMapUnmanaged(void){};
        for (package_docs.modules) |mod| {
            known.put(gpa, mod.name, {}) catch {};
        }
        return .{
            .package_docs = package_docs,
            .known_modules = known,
            .current_module = null,
            .current_module_entries = null,
        };
    }

    fn deinit(self: *RenderContext, gpa: Allocator) void {
        self.known_modules.deinit(gpa);
    }
};

/// Generate the complete HTML documentation site from PackageDocs.
/// Creates directories and writes all files under `output_dir_path`.
pub fn renderPackageDocs(
    gpa: Allocator,
    package_docs: *const DocModel.PackageDocs,
    output_dir_path: []const u8,
) !void {
    // Ensure the output directory exists
    std.fs.cwd().makePath(output_dir_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var output_dir = try std.fs.cwd().openDir(output_dir_path, .{});
    defer output_dir.close();

    var ctx = RenderContext.init(package_docs, gpa);
    defer ctx.deinit(gpa);

    // Write static assets
    try writeStaticAssets(output_dir);

    if (package_docs.modules.len == 1) {
        // Single module: write module content directly to root index.html
        ctx.single_module_at_root = true;
        const mod = &package_docs.modules[0];
        ctx.current_module = mod.name;
        ctx.current_module_entries = mod.entries;
        try writeModulePageToDir(&ctx, gpa, output_dir, mod, "");
        ctx.current_module = null;
        ctx.current_module_entries = null;
    } else {
        // Multiple modules: write package index and per-module pages
        try writePackageIndex(&ctx, gpa, output_dir);

        for (package_docs.modules) |*mod| {
            ctx.current_module = mod.name;
            ctx.current_module_entries = mod.entries;
            try writeModulePage(&ctx, gpa, output_dir, mod);
        }
        ctx.current_module = null;
        ctx.current_module_entries = null;
    }
}

fn writeStaticAssets(dir: std.fs.Dir) !void {
    try dir.writeFile(.{ .sub_path = "styles.css", .data = embedded_css });
    try dir.writeFile(.{ .sub_path = "search.js", .data = embedded_js });
}

fn writePackageIndex(ctx: *const RenderContext, gpa: Allocator, dir: std.fs.Dir) !void {
    const file = try dir.createFile("index.html", .{});
    defer file.close();
    var buf: [4096]u8 = undefined;
    var bw = file.writer(&buf);
    const w = &bw.interface;

    var index_title_buf: [256]u8 = undefined;
    const index_title = std.fmt.bufPrint(&index_title_buf, "{s} Docs", .{ctx.package_docs.name}) catch ctx.package_docs.name;
    try writeHtmlHead(w, index_title, "");
    try writeBodyOpen(w);
    try renderSidebar(w, ctx, gpa, "");

    // Main content
    try writeMainOpen(w, ctx, gpa, "");
    try w.writeAll("        <h1 class=\"module-name\">");
    try writeHtmlEscaped(w, ctx.package_docs.name);
    try w.writeAll("</h1>\n");

    // Module list
    try w.writeAll("        <ul class=\"index-module-links\">\n");
    for (ctx.package_docs.modules) |mod| {
        try w.writeAll("            <li><a href=\"");
        try writeHtmlEscaped(w, mod.name);
        try w.writeAll("/\">");
        try writeHtmlEscaped(w, mod.name);
        try w.writeAll("</a></li>\n");
    }
    try w.writeAll("        </ul>\n");

    try writeFooter(w);
    try w.writeAll("    </main>\n");
    try writeBodyClose(w);
    try bw.interface.flush();
}

fn writeModulePage(ctx: *const RenderContext, gpa: Allocator, dir: std.fs.Dir, mod: *const DocModel.ModuleDocs) !void {
    // Create module subdirectory
    dir.makeDir(mod.name) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var sub_dir = try dir.openDir(mod.name, .{});
    defer sub_dir.close();

    try writeModulePageToDir(ctx, gpa, sub_dir, mod, "../");
}

/// Write a module's documentation page as index.html in the given directory.
/// `base` is the relative path prefix for static assets (e.g. "" for root, "../" for subdirs).
fn writeModulePageToDir(ctx: *const RenderContext, gpa: Allocator, dir: std.fs.Dir, mod: *const DocModel.ModuleDocs, base: []const u8) !void {
    const file = try dir.createFile("index.html", .{});
    defer file.close();
    var buf: [4096]u8 = undefined;
    var bw = file.writer(&buf);
    const w = &bw.interface;

    var title_buf: [256]u8 = undefined;
    const title = std.fmt.bufPrint(&title_buf, "{s} Docs", .{mod.name}) catch mod.name;
    try writeHtmlHead(w, title, base);
    try writeBodyOpen(w);
    try renderSidebar(w, ctx, gpa, base);

    // Main content
    try writeMainOpen(w, ctx, gpa, base);
    try w.writeAll("        <h1 class=\"module-name\">");
    try writeHtmlEscaped(w, mod.name);
    if (mod.kind == .type_module) {
        try w.writeAll(" <span class=\"module-kind-badge\">Type Module</span>");
    }
    try w.writeAll("</h1>\n");

    // Build entry tree (automatically collapses redundant top-level node
    // matching the module name, e.g. Parser > Parser or Builtin > Builtin).
    const entry_tree = try buildEntryTree(gpa, mod.entries, mod.name);
    defer entry_tree.deinit(gpa);

    // Show collapsed type definition at module level
    if (entry_tree.collapsed_entry) |entry| {
        if (entry.kind == .nominal) {
            if (entry.type_signature) |sig| {
                try w.writeAll("        <code class=\"entry-type-def\">");
                try w.writeAll(":= ");
                try renderDocTypeHtml(w, ctx, sig, false);
                try w.writeAll("</code>\n");
            }
        }
    }

    // Module doc comment (fall back to collapsed entry's doc if module has none)
    const doc_comment = mod.module_doc orelse
        if (entry_tree.collapsed_entry) |entry| entry.doc_comment else null;
    if (doc_comment) |doc| {
        try w.writeAll("        <div class=\"module-doc\">\n");
        try renderDocComment(w, ctx, doc);
        try w.writeAll("        </div>\n");
    }

    // Render entries
    try renderEntryTree(w, ctx, entry_tree.root, 0);

    try writeFooter(w);
    try w.writeAll("    </main>\n");
    try writeBodyClose(w);
    try bw.interface.flush();
}

fn writeHtmlHead(w: Writer, title: []const u8, base: []const u8) !void {
    try w.writeAll("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n");
    try w.writeAll("    <meta charset=\"utf-8\">\n");
    try w.writeAll("    <title>");
    try writeHtmlEscaped(w, title);
    try w.writeAll("</title>\n");
    try w.writeAll("    <meta name=\"viewport\" content=\"width=device-width\">\n");
    try w.writeAll("    <link rel=\"stylesheet\" href=\"");
    try w.writeAll(base);
    try w.writeAll("styles.css\">\n");
    try w.writeAll("    <script src=\"");
    try w.writeAll(base);
    try w.writeAll("search.js\" defer></script>\n");
    try w.writeAll("</head>\n");
}

const link_svg_defs =
    \\<svg xmlns="http://www.w3.org/2000/svg" style="display:none">
    \\  <defs>
    \\    <symbol id="link-icon" viewBox="0 0 640 512">
    \\      <path d="M562.8 267.7c56.5-56.5 56.5-148 0-204.5c-50-50-128.8-56.5-186.3-15.4l-1.6 1.1c-14.4 10.3-17.7 30.3-7.4 44.6s30.3 17.7 44.6 7.4l1.6-1.1c32.1-22.9 76-19.3 103.8 8.6c31.5 31.5 31.5 82.5 0 114L405.3 334.8c-31.5 31.5-82.5 31.5-114 0c-27.9-27.9-31.5-71.8-8.6-103.8l1.1-1.6c10.3-14.4 6.9-34.4-7.4-44.6s-34.4-6.9-44.6 7.4l-1.1 1.6C189.5 251.2 196 330 246 380c56.5 56.5 148 56.5 204.5 0L562.8 267.7zM43.2 244.3c-56.5 56.5-56.5 148 0 204.5c50 50 128.8 56.5 186.3 15.4l1.6-1.1c14.4-10.3 17.7-30.3 7.4-44.6s-30.3-17.7-44.6-7.4l-1.6 1.1c-32.1 22.9-76 19.3-103.8-8.6C57 372 57 321 88.5 289.5L200.7 177.2c31.5-31.5 82.5-31.5 114 0c27.9 27.9 31.5 71.8 8.6 103.9l-1.1 1.6c-10.3 14.4-6.9 34.4 7.4 44.6s34.4 6.9 44.6-7.4l1.1-1.6C416.5 260.8 410 182 360 132c-56.5-56.5-148-56.5-204.5 0L43.2 244.3z"/>
    \\    </symbol>
    \\  </defs>
    \\</svg>
;

const link_svg_use =
    \\<svg class="link-icon"><use href="#link-icon"/></svg>
;

fn writeBodyOpen(w: Writer) !void {
    try w.writeAll("<body>\n");
    try w.writeAll(link_svg_defs);
    try w.writeAll("\n");
}

fn writeBodyClose(w: Writer) !void {
    try w.writeAll("</body>\n</html>\n");
}

const menu_toggle_svg =
    \\<svg viewBox="0 0 24 24" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
    \\    <path d="M3 6h18v2H3V6zm0 5h18v2H3v-2zm0 5h18v2H3v-2z"/>
    \\</svg>
;

fn writeMainOpen(w: Writer, ctx: *const RenderContext, gpa: Allocator, base: []const u8) !void {
    try w.writeAll("    <main>\n");
    try w.writeAll("        <button class=\"menu-toggle\" aria-label=\"Toggle sidebar\">");
    try w.writeAll(menu_toggle_svg);
    try w.writeAll("</button>\n");
    try w.writeAll("        <form id=\"module-search-form\">\n");
    try w.writeAll("            <input type=\"search\" id=\"module-search\" placeholder=\"Search Documentation\" autocomplete=\"off\" />\n");
    try w.writeAll("            <ul id=\"search-type-ahead\" class=\"hidden\">\n");
    try renderSearchEntries(w, ctx, gpa, base);
    try w.writeAll("            </ul>\n");
    try w.writeAll("        </form>\n");
}

fn writeFooter(w: Writer) !void {
    try w.writeAll("        <footer><p>Made by people who like to make nice things.</p></footer>\n");
}

fn sortSidebarNodeChildren(node: *SidebarNode) void {
    std.mem.sortUnstable(*SidebarNode, node.children.items, {}, lessThanSidebarNode);

    for (node.children.items) |child| {
        sortSidebarNodeChildren(child);
    }
}

fn lessThanSidebarNode(_: void, a: *SidebarNode, b: *SidebarNode) bool {
    // Types come first
    if (a.is_type and !b.is_type) return true;
    if (!a.is_type and b.is_type) return false;
    // Then sort alphabetically
    return std.mem.order(u8, a.name, b.name) == .lt;
}

const EntryTree = struct {
    root: *SidebarNode,
    /// When the tree had a single top-level child whose name matched the
    /// module name (e.g. Parser.roc defining only `Parser`, or Builtin where
    /// every entry lives under `Builtin.*`), that redundant level is collapsed
    /// and the child's entry is stored here so callers can surface its type
    /// definition at the module level.
    collapsed_entry: ?*const DocModel.DocEntry,

    fn deinit(self: EntryTree, gpa: Allocator) void {
        self.root.deinit(gpa);
    }
};

fn buildEntryTree(gpa: Allocator, entries: []const DocModel.DocEntry, module_name: []const u8) !EntryTree {
    const root = try SidebarNode.init(gpa, "", "", false);

    for (entries) |*entry| {
        var current = root;

        // Split entry name by dots
        var parts = try std.ArrayList([]const u8).initCapacity(gpa, 8);
        defer parts.deinit(gpa);

        var start: usize = 0;
        for (entry.name, 0..) |char, i| {
            if (char == '.') {
                try parts.append(gpa, entry.name[start..i]);
                start = i + 1;
            }
        }
        try parts.append(gpa, entry.name[start..]);

        // Build path through tree
        var path_so_far = try std.ArrayList(u8).initCapacity(gpa, 256);
        defer path_so_far.deinit(gpa);

        for (parts.items, 0..) |part, idx| {
            if (idx > 0) try path_so_far.append(gpa, '.');
            try path_so_far.appendSlice(gpa, part);

            const is_last = (idx == parts.items.len - 1);

            // Find or create child node
            var found: ?*SidebarNode = null;
            for (current.children.items) |child| {
                if (std.mem.eql(u8, child.name, part)) {
                    found = child;
                    break;
                }
            }

            if (found == null) {
                const full_path = try gpa.dupe(u8, path_so_far.items);
                const new_node = try SidebarNode.init(gpa, part, full_path, true);
                try current.children.append(gpa, new_node);
                found = new_node;
            }

            var node = found.?;

            if (is_last) {
                node.is_leaf = true;
                node.is_type = (entry.kind != .value);
                node.entry = entry;

                // Also add entry's children as nested nodes,
                // splitting dotted names into sub-trees
                for (entry.children) |*child_entry| {
                    try addChildToEntryTree(gpa, node, entry.name, child_entry);
                }
            }

            current = node;
        }
    }

    // Collapse: when the tree has a single top-level child whose name matches
    // the module, promote its children to the root and return its entry.
    var collapsed_entry: ?*const DocModel.DocEntry = null;
    if (root.children.items.len == 1 and
        std.mem.eql(u8, root.children.items[0].name, module_name))
    {
        const collapsed = root.children.items[0];
        collapsed_entry = collapsed.entry;

        root.children.deinit(gpa);
        root.children = collapsed.children;

        collapsed.children = std.ArrayList(*SidebarNode).empty;
        collapsed.deinit(gpa);
    }

    return .{ .root = root, .collapsed_entry = collapsed_entry };
}

/// Add a child entry to the content tree, splitting its name on dots
/// to create intermediate group nodes.
fn addChildToEntryTree(
    gpa: Allocator,
    parent_node: *SidebarNode,
    parent_entry_name: []const u8,
    child_entry: *const DocModel.DocEntry,
) !void {
    var current = parent_node;

    // Split child name by dots
    var child_parts = try std.ArrayList([]const u8).initCapacity(gpa, 8);
    defer child_parts.deinit(gpa);

    var start: usize = 0;
    for (child_entry.name, 0..) |char, ci| {
        if (char == '.') {
            try child_parts.append(gpa, child_entry.name[start..ci]);
            start = ci + 1;
        }
    }
    try child_parts.append(gpa, child_entry.name[start..]);

    // Build path through tree
    var child_path = try std.ArrayList(u8).initCapacity(gpa, 256);
    defer child_path.deinit(gpa);
    try child_path.appendSlice(gpa, parent_entry_name);

    for (child_parts.items, 0..) |part, idx| {
        try child_path.append(gpa, '.');
        try child_path.appendSlice(gpa, part);

        const is_child_last = (idx == child_parts.items.len - 1);

        // Find or create child node
        var found: ?*SidebarNode = null;
        for (current.children.items) |child| {
            if (std.mem.eql(u8, child.name, part)) {
                found = child;
                break;
            }
        }

        if (found == null) {
            const fp = try gpa.dupe(u8, child_path.items);
            const new_node = try SidebarNode.init(gpa, part, fp, true);
            try current.children.append(gpa, new_node);
            found = new_node;
        }

        const child_node = found.?;

        if (is_child_last) {
            child_node.is_leaf = true;
            child_node.is_type = (child_entry.kind != .value);
            child_node.entry = child_entry;

            // Recursively add this entry's own children
            for (child_entry.children) |*grandchild| {
                try addChildToEntryTree(gpa, child_node, child_path.items, grandchild);
            }
        }

        current = child_node;
    }
}

fn writeIndent(w: Writer, level: usize) !void {
    const max_indent = "                                        "; // 40 spaces (10 levels of 4)
    const spaces = @min(level * 4, max_indent.len);
    try w.writeAll(max_indent[0..spaces]);
}

fn renderEntryTree(
    w: Writer,
    ctx: *const RenderContext,
    node: *const SidebarNode,
    depth: usize,
) !void {
    // Skip the root node (empty name), process its children
    if (depth == 0) {
        for (node.children.items) |child| {
            try renderEntryTree(w, ctx, child, depth + 1);
        }
        return;
    }

    // Base indent: depth 1 = 2 levels (8 spaces), depth 2 = 4 levels (16 spaces), etc.
    // Each depth adds 2 indent levels: one for the entry, one for the children-container wrapper.
    const base = depth * 2;

    // Render this node if it's a leaf entry
    if (node.is_leaf) {
        if (node.entry) |entry| {
            // Determine CSS classes
            const type_class = if (node.is_type) "entry-type" else "entry-value";

            try writeIndent(w, base);
            try w.writeAll("<article class=\"entry ");
            try w.writeAll(type_class);
            try w.writeAll("\" id=\"");
            try writeHtmlEscaped(w, node.full_path);
            try w.writeAll("\">\n");

            const anchor_id = node.full_path;
            if (entry.kind == .@"opaque" or entry.kind == .nominal) {
                // Types rendered as a heading, with an optional type definition below.
                // Heading level scales with depth: depth 1 -> h2, depth 2 -> h3, etc.
                const heading_level: u8 = @intCast(@min(depth + 1, 6));
                try writeIndent(w, base + 1);
                try w.print("<h{d}>", .{heading_level});
                try w.writeAll("<a href=\"#");
                try writeHtmlEscaped(w, anchor_id);
                try w.writeAll("\" class=\"entry-anchor\" aria-label=\"Permalink to ");
                try writeHtmlEscaped(w, node.name);
                try w.writeAll("\">");
                try w.writeAll(link_svg_use);
                try w.writeAll("</a> ");
                try writeHtmlEscaped(w, node.name);
                try w.print("</h{d}>\n", .{heading_level});

                // Nominal types also show their type definition below the heading
                if (entry.kind == .nominal) {
                    if (entry.type_signature) |sig| {
                        try writeIndent(w, base + 1);
                        try w.writeAll("<code class=\"entry-type-def\">");
                        try w.writeAll(":= ");
                        try renderDocTypeHtml(w, ctx, sig, false);
                        try w.writeAll("</code>\n");
                    }
                }
            } else {
                // Signature block - styled as code, not a heading
                try writeIndent(w, base + 1);
                try w.writeAll("<div class=\"entry-signature\">\n");
                try writeIndent(w, base + 2);
                try w.writeAll("<a href=\"#");
                try writeHtmlEscaped(w, anchor_id);
                try w.writeAll("\" class=\"entry-anchor\" aria-label=\"Permalink to ");
                try writeHtmlEscaped(w, node.name);
                try w.writeAll("\">");
                try w.writeAll(link_svg_use);
                try w.writeAll("</a>\n");
                try writeIndent(w, base + 2);
                try w.writeAll("<code class=\"entry-signature-code\">");
                try renderEntrySignature(w, ctx, entry);
                try w.writeAll("</code>\n");
                try writeIndent(w, base + 1);
                try w.writeAll("</div>\n");
            }

            // Doc comment
            if (entry.doc_comment) |doc| {
                try writeIndent(w, base + 1);
                try w.writeAll("<div class=\"entry-doc\">\n");
                try renderDocComment(w, ctx, doc);
                try writeIndent(w, base + 1);
                try w.writeAll("</div>\n");
            }

            // Children container
            if (node.children.items.len > 0) {
                try writeIndent(w, base + 1);
                try w.writeAll("<div class=\"entry-children-container\">\n");
                for (node.children.items) |child| {
                    try renderEntryTree(w, ctx, child, depth + 1);
                }
                try writeIndent(w, base + 1);
                try w.writeAll("</div>\n");
            }

            try writeIndent(w, base);
            try w.writeAll("</article>\n");
        }
    } else if (node.children.items.len > 0) {
        // Non-leaf group node — render a group header and recurse at deeper depth
        try writeIndent(w, base);
        try w.writeAll("<section class=\"entry-group\" id=\"");
        try writeHtmlEscaped(w, node.full_path);
        try w.writeAll("\">\n");
        const heading_level: u8 = @intCast(@min(depth + 1, 6));
        try writeIndent(w, base + 1);
        try w.print("<h{d} class=\"entry-group-header\">", .{heading_level});
        try writeHtmlEscaped(w, node.name);
        try w.print("</h{d}>\n", .{heading_level});
        for (node.children.items) |child| {
            try renderEntryTree(w, ctx, child, depth + 1);
        }
        try writeIndent(w, base);
        try w.writeAll("</section>\n");
    }
}

const roc_logo_svg =
    \\<svg viewBox="0 -6 51 58" fill="none" xmlns="http://www.w3.org/2000/svg" role="img" aria-labelledby="logo-link">
    \\    <title id="logo-link">Home</title>
    \\    <polygon role="presentation" points="0,0 23.8834,3.21052 37.2438,19.0101 45.9665,16.6324 50.5,22 45,22 44.0315,26.3689 26.4673,39.3424 27.4527,45.2132 17.655,53 23.6751,22.7086" />
    \\</svg>
;

fn renderSidebarTree(
    w: Writer,
    module_name: []const u8,
    node: *SidebarNode,
    depth: usize,
) !void {
    // Skip root node
    if (depth > 0) {
        if (node.children.items.len > 0) {
            // Render as a type section with expandable sub-entries
            try w.writeAll("                        ");
            for (0..depth - 1) |_| {
                try w.writeAll("  ");
            }
            try w.writeAll("<li class=\"sidebar-type\">\n");
            try w.writeAll("                        ");
            for (0..depth - 1) |_| {
                try w.writeAll("  ");
            }
            try w.writeAll("  <a class=\"sidebar-type-name\" href=\"#");
            try writeHtmlEscaped(w, node.full_path);
            try w.writeAll("\">");
            try writeHtmlEscaped(w, node.name);
            try w.writeAll("</a>\n");
            try w.writeAll("                        ");
            for (0..depth - 1) |_| {
                try w.writeAll("  ");
            }
            try w.writeAll("  <ul class=\"sidebar-sub-entries\">\n");

            // Recurse for children
            for (node.children.items) |child| {
                try renderSidebarTree(w, module_name, child, depth + 1);
            }

            try w.writeAll("                        ");
            for (0..depth - 1) |_| {
                try w.writeAll("  ");
            }
            try w.writeAll("  </ul>\n");
            try w.writeAll("                        ");
            for (0..depth - 1) |_| {
                try w.writeAll("  ");
            }
            try w.writeAll("</li>\n");
        } else if (node.is_leaf) {
            if (depth == 1 and node.is_type) {
                // Top-level type with no children — render with the same
                // structure as types that have children so they all share
                // a consistent visual style in the sidebar.
                try w.writeAll("                        ");
                try w.writeAll("<li class=\"sidebar-type\">\n");
                try w.writeAll("                        ");
                try w.writeAll("  <a class=\"sidebar-type-name\" href=\"#");
                try writeHtmlEscaped(w, node.full_path);
                try w.writeAll("\">");
                try writeHtmlEscaped(w, node.name);
                try w.writeAll("</a>\n");
                try w.writeAll("                        ");
                try w.writeAll("</li>\n");
            } else {
                // Render as link
                try w.writeAll("                        ");
                for (0..depth - 1) |_| {
                    try w.writeAll("  ");
                }
                try w.writeAll("<li><a href=\"#");
                try writeHtmlEscaped(w, node.full_path);
                try w.writeAll("\">");
                try writeHtmlEscaped(w, node.name);
                try w.writeAll("</a></li>\n");
            }
        }
    } else {
        // Root node - just recurse
        for (node.children.items) |child| {
            try renderSidebarTree(w, module_name, child, depth + 1);
        }
    }
}

fn renderSidebarEntries(
    w: Writer,
    gpa: std.mem.Allocator,
    module_name: []const u8,
    entries: []const DocModel.DocEntry,
    _depth: usize,
) !void {
    _ = _depth; // No longer needed

    const entry_tree = try buildEntryTree(gpa, entries, module_name);
    defer entry_tree.deinit(gpa);

    try renderSidebarTree(w, module_name, entry_tree.root, 0);
}

fn renderSidebar(w: Writer, ctx: *const RenderContext, gpa: Allocator, base: []const u8) !void {
    try w.writeAll("    <nav id=\"sidebar-nav\">\n");
    try w.writeAll("        <div class=\"pkg-and-logo\">\n");
    try w.writeAll("            <a class=\"logo\" href=\"");
    // Use explicit path for empty base (index page)
    if (base.len == 0) {
        try w.writeAll(".");
    } else {
        try w.writeAll(base);
    }
    try w.writeAll("\">");
    try w.writeAll(roc_logo_svg);
    try w.writeAll("</a>\n");
    try w.writeAll("            <h1 class=\"pkg-full-name\"><a href=\"");
    try w.writeAll(base);
    try w.writeAll("\">");
    try writeHtmlEscaped(w, ctx.package_docs.name);
    try w.writeAll("</a></h1>\n");
    try w.writeAll("        </div>\n");

    try w.writeAll("        <div class=\"module-links-container\">\n");
    try w.writeAll("            <ul class=\"module-links\">\n");

    for (ctx.package_docs.modules) |mod| {
        const is_active = if (ctx.current_module) |cur|
            std.mem.eql(u8, cur, mod.name)
        else
            false;

        try w.writeAll("                <li class=\"sidebar-entry\">\n");
        try w.writeAll("                    <a class=\"sidebar-module-link");
        if (is_active) try w.writeAll(" active");
        try w.writeAll("\" data-module-name=\"");
        try writeHtmlEscaped(w, mod.name);
        try w.writeAll("\" href=\"");
        if (ctx.single_module_at_root) {
            // Single module rendered at root; link to root
            if (base.len == 0) {
                try w.writeAll(".");
            } else {
                try w.writeAll(base);
            }
        } else {
            try w.writeAll(base);
            try writeHtmlEscaped(w, mod.name);
            try w.writeAll("/");
        }
        try w.writeAll("\">");
        try w.writeAll("<button class=\"entry-toggle\">&#9654;</button>");
        try w.writeAll("<span>");
        try writeHtmlEscaped(w, mod.name);
        try w.writeAll("</span></a>\n");

        // Sub-entries - grouped hierarchically
        try w.writeAll("                    <ul class=\"sidebar-sub-entries\">\n");
        try renderSidebarEntries(w, gpa, mod.name, mod.entries, 0);
        try w.writeAll("                    </ul>\n");
        try w.writeAll("                </li>\n");
    }

    try w.writeAll("            </ul>\n");
    try w.writeAll("        </div>\n");
    try w.writeAll("    </nav>\n");
}

fn renderSearchEntries(w: Writer, ctx: *const RenderContext, gpa: Allocator, base: []const u8) !void {
    for (ctx.package_docs.modules) |mod| {
        const entry_tree = try buildEntryTree(gpa, mod.entries, mod.name);
        defer entry_tree.deinit(gpa);
        try renderSearchTree(w, ctx, mod.name, entry_tree.root, base);
    }
}

/// Walk the entry tree depth-first, emitting a search type-ahead <li> for
/// every leaf node that carries a DocEntry.
fn renderSearchTree(
    w: Writer,
    ctx: *const RenderContext,
    module_name: []const u8,
    node: *const SidebarNode,
    base: []const u8,
) !void {
    if (node.entry) |entry| {
        try w.writeAll("            <li class=\"hidden\"><a class=\"type-ahead-link\" href=\"");
        if (ctx.single_module_at_root) {
            try w.writeAll("#");
        } else {
            try w.writeAll(base);
            try writeHtmlEscaped(w, module_name);
            try w.writeAll("/#");
        }
        try writeHtmlEscaped(w, node.full_path);
        try w.writeAll("\">");

        // Build the display prefix from full_path minus the last component (node.name).
        // The full_path may still contain the module name prefix from before tree
        // collapse (e.g. "Builtin.Str.is_empty" → prefix "Builtin.Str"). Strip the
        // module name so we display "Str" instead of "Builtin.Str" (or avoid
        // "Pipeline.Pipeline" duplication in multi-module packages).
        const full_path_prefix = if (node.full_path.len > node.name.len)
            node.full_path[0 .. node.full_path.len - node.name.len - 1] // strip ".defName"
        else
            ""; // full_path == node.name, no intermediate path

        const display_prefix = if (std.mem.startsWith(u8, full_path_prefix, module_name) and
            full_path_prefix.len > module_name.len and
            full_path_prefix[module_name.len] == '.')
            full_path_prefix[module_name.len + 1 ..]
        else if (std.mem.eql(u8, full_path_prefix, module_name))
            "" // entire prefix is just the module name
        else
            full_path_prefix;

        const has_prefix = !ctx.single_module_at_root or display_prefix.len > 0;
        if (has_prefix) {
            try w.writeAll("<span class=\"type-ahead-module-name\">");
            if (!ctx.single_module_at_root) {
                try writeHtmlEscaped(w, module_name);
                if (display_prefix.len > 0) {
                    try w.writeAll(".");
                }
            }
            try writeHtmlEscaped(w, display_prefix);
            try w.writeAll("</span>");
            try w.writeAll(".");
        }
        try w.writeAll("<span class=\"type-ahead-def-name\">");
        try writeHtmlEscaped(w, node.name);
        try w.writeAll("</span>");
        if (entry.type_signature) |sig| {
            // Suppress type links inside search entries to avoid invalid
            // nested <a> tags (the entire entry is already wrapped in <a>).
            var no_links_ctx = ctx.*;
            no_links_ctx.suppress_type_links = true;
            const sig_ctx: *const RenderContext = &no_links_ctx;

            try w.writeAll(" <span class=\"type-ahead-signature\">");
            switch (entry.kind) {
                .value, .alias => {
                    try w.writeAll(": ");
                    try renderDocTypeHtml(w, sig_ctx, sig, false);
                },
                .nominal => {
                    try w.writeAll(":= ");
                    try renderDocTypeHtml(w, sig_ctx, sig, false);
                },
                .@"opaque" => {},
            }
            try w.writeAll("</span>");
        }
        try w.writeAll("</a></li>\n");
    }

    for (node.children.items) |child| {
        try renderSearchTree(w, ctx, module_name, child, base);
    }
}

fn renderEntrySignature(w: Writer, ctx: *const RenderContext, entry: *const DocModel.DocEntry) !void {
    try w.writeAll("<strong>");

    // Display only the identifier (last component) of the entry name
    // For "Builtin.Str.Utf8Problem.is_eq", display as "is_eq"
    const display_name = if (std.mem.lastIndexOfScalar(u8, entry.name, '.')) |idx|
        entry.name[idx + 1 ..]
    else
        entry.name;

    try writeHtmlEscaped(w, display_name);
    try w.writeAll("</strong>");

    if (entry.type_signature) |sig| {
        switch (entry.kind) {
            .value => {
                try w.writeAll(" : ");
                try renderDocTypeHtml(w, ctx, sig, false);
            },
            .alias => {
                try w.writeAll(" : ");
                try renderDocTypeHtml(w, ctx, sig, false);
            },
            .nominal => {
                try w.writeAll(" := ");
                try renderDocTypeHtml(w, ctx, sig, false);
            },
            .@"opaque" => {
                try w.writeAll(" :: <span class=\"type\">&lt;hidden&gt;</span>");
            },
        }
    }
}

fn renderDocComment(w: Writer, ctx: *const RenderContext, doc: []const u8) !void {
    var pos: usize = 0;

    while (true) {
        const fence_pos = findCodeFence(doc, pos) orelse {
            // No more code fences; render the rest as paragraphs
            try renderParagraphs(w, ctx, doc[pos..]);
            break;
        };

        // Render text before the code fence as paragraphs
        if (fence_pos > pos) {
            try renderParagraphs(w, ctx, doc[pos..fence_pos]);
        }

        // Skip past the opening fence line (```roc, ```, etc.)
        pos = skipLine(doc, fence_pos);

        // Find the closing fence
        const close_pos = findCodeFence(doc, pos) orelse {
            // Unclosed fence; render the rest as a code block
            const code = std.mem.trimRight(u8, doc[pos..], "\n\r");
            if (code.len > 0) {
                try w.writeAll("                <pre><code>");
                try writeHtmlEscaped(w, code);
                try w.writeAll("</code></pre>\n");
            }
            break;
        };

        // Render the code block content
        const code = std.mem.trimRight(u8, doc[pos..close_pos], "\n\r");
        if (code.len > 0) {
            try w.writeAll("                <pre><code>");
            try writeHtmlEscaped(w, code);
            try w.writeAll("</code></pre>\n");
        }

        // Skip past the closing fence line
        pos = skipLine(doc, close_pos);
    }
}

/// Returns the byte position of the next ``` that starts at a line boundary,
/// searching from `start`. Returns null if none is found.
fn findCodeFence(doc: []const u8, start: usize) ?usize {
    var i = start;
    // Ensure we begin at a line boundary; if not, advance to the next one
    if (i > 0 and (i >= doc.len or doc[i - 1] != '\n')) {
        while (i < doc.len and doc[i] != '\n') i += 1;
        if (i < doc.len) i += 1;
    }
    while (i + 2 < doc.len) {
        if (doc[i] == '`' and doc[i + 1] == '`' and doc[i + 2] == '`') {
            return i;
        }
        // Advance to the next line
        while (i < doc.len and doc[i] != '\n') i += 1;
        if (i < doc.len) i += 1;
    }
    return null;
}

/// Returns the position right after the newline that ends the line starting at `pos`.
fn skipLine(doc: []const u8, pos: usize) usize {
    var i = pos;
    while (i < doc.len and doc[i] != '\n') i += 1;
    if (i < doc.len) i += 1;
    return i;
}

/// Splits `text` on blank lines and emits each non-empty paragraph as a `<p>` element.
fn renderParagraphs(w: Writer, ctx: *const RenderContext, text: []const u8) !void {
    var start: usize = 0;
    var i: usize = 0;
    while (i < text.len) {
        if (i + 1 < text.len and text[i] == '\n' and text[i + 1] == '\n') {
            // Found a blank line — emit the accumulated paragraph
            const para = std.mem.trim(u8, text[start..i], " \t\n\r");
            if (para.len > 0) {
                try w.writeAll("                <p>");
                try writeDocText(w, ctx, para);
                try w.writeAll("</p>\n");
            }
            // Skip past all consecutive blank lines
            while (i < text.len and (text[i] == '\n' or text[i] == '\r')) {
                i += 1;
            }
            start = i;
        } else {
            i += 1;
        }
    }
    // Final paragraph (no trailing blank line)
    const para = std.mem.trim(u8, text[start..], " \t\n\r");
    if (para.len > 0) {
        try w.writeAll("                <p>");
        try writeDocText(w, ctx, para);
        try w.writeAll("</p>\n");
    }
}

/// Writes HTML-escaped text, rendering `inline code` spans as <code> elements,
/// [label](url) markdown links as <a href> elements, and [ref] shorthand links
/// to other doc entries (e.g. [Str], [Str.reserve]) as same-page or cross-module
/// anchors.
fn writeDocText(w: Writer, ctx: *const RenderContext, text: []const u8) !void {
    var i: usize = 0;
    var plain_start: usize = 0;
    while (i < text.len) {
        if (text[i] == '`') {
            // Flush any plain text accumulated before this backtick
            try writeHtmlEscaped(w, text[plain_start..i]);
            i += 1;
            // Find the closing backtick
            const code_start = i;
            while (i < text.len and text[i] != '`') i += 1;
            if (i < text.len) {
                // Found closing backtick — render as <code>
                try w.writeAll("<code>");
                try writeHtmlEscaped(w, text[code_start..i]);
                try w.writeAll("</code>");
                i += 1; // skip closing backtick
            } else {
                // No closing backtick — treat the opening backtick as literal text
                try writeHtmlEscaped(w, text[code_start - 1 .. i]);
            }
            plain_start = i;
        } else if (text[i] == '[') {
            if (parseMarkdownLink(text, i)) |link| {
                try writeHtmlEscaped(w, text[plain_start..i]);
                try w.writeAll("<a href=\"");
                try writeHtmlEscaped(w, link.url);
                try w.writeAll("\">");
                // Recurse so inline code inside the label still renders
                try writeDocText(w, ctx, link.label);
                try w.writeAll("</a>");
                i = link.end;
                plain_start = i;
            } else if (parseDocRef(text, i)) |ref| {
                try writeHtmlEscaped(w, text[plain_start..i]);
                try w.writeAll("<a href=\"");
                try writeDocRefHref(w, ctx, ref.label);
                try w.writeAll("\">");
                try writeHtmlEscaped(w, ref.label);
                try w.writeAll("</a>");
                i = ref.end;
                plain_start = i;
            } else {
                i += 1;
            }
        } else {
            i += 1;
        }
    }
    // Flush any remaining plain text
    try writeHtmlEscaped(w, text[plain_start..]);
}

const MarkdownLink = struct {
    label: []const u8,
    url: []const u8,
    end: usize,
};

/// Parses a `[label](url)` markdown link starting at `start`, which must point
/// to `[`. Returns null if the pattern doesn't match — in that case the caller
/// should treat the `[` as literal text.
fn parseMarkdownLink(text: []const u8, start: usize) ?MarkdownLink {
    std.debug.assert(text[start] == '[');
    var j = start + 1;
    while (j < text.len and text[j] != ']') {
        if (text[j] == '\n') return null;
        j += 1;
    }
    if (j >= text.len) return null;
    const label_end = j;
    if (label_end + 1 >= text.len or text[label_end + 1] != '(') return null;
    // Allow balanced parentheses inside the URL (e.g. a trailing ')' in a
    // Wikipedia link like `Union_(set_theory)`): only a `)` at depth 0
    // terminates the destination.
    var k = label_end + 2;
    var depth: usize = 0;
    while (k < text.len) : (k += 1) {
        const c = text[k];
        if (c == '\n') return null;
        if (c == '(') {
            depth += 1;
        } else if (c == ')') {
            if (depth == 0) break;
            depth -= 1;
        }
    }
    if (k >= text.len) return null;
    return .{
        .label = text[start + 1 .. label_end],
        .url = text[label_end + 2 .. k],
        .end = k + 1,
    };
}

const DocRef = struct {
    label: []const u8,
    end: usize,
};

/// Parses a shorthand `[Name]` or `[Name.member]` reference to another doc
/// entry. The label must be a (possibly dotted) identifier, and the closing
/// bracket must NOT be followed by `(` — that case is handled by
/// `parseMarkdownLink`. Returns null otherwise.
fn parseDocRef(text: []const u8, start: usize) ?DocRef {
    std.debug.assert(text[start] == '[');
    const label_start = start + 1;
    if (label_start >= text.len) return null;
    if (!isIdentStart(text[label_start])) return null;
    var j = label_start + 1;
    while (j < text.len and text[j] != ']') {
        const c = text[j];
        if (c == '.') {
            // A dot must be followed by an identifier start char
            if (j + 1 >= text.len or !isIdentStart(text[j + 1])) return null;
        } else if (!isIdentCont(c)) {
            return null;
        }
        j += 1;
    }
    if (j >= text.len) return null;
    // Reject if followed by '(' — that's a markdown link, handled elsewhere.
    if (j + 1 < text.len and text[j + 1] == '(') return null;
    return .{
        .label = text[label_start..j],
        .end = j + 1,
    };
}

fn isIdentStart(c: u8) bool {
    return (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z') or c == '_';
}

fn isIdentCont(c: u8) bool {
    return isIdentStart(c) or (c >= '0' and c <= '9');
}

/// Writes the href for a shorthand doc reference like `Str` or `Str.reserve`.
/// If the label's first segment names a known module, the link points at that
/// module's page; otherwise it falls back to an anchor on the current page.
fn writeDocRefHref(w: Writer, ctx: *const RenderContext, label: []const u8) !void {
    const first_dot = std.mem.indexOfScalar(u8, label, '.');
    const head = if (first_dot) |d| label[0..d] else label;

    if (ctx.known_modules.contains(head)) {
        const is_same_module = if (ctx.current_module) |cur|
            std.mem.eql(u8, head, cur)
        else
            false;

        if (is_same_module) {
            // Same-page anchor. Entry ids are prefixed with the module name.
            try w.writeAll("#");
            try writeHtmlEscaped(w, head);
            if (first_dot) |d| {
                try writeHtmlEscaped(w, label[d..]);
            }
            return;
        }

        // Cross-module link.
        if (ctx.current_module) |_| {
            try w.writeAll("../");
        }
        try writeHtmlEscaped(w, head);
        try w.writeAll("/");
        if (first_dot != null) {
            try w.writeAll("#");
            try writeHtmlEscaped(w, label);
        }
        return;
    }

    // Not a known module — treat as a reference relative to the current
    // module. Entry ids are `<module>.<label>`.
    try w.writeAll("#");
    if (ctx.current_module) |cur| {
        try writeHtmlEscaped(w, cur);
        try w.writeAll(".");
    }
    try writeHtmlEscaped(w, label);
}

fn renderDocTypeHtml(w: Writer, ctx: *const RenderContext, doc_type: *const DocType, needs_parens: bool) !void {
    switch (doc_type.*) {
        .type_ref => |ref| {
            if (!ctx.suppress_type_links and resolveTypeLink(ctx, ref.module_path)) {
                try w.writeAll("<a href=\"");
                try writeTypeLink(w, ctx, ref.module_path, ref.type_name);
                try w.writeAll("\">");
                try w.writeAll("<span class=\"type\">");
                // Display only the last component of the type name
                const display_name = if (std.mem.lastIndexOfScalar(u8, ref.type_name, '.')) |idx|
                    ref.type_name[idx + 1 ..]
                else
                    ref.type_name;
                try writeHtmlEscaped(w, display_name);
                try w.writeAll("</span></a>");
            } else {
                try w.writeAll("<span class=\"type\">");
                // Display only the last component of the type name
                const display_name = if (std.mem.lastIndexOfScalar(u8, ref.type_name, '.')) |idx|
                    ref.type_name[idx + 1 ..]
                else
                    ref.type_name;
                try writeHtmlEscaped(w, display_name);
                try w.writeAll("</span>");
            }
        },
        .type_var => |name| {
            try w.writeAll("<span class=\"type-var\">");
            try writeHtmlEscaped(w, name);
            try w.writeAll("</span>");
        },
        .function => |func| {
            if (needs_parens) try w.writeAll("(");
            for (func.args, 0..) |arg, i| {
                if (i > 0) try w.writeAll(", ");
                try renderDocTypeHtml(w, ctx, arg, true);
            }
            if (func.effectful) {
                try w.writeAll(" =&gt; ");
            } else {
                try w.writeAll(" -&gt; ");
            }
            try renderDocTypeHtml(w, ctx, func.ret, false);
            if (needs_parens) try w.writeAll(")");
        },
        .record => |rec| {
            try w.writeAll("{ ");
            if (rec.is_open) {
                try w.writeAll("..");
                if (rec.ext) |ext| {
                    try renderDocTypeHtml(w, ctx, ext, false);
                }
                if (rec.fields.len > 0) try w.writeAll(", ");
            }
            for (rec.fields, 0..) |field, i| {
                if (i > 0) try w.writeAll(", ");
                try writeHtmlEscaped(w, field.name);
                try w.writeAll(" : ");
                try renderDocTypeHtml(w, ctx, field.type, false);
            }
            try w.writeAll(" }");
        },
        .tag_union => |tu| {
            try w.writeAll("[");
            for (tu.tags, 0..) |tag, i| {
                if (i > 0) try w.writeAll(", ");
                try w.writeAll("<span class=\"type\">");
                try writeHtmlEscaped(w, tag.name);
                try w.writeAll("</span>");
                if (tag.args.len > 0) {
                    try w.writeAll("(");
                    for (tag.args, 0..) |arg, j| {
                        if (j > 0) try w.writeAll(", ");
                        try renderDocTypeHtml(w, ctx, arg, false);
                    }
                    try w.writeAll(")");
                }
            }
            if (tu.is_open) {
                if (tu.tags.len > 0) try w.writeAll(", ");
                try w.writeAll("..");
                if (tu.ext) |ext| {
                    try renderDocTypeHtml(w, ctx, ext, false);
                }
            }
            try w.writeAll("]");
        },
        .tuple => |tup| {
            try w.writeAll("(");
            for (tup.elems, 0..) |elem, i| {
                if (i > 0) try w.writeAll(", ");
                try renderDocTypeHtml(w, ctx, elem, false);
            }
            try w.writeAll(")");
        },
        .apply => |app| {
            try renderDocTypeHtml(w, ctx, app.constructor, false);
            try w.writeAll("(");
            for (app.args, 0..) |arg, i| {
                if (i > 0) try w.writeAll(", ");
                try renderDocTypeHtml(w, ctx, arg, false);
            }
            try w.writeAll(")");
        },
        .where_clause => |wc| {
            try renderDocTypeHtml(w, ctx, wc.type, needs_parens);
            try w.writeAll(" <span class=\"kw\">where</span> { ");
            for (wc.constraints, 0..) |constraint, i| {
                if (i > 0) try w.writeAll(", ");
                try w.writeAll("<span class=\"type-var\">");
                try writeHtmlEscaped(w, constraint.type_var);
                try w.writeAll("</span>.");
                try writeHtmlEscaped(w, constraint.method_name);
                try w.writeAll(" : ");
                try renderDocTypeHtml(w, ctx, constraint.signature, false);
            }
            try w.writeAll(" }");
        },
        .wildcard => {
            try w.writeAll("_");
        },
        .@"error" => {
            try w.writeAll("?");
        },
    }
}

/// Resolve a short type name to its full path within current module
/// For example, "Dec" -> "Num.Dec"
fn resolveTypeNameToFullPath(
    ctx: *const RenderContext,
    type_name: []const u8,
) ?[]const u8 {
    // If it already has a dot, it's a full path
    if (std.mem.indexOf(u8, type_name, ".") != null) {
        return type_name;
    }

    // Search current module entries for a match
    if (ctx.current_module_entries) |entries| {
        for (entries) |*entry| {
            // Check if entry.name ends with ".{type_name}"
            // This handles cases like "Num.Dec" where type_name is "Dec"
            if (std.mem.endsWith(u8, entry.name, type_name)) {
                const dot_pos = entry.name.len - type_name.len;
                if (dot_pos == 0) {
                    // Exact match (top-level type like "Bool")
                    return type_name;
                } else if (dot_pos > 0 and entry.name[dot_pos - 1] == '.') {
                    // Match after a dot (nested type like "Num.Dec")
                    return entry.name;
                }
            }
        }
    }

    // Default to original name if not found
    return type_name;
}

/// Check whether a type reference is linkable.
fn resolveTypeLink(
    ctx: *const RenderContext,
    module_path: []const u8,
) bool {
    if (module_path.len == 0) return true; // anchor in current page
    if (ctx.current_module) |cur| {
        if (std.mem.eql(u8, module_path, cur)) return true;
    }
    if (ctx.known_modules.contains(module_path)) return true;
    return false;
}

/// Write the href value for a type link.
fn writeTypeLink(
    w: Writer,
    ctx: *const RenderContext,
    module_path: []const u8,
    type_name: []const u8,
) !void {
    // Determine the target module
    const target_module = if (module_path.len > 0)
        module_path
    else if (ctx.current_module) |cur|
        cur
    else
        "";

    if (target_module.len == 0) {
        // Fallback: unqualified anchor (shouldn't happen)
        try w.writeAll("#");
        try writeHtmlEscaped(w, type_name);
        return;
    }

    // Check if same module
    const is_same_module = if (ctx.current_module) |cur|
        std.mem.eql(u8, target_module, cur)
    else
        false;

    // Resolve the full path for short names (e.g., "Dec" -> "Num.Dec")
    const full_type_name = if (is_same_module)
        resolveTypeNameToFullPath(ctx, type_name) orelse type_name
    else
        type_name;

    if (is_same_module) {
        // Same-page link with anchor to the entry's full_path
        // The entry IDs are the type name path (e.g., "Num.Dec")
        try w.writeAll("#");
        try writeHtmlEscaped(w, full_type_name);
    } else {
        // Cross-module link with relative path
        // If we're in a module page (current_module is set), use "../" prefix
        if (ctx.current_module) |_| {
            try w.writeAll("../");
        }
        try writeHtmlEscaped(w, target_module);
        try w.writeAll("/#");
        try writeHtmlEscaped(w, full_type_name);
    }
}

fn writeHtmlEscaped(w: Writer, text: []const u8) !void {
    for (text) |c| {
        switch (c) {
            '<' => try w.writeAll("&lt;"),
            '>' => try w.writeAll("&gt;"),
            '&' => try w.writeAll("&amp;"),
            '"' => try w.writeAll("&quot;"),
            '\'' => try w.writeAll("&#39;"),
            else => try w.writeAll(&[_]u8{c}),
        }
    }
}
