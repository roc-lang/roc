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
const embedded_font = @embedFile("static/RocMono.woff2");

const font_face_css =
    \\@font-face {
    \\    font-family: 'RocMono';
    \\    src: url('RocMono.woff2') format('woff2');
    \\    font-weight: normal;
    \\    font-style: normal;
    \\}
    \\
    \\
;

const Writer = *std.Io.Writer;

/// Context for rendering, shared across all pages.
const RenderContext = struct {
    package_docs: *const DocModel.PackageDocs,
    known_modules: std.StringHashMapUnmanaged(void),
    current_module: ?[]const u8,

    fn init(package_docs: *const DocModel.PackageDocs, gpa: Allocator) RenderContext {
        var known = std.StringHashMapUnmanaged(void){};
        for (package_docs.modules) |mod| {
            known.put(gpa, mod.name, {}) catch {};
        }
        return .{
            .package_docs = package_docs,
            .known_modules = known,
            .current_module = null,
        };
    }

    fn deinit(self: *RenderContext, gpa: Allocator) void {
        self.known_modules.deinit(gpa);
    }
};

// ── Public entry point ──────────────────────────────────────────────

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

    // Write package index page
    try writePackageIndex(&ctx, output_dir);

    // Write per-module pages
    for (package_docs.modules) |*mod| {
        ctx.current_module = mod.name;
        try writeModulePage(&ctx, output_dir, mod);
    }
    ctx.current_module = null;
}

// ── Static assets ───────────────────────────────────────────────────

fn writeStaticAssets(dir: std.fs.Dir) !void {
    // CSS with @font-face prepended
    {
        const file = try dir.createFile("styles.css", .{});
        defer file.close();
        try file.writeAll(font_face_css);
        try file.writeAll(embedded_css);
    }

    try dir.writeFile(.{ .sub_path = "search.js", .data = embedded_js });
    try dir.writeFile(.{ .sub_path = "RocMono.woff2", .data = embedded_font });
}

// ── Package index page ──────────────────────────────────────────────

fn writePackageIndex(ctx: *const RenderContext, dir: std.fs.Dir) !void {
    const file = try dir.createFile("index.html", .{});
    defer file.close();
    var buf: [4096]u8 = undefined;
    var bw = file.writer(&buf);
    const w = &bw.interface;

    try writeHtmlHead(w, ctx.package_docs.name, "");
    try writeBodyOpen(w);
    try renderSidebar(w, ctx, "");

    // Main content
    try w.writeAll("    <main>\n");
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

// ── Module page ─────────────────────────────────────────────────────

fn writeModulePage(ctx: *const RenderContext, dir: std.fs.Dir, mod: *const DocModel.ModuleDocs) !void {
    // Create module subdirectory
    dir.makeDir(mod.name) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var sub_dir = try dir.openDir(mod.name, .{});
    defer sub_dir.close();

    const file = try sub_dir.createFile("index.html", .{});
    defer file.close();
    var buf: [4096]u8 = undefined;
    var bw = file.writer(&buf);
    const w = &bw.interface;

    try writeHtmlHead(w, mod.name, "../");
    try writeBodyOpen(w);
    try renderSidebar(w, ctx, "../");

    // Main content
    try w.writeAll("    <main>\n");
    try w.writeAll("        <h1 class=\"module-name\">");
    try writeHtmlEscaped(w, mod.name);
    try w.writeAll("</h1>\n");

    // Module doc comment
    if (mod.module_doc) |doc| {
        try w.writeAll("        <div class=\"module-doc\">\n");
        try renderDocComment(w, doc);
        try w.writeAll("        </div>\n");
    }

    // Entries
    for (mod.entries) |*entry| {
        try renderEntry(w, ctx, entry);
    }

    try writeFooter(w);
    try w.writeAll("    </main>\n");
    try writeBodyClose(w);
    try bw.interface.flush();
}

// ── HTML skeleton helpers ───────────────────────────────────────────

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

fn writeBodyOpen(w: Writer) !void {
    try w.writeAll("<body>\n");
}

fn writeBodyClose(w: Writer) !void {
    try w.writeAll("</body>\n</html>\n");
}

fn writeFooter(w: Writer) !void {
    try w.writeAll("        <footer><p>Made by people who like to make nice things.</p></footer>\n");
}

// ── Sidebar ─────────────────────────────────────────────────────────

const roc_logo_svg =
    \\<svg viewBox="0 -6 51 58" fill="none" xmlns="http://www.w3.org/2000/svg" role="img" aria-labelledby="logo-link">
    \\    <title id="logo-link">Home</title>
    \\    <polygon role="presentation" points="0,0 23.8834,3.21052 37.2438,19.0101 45.9665,16.6324 50.5,22 45,22 44.0315,26.3689 26.4673,39.3424 27.4527,45.2132 17.655,53 23.6751,22.7086" />
    \\</svg>
;

fn renderSidebar(w: Writer, ctx: *const RenderContext, base: []const u8) !void {
    try w.writeAll("    <nav id=\"sidebar-nav\">\n");
    try w.writeAll("        <div class=\"pkg-and-logo\">\n");
    try w.writeAll("            <a class=\"logo\" href=\"");
    try w.writeAll(base);
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
    try w.writeAll("            <div class=\"module-links\">\n");

    for (ctx.package_docs.modules) |mod| {
        const is_active = if (ctx.current_module) |cur|
            std.mem.eql(u8, cur, mod.name)
        else
            false;

        try w.writeAll("                <div class=\"sidebar-entry\">\n");
        try w.writeAll("                    <a class=\"sidebar-module-link");
        if (is_active) try w.writeAll(" active");
        try w.writeAll("\" data-module-name=\"");
        try writeHtmlEscaped(w, mod.name);
        try w.writeAll("\" href=\"");
        try w.writeAll(base);
        try writeHtmlEscaped(w, mod.name);
        try w.writeAll("/\">");
        try w.writeAll("<button class=\"entry-toggle\">&#9656;</button>");
        try w.writeAll("<span>");
        try writeHtmlEscaped(w, mod.name);
        try w.writeAll("</span></a>\n");

        // Sub-entries
        try w.writeAll("                    <div class=\"sidebar-sub-entries\">\n");
        for (mod.entries) |entry| {
            try w.writeAll("                        <a href=\"");
            try w.writeAll(base);
            try writeHtmlEscaped(w, mod.name);
            try w.writeAll("/#");
            try writeHtmlEscaped(w, entry.name);
            try w.writeAll("\">");
            try writeHtmlEscaped(w, entry.name);
            try w.writeAll("</a>\n");
        }
        try w.writeAll("                    </div>\n");
        try w.writeAll("                </div>\n");
    }

    try w.writeAll("            </div>\n");
    try w.writeAll("        </div>\n");
    try w.writeAll("    </nav>\n");
}

// ── Entry rendering ─────────────────────────────────────────────────

fn renderEntry(w: Writer, ctx: *const RenderContext, entry: *const DocModel.DocEntry) !void {
    try w.writeAll("        <section>\n");

    // Heading with anchor
    try w.writeAll("            <h3 id=\"");
    try writeHtmlEscaped(w, entry.name);
    try w.writeAll("\" class=\"entry-name\">");
    try w.writeAll("<a href=\"#");
    try writeHtmlEscaped(w, entry.name);
    try w.writeAll("\">&#128279;</a> ");

    // Type signature
    try w.writeAll("<code>");
    try renderEntrySignature(w, ctx, entry);
    try w.writeAll("</code>");

    try w.writeAll("</h3>\n");

    // Doc comment
    if (entry.doc_comment) |doc| {
        try w.writeAll("            <div class=\"entry-doc\">\n");
        try renderDocComment(w, doc);
        try w.writeAll("            </div>\n");
    }

    // Children (record fields for nominal types)
    if (entry.children.len > 0) {
        try w.writeAll("            <div class=\"entry-children\">\n");
        for (entry.children) |*child| {
            try w.writeAll("                <div class=\"entry-child\"><code>");
            try writeHtmlEscaped(w, child.name);
            try w.writeAll("</code></div>\n");
        }
        try w.writeAll("            </div>\n");
    }

    try w.writeAll("        </section>\n");
}

fn renderEntrySignature(w: Writer, ctx: *const RenderContext, entry: *const DocModel.DocEntry) !void {
    try w.writeAll("<strong>");
    try writeHtmlEscaped(w, entry.name);
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
                try w.writeAll(" :: ");
                try renderDocTypeHtml(w, ctx, sig, false);
            },
        }
    }
}

// ── Doc comment rendering ───────────────────────────────────────────

fn renderDocComment(w: Writer, doc: []const u8) !void {
    // Split on blank lines into <p> tags
    var start: usize = 0;
    var i: usize = 0;
    while (i < doc.len) {
        if (i + 1 < doc.len and doc[i] == '\n' and doc[i + 1] == '\n') {
            // Found blank line — emit paragraph
            const para = std.mem.trim(u8, doc[start..i], " \t\n\r");
            if (para.len > 0) {
                try w.writeAll("                <p>");
                try writeHtmlEscaped(w, para);
                try w.writeAll("</p>\n");
            }
            // Skip past blank lines
            while (i < doc.len and (doc[i] == '\n' or doc[i] == '\r')) {
                i += 1;
            }
            start = i;
        } else {
            i += 1;
        }
    }
    // Final paragraph
    const para = std.mem.trim(u8, doc[start..], " \t\n\r");
    if (para.len > 0) {
        try w.writeAll("                <p>");
        try writeHtmlEscaped(w, para);
        try w.writeAll("</p>\n");
    }
}

// ── DocType → HTML ──────────────────────────────────────────────────

fn renderDocTypeHtml(w: Writer, ctx: *const RenderContext, doc_type: *const DocType, needs_parens: bool) !void {
    switch (doc_type.*) {
        .type_ref => |ref| {
            if (resolveTypeLink(ctx, ref.module_path, ref.type_name)) |_| {
                try w.writeAll("<a href=\"");
                try writeTypeLink(w, ctx, ref.module_path, ref.type_name);
                try w.writeAll("\">");
                try w.writeAll("<span class=\"type\">");
                try writeHtmlEscaped(w, ref.type_name);
                try w.writeAll("</span></a>");
            } else {
                try w.writeAll("<span class=\"type\">");
                try writeHtmlEscaped(w, ref.type_name);
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
            if (rec.ext) |ext| {
                try w.writeAll("..");
                try renderDocTypeHtml(w, ctx, ext, false);
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
            try w.writeAll("]");
            if (tu.ext) |ext| {
                try renderDocTypeHtml(w, ctx, ext, false);
            }
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

// ── Link resolution ─────────────────────────────────────────────────

/// Check whether a type reference is linkable.
fn resolveTypeLink(
    ctx: *const RenderContext,
    module_path: []const u8,
    type_name: []const u8,
) ?bool {
    _ = type_name;
    if (module_path.len == 0) return true; // anchor in current page
    if (ctx.current_module) |cur| {
        if (std.mem.eql(u8, module_path, cur)) return true;
    }
    if (ctx.known_modules.contains(module_path)) return true;
    return null;
}

/// Write the href value for a type link.
fn writeTypeLink(
    w: Writer,
    ctx: *const RenderContext,
    module_path: []const u8,
    type_name: []const u8,
) !void {
    if (module_path.len == 0) {
        // Anchor in current page
        try w.writeAll("#");
        try writeHtmlEscaped(w, type_name);
        return;
    }
    if (ctx.current_module) |cur| {
        if (std.mem.eql(u8, module_path, cur)) {
            try w.writeAll("#");
            try writeHtmlEscaped(w, type_name);
            return;
        }
    }
    // Cross-module link
    try w.writeAll("../");
    try writeHtmlEscaped(w, module_path);
    try w.writeAll("/#");
    try writeHtmlEscaped(w, type_name);
}

// ── HTML escaping ───────────────────────────────────────────────────

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
