//! Optional tab-separated search metadata. All text is HTML-escaped before output.
const std = @import("std");
const reporting = @import("reporting");
const Allocator = std.mem.Allocator;

/// Invalid metadata, unreadable input, or failures writing the generated index.
pub const Error = Allocator.Error || std.Io.Dir.ReadFileAllocError || std.Io.Dir.OpenError || std.Io.Dir.WriteFileError || std.Io.Writer.Error || error{
    InvalidSearchConfig,
    InvalidSearchUrl,
    InvalidUtf8,
    UnknownSearchCategory,
    TermTargetMustBeRelative,
    UnknownSearchAnchor,
};

/// Location of a configuration failure; zero means a file-level failure.
pub const Diagnostic = struct {
    line: usize = 0,
};

/// Append configured categories, documents, and curated terms to the generated index.
/// Relative source files are resolved against the configuration file's directory.
pub fn apply(gpa: Allocator, io: std.Io, output: []const u8, config_path: []const u8, diagnostic: *Diagnostic) Error!void {
    diagnostic.* = .{};
    const config = try std.Io.Dir.cwd().readFileAlloc(io, config_path, gpa, .limited(8 * 1024 * 1024));
    defer gpa.free(config);
    if (!std.unicode.utf8ValidateSlice(config)) return error.InvalidUtf8;
    var directory = try std.Io.Dir.cwd().openDir(io, output, .{});
    defer directory.close(io);
    const original = try directory.readFileAlloc(io, "search-index.html", gpa, .limited(64 * 1024 * 1024));
    defer gpa.free(original);
    var html: std.Io.Writer.Allocating = .init(gpa);
    defer html.deinit();
    const w = &html.writer;
    try w.writeAll(original);
    var categories = std.StringHashMap(void).init(gpa);
    defer categories.deinit();
    try categories.put("api", {});
    try categories.put("guides", {});
    var lines = std.mem.splitScalar(u8, config, '\n');
    // Categories are declared first so documents can be validated regardless of order.
    while (lines.next()) |raw| {
        const line = std.mem.trim(u8, raw, "\r");
        if (line.len == 0 or line[0] == '#') continue;
        var fields = std.mem.splitScalar(u8, line, '\t');
        if (std.mem.eql(u8, fields.next().?, "category")) {
            // Malformed records are diagnosed in the second pass, with a line number.
            if (fields.next()) |id| {
                if (id.len > 0) try categories.put(id, {});
            }
        }
    }
    lines.reset();
    while (lines.next()) |raw| {
        diagnostic.line += 1;
        const line = std.mem.trim(u8, raw, "\r");
        if (line.len == 0 or line[0] == '#') continue;
        try renderLine(gpa, io, directory, config_path, w, &categories, line);
    }
    diagnostic.line = 0;
    try directory.writeFile(io, .{ .sub_path = "search-index.html", .data = html.written() });
}

/// Escape both attribute values and document text.
fn escaped(w: *std.Io.Writer, text: []const u8) std.Io.Writer.Error!void {
    try reporting.writeEscapedHtml(w, text);
}

/// Validate one TSV record and append its escaped index markup.
fn renderLine(gpa: Allocator, io: std.Io, directory: std.Io.Dir, config_path: []const u8, w: *std.Io.Writer, categories: *std.StringHashMap(void), line: []const u8) Error!void {
    var fields: [5][]const u8 = undefined;
    var count: usize = 0;
    var split = std.mem.splitScalar(u8, line, '\t');
    while (split.next()) |field| {
        if (count == fields.len or field.len == 0) return error.InvalidSearchConfig;
        fields[count] = field;
        count += 1;
    }
    if (std.mem.eql(u8, fields[0], "all") and count == 2) {
        try w.writeAll("<span data-search-all-label>");
        try escaped(w, fields[1]);
        try w.writeAll("</span>\n");
    } else if (std.mem.eql(u8, fields[0], "suggestion") and count == 3) {
        try w.writeAll("<span data-search-suggestion=\"");
        try escaped(w, fields[1]);
        try w.writeAll("\">");
        try escaped(w, fields[2]);
        try w.writeAll("</span>\n");
    } else if (std.mem.eql(u8, fields[0], "category") and count == 3) {
        try w.writeAll("<nav data-search-categories><span data-category=\"");
        try escaped(w, fields[1]);
        try w.writeAll("\">");
        try escaped(w, fields[2]);
        try w.writeAll("</span></nav>\n");
    } else if (std.mem.eql(u8, fields[0], "document") and count == 5) {
        if (!categories.contains(fields[1])) return error.UnknownSearchCategory;
        try validateUrl(fields[3]);
        const source_path = try std.fs.path.resolve(gpa, &.{ std.fs.path.dirname(config_path) orelse ".", fields[4] });
        defer gpa.free(source_path);
        const source = try std.Io.Dir.cwd().readFileAlloc(io, source_path, gpa, .limited(8 * 1024 * 1024));
        defer gpa.free(source);
        if (!std.unicode.utf8ValidateSlice(source)) return error.InvalidUtf8;
        try w.writeAll("<article data-full-navigation data-category=\"");
        try escaped(w, fields[1]);
        try w.writeAll("\" data-title=\"");
        try escaped(w, fields[2]);
        try w.writeAll("\" data-href=\"");
        try escaped(w, fields[3]);
        try w.writeAll("\"><p>");
        try escaped(w, source);
        try w.writeAll("</p></article>\n");
    } else if (std.mem.eql(u8, fields[0], "term") and count == 5) {
        if (!categories.contains(fields[1])) return error.UnknownSearchCategory;
        try validateUrl(fields[3]);
        // Curated terms point into this generated site. Verify both page and anchor.
        try validateTarget(gpa, io, directory, fields[3]);
        try w.writeAll("<span data-category=\"");
        try escaped(w, fields[1]);
        try w.writeAll("\" data-search-term=\"");
        try escaped(w, fields[2]);
        try w.writeAll("\" data-href=\"");
        try escaped(w, fields[3]);
        try w.writeAll("\">");
        try escaped(w, fields[4]);
        try w.writeAll("</span>\n");
    } else return error.InvalidSearchConfig;
}

/// Require a same-site URL without browser-normalized whitespace or separators.
fn validateUrl(url: []const u8) error{InvalidSearchUrl}!void {
    if (url.len == 0) return error.InvalidSearchUrl;
    for (url) |byte| {
        if (byte <= 0x20 or byte == 0x7f) return error.InvalidSearchUrl;
    }
    // Search destinations are same-site paths; never executable schemes or network URLs.
    if (std.mem.findScalar(u8, url, ':') != null or std.mem.startsWith(u8, url, "//") or
        std.mem.findScalar(u8, url, '\\') != null) return error.InvalidSearchUrl;
}

/// Check that a relative destination and optional anchor exist in generated output.
fn validateTarget(gpa: Allocator, io: std.Io, dir: std.Io.Dir, url: []const u8) Error!void {
    const hash = std.mem.findScalar(u8, url, '#') orelse url.len;
    const page = url[0..hash];
    if (std.mem.startsWith(u8, page, "/") or std.mem.find(u8, page, "..") != null or std.mem.findScalar(u8, page, '?') != null) return error.TermTargetMustBeRelative;
    const path = if (page.len == 0 or std.mem.endsWith(u8, page, "/"))
        try std.fmt.allocPrint(gpa, "{s}index.html", .{page})
    else
        try gpa.dupe(u8, page);
    defer gpa.free(path);
    const content = try dir.readFileAlloc(io, path, gpa, .limited(64 * 1024 * 1024));
    defer gpa.free(content);
    if (hash < url.len) {
        var anchor: std.Io.Writer.Allocating = .init(gpa);
        defer anchor.deinit();
        try anchor.writer.writeAll("id=\"");
        try escaped(&anchor.writer, url[hash + 1 ..]);
        try anchor.writer.writeAll("\"");
        if (std.mem.find(u8, content, anchor.written()) == null) return error.UnknownSearchAnchor;
    }
}

test "search destinations reject executable and network URLs" {
    try validateUrl("langref/types/#nominal-types");
    try std.testing.expectError(error.InvalidSearchUrl, validateUrl("javascript:alert(1)"));
    try std.testing.expectError(error.InvalidSearchUrl, validateUrl("//example.com"));
}

test "search config indexes relative sources and validates curated anchors" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(path);
    const config_path = try std.fs.path.join(gpa, &.{ path, "search.tsv" });
    defer gpa.free(config_path);
    try tmp.dir.writeFile(io, .{ .sub_path = "search-index.html", .data = "<meta charset=\"utf-8\">" });
    try tmp.dir.writeFile(io, .{ .sub_path = "index.html", .data = "<h2 id=\"fold\">fold</h2>" });
    try tmp.dir.writeFile(io, .{ .sub_path = "example.roc", .data = "value = \"<script> & text\"" });
    try tmp.dir.writeFile(io, .{
        .sub_path = "search.tsv",
        .data = "all\tEverything\n" ++
            "document\trecipes\tExample\t/examples/demo/\texample.roc\n" ++
            "category\trecipes\tRecipes & examples\n" ++
            "suggestion\t|>\tLook up <syntax>\n" ++
            "term\tapi\tlreduce\t#fold\tfold\n",
    });
    var diagnostic: Diagnostic = .{};
    try apply(gpa, io, path, config_path, &diagnostic);
    const index = try tmp.dir.readFileAlloc(io, "search-index.html", gpa, .limited(4096));
    defer gpa.free(index);
    try std.testing.expect(std.mem.find(u8, index, "Recipes &amp; examples") != null);
    try std.testing.expect(std.mem.find(u8, index, "<article data-full-navigation") != null);
    try std.testing.expect(std.mem.find(u8, index, "&lt;script&gt; &amp; text") != null);
    try std.testing.expect(std.mem.find(u8, index, "data-search-term=\"lreduce\"") != null);
    try std.testing.expect(std.mem.find(u8, index, "data-search-suggestion=\"|&gt;\">Look up &lt;syntax&gt;") != null);
    try std.testing.expectError(error.UnknownSearchAnchor, validateTarget(gpa, io, tmp.dir, "#missing"));
    try std.testing.expectError(error.TermTargetMustBeRelative, validateTarget(gpa, io, tmp.dir, "../index.html"));
    try tmp.dir.writeFile(io, .{ .sub_path = "search.tsv", .data = "term\tunknown\tx\t#fold\tfold\n" });
    try std.testing.expectError(error.UnknownSearchCategory, apply(gpa, io, path, config_path, &diagnostic));
    try std.testing.expectEqual(@as(usize, 1), diagnostic.line);
}

test "search config failures leave the index unchanged and identify the record" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(path);
    const config_path = try std.fs.path.join(gpa, &.{ path, "search.tsv" });
    defer gpa.free(config_path);
    const original = "<meta charset=\"utf-8\">";
    try tmp.dir.writeFile(io, .{ .sub_path = "search-index.html", .data = original });
    try tmp.dir.writeFile(io, .{ .sub_path = "index.html", .data = "<h2 id=\"fold\">fold</h2>" });
    const cases = .{
        .{ "suggestion\tquery", error.InvalidSearchConfig },
        .{ "suggestion\t\tDescription", error.InvalidSearchConfig },
        .{ "suggestion\tquery\tDescription\textra", error.InvalidSearchConfig },
        .{ "category", error.InvalidSearchConfig },
        .{ "category\t\tEmpty", error.InvalidSearchConfig },
        .{ "all\tEverything\textra", error.InvalidSearchConfig },
        .{ "term\tmissing\tlreduce\t#fold\tfold", error.UnknownSearchCategory },
        .{ "term\tapi\tlreduce\t#missing\tfold", error.UnknownSearchAnchor },
        .{ "term\tapi\tlreduce\tmissing/\tfold", error.FileNotFound },
        .{ "term\tapi\tlreduce\t../index.html\tfold", error.TermTargetMustBeRelative },
        .{ "document\tguides\tExample\t //example.com\tmissing.roc", error.InvalidSearchUrl },
        .{ "document\tguides\tExample\t/examples/\tmissing.roc", error.FileNotFound },
    };
    inline for (cases) |case| {
        try tmp.dir.writeFile(io, .{ .sub_path = "search.tsv", .data = "# comment\nall\tEverything\n" ++ case[0] ++ "\n" });
        var diagnostic: Diagnostic = .{};
        try std.testing.expectError(case[1], apply(gpa, io, path, config_path, &diagnostic));
        try std.testing.expectEqual(@as(usize, 3), diagnostic.line);
        const index = try tmp.dir.readFileAlloc(io, "search-index.html", gpa, .limited(4096));
        defer gpa.free(index);
        try std.testing.expectEqualStrings(original, index);
    }
}
