//! Markdown -> HTML renderer for the language reference ("langref") articles.
//!
//! The langref lives as a directory of CommonMark-ish `.md` files (see
//! `docs/langref`). When `roc docs --with-lang-ref` is used, those files are
//! read from disk, ordered to match the README outline, and rendered into the
//! generated documentation site using the same page chrome as module pages.
//!
//! This renderer deliberately supports only the Markdown subset the langref
//! actually uses: ATX headings (with GitHub-style anchor slugs), fenced code
//! blocks, ordered/unordered nested lists, GitHub pipe tables, blockquotes,
//! and inline code/emphasis/links. Relative links between articles (e.g.
//! `functions#effectful-functions` or `types.md#nominal-types`) are rewritten
//! to point at the generated article pages.

const std = @import("std");
const Allocator = std.mem.Allocator;

const Writer = *std.Io.Writer;
const RenderError = Allocator.Error || error{WriteFailed};

/// A single language-reference article, sourced from one `.md` file.
pub const Article = struct {
    /// File stem (e.g. "expressions"). README's stem is "README".
    slug: []const u8,
    /// Raw Markdown of the first level-1 heading, used as the page/sidebar
    /// title (may contain inline `code`/_emphasis_). Falls back to the slug
    /// when the file has no `# ` heading.
    title: []const u8,
    /// Filesystem path the article was read from (for diagnostics).
    source_path: []const u8,
    /// Full Markdown contents of the file.
    markdown: []const u8,
    /// True for the README, which becomes the section's landing page
    /// (`langref/index.html`).
    is_index: bool,
};

/// The loaded set of langref articles, ordered for display (README first, then
/// README-outline order, then any remaining files alphabetically). Owns all of
/// its backing allocations.
pub const LangRef = struct {
    articles: []Article,
    gpa: Allocator,

    pub fn deinit(self: *LangRef) void {
        for (self.articles) |a| {
            self.gpa.free(a.slug);
            self.gpa.free(a.title);
            self.gpa.free(a.source_path);
            self.gpa.free(a.markdown);
        }
        self.gpa.free(self.articles);
    }

    /// Returns the article designated as the landing page (README), if any.
    pub fn index(self: *const LangRef) ?*const Article {
        for (self.articles) |*a| {
            if (a.is_index) return a;
        }
        return null;
    }
};

/// Maximum size of a single langref `.md` file we will read.
const max_article_bytes = 8 * 1024 * 1024;

/// Errors that can occur while loading a `LangRef` from disk.
pub const LoadError = std.Io.Dir.OpenError ||
    std.Io.Dir.Iterator.Error ||
    std.Io.Dir.ReadFileAllocError ||
    Allocator.Error ||
    error{LangRefDirEmpty};

/// Reads every `.md` file in `dir_path`, derives titles and ordering, and
/// returns the resulting `LangRef`. The caller owns the result and must call
/// `deinit`. Returns an error if the directory cannot be opened or is empty.
pub fn load(gpa: Allocator, io: std.Io, dir_path: []const u8) LoadError!LangRef {
    var dir = try std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true });
    defer dir.close(io);

    // Collect the articles in filesystem (non-deterministic) order first.
    var raw = std.ArrayList(Article).empty;
    errdefer {
        for (raw.items) |a| {
            gpa.free(a.slug);
            gpa.free(a.title);
            gpa.free(a.source_path);
            gpa.free(a.markdown);
        }
        raw.deinit(gpa);
    }

    var it = dir.iterate();
    while (try it.next(io)) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".md")) continue;

        const full_path = try std.fs.path.join(gpa, &.{ dir_path, entry.name });
        defer gpa.free(full_path);

        const content = try std.Io.Dir.cwd().readFileAlloc(io, full_path, gpa, .limited(max_article_bytes));
        errdefer gpa.free(content);

        const stem = entry.name[0 .. entry.name.len - ".md".len];
        const is_index = std.ascii.eqlIgnoreCase(stem, "README");

        const title_src = firstHeading1(content) orelse stem;

        const slug = try gpa.dupe(u8, stem);
        errdefer gpa.free(slug);
        const title = try gpa.dupe(u8, title_src);
        errdefer gpa.free(title);
        const src = try gpa.dupe(u8, full_path);
        errdefer gpa.free(src);

        try raw.append(gpa, .{
            .slug = slug,
            .title = title,
            .source_path = src,
            .markdown = content,
            .is_index = is_index,
        });
    }

    if (raw.items.len == 0) return error.LangRefDirEmpty;

    const order = try computeOrder(gpa, raw.items);
    defer gpa.free(order);

    const articles = try gpa.alloc(Article, raw.items.len);
    for (order, 0..) |src_idx, dst_idx| {
        articles[dst_idx] = raw.items[src_idx];
    }
    // `articles` now owns every Article's allocations; release the temporary
    // list container without freeing the moved contents.
    raw.deinit(gpa);

    return .{ .articles = articles, .gpa = gpa };
}

/// Builds a display-order permutation of `raw`: the README index first, then
/// articles in the order they are first referenced by the README outline, then
/// any remaining articles alphabetically by slug.
fn computeOrder(gpa: Allocator, raw: []const Article) Allocator.Error![]usize {
    const order = try gpa.alloc(usize, raw.len);
    errdefer gpa.free(order);

    const used = try gpa.alloc(bool, raw.len);
    defer gpa.free(used);
    @memset(used, false);

    var n: usize = 0;

    // 1. README (index) goes first and becomes the landing page.
    var readme_md: ?[]const u8 = null;
    for (raw, 0..) |a, i| {
        if (a.is_index) {
            order[n] = i;
            n += 1;
            used[i] = true;
            readme_md = a.markdown;
            break;
        }
    }

    // 2. Articles in the order their slug is first referenced by the README.
    if (readme_md) |md| {
        var pos: usize = 0;
        while (std.mem.findPos(u8, md, pos, "](")) |open| {
            pos = open + 2;
            const close = std.mem.findScalarPos(u8, md, pos, ')') orelse break;
            const target = md[pos..close];
            pos = close + 1;

            const slug = linkTargetSlug(target) orelse continue;
            for (raw, 0..) |a, i| {
                if (used[i] or a.is_index) continue;
                if (std.mem.eql(u8, a.slug, slug)) {
                    order[n] = i;
                    n += 1;
                    used[i] = true;
                    break;
                }
            }
        }
    }

    // 3. Remaining articles alphabetically by slug.
    while (n < raw.len) {
        var best: ?usize = null;
        for (raw, 0..) |a, i| {
            if (used[i]) continue;
            if (best == null or std.mem.lessThan(u8, a.slug, raw[best.?].slug)) {
                best = i;
            }
        }
        const b = best.?;
        order[n] = b;
        n += 1;
        used[b] = true;
    }

    return order;
}

/// Extracts the article slug a relative Markdown link target refers to, or null
/// for external/absolute/anchor-only links. e.g. "types.md#nominal" -> "types",
/// "pattern-matching" -> "pattern-matching", "#values" -> null,
/// "https://x" -> null.
fn linkTargetSlug(target: []const u8) ?[]const u8 {
    if (target.len == 0) return null;
    if (target[0] == '#' or target[0] == '/') return null;
    if (std.mem.find(u8, target, "://") != null) return null;
    if (std.mem.startsWith(u8, target, "mailto:")) return null;

    var path = target;
    if (std.mem.findScalar(u8, path, '#')) |hash| path = path[0..hash];
    if (std.mem.endsWith(u8, path, ".md")) path = path[0 .. path.len - ".md".len];
    if (path.len == 0) return null;
    return resolveSlugAlias(path);
}

/// Returns the raw text of the first `# ` (level-1) heading, or null.
fn firstHeading1(md: []const u8) ?[]const u8 {
    var lines = std.mem.splitScalar(u8, md, '\n');
    while (lines.next()) |raw_line| {
        const line = std.mem.trimEnd(u8, raw_line, "\r");
        const trimmed = std.mem.trimStart(u8, line, " ");
        if (trimmed.len >= 2 and trimmed[0] == '#' and trimmed[1] == ' ') {
            return splitHeadingAttr(trimmed[2..]).text;
        }
    }
    return null;
}

/// A heading's display text together with an optional explicit anchor id from a
/// trailing `{#custom-id}` attribute (Pandoc/kramdown style).
const HeadingText = struct { text: []const u8, id: ?[]const u8 };

/// Strips surrounding whitespace, a trailing `#` ATX closing sequence, and a
/// trailing `{#id}` attribute. The captured id (when present and well-formed)
/// is returned so the heading can use it verbatim as its anchor.
fn splitHeadingAttr(text: []const u8) HeadingText {
    var t = std.mem.trim(u8, text, " \t");

    var id: ?[]const u8 = null;
    if (t.len >= 4 and t[t.len - 1] == '}') {
        // Find the matching opening brace.
        var open: usize = t.len - 1;
        while (open > 0) : (open -= 1) {
            if (t[open] == '{') break;
        }
        if (t[open] == '{' and open + 1 < t.len - 1 and t[open + 1] == '#') {
            const inner = std.mem.trim(u8, t[open + 2 .. t.len - 1], " \t");
            if (inner.len > 0 and isValidAttrId(inner)) {
                id = inner;
                t = std.mem.trim(u8, t[0..open], " \t");
            }
        }
    }

    // Drop a trailing run of '#'s (optionally space-separated), as in
    // "## Heading ##".
    t = std.mem.trimEnd(u8, t, "#");
    t = std.mem.trim(u8, t, " \t");
    return .{ .text = t, .id = id };
}

/// A `{#id}` attribute id may only contain anchor-safe characters; anything
/// else means the `{…}` is literal heading text, not an attribute.
fn isValidAttrId(id: []const u8) bool {
    for (id) |c| {
        if (!isAsciiAlnum(c) and c != '-' and c != '_') return false;
    }
    return true;
}

// Rendering

const RenderCtx = struct {
    w: Writer,
    gpa: Allocator,
    articles: []const Article,
    slugger: *Slugger,
    /// Relative path from the current page to the `langref/` directory, used to
    /// build links to other articles. Empty for the README landing page (served
    /// at `/langref/`, so siblings are bare slugs); "../" for an article page
    /// (served at `/langref/<slug>/`, so siblings live one level up).
    langref_base: []const u8 = "",
    /// Set once the first level-1 heading is skipped (it is rendered as the
    /// page title by `renderArticleBody`).
    h1_consumed: bool = false,
};

/// Returns the article's title as plain text (inline formatting removed), for
/// use in `<title>` tags and the sidebar. Caller owns the returned slice.
pub fn titlePlainText(gpa: Allocator, article: *const Article) Allocator.Error![]u8 {
    return plainText(gpa, article.title);
}

/// Renders an article's title as inline HTML, preserving inline formatting:
/// `code` spans become `<code>`, emphasis becomes `<em>`/`<strong>`. Used for
/// sidebar links so e.g. the "`if` / `else`" heading shows its keywords as
/// inline code rather than literal backticks.
pub fn renderTitleInline(
    w: Writer,
    gpa: Allocator,
    articles: []const Article,
    article: *const Article,
) RenderError!void {
    var slugger = Slugger.init(gpa);
    defer slugger.deinit();
    var rctx = RenderCtx{
        .w = w,
        .gpa = gpa,
        .articles = articles,
        .slugger = &slugger,
    };
    try renderInline(&rctx, article.title);
}

/// Renders an article's body — everything that goes inside `.main-content`:
/// an `<h1 class="module-name">` title followed by a `<section>` with the
/// converted Markdown.
pub fn renderArticleBody(
    w: Writer,
    gpa: Allocator,
    articles: []const Article,
    article: *const Article,
    langref_base: []const u8,
) RenderError!void {
    var slugger = Slugger.init(gpa);
    defer slugger.deinit();

    var rctx = RenderCtx{
        .w = w,
        .gpa = gpa,
        .articles = articles,
        .slugger = &slugger,
        .langref_base = langref_base,
    };

    // Page title (rendered from the first `# ` heading; the heading itself is
    // skipped while rendering the body so it is not duplicated).
    try w.writeAll("        <h1 class=\"module-name langref-title\">");
    try renderInline(&rctx, article.title);
    try w.writeAll("</h1>\n");

    var lines = std.ArrayList([]const u8).empty;
    defer lines.deinit(gpa);
    try splitLines(gpa, article.markdown, &lines);

    try w.writeAll("        <section class=\"langref-article\">\n");
    try renderBlocks(&rctx, lines.items);
    try w.writeAll("        </section>\n");
}

fn splitLines(gpa: Allocator, text: []const u8, out: *std.ArrayList([]const u8)) Allocator.Error!void {
    var start: usize = 0;
    var i: usize = 0;
    while (i < text.len) : (i += 1) {
        if (text[i] == '\n') {
            try out.append(gpa, std.mem.trimEnd(u8, text[start..i], "\r"));
            start = i + 1;
        }
    }
    if (start < text.len) {
        try out.append(gpa, std.mem.trimEnd(u8, text[start..], "\r"));
    }
}

fn renderBlocks(rctx: *RenderCtx, lines: []const []const u8) RenderError!void {
    var i: usize = 0;
    while (i < lines.len) {
        const line = lines[i];

        if (isBlank(line)) {
            i += 1;
            continue;
        }

        // Fenced code block.
        if (fenceInfo(line)) |fence| {
            i += 1;
            const code_start = i;
            while (i < lines.len and !isFenceClose(lines[i], fence)) : (i += 1) {}
            try renderCodeBlock(rctx, lines[code_start..i]);
            if (i < lines.len) i += 1; // consume closing fence
            continue;
        }

        // ATX heading.
        if (headingInfo(line)) |h| {
            if (h.level == 1 and !rctx.h1_consumed) {
                rctx.h1_consumed = true;
                i += 1;
                continue;
            }
            try renderHeading(rctx, h.level, h.text, h.id);
            i += 1;
            continue;
        }

        // Blockquote.
        if (isBlockquote(line)) {
            const start = i;
            while (i < lines.len and isBlockquote(lines[i])) : (i += 1) {}
            try renderBlockquote(rctx, lines[start..i]);
            continue;
        }

        // GitHub pipe table (header row followed by a delimiter row).
        if (i + 1 < lines.len and isTableRow(line) and isTableDelimiter(lines[i + 1])) {
            const start = i;
            i += 2;
            while (i < lines.len and isTableRow(lines[i])) : (i += 1) {}
            try renderTable(rctx, lines[start..i]);
            continue;
        }

        // List.
        if (listItemInfo(line) != null) {
            i = try renderList(rctx, lines, i, indentOf(line));
            continue;
        }

        // Paragraph: gather until a blank line or a line that begins another
        // block construct.
        {
            const start = i;
            i += 1;
            while (i < lines.len and !isBlank(lines[i]) and !startsNewBlock(lines, i)) : (i += 1) {}
            try renderParagraph(rctx, lines[start..i]);
            continue;
        }
    }
}

/// Whether the line at `i` begins a block that should interrupt a paragraph.
fn startsNewBlock(lines: []const []const u8, i: usize) bool {
    const line = lines[i];
    if (headingInfo(line) != null) return true;
    if (fenceInfo(line) != null) return true;
    if (isBlockquote(line)) return true;
    if (listItemInfo(line) != null) return true;
    if (i + 1 < lines.len and isTableRow(line) and isTableDelimiter(lines[i + 1])) return true;
    return false;
}

fn renderParagraph(rctx: *RenderCtx, lines: []const []const u8) RenderError!void {
    // Join soft-wrapped lines into a single buffer so inline constructs that
    // span a line break (e.g. a link) still parse.
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(rctx.gpa);
    for (lines, 0..) |ln, idx| {
        if (idx > 0) try buf.append(rctx.gpa, '\n');
        try buf.appendSlice(rctx.gpa, std.mem.trim(u8, ln, " \t"));
    }
    try rctx.w.writeAll("            <p>");
    try renderInline(rctx, buf.items);
    try rctx.w.writeAll("</p>\n");
}

fn renderHeading(rctx: *RenderCtx, level: u8, text: []const u8, explicit_id: ?[]const u8) RenderError!void {
    const w = rctx.w;

    // An explicit `{#id}` attribute wins; otherwise derive a GitHub-style slug
    // from the heading text. Either way the id is recorded with the slugger so
    // later auto-generated slugs don't collide with it.
    var owned_slug: ?[]u8 = null;
    defer if (owned_slug) |s| rctx.gpa.free(s);
    const id: []const u8 = if (explicit_id) |eid| blk: {
        try rctx.slugger.reserve(eid);
        break :blk eid;
    } else blk: {
        const plain = try plainText(rctx.gpa, text);
        defer rctx.gpa.free(plain);
        const slug = try rctx.slugger.makeSlug(plain);
        owned_slug = slug;
        break :blk slug;
    };

    var tag: [3]u8 = .{ 'h', '0' + level, 0 };
    try w.writeAll("            <");
    try w.writeAll(tag[0..2]);
    try w.writeAll(" id=\"");
    try writeEscaped(w, id);
    try w.writeAll("\">");
    try renderInline(rctx, text);
    try w.writeAll("</");
    try w.writeAll(tag[0..2]);
    try w.writeAll(">\n");
}

fn renderCodeBlock(rctx: *RenderCtx, lines: []const []const u8) RenderError!void {
    const w = rctx.w;
    try w.writeAll("            <pre><code>");
    for (lines, 0..) |ln, idx| {
        if (idx > 0) try w.writeAll("\n");
        try writeEscaped(w, ln);
    }
    try w.writeAll("</code></pre>\n");
}

fn renderBlockquote(rctx: *RenderCtx, lines: []const []const u8) RenderError!void {
    const w = rctx.w;

    var inner = std.ArrayList([]const u8).empty;
    defer inner.deinit(rctx.gpa);
    for (lines) |ln| {
        const t = std.mem.trimStart(u8, ln, " ");
        var content = t[1..]; // drop '>'
        if (content.len > 0 and content[0] == ' ') content = content[1..];
        try inner.append(rctx.gpa, content);
    }

    try w.writeAll("            <blockquote>\n");
    try renderBlocks(rctx, inner.items);
    try w.writeAll("            </blockquote>\n");
}

fn renderTable(rctx: *RenderCtx, lines: []const []const u8) RenderError!void {
    const w = rctx.w;
    std.debug.assert(lines.len >= 2);

    try w.writeAll("            <table>\n");

    // Header row.
    try w.writeAll("                <thead><tr>");
    try renderTableCells(rctx, lines[0], "th");
    try w.writeAll("</tr></thead>\n");

    // Body rows (lines[1] is the delimiter; further delimiter-looking rows,
    // as used by the numbers table's grid separators, are skipped).
    try w.writeAll("                <tbody>\n");
    for (lines[2..]) |row| {
        if (isTableDelimiter(row)) continue;
        try w.writeAll("                    <tr>");
        try renderTableCells(rctx, row, "td");
        try w.writeAll("</tr>\n");
    }
    try w.writeAll("                </tbody>\n");
    try w.writeAll("            </table>\n");
}

fn renderTableCells(rctx: *RenderCtx, row: []const u8, comptime cell_tag: []const u8) RenderError!void {
    const w = rctx.w;
    var trimmed = std.mem.trim(u8, row, " \t");
    // Strip the outer pipes if present.
    if (trimmed.len > 0 and trimmed[0] == '|') trimmed = trimmed[1..];
    if (trimmed.len > 0 and trimmed[trimmed.len - 1] == '|') trimmed = trimmed[0 .. trimmed.len - 1];

    var start: usize = 0;
    var i: usize = 0;
    var in_code = false;
    while (i <= trimmed.len) : (i += 1) {
        const at_end = i == trimmed.len;
        if (!at_end and trimmed[i] == '`') in_code = !in_code;
        if (at_end or (trimmed[i] == '|' and !in_code)) {
            const cell = std.mem.trim(u8, trimmed[start..i], " \t");
            try w.writeAll("<" ++ cell_tag ++ ">");
            try renderInline(rctx, cell);
            try w.writeAll("</" ++ cell_tag ++ ">");
            start = i + 1;
        }
    }
}

/// Renders a list starting at `lines[start]` whose items are indented by
/// `list_indent` spaces. Returns the index of the first line after the list.
fn renderList(rctx: *RenderCtx, lines: []const []const u8, start: usize, list_indent: usize) RenderError!usize {
    const w = rctx.w;
    const first = listItemInfo(lines[start]).?;
    const ordered = first.ordered;

    try w.writeAll(if (ordered) "            <ol>\n" else "            <ul>\n");

    var i = start;
    while (i < lines.len) {
        if (isBlank(lines[i])) {
            // A blank line continues the list only if a following line is part
            // of it (same-level item or deeper-indented content).
            const j = nextNonBlank(lines, i + 1) orelse break;
            if (indentOf(lines[j]) < list_indent) break;
            if (listItemInfo(lines[j]) == null and indentOf(lines[j]) <= list_indent) break;
            i = j;
            continue;
        }

        const info = listItemInfo(lines[i]) orelse break;
        if (info.indent < list_indent) break;
        if (info.indent > list_indent) break; // defensive; nested lists handled below
        if (info.ordered != ordered) break; // a different list type starts a new list

        try w.writeAll("                <li>");
        try renderInline(rctx, lines[i][info.content_start..]);
        i += 1;

        // Gather any content belonging to this item: nested lists or
        // continuation lines, all indented deeper than `list_indent`.
        while (i < lines.len) {
            if (isBlank(lines[i])) {
                const j = nextNonBlank(lines, i + 1) orelse break;
                if (indentOf(lines[j]) <= list_indent) break;
                i = j;
                continue;
            }
            const child = listItemInfo(lines[i]);
            if (child != null and child.?.indent > list_indent) {
                try w.writeAll("\n");
                i = try renderList(rctx, lines, i, child.?.indent);
                continue;
            }
            if (child == null and indentOf(lines[i]) > list_indent) {
                // Continuation text for the current item.
                try w.writeAll(" ");
                try renderInline(rctx, std.mem.trimStart(u8, lines[i], " "));
                i += 1;
                continue;
            }
            break;
        }

        try w.writeAll("</li>\n");
    }

    try w.writeAll(if (ordered) "            </ol>\n" else "            </ul>\n");
    return i;
}

fn nextNonBlank(lines: []const []const u8, from: usize) ?usize {
    var j = from;
    while (j < lines.len and isBlank(lines[j])) : (j += 1) {}
    return if (j < lines.len) j else null;
}

// Inline rendering

/// Renders inline Markdown: `code` spans, **strong**/_em_ emphasis, and
/// `[label](url)` links (with relative links rewritten to article pages).
fn renderInline(rctx: *RenderCtx, text: []const u8) RenderError!void {
    const w = rctx.w;
    var i: usize = 0;
    var plain_start: usize = 0;
    while (i < text.len) {
        const c = text[i];
        switch (c) {
            '\\' => {
                // Backslash escapes the next ASCII punctuation char.
                if (i + 1 < text.len and isAsciiPunct(text[i + 1])) {
                    try writeEscaped(w, text[plain_start..i]);
                    try writeEscaped(w, text[i + 1 .. i + 2]);
                    i += 2;
                    plain_start = i;
                } else {
                    i += 1;
                }
            },
            '`' => {
                if (parseCodeSpan(text, i)) |span| {
                    try writeEscaped(w, text[plain_start..i]);
                    try w.writeAll("<code>");
                    try writeEscaped(w, span.content);
                    try w.writeAll("</code>");
                    i = span.end;
                    plain_start = i;
                } else {
                    i += 1;
                }
            },
            '[' => {
                if (parseLink(text, i)) |link| {
                    try writeEscaped(w, text[plain_start..i]);
                    try w.writeAll("<a href=\"");
                    try writeLinkHref(rctx, link.url);
                    try w.writeAll("\">");
                    try renderInline(rctx, link.label);
                    try w.writeAll("</a>");
                    i = link.end;
                    plain_start = i;
                } else {
                    i += 1;
                }
            },
            '*', '_' => {
                if (parseEmphasis(text, i)) |emph| {
                    try writeEscaped(w, text[plain_start..i]);
                    try w.writeAll(if (emph.strong) "<strong>" else "<em>");
                    try renderInline(rctx, emph.content);
                    try w.writeAll(if (emph.strong) "</strong>" else "</em>");
                    i = emph.end;
                    plain_start = i;
                } else {
                    i += 1;
                }
            },
            else => i += 1,
        }
    }
    try writeEscaped(w, text[plain_start..]);
}

const CodeSpan = struct { content: []const u8, end: usize };

/// Parses an inline code span starting at a run of backticks. The closing run
/// must have the same number of backticks. A single leading/trailing space is
/// stripped (CommonMark). Returns null if there is no matching close.
fn parseCodeSpan(text: []const u8, start: usize) ?CodeSpan {
    var run: usize = 0;
    while (start + run < text.len and text[start + run] == '`') run += 1;
    const content_start = start + run;

    var i = content_start;
    while (i < text.len) {
        if (text[i] == '`') {
            var close: usize = 0;
            while (i + close < text.len and text[i + close] == '`') close += 1;
            if (close == run) {
                var content = text[content_start..i];
                if (content.len >= 2 and content[0] == ' ' and content[content.len - 1] == ' ') {
                    content = content[1 .. content.len - 1];
                }
                return .{ .content = content, .end = i + close };
            }
            i += close;
        } else {
            i += 1;
        }
    }
    return null;
}

const Link = struct { label: []const u8, url: []const u8, end: usize };

/// Parses a `[label](url)` link starting at `[`. Returns null when the pattern
/// does not match (caller treats `[` as literal).
fn parseLink(text: []const u8, start: usize) ?Link {
    var j = start + 1;
    var depth: usize = 0;
    while (j < text.len) : (j += 1) {
        const ch = text[j];
        if (ch == '\n') return null;
        if (ch == '[') {
            depth += 1;
        } else if (ch == ']') {
            if (depth == 0) break;
            depth -= 1;
        }
    }
    if (j >= text.len) return null;
    const label_end = j;
    if (label_end + 1 >= text.len or text[label_end + 1] != '(') return null;

    var k = label_end + 2;
    var paren_depth: usize = 0;
    while (k < text.len) : (k += 1) {
        const ch = text[k];
        if (ch == '\n') return null;
        if (ch == '(') {
            paren_depth += 1;
        } else if (ch == ')') {
            if (paren_depth == 0) break;
            paren_depth -= 1;
        }
    }
    if (k >= text.len) return null;
    return .{
        .label = text[start + 1 .. label_end],
        .url = text[label_end + 2 .. k],
        .end = k + 1,
    };
}

const Emphasis = struct { content: []const u8, strong: bool, end: usize };

/// Parses `*em*`, `_em_`, `**strong**`, or `__strong__` starting at `start`.
/// Uses simplified CommonMark flanking rules so intraword underscores
/// (`snake_case`) are not treated as emphasis. Returns null on no match.
fn parseEmphasis(text: []const u8, start: usize) ?Emphasis {
    const delim = text[start];
    var run: usize = 0;
    while (start + run < text.len and text[start + run] == delim) run += 1;
    const strong = run >= 2;
    const marker_len: usize = if (strong) 2 else 1;
    const content_start = start + marker_len;
    if (content_start >= text.len) return null;
    // Opening delimiter must be left-flanking: not followed by whitespace.
    if (isSpace(text[content_start])) return null;
    // Intraword underscore: `_` is not an opener after an alphanumeric char.
    if (delim == '_' and start > 0 and isAsciiAlnum(text[start - 1])) return null;

    var i = content_start;
    while (i < text.len) {
        if (text[i] == delim) {
            var close: usize = 0;
            while (i + close < text.len and text[i + close] == delim) close += 1;
            if (close >= marker_len and i > content_start and !isSpace(text[i - 1])) {
                // Intraword underscore: `_` is not a closer before an alnum.
                const after = i + marker_len;
                if (!(delim == '_' and after < text.len and isAsciiAlnum(text[after]))) {
                    return .{
                        .content = text[content_start..i],
                        .strong = strong,
                        .end = i + marker_len,
                    };
                }
            }
            i += close;
        } else {
            i += 1;
        }
    }
    return null;
}

/// Writes the (HTML-escaped) href for a Markdown link URL, rewriting relative
/// links to other langref articles so they point at the generated pages.
fn writeLinkHref(rctx: *RenderCtx, url: []const u8) RenderError!void {
    const w = rctx.w;
    if (url.len == 0) return;

    // Anchors, absolute paths, and external/scheme URLs pass through unchanged.
    if (url[0] == '#' or url[0] == '/') {
        try writeEscaped(w, url);
        return;
    }
    if (std.mem.find(u8, url, "://") != null or std.mem.startsWith(u8, url, "mailto:")) {
        try writeEscaped(w, url);
        return;
    }

    var path = url;
    var anchor: []const u8 = "";
    if (std.mem.findScalar(u8, url, '#')) |hash| {
        path = url[0..hash];
        anchor = url[hash..]; // includes the '#'
    }
    if (std.mem.endsWith(u8, path, ".md")) path = path[0 .. path.len - ".md".len];

    if (path.len == 0) {
        // Same-page anchor (e.g. "#section" written as a bare fragment).
        try writeEscaped(w, anchor);
        return;
    }

    // Resolve aliases (e.g. `conditionals` -> the `if-else` article).
    path = resolveSlugAlias(path);

    // Links to other articles are resolved relative to the current page via
    // `langref_base` (the path back to the `langref/` directory): empty on the
    // README landing page (served at `/langref/`), "../" on an article page
    // (served at `/langref/<slug>/`). A reference to another article is that
    // base plus its extensionless slug; the README is the `langref/` directory
    // itself ("./" when we are already in it, otherwise `langref_base`).
    if (std.ascii.eqlIgnoreCase(path, "README")) {
        try writeEscaped(w, if (rctx.langref_base.len == 0) "./" else rctx.langref_base);
    } else {
        try writeEscaped(w, rctx.langref_base);
        try writeEscaped(w, path);
    }
    try writeEscaped(w, anchor);
}

/// Maps a link's slug to another article's slug. Used where the langref refers
/// to a topic by a name that does not have its own page, so the link should
/// land on the page that actually covers it.
const SlugAlias = struct { from: []const u8, to: []const u8 };
const slug_aliases = [_]SlugAlias{
    // "Conditionals" is documented on the `if` / `else` page.
    .{ .from = "conditionals", .to = "if-else" },
};

fn resolveSlugAlias(slug: []const u8) []const u8 {
    for (slug_aliases) |alias| {
        if (std.ascii.eqlIgnoreCase(slug, alias.from)) return alias.to;
    }
    return slug;
}

/// Produces the plain-text content of an inline Markdown string (formatting
/// markers removed, link labels kept). Caller owns the returned slice.
fn plainText(gpa: Allocator, text: []const u8) Allocator.Error![]u8 {
    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(gpa);
    var i: usize = 0;
    while (i < text.len) {
        const c = text[i];
        switch (c) {
            '\\' => {
                if (i + 1 < text.len and isAsciiPunct(text[i + 1])) {
                    try out.append(gpa, text[i + 1]);
                    i += 2;
                } else {
                    try out.append(gpa, c);
                    i += 1;
                }
            },
            '`' => {
                if (parseCodeSpan(text, i)) |span| {
                    try out.appendSlice(gpa, span.content);
                    i = span.end;
                } else {
                    i += 1;
                }
            },
            '[' => {
                if (parseLink(text, i)) |link| {
                    const inner = try plainText(gpa, link.label);
                    defer gpa.free(inner);
                    try out.appendSlice(gpa, inner);
                    i = link.end;
                } else {
                    i += 1;
                }
            },
            '*', '_' => {
                if (parseEmphasis(text, i)) |emph| {
                    const inner = try plainText(gpa, emph.content);
                    defer gpa.free(inner);
                    try out.appendSlice(gpa, inner);
                    i = emph.end;
                } else {
                    try out.append(gpa, c);
                    i += 1;
                }
            },
            else => {
                try out.append(gpa, c);
                i += 1;
            },
        }
    }
    return out.toOwnedSlice(gpa);
}

// Block predicates

fn isBlank(line: []const u8) bool {
    return std.mem.trim(u8, line, " \t").len == 0;
}

fn indentOf(line: []const u8) usize {
    var n: usize = 0;
    while (n < line.len and line[n] == ' ') n += 1;
    return n;
}

const Fence = struct { char: u8, count: usize };

fn fenceInfo(line: []const u8) ?Fence {
    const t = std.mem.trimStart(u8, line, " ");
    if (t.len < 3) return null;
    const ch = t[0];
    if (ch != '`' and ch != '~') return null;
    var count: usize = 0;
    while (count < t.len and t[count] == ch) count += 1;
    if (count < 3) return null;
    return .{ .char = ch, .count = count };
}

fn isFenceClose(line: []const u8, fence: Fence) bool {
    const t = std.mem.trim(u8, line, " ");
    if (t.len < fence.count) return false;
    for (t) |ch| {
        if (ch != fence.char) return false;
    }
    return true;
}

const Heading = struct { level: u8, text: []const u8, id: ?[]const u8 };

fn headingInfo(line: []const u8) ?Heading {
    var t = line;
    // Up to 3 leading spaces are allowed before the '#'s.
    var spaces: usize = 0;
    while (spaces < t.len and spaces < 4 and t[spaces] == ' ') spaces += 1;
    if (spaces > 3) return null;
    t = t[spaces..];

    var level: u8 = 0;
    while (level < t.len and t[level] == '#') level += 1;
    if (level == 0 or level > 6) return null;
    if (level >= t.len) return null; // "###" with nothing after is not a heading
    if (t[level] != ' ') return null;

    const attr = splitHeadingAttr(t[level + 1 ..]);
    return .{ .level = level, .text = attr.text, .id = attr.id };
}

fn isBlockquote(line: []const u8) bool {
    const t = std.mem.trimStart(u8, line, " ");
    return t.len > 0 and t[0] == '>';
}

fn isTableRow(line: []const u8) bool {
    if (isBlank(line)) return false;
    return std.mem.findScalar(u8, line, '|') != null;
}

/// A GitHub table delimiter row: only `|`, `-`, `:`, and spaces, with at least
/// one `-`.
fn isTableDelimiter(line: []const u8) bool {
    const t = std.mem.trim(u8, line, " \t");
    if (t.len == 0) return false;
    var seen_dash = false;
    for (t) |ch| {
        switch (ch) {
            '-' => seen_dash = true,
            '|', ':', ' ', '\t' => {},
            else => return false,
        }
    }
    return seen_dash;
}

const ListItem = struct { indent: usize, ordered: bool, content_start: usize };

fn listItemInfo(line: []const u8) ?ListItem {
    const indent = indentOf(line);
    const rest = line[indent..];
    if (rest.len < 2) return null;

    // Unordered: '-', '*', or '+' followed by a space.
    if ((rest[0] == '-' or rest[0] == '*' or rest[0] == '+') and rest[1] == ' ') {
        return .{ .indent = indent, .ordered = false, .content_start = indent + 2 };
    }

    // Ordered: one or more digits, then '.' or ')', then a space.
    var d: usize = 0;
    while (d < rest.len and isAsciiDigit(rest[d])) d += 1;
    if (d > 0 and d + 1 < rest.len and (rest[d] == '.' or rest[d] == ')') and rest[d + 1] == ' ') {
        return .{ .indent = indent, .ordered = true, .content_start = indent + d + 2 };
    }

    return null;
}

// Slugger (GitHub-compatible heading anchors)

/// Generates GitHub-style heading anchor slugs, appending `-1`, `-2`, … to
/// disambiguate repeated slugs within a single document.
const Slugger = struct {
    gpa: Allocator,
    occurrences: std.StringHashMapUnmanaged(u32) = .empty,

    fn init(gpa: Allocator) Slugger {
        return .{ .gpa = gpa };
    }

    fn deinit(self: *Slugger) void {
        var it = self.occurrences.keyIterator();
        while (it.next()) |key| {
            self.gpa.free(key.*);
        }
        self.occurrences.deinit(self.gpa);
    }

    /// Returns an owned slug for `plain`. Caller frees.
    fn makeSlug(self: *Slugger, plain: []const u8) Allocator.Error![]u8 {
        const base = try slugify(self.gpa, plain);
        defer self.gpa.free(base);

        var result = try self.gpa.dupe(u8, base);
        errdefer self.gpa.free(result);

        while (self.occurrences.contains(result)) {
            const gop = try self.occurrences.getOrPut(self.gpa, base);
            if (!gop.found_existing) {
                // `base` should already be present once a collision occurs, but
                // guard against an unexpected state by seeding it.
                gop.key_ptr.* = try self.gpa.dupe(u8, base);
                gop.value_ptr.* = 0;
            }
            gop.value_ptr.* += 1;
            const count = gop.value_ptr.*;
            self.gpa.free(result);
            result = try std.fmt.allocPrint(self.gpa, "{s}-{d}", .{ base, count });
        }

        // Record `result` as seen (with a stable, owned key).
        const gop = try self.occurrences.getOrPut(self.gpa, result);
        if (!gop.found_existing) {
            gop.key_ptr.* = try self.gpa.dupe(u8, result);
            gop.value_ptr.* = 0;
        }

        return result;
    }

    /// Records an author-provided explicit id as seen, so a later
    /// auto-generated slug that would match it gets disambiguated instead.
    fn reserve(self: *Slugger, id: []const u8) Allocator.Error!void {
        const gop = try self.occurrences.getOrPut(self.gpa, id);
        if (!gop.found_existing) {
            gop.key_ptr.* = try self.gpa.dupe(u8, id);
            gop.value_ptr.* = 0;
        }
    }
};

/// GitHub slug rules: lowercase ASCII, drop characters that are not letters,
/// digits, `-`, or `_`, and convert spaces to `-`. Non-ASCII bytes pass
/// through. Surrounding whitespace is trimmed first.
fn slugify(gpa: Allocator, plain: []const u8) Allocator.Error![]u8 {
    const trimmed = std.mem.trim(u8, plain, " \t\r\n");
    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(gpa);
    for (trimmed) |ch| {
        if (ch >= 'A' and ch <= 'Z') {
            try out.append(gpa, ch - 'A' + 'a');
        } else if (isAsciiAlnum(ch) or ch == '-' or ch == '_') {
            try out.append(gpa, ch);
        } else if (ch == ' ' or ch == '\t') {
            try out.append(gpa, '-');
        } else if (ch >= 0x80) {
            try out.append(gpa, ch);
        }
        // else: drop punctuation
    }
    return out.toOwnedSlice(gpa);
}

// Small helpers

fn writeEscaped(w: Writer, text: []const u8) error{WriteFailed}!void {
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

fn isSpace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}

fn isAsciiDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAsciiAlnum(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or isAsciiDigit(c);
}

fn isAsciiPunct(c: u8) bool {
    return switch (c) {
        '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', '\\', ']', '^', '_', '`', '{', '|', '}', '~' => true,
        else => false,
    };
}

// Tests

const testing = std.testing;

fn expectInline(gpa: Allocator, text: []const u8, expected: []const u8) (RenderError || error{TestExpectedEqual})!void {
    var aw = std.Io.Writer.Allocating.init(gpa);
    defer aw.deinit();
    var slugger = Slugger.init(gpa);
    defer slugger.deinit();
    var rctx = RenderCtx{
        .w = &aw.writer,
        .gpa = gpa,
        .articles = &[_]Article{},
        .slugger = &slugger,
    };
    try renderInline(&rctx, text);
    try testing.expectEqualStrings(expected, aw.written());
}

fn expectBody(gpa: Allocator, article: *const Article, needle: []const u8) (RenderError || error{NeedleNotFound})!void {
    var aw = std.Io.Writer.Allocating.init(gpa);
    defer aw.deinit();
    try renderArticleBody(&aw.writer, gpa, &[_]Article{}, article, "");
    if (std.mem.find(u8, aw.written(), needle) == null) {
        std.debug.print("expected body to contain:\n{s}\n--- actual ---\n{s}\n", .{ needle, aw.written() });
        return error.NeedleNotFound;
    }
}

test "slugify follows GitHub rules" {
    const gpa = testing.allocator;

    const cases = [_]struct { in: []const u8, out: []const u8 }{
        .{ .in = "Effectful Functions", .out = "effectful-functions" },
        .{ .in = "if and else", .out = "if-and-else" },
        .{ .in = "and / or", .out = "and--or" },
        .{ .in = "Type Modules", .out = "type-modules" },
        .{ .in = "  Trailing  ", .out = "trailing" },
        .{ .in = "snake_case", .out = "snake_case" },
    };
    for (cases) |c| {
        const got = try slugify(gpa, c.in);
        defer gpa.free(got);
        try testing.expectEqualStrings(c.out, got);
    }
}

test "Slugger disambiguates repeated slugs" {
    const gpa = testing.allocator;
    var slugger = Slugger.init(gpa);
    defer slugger.deinit();

    const a = try slugger.makeSlug("Heading");
    defer gpa.free(a);
    const b = try slugger.makeSlug("Heading");
    defer gpa.free(b);
    const c = try slugger.makeSlug("Heading");
    defer gpa.free(c);

    try testing.expectEqualStrings("heading", a);
    try testing.expectEqualStrings("heading-1", b);
    try testing.expectEqualStrings("heading-2", c);
}

test "renderInline emphasis, code, and escaping" {
    const gpa = testing.allocator;
    try expectInline(gpa, "_total_", "<em>total</em>");
    try expectInline(gpa, "**bold**", "<strong>bold</strong>");
    try expectInline(gpa, "`code`", "<code>code</code>");
    // Intraword underscores are not emphasis.
    try expectInline(gpa, "snake_case_name", "snake_case_name");
    // HTML is escaped.
    try expectInline(gpa, "a < b & c", "a &lt; b &amp; c");
    // Inline code is escaped, not interpreted.
    try expectInline(gpa, "`a -> b`", "<code>a -&gt; b</code>");
}

test "renderInline rewrites relative links and preserves external ones" {
    const gpa = testing.allocator;
    try expectInline(gpa, "[match](pattern-matching#match)", "<a href=\"pattern-matching#match\">match</a>");
    try expectInline(gpa, "[v](types.md#nominal-types)", "<a href=\"types#nominal-types\">v</a>");
    try expectInline(gpa, "[home](README.md)", "<a href=\"./\">home</a>");
    try expectInline(gpa, "[here](#values)", "<a href=\"#values\">here</a>");
    try expectInline(gpa, "[ext](https://roc-lang.org)", "<a href=\"https://roc-lang.org\">ext</a>");
}

test "headingInfo and listItemInfo" {
    try testing.expectEqual(@as(?u8, 2), if (headingInfo("## Hello")) |h| h.level else null);
    try testing.expectEqual(@as(?u8, 4), if (headingInfo("#### Deep")) |h| h.level else null);
    try testing.expect(headingInfo("#NoSpace") == null);
    try testing.expect(headingInfo("Just text") == null);

    try testing.expect(listItemInfo("- item") != null);
    try testing.expect(listItemInfo("  - nested").?.indent == 2);
    try testing.expect(listItemInfo("1. first").?.ordered);
    try testing.expect(listItemInfo("not a list") == null);
}

test "renderArticleBody produces titled, anchored output" {
    const gpa = testing.allocator;
    const md =
        "# Title\n\nHello _world_.\n\n## A Section\n\n- one\n- two\n";
    const article = Article{
        .slug = "demo",
        .title = "Title",
        .source_path = "",
        .markdown = md,
        .is_index = false,
    };
    try expectBody(gpa, &article, "<h1 class=\"module-name langref-title\">Title</h1>");
    try expectBody(gpa, &article, "<h2 id=\"a-section\">A Section</h2>");
    try expectBody(gpa, &article, "<em>world</em>");
    try expectBody(gpa, &article, "<li>one</li>");
    // The first H1 is rendered as the page title, not duplicated in the body.
    try expectBody(gpa, &article, "<section class=\"langref-article\">");
}

test "renderArticleBody resolves cross-article links against langref_base" {
    const gpa = testing.allocator;
    const md =
        "# T\n\nSee [values](expressions.md#value) and the [home](README.md).\n";
    const article = Article{
        .slug = "statements",
        .title = "T",
        .source_path = "",
        .markdown = md,
        .is_index = false,
    };

    // README landing page (served at `/langref/`): siblings are bare slugs and
    // the README self-link is the current directory.
    {
        var aw = std.Io.Writer.Allocating.init(gpa);
        defer aw.deinit();
        try renderArticleBody(&aw.writer, gpa, &[_]Article{}, &article, "");
        try testing.expect(std.mem.find(u8, aw.written(), "href=\"expressions#value\"") != null);
        try testing.expect(std.mem.find(u8, aw.written(), "href=\"./\"") != null);
    }

    // Article page (served at `/langref/<slug>/`): siblings and the README both
    // live one level up, so links are prefixed with "../".
    {
        var aw = std.Io.Writer.Allocating.init(gpa);
        defer aw.deinit();
        try renderArticleBody(&aw.writer, gpa, &[_]Article{}, &article, "../");
        try testing.expect(std.mem.find(u8, aw.written(), "href=\"../expressions#value\"") != null);
        try testing.expect(std.mem.find(u8, aw.written(), "href=\"../\"") != null);
    }
}

test "renderArticleBody honors explicit heading ids and hides the attribute" {
    const gpa = testing.allocator;
    const md =
        "# T\n\n## [Values](#values) {#values}\n\nbody\n";
    const article = Article{
        .slug = "t",
        .title = "T",
        .source_path = "",
        .markdown = md,
        .is_index = false,
    };
    // The explicit id is used verbatim...
    try expectBody(gpa, &article, "<h2 id=\"values\">");
    // ...and the `{#values}` attribute is not rendered as visible text.
    var aw = std.Io.Writer.Allocating.init(gpa);
    defer aw.deinit();
    try renderArticleBody(&aw.writer, gpa, &[_]Article{}, &article, "");
    try testing.expect(std.mem.find(u8, aw.written(), "{#values}") == null);
}

test "splitHeadingAttr parses and strips the id" {
    const a = splitHeadingAttr("Values {#custom-id}");
    try testing.expectEqualStrings("Values", a.text);
    try testing.expectEqualStrings("custom-id", a.id.?);

    // A brace group that is not a valid `{#id}` attribute stays literal.
    const b = splitHeadingAttr("Set {a, b}");
    try testing.expectEqualStrings("Set {a, b}", b.text);
    try testing.expect(b.id == null);
}

test "linkTargetSlug extracts article slugs" {
    try testing.expectEqualStrings("types", linkTargetSlug("types.md#nominal").?);
    try testing.expectEqualStrings("pattern-matching", linkTargetSlug("pattern-matching").?);
    try testing.expect(linkTargetSlug("#values") == null);
    try testing.expect(linkTargetSlug("https://x.org") == null);
}

test "conditionals links resolve to the if-else page" {
    const gpa = testing.allocator;
    try expectInline(gpa, "[c](conditionals.md)", "<a href=\"if-else\">c</a>");
    try expectInline(gpa, "[if](conditionals.md#if)", "<a href=\"if-else#if\">if</a>");
    try testing.expectEqualStrings("if-else", linkTargetSlug("conditionals.md#if").?);
}

test "renderArticleBody renders a pipe table" {
    const gpa = testing.allocator;
    const md =
        "# T\n\n| A | B |\n|---|---|\n| 1 | 2 |\n";
    const article = Article{
        .slug = "t",
        .title = "T",
        .source_path = "",
        .markdown = md,
        .is_index = false,
    };
    try expectBody(gpa, &article, "<thead><tr><th>A</th><th>B</th></tr></thead>");
    try expectBody(gpa, &article, "<td>1</td><td>2</td>");
}
