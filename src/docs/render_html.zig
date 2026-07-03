//! HTML documentation renderer for Roc modules.
//!
//! Generates a standalone HTML documentation site from `PackageDocs`.
//! Each module gets its own page with linked type signatures, and an
//! index page lists all modules in the package.

const std = @import("std");
const Allocator = std.mem.Allocator;
const DocModel = @import("DocModel.zig");
const render_markdown = @import("render_markdown.zig");
const collections = @import("collections");
const DocType = DocModel.DocType;
/// Errors that can occur while rendering HTML documentation.
pub const RenderError = Allocator.Error || std.Io.Dir.CreateDirPathError || std.Io.Dir.OpenError || std.Io.Dir.WriteFileError || std.Io.File.OpenError || std.Io.File.Writer.Error || std.Io.Writer.Error;

/// Synthetic `data-module-name` used for the Language Reference sidebar entry,
/// chosen so it cannot collide with a real module name.
const langref_sidebar_id = "__lang_ref__";

// Static assets embedded at compile time
const embedded_css = @embedFile("static/styles.css");
const embedded_js = @embedFile("static/search.js");
const embedded_favicon = @embedFile("static/favicon.svg");

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

    fn init(gpa: Allocator, name: []const u8, full_path: []const u8, owns_full_path: bool) Allocator.Error!*SidebarNode {
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

/// Base URL of the published builtin documentation, used when the docs being
/// generated reference builtin types but the builtins are not part of the
/// package being documented. Each builtin type is a top-level page here (e.g.
/// `…/Str`), matching how `reshapeBuiltin` lays the site out.
const builtins_docs_base_url = "https://roc-lang.org/builtins/main/";

/// Builtin types documented as nested types on another type's page rather than
/// on their own top-level page, mapped to the owning page. Mirrors the builtins'
/// structure (the numeric types live under `Num`); an external reference to one
/// of these resolves to `…/Num#U8`. Types not listed here are assumed to have
/// their own page. Keep in sync with the builtins.
const builtin_nested_type_owners = [_]struct { name: []const u8, owner: []const u8 }{
    .{ .name = "U8", .owner = "Num" },   .{ .name = "U16", .owner = "Num" },
    .{ .name = "U32", .owner = "Num" },  .{ .name = "U64", .owner = "Num" },
    .{ .name = "U128", .owner = "Num" }, .{ .name = "I8", .owner = "Num" },
    .{ .name = "I16", .owner = "Num" },  .{ .name = "I32", .owner = "Num" },
    .{ .name = "I64", .owner = "Num" },  .{ .name = "I128", .owner = "Num" },
    .{ .name = "F32", .owner = "Num" },  .{ .name = "F64", .owner = "Num" },
    .{ .name = "Dec", .owner = "Num" },  .{ .name = "Numeral", .owner = "Num" },
};

/// Writes the published URL for a builtin type referenced from another package's
/// docs: `…/Num#U8` for a nested type, `…/Str` for a top-level one.
fn writeBuiltinTypeUrl(w: Writer, type_name: []const u8) (Allocator.Error || error{WriteFailed})!void {
    try w.writeAll(builtins_docs_base_url);
    for (builtin_nested_type_owners) |nested| {
        if (std.mem.eql(u8, nested.name, type_name)) {
            try writeHtmlEscaped(w, nested.owner);
            try w.writeAll("#");
            break;
        }
    }
    try writeHtmlEscaped(w, type_name);
}

/// A `[Name]` shorthand reference that resolved to an HTML anchor that does
/// not correspond to any `id="…"` in the rendered docs site.
pub const BrokenLink = struct {
    /// Module whose doc comment contained the bad reference.
    source_module: []const u8,
    /// Filesystem path to the source `.roc` file the doc comment was
    /// extracted from, when known. Empty when the source isn't tracked
    /// (e.g. in unit tests that build a `PackageDocs` in memory).
    source_path: []const u8,
    /// 1-based source line number of the line containing the `[Name]`.
    /// Zero when the source line couldn't be determined.
    source_line: u32,
    /// Label as written by the user, e.g. `div_by` or `Str.foo`. Slice is
    /// owned by the caller-provided allocator (see `renderPackageDocs`).
    label: []const u8,
    /// Anchor we resolved the label to. For same-page or relative refs this
    /// is the bare fragment (e.g. `Builtin.div_by`); for cross-module refs
    /// it is the anchor expected to exist on the target module's page.
    /// Slice is owned by the caller-provided allocator.
    resolved_anchor: []const u8,
};

/// Context for rendering, shared across all pages.
const RenderContext = struct {
    package_docs: *const DocModel.PackageDocs,
    known_modules: std.StringHashMapUnmanaged(void),
    current_module: ?[]const u8,
    current_module_entries: ?[]const DocModel.DocEntry = null,
    /// Maps a short type name (e.g. "U8", "Utf8Problem") to its module-relative
    /// dotted path (e.g. "Num.U8", "Str.Utf8Problem") within `current_module`.
    /// HTML anchors are built as `<module>.<relative_path>`, so without this
    /// table a reference to `U8` inside Builtin would link to `#Builtin.U8` even
    /// though the actual entry id is `#Builtin.Num.U8`. Type-name keys collide
    /// only across different sub-namespaces with the same final segment; the
    /// first-seen entry wins (matching source order).
    current_module_anchors: std.StringHashMapUnmanaged([]const u8) = .empty,
    /// When true, the single module's page is rendered at the root (index.html),
    /// so links to the module should point to the root instead of a subdirectory.
    single_module_at_root: bool = false,
    /// When true, renderDocTypeHtml emits plain <span> instead of <a> for type
    /// references. Used inside search entries to avoid invalid nested <a> tags.
    suppress_type_links: bool = false,
    /// True when the package being documented is Builtin itself, so references
    /// to builtin types stay local instead of pointing at roc-lang.org.
    documenting_builtin: bool = false,
    /// Maps a builtin type's short name to its owning promoted module (e.g.
    /// "U8" -> "Num", "Utf8Problem" -> "Str"), for the modules reshaped out of
    /// `Builtin`. Lets a bare `[U8]` shorthand in one type's docs resolve to the
    /// page that actually documents it. Keys/values are slices into the
    /// PackageDocs and live for the duration of rendering.
    builtin_type_owners: std.StringHashMapUnmanaged([]const u8) = .empty,
    /// Names of the modules promoted out of `Builtin` by `reshapeBuiltin`. These
    /// use bare anchors (no `<module>.` prefix). Keys are slices into PackageDocs.
    builtin_modules: std.StringHashMapUnmanaged(void) = .empty,
    /// Every `id="…"` value that will be emitted across all module pages,
    /// computed up front so `writeDocRefHref` can verify each `[Name]`
    /// shorthand resolves to a real anchor. Keys are slices into
    /// `anchor_arena`.
    all_anchors: std.StringHashMapUnmanaged(void) = .empty,
    /// Backing storage for `all_anchors` keys.
    anchor_arena: collections.SingleThreadArena,
    /// When non-null, broken `[Name]` references are appended here as they
    /// are encountered. The list and its strings are allocated with
    /// `broken_links_gpa`; the caller owns and must free them.
    broken_links: ?*std.ArrayListUnmanaged(BrokenLink) = null,
    broken_links_gpa: ?Allocator = null,
    /// Source path of the module currently being rendered, if known.
    /// Mutated as we move between modules, so it lives behind a pointer
    /// to keep `*const RenderContext` callers able to update it.
    current_source_path: *[]const u8,
    /// Tracks the doc comment currently being rendered (set by
    /// `renderDocComment`). Behind a pointer for the same reason as
    /// `current_source_path`. Used to translate a byte offset within
    /// the doc into a 1-based source line for broken-link diagnostics.
    active_doc: *ActiveDoc,
    /// The loaded language-reference articles, when `--with-lang-ref` is used.
    /// When set, a "Language Reference" section is added to the sidebar and the
    /// articles are rendered as additional pages under `langref/`.
    langref: ?*const render_markdown.LangRef = null,

    pub const ActiveDoc = struct {
        /// Full doc-comment string currently being walked.
        doc: []const u8 = "",
        /// 1-based source line of the first character of `doc`. Zero
        /// when unknown — broken-link reports then get `source_line = 0`.
        start_line: u32 = 0,
    };

    fn init(package_docs: *const DocModel.PackageDocs, gpa: Allocator) Allocator.Error!RenderContext {
        var known = std.StringHashMapUnmanaged(void){};
        errdefer known.deinit(gpa);
        var documenting_builtin = false;
        var builtin_type_owners = std.StringHashMapUnmanaged([]const u8){};
        errdefer builtin_type_owners.deinit(gpa);
        var builtin_modules = std.StringHashMapUnmanaged(void){};
        errdefer builtin_modules.deinit(gpa);
        for (package_docs.modules) |mod| {
            try known.put(gpa, mod.name, {});
            // A module literally named "Builtin" means we're rendering the
            // builtins before reshaping; a `builtin_derived` module means the
            // types have been promoted out of `Builtin`. Either way we're
            // documenting the builtins, so builtin type refs stay on-site.
            if (std.mem.eql(u8, mod.name, "Builtin") or mod.builtin_derived) documenting_builtin = true;
            if (mod.builtin_derived) {
                try builtin_modules.put(gpa, mod.name, {});
                for (mod.entries) |*entry| {
                    try collectBuiltinTypeOwners(&builtin_type_owners, gpa, entry, mod.name);
                }
            }
        }

        var arena = collections.SingleThreadArena.init(gpa);
        errdefer arena.deinit();
        var anchors = std.StringHashMapUnmanaged(void).empty;
        errdefer anchors.deinit(gpa);
        for (package_docs.modules) |mod| {
            // The bare module name on its own is a valid link target: a
            // same-module `[Foo]` writes `#Foo` and a cross-module `[Foo]`
            // navigates to the module page (no fragment).
            try addAnchor(&anchors, gpa, arena.allocator(), mod.name);
            try collectAnchorsForEntries(&anchors, gpa, arena.allocator(), mod.name, mod.entries, "", mod.builtin_derived);
        }

        const active_doc = try arena.allocator().create(ActiveDoc);
        active_doc.* = .{};
        const current_source_path = try arena.allocator().create([]const u8);
        current_source_path.* = "";

        return .{
            .package_docs = package_docs,
            .known_modules = known,
            .current_module = null,
            .current_module_entries = null,
            .documenting_builtin = documenting_builtin,
            .builtin_type_owners = builtin_type_owners,
            .builtin_modules = builtin_modules,
            .all_anchors = anchors,
            .anchor_arena = arena,
            .current_source_path = current_source_path,
            .active_doc = active_doc,
        };
    }

    fn deinit(self: *RenderContext, gpa: Allocator) void {
        self.current_module_anchors.deinit(gpa);
        self.known_modules.deinit(gpa);
        self.builtin_type_owners.deinit(gpa);
        self.builtin_modules.deinit(gpa);
        self.all_anchors.deinit(gpa);
        self.anchor_arena.deinit();
    }

    /// The promoted module that documents builtin type `head` (e.g. "Num" for
    /// "U8"), or null when `head` is not a builtin type.
    fn builtinTypeOwner(self: *const RenderContext, head: []const u8) ?[]const u8 {
        return self.builtin_type_owners.get(head);
    }

    /// Whether `module_name` was promoted out of `Builtin` and therefore uses
    /// bare anchors (no `<module>.` prefix).
    fn isBuiltinDerived(self: *const RenderContext, module_name: []const u8) bool {
        return self.builtin_modules.contains(module_name);
    }

    /// Record a broken `[Name]` reference. `bracket_offset` is the byte
    /// offset of the `[` within the active doc comment; together with
    /// `active_doc.start_line` it pins the report to a source line.
    /// No-op when collection is disabled.
    fn reportBrokenLink(
        self: *const RenderContext,
        label: []const u8,
        resolved_anchor: []const u8,
        bracket_offset: usize,
    ) Allocator.Error!void {
        const list = self.broken_links orelse return;
        const gpa = self.broken_links_gpa orelse return;
        const source_module = self.current_module orelse "";

        const active = self.active_doc.*;
        const source_line: u32 = if (active.start_line == 0)
            0
        else blk: {
            const end = @min(bracket_offset, active.doc.len);
            var newlines: u32 = 0;
            for (active.doc[0..end]) |c| {
                if (c == '\n') newlines += 1;
            }
            break :blk active.start_line + newlines;
        };

        const label_dup = try gpa.dupe(u8, label);
        errdefer gpa.free(label_dup);
        const anchor_dup = try gpa.dupe(u8, resolved_anchor);
        errdefer gpa.free(anchor_dup);
        try list.append(gpa, .{
            .source_module = source_module,
            .source_path = self.current_source_path.*,
            .source_line = source_line,
            .label = label_dup,
            .resolved_anchor = anchor_dup,
        });
    }

    /// Replace `current_module` and `current_module_entries`, then rebuild the
    /// anchor map from the new module's entries. Anchor map keys and values
    /// are slices into entry names owned by `PackageDocs`, so clearing the map
    /// retains the storage but invalidates no memory we own.
    fn enterModule(self: *RenderContext, gpa: Allocator, mod: *const DocModel.ModuleDocs) Allocator.Error!void {
        self.current_module = mod.name;
        self.current_module_entries = mod.entries;
        self.current_source_path.* = mod.source_path orelse "";
        self.current_module_anchors.clearRetainingCapacity();
        try populateAnchorMap(&self.current_module_anchors, gpa, self.anchor_arena.allocator(), mod.name, mod.entries, "");
    }

    fn leaveModule(self: *RenderContext) void {
        self.current_module = null;
        self.current_module_entries = null;
        self.current_source_path.* = "";
        self.current_module_anchors.clearRetainingCapacity();
    }

    /// Look up the head segment of `name` in the anchor map. Returns the
    /// remapped path (e.g. `Num.U8` for `U8`) when the head is mapped, or
    /// null otherwise. The optional `tail` is the dotted suffix to append
    /// after the remapped head (e.g. `.default`); callers handle writing it.
    fn lookupAnchorHead(self: *const RenderContext, head: []const u8) ?[]const u8 {
        return self.current_module_anchors.get(head);
    }
};

/// Build the anchor map for a module by walking every entry name. The
/// extractor produces flat entries whose `name` is the fully-qualified dotted
/// path (e.g. `Builtin.Num.U8.default`); intermediate path components like
/// `Num` and `U8` are not standalone entries but become group nodes in the
/// rendered tree (so anchors `id="Builtin.Num.U8"` exist on the page). For
/// each entry we walk its prefixes — minus the leaf component for value
/// entries — and record `short_name → module_relative_path` so a reference to
/// a bare `U8` can be linked to `Builtin.Num.U8`. Keys and values are slices
/// into `entry.name`, which is owned by the PackageDocs and lives for the
/// duration of rendering.
fn populateAnchorMap(
    map: *std.StringHashMapUnmanaged([]const u8),
    gpa: Allocator,
    arena: Allocator,
    module_name: []const u8,
    entries: []const DocModel.DocEntry,
    parent_rel_path: []const u8,
) Allocator.Error!void {
    for (entries) |entry| {
        const local_name = moduleRelativeEntryName(module_name, entry.name);
        const entry_rel_path = if (parent_rel_path.len == 0)
            try arena.dupe(u8, local_name)
        else
            try std.fmt.allocPrint(arena, "{s}.{s}", .{ parent_rel_path, local_name });

        // For value entries, the final dotted segment is the value's own name
        // (e.g. `default` in `Builtin.Num.U8.default`) — exclude it so we only
        // map type-like prefixes (`Num`, `Num.U8`).
        var rel_end: usize = entry_rel_path.len;
        if (entry.kind == .value) {
            const last_dot = std.mem.findScalarLast(u8, entry_rel_path, '.') orelse {
                try populateAnchorMap(map, gpa, arena, module_name, entry.children, entry_rel_path);
                continue;
            };
            rel_end = last_dot;
        }

        // Walk each `.`-separated prefix of the relative path.
        var seg_start: usize = 0;
        while (seg_start < rel_end) {
            const next_dot = std.mem.findScalarPos(u8, entry_rel_path[0..rel_end], seg_start, '.');
            const seg_end = next_dot orelse rel_end;
            const short_name = entry_rel_path[seg_start..seg_end];
            const prefix_path = entry_rel_path[0..seg_end];

            const result = try map.getOrPut(gpa, short_name);
            if (!result.found_existing) {
                result.value_ptr.* = prefix_path;
            }

            seg_start = if (next_dot) |d| d + 1 else rel_end;
        }

        try populateAnchorMap(map, gpa, arena, module_name, entry.children, entry_rel_path);
    }
}

/// Record every type (not value) reachable from `entry` as owned by `module`,
/// keyed by its short name, so a bare `[U8]` reference resolves to the page that
/// documents it. First-seen wins on short-name collisions.
fn collectBuiltinTypeOwners(
    map: *std.StringHashMapUnmanaged([]const u8),
    gpa: Allocator,
    entry: *const DocModel.DocEntry,
    module: []const u8,
) Allocator.Error!void {
    if (entry.kind != .value) {
        const short = if (std.mem.findScalarLast(u8, entry.name, '.')) |d| entry.name[d + 1 ..] else entry.name;
        const result = try map.getOrPut(gpa, short);
        if (!result.found_existing) result.value_ptr.* = module;
    }
    for (entry.children) |*child| {
        try collectBuiltinTypeOwners(map, gpa, child, module);
    }
}

fn moduleRelativeEntryName(module_name: []const u8, entry_name: []const u8) []const u8 {
    if (std.mem.startsWith(u8, entry_name, module_name) and
        entry_name.len > module_name.len and
        entry_name[module_name.len] == '.')
    {
        return entry_name[module_name.len + 1 ..];
    }
    return entry_name;
}

/// Add `name` to the anchor set, duplicating the slice into `arena` so the
/// stored key outlives any caller-owned input. No-op when already present.
fn addAnchor(
    set: *std.StringHashMapUnmanaged(void),
    gpa: Allocator,
    arena: Allocator,
    name: []const u8,
) Allocator.Error!void {
    const result = try set.getOrPut(gpa, name);
    if (!result.found_existing) {
        result.key_ptr.* = try arena.dupe(u8, name);
    }
}

/// Walk every entry recursively and add each dotted prefix of its full path
/// (including the leaf segment, even for value entries) to `set`. Mirrors the
/// shape of `buildEntryTree` so every `id="…"` value emitted by the renderer
/// — both leaf entry ids and intermediate group-node ids — has a matching
/// entry here. `parent_path` is the full path of the entry whose `children`
/// we are walking, or empty for top-level entries (whose own `name` already
/// contains the module prefix).
fn collectAnchorsForEntries(
    set: *std.StringHashMapUnmanaged(void),
    gpa: Allocator,
    arena: Allocator,
    module_name: []const u8,
    entries: []const DocModel.DocEntry,
    parent_path: []const u8,
    builtin_derived: bool,
) Allocator.Error!void {
    for (entries) |entry| {
        const full_path = if (parent_path.len == 0) blk: {
            if (entryNameHasModulePrefix(module_name, entry.name)) {
                break :blk try arena.dupe(u8, entry.name);
            }
            break :blk try std.fmt.allocPrint(arena, "{s}.{s}", .{ module_name, entry.name });
        } else try std.fmt.allocPrint(arena, "{s}.{s}", .{ parent_path, entry.name });

        // Add every dotted prefix of `full_path` as a valid anchor. For promoted
        // builtin modules, also add the prefix-stripped form, since their ids and
        // links are bare (`U8` rather than `Num.U8`).
        const variants = [_][]const u8{ full_path, if (builtin_derived) moduleRelativeEntryName(module_name, full_path) else full_path };
        for (variants) |path| {
            var i: usize = 0;
            while (i <= path.len) : (i += 1) {
                if (i == path.len or path[i] == '.') {
                    const result = try set.getOrPut(gpa, path[0..i]);
                    if (!result.found_existing) {
                        // Key slice points into `path`, which lives in `arena`.
                        result.key_ptr.* = path[0..i];
                    }
                }
            }
        }

        try collectAnchorsForEntries(set, gpa, arena, module_name, entry.children, full_path, builtin_derived);
    }
}

fn entryNameHasModulePrefix(module_name: []const u8, entry_name: []const u8) bool {
    return std.mem.eql(u8, entry_name, module_name) or
        (std.mem.startsWith(u8, entry_name, module_name) and
            entry_name.len > module_name.len and
            entry_name[module_name.len] == '.');
}

/// Generate the complete HTML documentation site from PackageDocs.
/// Creates directories and writes all files under `output_dir_path`.
///
/// When `broken_links_out` is non-null, every `[Name]` shorthand reference
/// that resolves to a non-existent anchor is appended to it. The entries
/// (including their `label` and `resolved_anchor` strings) are allocated
/// with `gpa` and become the caller's responsibility.
pub fn renderPackageDocs(
    gpa: Allocator,
    io: std.Io,
    package_docs: *const DocModel.PackageDocs,
    output_dir_path: []const u8,
    broken_links_out: ?*std.ArrayListUnmanaged(BrokenLink),
    langref: ?*const render_markdown.LangRef,
) RenderError!void {
    // Ensure the output directory exists
    std.Io.Dir.cwd().createDirPath(io, output_dir_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var output_dir = try std.Io.Dir.cwd().openDir(io, output_dir_path, .{});
    defer output_dir.close(io);

    var ctx = try RenderContext.init(package_docs, gpa);
    defer ctx.deinit(gpa);
    ctx.broken_links = broken_links_out;
    ctx.broken_links_gpa = if (broken_links_out != null) gpa else null;
    ctx.langref = langref;

    // Write static assets
    try writeStaticAssets(io, output_dir);

    if (package_docs.modules.len == 1) {
        // Single module: write module content directly to root index.html
        ctx.single_module_at_root = true;
        const mod = &package_docs.modules[0];
        try ctx.enterModule(gpa, mod);
        try writeModulePageToDir(&ctx, gpa, io, output_dir, mod, "");
        ctx.leaveModule();
    } else {
        // Multiple modules: write package index and per-module pages
        try writePackageIndex(&ctx, gpa, io, output_dir);

        for (package_docs.modules) |*mod| {
            try ctx.enterModule(gpa, mod);
            try writeModulePage(&ctx, gpa, io, output_dir, mod);
        }
        ctx.leaveModule();
    }

    // Language Reference articles (rendered into a `langref/` subdirectory).
    if (langref) |lr| {
        try writeLangRefPages(&ctx, gpa, io, output_dir, lr);
    }
}

/// Writes each langref article as a page under `langref/`: the README becomes
/// `langref/index.html` (the section landing page, served at `/langref/`) and
/// every other article becomes `langref/<slug>/index.html`, so its URL is the
/// extensionless `/langref/<slug>`. The README sits one level below the site
/// root and each article two levels below, so their relative asset/link bases
/// differ (see `writeLangRefArticlePage`).
fn writeLangRefPages(
    ctx: *const RenderContext,
    gpa: Allocator,
    io: std.Io,
    dir: std.Io.Dir,
    langref: *const render_markdown.LangRef,
) RenderError!void {
    dir.createDirPath(io, "langref") catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };
    var sub_dir = try dir.openDir(io, "langref", .{});
    defer sub_dir.close(io);

    for (langref.articles) |*article| {
        if (article.is_index) {
            try writeLangRefArticlePage(ctx, gpa, io, sub_dir, langref, article, "index.html");
            continue;
        }

        // Each article gets its own directory so the page can be `index.html`,
        // giving the extensionless `/langref/<slug>` URL. Slugs are filename-safe.
        sub_dir.createDirPath(io, article.slug) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };
        var article_dir = try sub_dir.openDir(io, article.slug, .{});
        defer article_dir.close(io);
        try writeLangRefArticlePage(ctx, gpa, io, article_dir, langref, article, "index.html");
    }
}

fn writeLangRefArticlePage(
    ctx: *const RenderContext,
    gpa: Allocator,
    io: std.Io,
    dir: std.Io.Dir,
    langref: *const render_markdown.LangRef,
    article: *const render_markdown.Article,
    file_name: []const u8,
) RenderError!void {
    const file = try dir.createFile(io, file_name, .{});
    defer file.close(io);
    var buf: [4096]u8 = undefined;
    var bw = file.writer(io, &buf);
    const w = &bw.interface;

    // Page title comes from the article's first heading; fall back to its slug.
    var title_buf: [256]u8 = undefined;
    const plain_title = render_markdown.titlePlainText(gpa, article) catch null;
    defer if (plain_title) |t| gpa.free(t);
    const title = std.fmt.bufPrint(&title_buf, "{s} Docs", .{plain_title orelse article.slug}) catch (plain_title orelse article.slug);

    // The README landing page is served at `/langref/` (one level below the
    // site root), while every other article is served at `/langref/<slug>/`
    // (two levels below). `base` reaches the site root — for assets, module
    // links and search entries — and `langref_base` reaches the `langref/`
    // directory, for links to sibling articles.
    const base = if (article.is_index) "../" else "../../";
    const langref_base = if (article.is_index) "" else "../";
    try writeHtmlHead(w, title, base);
    try writeBodyOpen(w);
    try renderSidebar(w, ctx, gpa, base);

    try writeMainOpen(w, ctx, gpa, base);
    try render_markdown.renderArticleBody(w, gpa, langref.articles, article, langref_base);
    try writeFooter(w);
    try w.writeAll("    </main>\n");
    try writeBodyClose(w);
    try bw.interface.flush();
}

fn writeStaticAssets(io: std.Io, dir: std.Io.Dir) RenderError!void {
    try dir.writeFile(io, .{ .sub_path = "styles.css", .data = embedded_css });
    try dir.writeFile(io, .{ .sub_path = "search.js", .data = embedded_js });
    try dir.writeFile(io, .{ .sub_path = "favicon.svg", .data = embedded_favicon });
}

fn writePackageIndex(ctx: *const RenderContext, gpa: Allocator, io: std.Io, dir: std.Io.Dir) RenderError!void {
    const file = try dir.createFile(io, "index.html", .{});
    defer file.close(io);
    var buf: [4096]u8 = undefined;
    var bw = file.writer(io, &buf);
    const w = &bw.interface;

    // The builtins package's real name ("Builtin") is an implementation detail
    // we keep out of the docs; the site is presented generically as
    // "Documentation".
    const display_name = if (ctx.documenting_builtin) "Documentation" else ctx.package_docs.name;

    var index_title_buf: [256]u8 = undefined;
    const index_title = std.fmt.bufPrint(&index_title_buf, "{s} Docs", .{display_name}) catch display_name;
    try writeHtmlHead(w, index_title, "");
    try writeBodyOpen(w);
    try renderSidebar(w, ctx, gpa, "");

    // Main content
    try writeMainOpen(w, ctx, gpa, "");
    try w.writeAll("        <h1 class=\"module-name\">");
    try writeHtmlEscaped(w, display_name);
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

fn writeModulePage(ctx: *const RenderContext, gpa: Allocator, io: std.Io, dir: std.Io.Dir, mod: *const DocModel.ModuleDocs) RenderError!void {
    // Create module subdirectory
    dir.createDirPath(io, mod.name) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var sub_dir = try dir.openDir(io, mod.name, .{});
    defer sub_dir.close(io);

    try writeModulePageToDir(ctx, gpa, io, sub_dir, mod, "../");
}

/// Write a module's documentation page as index.html in the given directory.
/// `base` is the relative path prefix for static assets (e.g. "" for root, "../" for subdirs).
fn writeModulePageToDir(ctx: *const RenderContext, gpa: Allocator, io: std.Io, dir: std.Io.Dir, mod: *const DocModel.ModuleDocs, base: []const u8) RenderError!void {
    const file = try dir.createFile(io, "index.html", .{});
    defer file.close(io);
    var buf: [4096]u8 = undefined;
    var bw = file.writer(io, &buf);
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
    try w.writeAll("</h1>\n");

    // Build entry tree (automatically collapses redundant top-level node
    // matching the module name, e.g. Parser > Parser or Builtin > Builtin).
    const entry_tree = try buildEntryTree(gpa, mod.entries, mod.name, mod.builtin_derived);
    defer entry_tree.deinit(gpa);

    // Show collapsed type definition at module level
    if (entry_tree.collapsed_entry) |entry| {
        if (entry.kind == .nominal) {
            if (entry.type_signature) |sig| {
                try w.writeAll("        <code class=\"entry-type-def\">");
                try w.writeAll(":= ");
                try renderDocTypeHtml(w, ctx, gpa, sig, false);
                try w.writeAll("</code>\n");
            }
        }
    }

    // Module doc comment (fall back to collapsed entry's doc if module has none)
    const doc_with_line: ?struct { doc: []const u8, start_line: u32 } = if (mod.module_doc) |doc|
        .{ .doc = doc, .start_line = mod.module_doc_start_line }
    else if (entry_tree.collapsed_entry) |entry|
        if (entry.doc_comment) |doc| .{ .doc = doc, .start_line = entry.doc_comment_start_line } else null
    else
        null;
    if (doc_with_line) |dwl| {
        try w.writeAll("        <div class=\"module-doc\">\n");
        try renderDocComment(w, ctx, dwl.doc, dwl.start_line);
        try w.writeAll("        </div>\n");
    }

    // Render entries
    try renderEntryTree(w, ctx, gpa, entry_tree.root, 0);

    try writeFooter(w);
    try w.writeAll("    </main>\n");
    try writeBodyClose(w);
    try bw.interface.flush();
}

fn writeHtmlHead(w: Writer, title: []const u8, base: []const u8) (Allocator.Error || error{WriteFailed})!void {
    try w.writeAll("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n");
    try w.writeAll("    <meta charset=\"utf-8\">\n");
    try w.writeAll("    <title>");
    try writeHtmlEscaped(w, title);
    try w.writeAll("</title>\n");
    try w.writeAll("    <meta name=\"viewport\" content=\"width=device-width\">\n");
    try w.writeAll("    <link rel=\"icon\" type=\"image/svg+xml\" href=\"");
    try w.writeAll(base);
    try w.writeAll("favicon.svg\">\n");
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

fn writeBodyOpen(w: Writer) error{WriteFailed}!void {
    try w.writeAll("<body>\n");
    try w.writeAll(link_svg_defs);
    try w.writeAll("\n");
}

fn writeBodyClose(w: Writer) error{WriteFailed}!void {
    try w.writeAll("</body>\n</html>\n");
}

const menu_toggle_svg =
    \\<svg viewBox="0 6 18 12" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
    \\    <path d="M0 6h18v2H0V6zm0 5h18v2H0v-2zm0 5h18v2H0v-2z"/>
    \\</svg>
;

fn writeMainOpen(w: Writer, ctx: *const RenderContext, gpa: Allocator, base: []const u8) (Allocator.Error || error{WriteFailed})!void {
    try w.writeAll("    <main>\n");
    try w.writeAll("        <form id=\"module-search-form\">\n");
    try w.writeAll("            <input type=\"search\" id=\"module-search\" placeholder=\"Search Documentation\" autocomplete=\"off\" />\n");
    // The no-JS input must be a sibling (not nested inside <noscript>) so it
    // participates in the form's flex layout. With JS enabled it is hidden by
    // default CSS; the <noscript><style> below swaps which input is visible
    // when JS is disabled. Avoid putting layout-relevant elements inside
    // <noscript> — Chrome can render them as literal text.
    try w.writeAll("            <input type=\"search\" id=\"module-search-nojs\" placeholder=\"Enable JavaScript to search\" autocomplete=\"off\" disabled aria-label=\"Search requires JavaScript\" />\n");
    try w.writeAll("            <noscript><style>#module-search{display:none}#module-search-nojs{display:block}</style></noscript>\n");
    try w.writeAll("            <button class=\"menu-toggle\" type=\"button\" aria-label=\"Toggle sidebar\">");
    try w.writeAll(menu_toggle_svg);
    try w.writeAll("</button>\n");
    try w.writeAll("            <ul id=\"search-type-ahead\" class=\"hidden\">\n");
    try renderSearchEntries(w, ctx, gpa, base);
    try w.writeAll("            </ul>\n");
    try w.writeAll("        </form>\n");
    try w.writeAll("        <div class=\"main-content\">\n");
}

fn writeFooter(w: Writer) error{WriteFailed}!void {
    try w.writeAll("        </div>\n");
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

fn buildEntryTree(gpa: Allocator, entries: []const DocModel.DocEntry, module_name: []const u8, builtin_derived: bool) Allocator.Error!EntryTree {
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
        if (!entryNameHasModulePrefix(module_name, entry.name)) {
            try path_so_far.appendSlice(gpa, module_name);
        }

        for (parts.items, 0..) |part, idx| {
            if (idx > 0 or path_so_far.items.len > 0) try path_so_far.append(gpa, '.');
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
                    try addChildToEntryTree(gpa, node, node.full_path, child_entry);
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

    // Promoted builtin modules drop the redundant `<module>.` prefix from every
    // anchor (id, sidebar, search, permalink all read `node.full_path`), so e.g.
    // `Hasher.write_u8` renders as `write_u8`.
    if (builtin_derived) {
        for (root.children.items) |child| {
            try stripModulePrefixFromTree(gpa, child, module_name);
        }
    }

    return .{ .root = root, .collapsed_entry = collapsed_entry };
}

/// Recursively rewrite each node's `full_path` to drop a leading `<module>.`.
fn stripModulePrefixFromTree(gpa: Allocator, node: *SidebarNode, module_name: []const u8) Allocator.Error!void {
    const fp = node.full_path;
    if (fp.len > module_name.len and
        std.mem.startsWith(u8, fp, module_name) and
        fp[module_name.len] == '.')
    {
        const stripped = try gpa.dupe(u8, fp[module_name.len + 1 ..]);
        if (node.owns_full_path) gpa.free(node.full_path);
        node.full_path = stripped;
        node.owns_full_path = true;
    }
    for (node.children.items) |child| {
        try stripModulePrefixFromTree(gpa, child, module_name);
    }
}

/// Add a child entry to the content tree, splitting its name on dots
/// to create intermediate group nodes.
fn addChildToEntryTree(
    gpa: Allocator,
    parent_node: *SidebarNode,
    parent_entry_name: []const u8,
    child_entry: *const DocModel.DocEntry,
) Allocator.Error!void {
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

fn writeIndent(w: Writer, level: usize) error{WriteFailed}!void {
    const max_indent = "                                        "; // 40 spaces (10 levels of 4)
    const spaces = @min(level * 4, max_indent.len);
    try w.writeAll(max_indent[0..spaces]);
}

fn renderEntryTree(
    w: Writer,
    ctx: *const RenderContext,
    gpa: Allocator,
    node: *const SidebarNode,
    depth: usize,
) (Allocator.Error || error{WriteFailed})!void {
    // Skip the root node (empty name), process its children
    if (depth == 0) {
        for (node.children.items) |child| {
            try renderEntryTree(w, ctx, gpa, child, depth + 1);
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
                try w.writeAll("<a href=\"#");
                try writeHtmlEscaped(w, anchor_id);
                try w.writeAll("\" class=\"entry-name-link\">");
                try writeHtmlEscaped(w, node.name);
                try w.writeAll("</a>");
                try w.print("</h{d}>\n", .{heading_level});

                // Nominal types also show their type definition below the heading
                if (entry.kind == .nominal) {
                    if (entry.type_signature) |sig| {
                        try writeIndent(w, base + 1);
                        try w.writeAll("<code class=\"entry-type-def\">");
                        try w.writeAll(":= ");
                        try renderDocTypeHtml(w, ctx, gpa, sig, false);
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
                try renderEntrySignature(w, ctx, gpa, entry, anchor_id);
                try w.writeAll("</code>\n");
                try writeIndent(w, base + 1);
                try w.writeAll("</div>\n");
            }

            // Doc comment
            if (entry.doc_comment) |doc| {
                try writeIndent(w, base + 1);
                try w.writeAll("<div class=\"entry-doc\">\n");
                try renderDocComment(w, ctx, doc, entry.doc_comment_start_line);
                try writeIndent(w, base + 1);
                try w.writeAll("</div>\n");
            }

            // Children container
            if (node.children.items.len > 0) {
                try writeIndent(w, base + 1);
                try w.writeAll("<div class=\"entry-children-container\">\n");
                for (node.children.items) |child| {
                    try renderEntryTree(w, ctx, gpa, child, depth + 1);
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
            try renderEntryTree(w, ctx, gpa, child, depth + 1);
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
    module_link_prefix: []const u8,
    node: *SidebarNode,
    depth: usize,
) (Allocator.Error || error{WriteFailed})!void {
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
            try w.writeAll("  <a class=\"sidebar-type-name\" href=\"");
            try w.writeAll(module_link_prefix);
            try w.writeAll("#");
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
                try renderSidebarTree(w, module_name, module_link_prefix, child, depth + 1);
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
                try w.writeAll("  <a class=\"sidebar-type-name\" href=\"");
                try w.writeAll(module_link_prefix);
                try w.writeAll("#");
                try writeHtmlEscaped(w, node.full_path);
                try w.writeAll("\">");
                try writeHtmlEscaped(w, node.name);
                try w.writeAll("</a>\n");
                try w.writeAll("                        ");
                try w.writeAll("</li>\n");
            } else {
                // Render as link. The `sidebar-value` class marks it as a value
                // or method (not a type), which is what gets the left bar.
                try w.writeAll("                        ");
                for (0..depth - 1) |_| {
                    try w.writeAll("  ");
                }
                try w.writeAll("<li><a class=\"sidebar-value\" href=\"");
                try w.writeAll(module_link_prefix);
                try w.writeAll("#");
                try writeHtmlEscaped(w, node.full_path);
                try w.writeAll("\">");
                try writeHtmlEscaped(w, node.name);
                try w.writeAll("</a></li>\n");
            }
        }
    } else {
        // Root node - just recurse
        for (node.children.items) |child| {
            try renderSidebarTree(w, module_name, module_link_prefix, child, depth + 1);
        }
    }
}

fn renderSidebarEntries(
    w: Writer,
    gpa: std.mem.Allocator,
    module_name: []const u8,
    module_link_prefix: []const u8,
    entries: []const DocModel.DocEntry,
    builtin_derived: bool,
    _depth: usize,
) (Allocator.Error || error{WriteFailed})!void {
    _ = _depth; // No longer needed

    const entry_tree = try buildEntryTree(gpa, entries, module_name, builtin_derived);
    defer entry_tree.deinit(gpa);

    try renderSidebarTree(w, module_name, module_link_prefix, entry_tree.root, 0);
}

/// Render one module's sidebar entry: a `sidebar-entry` whose `sidebar-module-link`
/// names the module and toggles a `sidebar-sub-entries` list of its members.
fn renderSidebarModuleEntry(w: Writer, ctx: *const RenderContext, gpa: Allocator, base: []const u8, mod: DocModel.ModuleDocs) (Allocator.Error || error{WriteFailed})!void {
    const is_active = if (ctx.current_module) |cur|
        std.mem.eql(u8, cur, mod.name)
    else
        false;

    // Build the href prefix for entries inside this module so that clicking a
    // sidebar entry from another module's page navigates to the correct module
    // page (not just changes the fragment on the current page).
    var module_link_prefix = std.ArrayList(u8).empty;
    defer module_link_prefix.deinit(gpa);
    if (ctx.single_module_at_root) {
        if (base.len != 0) try module_link_prefix.appendSlice(gpa, base);
    } else {
        try module_link_prefix.appendSlice(gpa, base);
        // module names contain only identifier characters, no escaping needed for href
        try module_link_prefix.appendSlice(gpa, mod.name);
        try module_link_prefix.append(gpa, '/');
    }

    try w.writeAll("                <li class=\"sidebar-entry\">\n");
    try w.writeAll("                    <a class=\"sidebar-module-link");
    if (is_active) try w.writeAll(" active");
    try w.writeAll("\" data-module-name=\"");
    try writeHtmlEscaped(w, mod.name);
    try w.writeAll("\" href=\"");
    if (module_link_prefix.items.len == 0) {
        try w.writeAll(".");
    } else {
        try w.writeAll(module_link_prefix.items);
    }
    try w.writeAll("\">");
    try w.writeAll("<button class=\"entry-toggle\"></button>");
    try w.writeAll("<span>");
    try writeHtmlEscaped(w, mod.name);
    try w.writeAll("</span></a>\n");

    // Sub-entries - grouped hierarchically
    try w.writeAll("                    <ul class=\"sidebar-sub-entries\">\n");
    try renderSidebarEntries(w, gpa, mod.name, module_link_prefix.items, mod.entries, mod.builtin_derived, 0);
    try w.writeAll("                    </ul>\n");
    try w.writeAll("                </li>\n");
}

fn renderSidebar(w: Writer, ctx: *const RenderContext, gpa: Allocator, base: []const u8) (Allocator.Error || error{WriteFailed})!void {
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
    // The roc builtins site (the only docs rendered with a langref) covers the
    // whole language, not just the "Builtin" package, so its title is the
    // generic "Documentation" in the page font rather than the package name in
    // the monospace used for code identifiers.
    try w.writeAll("            <h1 class=\"pkg-full-name");
    if (ctx.langref != null) try w.writeAll(" prose-label");
    try w.writeAll("\"><a href=\"");
    try w.writeAll(base);
    try w.writeAll("\">");
    if (ctx.langref != null) {
        try w.writeAll("Documentation");
    } else {
        try writeHtmlEscaped(w, ctx.package_docs.name);
    }
    try w.writeAll("</a></h1>\n");
    try w.writeAll("        </div>\n");

    try w.writeAll("        <div class=\"module-links-container\">\n");
    try w.writeAll("            <ul class=\"module-links\">\n");

    // The promoted builtin types are grouped under one collapsible "Builtin
    // Types" entry so the (long) list can be hidden in one click and doesn't
    // push the Language Reference section down. Everything else renders as a
    // top-level module entry.
    var rendered_builtin_group = false;
    for (ctx.package_docs.modules) |mod| {
        if (mod.builtin_derived and !rendered_builtin_group) {
            rendered_builtin_group = true;
            try w.writeAll("                <li class=\"sidebar-entry\">\n");
            try w.writeAll("                    <a class=\"sidebar-module-link active prose-label\" data-module-name=\"__builtin_types__\" href=\"");
            try writeHtmlEscaped(w, base);
            try w.writeAll("\">");
            try w.writeAll("<button class=\"entry-toggle\"></button>");
            try w.writeAll("<span>Builtin Types</span></a>\n");
            try w.writeAll("                    <ul class=\"sidebar-sub-entries\">\n");
            for (ctx.package_docs.modules) |inner| {
                if (inner.builtin_derived) try renderSidebarModuleEntry(w, ctx, gpa, base, inner);
            }
            try w.writeAll("                    </ul>\n");
            try w.writeAll("                </li>\n");
        } else if (!mod.builtin_derived) {
            try renderSidebarModuleEntry(w, ctx, gpa, base, mod);
        }
    }

    if (ctx.langref) |lr| {
        try renderLangRefSidebar(w, gpa, lr, base);
    }

    try w.writeAll("            </ul>\n");
    try w.writeAll("        </div>\n");
    try w.writeAll("    </nav>\n");
}

/// Renders the "Language Reference" sidebar section: an expandable entry (open
/// by default) whose landing link points at `langref/` (the README) and whose
/// sub-entries link to each article page.
fn renderLangRefSidebar(
    w: Writer,
    gpa: Allocator,
    langref: *const render_markdown.LangRef,
    base: []const u8,
) (Allocator.Error || error{WriteFailed})!void {
    // Links to langref pages are always `<base>langref/…`, because `base`
    // resolves to the site root from the current page and every langref page
    // lives directly under `langref/`.
    try w.writeAll("                <li class=\"sidebar-entry\">\n");
    try w.writeAll("                    <a class=\"sidebar-module-link active prose-label\" data-module-name=\"");
    try w.writeAll(langref_sidebar_id);
    try w.writeAll("\" href=\"");
    try writeHtmlEscaped(w, base);
    try w.writeAll("langref/\">");
    try w.writeAll("<button class=\"entry-toggle\"></button>");
    try w.writeAll("<span>Language Reference</span></a>\n");

    try w.writeAll("                    <ul class=\"sidebar-sub-entries langref-articles\">\n");
    for (langref.articles) |*article| {
        if (article.is_index) continue; // the README is the section landing page

        try w.writeAll("                        <li><a href=\"");
        try writeHtmlEscaped(w, base);
        try w.writeAll("langref/");
        // Slugs contain only filename-safe characters, but escape defensively.
        // No `.html`: each article is served at the extensionless `/langref/<slug>`.
        try writeHtmlEscaped(w, article.slug);
        try w.writeAll("\">");
        // Render the title inline so backtick spans (e.g. the "`if` / `else`"
        // heading) become inline `<code>` rather than literal backticks.
        try render_markdown.renderTitleInline(w, gpa, langref.articles, article);
        try w.writeAll("</a></li>\n");
    }
    try w.writeAll("                    </ul>\n");
    try w.writeAll("                </li>\n");
}

fn renderSearchEntries(w: Writer, ctx: *const RenderContext, gpa: Allocator, base: []const u8) (Allocator.Error || error{WriteFailed})!void {
    for (ctx.package_docs.modules) |mod| {
        const entry_tree = try buildEntryTree(gpa, mod.entries, mod.name, mod.builtin_derived);
        defer entry_tree.deinit(gpa);
        try renderSearchTree(w, ctx, gpa, mod.name, entry_tree.root, base);
    }
}

/// Walk the entry tree depth-first, emitting a search type-ahead <li> for
/// every leaf node that carries a DocEntry.
fn renderSearchTree(
    w: Writer,
    ctx: *const RenderContext,
    gpa: Allocator,
    module_name: []const u8,
    node: *const SidebarNode,
    base: []const u8,
) (Allocator.Error || error{WriteFailed})!void {
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
                    try renderDocTypeHtml(w, sig_ctx, gpa, sig, false);
                },
                .nominal => {
                    try w.writeAll(":= ");
                    try renderDocTypeHtml(w, sig_ctx, gpa, sig, false);
                },
                .@"opaque" => {},
            }
            try w.writeAll("</span>");
        }
        try w.writeAll("</a></li>\n");
    }

    for (node.children.items) |child| {
        try renderSearchTree(w, ctx, gpa, module_name, child, base);
    }
}

fn renderEntrySignature(w: Writer, ctx: *const RenderContext, gpa: Allocator, entry: *const DocModel.DocEntry, anchor_id: []const u8) (Allocator.Error || error{WriteFailed})!void {
    // Display only the identifier (last component) of the entry name
    // For "Builtin.Str.Utf8Problem.is_eq", display as "is_eq"
    const display_name = if (std.mem.findScalarLast(u8, entry.name, '.')) |idx|
        entry.name[idx + 1 ..]
    else
        entry.name;

    try w.writeAll("<a href=\"#");
    try writeHtmlEscaped(w, anchor_id);
    try w.writeAll("\" class=\"entry-name-link\"><strong>");
    try writeHtmlEscaped(w, display_name);
    try w.writeAll("</strong></a>");

    if (entry.type_signature) |sig| {
        switch (entry.kind) {
            .value => {
                try w.writeAll(" : ");
                try renderDocTypeHtml(w, ctx, gpa, sig, false);
            },
            .alias => {
                try w.writeAll(" : ");
                try renderDocTypeHtml(w, ctx, gpa, sig, false);
            },
            .nominal => {
                try w.writeAll(" := ");
                try renderDocTypeHtml(w, ctx, gpa, sig, false);
            },
            .@"opaque" => {
                try w.writeAll(" :: <span class=\"type\">&lt;hidden&gt;</span>");
            },
        }
    }
}

fn renderDocComment(w: Writer, ctx: *const RenderContext, doc: []const u8, start_line: u32) (Allocator.Error || error{WriteFailed})!void {
    // Track the active doc so writeDocRefHref can resolve a `[label]` byte
    // offset within `doc` back to a 1-based source line. Restored on exit
    // to support nested rendering (e.g. a module doc above an entry doc).
    const previous = ctx.active_doc.*;
    defer ctx.active_doc.* = previous;
    ctx.active_doc.* = .{ .doc = doc, .start_line = start_line };

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
            const code = std.mem.trimEnd(u8, doc[pos..], "\n\r");
            if (code.len > 0) {
                try w.writeAll("                <pre><code>");
                try writeHtmlEscaped(w, code);
                try w.writeAll("</code></pre>\n");
            }
            break;
        };

        // Render the code block content
        const code = std.mem.trimEnd(u8, doc[pos..close_pos], "\n\r");
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
fn renderParagraphs(w: Writer, ctx: *const RenderContext, text: []const u8) (Allocator.Error || error{WriteFailed})!void {
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
fn writeDocText(w: Writer, ctx: *const RenderContext, text: []const u8) (Allocator.Error || error{WriteFailed})!void {
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
                const bracket_offset = bracketOffsetInActiveDoc(ctx, text, i);
                try writeDocRefHref(w, ctx, ref.label, bracket_offset);
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

/// Compute the byte offset of `text[i]` within `active_doc.doc`, when `text`
/// is a slice into that doc. Returns the offset, or 0 if the slice is not
/// inside the active doc (e.g. unit tests that bypass `renderDocComment`).
fn bracketOffsetInActiveDoc(ctx: *const RenderContext, text: []const u8, i: usize) usize {
    const doc = ctx.active_doc.doc;
    if (doc.len == 0 or text.len == 0) return 0;
    const text_addr = @intFromPtr(text.ptr);
    const doc_addr = @intFromPtr(doc.ptr);
    if (text_addr < doc_addr) return 0;
    const text_offset = text_addr - doc_addr;
    if (text_offset >= doc.len) return 0;
    return text_offset + i;
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
///
/// As a side effect, when the resolved fragment (`#…`) does not correspond
/// to any `id="…"` in the rendered site, the reference is reported via
/// `ctx.reportBrokenLink` (with the source line derived from
/// `bracket_offset`). The href is still written so output is unchanged —
/// broken-link collection is observational.
fn writeDocRefHref(w: Writer, ctx: *const RenderContext, label: []const u8, bracket_offset: usize) (Allocator.Error || error{WriteFailed})!void {
    const first_dot = std.mem.findScalar(u8, label, '.');
    const head = if (first_dot) |d| label[0..d] else label;

    // Build the anchor we are about to write into a small stack buffer so we
    // can both emit it and validate it against `all_anchors`. The buffer is
    // sized for the longest realistic label (module + dotted path).
    var anchor_buf: [512]u8 = undefined;
    var anchor_len: usize = 0;
    var anchor_overflow = false;
    const append = struct {
        fn run(buf: *[512]u8, len: *usize, overflow: *bool, s: []const u8) void {
            if (overflow.*) return;
            if (len.* + s.len > buf.len) {
                overflow.* = true;
                return;
            }
            @memcpy(buf[len.* .. len.* + s.len], s);
            len.* += s.len;
        }
    }.run;

    if (ctx.known_modules.contains(head)) {
        const is_same_module = if (ctx.current_module) |cur|
            std.mem.eql(u8, head, cur)
        else
            false;

        // Promoted builtin modules use bare anchors, so the fragment drops the
        // `<module>.` head: `[Str.reserve]` -> `#reserve`, `[List.first]` ->
        // `../List/#first`, and `[Str]`/`[List]` land on the page itself.
        if (ctx.isBuiltinDerived(head)) {
            const frag = if (first_dot) |d| label[d + 1 ..] else "";
            if (is_same_module) {
                try w.writeAll("#");
            } else {
                if (ctx.current_module) |_| try w.writeAll("../");
                try writeHtmlEscaped(w, head);
                try w.writeAll("/");
                if (frag.len > 0) try w.writeAll("#");
            }
            try writeHtmlEscaped(w, frag);
            append(&anchor_buf, &anchor_len, &anchor_overflow, frag);
            try validateAnchor(ctx, label, anchor_buf[0..anchor_len], anchor_overflow, bracket_offset);
            return;
        }

        if (is_same_module) {
            // Same-page anchor. Entry ids are prefixed with the module name.
            try w.writeAll("#");
            try writeHtmlEscaped(w, head);
            append(&anchor_buf, &anchor_len, &anchor_overflow, head);
            if (first_dot) |d| {
                try writeHtmlEscaped(w, label[d..]);
                append(&anchor_buf, &anchor_len, &anchor_overflow, label[d..]);
            }
            try validateAnchor(ctx, label, anchor_buf[0..anchor_len], anchor_overflow, bracket_offset);
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
            append(&anchor_buf, &anchor_len, &anchor_overflow, label);
            try validateAnchor(ctx, label, anchor_buf[0..anchor_len], anchor_overflow, bracket_offset);
        }
        // No fragment — the link points at the module's index page, which is
        // guaranteed to exist by the `known_modules.contains` check above.
        return;
    }

    // A bare reference to a builtin type that lives on another promoted page
    // (e.g. `[U8]` from `Str`, where `U8` is documented under `Num`). Its id is
    // `<owner>.<label>`, so link there directly.
    if (ctx.builtinTypeOwner(head)) |owner| {
        // Bare anchors: the owning page's id for this type is `label` itself
        // (e.g. `U8`, `U8.default`), with no `<owner>.` prefix.
        const is_same_module = if (ctx.current_module) |cur| std.mem.eql(u8, owner, cur) else false;
        if (is_same_module) {
            try w.writeAll("#");
        } else {
            if (ctx.current_module) |_| try w.writeAll("../");
            try writeHtmlEscaped(w, owner);
            try w.writeAll("/#");
        }
        try writeHtmlEscaped(w, label);
        append(&anchor_buf, &anchor_len, &anchor_overflow, label);
        try validateAnchor(ctx, label, anchor_buf[0..anchor_len], anchor_overflow, bracket_offset);
        return;
    }

    // Not a known module — treat as a reference relative to the current
    // module. Entry ids are `<module>.<label>`. Substitute the first segment
    // through the anchor map so a label like `U8` resolves to `Num.U8` and
    // `U8.default` resolves to `Num.U8.default`.
    const resolved_head = ctx.lookupAnchorHead(head) orelse head;

    // Promoted builtin modules use bare anchors, so strip any current-module
    // prefix the anchor map left on the resolved head (`Num.F32` -> `F32`).
    if (if (ctx.current_module) |cur| ctx.isBuiltinDerived(cur) else false) {
        const bare_head = if (ctx.current_module) |cur|
            (if (std.mem.startsWith(u8, resolved_head, cur) and resolved_head.len > cur.len and resolved_head[cur.len] == '.')
                resolved_head[cur.len + 1 ..]
            else
                resolved_head)
        else
            resolved_head;
        try w.writeAll("#");
        try writeHtmlEscaped(w, bare_head);
        append(&anchor_buf, &anchor_len, &anchor_overflow, bare_head);
        if (first_dot) |d| {
            try writeHtmlEscaped(w, label[d..]);
            append(&anchor_buf, &anchor_len, &anchor_overflow, label[d..]);
        }
        try validateAnchor(ctx, label, anchor_buf[0..anchor_len], anchor_overflow, bracket_offset);
        return;
    }

    try w.writeAll("#");
    if (ctx.current_module) |cur| {
        try writeHtmlEscaped(w, cur);
        try w.writeAll(".");
        append(&anchor_buf, &anchor_len, &anchor_overflow, cur);
        append(&anchor_buf, &anchor_len, &anchor_overflow, ".");
    }
    try writeHtmlEscaped(w, resolved_head);
    append(&anchor_buf, &anchor_len, &anchor_overflow, resolved_head);
    if (first_dot) |d| {
        try writeHtmlEscaped(w, label[d..]);
        append(&anchor_buf, &anchor_len, &anchor_overflow, label[d..]);
    }
    try validateAnchor(ctx, label, anchor_buf[0..anchor_len], anchor_overflow, bracket_offset);
}

/// Report `label` as broken when its resolved anchor isn't in `all_anchors`.
/// Anchors longer than the stack buffer (`overflow`) are skipped — those are
/// far longer than any real Roc identifier path, so a false negative there
/// is preferable to truncating and reporting a phantom mismatch.
fn validateAnchor(
    ctx: *const RenderContext,
    label: []const u8,
    anchor: []const u8,
    overflow: bool,
    bracket_offset: usize,
) Allocator.Error!void {
    if (overflow) return;
    if (anchor.len == 0) return;
    if (ctx.all_anchors.contains(anchor)) return;
    try ctx.reportBrokenLink(label, anchor, bracket_offset);
}

fn renderDocTypeHtml(
    w: Writer,
    ctx: *const RenderContext,
    gpa: Allocator,
    doc_type: *const DocType,
    needs_parens: bool,
) (Allocator.Error || error{WriteFailed})!void {
    const RenderFrame = union(enum) {
        doc_type: struct {
            value: *const DocType,
            needs_parens: bool,
        },
        html: []const u8,
        escaped: []const u8,
        type_ref: DocType.TypeRef,
    };

    var frames = std.ArrayList(RenderFrame).empty;
    defer frames.deinit(gpa);

    try frames.append(gpa, .{ .doc_type = .{ .value = doc_type, .needs_parens = needs_parens } });
    while (frames.pop()) |frame| {
        switch (frame) {
            .html => |html| try w.writeAll(html),
            .escaped => |text| try writeHtmlEscaped(w, text),
            .type_ref => |ref| {
                const display_name = if (std.mem.findScalarLast(u8, ref.type_name, '.')) |idx|
                    ref.type_name[idx + 1 ..]
                else
                    ref.type_name;

                if (!ctx.suppress_type_links and resolveTypeLink(ctx, ref.module_path)) {
                    try w.writeAll("<a href=\"");
                    try writeTypeLink(w, ctx, ref.module_path, ref.type_name);
                    try w.writeAll("\"><span class=\"type\">");
                    try writeHtmlEscaped(w, display_name);
                    try w.writeAll("</span></a>");
                } else {
                    try w.writeAll("<span class=\"type\">");
                    try writeHtmlEscaped(w, display_name);
                    try w.writeAll("</span>");
                }
            },
            .doc_type => |item| {
                switch (item.value.*) {
                    .type_ref => |ref| {
                        try frames.append(gpa, .{ .type_ref = ref });
                    },
                    .type_var => |name| {
                        try frames.append(gpa, .{ .html = "</span>" });
                        try frames.append(gpa, .{ .escaped = name });
                        try frames.append(gpa, .{ .html = "<span class=\"type-var\">" });
                    },
                    .function => |func| {
                        if (item.needs_parens) try frames.append(gpa, .{ .html = ")" });
                        try frames.append(gpa, .{ .doc_type = .{ .value = func.ret, .needs_parens = false } });
                        try frames.append(gpa, .{ .html = if (func.effectful)
                            "<span class=\"sig-arrow\"> =&gt; </span>"
                        else
                            "<span class=\"sig-arrow\"> -&gt; </span>" });
                        var i = func.args.len;
                        while (i > 0) {
                            i -= 1;
                            try frames.append(gpa, .{ .doc_type = .{ .value = func.args[i], .needs_parens = true } });
                            if (i > 0) try frames.append(gpa, .{ .html = ", " });
                        }
                        if (item.needs_parens) try frames.append(gpa, .{ .html = "(" });
                    },
                    .record => |rec| {
                        try frames.append(gpa, .{ .html = " }" });
                        var i = rec.fields.len;
                        while (i > 0) {
                            i -= 1;
                            const field = rec.fields[i];
                            try frames.append(gpa, .{ .doc_type = .{ .value = field.type, .needs_parens = false } });
                            try frames.append(gpa, .{ .html = " : " });
                            try frames.append(gpa, .{ .escaped = field.name });
                            if (i > 0) try frames.append(gpa, .{ .html = ", " });
                        }
                        if (rec.is_open) {
                            if (rec.fields.len > 0) try frames.append(gpa, .{ .html = ", " });
                            if (rec.ext) |ext| {
                                try frames.append(gpa, .{ .doc_type = .{ .value = ext, .needs_parens = false } });
                            }
                            try frames.append(gpa, .{ .html = ".." });
                        }
                        try frames.append(gpa, .{ .html = "{ " });
                    },
                    .tag_union => |tu| {
                        try frames.append(gpa, .{ .html = "]" });
                        if (tu.is_open) {
                            if (tu.ext) |ext| {
                                try frames.append(gpa, .{ .doc_type = .{ .value = ext, .needs_parens = false } });
                            }
                            try frames.append(gpa, .{ .html = ".." });
                            if (tu.tags.len > 0) try frames.append(gpa, .{ .html = ", " });
                        }
                        var i = tu.tags.len;
                        while (i > 0) {
                            i -= 1;
                            const tag = tu.tags[i];
                            if (tag.args.len > 0) {
                                try frames.append(gpa, .{ .html = ")" });
                                var j = tag.args.len;
                                while (j > 0) {
                                    j -= 1;
                                    try frames.append(gpa, .{ .doc_type = .{ .value = tag.args[j], .needs_parens = false } });
                                    if (j > 0) try frames.append(gpa, .{ .html = ", " });
                                }
                                try frames.append(gpa, .{ .html = "(" });
                            }
                            try frames.append(gpa, .{ .html = "</span>" });
                            try frames.append(gpa, .{ .escaped = tag.name });
                            try frames.append(gpa, .{ .html = "<span class=\"type\">" });
                            if (i > 0) try frames.append(gpa, .{ .html = ", " });
                        }
                        try frames.append(gpa, .{ .html = "[" });
                    },
                    .tuple => |tup| {
                        try frames.append(gpa, .{ .html = ")" });
                        var i = tup.elems.len;
                        while (i > 0) {
                            i -= 1;
                            try frames.append(gpa, .{ .doc_type = .{ .value = tup.elems[i], .needs_parens = false } });
                            if (i > 0) try frames.append(gpa, .{ .html = ", " });
                        }
                        try frames.append(gpa, .{ .html = "(" });
                    },
                    .apply => |app| {
                        try frames.append(gpa, .{ .html = ")" });
                        var i = app.args.len;
                        while (i > 0) {
                            i -= 1;
                            try frames.append(gpa, .{ .doc_type = .{ .value = app.args[i], .needs_parens = false } });
                            if (i > 0) try frames.append(gpa, .{ .html = ", " });
                        }
                        try frames.append(gpa, .{ .html = "(" });
                        try frames.append(gpa, .{ .doc_type = .{ .value = app.constructor, .needs_parens = false } });
                    },
                    .where_clause => |wc| {
                        try frames.append(gpa, .{ .html = " }" });
                        var i = wc.constraints.len;
                        while (i > 0) {
                            i -= 1;
                            const constraint = wc.constraints[i];
                            try frames.append(gpa, .{ .doc_type = .{ .value = constraint.signature, .needs_parens = false } });
                            try frames.append(gpa, .{ .html = " : " });
                            try frames.append(gpa, .{ .escaped = constraint.method_name });
                            try frames.append(gpa, .{ .html = "</span>." });
                            try frames.append(gpa, .{ .escaped = constraint.type_var });
                            try frames.append(gpa, .{ .html = "<span class=\"type-var\">" });
                            if (i > 0) try frames.append(gpa, .{ .html = ", " });
                        }
                        try frames.append(gpa, .{ .html = " <span class=\"kw\">where</span> { " });
                        try frames.append(gpa, .{ .doc_type = .{ .value = wc.type, .needs_parens = item.needs_parens } });
                    },
                    .wildcard => {
                        try frames.append(gpa, .{ .html = "_" });
                    },
                    .@"error" => {
                        try frames.append(gpa, .{ .html = "?" });
                    },
                }
            },
        }
    }
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
) (Allocator.Error || error{WriteFailed})!void {
    // An empty module_path comes from `resolveModulePathFromBase` for `.builtin`
    // references (Str, List, Bool, etc). When we are not documenting the builtins
    // ourselves, point at their published docs instead of a same-page anchor.
    if (module_path.len == 0 and !ctx.documenting_builtin) {
        try writeBuiltinTypeUrl(w, type_name);
        return;
    }

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

    const is_same_module = if (ctx.current_module) |cur|
        std.mem.eql(u8, target_module, cur)
    else
        false;

    // The compiler reports types by their short name (e.g. "U8"), but the HTML
    // anchor for a type nested under a sub-namespace is built from its full
    // path within the module (e.g. "Builtin.Num.U8"). When linking inside the
    // current module, look the head segment up in the anchor map so the link
    // resolves to the actual entry id.
    const first_dot = std.mem.findScalar(u8, type_name, '.');
    const head = if (first_dot) |d| type_name[0..d] else type_name;
    const tail: []const u8 = if (first_dot) |d| type_name[d..] else "";
    const remapped_head: ?[]const u8 = if (is_same_module) ctx.lookupAnchorHead(head) else null;
    const effective_head = remapped_head orelse head;

    // HTML anchors use module-qualified dotted paths (e.g. "Builtin.Str.Utf8Problem").
    // The compiler may provide a bare type_name ("Str") or a partial path
    // ("Str.Utf8Problem"); prepend the module name unless it's already there.
    const already_qualified = std.mem.startsWith(u8, effective_head, target_module) and
        effective_head.len > target_module.len and
        effective_head[target_module.len] == '.';

    if (is_same_module) {
        try w.writeAll("#");
    } else {
        if (ctx.current_module) |_| {
            try w.writeAll("../");
        }
        try writeHtmlEscaped(w, target_module);
        try w.writeAll("/#");
    }

    // The promoted builtin modules use bare anchors (the `<module>.` prefix is
    // stripped from their ids), so drop it from the fragment too.
    if (ctx.isBuiltinDerived(target_module)) {
        const bare_head = if (already_qualified) effective_head[target_module.len + 1 ..] else effective_head;
        try writeHtmlEscaped(w, bare_head);
        try writeHtmlEscaped(w, tail);
        return;
    }

    if (!already_qualified) {
        try writeHtmlEscaped(w, target_module);
        try w.writeAll(".");
    }
    try writeHtmlEscaped(w, effective_head);
    try writeHtmlEscaped(w, tail);
}

fn writeHtmlEscaped(w: Writer, text: []const u8) (Allocator.Error || error{WriteFailed})!void {
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

test "renderDocTypeHtml handles deeply nested types without call stack recursion" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const leaf = try gpa.create(DocType);
    leaf.* = .{ .type_var = try gpa.dupe(u8, "a") };

    var root: *const DocType = leaf;
    for (0..20_000) |_| {
        const constructor = try gpa.create(DocType);
        constructor.* = .{ .type_ref = .{
            .module_path = try gpa.dupe(u8, ""),
            .type_name = try gpa.dupe(u8, "Wrap"),
        } };

        const args = try gpa.alloc(*const DocType, 1);
        args[0] = root;

        const app = try gpa.create(DocType);
        app.* = .{ .apply = .{
            .constructor = constructor,
            .args = args,
        } };
        root = app;
    }
    defer {
        root.deinit(gpa);
        gpa.destroy(root);
    }

    const package_docs = DocModel.PackageDocs{
        .name = "Test",
        .modules = &[_]DocModel.ModuleDocs{},
    };
    var ctx = try RenderContext.init(&package_docs, gpa);
    defer ctx.deinit(gpa);
    ctx.suppress_type_links = true;

    var output: std.Io.Writer.Allocating = .init(gpa);
    defer output.deinit();

    try renderDocTypeHtml(&output.writer, &ctx, gpa, root, false);
    try testing.expect(std.mem.startsWith(u8, output.written(), "<span class=\"type\">Wrap</span>("));
}

test "writeDocRefHref reports broken shorthand refs" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Build a minimal PackageDocs whose Builtin module exposes
    // `Builtin.Num.U8` with a child `div_by`. The doc comment on `div_by`
    // contains a `[div_by]` reference that, per the existing resolver,
    // produces `#Builtin.div_by` — a fragment that does not exist on the
    // page. The renderer should report exactly that ref as broken, while
    // leaving correct refs (`[U8]`, `[U8.div_by]`) alone.
    // Doc spans three lines so we can verify the reported source line equals
    // `doc_comment_start_line + offset_of_line_with_ref`.
    const doc_with_refs = try gpa.dupe(u8,
        \\First line, no refs.
        \\Second line: [U8] is valid.
        \\Third line: [div_by] is broken.
    );
    const div_by_name = try gpa.dupe(u8, "div_by");
    const div_by_entry = DocModel.DocEntry{
        .name = div_by_name,
        .kind = .value,
        .type_signature = null,
        .doc_comment = doc_with_refs,
        .children = try gpa.alloc(DocModel.DocEntry, 0),
        // Pretend the first ## line of this doc lives on source line 41.
        .doc_comment_start_line = 41,
    };

    const u8_children = try gpa.alloc(DocModel.DocEntry, 1);
    u8_children[0] = div_by_entry;
    const u8_name = try gpa.dupe(u8, "Builtin.Num.U8");
    const u8_entry = DocModel.DocEntry{
        .name = u8_name,
        .kind = .nominal,
        .type_signature = null,
        .doc_comment = null,
        .children = u8_children,
    };

    const entries = try gpa.alloc(DocModel.DocEntry, 1);
    entries[0] = u8_entry;
    var module = DocModel.ModuleDocs{
        .name = try gpa.dupe(u8, "Builtin"),
        .package_name = try gpa.dupe(u8, "Builtin"),
        .kind = .package,
        .module_doc = null,
        .entries = entries,
        .source_path = try gpa.dupe(u8, "/fake/Builtin.roc"),
    };
    defer module.deinit(gpa);

    const modules = try gpa.alloc(DocModel.ModuleDocs, 1);
    modules[0] = module;
    // Leak the outer modules slice intentionally — `module.deinit` already
    // freed everything inside, and we want the module struct itself to
    // remain valid for that deinit call. Free the slice here to balance.
    defer gpa.free(modules);

    const package_docs = DocModel.PackageDocs{
        .name = try gpa.dupe(u8, "Builtin"),
        .modules = modules,
    };
    defer gpa.free(package_docs.name);

    // Render to a tmp dir so we exercise the full pipeline.
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const tmp_path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", gpa);
    defer gpa.free(tmp_path);

    var broken_links: std.ArrayListUnmanaged(BrokenLink) = .empty;
    defer {
        for (broken_links.items) |bl| {
            gpa.free(bl.label);
            gpa.free(bl.resolved_anchor);
        }
        broken_links.deinit(gpa);
    }

    try renderPackageDocs(gpa, std.testing.io, &package_docs, tmp_path, &broken_links, null);

    // Exactly the `[div_by]` ref should be flagged. It sits on the third
    // line of the doc, which started at source line 41 → expected line 43.
    try testing.expectEqual(@as(usize, 1), broken_links.items.len);
    const bl = broken_links.items[0];
    try testing.expectEqualStrings("Builtin", bl.source_module);
    try testing.expectEqualStrings("/fake/Builtin.roc", bl.source_path);
    try testing.expectEqual(@as(u32, 43), bl.source_line);
    try testing.expectEqualStrings("div_by", bl.label);
    try testing.expectEqualStrings("Builtin.div_by", bl.resolved_anchor);
}

test "builtin_nested_type_owners lists every numeric type under Num" {
    // External references to builtin numeric types (e.g. `U8` in another
    // package's signature) link to `…/Num#U8` via `builtin_nested_type_owners`.
    // The type system's precision enums are the source of truth for which
    // numeric types exist, so if a new one is added there it must also be added
    // to the table — otherwise its external links would 404 at `…/U8`.
    const tt = @import("types").types;
    inline for (.{ tt.Int.Precision, tt.Frac.Precision }) |Precision| {
        inline for (@typeInfo(Precision).@"enum".fields) |field| {
            var owner: ?[]const u8 = null;
            for (builtin_nested_type_owners) |nested| {
                // Enum fields are lower-case (`u8`, `dec`); the docs names are
                // capitalized (`U8`, `Dec`).
                if (std.ascii.eqlIgnoreCase(nested.name, field.name)) {
                    owner = nested.owner;
                    break;
                }
            }
            if (owner) |o| {
                try std.testing.expectEqualStrings("Num", o);
            } else {
                std.debug.print(
                    "numeric type '{s}' is missing from builtin_nested_type_owners " ++
                        "in render_html.zig; add it (owner \"Num\") so external doc " ++
                        "links resolve to the right page.\n",
                    .{field.name},
                );
                return error.NumericTypeMissingFromTable;
            }
        }
    }
}
