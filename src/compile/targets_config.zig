//! Target link configuration types for platform headers.
//!
//! These types represent the `targets:` section of a platform header,
//! which specifies what files to link for each supported target.
//!
//! Moved from `src/cli/target.zig` to the compile layer so that
//! `parseHeaderDeps()` can extract TargetsConfig during header parsing,
//! eliminating redundant re-parsing in CLI commands.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const check = @import("check");
const CoreCtx = @import("ctx").CoreCtx;
const parse = @import("parse");
const reporting = @import("reporting");
const roc_target = @import("roc_target");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const RocTarget = roc_target.RocTarget;
const checked = check.CheckedArtifact;

/// Individual link item from a targets section.
/// Can be a file path (relative to files/ directory) or a special identifier.
pub const LinkItem = union(enum) {
    /// A file path (string literal in the source).
    /// Path is relative to the targets/<target>/ directory.
    file_path: []const u8,

    /// The compiled Roc application.
    app,

    /// Windows GUI subsystem flag (/subsystem:windows).
    win_gui,
};

/// How a wasm target obtains linear memory.
pub const WasmImportMemory = enum {
    no,
    uninitialized,
    zeroed,

    pub fn importsMemory(self: WasmImportMemory) bool {
        return self != .no;
    }

    pub fn importedMemoryIsZeroed(self: WasmImportMemory) bool {
        return self == .zeroed;
    }

    pub fn fromTagName(name: []const u8) ?WasmImportMemory {
        if (std.mem.eql(u8, name, "No")) return .no;
        if (std.mem.eql(u8, name, "Uninitialized")) return .uninitialized;
        if (std.mem.eql(u8, name, "Zeroed")) return .zeroed;
        return null;
    }
};

/// Optional wasm-specific settings from a target record in a platform header.
pub const WasmTargetConfig = struct {
    /// Final host-visible function exports. Null records an omitted field while
    /// parsing; linked wasm targets reject it before compilation. An explicit
    /// empty slice exports no functions.
    exports: ?[]const []const u8 = null,
    import_memory: WasmImportMemory = .no,
    minimum_memory: ?usize = null,
    maximum_memory: ?usize = null,
    initial_stack_size: ?usize = null,
    global_base: ?u32 = null,
    import_memory_ident: ?[]const u8 = null,
    minimum_memory_ident: ?[]const u8 = null,
    maximum_memory_ident: ?[]const u8 = null,
    initial_stack_size_ident: ?[]const u8 = null,
    global_base_ident: ?[]const u8 = null,

    /// Whether the host owns the addresses below `global_base`, so the stack
    /// sits above static data and compiled code checks its bounds. Known from
    /// the header alone, before a `global_base` naming a constant resolves.
    pub fn reservesLowMemory(self: WasmTargetConfig) bool {
        return self.global_base != null or self.global_base_ident != null;
    }

    fn deinit(self: WasmTargetConfig, allocator: Allocator) void {
        if (self.exports) |exports| {
            for (exports) |name| allocator.free(name);
            allocator.free(exports);
        }
        if (self.import_memory_ident) |ident| allocator.free(ident);
        if (self.minimum_memory_ident) |ident| allocator.free(ident);
        if (self.maximum_memory_ident) |ident| allocator.free(ident);
        if (self.initial_stack_size_ident) |ident| allocator.free(ident);
        if (self.global_base_ident) |ident| allocator.free(ident);
    }
};

/// Reason an identifier-backed target config field could not be resolved.
pub const TargetConfigResolveReason = enum {
    missing_top_level_value,
    not_constant,
    unevaluated_constant,
    unimplemented_declaration,
    expected_import_memory,
    expected_unsigned_integer,
    integer_out_of_range,
    expected_positive_integer,

    pub fn message(self: TargetConfigResolveReason) []const u8 {
        return switch (self) {
            .missing_top_level_value => "does not name a top-level value in the platform module",
            .not_constant => "names a function, but target configuration requires a constant",
            .unevaluated_constant => "does not have a stored compile-time constant value",
            .unimplemented_declaration => "has a type annotation but no implementation",
            .expected_import_memory => "must resolve to Zeroed, Uninitialized, or No",
            .expected_unsigned_integer => "must resolve to a non-negative whole number",
            .integer_out_of_range => "resolves to a number outside the supported range",
            .expected_positive_integer => "must resolve to a whole number greater than 0",
        };
    }
};

/// Context for reporting an invalid identifier-backed target config field.
pub const TargetConfigResolveDiagnostic = struct {
    target: RocTarget,
    output: OutputKind,
    field_name: []const u8,
    ident_name: []const u8,
    reason: TargetConfigResolveReason,
};

/// Which header string a rejected target path came from.
pub const TargetPathField = enum {
    inputs_dir,
    input_file,
};

/// Why a target path string cannot stay inside the platform directory.
pub const TargetPathProblem = enum {
    absolute,
    windows_prefix,
    parent_component,

    pub fn message(self: TargetPathProblem) []const u8 {
        return switch (self) {
            .absolute => "is an absolute path",
            .windows_prefix => "starts with a drive letter or network share prefix",
            .parent_component => "contains a `..` component",
        };
    }

    /// Classify a header path string, returning null when it is a plain
    /// relative path that stays inside the directory it is joined onto.
    /// Both `/` and `\` count as separators so the answer is the same on
    /// every host, whichever OS later resolves the joined path.
    pub fn of(path: []const u8) ?TargetPathProblem {
        if (path.len >= 2 and isPathSeparator(path[0]) and isPathSeparator(path[1])) return .windows_prefix;
        if (path.len >= 1 and isPathSeparator(path[0])) return .absolute;
        if (path.len >= 2 and std.ascii.isAlphabetic(path[0]) and path[1] == ':') return .windows_prefix;

        var components = std.mem.tokenizeAny(u8, path, "/\\");
        while (components.next()) |component| {
            if (std.mem.eql(u8, component, "..")) return .parent_component;
        }
        return null;
    }

    fn isPathSeparator(byte: u8) bool {
        return byte == '/' or byte == '\\';
    }
};

/// A target path string in a platform header that `fromAST` rejected.
pub const InvalidTargetPathDiagnostic = struct {
    field: TargetPathField,
    problem: TargetPathProblem,
    /// The path as written, borrowed from the AST's source.
    path: []const u8,
    /// Source region of the path's string contents.
    region: base.Region,

    /// Build a report that points at the rejected string in the platform
    /// source. `env` must be the environment the AST was parsed from, with
    /// its line starts calculated.
    pub fn toReport(
        self: InvalidTargetPathDiagnostic,
        allocator: Allocator,
        env: *const base.CommonEnv,
        filename: []const u8,
    ) Allocator.Error!reporting.Report {
        const field_description = switch (self.field) {
            .inputs_dir => "The `inputs_dir`",
            .input_file => "This target input file",
        };
        const headline = try std.fmt.allocPrint(
            allocator,
            "{s} path `{s}` {s}.",
            .{ field_description, self.path, self.problem.message() },
        );
        defer allocator.free(headline);

        var report = try reporting.Report.init(allocator, "Invalid Target Path", headline, .runtime_error);
        errdefer report.deinit();
        try report.document.addReflowingTextWithBackticks(
            "Paths in a platform's `targets` section must be relative paths that stay inside the platform directory, so they cannot be absolute, start with a drive letter or network share, or contain `..` components.",
        );
        const region_info = base.RegionInfo.position(env.source, env.line_starts.items.items, self.region.start.offset, self.region.end.offset) catch {
            return report;
        };
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        const owned_filename = try report.addOwnedString(filename);
        try report.addSourceContext(region_info, owned_filename, env.source, env.line_starts.items.items);
        return report;
    }
};

/// A field of a target entry's config record. Some fields have a second
/// accepted spelling; both name the same field.
pub const TargetConfigField = enum {
    inputs,
    output,
    exports,
    import_memory,
    minimum_memory,
    maximum_memory,
    initial_stack_size,
    global_base,

    pub fn fromName(name: []const u8) ?TargetConfigField {
        const spellings = [_]struct { []const u8, TargetConfigField }{
            .{ "inputs", .inputs },
            .{ "output", .output },
            .{ "exports", .exports },
            .{ "import_memory", .import_memory },
            .{ "minimum_memory", .minimum_memory },
            .{ "initial_memory", .minimum_memory },
            .{ "maximum_memory", .maximum_memory },
            .{ "max_memory", .maximum_memory },
            .{ "initial_stack_size", .initial_stack_size },
            .{ "stack_size", .initial_stack_size },
            .{ "global_base", .global_base },
        };
        for (spellings) |spelling| {
            if (std.mem.eql(u8, name, spelling[0])) return spelling[1];
        }
        return null;
    }

    /// Whether the field configures a wasm module, and so belongs only on a
    /// wasm target.
    pub fn wasmOnly(self: TargetConfigField) bool {
        return switch (self) {
            .inputs, .output => false,
            .exports, .import_memory, .minimum_memory, .maximum_memory, .initial_stack_size, .global_base => true,
        };
    }
};

/// Why a target entry's config record was rejected.
pub const TargetEntryProblem = enum {
    unknown_field,
    duplicate_field,
    wasm_only_field,
    unknown_input,
    expected_input_files,
    expected_output_kind,
    expected_export_names,
    expected_import_memory,
    expected_unsigned_integer,
    integer_out_of_range,
    expected_positive_integer,
};

/// A target entry config field in a platform header that `fromAST` rejected.
pub const InvalidTargetEntryDiagnostic = struct {
    problem: TargetEntryProblem,
    target: RocTarget,
    /// The field name as written, or for `unknown_input` the input as
    /// written; borrowed from the AST's source.
    field_name: []const u8,
    /// Source region of `field_name`.
    region: base.Region,

    /// Build a report that points at the rejected field in the platform
    /// source. `env` must be the environment the AST was parsed from, with
    /// its line starts calculated.
    pub fn toReport(
        self: InvalidTargetEntryDiagnostic,
        allocator: Allocator,
        env: *const base.CommonEnv,
        filename: []const u8,
    ) Allocator.Error!reporting.Report {
        const target_name = @tagName(self.target);
        const headline = switch (self.problem) {
            .unknown_field => try std.fmt.allocPrint(allocator, "The {s} target has a field named `{s}`, which is not a target configuration field.", .{ target_name, self.field_name }),
            .duplicate_field => try std.fmt.allocPrint(allocator, "The {s} target sets `{s}` more than once.", .{ target_name, self.field_name }),
            .wasm_only_field => try std.fmt.allocPrint(allocator, "The {s} target sets `{s}`, which only applies to WebAssembly targets.", .{ target_name, self.field_name }),
            .unknown_input => try std.fmt.allocPrint(allocator, "The {s} target's inputs include `{s}`, which is not a known input.", .{ target_name, self.field_name }),
            .expected_input_files => try std.fmt.allocPrint(allocator, "The {s} target's `{s}` must be a list of input files.", .{ target_name, self.field_name }),
            .expected_output_kind => try std.fmt.allocPrint(allocator, "The {s} target's `{s}` must be `Exe`, `Archive`, or `Shared`.", .{ target_name, self.field_name }),
            .expected_export_names => try std.fmt.allocPrint(allocator, "The {s} target's `{s}` must be a list of function names written as strings.", .{ target_name, self.field_name }),
            .expected_import_memory => try std.fmt.allocPrint(allocator, "The {s} target's `{s}` must be `Zeroed`, `Uninitialized`, `No`, or the name of a top-level constant.", .{ target_name, self.field_name }),
            .expected_unsigned_integer => try std.fmt.allocPrint(allocator, "The {s} target's `{s}` must be a non-negative whole number or the name of a top-level constant.", .{ target_name, self.field_name }),
            .integer_out_of_range => try std.fmt.allocPrint(allocator, "The {s} target's `{s}` is outside the supported range.", .{ target_name, self.field_name }),
            .expected_positive_integer => try std.fmt.allocPrint(allocator, "The {s} target's `{s}` must be greater than 0.", .{ target_name, self.field_name }),
        };
        defer allocator.free(headline);

        var report = try reporting.Report.init(allocator, "Invalid Target Configuration", headline, .runtime_error);
        errdefer report.deinit();
        switch (self.problem) {
            .unknown_field => try report.document.addReflowingTextWithBackticks(
                "A target's fields are `inputs` and `output`, and on WebAssembly targets also `exports`, `import_memory`, `minimum_memory`, `maximum_memory`, `initial_stack_size`, and `global_base`.",
            ),
            .unknown_input => try report.document.addReflowingTextWithBackticks(
                "Each input is a file path string, `app` for the compiled application, or `win_gui`.",
            ),
            .duplicate_field,
            .wasm_only_field,
            .expected_input_files,
            .expected_output_kind,
            .expected_export_names,
            .expected_import_memory,
            .expected_unsigned_integer,
            .integer_out_of_range,
            => {},
            .expected_positive_integer => try report.document.addReflowingTextWithBackticks(
                "Every address below `global_base` belongs to the host. A platform whose host reserves no low memory leaves `global_base` out.",
            ),
        }
        const region_info = base.RegionInfo.position(env.source, env.line_starts.items.items, self.region.start.offset, self.region.end.offset) catch {
            return report;
        };
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        const owned_filename = try report.addOwnedString(filename);
        try report.addSourceContext(region_info, owned_filename, env.source, env.line_starts.items.items);
        return report;
    }
};

/// What `fromAST` rejected in a platform header's targets section.
pub const TargetsConfigDiagnostic = union(enum) {
    path: InvalidTargetPathDiagnostic,
    entry: InvalidTargetEntryDiagnostic,

    pub fn toReport(
        self: TargetsConfigDiagnostic,
        allocator: Allocator,
        env: *const base.CommonEnv,
        filename: []const u8,
    ) Allocator.Error!reporting.Report {
        return switch (self) {
            .path => |diagnostic| diagnostic.toReport(allocator, env, filename),
            .entry => |diagnostic| diagnostic.toReport(allocator, env, filename),
        };
    }
};

/// Link specification for a single target.
/// Contains the artifact kind and the ordered list of items to link for this target.
pub const TargetLinkSpec = struct {
    target: RocTarget,
    output: OutputKind,
    items: []const LinkItem,
    wasm: ?WasmTargetConfig = null,

    fn hasDeclaredTargetFiles(self: TargetLinkSpec) bool {
        for (self.items) |item| {
            switch (item) {
                .file_path => return true,
                .app, .win_gui => {},
            }
        }
        return false;
    }
};

fn freeLinkSpec(allocator: Allocator, spec: TargetLinkSpec) void {
    if (spec.wasm) |wasm| wasm.deinit(allocator);
    for (spec.items) |item| switch (item) {
        .file_path => |fp| allocator.free(fp),
        .app, .win_gui => {},
    };
    allocator.free(spec.items);
}

/// Kind of artifact a target entry produces.
pub const OutputKind = enum {
    /// Linked executable. For wasm32, a command module (has an entry).
    exe,
    /// A static archive (.a, .lib) of the declared host inputs and the
    /// compiled app, with input archives flattened in.
    archive,
    /// Shared library (.so, .dylib, .dll). For wasm32, a reactor module
    /// (no entry, the provides entrypoints exported).
    shared,

    /// Parse an output kind from its header tag spelling (Exe, Archive, Shared).
    pub fn fromTagName(name: []const u8) ?OutputKind {
        if (std.mem.eql(u8, name, "Exe")) return .exe;
        if (std.mem.eql(u8, name, "Archive")) return .archive;
        if (std.mem.eql(u8, name, "Shared")) return .shared;
        return null;
    }
};

/// Complete targets configuration from a platform header.
pub const TargetsConfig = struct {
    /// Base directory for target-specific input files (e.g., "targets/").
    inputs_dir: ?[]const u8,

    /// Per-target specifications (in priority order). Each target appears at most once.
    targets: []const TargetLinkSpec,

    /// Free all owned memory. Call this when the TargetsConfig is no longer needed,
    /// but only if it was created via `fromAST()` (which dupes all strings).
    /// Do NOT call this on TargetsConfig values created with comptime/static data.
    pub fn deinit(self: TargetsConfig, allocator: Allocator) void {
        if (self.inputs_dir) |fd| allocator.free(fd);
        for (self.targets) |spec| freeLinkSpec(allocator, spec);
        allocator.free(self.targets);
    }

    /// Get the link spec for a specific target.
    pub fn getLinkSpec(self: TargetsConfig, target: RocTarget) ?TargetLinkSpec {
        for (self.targets) |spec| {
            if (spec.target == target) {
                return spec;
            }
        }
        return null;
    }

    /// Get all supported targets.
    pub fn getSupportedTargets(self: TargetsConfig) []const TargetLinkSpec {
        return self.targets;
    }

    pub fn resolveCheckedConstants(
        self: TargetsConfig,
        allocator: Allocator,
        checked_module: *const checked.CheckedModuleArtifact,
        diagnostic: *TargetConfigResolveDiagnostic,
    ) error{TargetConfigInvalid}!void {
        try resolveLinkTypeCheckedConstants(allocator, checked_module, @constCast(self.targets), diagnostic);
    }

    pub fn validateDeclaredTargetFilesExist(
        self: TargetsConfig,
        allocator: Allocator,
        filesystem: CoreCtx,
        platform_dir: []const u8,
        target: RocTarget,
    ) Allocator.Error!TargetFileValidation {
        var issues = std.ArrayList(TargetFileValidationIssue).empty;
        errdefer {
            for (issues.items) |issue| issue.deinit(allocator);
            issues.deinit(allocator);
        }

        const spec = self.getLinkSpec(target) orelse {
            return .{ .issues = try issues.toOwnedSlice(allocator) };
        };

        if (!spec.hasDeclaredTargetFiles()) return .{ .issues = try issues.toOwnedSlice(allocator) };

        const inputs_dir = self.inputs_dir orelse "targets";
        const inputs_dir_path = try std.fs.path.join(allocator, &.{ platform_dir, inputs_dir });
        var inputs_dir_path_owned = true;
        defer if (inputs_dir_path_owned) allocator.free(inputs_dir_path);

        if (!filesystem.fileExists(inputs_dir_path)) {
            try issues.append(allocator, .{
                .missing_inputs_directory = .{
                    .inputs_dir = inputs_dir,
                    .expected_path = inputs_dir_path,
                },
            });
            inputs_dir_path_owned = false;
            return .{ .issues = try issues.toOwnedSlice(allocator) };
        }

        const target_name = @tagName(spec.target);
        for (spec.items) |item| {
            switch (item) {
                .file_path => |path| {
                    const full_path = try std.fs.path.join(allocator, &.{ platform_dir, inputs_dir, target_name, path });
                    var full_path_owned = true;
                    defer if (full_path_owned) allocator.free(full_path);

                    if (!filesystem.fileExists(full_path)) {
                        try issues.append(allocator, .{
                            .missing_target_file = .{
                                .target = spec.target,
                                .output = spec.output,
                                .file_path = path,
                                .expected_full_path = full_path,
                            },
                        });
                        full_path_owned = false;
                    }
                },
                .app, .win_gui => {},
            }
        }

        return .{ .issues = try issues.toOwnedSlice(allocator) };
    }

    /// Create a TargetsConfig from a parsed AST.
    /// Returns null if the platform header has no targets section. An explicit
    /// empty `targets: {}` section returns a hostless config with zero targets.
    /// All string values are duped with the provided allocator, so the
    /// returned TargetsConfig owns its memory and is independent of the AST.
    /// Returns error.InvalidTargetConfig, describing what it rejected in
    /// `diagnostic`, when `inputs_dir` or a target input file is not a
    /// relative path that stays inside the platform directory, or when a
    /// target's config record has a field or value `parseTargetConfig`
    /// rejects.
    pub fn fromAST(
        allocator: Allocator,
        ast: anytype,
        diagnostic: *TargetsConfigDiagnostic,
    ) (Allocator.Error || error{InvalidTargetConfig})!?TargetsConfig {
        const NodeStore = parse.NodeStore;

        const store: *const NodeStore = &ast.store;

        // Get the file node first, then get the header from it
        const file = store.getFile();
        const header = store.getHeader(file.header);

        // Only platform headers have targets
        if (header != .platform) return null;
        const platform = header.platform;

        // If no targets section, return null
        const targets_section_idx = platform.targets orelse return null;
        const targets_section = store.getTargetsSection(targets_section_idx);

        // Extract inputs_dir from string literal token (StringPart token)
        // Dupe the string so we own the memory
        if (targets_section.inputs_dir) |tok_idx| {
            try checkTargetPath(ast, tok_idx, .inputs_dir, diagnostic);
        }
        const inputs_dir: ?[]const u8 = if (targets_section.inputs_dir) |tok_idx|
            try allocator.dupe(u8, ast.resolve(tok_idx))
        else
            null;
        errdefer if (inputs_dir) |fd| allocator.free(fd);

        const target_specs = try parseTargetSpecs(allocator, store, ast, targets_section.entries, diagnostic);
        errdefer freeTargetSpecs(allocator, target_specs);

        return TargetsConfig{
            .inputs_dir = inputs_dir,
            .targets = target_specs,
        };
    }

    fn checkTargetPath(
        ast: anytype,
        tok: parse.tokenize.Token.Idx,
        field: TargetPathField,
        diagnostic: *TargetsConfigDiagnostic,
    ) error{InvalidTargetConfig}!void {
        const path = ast.resolve(tok);
        const problem = TargetPathProblem.of(path) orelse return;
        diagnostic.* = .{ .path = .{
            .field = field,
            .problem = problem,
            .path = path,
            .region = ast.tokens.resolve(tok),
        } };
        return error.InvalidTargetConfig;
    }

    fn appendTargetFiles(
        allocator: Allocator,
        store: *const parse.NodeStore,
        ast: anytype,
        target: RocTarget,
        files: parse.AST.TargetFile.Span,
        link_items: *std.array_list.Managed(LinkItem),
        diagnostic: *TargetsConfigDiagnostic,
    ) (Allocator.Error || error{InvalidTargetConfig})!void {
        const file_indices = store.targetFileSlice(files);
        for (file_indices) |file_idx| {
            const target_file = store.getTargetFile(file_idx);

            switch (target_file) {
                .string_literal => |maybe_tok| {
                    if (maybe_tok) |tok| try checkTargetPath(ast, tok, .input_file, diagnostic);
                    const path = if (maybe_tok) |tok| ast.resolve(tok) else "";
                    try link_items.append(.{ .file_path = try allocator.dupe(u8, path) });
                },
                .special_ident => |tok| {
                    const ident = ast.resolve(tok);
                    if (std.mem.eql(u8, ident, "app")) {
                        try link_items.append(.app);
                    } else if (std.mem.eql(u8, ident, "win_gui")) {
                        try link_items.append(.win_gui);
                    } else {
                        diagnostic.* = .{ .entry = .{
                            .problem = .unknown_input,
                            .target = target,
                            .field_name = ident,
                            .region = ast.tokens.resolve(tok),
                        } };
                        return error.InvalidTargetConfig;
                    }
                },
                // The parser has already reported a malformed input.
                .malformed => continue,
            }
        }
    }

    fn storeIdent(
        allocator: Allocator,
        ast: anytype,
        ident: parse.tokenize.Token.Idx,
        field: *?[]const u8,
    ) Allocator.Error!void {
        if (field.*) |old_ident| allocator.free(old_ident);
        field.* = try allocator.dupe(u8, ast.resolve(ident));
    }

    const ParsedUnsigned = union(enum) {
        value: usize,
        out_of_range,
        not_unsigned,
    };

    fn parseUnsignedToken(allocator: Allocator, ast: anytype, tok: parse.tokenize.Token.Idx) Allocator.Error!ParsedUnsigned {
        const raw = ast.resolve(tok);
        var compact = try std.array_list.Managed(u8).initCapacity(allocator, raw.len);
        defer compact.deinit();
        for (raw) |byte| {
            if (byte != '_') compact.appendAssumeCapacity(byte);
        }
        if (compact.items.len > 0 and compact.items[0] == '-') return .not_unsigned;
        const parsed = std.fmt.parseInt(usize, compact.items, 0) catch |err| return switch (err) {
            error.Overflow => .out_of_range,
            error.InvalidCharacter => .not_unsigned,
        };
        return .{ .value = parsed };
    }

    /// The names in a wasm `exports` list, or null when an item is not a
    /// non-empty string literal.
    fn parseWasmExports(
        allocator: Allocator,
        store: *const parse.NodeStore,
        ast: anytype,
        values: parse.AST.TargetConfigValue.Span,
    ) Allocator.Error!?[]const []const u8 {
        var exports = std.array_list.Managed([]const u8).init(allocator);
        defer {
            for (exports.items) |name| allocator.free(name);
            exports.deinit();
        }

        for (store.targetConfigValueSlice(values)) |value_idx| {
            const value = store.getTargetConfigValue(value_idx);
            if (value != .string_literal) return null;
            const name = value.string_literal orelse return null;
            try exports.append(try allocator.dupe(u8, ast.resolve(name)));
        }
        return try exports.toOwnedSlice();
    }

    /// The output kind and wasm settings one target entry's config record
    /// declares.
    const ParsedTargetConfig = struct {
        output: OutputKind,
        wasm: ?WasmTargetConfig,
    };

    /// Parse one target entry's config record. Every field must be a known
    /// field given at most once, wasm settings may only appear on a wasm
    /// target, and every value must have its field's shape; anything else is
    /// reported in `diagnostic` rather than ignored, because linking without
    /// a setting the platform wrote would silently change its output.
    fn parseTargetConfig(
        allocator: Allocator,
        store: *const parse.NodeStore,
        ast: anytype,
        target: RocTarget,
        config_idx: parse.AST.TargetConfig.Idx,
        link_items: *std.array_list.Managed(LinkItem),
        diagnostic: *TargetsConfigDiagnostic,
    ) (Allocator.Error || error{InvalidTargetConfig})!ParsedTargetConfig {
        const config = store.getTargetConfig(config_idx);
        const entries = store.targetConfigEntrySlice(config.entries);
        var output: OutputKind = .exe;
        var wasm = WasmTargetConfig{};
        errdefer wasm.deinit(allocator);
        var seen: std.EnumSet(TargetConfigField) = .empty;

        for (entries) |entry_idx| {
            const entry = store.getTargetConfigEntry(entry_idx);
            const name = ast.resolve(entry.name);
            const value = store.getTargetConfigValue(entry.value);
            const reject = struct {
                fn at(
                    diag: *TargetsConfigDiagnostic,
                    problem: TargetEntryProblem,
                    target_tag: RocTarget,
                    field_name: []const u8,
                    region: base.Region,
                ) error{InvalidTargetConfig} {
                    diag.* = .{ .entry = .{ .problem = problem, .target = target_tag, .field_name = field_name, .region = region } };
                    return error.InvalidTargetConfig;
                }
            }.at;
            const name_region = ast.tokens.resolve(entry.name);

            const field = TargetConfigField.fromName(name) orelse
                return reject(diagnostic, .unknown_field, target, name, name_region);
            if (seen.contains(field)) return reject(diagnostic, .duplicate_field, target, name, name_region);
            seen.insert(field);
            if (field.wasmOnly() and target.toCpuArch() != .wasm32) {
                return reject(diagnostic, .wasm_only_field, target, name, name_region);
            }
            // The parser has already reported a malformed value.
            if (value == .malformed) continue;

            switch (field) {
                .inputs => {
                    if (value != .files) return reject(diagnostic, .expected_input_files, target, name, name_region);
                    try appendTargetFiles(allocator, store, ast, target, value.files, link_items, diagnostic);
                },
                .output => {
                    if (value != .tag_literal) return reject(diagnostic, .expected_output_kind, target, name, name_region);
                    output = OutputKind.fromTagName(ast.resolve(value.tag_literal)) orelse
                        return reject(diagnostic, .expected_output_kind, target, name, name_region);
                },
                .exports => {
                    if (value != .list) return reject(diagnostic, .expected_export_names, target, name, name_region);
                    wasm.exports = try parseWasmExports(allocator, store, ast, value.list) orelse
                        return reject(diagnostic, .expected_export_names, target, name, name_region);
                },
                .import_memory => switch (value) {
                    .tag_literal => |tag| wasm.import_memory = WasmImportMemory.fromTagName(ast.resolve(tag)) orelse
                        return reject(diagnostic, .expected_import_memory, target, name, name_region),
                    .ident => |ident| try storeIdent(allocator, ast, ident, &wasm.import_memory_ident),
                    .int_literal, .string_literal, .list, .files, .malformed => return reject(diagnostic, .expected_import_memory, target, name, name_region),
                },
                .minimum_memory => try parseUnsignedField(usize, allocator, ast, value, &wasm.minimum_memory, &wasm.minimum_memory_ident, target, name, name_region, diagnostic),
                .maximum_memory => try parseUnsignedField(usize, allocator, ast, value, &wasm.maximum_memory, &wasm.maximum_memory_ident, target, name, name_region, diagnostic),
                .initial_stack_size => try parseUnsignedField(usize, allocator, ast, value, &wasm.initial_stack_size, &wasm.initial_stack_size_ident, target, name, name_region, diagnostic),
                .global_base => {
                    try parseUnsignedField(u32, allocator, ast, value, &wasm.global_base, &wasm.global_base_ident, target, name, name_region, diagnostic);
                    if (wasm.global_base == 0) return reject(diagnostic, .expected_positive_integer, target, name, name_region);
                },
            }
        }

        var has_wasm_config = false;
        var fields = seen.iterator();
        while (fields.next()) |field| has_wasm_config = has_wasm_config or field.wasmOnly();
        return .{ .output = output, .wasm = if (has_wasm_config) wasm else null };
    }

    /// Store an unsigned wasm setting written as a literal that fits in `T`,
    /// or the top-level identifier that will supply it after checking.
    fn parseUnsignedField(
        comptime T: type,
        allocator: Allocator,
        ast: anytype,
        value: parse.AST.TargetConfigValue,
        out: *?T,
        ident_slot: *?[]const u8,
        target: RocTarget,
        field_name: []const u8,
        region: base.Region,
        diagnostic: *TargetsConfigDiagnostic,
    ) (Allocator.Error || error{InvalidTargetConfig})!void {
        const problem: TargetEntryProblem = switch (value) {
            .ident => |ident| return storeIdent(allocator, ast, ident, ident_slot),
            .int_literal => |tok| switch (try parseUnsignedToken(allocator, ast, tok)) {
                .value => |parsed| if (std.math.cast(T, parsed)) |narrowed| {
                    out.* = narrowed;
                    return;
                } else .integer_out_of_range,
                .out_of_range => .integer_out_of_range,
                .not_unsigned => .expected_unsigned_integer,
            },
            .string_literal, .tag_literal, .list, .files, .malformed => .expected_unsigned_integer,
        };
        diagnostic.* = .{ .entry = .{ .problem = problem, .target = target, .field_name = field_name, .region = region } };
        return error.InvalidTargetConfig;
    }

    /// Parse per-target specs from the targets section entries.
    fn parseTargetSpecs(
        allocator: Allocator,
        store: *const parse.NodeStore,
        ast: anytype,
        entries: parse.AST.TargetEntry.Span,
        diagnostic: *TargetsConfigDiagnostic,
    ) (Allocator.Error || error{InvalidTargetConfig})![]const TargetLinkSpec {
        const entry_indices = store.targetEntrySlice(entries);

        var specs = std.array_list.Managed(TargetLinkSpec).init(allocator);
        errdefer {
            for (specs.items) |spec| freeLinkSpec(allocator, spec);
            specs.deinit();
        }

        for (entry_indices) |entry_idx| {
            const entry = store.getTargetEntry(entry_idx);

            // Parse target name from token
            const target_name = ast.resolve(entry.target);
            const target = RocTarget.fromString(target_name) orelse continue; // Skip unknown targets

            var link_items = std.array_list.Managed(LinkItem).init(allocator);
            errdefer {
                for (link_items.items) |item| switch (item) {
                    .file_path => |fp| allocator.free(fp),
                    .app, .win_gui => {},
                };
                link_items.deinit();
            }

            const parsed = try parseTargetConfig(allocator, store, ast, target, entry.config, &link_items, diagnostic);
            errdefer if (parsed.wasm) |wasm| wasm.deinit(allocator);

            try specs.append(.{
                .target = target,
                .output = parsed.output,
                .items = try link_items.toOwnedSlice(),
                .wasm = parsed.wasm,
            });
        }

        return try specs.toOwnedSlice();
    }

    /// Free target specs allocated by parseTargetSpecs.
    fn freeTargetSpecs(allocator: Allocator, specs: []const TargetLinkSpec) void {
        for (specs) |spec| freeLinkSpec(allocator, spec);
        allocator.free(specs);
    }
};

/// Result of checking that every declared target file exists on disk.
pub const TargetFileValidation = struct {
    issues: []TargetFileValidationIssue,

    pub fn deinit(self: TargetFileValidation, allocator: Allocator) void {
        for (self.issues) |issue| issue.deinit(allocator);
        allocator.free(self.issues);
    }

    pub fn hasErrors(self: TargetFileValidation) bool {
        return self.issues.len > 0;
    }

    pub fn hasMissingInputsDirectory(self: TargetFileValidation) bool {
        for (self.issues) |issue| {
            switch (issue) {
                .missing_inputs_directory => return true,
                .missing_target_file => {},
            }
        }
        return false;
    }
};

/// A file-system issue found while validating declared target files.
pub const TargetFileValidationIssue = union(enum) {
    missing_inputs_directory: struct {
        inputs_dir: []const u8,
        expected_path: []const u8,
    },
    missing_target_file: struct {
        target: RocTarget,
        output: OutputKind,
        file_path: []const u8,
        expected_full_path: []const u8,
    },

    fn deinit(self: TargetFileValidationIssue, allocator: Allocator) void {
        switch (self) {
            .missing_inputs_directory => |issue| allocator.free(issue.expected_path),
            .missing_target_file => |issue| allocator.free(issue.expected_full_path),
        }
    }
};

fn resolveLinkTypeCheckedConstants(
    allocator: Allocator,
    checked_module: *const checked.CheckedModuleArtifact,
    specs: []TargetLinkSpec,
    diagnostic: *TargetConfigResolveDiagnostic,
) error{TargetConfigInvalid}!void {
    for (specs) |*spec| {
        var wasm = spec.wasm orelse continue;
        resolveWasmCheckedConstants(allocator, checked_module, spec.target, spec.output, &wasm, diagnostic) catch |err| {
            spec.wasm = wasm;
            return err;
        };
        spec.wasm = wasm;
    }
}

fn resolveWasmCheckedConstants(
    allocator: Allocator,
    checked_module: *const checked.CheckedModuleArtifact,
    target: RocTarget,
    output: OutputKind,
    wasm: *WasmTargetConfig,
    diagnostic: *TargetConfigResolveDiagnostic,
) error{TargetConfigInvalid}!void {
    try resolveWasmImportMemoryField(allocator, checked_module, target, output, "import_memory", &wasm.import_memory, &wasm.import_memory_ident, diagnostic);
    try resolveWasmUnsignedField(usize, allocator, checked_module, target, output, "minimum_memory", 0, &wasm.minimum_memory, &wasm.minimum_memory_ident, diagnostic);
    try resolveWasmUnsignedField(usize, allocator, checked_module, target, output, "maximum_memory", 0, &wasm.maximum_memory, &wasm.maximum_memory_ident, diagnostic);
    try resolveWasmUnsignedField(usize, allocator, checked_module, target, output, "initial_stack_size", 0, &wasm.initial_stack_size, &wasm.initial_stack_size_ident, diagnostic);
    try resolveWasmUnsignedField(u32, allocator, checked_module, target, output, "global_base", 1, &wasm.global_base, &wasm.global_base_ident, diagnostic);
}

fn resolveWasmImportMemoryField(
    allocator: Allocator,
    checked_module: *const checked.CheckedModuleArtifact,
    target: RocTarget,
    output: OutputKind,
    field_name: []const u8,
    out: *WasmImportMemory,
    ident_slot: *?[]const u8,
    diagnostic: *TargetConfigResolveDiagnostic,
) error{TargetConfigInvalid}!void {
    const ident = ident_slot.* orelse return;
    var reason: TargetConfigResolveReason = .missing_top_level_value;
    const node = topLevelConstNode(checked_module, ident, &reason) orelse {
        diagnostic.* = .{ .target = target, .output = output, .field_name = field_name, .ident_name = ident, .reason = reason };
        return error.TargetConfigInvalid;
    };
    const value = constWasmImportMemory(checked_module, node) orelse {
        diagnostic.* = .{ .target = target, .output = output, .field_name = field_name, .ident_name = ident, .reason = .expected_import_memory };
        return error.TargetConfigInvalid;
    };
    out.* = value;
    allocator.free(ident);
    ident_slot.* = null;
}

/// Resolves an unsigned wasm config field named by `ident_slot` to a value
/// that fits in `T` and is at least `minimum`, which is 0 or 1.
fn resolveWasmUnsignedField(
    comptime T: type,
    allocator: Allocator,
    checked_module: *const checked.CheckedModuleArtifact,
    target: RocTarget,
    output: OutputKind,
    field_name: []const u8,
    minimum: T,
    out: *?T,
    ident_slot: *?[]const u8,
    diagnostic: *TargetConfigResolveDiagnostic,
) error{TargetConfigInvalid}!void {
    const ident = ident_slot.* orelse return;
    var reason: TargetConfigResolveReason = .expected_unsigned_integer;
    const node = topLevelConstNode(checked_module, ident, &reason) orelse {
        diagnostic.* = .{ .target = target, .output = output, .field_name = field_name, .ident_name = ident, .reason = reason };
        return error.TargetConfigInvalid;
    };
    const value = constUnsigned(checked_module, node, &reason) orelse {
        diagnostic.* = .{ .target = target, .output = output, .field_name = field_name, .ident_name = ident, .reason = reason };
        return error.TargetConfigInvalid;
    };
    const narrowed = std.math.cast(T, value) orelse {
        diagnostic.* = .{ .target = target, .output = output, .field_name = field_name, .ident_name = ident, .reason = .integer_out_of_range };
        return error.TargetConfigInvalid;
    };
    if (narrowed < minimum) {
        diagnostic.* = .{ .target = target, .output = output, .field_name = field_name, .ident_name = ident, .reason = .expected_positive_integer };
        return error.TargetConfigInvalid;
    }
    out.* = narrowed;
    allocator.free(ident);
    ident_slot.* = null;
}

fn topLevelConstNode(
    checked_module: *const checked.CheckedModuleArtifact,
    ident: []const u8,
    reason: *TargetConfigResolveReason,
) ?checked.ConstNodeId {
    for (checked_module.top_level_values.entries) |entry| {
        const source_name = checked_module.canonical_names.exportNameText(entry.source_name);
        if (!Ident.textEql(source_name, ident)) continue;

        const const_ref = switch (entry.value) {
            .const_ref => |ref| ref,
            .procedure_binding => {
                reason.* = .not_constant;
                return null;
            },
        };
        const template = checked_module.const_templates.get(const_ref);
        return switch (template.state) {
            .stored_const => |stored| stored.node,
            .reserved, .eval_template => blk: {
                reason.* = .unevaluated_constant;
                break :blk null;
            },
            .unimplemented => blk: {
                reason.* = .unimplemented_declaration;
                break :blk null;
            },
        };
    }

    reason.* = .missing_top_level_value;
    return null;
}

fn constWasmImportMemory(checked_module: *const checked.CheckedModuleArtifact, node: checked.ConstNodeId) ?WasmImportMemory {
    const value = checked_module.const_store.get(node);
    if (value == .nominal) return constWasmImportMemory(checked_module, value.nominal.backing);
    if (value != .tag or value.tag.payloads.len != 0) return null;
    return WasmImportMemory.fromTagName(value.tag.tag_name);
}

fn constUnsigned(
    checked_module: *const checked.CheckedModuleArtifact,
    node: checked.ConstNodeId,
    reason: *TargetConfigResolveReason,
) ?usize {
    const value = checked_module.const_store.get(node);
    if (value == .nominal) return constUnsigned(checked_module, value.nominal.backing, reason);
    if (value == .scalar) return scalarUnsigned(value.scalar, reason);
    reason.* = .expected_unsigned_integer;
    return null;
}

fn scalarUnsigned(scalar: checked.ConstScalar, reason: *TargetConfigResolveReason) ?usize {
    const value: u128 = switch (scalar) {
        inline .u8, .u16, .u32, .u64, .u128 => |v| v,
        inline .i8, .i16, .i32, .i64, .i128 => |v| signedScalarUnsigned(v, reason) orelse return null,
        .dec_bits => |v| decScalarUnsigned(v, reason) orelse return null,
        .f32_bits, .f64_bits => {
            reason.* = .expected_unsigned_integer;
            return null;
        },
    };

    if (value > std.math.maxInt(usize)) {
        reason.* = .integer_out_of_range;
        return null;
    }
    return @intCast(value);
}

fn signedScalarUnsigned(value: anytype, reason: *TargetConfigResolveReason) ?u128 {
    if (value < 0) {
        reason.* = .expected_unsigned_integer;
        return null;
    }
    return @intCast(value);
}

fn decScalarUnsigned(value: i128, reason: *TargetConfigResolveReason) ?u128 {
    const scale = builtins.dec.RocDec.one_point_zero_i128;
    if (value < 0 or @rem(value, scale) != 0) {
        reason.* = .expected_unsigned_integer;
        return null;
    }
    const whole = @divTrunc(value, scale);
    return @intCast(whole);
}

// Tests
const testing = std.testing;

test "getLinkSpec returns correct spec for supported target" {
    const config = TargetsConfig{
        .inputs_dir = "targets",
        .targets = &.{
            .{ .target = .x64mac, .output = .exe, .items = &.{ .{ .file_path = "libhost.a" }, .app } },
            .{ .target = .arm64mac, .output = .shared, .items = &.{.app} },
        },
    };

    const spec = config.getLinkSpec(.x64mac);
    try testing.expect(spec != null);
    try testing.expectEqual(RocTarget.x64mac, spec.?.target);
    try testing.expectEqual(OutputKind.exe, spec.?.output);
    try testing.expectEqual(@as(usize, 2), spec.?.items.len);

    const shared_spec = config.getLinkSpec(.arm64mac);
    try testing.expectEqual(OutputKind.shared, shared_spec.?.output);
}

test "getLinkSpec returns null for unsupported target" {
    const config = TargetsConfig{
        .inputs_dir = "targets",
        .targets = &.{
            .{ .target = .x64mac, .output = .exe, .items = &.{.app} },
        },
    };

    // x64musl is not in the config
    const spec = config.getLinkSpec(.x64musl);
    try testing.expect(spec == null);
}

test "fromAST accepts explicit hostless targets section" {
    const allocator = testing.allocator;

    const source =
        \\platform ""
        \\    requires { make_glue : List({}) -> Try(List({}), Str) }
        \\    exposes []
        \\    packages {}
    ++ "\n    provides { \"" ++ @import("builtins").shim_symbols.roc_make_glue ++ "\": make_glue_for_host }\n" ++
        \\    targets: {}
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    const ast = try parse.file(allocator, &env);
    defer ast.deinit();

    try testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    var diagnostic: TargetsConfigDiagnostic = undefined;
    const maybe_config = try TargetsConfig.fromAST(allocator, ast, &diagnostic);
    try testing.expect(maybe_config != null);

    const config = maybe_config.?;
    defer config.deinit(allocator);

    try testing.expect(config.inputs_dir == null);
    try testing.expectEqual(@as(usize, 0), config.targets.len);
}

test "fromAST captures explicit wasm exports" {
    const allocator = testing.allocator;

    const source =
        \\platform ""
        \\    requires { main : {} }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host }
        \\    targets: {
        \\        inputs_dir: "targets/",
        \\        wasm32: {
        \\            inputs: ["host.wasm", app],
        \\            output: Shared,
        \\            exports: ["start", "update"],
        \\        },
        \\    }
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    const ast = try parse.file(allocator, &env);
    defer ast.deinit();

    try testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    var diagnostic: TargetsConfigDiagnostic = undefined;
    const maybe_config = try TargetsConfig.fromAST(allocator, ast, &diagnostic);
    try testing.expect(maybe_config != null);

    const config = maybe_config.?;
    defer config.deinit(allocator);

    try testing.expectEqual(@as(usize, 1), config.targets.len);
    const exports = config.targets[0].wasm.?.exports.?;
    try testing.expectEqual(@as(usize, 2), exports.len);
    try testing.expectEqualStrings("start", exports[0]);
    try testing.expectEqualStrings("update", exports[1]);
}

test "fromAST captures punned wasm identifier config" {
    const allocator = testing.allocator;

    const source =
        \\platform ""
        \\    requires { main : {} }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host }
        \\    targets: {
        \\        inputs_dir: "targets/",
        \\        wasm32: {
        \\            inputs: ["libhost.a", app],
        \\            output: Shared,
        \\            exports: [],
        \\            import_memory,
        \\            minimum_memory,
        \\            maximum_memory,
        \\            initial_stack_size,
        \\            global_base,
        \\        },
        \\    }
        \\
        \\import_memory = Zeroed
        \\minimum_memory = 65536
        \\maximum_memory = 65536
        \\initial_stack_size = 14752
        \\global_base = 6592
        \\
    ;

    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    const ast = try parse.file(allocator, &env);
    defer ast.deinit();

    try testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    var diagnostic: TargetsConfigDiagnostic = undefined;
    const maybe_config = try TargetsConfig.fromAST(allocator, ast, &diagnostic);
    try testing.expect(maybe_config != null);

    const config = maybe_config.?;
    defer config.deinit(allocator);

    try testing.expectEqual(@as(usize, 1), config.targets.len);
    try testing.expectEqual(OutputKind.shared, config.targets[0].output);
    const wasm = config.targets[0].wasm orelse return error.TestUnexpectedResult;
    try testing.expectEqualStrings("import_memory", wasm.import_memory_ident.?);
    try testing.expectEqualStrings("minimum_memory", wasm.minimum_memory_ident.?);
    try testing.expectEqualStrings("maximum_memory", wasm.maximum_memory_ident.?);
    try testing.expectEqualStrings("initial_stack_size", wasm.initial_stack_size_ident.?);
    try testing.expectEqualStrings("global_base", wasm.global_base_ident.?);
}

test "validateDeclaredTargetFilesExist checks selected target files" {
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDir(testing.io, "targets", .default_dir);
    try tmp.dir.createDir(testing.io, "targets/x64mac", .default_dir);
    try tmp.dir.createDir(testing.io, "targets/wasm32", .default_dir);
    try tmp.dir.writeFile(testing.io, .{ .sub_path = "targets/x64mac/libhost.a", .data = "" });

    const platform_dir = try tmp.dir.realPathFileAlloc(testing.io, ".", allocator);
    defer allocator.free(platform_dir);
    const filesystem = CoreCtx.default(allocator, allocator, testing.io);

    const x64_items: []const LinkItem = &.{ .{ .file_path = "libhost.a" }, .app };
    const wasm_items: []const LinkItem = &.{ .{ .file_path = "host.wasm" }, .app };
    const specs: []const TargetLinkSpec = &.{
        .{ .target = .x64mac, .output = .shared, .items = x64_items },
        .{ .target = .wasm32, .output = .shared, .items = wasm_items },
    };
    const config = TargetsConfig{
        .inputs_dir = "targets",
        .targets = specs,
    };

    const x64_validation = try config.validateDeclaredTargetFilesExist(allocator, filesystem, platform_dir, .x64mac);
    defer x64_validation.deinit(allocator);

    try testing.expectEqual(@as(usize, 0), x64_validation.issues.len);

    const wasm_validation = try config.validateDeclaredTargetFilesExist(allocator, filesystem, platform_dir, .wasm32);
    defer wasm_validation.deinit(allocator);

    try testing.expectEqual(@as(usize, 1), wasm_validation.issues.len);
    try testing.expect(wasm_validation.issues[0] == .missing_target_file);
    const missing_file = wasm_validation.issues[0].missing_target_file;
    try testing.expectEqual(RocTarget.wasm32, missing_file.target);
    try testing.expectEqual(OutputKind.shared, missing_file.output);
    try testing.expectEqualStrings("host.wasm", missing_file.file_path);
    const expected_path = try std.fs.path.join(allocator, &.{ platform_dir, "targets", "wasm32", "host.wasm" });
    defer allocator.free(expected_path);
    try testing.expectEqualStrings(expected_path, missing_file.expected_full_path);
}

test "validateDeclaredTargetFilesExist uses default targets directory" {
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const platform_dir = try tmp.dir.realPathFileAlloc(testing.io, ".", allocator);
    defer allocator.free(platform_dir);
    const filesystem = CoreCtx.default(allocator, allocator, testing.io);

    const items: []const LinkItem = &.{ .{ .file_path = "libhost.a" }, .app };
    const specs: []const TargetLinkSpec = &.{
        .{ .target = .x64mac, .output = .exe, .items = items },
    };
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = specs,
    };

    const validation = try config.validateDeclaredTargetFilesExist(allocator, filesystem, platform_dir, .x64mac);
    defer validation.deinit(allocator);

    try testing.expectEqual(@as(usize, 1), validation.issues.len);
    try testing.expect(validation.issues[0] == .missing_inputs_directory);
    const missing_dir = validation.issues[0].missing_inputs_directory;
    try testing.expectEqualStrings("targets", missing_dir.inputs_dir);
    try testing.expect(std.mem.endsWith(u8, missing_dir.expected_path, "targets"));
}

fn platformSourceWithTargets(comptime targets: []const u8) []const u8 {
    return
    \\platform ""
    \\    requires { main : {} }
    \\    exposes []
    \\    packages {}
    \\    provides { "roc_main": main_for_host }
    \\    targets: {
    \\
    ++ targets ++
        \\    }
        \\
    ;
}

fn expectInvalidTargetPath(
    comptime targets: []const u8,
    expected_field: TargetPathField,
    expected_problem: TargetPathProblem,
    expected_path: []const u8,
) (Allocator.Error || error{ TestExpectedEqual, TestExpectedError })!void {
    const allocator = testing.allocator;

    const source_copy = try allocator.dupe(u8, platformSourceWithTargets(targets));
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    const ast = try parse.file(allocator, &env);
    defer ast.deinit();

    try testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    var diagnostic: TargetsConfigDiagnostic = undefined;
    const result = TargetsConfig.fromAST(allocator, ast, &diagnostic);
    if (result) |maybe_config| {
        if (maybe_config) |config| config.deinit(allocator);
        return error.TestExpectedError;
    } else |err| {
        try testing.expectEqual(error.InvalidTargetConfig, err);
    }

    const path_diagnostic = diagnostic.path;
    try testing.expectEqual(expected_field, path_diagnostic.field);
    try testing.expectEqual(expected_problem, path_diagnostic.problem);
    try testing.expectEqualStrings(expected_path, path_diagnostic.path);
    try testing.expectEqualStrings(
        expected_path,
        source_copy[path_diagnostic.region.start.offset..path_diagnostic.region.end.offset],
    );
}

test "fromAST rejects target file with leading parent component" {
    try expectInvalidTargetPath(
        \\        x64linux: { inputs: ["../../../../etc/secret.a", app] },
        \\
    , .input_file, .parent_component, "../../../../etc/secret.a");
}

test "fromAST rejects target file with parent component in the middle" {
    try expectInvalidTargetPath(
        \\        x64linux: { inputs: ["sub/../../escape.a", app] },
        \\
    , .input_file, .parent_component, "sub/../../escape.a");
}

test "fromAST rejects target file with backslash parent component" {
    try expectInvalidTargetPath(
        \\        x64linux: { inputs: ["sub\\..\\..\\escape.a", app] },
        \\
    , .input_file, .parent_component, "sub\\\\..\\\\..\\\\escape.a");
}

test "fromAST rejects absolute target file" {
    try expectInvalidTargetPath(
        \\        x64linux: { inputs: ["/abs/libhost.a", app] },
        \\
    , .input_file, .absolute, "/abs/libhost.a");
}

test "fromAST rejects target file with drive prefix" {
    try expectInvalidTargetPath(
        \\        x64linux: { inputs: ["C:\\x\\libhost.a", app] },
        \\
    , .input_file, .windows_prefix, "C:\\\\x\\\\libhost.a");
}

test "fromAST rejects target file with UNC prefix" {
    try expectInvalidTargetPath(
        \\        x64linux: { inputs: ["\\\\server\\share\\libhost.a", app] },
        \\
    , .input_file, .windows_prefix, "\\\\\\\\server\\\\share\\\\libhost.a");
}

test "fromAST rejects inputs_dir with leading parent component" {
    try expectInvalidTargetPath(
        \\        inputs_dir: "../../outside",
        \\        x64linux: { inputs: ["libhost.a", app] },
        \\
    , .inputs_dir, .parent_component, "../../outside");
}

test "fromAST rejects inputs_dir with parent component in the middle" {
    try expectInvalidTargetPath(
        \\        inputs_dir: "targets/../..",
        \\        x64linux: { inputs: ["libhost.a", app] },
        \\
    , .inputs_dir, .parent_component, "targets/../..");
}

test "fromAST rejects absolute inputs_dir" {
    try expectInvalidTargetPath(
        \\        inputs_dir: "/abs",
        \\        x64linux: { inputs: ["libhost.a", app] },
        \\
    , .inputs_dir, .absolute, "/abs");
}

test "fromAST rejects inputs_dir with drive prefix" {
    try expectInvalidTargetPath(
        \\        inputs_dir: "C:\\x",
        \\        x64linux: { inputs: ["libhost.a", app] },
        \\
    , .inputs_dir, .windows_prefix, "C:\\\\x");
}

test "fromAST rejects inputs_dir with UNC prefix" {
    try expectInvalidTargetPath(
        \\        inputs_dir: "\\\\server\\share",
        \\        x64linux: { inputs: ["libhost.a", app] },
        \\
    , .inputs_dir, .windows_prefix, "\\\\\\\\server\\\\share");
}

fn expectInvalidTargetEntry(
    comptime targets: []const u8,
    expected_problem: TargetEntryProblem,
    expected_target: RocTarget,
    expected_name: []const u8,
) (Allocator.Error || error{ TestExpectedEqual, TestExpectedError })!void {
    const allocator = testing.allocator;

    const source_copy = try allocator.dupe(u8, platformSourceWithTargets(targets));
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    const ast = try parse.file(allocator, &env);
    defer ast.deinit();

    try testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    var diagnostic: TargetsConfigDiagnostic = undefined;
    const result = TargetsConfig.fromAST(allocator, ast, &diagnostic);
    if (result) |maybe_config| {
        if (maybe_config) |config| config.deinit(allocator);
        return error.TestExpectedError;
    } else |err| {
        try testing.expectEqual(error.InvalidTargetConfig, err);
    }

    const entry_diagnostic = diagnostic.entry;
    try testing.expectEqual(expected_problem, entry_diagnostic.problem);
    try testing.expectEqual(expected_target, entry_diagnostic.target);
    try testing.expectEqualStrings(expected_name, entry_diagnostic.field_name);
    try testing.expectEqualStrings(
        expected_name,
        source_copy[entry_diagnostic.region.start.offset..entry_diagnostic.region.end.offset],
    );
}

test "fromAST rejects an unknown target field" {
    try expectInvalidTargetEntry(
        \\        wasm32: { inputs: ["host.wasm", app], exports: [], stack_first: False },
        \\
    , .unknown_field, .wasm32, "stack_first");
}

test "fromAST rejects a field given twice, including through its other spelling" {
    try expectInvalidTargetEntry(
        \\        wasm32: { inputs: ["host.wasm", app], exports: [], minimum_memory: 65536, initial_memory: 131072 },
        \\
    , .duplicate_field, .wasm32, "initial_memory");
}

test "fromAST rejects a wasm field on a native target" {
    try expectInvalidTargetEntry(
        \\        x64linux: { inputs: ["host.o", app], global_base: 1024 },
        \\
    , .wasm_only_field, .x64linux, "global_base");
}

test "fromAST rejects an unknown input identifier" {
    try expectInvalidTargetEntry(
        \\        x64linux: { inputs: ["host.o", ap] },
        \\
    , .unknown_input, .x64linux, "ap");
}

test "fromAST rejects an unknown output kind" {
    try expectInvalidTargetEntry(
        \\        x64linux: { inputs: ["host.o", app], output: Library },
        \\
    , .expected_output_kind, .x64linux, "output");
}

test "fromAST rejects an export that is not a string" {
    try expectInvalidTargetEntry(
        \\        wasm32: { inputs: ["host.wasm", app], exports: ["start", 7] },
        \\
    , .expected_export_names, .wasm32, "exports");
}

test "fromAST rejects an unknown import_memory tag" {
    try expectInvalidTargetEntry(
        \\        wasm32: { inputs: ["host.wasm", app], exports: [], import_memory: Shared },
        \\
    , .expected_import_memory, .wasm32, "import_memory");
}

test "fromAST rejects a memory size that is not a whole number" {
    try expectInvalidTargetEntry(
        \\        wasm32: { inputs: ["host.wasm", app], exports: [], maximum_memory: "lots" },
        \\
    , .expected_unsigned_integer, .wasm32, "maximum_memory");
}

test "fromAST rejects a global_base literal outside u32" {
    try expectInvalidTargetEntry(
        \\        wasm32: { inputs: ["host.wasm", app], exports: [], global_base: 0x1_0000_0000 },
        \\
    , .integer_out_of_range, .wasm32, "global_base");
}

test "fromAST rejects a zero global_base" {
    try expectInvalidTargetEntry(
        \\        wasm32: { inputs: ["host.wasm", app], exports: [], global_base: 0 },
        \\
    , .expected_positive_integer, .wasm32, "global_base");
}

test "fromAST accepts nested relative target paths" {
    const allocator = testing.allocator;

    const source_copy = try allocator.dupe(u8, platformSourceWithTargets(
        \\        inputs_dir: "platform/targets/",
        \\        x64linux: { inputs: ["sub/host.a", "./crt1.o", "..hidden/lib.a", app] },
        \\
    ));
    defer allocator.free(source_copy);

    var env = try base.CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    const ast = try parse.file(allocator, &env);
    defer ast.deinit();

    try testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    var diagnostic: TargetsConfigDiagnostic = undefined;
    const config = (try TargetsConfig.fromAST(allocator, ast, &diagnostic)) orelse return error.TestUnexpectedResult;
    defer config.deinit(allocator);

    try testing.expectEqualStrings("platform/targets/", config.inputs_dir.?);
    const items = config.targets[0].items;
    try testing.expectEqual(@as(usize, 4), items.len);
    try testing.expectEqualStrings("sub/host.a", items[0].file_path);
    try testing.expectEqualStrings("./crt1.o", items[1].file_path);
    try testing.expectEqualStrings("..hidden/lib.a", items[2].file_path);
    try testing.expect(items[3] == .app);
}
