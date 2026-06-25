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
const parse = @import("parse");
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

/// Optional wasm-specific settings from a target record in a platform header.
pub const WasmTargetConfig = struct {
    import_memory: bool = false,
    minimum_memory: ?usize = null,
    maximum_memory: ?usize = null,
    initial_stack_size: ?usize = null,
    global_base: ?u32 = null,
    import_memory_ident: ?[]const u8 = null,
    minimum_memory_ident: ?[]const u8 = null,
    maximum_memory_ident: ?[]const u8 = null,
    initial_stack_size_ident: ?[]const u8 = null,
    global_base_ident: ?[]const u8 = null,

    fn deinit(self: WasmTargetConfig, allocator: Allocator) void {
        if (self.import_memory_ident) |ident| allocator.free(ident);
        if (self.minimum_memory_ident) |ident| allocator.free(ident);
        if (self.maximum_memory_ident) |ident| allocator.free(ident);
        if (self.initial_stack_size_ident) |ident| allocator.free(ident);
        if (self.global_base_ident) |ident| allocator.free(ident);
    }

    /// Whether any settings still need to be resolved from top-level constants.
    pub fn hasIdentifierBackedValues(self: WasmTargetConfig) bool {
        return self.import_memory_ident != null or
            self.minimum_memory_ident != null or
            self.maximum_memory_ident != null or
            self.initial_stack_size_ident != null or
            self.global_base_ident != null;
    }
};

/// Reason an identifier-backed target config field could not be resolved.
pub const TargetConfigResolveReason = enum {
    missing_top_level_value,
    not_constant,
    unevaluated_constant,
    expected_bool,
    expected_unsigned_integer,
    integer_out_of_range,

    pub fn message(self: TargetConfigResolveReason) []const u8 {
        return switch (self) {
            .missing_top_level_value => "does not name a top-level value in the platform module",
            .not_constant => "names a function, but target configuration requires a constant",
            .unevaluated_constant => "does not have a stored compile-time constant value",
            .expected_bool => "must resolve to True or False",
            .expected_unsigned_integer => "must resolve to a non-negative whole number",
            .integer_out_of_range => "resolves to a number outside the supported range",
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

/// Link specification for a single target.
/// Contains the artifact kind and the ordered list of items to link for this target.
pub const TargetLinkSpec = struct {
    target: RocTarget,
    output: OutputKind,
    items: []const LinkItem,
    wasm: ?WasmTargetConfig = null,
};

fn freeLinkSpec(allocator: Allocator, spec: TargetLinkSpec) void {
    if (spec.wasm) |wasm| wasm.deinit(allocator);
    for (spec.items) |item| switch (item) {
        .file_path => |fp| allocator.free(fp),
        else => {},
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

    /// Get the default target based on the current system.
    /// Returns the first target in the list that's compatible with the current host (OS and arch).
    pub fn getDefaultTarget(self: TargetsConfig) ?RocTarget {
        for (self.targets) |spec| {
            if (spec.target.isCompatibleWithHost()) {
                return spec.target;
            }
        }

        return null;
    }

    /// Get the default target for commands that must execute the result on this host.
    /// This excludes build-compatible targets such as wasm32 that are not native
    /// process executables for the default `roc` command, and targets that don't produce executables.
    pub fn getDefaultHostExecutableTarget(self: TargetsConfig) ?RocTarget {
        for (self.targets) |spec| {
            if (spec.output == .exe and spec.target.isExecutableOnHost()) {
                return spec.target;
            }
        }

        return null;
    }

    /// Check if a specific target is supported.
    pub fn supportsTarget(self: TargetsConfig, target: RocTarget) bool {
        return self.getLinkSpec(target) != null;
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

    /// Create a TargetsConfig from a parsed AST.
    /// Returns null if the platform header has no targets section. An explicit
    /// empty `targets: {}` section returns a hostless config with zero targets.
    /// All string values are duped with the provided allocator, so the
    /// returned TargetsConfig owns its memory and is independent of the AST.
    pub fn fromAST(allocator: Allocator, ast: anytype) Allocator.Error!?TargetsConfig {
        const NodeStore = parse.NodeStore;

        const store: *const NodeStore = &ast.store;

        // Get the file node first, then get the header from it
        const file = store.getFile();
        const header = store.getHeader(file.header);

        // Only platform headers have targets
        const platform = switch (header) {
            .platform => |p| p,
            else => return null,
        };

        // If no targets section, return null
        const targets_section_idx = platform.targets orelse return null;
        const targets_section = store.getTargetsSection(targets_section_idx);

        // Extract inputs_dir from string literal token (StringPart token)
        // Dupe the string so we own the memory
        const inputs_dir: ?[]const u8 = if (targets_section.inputs_dir) |tok_idx|
            try allocator.dupe(u8, ast.resolve(tok_idx))
        else
            null;
        errdefer if (inputs_dir) |fd| allocator.free(fd);

        const target_specs = try parseTargetSpecs(allocator, store, ast, targets_section.entries);
        errdefer freeTargetSpecs(allocator, target_specs);

        return TargetsConfig{
            .inputs_dir = inputs_dir,
            .targets = target_specs,
        };
    }

    fn appendTargetFiles(
        allocator: Allocator,
        store: *const parse.NodeStore,
        ast: anytype,
        files: parse.AST.TargetFile.Span,
        link_items: *std.array_list.Managed(LinkItem),
    ) Allocator.Error!void {
        const file_indices = store.targetFileSlice(files);
        for (file_indices) |file_idx| {
            const target_file = store.getTargetFile(file_idx);

            switch (target_file) {
                .string_literal => |tok| {
                    const path = ast.resolve(tok);
                    try link_items.append(.{ .file_path = try allocator.dupe(u8, path) });
                },
                .special_ident => |tok| {
                    const ident = ast.resolve(tok);
                    if (std.mem.eql(u8, ident, "app")) {
                        try link_items.append(.app);
                    } else if (std.mem.eql(u8, ident, "win_gui")) {
                        try link_items.append(.win_gui);
                    }
                },
                .malformed => continue,
            }
        }
    }

    fn clearTargetFiles(allocator: Allocator, link_items: *std.array_list.Managed(LinkItem)) void {
        for (link_items.items) |item| switch (item) {
            .file_path => |fp| allocator.free(fp),
            else => {},
        };
        link_items.clearRetainingCapacity();
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

    fn targetConfigIdentToken(value: parse.AST.TargetConfigValue) ?parse.tokenize.Token.Idx {
        return switch (value) {
            .ident => |ident| ident,
            else => null,
        };
    }

    fn parseUnsignedToken(allocator: Allocator, ast: anytype, tok: parse.tokenize.Token.Idx) Allocator.Error!?usize {
        const raw = ast.resolve(tok);
        var compact = try std.array_list.Managed(u8).initCapacity(allocator, raw.len);
        defer compact.deinit();
        for (raw) |byte| {
            if (byte != '_') compact.appendAssumeCapacity(byte);
        }
        return std.fmt.parseInt(usize, compact.items, 0) catch null;
    }

    fn parseUnsignedValue(
        allocator: Allocator,
        store: *const parse.NodeStore,
        ast: anytype,
        value_idx: parse.AST.TargetConfigValue.Idx,
    ) Allocator.Error!?usize {
        return switch (store.getTargetConfigValue(value_idx)) {
            .int_literal => |tok| try parseUnsignedToken(allocator, ast, tok),
            else => null,
        };
    }

    fn parseBoolValue(
        store: *const parse.NodeStore,
        ast: anytype,
        value_idx: parse.AST.TargetConfigValue.Idx,
    ) ?bool {
        return switch (store.getTargetConfigValue(value_idx)) {
            .tag_literal => |tok| blk: {
                const tag = ast.resolve(tok);
                if (std.mem.eql(u8, tag, "True")) break :blk true;
                if (std.mem.eql(u8, tag, "False")) break :blk false;
                break :blk null;
            },
            .string_literal => |tok| blk: {
                const value = ast.resolve(tok);
                if (std.mem.eql(u8, value, "env.memory")) break :blk true;
                break :blk null;
            },
            else => null,
        };
    }

    fn parseWasmConfig(
        allocator: Allocator,
        store: *const parse.NodeStore,
        ast: anytype,
        config_idx: parse.AST.TargetConfig.Idx,
        link_items: *std.array_list.Managed(LinkItem),
    ) Allocator.Error!?WasmTargetConfig {
        const config = store.getTargetConfig(config_idx);
        const entries = store.targetConfigEntrySlice(config.entries);
        var wasm = WasmTargetConfig{};
        errdefer wasm.deinit(allocator);
        var has_wasm_config = false;

        for (entries) |entry_idx| {
            const entry = store.getTargetConfigEntry(entry_idx);
            const name = ast.resolve(entry.name);
            const value = store.getTargetConfigValue(entry.value);

            if (std.mem.eql(u8, name, "inputs")) {
                switch (value) {
                    .files => |files| {
                        clearTargetFiles(allocator, link_items);
                        try appendTargetFiles(allocator, store, ast, files, link_items);
                    },
                    else => {},
                }
            } else if (std.mem.eql(u8, name, "import_memory")) {
                if (parseBoolValue(store, ast, entry.value)) |import_memory| {
                    wasm.import_memory = import_memory;
                    has_wasm_config = true;
                } else if (targetConfigIdentToken(value)) |ident| {
                    try storeIdent(allocator, ast, ident, &wasm.import_memory_ident);
                    has_wasm_config = true;
                }
            } else if (std.mem.eql(u8, name, "minimum_memory") or std.mem.eql(u8, name, "initial_memory")) {
                if (try parseUnsignedValue(allocator, store, ast, entry.value)) |bytes| {
                    wasm.minimum_memory = bytes;
                    has_wasm_config = true;
                } else if (targetConfigIdentToken(value)) |ident| {
                    try storeIdent(allocator, ast, ident, &wasm.minimum_memory_ident);
                    has_wasm_config = true;
                }
            } else if (std.mem.eql(u8, name, "maximum_memory") or std.mem.eql(u8, name, "max_memory")) {
                if (try parseUnsignedValue(allocator, store, ast, entry.value)) |bytes| {
                    wasm.maximum_memory = bytes;
                    has_wasm_config = true;
                } else if (targetConfigIdentToken(value)) |ident| {
                    try storeIdent(allocator, ast, ident, &wasm.maximum_memory_ident);
                    has_wasm_config = true;
                }
            } else if (std.mem.eql(u8, name, "initial_stack_size") or std.mem.eql(u8, name, "stack_size")) {
                if (try parseUnsignedValue(allocator, store, ast, entry.value)) |bytes| {
                    wasm.initial_stack_size = bytes;
                    has_wasm_config = true;
                } else if (targetConfigIdentToken(value)) |ident| {
                    try storeIdent(allocator, ast, ident, &wasm.initial_stack_size_ident);
                    has_wasm_config = true;
                }
            } else if (std.mem.eql(u8, name, "global_base")) {
                if (try parseUnsignedValue(allocator, store, ast, entry.value)) |bytes| {
                    if (std.math.cast(u32, bytes)) |global_base| {
                        wasm.global_base = global_base;
                        has_wasm_config = true;
                    }
                } else if (targetConfigIdentToken(value)) |ident| {
                    try storeIdent(allocator, ast, ident, &wasm.global_base_ident);
                    has_wasm_config = true;
                }
            }
        }

        return if (has_wasm_config) wasm else null;
    }

    /// Parse the output kind from a target entry's config record.
    /// Defaults to .exe when no output: field is present.
    fn parseOutputKind(
        store: *const parse.NodeStore,
        ast: anytype,
        config_idx: parse.AST.TargetConfig.Idx,
    ) OutputKind {
        const config = store.getTargetConfig(config_idx);
        const entries = store.targetConfigEntrySlice(config.entries);
        for (entries) |entry_idx| {
            const entry = store.getTargetConfigEntry(entry_idx);
            const name = ast.resolve(entry.name);
            if (std.mem.eql(u8, name, "output")) {
                switch (store.getTargetConfigValue(entry.value)) {
                    .tag_literal => |tok| {
                        if (OutputKind.fromTagName(ast.resolve(tok))) |kind| {
                            return kind;
                        }
                    },
                    else => {},
                }
            }
        }
        return .exe;
    }

    /// Parse per-target specs from the targets section entries.
    fn parseTargetSpecs(
        allocator: Allocator,
        store: *const parse.NodeStore,
        ast: anytype,
        entries: parse.AST.TargetEntry.Span,
    ) Allocator.Error![]const TargetLinkSpec {
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
                    else => {},
                };
                link_items.deinit();
            }

            const wasm_config = try parseWasmConfig(allocator, store, ast, entry.config, &link_items);

            try specs.append(.{
                .target = target,
                .output = parseOutputKind(store, ast, entry.config),
                .items = try link_items.toOwnedSlice(),
                .wasm = wasm_config,
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
    try resolveWasmBoolField(allocator, checked_module, target, output, "import_memory", &wasm.import_memory, &wasm.import_memory_ident, diagnostic);
    try resolveWasmUsizeField(allocator, checked_module, target, output, "minimum_memory", &wasm.minimum_memory, &wasm.minimum_memory_ident, diagnostic);
    try resolveWasmUsizeField(allocator, checked_module, target, output, "maximum_memory", &wasm.maximum_memory, &wasm.maximum_memory_ident, diagnostic);
    try resolveWasmUsizeField(allocator, checked_module, target, output, "initial_stack_size", &wasm.initial_stack_size, &wasm.initial_stack_size_ident, diagnostic);
    try resolveWasmU32Field(allocator, checked_module, target, output, "global_base", &wasm.global_base, &wasm.global_base_ident, diagnostic);
}

fn resolveWasmBoolField(
    allocator: Allocator,
    checked_module: *const checked.CheckedModuleArtifact,
    target: RocTarget,
    output: OutputKind,
    field_name: []const u8,
    out: *bool,
    ident_slot: *?[]const u8,
    diagnostic: *TargetConfigResolveDiagnostic,
) error{TargetConfigInvalid}!void {
    const ident = ident_slot.* orelse return;
    var reason: TargetConfigResolveReason = .missing_top_level_value;
    const node = topLevelConstNode(checked_module, ident, &reason) orelse {
        diagnostic.* = .{ .target = target, .output = output, .field_name = field_name, .ident_name = ident, .reason = reason };
        return error.TargetConfigInvalid;
    };
    const value = constBool(checked_module, node) orelse {
        diagnostic.* = .{ .target = target, .output = output, .field_name = field_name, .ident_name = ident, .reason = .expected_bool };
        return error.TargetConfigInvalid;
    };
    out.* = value;
    allocator.free(ident);
    ident_slot.* = null;
}

fn resolveWasmUsizeField(
    allocator: Allocator,
    checked_module: *const checked.CheckedModuleArtifact,
    target: RocTarget,
    output: OutputKind,
    field_name: []const u8,
    out: *?usize,
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
    out.* = value;
    allocator.free(ident);
    ident_slot.* = null;
}

fn resolveWasmU32Field(
    allocator: Allocator,
    checked_module: *const checked.CheckedModuleArtifact,
    target: RocTarget,
    output: OutputKind,
    field_name: []const u8,
    out: *?u32,
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
    const narrowed = std.math.cast(u32, value) orelse {
        diagnostic.* = .{ .target = target, .output = output, .field_name = field_name, .ident_name = ident, .reason = .integer_out_of_range };
        return error.TargetConfigInvalid;
    };
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
        };
    }

    reason.* = .missing_top_level_value;
    return null;
}

fn constBool(checked_module: *const checked.CheckedModuleArtifact, node: checked.ConstNodeId) ?bool {
    return switch (checked_module.const_store.get(node)) {
        .nominal => |nominal| constBool(checked_module, nominal.backing),
        .tag => |tag| blk: {
            if (tag.payloads.len != 0) break :blk null;
            if (std.mem.eql(u8, tag.tag_name, "True")) break :blk true;
            if (std.mem.eql(u8, tag.tag_name, "False")) break :blk false;
            break :blk null;
        },
        .str => |str| blk: {
            const bytes = checked_module.const_store.strBytes(str);
            if (std.mem.eql(u8, bytes, "env.memory")) break :blk true;
            break :blk null;
        },
        else => null,
    };
}

fn constUnsigned(
    checked_module: *const checked.CheckedModuleArtifact,
    node: checked.ConstNodeId,
    reason: *TargetConfigResolveReason,
) ?usize {
    return switch (checked_module.const_store.get(node)) {
        .nominal => |nominal| constUnsigned(checked_module, nominal.backing, reason),
        .scalar => |scalar| scalarUnsigned(scalar, reason),
        else => blk: {
            reason.* = .expected_unsigned_integer;
            break :blk null;
        },
    };
}

fn scalarUnsigned(scalar: checked.ConstScalar, reason: *TargetConfigResolveReason) ?usize {
    const value: u128 = switch (scalar) {
        .u8 => |v| v,
        .u16 => |v| v,
        .u32 => |v| v,
        .u64 => |v| v,
        .u128 => |v| v,
        .i8 => |v| signedScalarUnsigned(v, reason) orelse return null,
        .i16 => |v| signedScalarUnsigned(v, reason) orelse return null,
        .i32 => |v| signedScalarUnsigned(v, reason) orelse return null,
        .i64 => |v| signedScalarUnsigned(v, reason) orelse return null,
        .i128 => |v| signedScalarUnsigned(v, reason) orelse return null,
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
const builtin = @import("builtin");

test "getDefaultTarget returns first compatible target" {
    // Create a config with only x64glibc (not x64musl)
    // On a Linux x64 system, both are compatible, but we only include glibc
    const config = TargetsConfig{
        .inputs_dir = "targets",
        .targets = &.{
            .{ .target = .x64glibc, .output = .exe, .items = &.{.app} },
        },
    };

    // getDefaultTarget should return x64glibc if we're on Linux x64
    // (since both x64musl and x64glibc are compatible with Linux x64)
    if (builtin.target.os.tag == .linux and builtin.target.cpu.arch == .x86_64) {
        const result = config.getDefaultTarget();
        try testing.expect(result != null);
        try testing.expectEqual(RocTarget.x64glibc, result.?);
    }
}

test "getDefaultHostExecutableTarget excludes wasm" {
    const config = TargetsConfig{
        .inputs_dir = "targets",
        .targets = &.{
            .{ .target = .wasm32, .output = .exe, .items = &.{.app} },
        },
    };

    try testing.expectEqual(RocTarget.wasm32, config.getDefaultTarget().?);
    try testing.expect(config.getDefaultHostExecutableTarget() == null);
}

test "getDefaultHostExecutableTarget excludes non-exe outputs" {
    const config = TargetsConfig{
        .inputs_dir = "targets",
        .targets = &.{
            .{ .target = .x64mac, .output = .shared, .items = &.{.app} },
            .{ .target = .arm64mac, .output = .shared, .items = &.{.app} },
        },
    };

    try testing.expect(config.getDefaultHostExecutableTarget() == null);
}

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
        \\    provides { "roc_make_glue": make_glue_for_host }
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

    const maybe_config = try TargetsConfig.fromAST(allocator, ast);
    try testing.expect(maybe_config != null);

    const config = maybe_config.?;
    defer config.deinit(allocator);

    try testing.expect(config.inputs_dir == null);
    try testing.expectEqual(@as(usize, 0), config.targets.len);
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
        \\            import_memory,
        \\            minimum_memory,
        \\            maximum_memory,
        \\            initial_stack_size,
        \\            global_base,
        \\        },
        \\    }
        \\
        \\import_memory = True
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

    const maybe_config = try TargetsConfig.fromAST(allocator, ast);
    try testing.expect(maybe_config != null);

    const config = maybe_config.?;
    defer config.deinit(allocator);

    try testing.expectEqual(@as(usize, 1), config.targets.len);
    try testing.expectEqual(OutputKind.shared, config.targets[0].output);
    const wasm = config.targets[0].wasm orelse return error.TestUnexpectedResult;
    try testing.expect(wasm.hasIdentifierBackedValues());
    try testing.expectEqualStrings("import_memory", wasm.import_memory_ident.?);
    try testing.expectEqualStrings("minimum_memory", wasm.minimum_memory_ident.?);
    try testing.expectEqualStrings("maximum_memory", wasm.maximum_memory_ident.?);
    try testing.expectEqualStrings("initial_stack_size", wasm.initial_stack_size_ident.?);
    try testing.expectEqualStrings("global_base", wasm.global_base_ident.?);
}
