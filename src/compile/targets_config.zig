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
    link_type: LinkType,
    field_name: []const u8,
    ident_name: []const u8,
    reason: TargetConfigResolveReason,
};

/// Link specification for a single target.
/// Contains the ordered list of items to link for this target.
pub const TargetLinkSpec = struct {
    target: RocTarget,
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

/// Type of output binary.
pub const LinkType = enum {
    /// Executable binary.
    exe,
    /// Static library (.a, .lib).
    static_lib,
    /// Shared/dynamic library (.so, .dylib, .dll).
    shared_lib,
};

/// Complete targets configuration from a platform header.
pub const TargetsConfig = struct {
    /// Base directory for target-specific files (e.g., "targets/").
    files_dir: ?[]const u8,

    /// Executable target specifications (in priority order).
    exe: []const TargetLinkSpec,

    /// Static library target specifications (in priority order).
    static_lib: []const TargetLinkSpec,

    /// Shared library target specifications (in priority order).
    shared_lib: []const TargetLinkSpec,

    /// Free all owned memory. Call this when the TargetsConfig is no longer needed,
    /// but only if it was created via `fromAST()` (which dupes all strings).
    /// Do NOT call this on TargetsConfig values created with comptime/static data.
    pub fn deinit(self: TargetsConfig, allocator: Allocator) void {
        if (self.files_dir) |fd| allocator.free(fd);
        for (self.exe) |spec| freeLinkSpec(allocator, spec);
        allocator.free(self.exe);
        for (self.static_lib) |spec| freeLinkSpec(allocator, spec);
        allocator.free(self.static_lib);
        for (self.shared_lib) |spec| freeLinkSpec(allocator, spec);
        allocator.free(self.shared_lib);
    }

    /// Get the link spec for a specific target and link type.
    pub fn getLinkSpec(self: TargetsConfig, target: RocTarget, link_type: LinkType) ?TargetLinkSpec {
        const specs = switch (link_type) {
            .exe => self.exe,
            .static_lib => self.static_lib,
            .shared_lib => self.shared_lib,
        };
        for (specs) |spec| {
            if (spec.target == target) {
                return spec;
            }
        }
        return null;
    }

    /// Get the default target for a given link type based on the current system.
    /// Returns the first target in the list that's compatible with the current host (OS and arch).
    pub fn getDefaultTarget(self: TargetsConfig, link_type: LinkType) ?RocTarget {
        const specs = switch (link_type) {
            .exe => self.exe,
            .static_lib => self.static_lib,
            .shared_lib => self.shared_lib,
        };

        // First pass: look for exact OS and architecture match
        for (specs) |spec| {
            if (spec.target.isCompatibleWithHost()) {
                return spec.target;
            }
        }

        return null;
    }

    /// Get the default target for commands that must execute the result on this host.
    /// This excludes build-compatible targets such as wasm32 that are not native
    /// process executables for `roc run`.
    pub fn getDefaultHostExecutableTarget(self: TargetsConfig, link_type: LinkType) ?RocTarget {
        const specs = switch (link_type) {
            .exe => self.exe,
            .static_lib => self.static_lib,
            .shared_lib => self.shared_lib,
        };

        for (specs) |spec| {
            if (spec.target.isExecutableOnHost()) {
                return spec.target;
            }
        }

        return null;
    }

    /// Result of finding a compatible target.
    pub const CompatibleTarget = struct {
        target: RocTarget,
        link_type: LinkType,
    };

    /// Get the first compatible target across all link types.
    /// Iterates through exe, static_lib, shared_lib in order,
    /// returning the first target compatible with the current host.
    pub fn getFirstCompatibleTarget(self: TargetsConfig) ?CompatibleTarget {
        const link_types = [_]LinkType{ .exe, .static_lib, .shared_lib };

        for (link_types) |lt| {
            const specs = self.getSupportedTargets(lt);
            for (specs) |spec| {
                if (spec.target.isCompatibleWithHost()) {
                    return CompatibleTarget{ .target = spec.target, .link_type = lt };
                }
            }
        }

        return null;
    }

    /// Check if a specific target is supported.
    pub fn supportsTarget(self: TargetsConfig, target: RocTarget, link_type: LinkType) bool {
        return self.getLinkSpec(target, link_type) != null;
    }

    /// Get all supported targets for a link type.
    pub fn getSupportedTargets(self: TargetsConfig, link_type: LinkType) []const TargetLinkSpec {
        return switch (link_type) {
            .exe => self.exe,
            .static_lib => self.static_lib,
            .shared_lib => self.shared_lib,
        };
    }

    pub fn resolveCheckedConstants(
        self: TargetsConfig,
        allocator: Allocator,
        checked_module: *const checked.CheckedModuleArtifact,
        diagnostic: *TargetConfigResolveDiagnostic,
    ) error{TargetConfigInvalid}!void {
        try resolveLinkTypeCheckedConstants(allocator, checked_module, @constCast(self.exe), .exe, diagnostic);
        try resolveLinkTypeCheckedConstants(allocator, checked_module, @constCast(self.static_lib), .static_lib, diagnostic);
        try resolveLinkTypeCheckedConstants(allocator, checked_module, @constCast(self.shared_lib), .shared_lib, diagnostic);
    }

    /// Create a TargetsConfig from a parsed AST.
    /// Returns null if the platform header has no targets section.
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

        // Extract files_dir from string literal token (StringPart token)
        // Dupe the string so we own the memory
        const files_dir: ?[]const u8 = if (targets_section.files_path) |tok_idx|
            try allocator.dupe(u8, ast.resolve(tok_idx))
        else
            null;
        errdefer if (files_dir) |fd| allocator.free(fd);

        // Convert exe link type
        const exe_specs = try parseLinkTypeSpecs(allocator, store, ast, targets_section.exe);
        errdefer freeLinkTypeSpecs(allocator, exe_specs);

        // Convert static_lib link type
        const static_lib_specs = try parseLinkTypeSpecs(allocator, store, ast, targets_section.static_lib);
        errdefer freeLinkTypeSpecs(allocator, static_lib_specs);

        // shared_lib to be added later
        const empty_specs: []const TargetLinkSpec = &.{};

        return TargetsConfig{
            .files_dir = files_dir,
            .exe = exe_specs,
            .static_lib = static_lib_specs,
            .shared_lib = empty_specs,
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

            if (std.mem.eql(u8, name, "files")) {
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

    /// Parse link specs for a single link type (exe, static_lib, shared_lib).
    fn parseLinkTypeSpecs(
        allocator: Allocator,
        store: *const parse.NodeStore,
        ast: anytype,
        link_type_idx: anytype,
    ) Allocator.Error![]const TargetLinkSpec {
        const idx = link_type_idx orelse return &.{};

        const link_type = store.getTargetLinkType(idx);
        const entry_indices = store.targetEntrySlice(link_type.entries);

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
                .items = try link_items.toOwnedSlice(),
                .wasm = wasm_config,
            });
        }

        return try specs.toOwnedSlice();
    }

    /// Free link type specs allocated by parseLinkTypeSpecs.
    fn freeLinkTypeSpecs(allocator: Allocator, specs: []const TargetLinkSpec) void {
        for (specs) |spec| freeLinkSpec(allocator, spec);
        allocator.free(specs);
    }
};

fn resolveLinkTypeCheckedConstants(
    allocator: Allocator,
    checked_module: *const checked.CheckedModuleArtifact,
    specs: []TargetLinkSpec,
    link_type: LinkType,
    diagnostic: *TargetConfigResolveDiagnostic,
) error{TargetConfigInvalid}!void {
    for (specs) |*spec| {
        var wasm = spec.wasm orelse continue;
        resolveWasmCheckedConstants(allocator, checked_module, spec.target, link_type, &wasm, diagnostic) catch |err| {
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
    link_type: LinkType,
    wasm: *WasmTargetConfig,
    diagnostic: *TargetConfigResolveDiagnostic,
) error{TargetConfigInvalid}!void {
    try resolveWasmBoolField(allocator, checked_module, target, link_type, "import_memory", &wasm.import_memory, &wasm.import_memory_ident, diagnostic);
    try resolveWasmUsizeField(allocator, checked_module, target, link_type, "minimum_memory", &wasm.minimum_memory, &wasm.minimum_memory_ident, diagnostic);
    try resolveWasmUsizeField(allocator, checked_module, target, link_type, "maximum_memory", &wasm.maximum_memory, &wasm.maximum_memory_ident, diagnostic);
    try resolveWasmUsizeField(allocator, checked_module, target, link_type, "initial_stack_size", &wasm.initial_stack_size, &wasm.initial_stack_size_ident, diagnostic);
    try resolveWasmU32Field(allocator, checked_module, target, link_type, "global_base", &wasm.global_base, &wasm.global_base_ident, diagnostic);
}

fn resolveWasmBoolField(
    allocator: Allocator,
    checked_module: *const checked.CheckedModuleArtifact,
    target: RocTarget,
    link_type: LinkType,
    field_name: []const u8,
    out: *bool,
    ident_slot: *?[]const u8,
    diagnostic: *TargetConfigResolveDiagnostic,
) error{TargetConfigInvalid}!void {
    const ident = ident_slot.* orelse return;
    var reason: TargetConfigResolveReason = .missing_top_level_value;
    const node = topLevelConstNode(checked_module, ident, &reason) orelse {
        diagnostic.* = .{ .target = target, .link_type = link_type, .field_name = field_name, .ident_name = ident, .reason = reason };
        return error.TargetConfigInvalid;
    };
    const value = constBool(checked_module, node) orelse {
        diagnostic.* = .{ .target = target, .link_type = link_type, .field_name = field_name, .ident_name = ident, .reason = .expected_bool };
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
    link_type: LinkType,
    field_name: []const u8,
    out: *?usize,
    ident_slot: *?[]const u8,
    diagnostic: *TargetConfigResolveDiagnostic,
) error{TargetConfigInvalid}!void {
    const ident = ident_slot.* orelse return;
    var reason: TargetConfigResolveReason = .expected_unsigned_integer;
    const node = topLevelConstNode(checked_module, ident, &reason) orelse {
        diagnostic.* = .{ .target = target, .link_type = link_type, .field_name = field_name, .ident_name = ident, .reason = reason };
        return error.TargetConfigInvalid;
    };
    const value = constUnsigned(checked_module, node, &reason) orelse {
        diagnostic.* = .{ .target = target, .link_type = link_type, .field_name = field_name, .ident_name = ident, .reason = reason };
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
    link_type: LinkType,
    field_name: []const u8,
    out: *?u32,
    ident_slot: *?[]const u8,
    diagnostic: *TargetConfigResolveDiagnostic,
) error{TargetConfigInvalid}!void {
    const ident = ident_slot.* orelse return;
    var reason: TargetConfigResolveReason = .expected_unsigned_integer;
    const node = topLevelConstNode(checked_module, ident, &reason) orelse {
        diagnostic.* = .{ .target = target, .link_type = link_type, .field_name = field_name, .ident_name = ident, .reason = reason };
        return error.TargetConfigInvalid;
    };
    const value = constUnsigned(checked_module, node, &reason) orelse {
        diagnostic.* = .{ .target = target, .link_type = link_type, .field_name = field_name, .ident_name = ident, .reason = reason };
        return error.TargetConfigInvalid;
    };
    const narrowed = std.math.cast(u32, value) orelse {
        diagnostic.* = .{ .target = target, .link_type = link_type, .field_name = field_name, .ident_name = ident, .reason = .integer_out_of_range };
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
        .files_dir = "targets",
        .exe = &.{
            .{ .target = .x64glibc, .items = &.{.app} },
        },
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    // getDefaultTarget should return x64glibc if we're on Linux x64
    // (since both x64musl and x64glibc are compatible with Linux x64)
    if (builtin.target.os.tag == .linux and builtin.target.cpu.arch == .x86_64) {
        const result = config.getDefaultTarget(.exe);
        try testing.expect(result != null);
        try testing.expectEqual(RocTarget.x64glibc, result.?);
    }
}

test "getDefaultHostExecutableTarget excludes wasm" {
    const config = TargetsConfig{
        .files_dir = "targets",
        .exe = &.{
            .{ .target = .wasm32, .items = &.{.app} },
        },
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    try testing.expectEqual(RocTarget.wasm32, config.getDefaultTarget(.exe).?);
    try testing.expect(config.getDefaultHostExecutableTarget(.exe) == null);
}

test "getFirstCompatibleTarget finds exe target first" {
    const config = TargetsConfig{
        .files_dir = "targets",
        .exe = &.{
            .{ .target = .x64mac, .items = &.{.app} },
            .{ .target = .arm64mac, .items = &.{.app} },
        },
        .static_lib = &.{
            .{ .target = .wasm32, .items = &.{.app} },
        },
        .shared_lib = &.{},
    };

    // On macOS x64, should find x64mac from exe targets
    if (builtin.target.os.tag == .macos and builtin.target.cpu.arch == .x86_64) {
        const result = config.getFirstCompatibleTarget();
        try testing.expect(result != null);
        try testing.expectEqual(RocTarget.x64mac, result.?.target);
        try testing.expectEqual(LinkType.exe, result.?.link_type);
    }
    // On macOS arm64, should find arm64mac from exe targets
    if (builtin.target.os.tag == .macos and builtin.target.cpu.arch == .aarch64) {
        const result = config.getFirstCompatibleTarget();
        try testing.expect(result != null);
        try testing.expectEqual(RocTarget.arm64mac, result.?.target);
        try testing.expectEqual(LinkType.exe, result.?.link_type);
    }
}

test "getFirstCompatibleTarget returns null when no compatible target" {
    // Create a config with only Windows targets
    const config = TargetsConfig{
        .files_dir = "targets",
        .exe = &.{
            .{ .target = .x64win, .items = &.{.app} },
            .{ .target = .arm64win, .items = &.{.app} },
        },
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    // On non-Windows systems, no compatible target should be found
    if (builtin.target.os.tag != .windows) {
        const result = config.getFirstCompatibleTarget();
        try testing.expect(result == null);
    }
}

test "getLinkSpec returns correct spec for supported target" {
    const config = TargetsConfig{
        .files_dir = "targets",
        .exe = &.{
            .{ .target = .x64mac, .items = &.{ .{ .file_path = "libhost.a" }, .app } },
            .{ .target = .arm64mac, .items = &.{.app} },
        },
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    const spec = config.getLinkSpec(.x64mac, .exe);
    try testing.expect(spec != null);
    try testing.expectEqual(RocTarget.x64mac, spec.?.target);
    try testing.expectEqual(@as(usize, 2), spec.?.items.len);
}

test "getLinkSpec returns null for unsupported target" {
    const config = TargetsConfig{
        .files_dir = "targets",
        .exe = &.{
            .{ .target = .x64mac, .items = &.{.app} },
        },
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    // x64musl is not in the config
    const spec = config.getLinkSpec(.x64musl, .exe);
    try testing.expect(spec == null);
}

test "fromAST captures punned wasm identifier config" {
    const allocator = testing.allocator;

    const source =
        \\platform ""
        \\    requires { main : {} }
        \\    exposes []
        \\    packages {}
        \\    provides { main_for_host: "main" }
        \\    targets: {
        \\        files: "targets/",
        \\        static_lib: {
        \\            wasm32: {
        \\                files: ["libhost.a", app],
        \\                import_memory,
        \\                minimum_memory,
        \\                maximum_memory,
        \\                initial_stack_size,
        \\                global_base,
        \\            },
        \\        }
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

    const ast = try parse.parse(allocator, &env);
    defer ast.deinit();

    try testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const maybe_config = try TargetsConfig.fromAST(allocator, ast);
    try testing.expect(maybe_config != null);

    const config = maybe_config.?;
    defer config.deinit(allocator);

    try testing.expectEqual(@as(usize, 1), config.static_lib.len);
    const wasm = config.static_lib[0].wasm orelse return error.TestUnexpectedResult;
    try testing.expect(wasm.hasIdentifierBackedValues());
    try testing.expectEqualStrings("import_memory", wasm.import_memory_ident.?);
    try testing.expectEqualStrings("minimum_memory", wasm.minimum_memory_ident.?);
    try testing.expectEqualStrings("maximum_memory", wasm.maximum_memory_ident.?);
    try testing.expectEqualStrings("initial_stack_size", wasm.initial_stack_size_ident.?);
    try testing.expectEqualStrings("global_base", wasm.global_base_ident.?);
}
