//! CompletionBuilder - centralized completion item building.
//!
//! This module provides a builder pattern for constructing LSP completion items.
//! It handles deduplication, type formatting, and completion item creation for:
//! - Local variables and function definitions
//! - Module names and module members
//! - Type names (for type annotations)
//! - Record fields (for dot access)
//! - Methods (for static dispatch)

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const compile = @import("compile");

const completion_handler = @import("../handlers/completion.zig");
const builtin_completion = @import("builtins.zig");
const scope_map = @import("../scope_map.zig");
const module_lookup = @import("../module_lookup.zig");
const doc_comments = @import("../doc_comments.zig");

const Allocator = std.mem.Allocator;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const BuildEnv = compile.BuildEnv;
const DebugFlags = @import("../syntax.zig").DebugFlags;

const CompletionItem = completion_handler.CompletionItem;
const CompletionItemKind = completion_handler.CompletionItemKind;

/// Builder for constructing completion item lists.
/// Handles deduplication and provides methods for adding different types of completions.
pub const CompletionBuilder = struct {
    allocator: Allocator,
    items: *std.ArrayList(CompletionItem),
    seen_labels: std.StringHashMap(void),
    builtin_module_env: ?*ModuleEnv,
    debug: DebugFlags = .{},
    log_file: ?std.fs.File = null,
    /// Lazily-built scope map, shared across methods that need scope info.
    cached_scope: ?scope_map.ScopeMap = null,
    /// The qualified module ident idx the cached scope was built for (to detect
    /// mismatches). Uses qualified_module_ident so modules with the same bare
    /// name in different packages don't collide.
    cached_scope_module_ident: base.Ident.Idx = base.Ident.Idx.NONE,

    /// Initialize a new CompletionBuilder.
    pub fn init(allocator: Allocator, items: *std.ArrayList(CompletionItem), builtin_module_env: ?*ModuleEnv) CompletionBuilder {
        return .{
            .allocator = allocator,
            .items = items,
            .seen_labels = std.StringHashMap(void).init(allocator),
            .builtin_module_env = builtin_module_env,
        };
    }

    /// Initialize a new CompletionBuilder with debug logging.
    pub fn initWithDebug(allocator: Allocator, items: *std.ArrayList(CompletionItem), builtin_module_env: ?*ModuleEnv, debug: DebugFlags, log_file: ?std.fs.File) CompletionBuilder {
        return .{
            .allocator = allocator,
            .items = items,
            .seen_labels = std.StringHashMap(void).init(allocator),
            .builtin_module_env = builtin_module_env,
            .debug = debug,
            .log_file = log_file,
        };
    }

    /// Clean up resources used by the builder.
    pub fn deinit(self: *CompletionBuilder) void {
        self.seen_labels.deinit();
        if (self.cached_scope) |*s| s.deinit();
    }

    /// Get or build the scope map for the given module env.
    /// Reuses a previously built scope if the module's qualified ident idx matches.
    fn getOrBuildScope(self: *CompletionBuilder, module_env: *ModuleEnv) *scope_map.ScopeMap {
        const module_ident = module_env.qualified_module_ident;
        if (self.cached_scope != null and
            !self.cached_scope_module_ident.isNone() and
            @as(u32, @bitCast(self.cached_scope_module_ident)) == @as(u32, @bitCast(module_ident)))
        {
            return &self.cached_scope.?;
        }
        // Dispose of stale scope if the module changed.
        if (self.cached_scope) |*old| old.deinit();

        var scope = scope_map.ScopeMap.init(self.allocator);
        scope.build(module_env) catch {};
        self.cached_scope = scope;
        self.cached_scope_module_ident = module_ident;
        return &self.cached_scope.?;
    }

    fn logDebug(self: *CompletionBuilder, comptime fmt: []const u8, args: anytype) void {
        if (!self.debug.completion) return;
        var log_file = self.log_file orelse return;
        var buffer: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buffer, fmt, args) catch return;
        log_file.writeAll(msg) catch return;
        log_file.writeAll("\n") catch {};
        log_file.sync() catch {};
    }

    /// Add a completion item, returning true if it was added (not a duplicate).
    pub fn addItem(self: *CompletionBuilder, item: CompletionItem) !bool {
        // Check for duplicates
        if (self.seen_labels.contains(item.label)) {
            // Avoid leaking owned optional fields on duplicate rejection.
            // Label ownership is centralized below (we duplicate labels only for
            // accepted items), so duplicate callers can pass borrowed labels.
            self.freeOwnedOptionalFields(item);
            return false;
        }

        // Keep label lifetime simple and consistent: every retained completion
        // owns its label memory regardless of where it came from.
        const owned_label = try self.allocator.dupe(u8, item.label);
        errdefer self.allocator.free(owned_label);

        // Store the dedupe key using stable owned memory. This avoids dangling
        // map keys when callers pass transient labels (e.g. stack buffers).
        try self.seen_labels.put(owned_label, {});
        errdefer _ = self.seen_labels.remove(owned_label);

        var stored_item = item;
        stored_item.label = owned_label;

        // If appending fails, clean up all fields this builder owns.
        errdefer self.freeOwnedFields(stored_item);
        try self.items.append(self.allocator, stored_item);
        return true;
    }

    /// Free optional owned fields on a completion item that wasn't retained.
    ///
    /// We only free fields that are known to be owned by the builder. If new
    /// owned fields are added to CompletionItem in the future, update this list.
    fn freeOwnedOptionalFields(self: *CompletionBuilder, item: CompletionItem) void {
        if (item.detail) |detail| {
            self.allocator.free(detail);
        }
        if (item.documentation) |doc| {
            self.allocator.free(doc);
        }
        if (item.insertText) |insert_text| {
            self.allocator.free(insert_text);
        }
        if (item.sortText) |sort_text| {
            self.allocator.free(sort_text);
        }
    }

    /// Free all fields owned by a retained completion item.
    fn freeOwnedFields(self: *CompletionBuilder, item: CompletionItem) void {
        self.allocator.free(item.label);
        self.freeOwnedOptionalFields(item);
    }

    // Module Name Completions

    /// Add module name completions from all loaded packages/modules in BuildEnv.
    pub fn addModuleNameCompletionsFromEnv(self: *CompletionBuilder, env: *BuildEnv) !void {
        // Surface builtin modules (Str, List, etc.) even when they are not
        // present in schedulers. This keeps completions available while
        // preserving real backing data for members/types.
        try self.addBuiltinModuleNameCompletions();

        var sched_it = env.schedulers.iterator();
        while (sched_it.next()) |entry| {
            const sched = entry.value_ptr.*;
            for (sched.modules.items) |module_state| {
                const name = module_state.name;
                if (name.len == 0) continue;

                _ = try self.addItem(.{
                    .label = name,
                    .kind = @intFromEnum(CompletionItemKind.module),
                    .detail = null,
                });
            }
        }
    }

    /// Add module name completions from the current module's import statements.
    pub fn addModuleNameCompletions(self: *CompletionBuilder, module_env: *ModuleEnv) !void {
        // Add imported module names from import statements
        const import_statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (import_statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            if (stmt == .s_import) {
                const import_stmt = stmt.s_import;
                // Use alias if available, otherwise use the module name
                const name_idx = import_stmt.alias_tok orelse import_stmt.module_name_tok;
                const name = module_env.common.idents.getText(name_idx);

                if (name.len > 0) {
                    _ = try self.addItem(.{
                        .label = name,
                        .kind = @intFromEnum(CompletionItemKind.module),
                        .detail = null,
                    });
                }
            }
        }
    }

    // Module Member Completions

    /// Add completions for members of a specific module (e.g., Str.concat).
    pub fn addModuleMemberCompletions(
        self: *CompletionBuilder,
        env: *BuildEnv,
        module_name: []const u8,
        module_env_opt: ?*ModuleEnv,
    ) !void {
        // Builtins are backed by a single Builtin module env. Treat builtin type
        // names (Str, List, etc.) as top-level modules for member completions.
        if (builtin_completion.isBuiltinType(module_name)) {
            try self.addModuleMemberCompletionsFromModuleEnv(env.builtin_modules.builtin_module.env, module_name);
        }

        // Try to find the module in imported modules
        var sched_it = env.schedulers.iterator();
        while (sched_it.next()) |entry| {
            const sched = entry.value_ptr.*;
            for (sched.modules.items) |*module_state| {
                // Check if this module's name matches
                if (std.mem.eql(u8, module_state.name, module_name)) {
                    if (module_state.env) |*imported_env| {
                        try self.addModuleMemberCompletionsFromModuleEnv(imported_env, module_name);
                    }
                    return;
                }
            }
        }

        // Fall back to local module env for nominal type associated values.
        // Don't require module_name to match module_env.module_name - this
        // handles Record2.ttt where Record2 is a nominal type in the current module.
        if (module_env_opt) |module_env| {
            try self.addModuleMemberCompletionsFromModuleEnv(module_env, module_name);
        }
    }

    /// Add builtin module name completions (Str, List, Bool, etc.).
    fn addBuiltinModuleNameCompletions(self: *CompletionBuilder) !void {
        // We use the builtin type list as module names, because builtin modules
        // are surfaced as top-level namespaces for completion.
        for (builtin_completion.BUILTIN_TYPES) |builtin_name| {
            _ = try self.addItem(.{
                .label = builtin_name,
                .kind = @intFromEnum(CompletionItemKind.module),
                .detail = null,
            });
        }
    }

    /// Add module member completions from a specific ModuleEnv.
    pub fn addModuleMemberCompletionsFromModuleEnv(
        self: *CompletionBuilder,
        module_env: *ModuleEnv,
        module_name: []const u8,
    ) !void {
        var type_writer = module_env.initTypeWriter() catch null;
        defer if (type_writer) |*tw| tw.deinit();

        const exposed = &module_env.common.exposed_items;
        var iter = exposed.iterator();
        while (iter.next()) |exp_entry| {
            const ident_idx: base.Ident.Idx = @bitCast(exp_entry.ident_idx);
            const name = module_env.common.idents.getText(ident_idx);
            if (name.len == 0) continue;

            const without_module = stripModulePrefix(name, module_env.module_name);
            const label = blk: {
                // Module exports can be qualified (Module.member) or unqualified (member).
                // Prefer matching the actual module name to avoid leaking unrelated items,
                // but allow unqualified names when we are completing the module itself.
                const dot_index = std.mem.indexOfScalar(u8, without_module, '.');
                if (dot_index == null) {
                    if (!std.mem.eql(u8, module_env.module_name, module_name)) continue;
                    break :blk without_module;
                }

                if (!std.mem.startsWith(u8, without_module, module_name)) continue;
                if (without_module.len <= module_name.len or without_module[module_name.len] != '.') continue;

                const remainder = without_module[module_name.len + 1 ..];
                break :blk firstSegment(remainder);
            };
            if (label.len == 0) continue;

            const kind: u32 = if (label.len > 0 and std.ascii.isUpper(label[0]))
                @intFromEnum(CompletionItemKind.class)
            else
                @intFromEnum(CompletionItemKind.function);

            var detail: ?[]const u8 = null;
            var documentation: ?[]const u8 = null;
            if (type_writer) |*tw| {
                if (module_env.common.getNodeIndexById(self.allocator, ident_idx)) |node_idx| {
                    if (node_idx != 0) {
                        const def_idx: CIR.Def.Idx = @enumFromInt(node_idx);
                        const def = module_env.store.getDef(def_idx);
                        const type_var = ModuleEnv.varFrom(def.pattern);
                        // Type formatting is best-effort; missing type info is acceptable
                        tw.write(type_var, .one_line) catch {};
                        const type_str = tw.get();
                        if (type_str.len > 0) {
                            detail = self.allocator.dupe(u8, type_str) catch null;
                        }
                        tw.reset();

                        documentation = doc_comments.extractDocForDef(
                            self.allocator,
                            module_env.common.source,
                            &module_env.store,
                            def,
                        ) catch null;
                    }
                }
            }

            _ = try self.addItem(.{
                .label = label,
                .kind = kind,
                .detail = detail,
                .documentation = documentation,
            });
        }
    }

    /// Add completions for members under a qualified namespace chain.
    ///
    /// This handles nested nominal/module-like chains that are represented in
    /// CIR as fully qualified identifiers, e.g. `pkg.MyType.Sub.ta` while the
    /// cursor chain is `MyType.Sub.`.
    pub fn addNamespaceMemberCompletions(
        self: *CompletionBuilder,
        module_env: *ModuleEnv,
        namespace_chain: []const u8,
    ) !bool {
        var added_any = false;

        // Top-level defs.
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const ident_idx = module_lookup.extractIdentFromPattern(&module_env.store, def.pattern) orelse continue;
            const full_name = module_env.getIdentText(ident_idx);
            const label = namespaceMemberLabel(full_name, namespace_chain) orelse continue;
            if (label.len == 0) continue;

            const kind: u32 = if (std.ascii.isUpper(label[0]))
                @intFromEnum(CompletionItemKind.class)
            else
                @intFromEnum(CompletionItemKind.field);

            if (try self.addItem(.{ .label = label, .kind = kind, .detail = null })) {
                added_any = true;
            }
        }

        // Statement-bound defs (app-style declarations).
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const parts = getStatementParts(stmt);
            const pattern_idx = parts.pattern orelse continue;
            const ident_idx = module_lookup.extractIdentFromPattern(&module_env.store, pattern_idx) orelse continue;
            const full_name = module_env.getIdentText(ident_idx);
            const label = namespaceMemberLabel(full_name, namespace_chain) orelse continue;
            if (label.len == 0) continue;

            const kind: u32 = if (std.ascii.isUpper(label[0]))
                @intFromEnum(CompletionItemKind.class)
            else
                @intFromEnum(CompletionItemKind.field);

            if (try self.addItem(.{ .label = label, .kind = kind, .detail = null })) {
                added_any = true;
            }
        }

        return added_any;
    }

    // Type Completions

    /// Add type completions for type annotation context.
    pub fn addTypeCompletions(self: *CompletionBuilder, module_env: *ModuleEnv) !void {
        try self.addTypeNamesFromModuleEnv(module_env);
    }

    /// Add type completions from all modules in BuildEnv.
    pub fn addTypeCompletionsFromEnv(self: *CompletionBuilder, env: *BuildEnv) !void {
        var sched_it = env.schedulers.iterator();
        while (sched_it.next()) |entry| {
            const sched = entry.value_ptr.*;
            for (sched.modules.items) |*module_state| {
                if (module_state.env) |*module_env| {
                    try self.addTypeNamesFromModuleEnv(module_env);
                }
            }
        }
    }

    /// Add type names (aliases and nominals) from a ModuleEnv.
    fn addTypeNamesFromModuleEnv(self: *CompletionBuilder, module_env: *ModuleEnv) !void {
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_alias_decl, .s_nominal_decl => {
                    const header_idx = switch (stmt) {
                        .s_alias_decl => |a| a.header,
                        .s_nominal_decl => |n| n.header,
                        else => unreachable,
                    };
                    const header = module_env.store.getTypeHeader(header_idx);
                    const name = module_env.getIdentText(header.name);
                    if (name.len == 0) continue;

                    const documentation = doc_comments.extractDocForStatement(
                        self.allocator,
                        module_env.common.source,
                        &module_env.store,
                        stmt,
                        stmt_idx,
                    ) catch null;

                    _ = try self.addItem(.{
                        .label = name,
                        .kind = @intFromEnum(CompletionItemKind.class),
                        .detail = null,
                        .documentation = documentation,
                    });
                },
                else => {},
            }
        }
    }

    // Local Completions

    /// Add local definition completions (variables, functions in scope).
    pub fn addLocalCompletions(self: *CompletionBuilder, module_env: *ModuleEnv, cursor_offset: u32) !void {
        // Initialize type writer for formatting types
        var type_writer = module_env.initTypeWriter() catch null;
        defer if (type_writer) |*tw| tw.deinit();

        const scope = self.getOrBuildScope(module_env);

        // Add local variables in scope at cursor position
        for (scope.bindings.items) |binding| {
            if (!scope_map.ScopeMap.isVisibleAt(binding, cursor_offset)) continue;

            const name = module_env.getIdentText(binding.ident);
            if (name.len == 0) continue;

            const label = lastSegment(name);
            if (label.len == 0) continue;

            // Determine kind - parameters and local variables
            const kind: u32 = @intFromEnum(CompletionItemKind.variable);

            // Get type information for the binding
            var detail: ?[]const u8 = null;
            if (type_writer) |*tw| {
                const type_var = ModuleEnv.varFrom(binding.pattern_idx);
                // Type formatting is best-effort; missing type info is acceptable
                tw.write(type_var, .one_line) catch {};
                const type_str = tw.get();
                if (type_str.len > 0) {
                    detail = self.allocator.dupe(u8, type_str) catch null;
                }
                tw.reset();
            }

            const documentation = doc_comments.extractDocCommentBefore(
                self.allocator,
                module_env.common.source,
                module_env.store.getPatternRegion(binding.pattern_idx).start.offset,
            ) catch null;

            _ = try self.addItem(.{
                .label = label,
                .kind = kind,
                .detail = detail,
                .documentation = documentation,
            });
        }

        // Add definitions from all_defs (top-level)
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const pattern = module_env.store.getPattern(def.pattern);

            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            const name = module_env.getIdentText(ident_idx);
            if (name.len == 0) continue;
            if (std.mem.indexOfScalar(u8, name, '.') != null) continue;

            // Determine completion kind based on the expression type
            const expr = module_env.store.getExpr(def.expr);
            const kind: u32 = switch (expr) {
                .e_closure, .e_lambda, .e_hosted_lambda => @intFromEnum(CompletionItemKind.function),
                else => @intFromEnum(CompletionItemKind.variable),
            };

            // Get type information for the definition
            var detail: ?[]const u8 = null;
            if (type_writer) |*tw| {
                const type_var = ModuleEnv.varFrom(def.pattern);
                // Type formatting is best-effort; missing type info is acceptable
                tw.write(type_var, .one_line) catch {};
                const type_str = tw.get();
                if (type_str.len > 0) {
                    detail = self.allocator.dupe(u8, type_str) catch null;
                }
                tw.reset();
            }

            const documentation = doc_comments.extractDocForDef(
                self.allocator,
                module_env.common.source,
                &module_env.store,
                def,
            ) catch null;

            _ = try self.addItem(.{
                .label = name,
                .kind = kind,
                .detail = detail,
                .documentation = documentation,
            });
        }

        // Also check statements (apps use statements for definitions)
        const local_statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (local_statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const stmt_parts = getStatementParts(stmt);

            if (stmt_parts.pattern) |pattern_idx| {
                const pattern = module_env.store.getPattern(pattern_idx);

                const ident_idx = switch (pattern) {
                    .assign => |p| p.ident,
                    .as => |p| p.ident,
                    else => continue,
                };

                const name = module_env.getIdentText(ident_idx);
                if (name.len == 0) continue;
                if (std.mem.indexOfScalar(u8, name, '.') != null) continue;

                // Determine completion kind
                var kind: u32 = @intFromEnum(CompletionItemKind.variable);
                if (stmt_parts.expr) |expr_idx| {
                    const expr = module_env.store.getExpr(expr_idx);
                    kind = switch (expr) {
                        .e_closure, .e_lambda, .e_hosted_lambda => @intFromEnum(CompletionItemKind.function),
                        else => @intFromEnum(CompletionItemKind.variable),
                    };
                }

                // Get type information
                var detail: ?[]const u8 = null;
                if (type_writer) |*tw| {
                    const type_var = ModuleEnv.varFrom(pattern_idx);
                    // Type formatting is best-effort; missing type info is acceptable
                    tw.write(type_var, .one_line) catch {};
                    const type_str = tw.get();
                    if (type_str.len > 0) {
                        detail = self.allocator.dupe(u8, type_str) catch null;
                    }
                    tw.reset();
                }

                const documentation = doc_comments.extractDocForStatement(
                    self.allocator,
                    module_env.common.source,
                    &module_env.store,
                    stmt,
                    stmt_idx,
                ) catch null;

                _ = try self.addItem(.{
                    .label = name,
                    .kind = kind,
                    .detail = detail,
                    .documentation = documentation,
                });
            }
        }
    }

    // Record Field Completions

    /// Add record field completions for a record variable access (e.g., "myRecord.").
    pub fn addRecordFieldCompletions(
        self: *CompletionBuilder,
        module_env: *ModuleEnv,
        variable_name: []const u8,
        variable_start: u32,
    ) !void {
        self.logDebug("addRecordFieldCompletions: looking for '{s}' at offset {d}", .{ variable_name, variable_start });

        // Look up the definition by name in top-level defs/statements.
        // We use def/stmt lookup rather than ScopeMap bindings because the
        // binding's pattern_idx may come from incomplete code and lack full
        // type info (especially when using snapshots).

        // Check top-level definitions
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        self.logDebug("addRecordFieldCompletions: checking {d} top-level defs", .{defs_slice.len});

        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const pattern = module_env.store.getPattern(def.pattern);

            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            const name = module_env.getIdentText(ident_idx);
            self.logDebug("addRecordFieldCompletions: def '{s}' pattern={} has_annotation={}", .{ name, def.pattern, def.annotation != null });
            if (std.mem.eql(u8, name, variable_name)) {
                self.logDebug("addRecordFieldCompletions: FOUND def '{s}' pattern={} annotation={?}", .{ name, def.pattern, def.annotation });

                // Found the definition - get its type and extract record fields
                const type_var = ModuleEnv.varFrom(def.pattern);
                self.logDebug("addRecordFieldCompletions: type_var for def={}", .{type_var});
                try self.addFieldsFromTypeVar(module_env, type_var);
                return;
            }
        }

        // Check statements (apps use statements for definitions)
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        self.logDebug("addRecordFieldCompletions: checking {d} statements", .{statements_slice.len});
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const pattern_idx = switch (stmt) {
                .s_decl => |decl| decl.pattern,
                .s_var => |var_stmt| var_stmt.pattern_idx,
                else => continue,
            };

            const pattern = module_env.store.getPattern(pattern_idx);
            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            const name = module_env.getIdentText(ident_idx);
            self.logDebug("addRecordFieldCompletions: stmt '{s}'", .{name});
            if (std.mem.eql(u8, name, variable_name)) {
                self.logDebug("addRecordFieldCompletions: FOUND stmt '{s}'", .{name});
                // Found the definition - get its type and extract record fields
                const type_var = ModuleEnv.varFrom(pattern_idx);
                try self.addFieldsFromTypeVar(module_env, type_var);
                return;
            }
        }
    }

    /// Extract and add record fields from a type variable.
    pub fn addFieldsFromTypeVar(self: *CompletionBuilder, module_env: *ModuleEnv, type_var: types.Var) !void {
        const type_store = &module_env.types;

        var resolved = type_store.resolveVar(type_var);
        var content = resolved.desc.content;

        self.logDebug("addFieldsFromTypeVar: type_var={}, content tag={s}", .{ type_var, @tagName(content) });

        var steps: usize = 0;
        while (true) : (steps += 1) {
            if (steps > 8) break;

            if (content.unwrapRecord()) |record| {
                self.logDebug("addFieldsFromTypeVar: found record after resolve", .{});
                try self.addFieldsFromRecord(module_env, record);
                return;
            }

            switch (content) {
                .alias => |alias| {
                    const backing_var = type_store.getAliasBackingVar(alias);
                    resolved = type_store.resolveVar(backing_var);
                    content = resolved.desc.content;
                    continue;
                },
                .structure => |flat_type| {
                    switch (flat_type) {
                        .nominal_type => |nominal| {
                            const backing_var = type_store.getNominalBackingVar(nominal);
                            resolved = type_store.resolveVar(backing_var);
                            content = resolved.desc.content;
                            continue;
                        },
                        else => break,
                    }
                },
                else => break,
            }
        }

        // Try to get record fields, handling aliases that wrap records
        try self.addFieldsFromContent(module_env, content, 0);
    }

    /// Extract and add tuple element index completions from a type variable.
    /// For tuple types, adds completions like "0", "1", "2" for each element.
    pub fn addTupleIndexCompletions(self: *CompletionBuilder, module_env: *ModuleEnv, type_var: types.Var) !void {
        const type_store = &module_env.types;

        var resolved = type_store.resolveVar(type_var);
        var content = resolved.desc.content;

        var steps: usize = 0;
        while (true) : (steps += 1) {
            if (steps > 8) break;

            switch (content) {
                .structure => |flat_type| {
                    switch (flat_type) {
                        .tuple => |tuple| {
                            const elem_vars = type_store.sliceVars(tuple.elems);

                            var type_writer = module_env.initTypeWriter() catch null;
                            defer if (type_writer) |*tw| tw.deinit();

                            for (elem_vars, 0..) |elem_var, i| {
                                var detail: ?[]const u8 = null;
                                if (type_writer) |*tw| {
                                    // Type formatting is best-effort; missing type info is acceptable
                                    tw.write(elem_var, .one_line) catch {};
                                    const type_str = tw.get();
                                    if (type_str.len > 0) {
                                        detail = self.allocator.dupe(u8, type_str) catch null;
                                    }
                                    tw.reset();
                                }

                                // Use a stack buffer; addItem duplicates accepted labels.
                                var label_buf: [32]u8 = undefined;
                                const label = std.fmt.bufPrint(&label_buf, "{d}", .{i}) catch continue;
                                _ = try self.addItem(.{
                                    .label = label,
                                    .kind = @intFromEnum(CompletionItemKind.field),
                                    .detail = detail,
                                });
                            }
                            return;
                        },
                        .nominal_type => |nominal| {
                            const backing_var = type_store.getNominalBackingVar(nominal);
                            resolved = type_store.resolveVar(backing_var);
                            content = resolved.desc.content;
                            continue;
                        },
                        else => break,
                    }
                },
                .alias => |alias| {
                    const backing_var = type_store.getAliasBackingVar(alias);
                    resolved = type_store.resolveVar(backing_var);
                    content = resolved.desc.content;
                    continue;
                },
                else => break,
            }
        }
    }

    /// Add record field completions for a named definition in a specific module.
    ///
    /// This is used for module member accesses (e.g., Module.value.) where the
    /// member is a record. We resolve the member's type from the module's
    /// definition table, then extract its record fields.
    pub fn addRecordFieldsForModuleMember(self: *CompletionBuilder, module_env: *ModuleEnv, member_name: []const u8) !bool {
        if (module_lookup.findDefinitionByName(module_env, member_name)) |def_info| {
            const type_var = ModuleEnv.varFrom(def_info.pattern_idx);
            try self.addFieldsFromTypeVar(module_env, type_var);
            return true;
        }

        return false;
    }

    /// Resolve a record field's type variable from a receiver type.
    ///
    /// This unwraps aliases to find records and returns the field's type var
    /// for chained access resolution (e.g., myrec.subrec.).
    pub fn getFieldTypeVarFromTypeVar(
        _: *CompletionBuilder,
        module_env: *ModuleEnv,
        type_var: types.Var,
        field_name: []const u8,
    ) ?types.Var {
        const type_store = &module_env.types;

        var resolved = type_store.resolveVar(type_var);
        var content = resolved.desc.content;

        var steps: usize = 0;
        while (steps < 8) : (steps += 1) {
            if (content.unwrapRecord()) |record| {
                return findFieldVarInRecord(module_env, record, field_name);
            }

            switch (content) {
                .alias => |alias| {
                    const backing_var = type_store.getAliasBackingVar(alias);
                    resolved = type_store.resolveVar(backing_var);
                    content = resolved.desc.content;
                    continue;
                },
                .structure => |flat_type| {
                    switch (flat_type) {
                        .nominal_type => |nominal| {
                            const backing_var = type_store.getNominalBackingVar(nominal);
                            resolved = type_store.resolveVar(backing_var);
                            content = resolved.desc.content;
                            continue;
                        },
                        else => break,
                    }
                },
                else => break,
            }
        }

        return null;
    }

    /// Recursively extract fields from type content, unwrapping aliases.
    fn addFieldsFromContent(
        self: *CompletionBuilder,
        module_env: *ModuleEnv,
        content: types.Content,
        depth: usize,
    ) !void {
        const type_store = &module_env.types;

        self.logDebug("addFieldsFromContent: content tag={s}", .{@tagName(content)});

        if (depth > 16) return;

        // Check if this is directly a record
        if (content.unwrapRecord()) |record| {
            self.logDebug("addFieldsFromContent: found record!", .{});
            try self.addFieldsFromRecord(module_env, record);
            return;
        }

        // Check if this is an alias (e.g., a type alias wrapping a record)
        switch (content) {
            .alias => |alias| {
                self.logDebug("addFieldsFromContent: unwrapping alias", .{});
                // Get the backing type of the alias
                const backing_var = type_store.getAliasBackingVar(alias);
                const backing_resolved = type_store.resolveVar(backing_var);
                try self.addFieldsFromContent(module_env, backing_resolved.desc.content, depth + 1);
            },
            .structure => |flat_type| {
                self.logDebug("addFieldsFromContent: structure, flat_type tag={s}", .{@tagName(flat_type)});
            },
            else => {
                self.logDebug("addFieldsFromContent: not a record or alias, tag={s}", .{@tagName(content)});
            },
        }
    }

    /// Add completion items for fields in a record type.
    fn addFieldsFromRecord(self: *CompletionBuilder, module_env: *ModuleEnv, record: types.Record) !void {
        const type_store = &module_env.types;

        // Get the record fields
        const fields_slice = type_store.getRecordFieldsSlice(record.fields);
        const field_names = fields_slice.items(.name);
        const field_vars = fields_slice.items(.var_);

        self.logDebug("addFieldsFromRecord: record.fields={}, fields_slice.len={}, field_names.len={d}", .{ record.fields, fields_slice.len, field_names.len });

        // Initialize type writer for formatting field types
        var type_writer = module_env.initTypeWriter() catch null;
        defer if (type_writer) |*tw| tw.deinit();

        // Iterate over record fields
        for (field_names, field_vars) |field_name_idx, field_var| {
            const field_name = module_env.getIdentText(field_name_idx);
            self.logDebug("addFieldsFromRecord: field '{s}'", .{field_name});
            if (field_name.len == 0) continue;

            // Get field type for detail
            var detail: ?[]const u8 = null;
            if (type_writer) |*tw| {
                // Type formatting is best-effort; missing type info is acceptable
                tw.write(field_var, .one_line) catch {};
                const type_str = tw.get();
                if (type_str.len > 0) {
                    detail = self.allocator.dupe(u8, type_str) catch null;
                }
                tw.reset();
            }

            _ = try self.addItem(.{
                .label = field_name,
                .kind = @intFromEnum(CompletionItemKind.field),
                .detail = detail,
            });
        }
    }

    /// Find the type var for a specific field within a record.
    fn findFieldVarInRecord(module_env: *ModuleEnv, record: types.Record, field_name: []const u8) ?types.Var {
        const type_store = &module_env.types;
        const fields_slice = type_store.getRecordFieldsSlice(record.fields);
        const field_names = fields_slice.items(.name);
        const field_vars = fields_slice.items(.var_);

        for (field_names, field_vars) |field_name_idx, field_var| {
            const name = module_env.getIdentText(field_name_idx);
            if (std.mem.eql(u8, name, field_name)) {
                return field_var;
            }
        }

        return null;
    }

    // Method Completions

    /// Add method completions for static dispatch (e.g., "value.method()").
    pub fn addMethodCompletions(
        self: *CompletionBuilder,
        module_env: *ModuleEnv,
        variable_name: []const u8,
        variable_start: u32,
    ) !void {
        self.logDebug("addMethodCompletions: looking for '{s}' at offset {d}", .{ variable_name, variable_start });

        const scope = self.getOrBuildScope(module_env);
        self.logDebug("addMethodCompletions: scope has {d} bindings", .{scope.bindings.items.len});

        // Find the binding with matching name that's visible at the variable position
        var found_binding: ?scope_map.Binding = null;
        for (scope.bindings.items) |binding| {
            const name = module_env.getIdentText(binding.ident);
            const is_visible = scope_map.ScopeMap.isVisibleAt(binding, variable_start);
            if (!is_visible) continue;
            if (std.mem.eql(u8, name, variable_name)) {
                self.logDebug("addMethodCompletions: FOUND binding '{s}'", .{name});
                found_binding = binding;
                break;
            }
        }

        // Also check top-level definitions
        if (found_binding == null) {
            const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
            self.logDebug("addMethodCompletions: checking {d} top-level defs", .{defs_slice.len});
            for (defs_slice) |def_idx| {
                const def = module_env.store.getDef(def_idx);
                const pattern = module_env.store.getPattern(def.pattern);

                const ident_idx = switch (pattern) {
                    .assign => |p| p.ident,
                    .as => |p| p.ident,
                    else => continue,
                };

                const name = module_env.getIdentText(ident_idx);
                if (std.mem.eql(u8, name, variable_name)) {
                    self.logDebug("addMethodCompletions: FOUND def '{s}'", .{name});
                    // Found the definition - get its type and find methods
                    const type_var = ModuleEnv.varFrom(def.pattern);
                    try self.addMethodsFromTypeVar(module_env, type_var);
                    return;
                }
            }
        }

        // Also check statements (apps use statements for definitions)
        if (found_binding == null) {
            const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
            self.logDebug("addMethodCompletions: checking {d} statements", .{statements_slice.len});
            for (statements_slice) |stmt_idx| {
                const stmt = module_env.store.getStatement(stmt_idx);
                const pattern_idx = switch (stmt) {
                    .s_decl => |decl| decl.pattern,
                    .s_var => |var_stmt| var_stmt.pattern_idx,
                    else => continue,
                };

                const pattern = module_env.store.getPattern(pattern_idx);
                const ident_idx = switch (pattern) {
                    .assign => |p| p.ident,
                    .as => |p| p.ident,
                    else => continue,
                };

                const name = module_env.getIdentText(ident_idx);
                if (std.mem.eql(u8, name, variable_name)) {
                    self.logDebug("addMethodCompletions: FOUND stmt '{s}'", .{name});
                    // Found the definition - get its type and find methods
                    const type_var = ModuleEnv.varFrom(pattern_idx);
                    try self.addMethodsFromTypeVar(module_env, type_var);
                    return;
                }
            }
        }

        if (found_binding) |binding| {
            self.logDebug("addMethodCompletions: using binding pattern_idx={}", .{binding.pattern_idx});
            if (self.findExprTypeForPattern(module_env, binding.pattern_idx)) |type_var| {
                try self.addMethodsFromTypeVar(module_env, type_var);
                return;
            }
            // Get the type of this binding and find methods
            const type_var = ModuleEnv.varFrom(binding.pattern_idx);
            try self.addMethodsFromTypeVar(module_env, type_var);
        }
    }

    /// Extract methods available for a type variable and add them as completions.
    pub fn addMethodsFromTypeVar(self: *CompletionBuilder, module_env: *ModuleEnv, type_var: types.Var) !void {
        const type_store = &module_env.types;

        self.logDebug("addMethodsFromTypeVar: type_var={}", .{type_var});
        var resolved = type_store.resolveVar(type_var);
        var content = resolved.desc.content;

        var steps: usize = 0;
        while (true) : (steps += 1) {
            if (steps > 8) {
                self.logDebug("addMethodsFromTypeVar: hit step limit", .{});
                break;
            }

            const type_ident_opt: ?base.Ident.Idx = switch (content) {
                .alias => |alias| alias.ident.ident_idx,
                .structure => |flat_type| switch (flat_type) {
                    .nominal_type => |nominal| nominal.ident.ident_idx,
                    else => null,
                },
                else => null,
            };

            if (type_ident_opt) |type_ident| {
                const type_name = module_env.getIdentText(type_ident);
                self.logDebug("addMethodsFromTypeVar: type_ident={any} name={s}", .{ type_ident, type_name });

                // Route builtin type methods through the builtin module env to ensure
                // completions include real Builtin.roc backing data.
                if (self.builtin_module_env) |builtin_env| {
                    if (builtin_completion.isBuiltinType(type_name)) {
                        try self.addMethodsForTypeNameInEnv(builtin_env, type_name);
                    } else {
                        try self.addMethodsForTypeIdentInEnv(module_env, type_ident);
                    }
                } else {
                    try self.addMethodsForTypeIdentInEnv(module_env, type_ident);
                }
            }

            switch (content) {
                .flex => |flex| {
                    self.logDebug("addMethodsFromTypeVar: flex constraints", .{});
                    // Extract method names from flex constraints
                    try self.addMethodsFromConstraints(module_env, flex.constraints);
                    break;
                },
                .rigid => |rigid| {
                    self.logDebug("addMethodsFromTypeVar: rigid constraints", .{});
                    // Extract method names from rigid constraints
                    try self.addMethodsFromConstraints(module_env, rigid.constraints);
                    break;
                },
                .alias => |alias| {
                    self.logDebug("addMethodsFromTypeVar: alias unwrap", .{});
                    const backing_var = type_store.getAliasBackingVar(alias);
                    resolved = type_store.resolveVar(backing_var);
                    content = resolved.desc.content;
                    continue;
                },
                else => break,
            }
        }
    }

    /// Extract method names from static dispatch constraints and add them as completions.
    fn addMethodsFromConstraints(
        self: *CompletionBuilder,
        module_env: *ModuleEnv,
        constraints: types.StaticDispatchConstraint.SafeList.Range,
    ) !void {
        if (constraints.isEmpty()) {
            self.logDebug("addMethodsFromConstraints: empty", .{});
            return;
        }

        const constraints_slice = module_env.types.sliceStaticDispatchConstraints(constraints);
        self.logDebug("addMethodsFromConstraints: count={d}", .{constraints_slice.len});
        for (constraints_slice) |constraint| {
            const method_name = module_env.getIdentText(constraint.fn_name);

            if (method_name.len == 0) continue;

            // Try to get detail from the constraint's function type
            var detail: ?[]const u8 = null;
            var type_writer = module_env.initTypeWriter() catch null;
            defer if (type_writer) |*tw| tw.deinit();

            if (type_writer) |*tw| {
                // Type formatting is best-effort; missing type info is acceptable
                tw.write(constraint.fn_var, .one_line) catch {};
                const type_str = tw.get();
                if (type_str.len > 0) {
                    detail = self.allocator.dupe(u8, type_str) catch null;
                }
                tw.reset();
            }

            _ = try self.addItem(.{
                .label = method_name,
                .kind = @intFromEnum(CompletionItemKind.method),
                .detail = detail,
            });
        }
    }

    /// Find the type of an expression for a pattern.
    fn findExprTypeForPattern(self: *CompletionBuilder, module_env: *ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?types.Var {
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            if (def.pattern == pattern_idx) {
                self.logDebug("findExprTypeForPattern: def expr for pattern_idx={} expr_idx={}", .{ pattern_idx, def.expr });
                return ModuleEnv.varFrom(def.expr);
            }
        }

        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const stmt_parts = getStatementParts(stmt);
            if (stmt_parts.pattern) |stmt_pattern_idx| {
                if (stmt_pattern_idx == pattern_idx) {
                    if (stmt_parts.expr) |expr_idx| {
                        self.logDebug("findExprTypeForPattern: stmt expr for pattern_idx={} expr_idx={}", .{ pattern_idx, expr_idx });
                        return ModuleEnv.varFrom(expr_idx);
                    }
                }
            }
        }

        self.logDebug("findExprTypeForPattern: no expr for pattern_idx={}", .{pattern_idx});
        return null;
    }

    /// Add methods for a specific type identifier by searching method_idents.
    fn addMethodsForTypeIdentInEnv(self: *CompletionBuilder, module_env: *ModuleEnv, type_ident: base.Ident.Idx) !void {
        // Initialize type writer for formatting method signatures
        var type_writer = module_env.initTypeWriter() catch null;
        defer if (type_writer) |*tw| tw.deinit();

        // Get the type name for display purposes
        const type_name = module_env.getIdentText(type_ident);

        // Iterate through method_idents to find all methods for this type.
        // The method_idents maps (type_ident, method_ident) -> qualified_ident.
        const entries = module_env.method_idents.entries.items;
        for (entries) |entry| {
            // Check if this method is for our type
            if (entry.key.type_ident == type_ident) {
                const method_ident = entry.key.method_ident;
                const method_name = module_env.getIdentText(method_ident);

                if (method_name.len == 0) continue;

                // Try to get the method's type signature
                var detail: ?[]const u8 = null;
                const qualified_ident = entry.value;
                const qualified_name = module_env.getIdentText(qualified_ident);

                // Look up the method definition to get its type
                if (self.findMethodType(module_env, qualified_ident)) |method_type_var| {
                    if (type_writer) |*tw| {
                        // Type formatting is best-effort; missing type info is acceptable
                        tw.write(method_type_var, .one_line) catch {};
                        const type_str = tw.get();
                        if (type_str.len > 0) {
                            detail = self.allocator.dupe(u8, type_str) catch null;
                        }
                        tw.reset();
                    }
                }

                // If we couldn't get the type signature, at least show which type it's from
                if (detail == null and type_name.len > 0 and qualified_name.len > 0) {
                    detail = std.fmt.allocPrint(self.allocator, "method on {s}", .{type_name}) catch null;
                }

                // Extract documentation for the method definition.
                const documentation = self.findMethodDocumentation(module_env, qualified_ident);

                _ = try self.addItem(.{
                    .label = method_name,
                    .kind = @intFromEnum(CompletionItemKind.method),
                    .detail = detail,
                    .documentation = documentation,
                });
            }
        }
    }

    /// Add methods for a type name within the provided module environment.
    ///
    /// This is used to bridge from local types to builtin module methods by
    /// resolving the type name in the builtin module ident table.
    fn addMethodsForTypeNameInEnv(self: *CompletionBuilder, module_env: *ModuleEnv, type_name: []const u8) !void {
        // Try direct lookup first (e.g., "Str")
        if (module_env.common.findIdent(type_name)) |type_ident| {
            try self.addMethodsForTypeIdentInEnv(module_env, type_ident);
            return;
        }

        // Fall back to fully-qualified lookup (e.g., "Builtin.Str")
        const qualified = std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ module_env.module_name, type_name }) catch return;
        defer self.allocator.free(qualified);

        if (module_env.common.findIdent(qualified)) |type_ident| {
            try self.addMethodsForTypeIdentInEnv(module_env, type_ident);
        }
    }

    /// Find the documentation string for a method by its qualified identifier.
    ///
    /// Searches top-level definitions and statements for a def/decl whose
    /// pattern ident matches `qualified_ident`, then extracts the doc comment.
    fn findMethodDocumentation(self: *CompletionBuilder, module_env: *ModuleEnv, qualified_ident: base.Ident.Idx) ?[]const u8 {
        // Search all_defs for a matching definition.
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const pattern = module_env.store.getPattern(def.pattern);

            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            if (ident_idx == qualified_ident) {
                return doc_comments.extractDocForDef(
                    self.allocator,
                    module_env.common.source,
                    &module_env.store,
                    def,
                ) catch null;
            }
        }

        // Fall back to statements (apps use s_decl/s_var for definitions).
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const pattern_idx = switch (stmt) {
                .s_decl => |decl| decl.pattern,
                .s_var => |var_stmt| var_stmt.pattern_idx,
                else => continue,
            };

            const pattern = module_env.store.getPattern(pattern_idx);
            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            if (ident_idx == qualified_ident) {
                return doc_comments.extractDocForStatement(
                    self.allocator,
                    module_env.common.source,
                    &module_env.store,
                    stmt,
                    stmt_idx,
                ) catch null;
            }
        }

        return null;
    }

    /// Find the type of a method definition by its qualified identifier.
    fn findMethodType(_: *CompletionBuilder, module_env: *ModuleEnv, qualified_ident: base.Ident.Idx) ?types.Var {
        // Look through definitions to find the method
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const pattern = module_env.store.getPattern(def.pattern);

            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            if (ident_idx == qualified_ident) {
                return ModuleEnv.varFrom(def.pattern);
            }
        }

        // Also check statements
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const pattern_idx = switch (stmt) {
                .s_decl => |decl| decl.pattern,
                .s_var => |var_stmt| var_stmt.pattern_idx,
                else => continue,
            };

            const pattern = module_env.store.getPattern(pattern_idx);
            const ident_idx = switch (pattern) {
                .assign => |p| p.ident,
                .as => |p| p.ident,
                else => continue,
            };

            if (ident_idx == qualified_ident) {
                return ModuleEnv.varFrom(pattern_idx);
            }
        }

        return null;
    }

    // Tag Union Completions

    /// Add tag completions for a nominal type (e.g., `Color.` → `Red`, `Green`, `Blue`).
    /// Entry point for `TypeName.` completion on nominal types.
    ///
    /// Returns true if tags were found and added, false otherwise.
    pub fn addTagCompletionsForNominalType(
        self: *CompletionBuilder,
        module_env: *ModuleEnv,
        type_name: []const u8,
        requesting_module_name: ?[]const u8, // null = same module (always allowed)
    ) !bool {
        // Search all_statements for s_nominal_decl matching type_name
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_nominal_decl => |nom_decl| {
                    const header = module_env.store.getTypeHeader(nom_decl.header);
                    const rel_name = module_env.getIdentText(header.relative_name);

                    if (!std.mem.eql(u8, rel_name, type_name)) continue;

                    // Opaque check: if is_opaque and from another module, skip
                    if (nom_decl.is_opaque and requesting_module_name != null) {
                        if (!std.mem.eql(u8, requesting_module_name.?, module_env.module_name)) {
                            return false;
                        }
                    }

                    // Get the backing type annotation
                    const anno = module_env.store.getTypeAnno(nom_decl.anno);
                    return try self.addTagsFromTypeAnno(module_env, anno);
                },
                else => {},
            }
        }
        return false;
    }

    /// Extracts tags from a type annotation (handles tag_union directly).
    fn addTagsFromTypeAnno(
        self: *CompletionBuilder,
        module_env: *ModuleEnv,
        anno: CIR.TypeAnno,
    ) !bool {
        switch (anno) {
            .tag_union => |tu| {
                const tags_slice = module_env.store.sliceTypeAnnos(tu.tags);
                for (tags_slice) |tag_anno_idx| {
                    const tag_anno = module_env.store.getTypeAnno(tag_anno_idx);
                    switch (tag_anno) {
                        .tag => |t| {
                            const tag_name = module_env.getIdentText(t.name);
                            if (tag_name.len == 0) continue;

                            // Show the tag signature (e.g. "SubVal(Str)") as detail
                            const detail = self.formatTagSignature(module_env, tag_name, t.args);

                            _ = try self.addItem(.{
                                .label = tag_name,
                                .kind = @intFromEnum(CompletionItemKind.enum_member),
                                .detail = detail,
                            });
                        },
                        else => {},
                    }
                }
                return true;
            },
            else => return false,
        }
    }

    /// Format a tag signature like "SubVal(Str)" or "Cons(a, List(a))".
    /// Returns null for tags with no arguments.
    fn formatTagSignature(self: *CompletionBuilder, module_env: *ModuleEnv, tag_name: []const u8, args: CIR.TypeAnno.Span) ?[]const u8 {
        const args_slice = module_env.store.sliceTypeAnnos(args);
        if (args_slice.len == 0) return null;

        return self.formatTagSignatureInner(module_env, tag_name, args_slice) catch null;
    }

    fn formatTagSignatureInner(self: *CompletionBuilder, module_env: *ModuleEnv, tag_name: []const u8, args_slice: []const CIR.TypeAnno.Idx) ![]const u8 {
        var buf = std.ArrayList(u8){};
        errdefer buf.deinit(self.allocator);
        try buf.appendSlice(self.allocator, tag_name);
        try buf.append(self.allocator, '(');
        for (args_slice, 0..) |arg_idx, i| {
            if (i > 0) try buf.appendSlice(self.allocator, ", ");
            const arg_anno = module_env.store.getTypeAnno(arg_idx);
            self.writeTypeAnno(&buf, module_env, arg_anno);
        }
        try buf.append(self.allocator, ')');
        return try buf.toOwnedSlice(self.allocator);
    }

    /// Write a human-readable representation of a TypeAnno to a buffer.
    fn writeTypeAnno(self: *CompletionBuilder, buf: *std.ArrayList(u8), module_env: *ModuleEnv, anno: CIR.TypeAnno) void {
        const alloc = self.allocator;
        switch (anno) {
            .lookup => |t| {
                const name = module_env.getIdentText(t.name);
                buf.appendSlice(alloc, stripBuiltinPrefix(name)) catch return;
            },
            .rigid_var => |t| {
                buf.appendSlice(alloc, module_env.getIdentText(t.name)) catch return;
            },
            .rigid_var_lookup => |t| {
                const ref_anno = module_env.store.getTypeAnno(t.ref);
                self.writeTypeAnno(buf, module_env, ref_anno);
            },
            .apply => |a| {
                const name = module_env.getIdentText(a.name);
                buf.appendSlice(alloc, stripBuiltinPrefix(name)) catch return;
                const apply_args = module_env.store.sliceTypeAnnos(a.args);
                if (apply_args.len > 0) {
                    buf.append(alloc, '(') catch return;
                    for (apply_args, 0..) |arg_idx, i| {
                        if (i > 0) buf.appendSlice(alloc, ", ") catch return;
                        const arg_anno = module_env.store.getTypeAnno(arg_idx);
                        self.writeTypeAnno(buf, module_env, arg_anno);
                    }
                    buf.append(alloc, ')') catch return;
                }
            },
            .tag_union => |tu| {
                buf.append(alloc, '[') catch return;
                const tags = module_env.store.sliceTypeAnnos(tu.tags);
                for (tags, 0..) |tag_idx, i| {
                    if (i > 0) buf.appendSlice(alloc, ", ") catch return;
                    const tag_anno = module_env.store.getTypeAnno(tag_idx);
                    self.writeTypeAnno(buf, module_env, tag_anno);
                }
                buf.append(alloc, ']') catch return;
            },
            .tag => |t| {
                buf.appendSlice(alloc, module_env.getIdentText(t.name)) catch return;
                const tag_args = module_env.store.sliceTypeAnnos(t.args);
                if (tag_args.len > 0) {
                    buf.append(alloc, '(') catch return;
                    for (tag_args, 0..) |arg_idx, i| {
                        if (i > 0) buf.appendSlice(alloc, ", ") catch return;
                        const arg_anno = module_env.store.getTypeAnno(arg_idx);
                        self.writeTypeAnno(buf, module_env, arg_anno);
                    }
                    buf.append(alloc, ')') catch return;
                }
            },
            .tuple => |t| {
                buf.append(alloc, '(') catch return;
                const elems = module_env.store.sliceTypeAnnos(t.elems);
                for (elems, 0..) |elem_idx, i| {
                    if (i > 0) buf.appendSlice(alloc, ", ") catch return;
                    const elem_anno = module_env.store.getTypeAnno(elem_idx);
                    self.writeTypeAnno(buf, module_env, elem_anno);
                }
                buf.append(alloc, ')') catch return;
            },
            .record => |r| {
                buf.append(alloc, '{') catch return;
                const fields = module_env.store.sliceAnnoRecordFields(r.fields);
                for (fields, 0..) |field_idx, i| {
                    if (i > 0) buf.appendSlice(alloc, ", ") catch return;
                    const field = module_env.store.getAnnoRecordField(field_idx);
                    buf.appendSlice(alloc, module_env.getIdentText(field.name)) catch return;
                    buf.appendSlice(alloc, ": ") catch return;
                    const field_anno = module_env.store.getTypeAnno(field.ty);
                    self.writeTypeAnno(buf, module_env, field_anno);
                }
                buf.append(alloc, '}') catch return;
            },
            .@"fn" => |f| {
                const fn_args = module_env.store.sliceTypeAnnos(f.args);
                for (fn_args, 0..) |arg_idx, i| {
                    if (i > 0) buf.appendSlice(alloc, ", ") catch return;
                    const arg_anno = module_env.store.getTypeAnno(arg_idx);
                    self.writeTypeAnno(buf, module_env, arg_anno);
                }
                buf.appendSlice(alloc, if (f.effectful) " => " else " -> ") catch return;
                const ret_anno = module_env.store.getTypeAnno(f.ret);
                self.writeTypeAnno(buf, module_env, ret_anno);
            },
            .parens => |p| {
                buf.append(alloc, '(') catch return;
                const inner = module_env.store.getTypeAnno(p.anno);
                self.writeTypeAnno(buf, module_env, inner);
                buf.append(alloc, ')') catch return;
            },
            .underscore => buf.append(alloc, '_') catch return,
            .malformed => buf.appendSlice(alloc, "?") catch return,
        }
    }

    /// Add ambient/structural tag completions from the module's type store.
    /// These are tags that appear in expression context without qualification.
    pub fn addAmbientTagCompletions(
        self: *CompletionBuilder,
        module_env: *ModuleEnv,
    ) !void {
        // Iterate all tags in the type store
        const tag_names = module_env.types.tags.items.items(.name);
        for (tag_names) |name_idx| {
            const tag_name = module_env.getIdentText(name_idx);
            if (tag_name.len == 0) continue;

            _ = try self.addItem(.{
                .label = tag_name,
                .kind = @intFromEnum(CompletionItemKind.enum_member),
                .detail = null,
            });
        }
    }
};

// Helper Functions

/// Strip "Builtin." and "Num." prefixes for display (e.g. "Builtin.Str" → "Str", "Num.U64" → "U64").
fn stripBuiltinPrefix(name: []const u8) []const u8 {
    if (std.mem.startsWith(u8, name, "Builtin.")) {
        const without_builtin = name[8..];
        if (std.mem.startsWith(u8, without_builtin, "Num.")) {
            return without_builtin[4..];
        }
        return without_builtin;
    }
    if (std.mem.startsWith(u8, name, "Num.")) {
        return name[4..];
    }
    return name;
}

/// Strip module prefix from a name.
fn stripModulePrefix(name: []const u8, module_name: []const u8) []const u8 {
    var i: usize = 0;
    while (i < name.len) {
        const seg_start = i;
        const dot_idx = std.mem.indexOfScalarPos(u8, name, seg_start, '.') orelse name.len;
        const seg = name[seg_start..dot_idx];

        if (std.mem.eql(u8, seg, module_name)) {
            if (dot_idx < name.len) return name[dot_idx + 1 ..];
            return "";
        }

        if (dot_idx == name.len) break;
        i = dot_idx + 1;
    }

    return name;
}

/// Get the first segment of a dotted name.
fn firstSegment(name: []const u8) []const u8 {
    const dot_idx = std.mem.indexOfScalar(u8, name, '.') orelse name.len;
    return name[0..dot_idx];
}

/// Get the last segment of a dotted name.
fn lastSegment(name: []const u8) []const u8 {
    const dot_idx = std.mem.lastIndexOfScalar(u8, name, '.') orelse return name;
    if (dot_idx + 1 >= name.len) return name;
    return name[dot_idx + 1 ..];
}

/// Extract the immediate child label under `namespace_chain` from a full name.
///
/// Examples:
/// - full: `MyType.Sub.ta`, chain: `MyType.Sub` => `ta`
/// - full: `pkg.MyType.Sub.ta`, chain: `MyType.Sub` => `ta`
fn namespaceMemberLabel(full_name: []const u8, namespace_chain: []const u8) ?[]const u8 {
    if (namespace_chain.len == 0) return null;

    // Direct match: `MyType.Sub.<member>`
    if (std.mem.startsWith(u8, full_name, namespace_chain) and
        full_name.len > namespace_chain.len and
        full_name[namespace_chain.len] == '.')
    {
        const remainder = full_name[namespace_chain.len + 1 ..];
        if (remainder.len == 0) return null;
        return firstSegment(remainder);
    }

    // Qualified match: `<module>.MyType.Sub.<member>`
    var i: usize = 0;
    while (i < full_name.len) : (i += 1) {
        if (full_name[i] != '.') continue;
        const start = i + 1;
        if (start >= full_name.len) break;
        if (start + namespace_chain.len >= full_name.len) break;
        if (!std.mem.eql(u8, full_name[start .. start + namespace_chain.len], namespace_chain)) continue;
        if (full_name[start + namespace_chain.len] != '.') continue;

        const remainder_start = start + namespace_chain.len + 1;
        if (remainder_start >= full_name.len) return null;
        return firstSegment(full_name[remainder_start..]);
    }

    return null;
}

/// Parts extracted from a statement for common processing.
const StatementParts = struct {
    pattern: ?CIR.Pattern.Idx,
    expr: ?CIR.Expr.Idx,
    expr2: ?CIR.Expr.Idx,
};

/// Extract common parts from a statement.
fn getStatementParts(stmt: CIR.Statement) StatementParts {
    return switch (stmt) {
        .s_decl => |decl| .{ .pattern = decl.pattern, .expr = decl.expr, .expr2 = null },
        .s_var => |var_stmt| .{ .pattern = var_stmt.pattern_idx, .expr = var_stmt.expr, .expr2 = null },
        .s_reassign => |reassign| .{ .pattern = null, .expr = reassign.expr, .expr2 = null },
        .s_for => |for_stmt| .{ .pattern = for_stmt.patt, .expr = for_stmt.expr, .expr2 = for_stmt.body },
        .s_while => |while_stmt| .{ .pattern = null, .expr = while_stmt.cond, .expr2 = while_stmt.body },
        .s_expr => |expr_stmt| .{ .pattern = null, .expr = expr_stmt.expr, .expr2 = null },
        .s_expect => |expect_stmt| .{ .pattern = null, .expr = expect_stmt.body, .expr2 = null },
        .s_dbg => |dbg_stmt| .{ .pattern = null, .expr = dbg_stmt.expr, .expr2 = null },
        .s_return => |return_stmt| .{ .pattern = null, .expr = return_stmt.expr, .expr2 = null },
        .s_crash => .{ .pattern = null, .expr = null, .expr2 = null },
        .s_import, .s_alias_decl, .s_nominal_decl, .s_break, .s_type_anno, .s_type_var_alias, .s_runtime_error => .{ .pattern = null, .expr = null, .expr2 = null },
    };
}

// Tests

test "stripModulePrefix" {
    const testing = std.testing;

    try testing.expectEqualStrings("to_str", stripModulePrefix("Basic.to_str", "Basic"));
    try testing.expectEqualStrings("to_str", stripModulePrefix("MyModule.Basic.to_str", "Basic"));
    try testing.expectEqualStrings("Basic.to_str", stripModulePrefix("Basic.to_str", "Other"));
    try testing.expectEqualStrings("", stripModulePrefix("Basic", "Basic"));
    try testing.expectEqualStrings("foo", stripModulePrefix("foo", "Basic"));
}

test "firstSegment" {
    const testing = std.testing;

    try testing.expectEqualStrings("Basic", firstSegment("Basic.to_str"));
    try testing.expectEqualStrings("foo", firstSegment("foo"));
    try testing.expectEqualStrings("A", firstSegment("A.B.C"));
    try testing.expectEqualStrings("", firstSegment(""));
}

test "lastSegment" {
    const testing = std.testing;

    try testing.expectEqualStrings("to_str", lastSegment("Basic.to_str"));
    try testing.expectEqualStrings("foo", lastSegment("foo"));
    try testing.expectEqualStrings("C", lastSegment("A.B.C"));
    try testing.expectEqualStrings("", lastSegment(""));
}
