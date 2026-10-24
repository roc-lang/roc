//! Test-only LSP syntax driver.
//!
//! This driver is for unit tests that need server routing, document storage, or
//! handler response shapes. It returns explicit empty results and never creates
//! SyntaxChecker or BuildEnv.

const std = @import("std");
const can = @import("can");

const Diagnostics = @import("lsp").diagnostics;
const syntax = @import("lsp").syntax;
const completion_handler = @import("lsp").handlers.completion;
const document_symbol_handler = @import("lsp").handlers.document_symbol;

/// Minimal syntax-driver implementation used by LSP server tests.
pub const TestSyntaxDriver = struct {
    allocator: std.mem.Allocator,
    check_calls: usize = 0,
    /// When set, the first check waits until its cancel request is set and
    /// then fails with `error.Canceled`.
    cancel_gate: ?*CancelGate = null,
    canceled_checks: usize = 0,
    close_calls: usize = 0,
    imported_module_calls: usize = 0,
    hover_calls: usize = 0,
    definition_calls: usize = 0,
    highlight_calls: usize = 0,
    document_symbol_calls: usize = 0,
    completion_calls: usize = 0,
    rename_calls: usize = 0,
    prepare_rename_calls: usize = 0,
    references_calls: usize = 0,
    inlay_hint_calls: usize = 0,
    code_action_calls: usize = 0,
    forget_calls: usize = 0,

    pub const CheckError = syntax.SyntaxChecker.CheckError;

    /// Lets a test learn when the gated check started.
    pub const CancelGate = struct {
        io: std.Io,
        started: std.Io.Event = .unset,
    };
    pub const QueryError = syntax.SyntaxChecker.QueryError;

    pub const LspRange = struct {
        start_line: u32,
        start_col: u32,
        end_line: u32,
        end_col: u32,
    };

    pub const HoverResult = struct {
        type_str: []u8,
        range: ?LspRange,
    };

    pub const CheckedModule = struct {
        module_env: *can.ModuleEnv,

        pub fn deinit(_: CheckedModule) void {}
    };

    pub const DefinitionResult = struct {
        uri: []const u8,
        range: LspRange,
        origin_selection_range: ?LspRange = null,

        pub fn deinit(self: DefinitionResult, allocator: std.mem.Allocator) void {
            allocator.free(self.uri);
        }
    };

    pub const HighlightResult = struct {
        regions: []LspRange,

        pub fn deinit(self: HighlightResult, allocator: std.mem.Allocator) void {
            allocator.free(self.regions);
        }
    };

    pub fn init(
        allocator: std.mem.Allocator,
        _: std.Io,
        _: anytype,
        _: ?std.Io.File,
    ) TestSyntaxDriver {
        return .{ .allocator = allocator };
    }

    pub fn deinit(_: *TestSyntaxDriver) void {}

    pub fn forget(self: *TestSyntaxDriver, _: []const u8) std.mem.Allocator.Error!void {
        self.forget_calls += 1;
    }

    pub fn check(
        self: *TestSyntaxDriver,
        _: []const u8,
        _: ?[]const u8,
        _: ?[]const u8,
        cancel_request: ?*const std.atomic.Value(bool),
    ) CheckError![]Diagnostics.PublishDiagnostics {
        self.check_calls += 1;
        if (self.cancel_gate) |gate| if (self.check_calls == 1) {
            gate.started.set(gate.io);
            if (cancel_request) |request| {
                // Give up after ten seconds; the test then sees no canceled check.
                var waited_ms: usize = 0;
                while (!request.load(.acquire) and waited_ms < 10_000) : (waited_ms += 1) {
                    std.Io.sleep(gate.io, .fromMilliseconds(1), .awake) catch {};
                }
                if (request.load(.acquire)) {
                    self.canceled_checks += 1;
                    return error.Canceled;
                }
            }
        };
        return try self.allocator.alloc(Diagnostics.PublishDiagnostics, 0);
    }

    pub fn closeDocument(self: *TestSyntaxDriver, _: []const u8) void {
        self.close_calls += 1;
    }

    pub fn getCheckedModuleForDocument(_: *TestSyntaxDriver, _: []const u8, _: []const u8) std.mem.Allocator.Error!?CheckedModule {
        return null;
    }

    pub fn getTypeAtPosition(
        self: *TestSyntaxDriver,
        _: []const u8,
        _: ?[]const u8,
        _: u32,
        _: u32,
    ) QueryError!?HoverResult {
        self.hover_calls += 1;
        return null;
    }

    pub fn getDefinitionAtPosition(
        self: *TestSyntaxDriver,
        _: []const u8,
        _: ?[]const u8,
        _: u32,
        _: u32,
    ) QueryError!?DefinitionResult {
        self.definition_calls += 1;
        return null;
    }

    pub fn getHighlightsAtPosition(
        self: *TestSyntaxDriver,
        _: []const u8,
        _: ?[]const u8,
        _: u32,
        _: u32,
    ) QueryError!?HighlightResult {
        self.highlight_calls += 1;
        return null;
    }

    pub fn getDocumentSymbols(
        self: *TestSyntaxDriver,
        allocator: std.mem.Allocator,
        _: []const u8,
        _: []const u8,
    ) QueryError![]document_symbol_handler.SymbolInformation {
        self.document_symbol_calls += 1;
        return try allocator.alloc(document_symbol_handler.SymbolInformation, 0);
    }

    pub fn getCompletionsAtPosition(
        self: *TestSyntaxDriver,
        _: []const u8,
        _: ?[]const u8,
        _: u32,
        _: u32,
    ) QueryError!?completion_handler.CompletionResult {
        self.completion_calls += 1;
        return null;
    }

    pub fn getRenameEditsAtPosition(
        self: *TestSyntaxDriver,
        _: []const u8,
        _: ?[]const u8,
        _: u32,
        _: u32,
        _: []const u8,
    ) QueryError!?syntax.SyntaxChecker.RenameOutcome {
        self.rename_calls += 1;
        return null;
    }

    pub fn prepareRenameAtPosition(
        self: *TestSyntaxDriver,
        _: []const u8,
        _: ?[]const u8,
        _: u32,
        _: u32,
    ) QueryError!?syntax.SyntaxChecker.PrepareRenameResult {
        self.prepare_rename_calls += 1;
        return null;
    }

    pub fn getReferencesAtPosition(
        self: *TestSyntaxDriver,
        _: []const u8,
        _: ?[]const u8,
        _: u32,
        _: u32,
        _: bool,
    ) QueryError!?syntax.SyntaxChecker.ReferencesResult {
        self.references_calls += 1;
        return null;
    }

    pub fn getInlayHints(
        self: *TestSyntaxDriver,
        _: []const u8,
        _: ?[]const u8,
        _: u32,
        _: u32,
    ) QueryError!?syntax.SyntaxChecker.InlayHintsResult {
        self.inlay_hint_calls += 1;
        return null;
    }

    pub fn getCodeActions(
        self: *TestSyntaxDriver,
        _: []const u8,
        _: ?[]const u8,
        _: u32,
        _: u32,
        _: u32,
        _: u32,
    ) QueryError!?syntax.SyntaxChecker.CodeActionsResult {
        self.code_action_calls += 1;
        return null;
    }
};
