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
    imported_module_calls: usize = 0,
    hover_calls: usize = 0,
    definition_calls: usize = 0,
    highlight_calls: usize = 0,
    document_symbol_calls: usize = 0,
    completion_calls: usize = 0,

    pub const CheckError = syntax.SyntaxChecker.CheckError;
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

    pub const DefinitionResult = struct {
        uri: []const u8,
        range: LspRange,
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

    pub fn check(
        self: *TestSyntaxDriver,
        _: []const u8,
        _: ?[]const u8,
        _: ?[]const u8,
    ) CheckError![]Diagnostics.PublishDiagnostics {
        self.check_calls += 1;
        return try self.allocator.alloc(Diagnostics.PublishDiagnostics, 0);
    }

    pub fn getImportedModuleEnvs(self: *TestSyntaxDriver, _: []const u8) std.mem.Allocator.Error!?[]*can.ModuleEnv {
        self.imported_module_calls += 1;
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
};
