//! Shared spec shape for compiler-backed LSP integration cases.

const std = @import("std");
const lsp = @import("lsp");
const helpers = @import("helpers.zig");

/// Errors that can occur while running an LSP integration spec.
pub const SpecError = std.mem.Allocator.Error ||
    std.Io.Dir.RealPathFileAllocError ||
    std.Io.Dir.WriteFileError ||
    std.json.ParseError(std.json.Scanner) ||
    helpers.HelperError ||
    lsp.syntax.SyntaxChecker.CheckError ||
    lsp.server.RunWithStdIoError ||
    error{
        MissingCompletionItems,
        MissingDefinitionResult,
        MissingDocumentHighlightResult,
        MissingDocumentSymbols,
        MissingHoverResult,
        MissingResponseId,
        MissingResult,
        MissingServerInfo,
        MissingServerName,
        MissingStringField,
        MissingTextDocument,
        SkipZigTest,
        TestExpectedEqual,
        TestUnexpectedResult,
    };

/// Function pointer type for one integration spec body.
pub const RunFn = *const fn () SpecError!void;

/// Named LSP integration spec that can be scheduled by the harness.
pub const Spec = struct {
    name: []const u8,
    run: RunFn,
};
