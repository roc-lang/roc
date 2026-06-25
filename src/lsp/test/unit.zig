//! LSP unit test root.
//!
//! This root is intended for fast feedback. It covers protocol, transport,
//! document storage, line mapping, scope maps, doc comment extraction,
//! dependency graph bookkeeping, completion helper logic, token-only syntax
//! coloring, and server or handler JSON routing through a test syntax driver.
//!
//! Tests in this root must not create SyntaxChecker or BuildEnv. Compiler-backed
//! tests live in integration.zig so pure LSP behavior can be run without paying
//! for Roc source checking, compiled builtins, platform resolution, or app
//! checking setup.

const std = @import("std");

test "lsp unit tests" {
    std.testing.refAllDecls(@import("protocol_test.zig"));
    std.testing.refAllDecls(@import("server_test.zig"));
    std.testing.refAllDecls(@import("transport_test.zig"));
    std.testing.refAllDecls(@import("document_store_test.zig"));
    std.testing.refAllDecls(@import("line_info_test.zig"));
    std.testing.refAllDecls(@import("semantic_tokens_test.zig"));
    std.testing.refAllDecls(@import("handler_unit_tests.zig"));
    std.testing.refAllDecls(@import("completion_context_test.zig"));
    std.testing.refAllDecls(@import("scope_map_test.zig"));
    std.testing.refAllDecls(@import("lsp").dependency_graph);
    std.testing.refAllDecls(@import("lsp").type_utils);
    std.testing.refAllDecls(@import("lsp").cir_visitor);
    std.testing.refAllDecls(@import("lsp").cir_queries);
    std.testing.refAllDecls(@import("lsp").module_lookup);
    std.testing.refAllDecls(@import("lsp").doc_comments);
    std.testing.refAllDecls(@import("lsp").completion);
    std.testing.refAllDecls(@import("lsp").completion.builtins);
    std.testing.refAllDecls(@import("lsp").completion.builder);
}
