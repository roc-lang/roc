//! Unit tests for completion context detection.
//!
//! These tests exercise token and cursor classification only. They must stay
//! out of the integration root because they do not require SyntaxChecker,
//! BuildEnv, compiled builtins, or checked Roc modules.

const std = @import("std");
const completion_context = @import("lsp").completion;

test "completion context detects after_value_dot for lowercase identifier" {
    const source = "main = my_var.";
    const context = completion_context.detectCompletionContext(source, 0, 14);
    if (std.meta.activeTag(context) != .after_value_dot) return error.TestUnexpectedResult;
    try std.testing.expectEqualStrings("my_var", context.after_value_dot.access_chain);
}

test "completion context detects after_module_dot for uppercase identifier" {
    const source = "main = Str.";
    const context = completion_context.detectCompletionContext(source, 0, 11);
    if (std.meta.activeTag(context) != .after_module_dot) return error.TestUnexpectedResult;
    try std.testing.expectEqualStrings("Str", context.after_module_dot);
}

test "completion context detects after_receiver_dot for chained call" {
    const source = "main = val.func().";
    const context = completion_context.detectCompletionContext(source, 0, 18);
    if (std.meta.activeTag(context) != .after_receiver_dot) return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u32, 17), context.after_receiver_dot.dot_offset);
}

test "completion context detects expression context" {
    const source = "main = ";
    const context = completion_context.detectCompletionContext(source, 0, 7);
    if (std.meta.activeTag(context) != .expression) return error.TestUnexpectedResult;
}

test "completion context detects after_colon for type annotation" {
    const source = "foo : ";
    const context = completion_context.detectCompletionContext(source, 0, 6);
    if (std.meta.activeTag(context) != .after_colon) return error.TestUnexpectedResult;
}
