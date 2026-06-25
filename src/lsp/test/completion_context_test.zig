//! Unit tests for completion context detection.
//!
//! These tests exercise token and cursor classification only. They must stay
//! out of the integration root because they do not require SyntaxChecker,
//! BuildEnv, compiled builtins, or checked Roc modules.

const std = @import("std");
const completion_context = @import("lsp").completion;

test "completion context detects after_record_dot for lowercase identifier" {
    const source = "main = my_var.";
    const context = completion_context.detectCompletionContext(source, 0, 14);
    switch (context) {
        .after_value_dot => |access| {
            try std.testing.expectEqualStrings("my_var", access.access_chain);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "completion context detects after_module_dot for uppercase identifier" {
    const source = "main = Str.";
    const context = completion_context.detectCompletionContext(source, 0, 11);
    switch (context) {
        .after_module_dot => |module_name| {
            try std.testing.expectEqualStrings("Str", module_name);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "completion context detects after_receiver_dot for chained call" {
    const source = "main = val.func().";
    const context = completion_context.detectCompletionContext(source, 0, 18);
    switch (context) {
        .after_receiver_dot => |info| {
            try std.testing.expectEqual(@as(u32, 17), info.dot_offset);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "completion context detects expression context" {
    const source = "main = ";
    const context = completion_context.detectCompletionContext(source, 0, 7);
    switch (context) {
        .expression => {},
        else => return error.TestUnexpectedResult,
    }
}

test "completion context detects after_colon for type annotation" {
    const source = "foo : ";
    const context = completion_context.detectCompletionContext(source, 0, 6);
    switch (context) {
        .after_colon => {},
        else => return error.TestUnexpectedResult,
    }
}
