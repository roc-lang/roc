//! Tests for numeric literal size and type unification logic.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const check = @import("check");

const unify = @import("../unify.zig");

const TypesStore = types.TypesStore;
const Content = types.Content;
const Var = types.Var;
const Num = types.Num;
const problem = check.problem;
const snapshot = check.snapshot;
const occurs = check.occurs;

test "integer literal 255 fits in U8" {
    return error.SkipZigTest;
}

test "integer literal 256 does not fit in U8" {
    return error.SkipZigTest;
}

test "integer literal -128 fits in I8" {
    return error.SkipZigTest;
}

test "integer literal -129 does not fit in I8" {
    return error.SkipZigTest;
}

test "negative literal cannot unify with unsigned type" {
    return error.SkipZigTest;
}

test "float literal that fits in F32" {
    return error.SkipZigTest;
}

test "float literal that doesn't fit in F32" {
    return error.SkipZigTest;
}

test "float literal NaN doesn't fit in Dec" {
    return error.SkipZigTest;
}

test "two integer literals with different requirements unify to most restrictive" {
    return error.SkipZigTest;
}

test "positive and negative literals unify with sign requirement" {
    return error.SkipZigTest;
}
