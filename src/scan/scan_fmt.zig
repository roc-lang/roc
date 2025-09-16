const std = @import("std");

pub const Self = @This();

const BinOp = struct {
    lhs: Expr,
    rhs: Expr,
    op: Op,
};

const Op = enum(u8) {
    equals = '=',
    colon = ':',
    dot = '.',
    bang = '!',
    question = '?',
    minus = '-',
    // For 2-byte operators, choose unused ASCII bytes (e.g. ones that wouldn't tokenize as Op)
    colon_equals = ' ',
    thin_arrow = '\n',
    thick_arrow = '\t',
    colon_question = '\r',
    _, // Format unrecognized ops as binops even though they're invalid
};

const Expr = union(enum) {
    binop: BinOp,
    unary_op: UnaryOp,
};

currently_in: CurrentlyIn,

const CurrentlyIn = enum {
    stmt,
    expr,
    anno,
};

const Delimited = union(enum) {
    brace: Brace,
};

const Brace = enum {
    square,
    round,
    curly,
    pipe,
};

pub fn chompToken(self: *Self, token: Token) Allocator.Error!void {
    // TODO I want to dramatically simplify multiline vs not, just simply make it so that
    // in the tokenizer we preserve newlines on demand and can then just answer the question
    // "does anything from this start index to this end index contain a newline?"
    // prob need to have it preserve that forever btw. also maybe we can give it a comptime
    // flag for like "don't report comments bc I don't care" so the subsequent step never
    // has to deal with them. it can just be like hey unreachable bc you never give me those.

    switch (token) {
        .colon,
        .equals,
        .colon_equals,
        .thick_arrow,
        .thin_arrow,
        .close_round,
        .close_square,
        .close_curly,
        .pipe,
        => {
            // TODO you're allowed to have blank lines in between stmts and comments and that's it
        },
        .comment => {
            // TODO can affect multiline vs not, figure that out

            // TODO you're allowed to have blank lines in between stmts and comments and that's it
        },
        .minus => {
            // TODO special handling bc of numbers
        },
        .ident => {
            // TODO just copy verbatim, never newline
        },
        .single_line_str => {
            // TODO just copy verbatim, never newline. can translate tabs into \t.
        },
        .multi_line_str => {
            // TODO don't trim trailing whitespace, let the editor do that
        },
        .digits => {
            // TODO just copy em verbatim, never newline
        },
    }
}
