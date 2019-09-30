pub mod ast;
pub mod blankspace;
pub mod ident;
pub mod keyword;
pub mod module;
pub mod number_literal;
pub mod parser;
pub mod problems;
pub mod string_literal;

/// All module definitions begin with one of these:
///
/// app
/// api
/// api bridge
///
/// We parse these to guard against mistakes; in general, the build tool
/// is responsible for determining the root module (either an `app` or `api bridge`
/// module), and then all `api` modules should only ever be imported from
/// another module.
///
/// parsing the file
use bumpalo::collections::String;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use operator::Operator;
use parse::ast::{Attempting, CommentOrNewline, Def, Expr, Pattern, Spaceable};
use parse::blankspace::{
    space0, space0_after, space0_around, space0_before, space1, space1_before,
};
use parse::ident::{ident, Ident};
use parse::number_literal::number_literal;
use parse::parser::{
    and, attempt, between, char, either, loc, map, map_with_arena, one_of4, one_of5, one_of9,
    one_or_more, optional, sep_by0, skip_first, skip_second, string, then, unexpected,
    unexpected_eof, zero_or_more, Either, Fail, FailReason, ParseResult, Parser, State,
};
use parse::string_literal::string_literal;
use region::Located;

// pub fn api<'a>() -> impl Parser<'a, Module<'a>> {
//     and(
//         skip_first(string("api"), space1_around(ident())),
//         skip_first(string("exposes"), space1_around(ident())),
//     )
// }

// pub fn app<'a>() -> impl Parser<'a, Module<'a>> {
//     skip_first(string("app using Echo"))
// }

// pub fn api_bridge<'a>() -> impl Parser<'a, Module<'a>> {
//     and(
//         skip_first(string("api bridge"), space1_around(ident())),
//         skip_first(string("exposes"), space1_around(ident())),
//     )
// }

pub fn expr<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    // Recursive parsers must not directly invoke functions which return (impl Parser),
    // as this causes rustc to stack overflow. Thus, parse_expr must be a
    // separate function which recurses by calling itself directly.
    move |arena, state| parse_expr(min_indent, arena, state)
}

fn loc_parse_expr_body_without_operators<'a>(
    min_indent: u16,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Located<Expr<'a>>> {
    one_of9(
        loc_parenthetical_expr(min_indent),
        loc(string_literal()),
        loc(number_literal()),
        loc(record_literal(min_indent)),
        loc(closure(min_indent)),
        loc(list_literal(min_indent)),
        loc(when(min_indent)),
        loc(conditional(min_indent)),
        loc(ident_etc(min_indent)),
    )
    .parse(arena, state)
}

fn parse_expr<'a>(min_indent: u16, arena: &'a Bump, state: State<'a>) -> ParseResult<'a, Expr<'a>> {
    let expr_parser = map_with_arena(
        and(
            // First parse the body without operators, then try to parse possible operators after.
            move |arena, state| loc_parse_expr_body_without_operators(min_indent, arena, state),
            // Parse the operator, with optional spaces before it.
            //
            // Since spaces can only wrap an Expr, not an Operator, we have to first
            // parse the spaces and then attach them retroactively to the expression
            // preceding the operator (the one we parsed before considering operators).
            optional(and(
                and(space0(min_indent), loc(operator())),
                // The spaces *after* the operator can be attached directly to
                // the expression following the operator.
                space0_before(
                    loc(move |arena, state| parse_expr(min_indent, arena, state)),
                    min_indent,
                ),
            )),
        ),
        |arena, (loc_expr1, opt_operator)| match opt_operator {
            Some(((spaces_before_op, loc_op), loc_expr2)) => {
                let loc_expr1 = if spaces_before_op.is_empty() {
                    loc_expr1
                } else {
                    // Attach the spaces retroactively to the expression preceding the operator.
                    arena
                        .alloc(loc_expr1.value)
                        .with_spaces_after(spaces_before_op, loc_expr1.region)
                };
                let tuple = arena.alloc((loc_expr1, loc_op, loc_expr2));

                Expr::Operator(tuple)
            }
            None => loc_expr1.value,
        },
    );

    attempt(Attempting::Expression, expr_parser).parse(arena, state)
}

pub fn loc_parenthetical_expr<'a>(min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>> {
    map_with_arena(
        loc(and(
            between(
                char('('),
                space0_around(
                    loc(move |arena, state| parse_expr(min_indent, arena, state)),
                    min_indent,
                ),
                char(')'),
            ),
            optional(either(
                // There may optionally be function args after the ')'
                // e.g. ((foo bar) baz)
                loc_function_args(min_indent),
                // If there aren't any args, there may be a '=' or ':' after it.
                //
                // (It's a syntax error to write e.g. `foo bar =` - so if there
                // were any args, there is definitely no need to parse '=' or ':'!)
                //
                // Also, there may be a '.' for field access (e.g. `(foo).bar`),
                // but we only want to look for that if there weren't any args,
                // as if there were any args they'd have consumed it anyway
                // e.g. in `((foo bar) baz.blah)` the `.blah` will be consumed by the `baz` parser
                either(
                    one_or_more(skip_first(char('.'), unqualified_ident())),
                    and(space0(min_indent), either(char('='), char(':'))),
                ),
            )),
        )),
        |arena, loc_expr_with_extras| {
            // We parse the parenthetical expression *and* the arguments after it
            // in one region, so that (for example) the region for Apply includes its args.
            let (loc_expr, opt_extras) = loc_expr_with_extras.value;
            match opt_extras {
                Some(Either::First(loc_args)) => Located {
                    region: loc_expr_with_extras.region,
                    value: Expr::Apply(arena.alloc((loc_expr, loc_args))),
                },
                // '=' after optional spaces
                Some(Either::Second(Either::Second((_space_list, Either::First(()))))) => {
                    panic!("TODO handle def, making sure not to drop comments!");
                }
                // ':' after optional spaces
                Some(Either::Second(Either::Second((_space_list, Either::Second(()))))) => {
                    panic!("TODO handle annotation, making sure not to drop comments!");
                }
                // '.' and a record field immediately after ')', no optional spaces
                Some(Either::Second(Either::First(fields))) => Located {
                    region: loc_expr_with_extras.region,
                    value: Expr::Field(arena.alloc(loc_expr), fields),
                },
                None => loc_expr,
            }
        },
    )
}

/// A definition, consisting of one of these:
///
/// * A pattern followed by '=' and then an expression
/// * A type annotation
/// * Both
pub fn def<'a>(min_indent: u16) -> impl Parser<'a, Def<'a>> {
    // Indented more beyond the original indent.
    let indented_more = min_indent + 1;

    // TODO support type annotations
    map_with_arena(
        and(
            skip_second(
                space0_after(loc_closure_param(min_indent), min_indent),
                char('='),
            ),
            space0_before(
                loc(move |arena, state| parse_expr(indented_more, arena, state)),
                min_indent,
            ),
        ),
        |arena, (loc_pattern, loc_expr)| {
            // BodyOnly(Loc<Pattern<'a>>, &'a Loc<Expr<'a>>),
            Def::BodyOnly(loc_pattern, arena.alloc(loc_expr))
        },
    )
}

/// Same as def() but with space_before1 before each def, because each nested def must
/// have space separating it from the previous def.
pub fn nested_def<'a>(min_indent: u16) -> impl Parser<'a, (&'a [CommentOrNewline<'a>], Def<'a>)> {
    then(
        and(space1(min_indent), def(min_indent)),
        move |arena: &'a Bump, state, tuple| {
            // TODO verify spacing (I think?)
            Ok((tuple, state))
        },
    )
}

fn parse_def_expr<'a>(
    min_indent: u16,
    equals_sign_indent: u16,
    arena: &'a Bump,
    state: State<'a>,
    loc_first_pattern: Located<Pattern<'a>>,
) -> ParseResult<'a, Expr<'a>> {
    let original_indent = state.indent_col;

    if original_indent < min_indent {
        Err((
            Fail {
                attempting: state.attempting,
                reason: FailReason::DefOutdentedTooFar(
                    original_indent,
                    min_indent,
                    loc_first_pattern.region,
                ),
            },
            state,
        ))
    // `<` because '=' should be same indent or greater
    } else if equals_sign_indent < original_indent {
        panic!("TODO the = in this declaration seems outdented");
    } else {
        // Indented more beyond the original indent.
        let indented_more = original_indent + 1;

        then(
            and(
                // Parse the body of the first def. It doesn't need any spaces
                // around it parsed, because both the subsquent defs and the
                // final body will have space1_before on them.
                //
                // It should be indented more than the original, and it will
                // end when outdented again.
                loc(move |arena, state| parse_expr(indented_more, arena, state)),
                and(
                    // Optionally parse additional defs.
                    zero_or_more(nested_def(original_indent)),
                    // Parse the final expression that will be returned.
                    // It should be indented the same amount as the original.
                    space1_before(
                        loc(move |arena, state| parse_expr(original_indent, arena, state)),
                        indented_more,
                    ),
                ),
            ),
            move |arena, state, (loc_first_body, (mut defs, loc_ret))| {
                if state.indent_col != original_indent {
                    panic!("TODO return expr was indented differently from original def",);
                } else {
                    let first_def: Def<'a> =
                        // TODO if Parser were FnOnce instead of Fn, this might not need .clone()?
                        Def::BodyOnly(loc_first_pattern.clone(), arena.alloc(loc_first_body));

                    // Add the first def to the end of the defs. (It's fine that we
                    // reorder the first one to the end, because canonicalize will
                    // re-sort all of these based on dependencies anyway. Only
                    // their regions will ever be visible to the user.)
                    defs.push((&[], first_def));

                    Ok((Expr::Defs(arena.alloc((defs, loc_ret))), state))
                }
            },
        )
        .parse(arena, state)
    }
}

fn parse_nested_def_body<'a, S>(
    min_indent: u16,
    equals_sign_indent: u16,
    arena: &'a Bump,
    state: State<'a>,
    loc_pattern: Located<Pattern<'a>>,
) -> ParseResult<'a, Located<Expr<'a>>> {
    let original_indent = state.indent_col;

    if original_indent < min_indent {
        panic!("TODO this declaration is outdented too far");
    // `<` because '=' should be same indent or greater
    } else if equals_sign_indent < original_indent {
        panic!("TODO the = in this declaration seems outdented");
    } else {
        then(
            loc(move |arena, state| {
                parse_expr(original_indent + 1, arena, state)
            }),
            move |arena, state, loc_expr| {
                if state.indent_col != original_indent {
                    panic!(
                                "TODO the return expression was indented differently from the original assignment",
                            );
                } else {
                    Ok((loc_expr, state))
                }
            },
        ).parse(arena, state)
    }
}

fn loc_function_arg<'a>(min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>> {
    // Don't parse operators, because they have a higher precedence than function application.
    // If we encounter one, we're done parsing function args!
    move |arena, state| loc_parse_expr_body_without_operators(min_indent, arena, state)
}

fn closure<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    map_with_arena(
        skip_first(
            // All closures start with a '\' - e.g. (\x -> x + 1)
            char('\\'),
            and(
                // Parse the params
                one_or_more(space0_around(loc_closure_param(min_indent), min_indent)),
                skip_first(
                    // Parse the -> which separates params from body
                    string("->"),
                    // Parse the body
                    space0_before(
                        loc(move |arena, state| parse_expr(min_indent, arena, state)),
                        min_indent,
                    ),
                ),
            ),
        ),
        |arena, (params, loc_body)| Expr::Closure(arena.alloc((params, loc_body))),
    )
}

fn loc_closure_param<'a>(min_indent: u16) -> impl Parser<'a, Located<Pattern<'a>>> {
    move |arena, state| parse_closure_param(arena, state, min_indent)
}

fn parse_closure_param<'a>(
    arena: &'a Bump,
    state: State<'a>,
    min_indent: u16,
) -> ParseResult<'a, Located<Pattern<'a>>> {
    one_of5(
        // An ident is the most common param, e.g. \foo -> ...
        loc(ident_pattern()),
        // Underscore is also common, e.g. \_ -> ...
        loc(underscore_pattern()),
        // You can destructure records in params, e.g. \{ x, y } -> ...
        loc(record_destructure(min_indent)),
        // If you wrap it in parens, you can match any arbitrary pattern at all.
        // e.g. \User.UserId userId -> ...
        between(
            char('('),
            space0_around(loc(pattern(min_indent)), min_indent),
            char(')'),
        ),
        // The least common, but still allowed, e.g. \Foo -> ...
        loc(map(unqualified_variant(), |name| {
            Pattern::Variant(&[], name)
        })),
    )
    .parse(arena, state)
}

fn pattern<'a>(min_indent: u16) -> impl Parser<'a, Pattern<'a>> {
    one_of4(
        underscore_pattern(),
        variant_pattern(),
        ident_pattern(),
        record_destructure(min_indent),
    )
}

fn underscore_pattern<'a>() -> impl Parser<'a, Pattern<'a>> {
    map(char('_'), |_| Pattern::Underscore)
}

fn record_destructure<'a>(min_indent: u16) -> impl Parser<'a, Pattern<'a>> {
    map(
        collection(
            char('{'),
            loc(ident_pattern()),
            char(','),
            char('}'),
            min_indent,
        ),
        |loc_fields| Pattern::RecordDestructure(loc_fields),
    )
}

fn variant_pattern<'a>() -> impl Parser<'a, Pattern<'a>> {
    move |_, _| panic!("TODO support variant patterns, including qualified and/or applied")
}

fn ident_pattern<'a>() -> impl Parser<'a, Pattern<'a>> {
    map(unqualified_ident(), Pattern::Identifier)
}

pub fn when<'a>(_min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    map(string(keyword::WHEN), |_| {
        panic!("TODO implement WHEN");
    })
}

pub fn conditional<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    // TODO figure out how to remove this code duplication in a way rustc
    // accepts. I tried making a helper functions and couldn't resolve the
    // lifetime errors, so I manually inlined them and moved on.
    one_of4(
        map_with_arena(
            skip_first(
                string(keyword::IF),
                space1_before(loc(expr(min_indent)), min_indent),
            ),
            |arena, loc_expr| Expr::If(arena.alloc(loc_expr)),
        ),
        map_with_arena(
            skip_first(
                string(keyword::THEN),
                space1_before(loc(expr(min_indent)), min_indent),
            ),
            |arena, loc_expr| Expr::Then(arena.alloc(loc_expr)),
        ),
        map_with_arena(
            skip_first(
                string(keyword::ELSE),
                space1_before(loc(expr(min_indent)), min_indent),
            ),
            |arena, loc_expr| Expr::Else(arena.alloc(loc_expr)),
        ),
        map_with_arena(
            skip_first(
                string(keyword::CASE),
                space1_before(loc(expr(min_indent)), min_indent),
            ),
            |arena, loc_expr| Expr::Case(arena.alloc(loc_expr)),
        ),
    )
}
pub fn loc_function_args<'a>(min_indent: u16) -> impl Parser<'a, Vec<'a, Located<Expr<'a>>>> {
    one_or_more(space1_before(loc_function_arg(min_indent), min_indent))
}

/// When we parse an ident like `foo ` it could be any of these:
///
/// 1. A standalone variable with trailing whitespace (e.g. because an operator is next)
/// 2. The beginning of a function call (e.g. `foo bar baz`)
/// 3. The beginning of a defniition (e.g. `foo =`)
/// 4. The beginning of a type annotation (e.g. `foo :`)
/// 5. A reserved keyword (e.g. `if ` or `case `), meaning we should do something else.
pub fn ident_etc<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    then(
        and(
            loc(ident()),
            optional(either(
                // There may optionally be function args after this ident
                loc_function_args(min_indent),
                // If there aren't any args, there may be a '=' or ':' after it.
                // (It's a syntax error to write e.g. `foo bar =` - so if there
                // were any args, there is definitely no need to parse '=' or ':'!)
                and(space0(min_indent), either(equals_with_indent(), char(':'))),
            )),
        ),
        move |arena, state, (loc_ident, opt_extras)| {
            // This appears to be a var, keyword, or function application.
            match opt_extras {
                Some(Either::First(loc_args)) => {
                    let loc_expr = Located {
                        region: loc_ident.region,
                        value: ident_to_expr(loc_ident.value),
                    };

                    Ok((Expr::Apply(arena.alloc((loc_expr, loc_args))), state))
                }
                Some(Either::Second((_space_list, Either::First(indent)))) => {
                    let value: Pattern<'a> = Pattern::from_ident(arena, loc_ident.value);
                    let region = loc_ident.region;
                    let loc_pattern = Located { region, value };
                    let (spaces, state) = space0(min_indent).parse(arena, state)?;
                    let (parsed_expr, state) =
                        parse_def_expr(min_indent, indent, arena, state, loc_pattern)?;

                    let answer = if spaces.is_empty() {
                        parsed_expr
                    } else {
                        Expr::SpaceBefore(arena.alloc(parsed_expr), spaces)
                    };

                    Ok((answer, state))
                }
                Some(Either::Second((_space_list, Either::Second(())))) => {
                    panic!("TODO handle annotation, making sure not to drop comments!");
                }
                None => {
                    let ident = loc_ident.value.clone();
                    let len = ident.len();

                    Ok((ident_to_expr(ident), state))
                }
            }
        },
    )
}

pub fn equals_with_indent<'a>() -> impl Parser<'a, u16> {
    move |_arena, state: State<'a>| {
        let mut iter = state.input.chars();

        match iter.next() {
            Some(ch) if ch == '=' => {
                match iter.peekable().peek() {
                    // The '=' must not be followed by another `=` or `>`
                    Some(next_ch) if next_ch != &'=' && next_ch != &'>' => {
                        Ok((state.indent_col, state.advance_without_indenting(1)?))
                    }
                    Some(next_ch) => Err(unexpected(*next_ch, 0, state, Attempting::Def)),
                    None => Err(unexpected_eof(
                        1,
                        Attempting::Def,
                        state.advance_without_indenting(1)?,
                    )),
                }
            }
            Some(ch) => Err(unexpected(ch, 0, state, Attempting::Def)),
            None => Err(unexpected_eof(0, Attempting::Def, state)),
        }
    }
}

fn ident_to_expr<'a>(src: Ident<'a>) -> Expr<'a> {
    match src {
        Ident::Var(info) => Expr::Var(info.module_parts, info.value),
        Ident::Variant(info) => Expr::Variant(info.module_parts, info.value),
        Ident::Field(info) => Expr::QualifiedField(info.module_parts, info.value),
        Ident::AccessorFunction(string) => Expr::AccessorFunction(string),
        Ident::Malformed(string) => Expr::MalformedIdent(string),
    }
}

pub fn operator<'a>() -> impl Parser<'a, Operator> {
    one_of4(
        map(char('+'), |_| Operator::Plus),
        map(char('-'), |_| Operator::Minus),
        map(char('*'), |_| Operator::Star),
        map(char('/'), |_| Operator::Slash),
    )
}

pub fn list_literal<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    let elems = collection(
        char('['),
        loc(expr(min_indent)),
        char(','),
        char(']'),
        min_indent,
    );

    attempt(Attempting::List, map(elems, Expr::List))
}

pub fn record_literal<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    let field = map_with_arena(
        and(
            // You must have a field name, e.g. "email"
            loc(unqualified_ident()),
            // Having a value is optional; both `{ email }` and `{ email: blah }` work
            optional(skip_first(
                char(':'),
                space0_before(
                    loc(move |arena, state| parse_expr(min_indent, arena, state)),
                    min_indent,
                ),
            )),
        ),
        |arena, (label, opt_loc_expr)| match opt_loc_expr {
            Some(loc_expr) => Expr::AssignField(label, arena.alloc(loc_expr)),
            // If no value was provided, record it as a Var.
            // Canonicalize will know what to do with a Var later.
            None => Expr::Var(&[], label.value),
        },
    );
    let fields = collection(char('{'), loc(field), char(','), char('}'), min_indent);

    attempt(Attempting::List, map(fields, Expr::Record))
}

/// This is mainly for matching variants in closure params, e.g. \Foo -> ...
fn unqualified_variant<'a>() -> impl Parser<'a, &'a str> {
    variant_or_ident(|first_char| first_char.is_uppercase())
}

/// This could be:
///
/// * A record field, e.g. "email" in `.email` or in `email:`
/// * A named pattern match, e.g. "foo" in `foo =` or `foo ->` or `\foo ->`
fn unqualified_ident<'a>() -> impl Parser<'a, &'a str> {
    variant_or_ident(|first_char| first_char.is_lowercase())
}

fn variant_or_ident<'a, F>(pred: F) -> impl Parser<'a, &'a str>
where
    F: Fn(char) -> bool,
{
    move |arena, state: State<'a>| {
        let mut chars = state.input.chars();

        // pred will determine if this is a variant or ident (based on capitalization)
        let first_letter = match chars.next() {
            Some(first_char) => {
                if pred(first_char) {
                    first_char
                } else {
                    return Err(unexpected(
                        first_char,
                        0,
                        state,
                        Attempting::RecordFieldLabel,
                    ));
                }
            }
            None => {
                return Err(unexpected_eof(0, Attempting::RecordFieldLabel, state));
            }
        };

        let mut buf = String::with_capacity_in(1, arena);

        buf.push(first_letter);

        while let Some(ch) = chars.next() {
            // After the first character, only these are allowed:
            //
            // * Unicode alphabetic chars - you might include `鹏` if that's clear to your readers
            // * ASCII digits - e.g. `1` but not `¾`, both of which pass .is_numeric()
            // * A ':' indicating the end of the field
            if ch.is_alphabetic() || ch.is_ascii_digit() {
                buf.push(ch);
            } else {
                // This is the end of the field. We're done!
                break;
            }
        }

        let chars_parsed = buf.len();

        Ok((
            buf.into_bump_str(),
            state.advance_without_indenting(chars_parsed)?,
        ))
    }
}

/// Parse zero or more elements between two braces (e.g. square braces).
/// Elements can be optionally surrounded by spaces, and are separated by a
/// delimiter (e.g comma-separated). Braces and delimiters get discarded.
pub fn collection<'a, Elem, OpeningBrace, ClosingBrace, Delimiter, S>(
    opening_brace: OpeningBrace,
    elem: Elem,
    delimiter: Delimiter,
    closing_brace: ClosingBrace,
    min_indent: u16,
) -> impl Parser<'a, Vec<'a, Located<S>>>
where
    OpeningBrace: Parser<'a, ()>,
    Elem: Parser<'a, Located<S>>,
    Elem: 'a,
    Delimiter: Parser<'a, ()>,
    S: Spaceable<'a>,
    S: 'a,
    ClosingBrace: Parser<'a, ()>,
{
    skip_first(
        opening_brace,
        skip_second(
            sep_by0(delimiter, space0_around(elem, min_indent)),
            closing_brace,
        ),
    )
}
