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
use operator::{CalledVia, Operator};
use parse;
use parse::ast::{Attempting, Def, Expr, Pattern, Spaceable};
use parse::blankspace::{
    space0, space0_after, space0_around, space0_before, space1, space1_around, space1_before,
};
use parse::ident::{ident, Ident, MaybeQualified};
use parse::number_literal::number_literal;
use parse::parser::{
    and, attempt, between, char, either, loc, map, map_with_arena, not, not_followed_by, one_of16,
    one_of2, one_of5, one_of9, one_or_more, optional, sep_by0, skip_first, skip_second, string,
    then, unexpected, unexpected_eof, zero_or_more, Either, Fail, FailReason, ParseResult, Parser,
    State,
};
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
        loc(closure(min_indent)),
        loc(record_literal(min_indent)),
        loc(list_literal(min_indent)),
        loc(case_expr(min_indent)),
        loc(if_expr(min_indent)),
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

    expr_parser.parse(arena, state)
}

pub fn loc_parenthetical_expr<'a>(min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>> {
    then(
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
                    and(space0(min_indent), equals_with_indent()),
                ),
            )),
        )),
        move |arena, state, loc_expr_with_extras| {
            // We parse the parenthetical expression *and* the arguments after it
            // in one region, so that (for example) the region for Apply includes its args.
            let (loc_expr, opt_extras) = loc_expr_with_extras.value;

            match opt_extras {
                Some(Either::First(loc_args)) => Ok((
                    Located {
                        region: loc_expr_with_extras.region,
                        value: Expr::Apply(arena.alloc((loc_expr, loc_args, CalledVia::Space))),
                    },
                    state,
                )),
                // '=' after optional spaces
                Some(Either::Second(Either::Second((spaces_before_equals, equals_indent)))) => {
                    let region = loc_expr.region;

                    // Re-parse the Expr as a Pattern.
                    let pattern = match expr_to_pattern(arena, &loc_expr.value) {
                        Ok(valid) => valid,
                        Err(fail) => return Err((fail, state)),
                    };

                    // Make sure we don't discard the spaces - might be comments in there!
                    let value = if spaces_before_equals.is_empty() {
                        pattern
                    } else {
                        Pattern::SpaceAfter(arena.alloc(pattern), spaces_before_equals)
                    };

                    let loc_first_pattern = Located {
                        region: region.clone(),
                        value,
                    };

                    // Continue parsing the expression as a Def.
                    let (spaces_after_equals, state) = space0(min_indent).parse(arena, state)?;
                    let (parsed_expr, state) =
                        parse_def_expr(min_indent, equals_indent, arena, state, loc_first_pattern)?;

                    let value = if spaces_after_equals.is_empty() {
                        parsed_expr
                    } else {
                        Expr::SpaceBefore(arena.alloc(parsed_expr), spaces_after_equals)
                    };

                    Ok((Located { value, region }, state))
                }
                // '.' and a record field immediately after ')', no optional spaces
                Some(Either::Second(Either::First(fields))) => Ok((
                    Located {
                        region: loc_expr_with_extras.region,
                        value: Expr::Field(arena.alloc(loc_expr), fields),
                    },
                    state,
                )),
                None => Ok((loc_expr, state)),
            }
        },
    )
}

/// If the given Expr would parse the same way as a valid Pattern, convert it.
/// Example: (foo) could be either an Expr::Var("foo") or Pattern::Identifier("foo")
fn expr_to_pattern<'a>(arena: &'a Bump, expr: &Expr<'a>) -> Result<Pattern<'a>, Fail> {
    match expr {
        Expr::Var(module_parts, value) => {
            if module_parts.is_empty() {
                Ok(Pattern::Identifier(value))
            } else {
                Ok(Pattern::QualifiedIdentifier(MaybeQualified {
                    module_parts,
                    value,
                }))
            }
        }
        Expr::Variant(module_parts, value) => Ok(Pattern::Variant(module_parts, value)),
        Expr::Apply((loc_val, loc_args, _)) => {
            let region = loc_val.region.clone();
            let value = expr_to_pattern(arena, &loc_val.value)?;
            let val_pattern = arena.alloc(Located { region, value });

            let mut arg_patterns = Vec::with_capacity_in(loc_args.len(), arena);

            for loc_arg in loc_args {
                let region = loc_arg.region.clone();
                let value = expr_to_pattern(arena, &loc_arg.value)?;

                arg_patterns.push(Located { region, value });
            }

            let pattern = Pattern::Apply(val_pattern, arg_patterns.into_bump_slice());

            Ok(pattern)
        }

        Expr::SpaceBefore(sub_expr, spaces) => Ok(Pattern::SpaceBefore(
            arena.alloc(expr_to_pattern(arena, sub_expr)?),
            spaces,
        )),
        Expr::SpaceAfter(sub_expr, spaces) => Ok(Pattern::SpaceAfter(
            arena.alloc(expr_to_pattern(arena, sub_expr)?),
            spaces,
        )),

        Expr::Record(loc_exprs) => {
            let mut loc_patterns = Vec::with_capacity_in(loc_exprs.len(), arena);

            for loc_expr in loc_exprs {
                let region = loc_expr.region.clone();
                let value = expr_to_pattern(arena, &loc_expr.value)?;

                loc_patterns.push(Located { region, value });
            }

            Ok(Pattern::RecordDestructure(loc_patterns))
        }

        Expr::Float(string) => Ok(Pattern::FloatLiteral(string)),
        Expr::Int(string) => Ok(Pattern::IntLiteral(string)),
        Expr::HexInt(string) => Ok(Pattern::HexIntLiteral(string)),
        Expr::OctalInt(string) => Ok(Pattern::OctalIntLiteral(string)),
        Expr::BinaryInt(string) => Ok(Pattern::BinaryIntLiteral(string)),
        Expr::Str(string) => Ok(Pattern::StrLiteral(string)),
        Expr::MalformedIdent(string) => Ok(Pattern::Malformed(string)),

        // These would not have parsed as patterns
        Expr::BlockStr(_)
        | Expr::AccessorFunction(_)
        | Expr::Field(_, _)
        | Expr::List(_)
        | Expr::Closure(_)
        | Expr::Operator(_)
        | Expr::AssignField(_, _)
        | Expr::Defs(_)
        | Expr::If(_)
        | Expr::Case(_, _)
        | Expr::MalformedClosure
        | Expr::PrecedenceConflict(_, _, _)
        | Expr::QualifiedField(_, _) => Err(Fail {
            attempting: Attempting::Def,
            reason: FailReason::InvalidPattern,
        }),
    }
}

/// A def beginning with a parenthetical pattern, for example:
///
/// (UserId userId) = ...
///
/// Note: Parenthetical patterns are a shorthand convenience, and may not have type annotations.
/// It would be too weird to parse; imagine `(UserId userId) : ...` above `(UserId userId) = ...`
pub fn loc_parenthetical_def<'a>(min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>> {
    move |arena, state| {
        let (loc_tuple, state) = loc(and(
            space0_after(
                between(
                    char('('),
                    space0_around(loc(pattern(min_indent)), min_indent),
                    char(')'),
                ),
                min_indent,
            ),
            equals_with_indent(),
        ))
        .parse(arena, state)?;

        let region = loc_tuple.region;
        let (loc_first_pattern, equals_sign_indent) = loc_tuple.value;
        let (value, state) = parse_def_expr(
            min_indent,
            equals_sign_indent,
            arena,
            state,
            loc_first_pattern,
        )?;

        Ok((Located { value, region }, state))
    }
}

/// The '=' used in a def can't be followed by another '=' (or else it's actually
/// an "==") and also it can't be followed by '>' (or else it's actually an "=>")
fn equals_for_def<'a>() -> impl Parser<'a, ()> {
    not_followed_by(char('='), one_of2(char('='), char('>')))
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
            // A pattern followed by '='
            skip_second(
                space0_after(loc_closure_param(min_indent), min_indent),
                equals_for_def(),
            ),
            // Spaces after the '=' (at a normal indentation level) and then the expr.
            // The expr itself must be indented more than the pattern and '='
            space0_before(
                loc(move |arena, state| parse_expr(indented_more, arena, state)),
                min_indent,
            ),
        ),
        |arena, (loc_pattern, loc_expr)| Def::BodyOnly(loc_pattern, arena.alloc(loc_expr)),
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
                reason: FailReason::OutdentedTooFar,
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
            attempt(
                Attempting::Def,
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
                        zero_or_more(and(space1(original_indent), def(original_indent))),
                        // Parse the final expression that will be returned.
                        // It should be indented the same amount as the original.
                        space1_before(
                            loc(move |arena, state| parse_expr(original_indent, arena, state)),
                            original_indent,
                        ),
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

fn loc_function_arg<'a>(min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>> {
    skip_first(
        // If this is a reserved keyword ("if", "then", "case, "when"), then
        // it is not a function argument!
        not(reserved_keyword()),
        // Don't parse operators, because they have a higher precedence than function application.
        // If we encounter one, we're done parsing function args!
        move |arena, state| loc_parse_function_arg(min_indent, arena, state),
    )
}

fn loc_parse_function_arg<'a>(
    min_indent: u16,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Located<Expr<'a>>> {
    one_of9(
        loc_parenthetical_expr(min_indent),
        loc(string_literal()),
        loc(number_literal()),
        loc(closure(min_indent)),
        loc(record_literal(min_indent)),
        loc(list_literal(min_indent)),
        loc(case_expr(min_indent)),
        loc(if_expr(min_indent)),
        loc(ident_without_apply()),
    )
    .parse(arena, state)
}

fn reserved_keyword<'a>() -> impl Parser<'a, ()> {
    one_of5(
        string(keyword::IF),
        string(keyword::THEN),
        string(keyword::ELSE),
        string(keyword::CASE),
        string(keyword::WHEN),
    )
}

fn closure<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    map_with_arena(
        skip_first(
            // All closures start with a '\' - e.g. (\x -> x + 1)
            char('\\'),
            // Once we see the '\', we're committed to parsing this as a closure.
            // It may turn out to be malformed, but it is definitely a closure.
            optional(and(
                // Parse the params
                attempt(
                    Attempting::ClosureParams,
                    one_or_more(space0_around(loc_closure_param(min_indent), min_indent)),
                ),
                skip_first(
                    // Parse the -> which separates params from body
                    string("->"),
                    // Parse the body
                    attempt(
                        Attempting::ClosureBody,
                        space0_before(
                            loc(move |arena, state| parse_expr(min_indent, arena, state)),
                            min_indent,
                        ),
                    ),
                ),
            )),
        ),
        |arena, opt_contents| match opt_contents {
            None => Expr::MalformedClosure,
            Some((params, loc_body)) => Expr::Closure(arena.alloc((params, loc_body))),
        },
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
    one_of5(
        underscore_pattern(),
        variant_pattern(),
        ident_pattern(),
        record_destructure(min_indent),
        string_pattern(),
    )
}

fn string_pattern<'a>() -> impl Parser<'a, Pattern<'a>> {
    map(parse::string_literal::parse(), Pattern::StrLiteral)
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
    map(unqualified_variant(), |name| Pattern::Variant(&[], name))
}

fn ident_pattern<'a>() -> impl Parser<'a, Pattern<'a>> {
    map(unqualified_ident(), Pattern::Identifier)
}

pub fn case_expr<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    then(
        and(
            case_with_indent(),
            attempt(
                Attempting::CaseCondition,
                skip_second(
                    space1_around(
                        loc(move |arena, state| parse_expr(min_indent, arena, state)),
                        min_indent,
                    ),
                    string(keyword::WHEN),
                ),
            ),
        ),
        move |arena, state, (case_indent, loc_condition)| {
            if case_indent < min_indent {
                panic!("TODO case wasns't indented enough");
            }

            // Everything in the branches must be indented at least as much as the case itself.
            let min_indent = case_indent;

            let (branches, state) =
                attempt(Attempting::CaseBranch, case_branches(min_indent)).parse(arena, state)?;

            Ok((Expr::Case(arena.alloc(loc_condition), branches), state))
        },
    )
}

pub fn case_branches<'a>(
    min_indent: u16,
) -> impl Parser<'a, Vec<'a, &'a (Located<Pattern<'a>>, Located<Expr<'a>>)>> {
    move |arena, state| {
        let mut branches: Vec<'a, &'a (Located<Pattern<'a>>, Located<Expr<'a>>)> =
            Vec::with_capacity_in(2, arena);

        // 1. Parse the first branch and get its indentation level. (It must be >= min_indent.)
        // 2. Parse the other branches. Their indentation levels must be == the first branch's.

        let (mut loc_first_pattern, state) =
            space1_before(loc(pattern(min_indent)), min_indent).parse(arena, state)?;
        let original_indent = state.indent_col;
        let indented_more = original_indent + 1;
        let (spaces_before_arrow, state) = space0(min_indent).parse(arena, state)?;

        // Record the spaces before the first "->", if any.
        if !spaces_before_arrow.is_empty() {
            let region = loc_first_pattern.region;
            let value =
                Pattern::SpaceAfter(arena.alloc(loc_first_pattern.value), spaces_before_arrow);

            loc_first_pattern = Located { region, value };
        };

        // Parse the first "->" and the expression after it.
        let (loc_first_expr, mut state) = skip_first(
            string("->"),
            // The expr must be indented more than the pattern preceding it
            space0_before(
                loc(move |arena, state| parse_expr(indented_more, arena, state)),
                indented_more,
            ),
        )
        .parse(arena, state)?;

        // Record this as the first branch, then optionally parse additional branches.
        branches.push(arena.alloc((loc_first_pattern, loc_first_expr)));

        let branch_parser = and(
            then(
                space1_around(loc(pattern(min_indent)), min_indent),
                move |_arena, state, loc_pattern| {
                    if state.indent_col == original_indent {
                        Ok((loc_pattern, state))
                    } else {
                        panic!(
                            "TODO additional branch didn't have same indentation as first branch"
                        );
                    }
                },
            ),
            skip_first(
                string("->"),
                space1_before(
                    loc(move |arena, state| parse_expr(min_indent, arena, state)),
                    min_indent,
                ),
            ),
        );

        loop {
            match branch_parser.parse(arena, state) {
                Ok((next_output, next_state)) => {
                    state = next_state;

                    branches.push(arena.alloc(next_output));
                }
                Err((_, old_state)) => {
                    state = old_state;

                    break;
                }
            }
        }

        Ok((branches, state))
    }
}

pub fn if_expr<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    map_with_arena(
        and(
            skip_first(
                string(keyword::IF),
                space1_around(
                    loc(move |arena, state| parse_expr(min_indent, arena, state)),
                    min_indent,
                ),
            ),
            and(
                skip_first(
                    string(keyword::THEN),
                    space1_around(
                        loc(move |arena, state| parse_expr(min_indent, arena, state)),
                        min_indent,
                    ),
                ),
                skip_first(
                    string(keyword::ELSE),
                    space1_before(
                        loc(move |arena, state| parse_expr(min_indent, arena, state)),
                        min_indent,
                    ),
                ),
            ),
        ),
        |arena, (condition, (then_branch, else_branch))| {
            Expr::If(arena.alloc((condition, then_branch, else_branch)))
        },
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

                    Ok((
                        Expr::Apply(arena.alloc((loc_expr, loc_args, CalledVia::Space))),
                        state,
                    ))
                }
                Some(Either::Second((spaces_before_equals, Either::First(equals_indent)))) => {
                    let pattern: Pattern<'a> = Pattern::from_ident(arena, loc_ident.value);
                    let value = if spaces_before_equals.is_empty() {
                        pattern
                    } else {
                        Pattern::SpaceAfter(arena.alloc(pattern), spaces_before_equals)
                    };
                    let region = loc_ident.region;
                    let loc_pattern = Located { region, value };
                    let (spaces_after_equals, state) = space0(min_indent).parse(arena, state)?;
                    let (parsed_expr, state) =
                        parse_def_expr(min_indent, equals_indent, arena, state, loc_pattern)?;

                    let answer = if spaces_after_equals.is_empty() {
                        parsed_expr
                    } else {
                        Expr::SpaceBefore(arena.alloc(parsed_expr), spaces_after_equals)
                    };

                    Ok((answer, state))
                }
                Some(Either::Second((_space_list, Either::Second(())))) => {
                    panic!("TODO handle annotation, making sure not to drop comments!");
                }
                None => {
                    let ident = loc_ident.value.clone();

                    Ok((ident_to_expr(ident), state))
                }
            }
        },
    )
}

pub fn ident_without_apply<'a>() -> impl Parser<'a, Expr<'a>> {
    then(loc(ident()), move |_arena, state, loc_ident| {
        Ok((ident_to_expr(loc_ident.value), state))
    })
}

pub fn equals_with_indent<'a>() -> impl Parser<'a, u16> {
    move |_arena, state: State<'a>| {
        let mut iter = state.input.chars();

        match iter.next() {
            Some(ch) if ch == '=' => {
                match iter.peekable().peek() {
                    // The '=' must not be followed by another `=` or `>`
                    // (See equals_for_def() for explanation)
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

pub fn case_with_indent<'a>() -> impl Parser<'a, u16> {
    move |arena, state: State<'a>| {
        string(keyword::CASE)
            .parse(arena, state)
            .map(|((), state)| ((state.indent_col, state)))
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
    one_of16(
        // Sorted from highest to lowest predicted usage in practice,
        // so that successful matches shorrt-circuit as early as possible.
        map(string("|>"), |_| Operator::Pizza),
        map(string("=="), |_| Operator::Equals),
        map(string("&&"), |_| Operator::And),
        map(string("||"), |_| Operator::Or),
        map(char('+'), |_| Operator::Plus),
        map(char('-'), |_| Operator::Minus),
        map(char('*'), |_| Operator::Star),
        map(char('/'), |_| Operator::Slash),
        map(char('<'), |_| Operator::LessThan),
        map(char('>'), |_| Operator::GreaterThan),
        map(string("<="), |_| Operator::LessThanOrEq),
        map(string(">="), |_| Operator::GreaterThanOrEq),
        map(char('^'), |_| Operator::Caret),
        map(char('%'), |_| Operator::Percent),
        map(string("//"), |_| Operator::DoubleSlash),
        map(string("%%"), |_| Operator::DoublePercent),
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

    then(
        and(
            attempt(Attempting::Record, loc(fields)),
            optional(and(space0(min_indent), equals_with_indent())),
        ),
        move |arena, state, (loc_field_exprs, opt_def)| match opt_def {
            None => Ok((Expr::Record(loc_field_exprs.value), state)),
            Some((spaces_before_equals, equals_indent)) => {
                let region = loc_field_exprs.region;
                let field_exprs = loc_field_exprs.value;
                let mut loc_patterns = Vec::with_capacity_in(field_exprs.len(), arena);

                for loc_field_expr in field_exprs {
                    let region = loc_field_expr.region;

                    // If this is a record destructure, these should all be
                    // unqualified Var expressions!
                    let value = match loc_field_expr.value {
                        Expr::Var(module_parts, value) => {
                            if module_parts.is_empty() {
                                Pattern::Identifier(value)
                            } else {
                                Pattern::QualifiedIdentifier(MaybeQualified {
                                    module_parts,
                                    value,
                                })
                            }
                        }
                        _ => {
                            panic!("TODO handle malformed record destructure.");
                            // Malformed("???"),
                        }
                    };

                    loc_patterns.push(Located { region, value });
                }

                let pattern = Pattern::RecordDestructure(loc_patterns);
                let value = if spaces_before_equals.is_empty() {
                    pattern
                } else {
                    Pattern::SpaceAfter(arena.alloc(pattern), spaces_before_equals)
                };
                let loc_pattern = Located { region, value };
                let (spaces_after_equals, state) = space0(min_indent).parse(arena, state)?;

                let (parsed_expr, state) =
                    parse_def_expr(min_indent, equals_indent, arena, state, loc_pattern)?;

                let answer = if spaces_after_equals.is_empty() {
                    parsed_expr
                } else {
                    Expr::SpaceBefore(arena.alloc(parsed_expr), spaces_after_equals)
                };

                Ok((answer, state))
            }
        },
    )
}

/// This is mainly for matching variants in closure params, e.g. \Foo -> ...
///
/// TODO: this should absolutely support qualified variants. Need to change it and rename it!
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

pub fn string_literal<'a>() -> impl Parser<'a, Expr<'a>> {
    map(parse::string_literal::parse(), Expr::Str)
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
