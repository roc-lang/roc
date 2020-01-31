#[macro_use]
pub mod parser;
pub mod ast;
pub mod blankspace;
pub mod ident;
pub mod keyword;
pub mod module;
pub mod number_literal;
pub mod problems;
pub mod string_literal;
pub mod type_annotation;

use crate::operator::{BinOp, CalledVia, UnaryOp};
use crate::parse::ast::{AssignedField, Attempting, Def, Expr, MaybeQualified, Pattern, Spaceable};
use crate::parse::blankspace::{
    space0, space0_after, space0_around, space0_before, space1, space1_around, space1_before,
};
use crate::parse::ident::{global_tag_or_ident, ident, lowercase_ident, Ident};
use crate::parse::number_literal::number_literal;
use crate::parse::parser::{
    allocated, char, not, not_followed_by, optional, sep_by1, string, then, unexpected,
    unexpected_eof, Either, Fail, FailReason, ParseResult, Parser, State,
};
use crate::region::{Located, Region};
use bumpalo::collections::Vec;
use bumpalo::Bump;

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
    one_of!(
        loc_parenthetical_expr(min_indent),
        loc!(string_literal()),
        loc!(number_literal()),
        loc!(closure(min_indent)),
        loc!(record_literal(min_indent)),
        loc!(list_literal(min_indent)),
        loc!(unary_op(min_indent)),
        loc!(when::expr(min_indent)),
        loc!(if_expr(min_indent)),
        loc!(ident_etc(min_indent))
    )
    .parse(arena, state)
}

/// Unary (!) or (-)
///
/// e.g. `!x` or `-x`
pub fn unary_op<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    one_of!(
        map_with_arena!(
            and!(
                loc!(char('!')),
                loc!(move |arena, state| parse_expr(min_indent, arena, state))
            ),
            |arena: &'a Bump, (loc_op, loc_expr): (Located<()>, Located<Expr<'a>>)| {
                Expr::UnaryOp(arena.alloc(loc_expr), loc_op.map(|_| UnaryOp::Not))
            }
        ),
        map_with_arena!(
            and!(
                loc!(char('-')),
                loc!(move |arena, state| parse_expr(min_indent, arena, state))
            ),
            |arena: &'a Bump, (loc_op, loc_expr): (Located<()>, Located<Expr<'a>>)| {
                Expr::UnaryOp(arena.alloc(loc_expr), loc_op.map(|_| UnaryOp::Negate))
            }
        )
    )
}

fn parse_expr<'a>(min_indent: u16, arena: &'a Bump, state: State<'a>) -> ParseResult<'a, Expr<'a>> {
    let expr_parser = crate::parse::parser::map_with_arena(
        and!(
            // First parse the body without operators, then try to parse possible operators after.
            move |arena, state| loc_parse_expr_body_without_operators(min_indent, arena, state),
            // Parse the operator, with optional spaces before it.
            //
            // Since spaces can only wrap an Expr, not an BinOp, we have to first
            // parse the spaces and then attach them retroactively to the expression
            // preceding the operator (the one we parsed before considering operators).
            optional(and!(
                and!(space0(min_indent), loc!(binop())),
                // The spaces *after* the operator can be attached directly to
                // the expression following the operator.
                space0_before(
                    loc!(move |arena, state| parse_expr(min_indent, arena, state)),
                    min_indent,
                )
            ))
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

                Expr::BinOp(tuple)
            }
            None => loc_expr1.value,
        },
    );

    expr_parser.parse(arena, state)
}

pub fn loc_parenthetical_expr<'a>(min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>> {
    then(
        loc!(and!(
            between!(
                char('('),
                map_with_arena!(
                    space0_around(
                        loc!(move |arena, state| parse_expr(min_indent, arena, state)),
                        min_indent,
                    ),
                    |arena: &'a Bump, loc_expr: Located<Expr<'a>>| {
                        Located {
                            region: loc_expr.region,
                            value: Expr::ParensAround(arena.alloc(loc_expr.value)),
                        }
                    }
                ),
                char(')')
            ),
            optional(either!(
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
                either!(
                    one_or_more!(skip_first!(char('.'), lowercase_ident())),
                    and!(space0(min_indent), equals_with_indent())
                )
            ))
        )),
        move |arena, state, loc_expr_with_extras| {
            // We parse the parenthetical expression *and* the arguments after it
            // in one region, so that (for example) the region for Apply includes its args.
            let (loc_expr, opt_extras) = loc_expr_with_extras.value;

            match opt_extras {
                Some(Either::First(loc_args)) => {
                    let mut allocated_args = Vec::with_capacity_in(loc_args.len(), arena);

                    for loc_arg in loc_args {
                        allocated_args.push(&*arena.alloc(loc_arg));
                    }

                    Ok((
                        Located {
                            region: loc_expr_with_extras.region,
                            value: Expr::Apply(
                                arena.alloc(loc_expr),
                                allocated_args,
                                CalledVia::Space,
                            ),
                        },
                        state,
                    ))
                }
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

                    let loc_first_pattern = Located { region, value };

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
                Some(Either::Second(Either::First(fields))) => {
                    let mut value = loc_expr.value;

                    for field in fields {
                        // Wrap the previous answer in the new one, so we end up
                        // with a nested Expr. That way, `foo.bar.baz` gets represented
                        // in the AST as if it had been written (foo.bar).baz all along.
                        value = Expr::Access(arena.alloc(value), field);
                    }

                    Ok((
                        Located {
                            region: loc_expr.region,
                            value,
                        },
                        state,
                    ))
                }
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
        Expr::GlobalTag(value) => Ok(Pattern::GlobalTag(value)),
        Expr::PrivateTag(value) => Ok(Pattern::PrivateTag(value)),
        Expr::Apply(loc_val, loc_args, _) => {
            let region = loc_val.region;
            let value = expr_to_pattern(arena, &loc_val.value)?;
            let val_pattern = arena.alloc(Located { region, value });

            let mut arg_patterns = Vec::with_capacity_in(loc_args.len(), arena);

            for loc_arg in loc_args {
                let region = loc_arg.region;
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

        Expr::ParensAround(sub_expr) | Expr::Nested(sub_expr) => expr_to_pattern(arena, sub_expr),

        Expr::Record {
            fields,
            update: None,
        } => {
            let mut loc_patterns = Vec::with_capacity_in(fields.len(), arena);

            for loc_assigned_field in fields {
                let region = loc_assigned_field.region;
                let value = assigned_expr_field_to_pattern(arena, &loc_assigned_field.value)?;

                loc_patterns.push(Located { region, value });
            }

            Ok(Pattern::RecordDestructure(loc_patterns))
        }

        Expr::Float(string) => Ok(Pattern::FloatLiteral(string)),
        Expr::Int(string) => Ok(Pattern::IntLiteral(string)),
        Expr::NonBase10Int {
            string,
            base,
            is_negative,
        } => Ok(Pattern::NonBase10Literal {
            string,
            base: *base,
            is_negative: *is_negative,
        }),
        Expr::Str(string) => Ok(Pattern::StrLiteral(string)),
        Expr::MalformedIdent(string) => Ok(Pattern::Malformed(string)),

        // These would not have parsed as patterns
        Expr::BlockStr(_)
        | Expr::AccessorFunction(_)
        | Expr::Access(_, _)
        | Expr::List(_)
        | Expr::Closure(_, _)
        | Expr::BinOp(_)
        | Expr::Defs(_, _)
        | Expr::If(_)
        | Expr::When(_, _)
        | Expr::MalformedClosure
        | Expr::PrecedenceConflict(_, _, _)
        | Expr::Record {
            update: Some(_), ..
        }
        | Expr::UnaryOp(_, _) => Err(Fail {
            attempting: Attempting::Def,
            reason: FailReason::InvalidPattern,
        }),
    }
}

/// use for expressions like { x: a + b }
pub fn assigned_expr_field_to_pattern<'a>(
    arena: &'a Bump,
    assigned_field: &AssignedField<'a, Expr<'a>>,
) -> Result<Pattern<'a>, Fail> {
    // the assigned fields always store spaces, but this slice is often empty
    Ok(match assigned_field {
        AssignedField::LabeledValue(name, spaces, value) => {
            let pattern = expr_to_pattern(arena, &value.value)?;
            let result = arena.alloc(Located {
                region: value.region,
                value: pattern,
            });
            if spaces.is_empty() {
                Pattern::RecordField(name.value, result)
            } else {
                Pattern::SpaceAfter(
                    arena.alloc(Pattern::RecordField(name.value, result)),
                    spaces,
                )
            }
        }
        AssignedField::LabelOnly(name) => Pattern::Identifier(name.value),
        AssignedField::SpaceBefore(nested, spaces) => Pattern::SpaceBefore(
            arena.alloc(assigned_expr_field_to_pattern(arena, nested)?),
            spaces,
        ),
        AssignedField::SpaceAfter(nested, spaces) => Pattern::SpaceAfter(
            arena.alloc(assigned_expr_field_to_pattern(arena, nested)?),
            spaces,
        ),
        AssignedField::Malformed(string) => Pattern::Malformed(string),
    })
}

/// Used for patterns like { x: Just _ }
pub fn assigned_pattern_field_to_pattern<'a>(
    arena: &'a Bump,
    assigned_field: &AssignedField<'a, Pattern<'a>>,
    backup_region: Region,
) -> Result<Located<Pattern<'a>>, Fail> {
    // the assigned fields always store spaces, but this slice is often empty
    Ok(match assigned_field {
        AssignedField::LabeledValue(name, spaces, value) => {
            let pattern = value.value.clone();
            let region = Region::span_across(&value.region, &value.region);
            let result = arena.alloc(Located {
                region: value.region,
                value: pattern,
            });
            if spaces.is_empty() {
                Located::at(region, Pattern::RecordField(name.value, result))
            } else {
                Located::at(
                    region,
                    Pattern::SpaceAfter(
                        arena.alloc(Pattern::RecordField(name.value, result)),
                        spaces,
                    ),
                )
            }
        }
        AssignedField::LabelOnly(name) => Located::at(name.region, Pattern::Identifier(name.value)),
        AssignedField::SpaceBefore(nested, spaces) => {
            let can_nested = assigned_pattern_field_to_pattern(arena, nested, backup_region)?;
            Located::at(
                can_nested.region,
                Pattern::SpaceBefore(arena.alloc(can_nested.value), spaces),
            )
        }
        AssignedField::SpaceAfter(nested, spaces) => {
            let can_nested = assigned_pattern_field_to_pattern(arena, nested, backup_region)?;
            Located::at(
                can_nested.region,
                Pattern::SpaceAfter(arena.alloc(can_nested.value), spaces),
            )
        }
        AssignedField::Malformed(string) => Located::at(backup_region, Pattern::Malformed(string)),
    })
}

/// A def beginning with a parenthetical pattern, for example:
///
/// (UserId userId) = ...
///
/// Note: Parenthetical patterns are a shorthand convenience, and may not have type annotations.
/// It would be too weird to parse; imagine `(UserId userId) : ...` above `(UserId userId) = ...`
pub fn loc_parenthetical_def<'a>(min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>> {
    move |arena, state| {
        let (loc_tuple, state) = loc!(and!(
            space0_after(
                between!(
                    char('('),
                    space0_around(loc_pattern(min_indent), min_indent),
                    char(')')
                ),
                min_indent,
            ),
            equals_with_indent()
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
    not_followed_by(char('='), one_of!(char('='), char('>')))
}

/// A definition, consisting of one of these:
///
/// * A type alias using `:`
/// * A pattern followed by '=' and then an expression
/// * A type annotation
/// * A type annotation followed on the next line by a pattern, an `=`, and an expression
pub fn def<'a>(min_indent: u16) -> impl Parser<'a, Def<'a>> {
    // Indented more beyond the original indent.
    let indented_more = min_indent + 1;

    // Constant or annotation
    map_with_arena!(
        and!(
            // A pattern followed by '=' or ':'
            space0_after(loc_closure_param(min_indent), min_indent),
            either!(
                // Constant
                skip_first!(
                    equals_for_def(),
                    // Spaces after the '=' (at a normal indentation level) and then the expr.
                    // The expr itself must be indented more than the pattern and '='
                    space0_before(
                        loc!(move |arena, state| parse_expr(indented_more, arena, state)),
                        min_indent,
                    )
                ),
                // Annotation
                skip_first!(
                    char(':'),
                    // Spaces after the ':' (at a normal indentation level) and then the type.
                    // The type itself must be indented more than the pattern and ':'
                    space0_before(type_annotation::located(indented_more), indented_more)
                )
            )
        ),
        |arena: &'a Bump, (loc_pattern, expr_or_ann)| match expr_or_ann {
            Either::First(loc_expr) => Def::Body(arena.alloc(loc_pattern), arena.alloc(loc_expr)),
            Either::Second(loc_ann) => {
                match &loc_pattern.value {
                    // Type aliases initially parse as either global tags
                    // or applied global tags, because they are always uppercase
                    Pattern::GlobalTag(name) => Def::Alias {
                        name: Located {
                            value: name,
                            region: loc_pattern.region.clone(),
                        },
                        vars: &[],
                        ann: loc_ann,
                    },
                    Pattern::Apply(
                        Located {
                            region,
                            value: Pattern::GlobalTag(name),
                        },
                        loc_vars,
                    ) => Def::Alias {
                        name: Located {
                            value: name,
                            region: region.clone(),
                        },
                        vars: loc_vars,
                        ann: loc_ann,
                    },
                    _ => {
                        // This is a regular Annotation
                        Def::Annotation(loc_pattern, loc_ann)
                    }
                }
            }
        }
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
            attempt!(
                Attempting::Def,
                and!(
                    // Parse the body of the first def. It doesn't need any spaces
                    // around it parsed, because both the subsquent defs and the
                    // final body will have space1_before on them.
                    //
                    // It should be indented more than the original, and it will
                    // end when outdented again.
                    loc!(move |arena, state| parse_expr(indented_more, arena, state)),
                    and!(
                        // Optionally parse additional defs.
                        zero_or_more!(allocated(space1_before(
                            loc!(def(original_indent)),
                            original_indent,
                        ))),
                        // Parse the final expression that will be returned.
                        // It should be indented the same amount as the original.
                        space1_before(
                            loc!(move |arena, state: State<'a>| {
                                parse_expr(original_indent, arena, state)
                            }),
                            original_indent,
                        )
                    )
                )
            ),
            move |arena, state, (loc_first_body, (mut defs, loc_ret))| {
                let first_def: Def<'a> =
                    // TODO is there some way to eliminate this .clone() here?
                    Def::Body(arena.alloc(loc_first_pattern.clone()), arena.alloc(loc_first_body));

                let loc_first_def = Located {
                    value: first_def,
                    region: loc_first_pattern.region,
                };

                // Add the first def to the end of the defs. (It's fine that we
                // reorder the first one to the end, because canonicalize will
                // sort all these defs based on their mutual dependencies anyway. Only
                // their regions will ever be visible to the user.)
                defs.push(arena.alloc(loc_first_def));

                Ok((Expr::Defs(defs, arena.alloc(loc_ret)), state))
            },
        )
        .parse(arena, state)
    }
}

fn parse_def_signature<'a>(
    min_indent: u16,
    colon_indent: u16,
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
    // `<` because ':' should be same indent or greater
    } else if colon_indent < original_indent {
        panic!("TODO the : in this declaration seems outdented");
    } else {
        // Indented more beyond the original indent.
        let indented_more = original_indent + 1;

        then(
            attempt!(
                Attempting::Def,
                and!(
                    // Parse the body of the first def. It doesn't need any spaces
                    // around it parsed, because both the subsquent defs and the
                    // final body will have space1_before on them.
                    //
                    // It should be indented more than the original, and it will
                    // end when outdented again.
                    type_annotation::located(indented_more),
                    and!(
                        // Optionally parse additional defs.
                        zero_or_more!(allocated(space1_before(
                            loc!(def(original_indent)),
                            original_indent,
                        ))),
                        // Parse the final expression that will be returned.
                        // It should be indented the same amount as the original.
                        space1_before(
                            loc!(move |arena, state: State<'a>| {
                                parse_expr(original_indent, arena, state)
                            }),
                            original_indent,
                        )
                    )
                )
            ),
            move |arena, state, (loc_first_annotation, (mut defs, loc_ret))| {
                let pat: Located<Pattern<'a>> = loc_first_pattern.clone();
                let ann: Located<ast::TypeAnnotation<'a>> = loc_first_annotation;
                let first_def: Def<'a> =
                    // TODO is there some way to eliminate this .clone() here?
                    Def::Annotation(pat, ann);

                let loc_first_def = Located {
                    value: first_def,
                    region: loc_first_pattern.region,
                };

                // contrary to defs with an expression body, we must ensure the annotation comes just before its
                // corresponding definition (the one with the body).
                defs.insert(0, arena.alloc(loc_first_def));

                Ok((Expr::Defs(defs, arena.alloc(loc_ret)), state))
            },
        )
        .parse(arena, state)
    }
}

fn loc_function_arg<'a>(min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>> {
    skip_first!(
        // If this is a reserved keyword ("if", "then", "case, "when"), then
        // it is not a function argument!
        not(reserved_keyword()),
        // Don't parse operators, because they have a higher precedence than function application.
        // If we encounter one, we're done parsing function args!
        move |arena, state| loc_parse_function_arg(min_indent, arena, state)
    )
}

fn loc_parse_function_arg<'a>(
    min_indent: u16,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Located<Expr<'a>>> {
    one_of!(
        loc_parenthetical_expr(min_indent),
        loc!(string_literal()),
        loc!(number_literal()),
        loc!(closure(min_indent)),
        loc!(record_literal(min_indent)),
        loc!(list_literal(min_indent)),
        loc!(unary_op(min_indent)),
        loc!(when::expr(min_indent)),
        loc!(if_expr(min_indent)),
        loc!(ident_without_apply())
    )
    .parse(arena, state)
}

fn reserved_keyword<'a>() -> impl Parser<'a, ()> {
    one_of!(
        string(keyword::IF),
        string(keyword::THEN),
        string(keyword::ELSE),
        string(keyword::WHEN),
        string(keyword::IS),
        string(keyword::AS)
    )
}

fn closure<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    map_with_arena!(
        skip_first!(
            // All closures start with a '\' - e.g. (\x -> x + 1)
            char('\\'),
            // Once we see the '\', we're committed to parsing this as a closure.
            // It may turn out to be malformed, but it is definitely a closure.
            optional(and!(
                // Parse the params
                attempt!(
                    Attempting::ClosureParams,
                    // Params are comma-separated
                    sep_by1(
                        char(','),
                        space0_around(loc_closure_param(min_indent), min_indent)
                    )
                ),
                skip_first!(
                    // Parse the -> which separates params from body
                    string("->"),
                    // Parse the body
                    attempt!(
                        Attempting::ClosureBody,
                        space0_before(
                            loc!(move |arena, state| parse_expr(min_indent, arena, state)),
                            min_indent,
                        )
                    )
                )
            ))
        ),
        |arena: &'a Bump, opt_contents| match opt_contents {
            None => Expr::MalformedClosure,
            Some((params, loc_body)) => Expr::Closure(arena.alloc(params), arena.alloc(loc_body)),
        }
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
    one_of!(
        // An ident is the most common param, e.g. \foo -> ...
        loc!(ident_pattern()),
        // Underscore is also common, e.g. \_ -> ...
        loc!(underscore_pattern()),
        // You can destructure records in params, e.g. \{ x, y } -> ...
        loc!(record_destructure(min_indent)),
        // If you wrap it in parens, you can match any arbitrary pattern at all.
        // e.g. \User.UserId userId -> ...
        between!(
            char('('),
            space0_around(loc_pattern(min_indent), min_indent),
            char(')')
        ),
        // The least common, but still allowed, e.g. \Foo -> ...
        loc_tag_pattern(min_indent)
    )
    .parse(arena, state)
}

fn loc_pattern<'a>(min_indent: u16) -> impl Parser<'a, Located<Pattern<'a>>> {
    one_of!(
        loc_parenthetical_pattern(min_indent),
        loc!(underscore_pattern()),
        loc_tag_pattern(min_indent),
        loc!(ident_pattern()),
        loc!(record_destructure(min_indent)),
        loc!(string_pattern()),
        loc!(number_pattern())
    )
}

fn loc_parenthetical_pattern<'a>(min_indent: u16) -> impl Parser<'a, Located<Pattern<'a>>> {
    between!(
        char('('),
        move |arena, state| loc_pattern(min_indent).parse(arena, state),
        char(')')
    )
}

fn number_pattern<'a>() -> impl Parser<'a, Pattern<'a>> {
    map_with_arena!(number_literal(), |arena, expr| {
        expr_to_pattern(arena, &expr).unwrap()
    })
}

fn string_pattern<'a>() -> impl Parser<'a, Pattern<'a>> {
    map!(
        crate::parse::string_literal::parse(),
        |result| match result {
            crate::parse::string_literal::StringLiteral::Line(string) =>
                Pattern::StrLiteral(string),
            crate::parse::string_literal::StringLiteral::Block(lines) =>
                Pattern::BlockStrLiteral(lines),
        }
    )
}

fn underscore_pattern<'a>() -> impl Parser<'a, Pattern<'a>> {
    map!(char('_'), |_| Pattern::Underscore)
}

fn record_destructure<'a>(min_indent: u16) -> impl Parser<'a, Pattern<'a>> {
    then(
        record_without_update!(loc_pattern(min_indent), min_indent),
        move |arena, state, assigned_fields| {
            let mut patterns = Vec::with_capacity_in(assigned_fields.len(), arena);
            for assigned_field in assigned_fields {
                match assigned_pattern_field_to_pattern(
                    arena,
                    &assigned_field.value,
                    assigned_field.region,
                ) {
                    Ok(pattern) => patterns.push(pattern),
                    Err(e) => return Err((e, state)),
                }
            }

            Ok((Pattern::RecordDestructure(patterns), state))
        },
    )
}

fn loc_tag_pattern<'a>(min_indent: u16) -> impl Parser<'a, Located<Pattern<'a>>> {
    map_with_arena!(
        and!(
            loc!(one_of!(
                map!(private_tag(), Pattern::PrivateTag),
                map!(global_tag(), Pattern::GlobalTag)
            )),
            // This can optionally be an applied pattern, e.g. (Foo bar) instead of (Foo)
            zero_or_more!(space1_before(loc_pattern(min_indent), min_indent))
        ),
        |arena: &'a Bump,
         (loc_tag, loc_args): (Located<Pattern<'a>>, Vec<'a, Located<Pattern<'a>>>)| {
            if loc_args.is_empty() {
                loc_tag
            } else {
                // TODO FIME this region doesn't cover the tag's
                // arguments; need to add them to the region!
                let region = loc_tag.region;
                let value = Pattern::Apply(&*arena.alloc(loc_tag), loc_args.into_bump_slice());

                Located { region, value }
            }
        }
    )
}

fn ident_pattern<'a>() -> impl Parser<'a, Pattern<'a>> {
    map!(lowercase_ident(), Pattern::Identifier)
}

mod when {
    use super::*;
    use crate::parse::ast::WhenBranch;

    /// Parser for when expressions.
    pub fn expr<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
        then(
            and!(
                when_with_indent(),
                attempt!(
                    Attempting::WhenCondition,
                    skip_second!(
                        space1_around(
                            loc!(move |arena, state| parse_expr(min_indent, arena, state)),
                            min_indent,
                        ),
                        string(keyword::IS)
                    )
                )
            ),
            move |arena, state, (case_indent, loc_condition)| {
                if case_indent < min_indent {
                    panic!("TODO case wasn't indented enough");
                }

                // Everything in the branches must be indented at least as much as the case itself.
                let min_indent = case_indent;

                let (branches, state) =
                    attempt!(Attempting::WhenBranch, branches(min_indent)).parse(arena, state)?;

                Ok((Expr::When(arena.alloc(loc_condition), branches), state))
            },
        )
    }

    /// Parsing when with indentation.
    fn when_with_indent<'a>() -> impl Parser<'a, u16> {
        move |arena, state: State<'a>| {
            string(keyword::WHEN)
                .parse(arena, state)
                .map(|((), state)| (state.indent_col, state))
        }
    }

    /// Parsing branches of when conditional.
    fn branches<'a>(min_indent: u16) -> impl Parser<'a, Vec<'a, &'a WhenBranch<'a>>> {
        move |arena, state| {
            let mut branches: Vec<'a, &'a WhenBranch<'a>> = Vec::with_capacity_in(2, arena);

            // 1. Parse the first branch and get its indentation level. (It must be >= min_indent.)
            // 2. Parse the other branches. Their indentation levels must be == the first branch's.

            let ((loc_first_patterns, loc_first_guard), state) =
                branch_alternatives(min_indent).parse(arena, state)?;
            let loc_first_pattern = loc_first_patterns.first().unwrap();
            let original_indent = loc_first_pattern.region.start_col;
            let indented_more = original_indent + 1;

            // Parse the first "->" and the expression after it.
            let (loc_first_expr, mut state) = branch_result(indented_more).parse(arena, state)?;

            // Record this as the first branch, then optionally parse additional branches.
            branches.push(arena.alloc(WhenBranch {
                patterns: loc_first_patterns,
                value: loc_first_expr,
                guard: loc_first_guard,
            }));

            let branch_parser = map!(
                and!(
                    then(
                        branch_alternatives(min_indent),
                        move |_arena, state, (loc_patterns, loc_guard)| {
                            if alternatives_indented_correctly(&loc_patterns, original_indent) {
                                Ok(((loc_patterns, loc_guard), state))
                            } else {
                                panic!(
                                "TODO additional branch didn't have same indentation as first branch"
                            );
                            }
                        },
                    ),
                    branch_result(indented_more)
                ),
                |((patterns, guard), expr)| WhenBranch {
                    patterns: patterns,
                    value: expr,
                    guard: guard
                }
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

    /// Parsing alternative patterns in when branches.
    fn branch_alternatives<'a>(
        min_indent: u16,
    ) -> impl Parser<'a, (Vec<'a, Located<Pattern<'a>>>, Option<Located<Expr<'a>>>)> {
        one_of!(
            map!(
                sep_by1(
                    char('|'),
                    space0_around(loc_pattern(min_indent), min_indent),
                ),
                |patterns| { (patterns, None) }
            ),
            map!(
                sep_by1(
                    char('|'),
                    space0_around(loc_pattern(min_indent), min_indent),
                ),
                |patterns| { (patterns, None) }
            )
        )
    }

    /// Check if alternatives of a when branch are indented correctly.
    fn alternatives_indented_correctly<'a>(
        loc_patterns: &'a Vec<'a, Located<Pattern<'a>>>,
        original_indent: u16,
    ) -> bool {
        let (first, rest) = loc_patterns.split_first().unwrap();
        let first_indented_correctly = first.region.start_col == original_indent;
        let rest_indented_correctly = rest
            .iter()
            .all(|when_pattern| when_pattern.region.start_col >= original_indent);
        first_indented_correctly && rest_indented_correctly
    }

    /// Parsing the righthandside of a branch in a when conditional.
    fn branch_result<'a>(indent: u16) -> impl Parser<'a, Located<Expr<'a>>> {
        skip_first!(
            string("->"),
            space0_before(
                loc!(move |arena, state| parse_expr(indent, arena, state)),
                indent,
            )
        )
    }
}

pub fn if_expr<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    map_with_arena!(
        and!(
            skip_first!(
                string(keyword::IF),
                space1_around(
                    loc!(move |arena, state| parse_expr(min_indent, arena, state)),
                    min_indent,
                )
            ),
            and!(
                skip_first!(
                    string(keyword::THEN),
                    space1_around(
                        loc!(move |arena, state| parse_expr(min_indent, arena, state)),
                        min_indent,
                    )
                ),
                skip_first!(
                    string(keyword::ELSE),
                    space1_before(
                        loc!(move |arena, state| parse_expr(min_indent, arena, state)),
                        min_indent,
                    )
                )
            )
        ),
        |arena: &'a Bump, (condition, (then_branch, else_branch))| {
            Expr::If(arena.alloc((condition, then_branch, else_branch)))
        }
    )
}

/// This is a helper function for parsing function args.
/// The rules for (-) are special-cased, and they come up in function args.
///
/// They work like this:
///
/// x - y  # "x minus y"
/// x-y    # "x minus y"
/// x- y   # "x minus y" (probably written in a rush)
/// x -y   # "call x, passing (-y)"
///
/// Since operators have higher precedence than function application,
/// any time we encounter a '-' it is unary iff it is both preceded by spaces
/// and is *not* followed by a whitespace character.
#[inline(always)]
fn unary_negate_function_arg<'a>(min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>> {
    then(
        // Spaces, then '-', then *not* more spaces.
        not_followed_by(
            and!(
                space1(min_indent),
                either!(
                    // Try to parse a number literal *before* trying to parse unary negate,
                    // because otherwise (foo -1) will parse as (foo (Num.neg 1))
                    loc!(number_literal()),
                    loc!(char('-'))
                )
            ),
            one_of!(char(' '), char('#'), char('\n')),
        ),
        move |arena, state, (spaces, num_or_minus_char)| {
            match num_or_minus_char {
                Either::First(loc_num_literal) => Ok((loc_num_literal, state)),
                Either::Second(Located { region, .. }) => {
                    let loc_op = Located {
                        region,
                        value: UnaryOp::Negate,
                    };

                    // Continue parsing the function arg as normal.
                    let (loc_expr, state) = loc_function_arg(min_indent).parse(arena, state)?;
                    let region = Region {
                        start_col: loc_op.region.start_col,
                        start_line: loc_op.region.start_line,
                        end_col: loc_expr.region.end_col,
                        end_line: loc_expr.region.end_line,
                    };
                    let value = Expr::UnaryOp(arena.alloc(loc_expr), loc_op);
                    let loc_expr = Located {
                        // Start from where the unary op started,
                        // and end where its argument expr ended.
                        // This is relevant in case (for example)
                        // we have an expression involving parens,
                        // for example `-(foo bar)`
                        region,
                        value,
                    };

                    // spaces can be empy if it's all space characters (no newlines or comments).
                    let value = if spaces.is_empty() {
                        loc_expr.value
                    } else {
                        Expr::SpaceBefore(arena.alloc(loc_expr.value), spaces)
                    };

                    Ok((
                        Located {
                            region: loc_expr.region,
                            value,
                        },
                        state,
                    ))
                }
            }
        },
    )
}

fn loc_function_args<'a>(min_indent: u16) -> impl Parser<'a, Vec<'a, Located<Expr<'a>>>> {
    one_or_more!(one_of!(
        unary_negate_function_arg(min_indent),
        space1_before(loc_function_arg(min_indent), min_indent)
    ))
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
        and!(
            loc!(ident()),
            optional(either!(
                // There may optionally be function args after this ident
                loc_function_args(min_indent),
                // If there aren't any args, there may be a '=' or ':' after it.
                // (It's a syntax error to write e.g. `foo bar =` - so if there
                // were any args, there is definitely no need to parse '=' or ':'!)
                and!(
                    space0(min_indent),
                    either!(equals_with_indent(), colon_with_indent())
                )
            ))
        ),
        move |arena, state, (loc_ident, opt_extras)| {
            // This appears to be a var, keyword, or function application.
            match opt_extras {
                Some(Either::First(loc_args)) => {
                    let loc_expr = Located {
                        region: loc_ident.region,
                        value: ident_to_expr(arena, loc_ident.value),
                    };

                    let mut allocated_args = Vec::with_capacity_in(loc_args.len(), arena);

                    for loc_arg in loc_args {
                        allocated_args.push(&*arena.alloc(loc_arg));
                    }

                    Ok((
                        Expr::Apply(arena.alloc(loc_expr), allocated_args, CalledVia::Space),
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
                Some(Either::Second((spaces_before_colon, Either::Second(colon_indent)))) => {
                    let pattern: Pattern<'a> = Pattern::from_ident(arena, loc_ident.value);
                    let value = if spaces_before_colon.is_empty() {
                        pattern
                    } else {
                        Pattern::SpaceAfter(arena.alloc(pattern), spaces_before_colon)
                    };
                    let region = loc_ident.region;
                    let loc_pattern = Located { region, value };
                    let (spaces_after_equals, state) = space0(min_indent).parse(arena, state)?;
                    let (parsed_expr, state) =
                        parse_def_signature(min_indent, colon_indent, arena, state, loc_pattern)?;

                    let answer = if spaces_after_equals.is_empty() {
                        parsed_expr
                    } else {
                        Expr::SpaceBefore(arena.alloc(parsed_expr), spaces_after_equals)
                    };

                    Ok((answer, state))
                }
                None => {
                    let ident = loc_ident.value.clone();

                    Ok((ident_to_expr(arena, ident), state))
                }
            }
        },
    )
}

pub fn ident_without_apply<'a>() -> impl Parser<'a, Expr<'a>> {
    then(loc!(ident()), move |arena, state, loc_ident| {
        Ok((ident_to_expr(arena, loc_ident.value), state))
    })
}

/// Like equals_for_def(), except it produces the indent_col of the state rather than ()
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

pub fn colon_with_indent<'a>() -> impl Parser<'a, u16> {
    move |_arena, state: State<'a>| {
        let mut iter = state.input.chars();

        match iter.next() {
            Some(ch) if ch == ':' => {
                match iter.peekable().peek() {
                    // The ':' must not be followed by `=`
                    Some(next_ch) if next_ch != &':' && next_ch != &'=' => {
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

fn ident_to_expr<'a>(arena: &'a Bump, src: Ident<'a>) -> Expr<'a> {
    match src {
        Ident::GlobalTag(string) => Expr::GlobalTag(string),
        Ident::PrivateTag(string) => Expr::PrivateTag(string),
        Ident::Access(info) => {
            let mut iter = info.value.iter();

            // The first value in the iterator is the variable name,
            // e.g. `foo` in `foo.bar.baz`
            let mut answer = match iter.next() {
                Some(var) => Expr::Var(info.module_parts, var),
                None => {
                    panic!("Parsed an Ident::Access with no parts");
                }
            };

            // The remaining items in the iterator are record field accesses,
            // e.g. `bar` in `foo.bar.baz`, followed by `baz`
            for field in iter {
                // Wrap the previous answer in the new one, so we end up
                // with a nested Expr. That way, `foo.bar.baz` gets represented
                // in the AST as if it had been written (foo.bar).baz all along.
                answer = Expr::Access(arena.alloc(answer), field);
            }

            answer
        }
        Ident::AccessorFunction(string) => Expr::AccessorFunction(string),
        Ident::Malformed(string) => Expr::MalformedIdent(string),
    }
}

fn binop<'a>() -> impl Parser<'a, BinOp> {
    one_of!(
        // Sorted from highest to lowest predicted usage in practice,
        // so that successful matches shorrt-circuit as early as possible.
        // The only exception to this is that operators which begin
        // with other valid operators (e.g. "<=" begins with "<") must
        // come before the shorter ones; otherwise, they will never
        // be reached because the shorter one will pass and consume!
        map!(string("|>"), |_| BinOp::Pizza),
        map!(string("=="), |_| BinOp::Equals),
        map!(string("!="), |_| BinOp::NotEquals),
        map!(string("&&"), |_| BinOp::And),
        map!(string("||"), |_| BinOp::Or),
        map!(char('+'), |_| BinOp::Plus),
        map!(char('*'), |_| BinOp::Star),
        map!(char('-'), |_| BinOp::Minus),
        map!(string("//"), |_| BinOp::DoubleSlash),
        map!(char('/'), |_| BinOp::Slash),
        map!(string("<="), |_| BinOp::LessThanOrEq),
        map!(char('<'), |_| BinOp::LessThan),
        map!(string(">="), |_| BinOp::GreaterThanOrEq),
        map!(char('>'), |_| BinOp::GreaterThan),
        map!(char('^'), |_| BinOp::Caret),
        map!(string("%%"), |_| BinOp::DoublePercent),
        map!(char('%'), |_| BinOp::Percent)
    )
}

pub fn list_literal<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    let elems = collection!(
        char('['),
        loc!(expr(min_indent)),
        char(','),
        char(']'),
        min_indent
    );

    parser::attempt(
        Attempting::List,
        map_with_arena!(elems, |arena, parsed_elems: Vec<'a, Located<Expr<'a>>>| {
            let mut allocated = Vec::with_capacity_in(parsed_elems.len(), arena);

            for parsed_elem in parsed_elems {
                allocated.push(&*arena.alloc(parsed_elem));
            }

            Expr::List(allocated)
        }),
    )
}

// Parser<'a, Vec<'a, Located<AssignedField<'a, S>>>>
pub fn record_literal<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    then(
        and!(
            attempt!(
                Attempting::Record,
                record!(loc!(expr(min_indent)), min_indent)
            ),
            optional(and!(
                space0(min_indent),
                either!(equals_with_indent(), colon_with_indent())
            ))
        ),
        move |arena, state, ((opt_update, loc_assigned_fields), opt_def)| match opt_def {
            None => {
                // This is a record literal, not a destructure.
                let mut value = Expr::Record {
                    update: opt_update.map(|loc_expr| &*arena.alloc(loc_expr)),
                    fields: loc_assigned_fields.value,
                };

                // there can be field access, e.g. `{ x : 4 }.x`
                let (accesses, state) =
                    optional(one_or_more!(skip_first!(char('.'), lowercase_ident())))
                        .parse(arena, state)?;

                if let Some(fields) = accesses {
                    for field in fields {
                        // Wrap the previous answer in the new one, so we end up
                        // with a nested Expr. That way, `foo.bar.baz` gets represented
                        // in the AST as if it had been written (foo.bar).baz all along.
                        value = Expr::Access(arena.alloc(value), field);
                    }
                }

                Ok((value, state))
            }
            Some((spaces_before_equals, Either::First(equals_indent))) => {
                // This is a record destructure def.
                let region = loc_assigned_fields.region;
                let assigned_fields = loc_assigned_fields.value;
                let mut loc_patterns = Vec::with_capacity_in(assigned_fields.len(), arena);

                for loc_assigned_field in assigned_fields {
                    let region = loc_assigned_field.region;
                    match assigned_expr_field_to_pattern(arena, &loc_assigned_field.value) {
                        Ok(value) => loc_patterns.push(Located { region, value }),
                        // an Expr became a pattern that should not be.
                        Err(e) => return Err((e, state)),
                    }
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
            Some((spaces_before_colon, Either::Second(colon_indent))) => {
                // This is a record type annotation
                let region = loc_assigned_fields.region;
                let assigned_fields = loc_assigned_fields.value;
                let mut loc_patterns = Vec::with_capacity_in(assigned_fields.len(), arena);

                for loc_assigned_field in assigned_fields {
                    let region = loc_assigned_field.region;
                    match assigned_expr_field_to_pattern(arena, &loc_assigned_field.value) {
                        Ok(value) => loc_patterns.push(Located { region, value }),
                        // an Expr became a pattern that should not be.
                        Err(e) => return Err((e, state)),
                    }
                }

                let pattern = Pattern::RecordDestructure(loc_patterns);
                let value = if spaces_before_colon.is_empty() {
                    pattern
                } else {
                    Pattern::SpaceAfter(arena.alloc(pattern), spaces_before_colon)
                };
                let loc_pattern = Located { region, value };
                let (spaces_after_equals, state) = space0(min_indent).parse(arena, state)?;
                let (parsed_expr, state) =
                    parse_def_signature(min_indent, colon_indent, arena, state, loc_pattern)?;

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

/// This is mainly for matching tags in closure params, e.g. \@Foo -> ...
fn private_tag<'a>() -> impl Parser<'a, &'a str> {
    // TODO should be refactored so the name is not allocated again.
    map_with_arena!(
        skip_first!(char('@'), global_tag()),
        |arena: &'a Bump, name: &'a str| {
            use bumpalo::collections::string::String;
            let mut buf = String::with_capacity_in(1 + name.len(), arena);
            buf.push('@');
            buf.push_str(name);
            buf.into_bump_str()
        }
    )
}

/// This is mainly for matching tags in closure params, e.g. \Foo -> ...
fn global_tag<'a>() -> impl Parser<'a, &'a str> {
    global_tag_or_ident(|first_char| first_char.is_uppercase())
}

pub fn string_literal<'a>() -> impl Parser<'a, Expr<'a>> {
    map!(
        crate::parse::string_literal::parse(),
        |result| match result {
            crate::parse::string_literal::StringLiteral::Line(string) => Expr::Str(string),
            crate::parse::string_literal::StringLiteral::Block(lines) => Expr::BlockStr(lines),
        }
    )
}
