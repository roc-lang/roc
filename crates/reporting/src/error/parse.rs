use roc_parse::parser::{ENumber, EReturn, ESingleQuote, EString, FileError, PList, SyntaxError};
use roc_problem::Severity;
use roc_region::all::{LineColumn, LineColumnRegion, LineInfo, Position, Region};
use std::path::PathBuf;

use crate::report::{Report, RocDocAllocator, RocDocBuilder};
use ven_pretty::DocAllocator;

pub fn parse_problem<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    starting_line: u32,
    parse_problem: FileError<SyntaxError<'a>>,
) -> Report<'a> {
    to_syntax_report(
        alloc,
        lines,
        filename,
        &parse_problem.problem.problem,
        lines.convert_line_column(LineColumn {
            line: starting_line,
            column: 0,
        }),
    )
}

fn note_for_record_type_indent<'a>(alloc: &'a RocDocAllocator<'a>) -> RocDocBuilder<'a> {
    alloc.note("I may be confused by indentation")
}

fn note_for_tag_union_type_indent<'a>(alloc: &'a RocDocAllocator<'a>) -> RocDocBuilder<'a> {
    alloc.note("I may be confused by indentation")
}

fn hint_for_tag_name<'a>(alloc: &'a RocDocAllocator<'a>) -> RocDocBuilder<'a> {
    alloc.concat([
        alloc.hint("Tag names "),
        alloc.reflow("start with an uppercase letter, like "),
        alloc.parser_suggestion("Err"),
        alloc.text(" or "),
        alloc.parser_suggestion("Green"),
        alloc.text("."),
    ])
}

fn record_patterns_look_like<'a>(alloc: &'a RocDocAllocator<'a>) -> RocDocBuilder<'a> {
    alloc.concat([
        alloc.reflow(r"Record pattern look like "),
        alloc.parser_suggestion("{ name, age: currentAge },"),
        alloc.reflow(" so I was expecting to see a field name next."),
    ])
}

fn list_patterns_look_like<'a>(alloc: &'a RocDocAllocator<'a>) -> RocDocBuilder<'a> {
    alloc.concat([
        alloc.reflow(r"Record pattern look like "),
        alloc.parser_suggestion("[1, 2, ..]"),
        alloc.reflow(" or "),
        alloc.parser_suggestion("[first, .., last]"),
    ])
}

fn to_syntax_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::SyntaxError<'a>,
    start: Position,
) -> Report<'a> {
    use SyntaxError::*;

    let severity = Severity::RuntimeError;
    let report = |doc| Report {
        filename: filename.clone(),
        doc,
        title: "PARSE PROBLEM".to_string(),
        severity,
    };

    match parse_problem {
        SyntaxError::ArgumentsBeforeEquals(region) => {
            let doc = alloc.stack([
                alloc.reflow("Unexpected tokens in front of the `=` symbol:"),
                alloc.region(lines.convert_region(*region), severity),
            ]);

            Report {
                filename,
                doc,
                title: "PARSE PROBLEM".to_string(),
                severity,
            }
        }
        Unexpected(region) => {
            let mut region = lines.convert_region(*region);
            if region.start().column == region.end().column {
                region = LineColumnRegion::new(region.start(), region.end().bump_column(1));
            }

            let doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("Unexpected token "),
                    // context(alloc, &parse_problem.context_stack, "here"),
                    alloc.text(":"),
                ]),
                alloc.region(region, severity),
            ]);

            report(doc)
        }
        NotEndOfFile(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I expected to reach the end of the file, but got stuck here:"),
                alloc.region(region, severity),
            ]);

            Report {
                filename,
                doc,
                title: "NOT END OF FILE".to_string(),
                severity,
            }
        }
        SyntaxError::Eof(region) => {
            let doc = alloc.stack([
                alloc.reflow("End of Field"),
                alloc.region(lines.convert_region(*region), severity),
            ]);

            Report {
                filename,
                doc,
                title: "PARSE PROBLEM".to_string(),
                severity,
            }
        }
        SyntaxError::OutdentedTooFar => {
            let doc = alloc.stack([alloc.reflow("OutdentedTooFar")]);

            Report {
                filename,
                doc,
                title: "PARSE PROBLEM".to_string(),
                severity,
            }
        }
        Type(typ) => to_type_report(alloc, lines, filename, typ, Position::default()),
        Pattern(pat) => to_pattern_report(alloc, lines, filename, pat, Position::default()),
        Expr(expr, start) => to_expr_report(
            alloc,
            lines,
            filename,
            Context::InDef(*start),
            expr,
            Position::default(),
        ),
        Header(header) => to_header_report(alloc, lines, filename, header, Position::default()),

        // If you're adding or changing syntax, please handle the case with a
        // good error message above instead of adding more unhandled cases below.
        InvalidPattern | BadUtf8 | Todo | ReservedKeyword(_) | NotYetImplemented(_) | Space(_) => {
            to_unhandled_parse_error_report(
                alloc,
                lines,
                filename,
                format!("{:?}", parse_problem),
                start,
                start,
            )
        }
    }
}

#[allow(clippy::enum_variant_names)]
enum Context {
    InNode(Node, Position),
    InDef(Position),
    InDefFinalExpr(Position),
}

enum Node {
    WhenCondition,
    WhenBranch,
    WhenIfGuard,
    IfCondition,
    IfThenBranch,
    IfElseBranch,
    ListElement,
    InsideParens,
    RecordConditionalDefault,
    StringFormat,
    Dbg,
    Expect,
    Return,
}

fn to_expr_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    context: Context,
    parse_problem: &roc_parse::parser::EExpr<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EExpr;
    let severity = Severity::RuntimeError;
    match parse_problem {
        EExpr::If(if_, pos) => to_if_report(alloc, lines, filename, if_, *pos),
        EExpr::When(when, pos) => to_when_report(alloc, lines, filename, when, *pos),
        EExpr::Closure(lambda, pos) => {
            to_lambda_report(alloc, lines, filename, context, lambda, *pos)
        }
        EExpr::List(list, pos) => to_list_report(alloc, lines, filename, list, *pos),
        EExpr::Str(string, pos) => to_str_report(alloc, lines, filename, string, *pos),
        EExpr::InParens(expr, pos) => to_expr_in_parens_report(alloc, lines, filename, expr, *pos),
        EExpr::Type(tipe, pos) => to_type_report(alloc, lines, filename, tipe, *pos),
        EExpr::ElmStyleFunction(region, pos) => {
            let surroundings = Region::new(start, *pos);
            let region = lines.convert_region(*region);

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a definition, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("Looks like you are trying to define a function. "),
                    alloc.reflow("In Roc, functions are always written as a lambda, like "),
                    alloc.parser_suggestion("increment = \\n -> n + 1"),
                    alloc.reflow("."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "ARGUMENTS BEFORE EQUALS".to_string(),
                severity,
            }
        }

        EExpr::BadOperator(op, pos) => {
            let surroundings = Region::new(start, *pos);
            let region = Region::new(*pos, pos.bump_column(op.len() as u32));

            let suggestion = match *op {
                "|" => vec![
                    alloc.reflow("Maybe you want "),
                    alloc.parser_suggestion("||"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("|>"),
                    alloc.reflow(" instead?"),
                ],
                "++" => vec![
                    alloc.reflow("To concatenate two lists or strings, try using "),
                    alloc.parser_suggestion("List.concat"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("Str.concat"),
                    alloc.reflow(" instead."),
                ],
                ":" => vec![alloc.stack([
                    alloc.concat([
                        alloc.reflow("The has-type operator "),
                        alloc.parser_suggestion(":"),
                        alloc.reflow(" can only occur in a definition's type signature, like"),
                    ]),
                    alloc
                        .vcat(vec![
                            alloc.text("increment : I64 -> I64"),
                            alloc.text("increment = \\x -> x + 1"),
                        ])
                        .indent(4),
                ])],
                "->" => match context {
                    Context::InNode(Node::WhenBranch, _pos) => {
                        return to_unexpected_arrow_report(alloc, lines, filename, *pos, start);
                    }

                    Context::InDef(_pos) => {
                        vec![alloc.stack([
                            alloc.reflow("Looks like you are trying to define a function. "),
                            alloc.reflow("In Roc, functions are always written as a lambda, like "),
                            alloc
                                .parser_suggestion("increment = \\n -> n + 1")
                                .indent(4),
                        ])]
                    }

                    _ => {
                        vec![alloc.stack([
                            alloc.concat([
                                alloc.reflow("The arrow "),
                                alloc.parser_suggestion("->"),
                                alloc.reflow(" is only used to define cases in a "),
                                alloc.keyword("`when`"),
                                alloc.reflow("expression:"),
                            ]),
                            alloc
                                .vcat(vec![
                                    alloc.text("when color is"),
                                    alloc.text("Red -> \"stop!\"").indent(4),
                                    alloc.text("Green -> \"go!\"").indent(4),
                                ])
                                .indent(4),
                            alloc.reflow("And to define a function:"),
                            alloc
                                .vcat(vec![
                                    alloc.text("increment : I64 -> I64"),
                                    alloc.text("increment = \\n -> n + 1"),
                                ])
                                .indent(4),
                        ])]
                    }
                },
                "!" => vec![
                    alloc.reflow("The boolean negation operator "),
                    alloc.parser_suggestion("!"),
                    alloc.reflow(" must occur immediately before an expression, like "),
                    alloc.parser_suggestion("!(List.isEmpty primes)"),
                    alloc.reflow(". There cannot be a space between the "),
                    alloc.parser_suggestion("!"),
                    alloc.reflow(" and the expression after it."),
                ],
                "<|" => vec![
                    alloc.reflow("Roc doesn't have a "),
                    alloc.parser_suggestion("<|"),
                    alloc.reflow(" operator. Please use parentheses "),
                    alloc.parser_suggestion("()"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("|>"),
                    alloc.reflow(" instead."),
                ],
                _ => vec![
                    alloc.reflow("I have no specific suggestion for this operator, see "),
                    alloc.parser_suggestion(
                        "https://www.roc-lang.org/tutorial#operator-desugaring-table ",
                    ),
                    alloc.reflow("for the full list of operators in Roc."),
                ],
            };

            let doc = alloc.stack([
                alloc.reflow(r"This looks like an operator, but it's not one I recognize!"),
                alloc.region_with_subregion(
                    lines.convert_region(surroundings),
                    lines.convert_region(region),
                    severity,
                ),
                alloc.concat(suggestion),
            ]);

            Report {
                filename,
                doc,
                title: "UNKNOWN OPERATOR".to_string(),
                severity,
            }
        }

        EExpr::Ident(_pos) => unreachable!("another branch would be taken"),

        EExpr::QualifiedTag(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am very confused by this identifier:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc
                        .reflow("Are you trying to qualify a name? I am expecting something like "),
                    alloc.parser_suggestion("Json.Decode.string"),
                    alloc.reflow(". Maybe you are trying to qualify a tag? Tags like "),
                    alloc.parser_suggestion("Err"),
                    alloc.reflow(" are globally scoped in roc, and cannot be qualified."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD IDENTIFIER".to_string(),
                severity,
            }
        }

        EExpr::Start(pos) | EExpr::IndentStart(pos) => {
            let (title, expecting) = match &context {
                Context::InNode { .. } | Context::InDef { .. } => (
                    "MISSING EXPRESSION",
                    alloc.concat([
                        alloc.reflow("I was expecting to see an expression like "),
                        alloc.parser_suggestion("42"),
                        alloc.reflow(" or "),
                        alloc.parser_suggestion("\"hello\""),
                        alloc.text("."),
                    ]),
                ),
                Context::InDefFinalExpr { .. } => (
                    "MISSING FINAL EXPRESSION",
                    alloc.stack([
                        alloc.concat([
                            alloc.reflow("This definition is missing a final expression."),
                            alloc.reflow(" A nested definition must be followed by"),
                            alloc.reflow(" either another definition, or an expression"),
                        ]),
                        alloc.vcat(vec![
                            alloc.text("x = 4").indent(4),
                            alloc.text("y = 2").indent(4),
                            alloc.text(""),
                            alloc.text("x + y").indent(4),
                        ]),
                    ]),
                ),
            };

            let (context_pos, a_thing) = match context {
                Context::InNode(node, pos) => match node {
                    Node::WhenCondition | Node::WhenBranch | Node::WhenIfGuard => (
                        pos,
                        alloc.concat([
                            alloc.text("a "),
                            alloc.keyword("when"),
                            alloc.text(" expression"),
                        ]),
                    ),
                    Node::IfCondition | Node::IfThenBranch | Node::IfElseBranch => (
                        pos,
                        alloc.concat([
                            alloc.text("an "),
                            alloc.keyword("if"),
                            alloc.text(" expression"),
                        ]),
                    ),
                    Node::ListElement => (pos, alloc.text("a list")),
                    Node::Dbg => (pos, alloc.text("a dbg statement")),
                    Node::Expect => (pos, alloc.text("an expect statement")),
                    Node::Return => (pos, alloc.text("a return statement")),
                    Node::RecordConditionalDefault => (pos, alloc.text("record field default")),
                    Node::StringFormat => (pos, alloc.text("a string format")),
                    Node::InsideParens => (pos, alloc.text("some parentheses")),
                },
                Context::InDef(pos) => (pos, alloc.text("a definition")),
                Context::InDefFinalExpr(pos) => {
                    (pos, alloc.text("a definition's final expression"))
                }
            };

            let surroundings = Region::new(context_pos, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.concat([
                    alloc.reflow(r"I am partway through parsing "),
                    a_thing,
                    alloc.reflow(", but I got stuck here:"),
                ]),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                expecting,
            ]);

            Report {
                filename,
                doc,
                title: title.to_string(),
                severity,
            }
        }

        EExpr::DefMissingFinalExpr(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a definition, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("This definition is missing a final expression."),
                    alloc.reflow(" A nested definition must be followed by"),
                    alloc.reflow(" either another definition, or an expression"),
                ]),
                alloc.vcat(vec![
                    alloc.text("x = 4").indent(4),
                    alloc.text("y = 2").indent(4),
                    alloc.text(""),
                    alloc.text("x + y").indent(4),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "MISSING FINAL EXPRESSION".to_string(),
                severity,
            }
        }

        EExpr::DefMissingFinalExpr2(expr, pos) => to_expr_report(
            alloc,
            lines,
            filename,
            Context::InDefFinalExpr(start),
            expr,
            *pos,
        ),

        EExpr::BadExprEnd(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("Whatever I am running into is confusing me a lot! "),
                    alloc.reflow("Normally I can give fairly specific hints, "),
                    alloc.reflow("but something is really tripping me up this time."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "SYNTAX PROBLEM".to_string(),
                severity,
            }
        }

        EExpr::Colon(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a definition, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("Looks like you are trying to define a function. "),
                    alloc.reflow("In Roc, functions are always written as a lambda, like "),
                    alloc.parser_suggestion("increment = \\n -> n + 1"),
                    alloc.reflow("."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "ARGUMENTS BEFORE EQUALS".to_string(),
                severity,
            }
        }

        EExpr::Record(erecord, pos) => {
            to_record_report(alloc, lines, filename, erecord, *pos, start)
        }

        EExpr::Space(error, pos) => to_space_report(alloc, lines, filename, error, *pos),

        &EExpr::Number(ENumber::End, pos) => {
            to_malformed_number_literal_report(alloc, lines, filename, pos)
        }

        EExpr::Ability(err, pos) => to_ability_def_report(alloc, lines, filename, err, *pos),

        EExpr::IndentEnd(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let snippet =
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity);

            let doc = match context {
                Context::InNode(Node::Expect, _) => alloc.stack([
                    alloc.reflow(
                        r"I am partway through parsing an expect statement, but I got stuck here:",
                    ),
                    snippet,
                    alloc.stack([
                        alloc.reflow(r"I was expecting a final expression, like so"),
                        alloc.vcat([
                            alloc.parser_suggestion("expect 1 + 1 == 2").indent(4),
                            alloc.parser_suggestion("\"done\"").indent(4),
                        ]),
                    ]),
                ]),
                _ => {
                    // generic
                    alloc.stack([
                        alloc.reflow(
                            r"I am partway through parsing an expression, but I got stuck here:",
                        ),
                        snippet,
                        alloc.concat([
                            alloc.reflow(r"Looks like the indentation ends prematurely here. "),
                            alloc.reflow(
                                r"Did you mean to have another expression after this line?",
                            ),
                        ]),
                    ])
                }
            };

            Report {
                filename,
                doc,
                title: "INDENT ENDS AFTER EXPRESSION".to_string(),
                severity,
            }
        }
        EExpr::Expect(e_expect, _position) => {
            let node = Node::Expect;
            to_dbg_or_expect_report(alloc, lines, filename, context, node, e_expect, start)
        }
        EExpr::Dbg(e_expect, _position) => {
            to_dbg_or_expect_report(alloc, lines, filename, context, Node::Dbg, e_expect, start)
        }
        EExpr::Import(e_import, position) => {
            to_import_report(alloc, lines, filename, e_import, *position)
        }
        EExpr::TrailingOperator(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing an expression, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([alloc.reflow("TODO provide more context.")]),
            ]);

            Report {
                filename,
                doc,
                title: "TRAILING OPERATOR".to_string(),
                severity,
            }
        }
        EExpr::UnexpectedComma(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am trying to parse an expression, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([alloc.reflow("This comma in an invalid position.")]),
            ]);

            Report {
                filename,
                doc,
                title: "UNEXPECTED COMMA".to_string(),
                severity,
            }
        }
        EExpr::Return(EReturn::Return(pos) | EReturn::IndentReturnValue(pos), start) => {
            to_expr_report(
                alloc,
                lines,
                filename,
                Context::InNode(Node::Return, *start),
                &EExpr::IndentStart(*pos),
                *pos,
            )
        }
        EExpr::Return(EReturn::ReturnValue(parse_problem, pos), start) => to_expr_report(
            alloc,
            lines,
            filename,
            Context::InNode(Node::Return, *start),
            parse_problem,
            *pos,
        ),
        EExpr::Return(EReturn::Space(parse_problem, pos), _) => {
            to_space_report(alloc, lines, filename, parse_problem, *pos)
        }
        // If you're adding or changing syntax, please handle the case with a
        // good error message above instead of adding more unhandled cases below.
        EExpr::End(pos)
        | EExpr::Dot(pos)
        | EExpr::Access(pos)
        | EExpr::UnaryNot(pos)
        | EExpr::UnaryNegate(pos)
        | EExpr::Pattern(_, pos)
        | EExpr::IndentDefBody(pos)
        | EExpr::IndentEquals(pos)
        | EExpr::IndentAnnotation(pos)
        | EExpr::Equals(pos)
        | EExpr::DoubleColon(pos)
        | EExpr::MalformedPattern(pos)
        | EExpr::DbgContinue(pos)
        | EExpr::Underscore(pos)
        | EExpr::Crash(pos)
        | EExpr::Try(pos)
        | EExpr::UnexpectedTopLevelExpr(pos) => to_unhandled_parse_error_report(
            alloc,
            lines,
            filename,
            format!("{:?}", parse_problem),
            *pos,
            start,
        ),
        EExpr::RecordUpdateOldBuilderField(region)
        | EExpr::RecordUpdateIgnoredField(region)
        | EExpr::RecordBuilderOldBuilderField(region) => to_unhandled_parse_error_report(
            alloc,
            lines,
            filename,
            format!("{:?}", parse_problem),
            region.start(),
            start,
        ),
    }
}

fn to_unhandled_parse_error_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem_str: String,
    pos: Position,
    start: Position,
) -> Report<'a> {
    let severity = Severity::Fatal;
    let surroundings = Region::new(start, pos);
    let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

    let doc = alloc.stack([
        alloc.reflow("I got stuck while parsing this:"),
        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
        alloc.reflow("Here's the internal parse problem:"),
        alloc.string(parse_problem_str).indent(4),
        alloc.reflow("Unfortunately, I'm not able to provide a more insightful error message for this syntax problem yet. This is considered a bug in the compiler."),
        alloc.note("If you'd like to contribute to Roc, this would be a good first issue!")
    ]);

    Report {
        filename,
        doc,
        title: "UNHANDLED PARSE ERROR".to_string(),
        severity,
    }
}

fn to_record_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    _parse_problem: &roc_parse::parser::ERecord<'a>,
    pos: Position,
    start: Position,
) -> Report<'a> {
    let surroundings = Region::new(start, pos);
    let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

    let severity = Severity::RuntimeError;
    let doc = alloc.stack([
        alloc.reflow(r"I am partway through parsing a record, but I got stuck here:"),
        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
        alloc.concat([alloc.reflow("TODO provide more context.")]),
    ]);

    Report {
        filename,
        doc,
        title: "RECORD PARSE PROBLEM".to_string(),
        severity,
    }
}

fn to_lambda_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    _context: Context,
    parse_problem: &roc_parse::parser::EClosure<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EClosure;

    let severity = Severity::RuntimeError;

    match *parse_problem {
        EClosure::Arrow(pos) => match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
            Next::Token("=>") => {
                let surroundings = Region::new(start, pos);
                let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                let doc = alloc.stack([
                    alloc
                        .reflow(r"I am partway through parsing a function argument list, but I got stuck here:"),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([
                        alloc.reflow("I was expecting a "),
                        alloc.parser_suggestion("->"),
                        alloc.reflow(" next."),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "WEIRD ARROW".to_string(),
                    severity,
                }
            }
            _ => {
                let surroundings = Region::new(start, pos);
                let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                let doc = alloc.stack([
                    alloc
                        .reflow(r"I am partway through parsing a function argument list, but I got stuck here:"),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([
                        alloc.reflow("I was expecting a "),
                        alloc.parser_suggestion("->"),
                        alloc.reflow(" next."),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "MISSING ARROW".to_string(),
                    severity,
                }
            }
        },

        EClosure::Comma(pos) => match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
            Next::Token("=>") => {
                let surroundings = Region::new(start, pos);
                let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                let doc = alloc.stack([
                    alloc
                        .reflow(r"I am partway through parsing a function argument list, but I got stuck here:"),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([
                        alloc.reflow("I was expecting a "),
                        alloc.parser_suggestion("->"),
                        alloc.reflow(" next."),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "WEIRD ARROW".to_string(),
                    severity,
                }
            }
            _ => {
                let surroundings = Region::new(start, pos);
                let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                let doc = alloc.stack([
                    alloc
                        .reflow(r"I am partway through parsing a function argument list, but I got stuck here:"),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([
                        alloc.reflow("I was expecting a "),
                        alloc.parser_suggestion("->"),
                        alloc.reflow(" next."),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "MISSING ARROW".to_string(),
                    severity,
                }
            }
        },

        EClosure::Arg(pos) => match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
            Next::Other(Some(',')) => {
                let surroundings = Region::new(start, pos);
                let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                let doc = alloc.stack([
                    alloc
                        .reflow(r"I am partway through parsing a function argument list, but I got stuck at this comma:"),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([
                        alloc.reflow("I was expecting an argument pattern before this, "),
                        alloc.reflow("so try adding an argument before the comma and see if that helps?"),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "UNFINISHED ARGUMENT LIST".to_string(),
                    severity,
                }
            }
            _ => {
                let surroundings = Region::new(start, pos);
                let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                let doc = alloc.stack([
                    alloc
                        .reflow(r"I am partway through parsing a function argument list, but I got stuck here:"),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([
                        alloc.reflow("I was expecting an argument pattern before this, "),
                        alloc.reflow("so try adding an argument and see if that helps?"),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "MISSING ARROW".to_string(),
                    severity,
                }
            }
        },

        EClosure::Start(_pos) => unreachable!("another branch would have been taken"),

        EClosure::Body(expr, pos) => {
            to_expr_report(alloc, lines, filename, Context::InDef(start), expr, pos)
        }
        EClosure::Pattern(ref pattern, pos) => {
            to_pattern_report(alloc, lines, filename, pattern, pos)
        }
        EClosure::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),

        EClosure::IndentArrow(pos) => to_unfinished_lambda_report(
            alloc,
            lines,
            filename,
            pos,
            start,
            alloc.concat([
                alloc.reflow(r"I just saw a pattern, so I was expecting to see a "),
                alloc.parser_suggestion("->"),
                alloc.reflow(" next."),
            ]),
        ),

        EClosure::IndentBody(pos) => to_unfinished_lambda_report(
            alloc,
            lines,
            filename,
            pos,
            start,
            alloc.concat([
                alloc.reflow(r"I just saw a pattern, so I was expecting to see a "),
                alloc.parser_suggestion("->"),
                alloc.reflow(" next."),
            ]),
        ),

        EClosure::IndentArg(pos) => to_unfinished_lambda_report(
            alloc,
            lines,
            filename,
            pos,
            start,
            alloc.concat([
                alloc.reflow(r"I just saw a pattern, so I was expecting to see a "),
                alloc.parser_suggestion("->"),
                alloc.reflow(" next."),
                alloc.reflow(r"I was expecting to see a expression next"),
            ]),
        ),
    }
}

fn to_unfinished_lambda_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    pos: Position,
    start: Position,
    message: RocDocBuilder<'a>,
) -> Report<'a> {
    let surroundings = Region::new(start, pos);
    let region = LineColumnRegion::from_pos(lines.convert_pos(pos));
    let severity = Severity::RuntimeError;

    let doc = alloc.stack([
        alloc.concat([
            alloc.reflow(r"I was partway through parsing a "),
            alloc.reflow(r"function, but I got stuck here:"),
        ]),
        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
        message,
    ]);

    Report {
        filename,
        doc,
        title: "UNFINISHED FUNCTION".to_string(),
        severity,
    }
}

fn to_str_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::EString<'a>,
    start: Position,
) -> Report<'a> {
    let severity = Severity::RuntimeError;

    match *parse_problem {
        EString::Open(_pos) => unreachable!("another branch would be taken"),
        EString::Format(expr, pos) => to_expr_report(
            alloc,
            lines,
            filename,
            Context::InNode(Node::StringFormat, start),
            expr,
            pos,
        ),
        EString::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),
        EString::UnknownEscape(pos) => {
            let surroundings = Region::new(start, pos);
            let region = Region::new(pos, pos.bump_column(2));

            let suggestion = |msg, sugg| {
                alloc
                    .text("- ")
                    .append(alloc.reflow(msg))
                    .append(alloc.parser_suggestion(sugg))
            };

            let doc = alloc.stack([
                alloc.concat([
                    alloc.reflow(r"I was partway through parsing a "),
                    alloc.reflow(r" string literal, but I got stuck here:"),
                ]),
                alloc.region_with_subregion(
                    lines.convert_region(surroundings),
                    lines.convert_region(region),
                    severity,
                ),
                alloc.concat([
                    alloc.reflow(r"This is not an escape sequence I recognize."),
                    alloc.reflow(r" After a backslash, I am looking for one of these:"),
                ]),
                alloc
                    .vcat(vec![
                        suggestion("A newline: ", "\\n"),
                        suggestion("A caret return: ", "\\r"),
                        suggestion("A tab: ", "\\t"),
                        suggestion("An escaped quote: ", "\\\""),
                        suggestion("An escaped backslash: ", "\\\\"),
                        suggestion("A unicode code point: ", "\\u(00FF)"),
                    ])
                    .indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD ESCAPE".to_string(),
                severity,
            }
        }
        EString::CodePtOpen(pos) | EString::CodePtEnd(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(
                    r"I am partway through parsing a unicode code point, but I got stuck here:",
                ),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"I was expecting a hexadecimal number, like "),
                    alloc.parser_suggestion("\\u(1100)"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("\\u(00FF)"),
                    alloc.text("."),
                ]),
                alloc.reflow(r"Learn more about working with unicode in roc at TODO"),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD CODE POINT".to_string(),
                severity,
            }
        }
        EString::FormatEnd(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I cannot find the end of this format expression:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"You could change it to something like "),
                    alloc.parser_suggestion("\"The count is $(count)\""),
                    alloc.reflow("."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "ENDLESS FORMAT".to_string(),
                severity,
            }
        }
        EString::EndlessSingleQuote(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I cannot find the end of this scalar literal (character literal):"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"You could change it to something like "),
                    alloc.parser_suggestion("'a'"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("'\n'"),
                    alloc.reflow("."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "ENDLESS SCALAR".to_string(),
                severity,
            }
        }
        EString::InvalidSingleQuote(e, pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = match e {
                ESingleQuote::Empty => {
                    alloc.stack([
                        alloc.concat([
                            alloc.reflow(r"I am part way through parsing this scalar literal (character literal), "),
                            alloc.reflow(r"but it appears to be empty - which is not a valid scalar."),
                        ]),
                        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                        alloc.concat([
                            alloc.reflow(r"You could change it to something like "),
                            alloc.parser_suggestion("'a'"),
                            alloc.reflow(" or "),
                            alloc.parser_suggestion("'\\n'"),
                            alloc.reflow(". "),
                            alloc.reflow("Note, roc strings use double quotes, like \"hello\".")
                        ]),
                    ])
                }
                ESingleQuote::TooLong => {
                    alloc.stack([
                        alloc.concat([
                            alloc.reflow(r"I am part way through parsing this scalar literal (character literal), "),
                            alloc.reflow(r"but it's too long to fit in a U32 so it's not a valid scalar."),
                        ]),
                        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                        alloc.concat([
                            alloc.reflow(r"You could change it to something like "),
                            alloc.parser_suggestion("'a'"),
                            alloc.reflow(" or "),
                            alloc.parser_suggestion("'\\n'"),
                            alloc.reflow(". "),
                            alloc.reflow("Note, roc strings use double quotes, like \"hello\".")
                        ]),
                    ])
                }
                ESingleQuote::InterpolationNotAllowed => {
                    alloc.stack([
                        alloc.concat([
                            alloc.reflow("I am part way through parsing this single-quote literal, "),
                            alloc.reflow("but I encountered a string interpolation like \"$(this)\","),
                            alloc.reflow("which is not allowed in single-quote literals."),
                        ]),
                        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                        alloc.concat([
                            alloc.reflow(r"You could change it to something like "),
                            alloc.parser_suggestion("'a'"),
                            alloc.reflow(" or "),
                            alloc.parser_suggestion("'\\n'"),
                            alloc.reflow(". "),
                            alloc.reflow("Note, roc strings use double quotes, like \"hello\".")
                        ]),
                    ])
                }
            };

            Report {
                filename,
                doc,
                title: "INVALID SCALAR".to_string(),
                severity,
            }
        }
        EString::EndlessSingleLine(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I cannot find the end of this string:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"You could change it to something like "),
                    alloc.parser_suggestion("\"to be or not to be\""),
                    alloc.reflow(" or even just "),
                    alloc.parser_suggestion("\"\""),
                    alloc.reflow("."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "ENDLESS STRING".to_string(),
                severity,
            }
        }
        EString::ExpectedDoubleQuoteGotSingleQuote(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I was expecting to see a string here, but I got a scalar literal."),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"You could change it to something like "),
                    alloc.parser_suggestion("\"to be or not to be\""),
                    alloc.reflow(" or even just "),
                    alloc.parser_suggestion("\"\""),
                    alloc.reflow(". "),
                    alloc.reflow("Note, roc strings use double quotes."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "EXPECTED STRING".to_string(),
                severity,
            }
        }
        EString::EndlessMultiLine(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I cannot find the end of this block string:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"You could change it to something like "),
                    alloc.parser_suggestion("\"\"\"to be or not to be\"\"\""),
                    alloc.reflow(" or even just "),
                    alloc.parser_suggestion("\"\"\"\"\"\""),
                    alloc.reflow("."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "ENDLESS STRING".to_string(),
                severity,
            }
        }
        EString::MultilineInsufficientIndent(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"This multiline string is not sufficiently indented:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"Lines in a multi-line string must be indented at least as "),
                    alloc.reflow("much as the beginning \"\"\". This extra indentation is automatically removed "),
                    alloc.reflow("from the string during compilation."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "INSUFFICIENT INDENT IN MULTI-LINE STRING".to_string(),
                severity,
            }
        }
        EString::InvalidUnicodeCodepoint(region) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(region.start()));

            let doc = alloc.stack([
                alloc.reflow(
                    r"I am partway through parsing a unicode code point, but I got stuck here:",
                ),
                alloc.region(region, severity),
                alloc.concat([
                    alloc.reflow(r"I was expecting a hexadecimal number, like "),
                    alloc.parser_suggestion("\\u(1100)"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("\\u(00FF)"),
                    alloc.text("."),
                ]),
                alloc.reflow(r"Learn more about working with unicode in roc at TODO"),
            ]);

            Report {
                filename,
                doc,
                title: "INVALID UNICODE CODE POINT".to_string(),
                severity,
            }
        }
        EString::UnicodeEscapeTooLarge(region) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(region.start()));

            let doc = alloc.stack([
                alloc.reflow(
                    r"I am partway through parsing a unicode code point, but I got stuck here:",
                ),
                alloc.region(region, severity),
                alloc.concat([
                    alloc.reflow(r"The unicode code point is too large to fit in a U32."),
                    alloc.reflow(r"Try using a smaller code point."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "UNICODE CODE POINT TOO LARGE".to_string(),
                severity,
            }
        }
    }
}
fn to_expr_in_parens_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::EInParens<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EInParens;
    let severity = Severity::RuntimeError;

    match *parse_problem {
        EInParens::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),
        EInParens::Expr(expr, pos) => to_expr_report(
            alloc,
            lines,
            filename,
            Context::InNode(Node::InsideParens, start),
            expr,
            pos,
        ),
        EInParens::Empty(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow("I am partway through parsing a parenthesized expression or tuple:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"I was expecting to see an expression next."),
                    alloc.reflow(r"Note, Roc doesn't use '()' as a null type."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "EMPTY PARENTHESES".to_string(),
                severity,
            }
        }
        EInParens::End(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc
                    .reflow("I am partway through parsing a record pattern, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(
                        r"I was expecting to see a closing parenthesis next, so try adding a ",
                    ),
                    alloc.parser_suggestion(")"),
                    alloc.reflow(" and see if that helps?"),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "UNFINISHED PARENTHESES".to_string(),
                severity,
            }
        }
        EInParens::Open(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(
                    r"I just started parsing an expression in parentheses, but I got stuck here:",
                ),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"An expression in parentheses looks like "),
                    alloc.parser_suggestion("(32)"),
                    alloc.reflow(r" or "),
                    alloc.parser_suggestion("(\"hello\")"),
                    alloc.reflow(" so I was expecting to see an expression next."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "UNFINISHED PARENTHESES".to_string(),
                severity,
            }
        }
    }
}

fn to_list_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::EList<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EList;

    let severity = Severity::RuntimeError;
    match *parse_problem {
        EList::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),

        EList::Expr(expr, pos) => to_expr_report(
            alloc,
            lines,
            filename,
            Context::InNode(Node::ListElement, start),
            expr,
            pos,
        ),

        EList::Open(pos) | EList::End(pos) => {
            match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
                Next::Other(Some(',')) => {
                    let surroundings = Region::new(start, pos);
                    let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                    let doc = alloc.stack([
                        alloc.reflow(
                            r"I am partway through started parsing a list, but I got stuck here:",
                        ),
                        alloc.region_with_subregion(
                            lines.convert_region(surroundings),
                            region,
                            severity,
                        ),
                        alloc.concat([
                            alloc
                                .reflow(r"I was expecting to see a list entry before this comma, "),
                            alloc.reflow(r"so try adding a list entry"),
                            alloc.reflow(r" and see if that helps?"),
                        ]),
                    ]);
                    Report {
                        filename,
                        doc,
                        title: "UNFINISHED LIST".to_string(),
                        severity,
                    }
                }
                _ => {
                    let surroundings = Region::new(start, pos);
                    let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                    let doc = alloc.stack([
                        alloc.reflow(
                            r"I am partway through started parsing a list, but I got stuck here:",
                        ),
                        alloc.region_with_subregion(
                            lines.convert_region(surroundings),
                            region,
                            severity,
                        ),
                        alloc.concat([
                            alloc.reflow(
                                r"I was expecting to see a closing square bracket before this, ",
                            ),
                            alloc.reflow(r"so try adding a "),
                            alloc.parser_suggestion("]"),
                            alloc.reflow(r" and see if that helps?"),
                        ]),
                        alloc.concat([
                            alloc.note("When "),
                            alloc.reflow(r"I get stuck like this, "),
                            alloc.reflow(r"it usually means that there is a missing parenthesis "),
                            alloc.reflow(r"or bracket somewhere earlier. "),
                            alloc.reflow(r"It could also be a stray keyword or operator."),
                        ]),
                    ]);

                    Report {
                        filename,
                        doc,
                        title: "UNFINISHED LIST".to_string(),
                        severity,
                    }
                }
            }
        }
    }
}

fn to_dbg_or_expect_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    context: Context,
    node: Node,
    parse_problem: &roc_parse::parser::EExpect<'a>,
    start: Position,
) -> Report<'a> {
    match parse_problem {
        roc_parse::parser::EExpect::Space(err, pos) => {
            to_space_report(alloc, lines, filename, err, *pos)
        }

        roc_parse::parser::EExpect::Dbg(_) => unreachable!("another branch would be taken"),
        roc_parse::parser::EExpect::Expect(_) => unreachable!("another branch would be taken"),

        roc_parse::parser::EExpect::Condition(e_expr, condition_start) => {
            // is adding context helpful here?
            to_expr_report(alloc, lines, filename, context, e_expr, *condition_start)
        }
        roc_parse::parser::EExpect::Continuation(e_expr, continuation_start) => {
            let context = Context::InNode(node, start);
            to_expr_report(alloc, lines, filename, context, e_expr, *continuation_start)
        }

        roc_parse::parser::EExpect::IndentCondition(_) => todo!(),
    }
}

fn to_import_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::EImport<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EImport::*;
    use roc_parse::parser::EImportParams;
    let severity = Severity::RuntimeError;

    match parse_problem {
        Import(_pos) => unreachable!("another branch would be taken"),
        IndentStart(pos)
        | PackageShorthand(pos)
        | PackageShorthandDot(pos)
        | ModuleName(pos)
        | IndentIngestedPath(pos)
        | IngestedPath(pos) => to_unfinished_import_report(
            alloc,
            lines,
            filename,
            *pos,
            start,
            alloc.stack([
                alloc.reflow("I was expecting to see a module name, like:"),
                alloc.parser_suggestion("import BigNum").indent(4),
                alloc.reflow("Or a package module name, like:"),
                alloc.parser_suggestion("import pf.Stdout").indent(4),
                alloc.reflow("Or a file path to ingest, like:"),
                alloc
                    .parser_suggestion("import \"users.json\" as users : Str")
                    .indent(4),
            ]),
        ),
        Params(EImportParams::Indent(pos), _)
        | IndentAs(pos)
        | As(pos)
        | IndentExposing(pos)
        | Exposing(pos)
        | EndNewline(pos) => to_unfinished_import_report(
            alloc,
            lines,
            filename,
            *pos,
            start,
            alloc.stack([
                alloc.concat([
                    alloc.reflow("I was expecting to see the "),
                    alloc.keyword("as"),
                    alloc.reflow(" keyword next, like:"),
                ]),
                alloc
                    .parser_suggestion("import svg.Path as SvgPath")
                    .indent(4),
                alloc.concat([
                    alloc.reflow("Or the "),
                    alloc.keyword("exposing"),
                    alloc.reflow(" keyword, like:"),
                ]),
                alloc
                    .parser_suggestion("import svg.Path exposing [arc, rx]")
                    .indent(4),
                alloc.reflow("Or module params, like:"),
                alloc
                    .parser_suggestion("import Menu { echo, read }")
                    .indent(4),
            ]),
        ),
        Params(EImportParams::Record(problem, pos), _) => {
            to_record_report(alloc, lines, filename, problem, *pos, start)
        }
        Params(EImportParams::RecordIgnoredFieldFound(region), _) => {
            let surroundings = Region::new(start, region.end());
            let region = lines.convert_region(*region);

            let doc = alloc.stack([
                alloc.reflow("I was partway through parsing module params, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.reflow(
                    "This is an ignored record field, but those are not allowed in module params.",
                ),
            ]);

            Report {
                filename,
                doc,
                title: "IGNORED RECORD FIELD IN MODULE PARAMS".to_string(),
                severity,
            }
        }
        Params(EImportParams::RecordUpdateFound(region), _) => {
            let surroundings = Region::new(start, region.end());
            let region = lines.convert_region(*region);

            let doc = alloc.stack([
                alloc.reflow("I was partway through parsing module params, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.reflow("It looks like you're trying to update a record, but module params require a standalone record literal."),
            ]);

            Report {
                filename,
                doc,
                title: "RECORD UPDATE IN MODULE PARAMS".to_string(),
                severity,
            }
        }
        Params(EImportParams::RecordBuilderFound(region), _) => {
            let surroundings = Region::new(start, region.end());
            let region = lines.convert_region(*region);

            let doc = alloc.stack([
                alloc.reflow("I was partway through parsing module params, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.reflow("It looks like you're trying to use a record builder, but module params require a standalone record literal."),
            ]);

            Report {
                filename,
                doc,
                title: "RECORD BUILDER IN MODULE PARAMS".to_string(),
                severity,
            }
        }
        IndentAlias(pos) | Alias(pos) => to_unfinished_import_report(
            alloc,
            lines,
            filename,
            *pos,
            start,
            alloc.concat([
                alloc.reflow("I just saw the "),
                alloc.keyword("as"),
                alloc.reflow(" keyword, so I was expecting to see an alias next."),
            ]),
        ),
        LowercaseAlias(region) => {
            let surroundings = Region::new(start, region.end());
            let region = lines.convert_region(*region);

            let doc = alloc.stack([
                alloc.reflow(r"This import is using a lowercase alias:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.reflow(r"Module names and aliases must start with an uppercase letter."),
            ]);

            Report {
                filename,
                doc,
                title: "LOWERCASE ALIAS".to_string(),
                severity,
            }
        }
        ExposingListStart(pos) => to_unfinished_import_report(
            alloc,
            lines,
            filename,
            *pos,
            start,
            alloc.concat([
                alloc.reflow("I just saw the "),
                alloc.keyword("exposing"),
                alloc.reflow(" keyword, so I was expecting to see "),
                alloc.keyword("["),
                alloc.reflow(" next."),
            ]),
        ),
        ExposedName(pos) | ExposingListEnd(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc
                    .reflow(r"I'm partway through parsing an exposing list, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.reflow(r"I was expecting a type, value, or function name next, like:"),
                alloc
                    .parser_suggestion("import Svg exposing [Path, arc, rx]")
                    .indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD EXPOSING".to_string(),
                severity,
            }
        }
        IndentIngestedName(pos) | IngestedName(pos) => to_unfinished_import_report(
            alloc,
            lines,
            filename,
            *pos,
            start,
            alloc.stack([
                alloc.reflow("I was expecting to see a name next, like:"),
                alloc
                    .parser_suggestion("import \"users.json\" as users : Str")
                    .indent(4),
            ]),
        ),
        Annotation(problem, pos) => to_type_report(alloc, lines, filename, problem, *pos),
        IndentAnnotation(pos) | IndentColon(pos) | Colon(pos) => to_unfinished_import_report(
            alloc,
            lines,
            filename,
            *pos,
            start,
            alloc.stack([
                alloc.reflow("I was expecting to see an annotation next, like:"),
                alloc
                    .parser_suggestion("import \"users.json\" as users : Str")
                    .indent(4),
            ]),
        ),
        Space(problem, pos) | Params(EImportParams::Space(problem, pos), _) => {
            to_space_report(alloc, lines, filename, problem, *pos)
        }
    }
}

fn to_unfinished_import_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    pos: Position,
    start: Position,
    message: RocDocBuilder<'a>,
) -> Report<'a> {
    let surroundings = Region::new(start, pos);
    let region = LineColumnRegion::from_pos(lines.convert_pos(pos));
    let severity = Severity::RuntimeError;

    let doc = alloc.stack([
        alloc.concat([
            alloc.reflow(r"I was partway through parsing an "),
            alloc.keyword("import"),
            alloc.reflow(r", but I got stuck here:"),
        ]),
        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
        message,
    ]);

    Report {
        filename,
        doc,
        title: "UNFINISHED IMPORT".to_string(),
        severity,
    }
}

fn to_if_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::EIf<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EIf;

    match *parse_problem {
        EIf::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),

        EIf::Condition(expr, pos) => to_expr_report(
            alloc,
            lines,
            filename,
            Context::InNode(Node::IfCondition, start),
            expr,
            pos,
        ),

        EIf::ThenBranch(expr, pos) => to_expr_report(
            alloc,
            lines,
            filename,
            Context::InNode(Node::IfThenBranch, start),
            expr,
            pos,
        ),

        EIf::ElseBranch(expr, pos) => to_expr_report(
            alloc,
            lines,
            filename,
            Context::InNode(Node::IfElseBranch, start),
            expr,
            pos,
        ),

        EIf::If(_pos) => unreachable!("another branch would be taken"),
        EIf::IndentIf(_pos) => unreachable!("another branch would be taken"),

        EIf::Then(pos) | EIf::IndentThenBranch(pos) | EIf::IndentThenToken(pos) => {
            to_unfinished_if_report(
                alloc,
                lines,
                filename,
                pos,
                start,
                alloc.concat([
                    alloc.reflow(r"I was expecting to see the "),
                    alloc.keyword("then"),
                    alloc.reflow(r" keyword next."),
                ]),
            )
        }

        EIf::Else(pos) | EIf::IndentElseBranch(pos) | EIf::IndentElseToken(pos) => {
            to_unfinished_if_report(
                alloc,
                lines,
                filename,
                pos,
                start,
                alloc.concat([
                    alloc.reflow(r"I was expecting to see the "),
                    alloc.keyword("else"),
                    alloc.reflow(r" keyword next."),
                ]),
            )
        }

        EIf::IndentCondition(pos) => to_unfinished_if_report(
            alloc,
            lines,
            filename,
            pos,
            start,
            alloc.concat([alloc.reflow(r"I was expecting to see a expression next")]),
        ),
    }
}

fn to_unfinished_if_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    pos: Position,
    start: Position,
    message: RocDocBuilder<'a>,
) -> Report<'a> {
    let surroundings = Region::new(start, pos);
    let region = LineColumnRegion::from_pos(lines.convert_pos(pos));
    let severity = Severity::RuntimeError;

    let doc = alloc.stack([
        alloc.concat([
            alloc.reflow(r"I was partway through parsing an "),
            alloc.keyword("if"),
            alloc.reflow(r" expression, but I got stuck here:"),
        ]),
        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
        message,
    ]);

    Report {
        filename,
        doc,
        title: "UNFINISHED IF".to_string(),
        severity,
    }
}

fn to_when_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::EWhen<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EWhen;
    let severity = Severity::RuntimeError;

    match *parse_problem {
        EWhen::IfGuard(nested, pos) => {
            match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
                Next::Token("->") => {
                    let surroundings = Region::new(start, pos);
                    let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                    let doc = alloc.stack([
                        alloc.reflow(
                            r"I just started parsing an if guard, but there is no guard condition:",
                        ),
                        alloc.region_with_subregion(
                            lines.convert_region(surroundings),
                            region,
                            severity,
                        ),
                        alloc.concat([alloc.reflow("Try adding an expression before the arrow!")]),
                    ]);

                    Report {
                        filename,
                        doc,
                        title: "IF GUARD NO CONDITION".to_string(),
                        severity,
                    }
                }
                _ => to_expr_report(
                    alloc,
                    lines,
                    filename,
                    Context::InNode(Node::WhenIfGuard, start),
                    nested,
                    pos,
                ),
            }
        }
        EWhen::Arrow(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.concat([
                    alloc.reflow(r"I am partway through parsing a "),
                    alloc.keyword("when"),
                    alloc.reflow(r" expression, but got stuck here:"),
                ]),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([alloc.reflow("I was expecting to see an arrow next.")]),
                note_for_when_indent_error(alloc),
            ]);

            Report {
                filename,
                doc,
                title: "MISSING ARROW".to_string(),
                severity,
            }
        }

        EWhen::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),

        EWhen::Branch(expr, pos) => to_expr_report(
            alloc,
            lines,
            filename,
            Context::InNode(Node::WhenBranch, start),
            expr,
            pos,
        ),

        EWhen::Condition(expr, pos) => to_expr_report(
            alloc,
            lines,
            filename,
            Context::InNode(Node::WhenCondition, start),
            expr,
            pos,
        ),

        EWhen::Bar(pos) => to_unfinished_when_report(
            alloc,
            lines,
            filename,
            pos,
            start,
            alloc.concat([
                alloc.reflow(r"I just saw a "),
                alloc.parser_suggestion(r"|"),
                alloc.reflow(r" so I was expecting to see a pattern next."),
            ]),
        ),

        EWhen::IfToken(_pos) => unreachable!("the if-token is optional"),
        EWhen::When(_pos) => unreachable!("another branch would be taken"),

        EWhen::Is(pos) => to_unfinished_when_report(
            alloc,
            lines,
            filename,
            pos,
            start,
            alloc.concat([
                alloc.reflow(r"I was expecting to see the "),
                alloc.keyword("is"),
                alloc.reflow(r" keyword next."),
            ]),
        ),

        EWhen::IndentCondition(pos) => to_unfinished_when_report(
            alloc,
            lines,
            filename,
            pos,
            start,
            alloc.concat([alloc.reflow(r"I was expecting to see a expression next")]),
        ),

        EWhen::IndentPattern(pos) => to_unfinished_when_report(
            alloc,
            lines,
            filename,
            pos,
            start,
            alloc.concat([alloc.reflow(r"I was expecting to see a pattern next")]),
        ),

        EWhen::IndentArrow(pos) => to_unfinished_when_report(
            alloc,
            lines,
            filename,
            pos,
            start,
            alloc.concat([
                alloc.reflow(r"I just saw a pattern, so I was expecting to see a "),
                alloc.parser_suggestion("->"),
                alloc.reflow(" next."),
            ]),
        ),

        EWhen::IndentIfGuard(pos) => to_unfinished_when_report(
            alloc,
            lines,
            filename,
            pos,
            start,
            alloc.concat([
                alloc.reflow(r"I just saw the "),
                alloc.keyword("if"),
                alloc.reflow(" keyword, so I was expecting to see an expression next."),
            ]),
        ),

        EWhen::IndentBranch(pos) => to_unfinished_when_report(
            alloc,
            lines,
            filename,
            pos,
            start,
            alloc.concat([
                alloc.reflow(r"I was expecting to see an expression next. "),
                alloc.reflow("What should I do when I run into this particular pattern?"),
            ]),
        ),

        EWhen::PatternAlignment(indent, pos) => to_unfinished_when_report(
            alloc,
            lines,
            filename,
            pos,
            start,
            alloc.concat([
                alloc.reflow(r"I suspect this is a pattern that is not indented enough? (by "),
                alloc.text(indent.to_string()),
                alloc.reflow(" spaces)"),
            ]),
        ),
        EWhen::Pattern(ref pat, pos) => to_pattern_report(alloc, lines, filename, pat, pos),
    }
}

fn to_unfinished_when_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    pos: Position,
    start: Position,
    message: RocDocBuilder<'a>,
) -> Report<'a> {
    match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
        Next::Token("->") => to_unexpected_arrow_report(alloc, lines, filename, pos, start),
        _ => {
            let severity = Severity::RuntimeError;
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.concat([
                    alloc.reflow(r"I was partway through parsing a "),
                    alloc.keyword("when"),
                    alloc.reflow(r" expression, but I got stuck here:"),
                ]),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                message,
                note_for_when_error(alloc),
            ]);

            Report {
                filename,
                doc,
                title: "UNFINISHED WHEN".to_string(),
                severity,
            }
        }
    }
}

fn to_unexpected_arrow_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    pos: Position,
    start: Position,
) -> Report<'a> {
    let surroundings = Region::new(start, pos);
    let region = Region::new(pos, pos.bump_column(2));
    let severity = Severity::RuntimeError;

    let doc = alloc.stack([
        alloc.concat([
            alloc.reflow(r"I am parsing a "),
            alloc.keyword("when"),
            alloc.reflow(r" expression right now, but this arrow is confusing me:"),
        ]),
        alloc.region_with_subregion(
            lines.convert_region(surroundings),
            lines.convert_region(region),
            severity,
        ),
        alloc.concat([
            alloc.reflow(r"It makes sense to see arrows around here, "),
            alloc.reflow(r"so I suspect it is something earlier. "),
            alloc.reflow(
                r"Maybe this pattern is indented a bit farther from the previous patterns?",
            ),
        ]),
        note_for_when_error(alloc),
    ]);

    Report {
        filename,
        doc,
        title: "UNEXPECTED ARROW".to_string(),
        severity,
    }
}

fn note_for_when_error<'a>(alloc: &'a RocDocAllocator<'a>) -> RocDocBuilder<'a> {
    alloc.stack([
        alloc.concat([
            alloc.note("Here is an example of a valid "),
            alloc.keyword("when"),
            alloc.reflow(r" expression for reference."),
        ]),
        alloc.vcat(vec![
            alloc.text("when List.first plants is").indent(4),
            alloc.text("Ok n ->").indent(6),
            alloc.text("n").indent(8),
            alloc.text(""),
            alloc.text("Err _ ->").indent(6),
            alloc.text("200").indent(8),
        ]),
        alloc.concat([
            alloc.reflow(
                "Notice the indentation. All patterns are aligned, and each branch is indented",
            ),
            alloc.reflow(" a bit more than the corresponding pattern. That is important!"),
        ]),
    ])
}

fn note_for_when_indent_error<'a>(alloc: &'a RocDocAllocator<'a>) -> RocDocBuilder<'a> {
    alloc.stack([
        alloc.concat([
            alloc.note("Sometimes I get confused by indentation, so try to make your "),
            alloc.keyword("when"),
            alloc.reflow(r" look something like this:"),
        ]),
        alloc.vcat(vec![
            alloc.text("when List.first plants is").indent(4),
            alloc.text("Ok n ->").indent(6),
            alloc.text("n").indent(8),
            alloc.text(""),
            alloc.text("Err _ ->").indent(6),
            alloc.text("200").indent(8),
        ]),
        alloc.concat([
            alloc.reflow(
                "Notice the indentation. All patterns are aligned, and each branch is indented",
            ),
            alloc.reflow(" a bit more than the corresponding pattern. That is important!"),
        ]),
    ])
}

fn to_pattern_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::EPattern<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EPattern;
    let severity = Severity::RuntimeError;

    match parse_problem {
        EPattern::Str(EString::Open(..), pos) | EPattern::Start(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I just started parsing a pattern, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.note("I may be confused by indentation"),
            ]);

            Report {
                filename,
                doc,
                title: "UNFINISHED PATTERN".to_string(),
                severity,
            }
        }
        EPattern::Str(err, pos) => to_str_report(alloc, lines, filename, err, *pos),
        EPattern::Record(record, pos) => to_precord_report(alloc, lines, filename, record, *pos),
        EPattern::List(list, pos) => to_plist_report(alloc, lines, filename, list, *pos),
        EPattern::PInParens(inparens, pos) => {
            to_pattern_in_parens_report(alloc, lines, filename, inparens, *pos)
        }
        &EPattern::NumLiteral(ENumber::End, pos) => {
            to_malformed_number_literal_report(alloc, lines, filename, pos)
        }
        // If you're adding or changing syntax, please handle the case with a
        // good error message above instead of adding more unhandled cases below.
        EPattern::AsKeyword(pos)
        | EPattern::AsIdentifier(pos)
        | EPattern::Underscore(pos)
        | EPattern::NotAPattern(pos)
        | EPattern::End(pos)
        | EPattern::Space(_, pos)
        | EPattern::IndentStart(pos)
        | EPattern::IndentEnd(pos)
        | EPattern::AsIndentStart(pos)
        | EPattern::AccessorFunction(pos)
        | EPattern::RecordUpdaterFunction(pos)
        | EPattern::ParenStart(pos)
        | EPattern::ParenEnd(pos) => to_unhandled_parse_error_report(
            alloc,
            lines,
            filename,
            format!("{:?}", parse_problem),
            *pos,
            start,
        ),
    }
}

fn to_precord_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::PRecord<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::PRecord;

    let severity = Severity::RuntimeError;

    match *parse_problem {
        PRecord::Open(pos) => match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
            Next::Keyword(keyword) => {
                let surroundings = Region::new(start, pos);
                let region = to_keyword_region(lines.convert_pos(pos), keyword);

                let doc = alloc.stack([
                    alloc.reflow(r"I just started parsing a record pattern, but I got stuck on this field name:"),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([
                        alloc.reflow(r"Looks like you are trying to use "),
                        alloc.keyword(keyword),
                        alloc.reflow(" as a field name, but that is a reserved word. Try using a different name!"),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "UNFINISHED RECORD PATTERN".to_string(),
                    severity,
                }
            }
            _ => {
                let surroundings = Region::new(start, pos);
                let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                let doc = alloc.stack([
                    alloc.reflow(r"I just started parsing a record pattern, but I got stuck here:"),
                    alloc.region_with_subregion(
                        lines.convert_region(surroundings),
                        region,
                        severity,
                    ),
                    record_patterns_look_like(alloc),
                ]);

                Report {
                    filename,
                    doc,
                    title: "UNFINISHED RECORD PATTERN".to_string(),
                    severity,
                }
            }
        },

        PRecord::End(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
                Next::Other(Some(c)) if c.is_alphabetic() => {
                    let doc = alloc.stack([
                        alloc.reflow(r"I am partway through parsing a record pattern, but I got stuck here:"),
                        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                        alloc.concat([
                            alloc.reflow(
                                r"I was expecting to see a colon, question mark, comma or closing curly brace.",
                            ),
                        ]),
                    ]);

                    Report {
                        filename,
                        doc,
                        title: "UNFINISHED RECORD PATTERN".to_string(),
                        severity,
                    }
                }
                _ => {
                    let doc = alloc.stack([
                alloc.reflow("I am partway through parsing a record pattern, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(
                        r"I was expecting to see a closing curly brace before this, so try adding a ",
                    ),
                    alloc.parser_suggestion("}"),
                    alloc.reflow(" and see if that helps?"),
                ]),
            ]);

                    Report {
                        filename,
                        doc,
                        title: "UNFINISHED RECORD PATTERN".to_string(),
                        severity,
                    }
                }
            }
        }

        PRecord::Field(pos) => match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
            Next::Keyword(keyword) => {
                let surroundings = Region::new(start, pos);
                let region = to_keyword_region(lines.convert_pos(pos), keyword);

                let doc = alloc.stack([
                    alloc.reflow(r"I just started parsing a record pattern, but I got stuck on this field name:"),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([
                        alloc.reflow(r"Looks like you are trying to use "),
                        alloc.keyword(keyword),
                        alloc.reflow(" as a field name, but that is a reserved word. Try using a different name!"),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "UNFINISHED RECORD PATTERN".to_string(),
                    severity,
                }
            }
            Next::Other(Some(',')) => todo!(),
            Next::Other(Some('}')) => unreachable!("or is it?"),
            _ => {
                let surroundings = Region::new(start, pos);
                let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                let doc = alloc.stack([
                    alloc.reflow(r"I am partway through parsing a record pattern, but I got stuck here:"),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([
                        alloc.reflow(r"I was expecting to see another record field defined next, so I am looking for a name like "),
                        alloc.parser_suggestion("userName"),
                        alloc.reflow(" or "),
                        alloc.parser_suggestion("plantHight"),
                        alloc.reflow("."),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "PROBLEM IN RECORD PATTERN".to_string(),
                    severity,
                }
            }
        },

        PRecord::Colon(_) => {
            unreachable!("because `foo` is a valid field; the colon is not required")
        }
        PRecord::Optional(_) => {
            unreachable!("because `foo` is a valid field; the question mark is not required")
        }

        PRecord::Pattern(pattern, pos) => to_pattern_report(alloc, lines, filename, pattern, pos),

        PRecord::Expr(expr, pos) => to_expr_report(
            alloc,
            lines,
            filename,
            Context::InNode(Node::RecordConditionalDefault, start),
            expr,
            pos,
        ),

        PRecord::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),
    }
}

fn to_plist_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &PList<'a>,
    start: Position,
) -> Report<'a> {
    let severity = Severity::RuntimeError;
    match *parse_problem {
        PList::Open(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I just started parsing a list pattern, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                list_patterns_look_like(alloc),
            ]);

            Report {
                filename,
                doc,
                title: "UNFINISHED LIST PATTERN".to_string(),
                severity,
            }
        }

        PList::End(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));
            let doc = alloc.stack([
                alloc.reflow("I am partway through parsing a list pattern, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(
                        r"I was expecting to see a closing square brace before this, so try adding a ",
                    ),
                    alloc.parser_suggestion("]"),
                    alloc.reflow(" and see if that helps?"),
                ])]);

            Report {
                filename,
                doc,
                title: "UNFINISHED LIST PATTERN".to_string(),
                severity,
            }
        }

        PList::Rest(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));
            let doc = alloc.stack([
                alloc.reflow("It looks like you may trying to write a list rest pattern, but it's not the form I expect:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(
                        r"List rest patterns, which match zero or more elements in a list, are denoted with ",
                    ),
                    alloc.parser_suggestion(".."),
                    alloc.reflow(" - is that what you meant?"),
                ])]);

            Report {
                filename,
                doc,
                title: "INCORRECT REST PATTERN".to_string(),
                severity,
            }
        }

        PList::Pattern(pattern, pos) => to_pattern_report(alloc, lines, filename, pattern, pos),

        PList::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),
    }
}

fn to_pattern_in_parens_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::PInParens<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::PInParens;

    let severity = Severity::RuntimeError;

    match *parse_problem {
        PInParens::Open(pos) => {
            // `Open` case is for exhaustiveness, this case shouldn't not be reachable practically.
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(
                    r"I just started parsing a pattern in parentheses, but I got stuck here:",
                ),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"A pattern in parentheses looks like "),
                    alloc.parser_suggestion("(Ok 32)"),
                    alloc.reflow(r" or "),
                    alloc.parser_suggestion("(\"hello\")"),
                    alloc.reflow(" so I was expecting to see an expression next."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "UNFINISHED PARENTHESES".to_string(),
                severity,
            }
        }

        PInParens::Empty(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));
            let severity = Severity::RuntimeError;

            let doc = alloc.stack([
                alloc.reflow("I am partway through parsing a parenthesized pattern or tuple:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"I was expecting to see a pattern next."),
                    alloc.reflow(r"Note, Roc doesn't use '()' as a null type."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "EMPTY PARENTHESES".to_string(),
                severity,
            }
        }

        PInParens::End(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow("I am partway through parsing a pattern in parentheses, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(
                        r"I was expecting to see a closing parenthesis before this, so try adding a ",
                    ),
                    alloc.parser_suggestion(")"),
                    alloc.reflow(" and see if that helps?"),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "UNFINISHED PARENTHESES".to_string(),
                severity,
            }
        }

        PInParens::Pattern(pattern, pos) => to_pattern_report(alloc, lines, filename, pattern, pos),

        PInParens::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),
    }
}

fn to_malformed_number_literal_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    start: Position,
) -> Report<'a> {
    let surroundings = Region::new(start, start);
    let region = LineColumnRegion::from_pos(lines.convert_pos(start));
    let severity = Severity::RuntimeError;

    let doc = alloc.stack([
        alloc.reflow(r"This number literal is malformed:"),
        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
    ]);

    Report {
        filename,
        doc,
        title: "INVALID NUMBER LITERAL".to_string(),
        severity,
    }
}

fn to_type_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::EType<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EType;
    let severity = Severity::RuntimeError;

    match parse_problem {
        EType::TRecord(record, pos) => to_trecord_report(alloc, lines, filename, record, *pos),
        EType::TTagUnion(tag_union, pos) => {
            to_ttag_union_report(alloc, lines, filename, tag_union, *pos)
        }
        EType::TInParens(tinparens, pos) => {
            to_tinparens_report(alloc, lines, filename, tinparens, *pos)
        }
        EType::TApply(tapply, pos) => to_tapply_report(alloc, lines, filename, tapply, *pos),
        EType::TInlineAlias(talias, _) => to_talias_report(alloc, lines, filename, talias),

        EType::TFunctionArgument(pos) => {
            match what_is_next(alloc.src_lines, lines.convert_pos(*pos)) {
                Next::Other(Some(',')) => {
                    let surroundings = Region::new(start, *pos);
                    let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

                    let doc = alloc.stack([
                    alloc.reflow(r"I just started parsing a function argument type, but I encountered two commas in a row:"),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([alloc.reflow("Try removing one of them.")]),
                ]);

                    Report {
                        filename,
                        doc,
                        title: "DOUBLE COMMA".to_string(),
                        severity,
                    }
                }
                Next::Other(_) | Next::Keyword(_) | Next::Close(_, _) | Next::Token(_) => todo!(),
            }
        }

        EType::TStart(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I just started parsing a type, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"I am expecting a type next, like "),
                    alloc.parser_suggestion("Bool"),
                    alloc.reflow(r" or "),
                    alloc.parser_suggestion("List a"),
                    alloc.reflow("."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "UNFINISHED TYPE".to_string(),
                severity,
            }
        }

        EType::TIndentStart(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I just started parsing a type, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.note("I may be confused by indentation"),
            ]);

            Report {
                filename,
                doc,
                title: "UNFINISHED TYPE".to_string(),
                severity,
            }
        }

        EType::TIndentEnd(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a type, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.note("I may be confused by indentation"),
            ]);

            Report {
                filename,
                doc,
                title: "UNFINISHED TYPE".to_string(),
                severity,
            }
        }

        EType::TAsIndentStart(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I just started parsing an inline type alias, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.note("I may be confused by indentation"),
            ]);

            Report {
                filename,
                doc,
                title: "UNFINISHED INLINE ALIAS".to_string(),
                severity,
            }
        }

        EType::TBadTypeVariable(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am expecting a type variable, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
            ]);

            Report {
                filename,
                doc,
                title: "BAD TYPE VARIABLE".to_string(),
                severity,
            }
        }
        // If you're adding or changing syntax, please handle the case with a
        // good error message above instead of adding more unhandled cases below.
        EType::Space(_, pos)
        | EType::UnderscoreSpacing(pos)
        | EType::TWildcard(pos)
        | EType::TInferred(pos)
        | EType::TEnd(pos)
        | EType::TWhereBar(pos)
        | EType::TImplementsClause(pos)
        | EType::TAbilityImpl(_, pos) => to_unhandled_parse_error_report(
            alloc,
            lines,
            filename,
            format!("{:?}", parse_problem),
            *pos,
            start,
        ),
    }
}

fn to_trecord_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::ETypeRecord<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::ETypeRecord;

    let severity = Severity::RuntimeError;

    match *parse_problem {
        ETypeRecord::Open(pos) => match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
            Next::Keyword(keyword) => {
                let surroundings = Region::new(start, pos);
                let region = to_keyword_region(lines.convert_pos(pos), keyword);

                let doc = alloc.stack([
                    alloc.reflow(r"I just started parsing a record type, but I got stuck on this field name:"),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([
                        alloc.reflow(r"Looks like you are trying to use "),
                        alloc.keyword(keyword),
                        alloc.reflow(" as a field name, but that is a reserved word. Try using a different name!"),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "UNFINISHED RECORD TYPE".to_string(),
                    severity,
                }
            }
            _ => {
                let surroundings = Region::new(start, pos);
                let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                let doc = alloc.stack([
                    alloc.reflow(r"I just started parsing a record type, but I got stuck here:"),
                    alloc.region_with_subregion(
                        lines.convert_region(surroundings),
                        region,
                        severity,
                    ),
                    alloc.concat([
                        alloc.reflow(r"Record types look like "),
                        alloc.parser_suggestion("{ name : String, age : Int },"),
                        alloc.reflow(" so I was expecting to see a field name next."),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "UNFINISHED RECORD TYPE".to_string(),
                    severity,
                }
            }
        },

        ETypeRecord::End(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
                Next::Other(Some(c)) if c.is_alphabetic() => {
                    let doc = alloc.stack([
                        alloc.reflow(r"I am partway through parsing a record type, but I got stuck here:"),
                        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                        alloc.concat([
                            alloc.reflow(
                                r"I was expecting to see a colon, two question marks (??), comma or closing curly brace.",
                            ),
                        ]),
                    ]);

                    Report {
                        filename,
                        doc,
                        title: "UNFINISHED RECORD TYPE".to_string(),
                        severity,
                    }
                }
                _ => {
                    let doc = alloc.stack([
                alloc.reflow("I am partway through parsing a record type, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(
                        r"I was expecting to see a closing curly brace before this, so try adding a ",
                    ),
                    alloc.parser_suggestion("}"),
                    alloc.reflow(" and see if that helps?"),
                ]),
            ]);

                    Report {
                        filename,
                        doc,
                        title: "UNFINISHED RECORD TYPE".to_string(),
                        severity,
                    }
                }
            }
        }

        ETypeRecord::Field(pos) => match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
            Next::Keyword(keyword) => {
                let surroundings = Region::new(start, pos);
                let region = to_keyword_region(lines.convert_pos(pos), keyword);

                let doc = alloc.stack([
                    alloc.reflow(r"I just started parsing a record type, but I got stuck on this field name:"),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([
                        alloc.reflow(r"Looks like you are trying to use "),
                        alloc.keyword(keyword),
                        alloc.reflow(" as a field name, but that is a reserved word. Try using a different name!"),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "UNFINISHED RECORD TYPE".to_string(),
                    severity,
                }
            }
            Next::Other(Some(',')) => todo!(),
            Next::Other(Some('}')) => unreachable!("or is it?"),
            _ => {
                let surroundings = Region::new(start, pos);
                let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                let doc = alloc.stack([
                    alloc.reflow(r"I am partway through parsing a record type, but I got stuck here:"),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([
                        alloc.reflow(r"I was expecting to see another record field defined next, so I am looking for a name like "),
                        alloc.parser_suggestion("userName"),
                        alloc.reflow(" or "),
                        alloc.parser_suggestion("plantHight"),
                        alloc.reflow("."),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "PROBLEM IN RECORD TYPE".to_string(),
                    severity,
                }
            }
        },

        ETypeRecord::Colon(_) => {
            unreachable!("because `foo` is a valid field; the colon is not required")
        }
        ETypeRecord::Optional(_) => {
            unreachable!("because `foo` is a valid field; the question mark is not required")
        }

        ETypeRecord::Type(tipe, pos) => to_type_report(alloc, lines, filename, tipe, pos),

        ETypeRecord::IndentOpen(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I just started parsing a record type, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"Record types look like "),
                    alloc.parser_suggestion("{ name : String, age : Int },"),
                    alloc.reflow(" so I was expecting to see a field name next."),
                ]),
                note_for_record_type_indent(alloc),
            ]);

            Report {
                filename,
                doc,
                title: "UNFINISHED RECORD TYPE".to_string(),
                severity,
            }
        }

        ETypeRecord::IndentEnd(pos) => {
            match next_line_starts_with_close_curly(alloc.src_lines, lines.convert_pos(pos)) {
                Some(curly_pos) => {
                    let surroundings = LineColumnRegion::new(lines.convert_pos(start), curly_pos);
                    let region = LineColumnRegion::from_pos(curly_pos);

                    let doc = alloc.stack([
                        alloc.reflow(
                            "I am partway through parsing a record type, but I got stuck here:",
                        ),
                        alloc.region_with_subregion(surroundings, region, severity),
                        alloc.concat([
                            alloc.reflow("I need this curly brace to be indented more. Try adding more spaces before it!"),
                        ]),
                    ]);

                    Report {
                        filename,
                        doc,
                        title: "NEED MORE INDENTATION".to_string(),
                        severity,
                    }
                }
                None => {
                    let surroundings = Region::new(start, pos);
                    let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                    let doc = alloc.stack([
                        alloc.reflow(
                            r"I am partway through parsing a record type, but I got stuck here:",
                        ),
                        alloc.region_with_subregion(
                            lines.convert_region(surroundings),
                            region,
                            severity,
                        ),
                        alloc.concat([
                            alloc.reflow("I was expecting to see a closing curly "),
                            alloc.reflow("brace before this, so try adding a "),
                            alloc.parser_suggestion("}"),
                            alloc.reflow(" and see if that helps?"),
                        ]),
                        note_for_record_type_indent(alloc),
                    ]);

                    Report {
                        filename,
                        doc,
                        title: "UNFINISHED RECORD TYPE".to_string(),
                        severity,
                    }
                }
            }
        }

        ETypeRecord::IndentColon(_) => {
            unreachable!("because `foo` is a valid field; the colon is not required")
        }

        ETypeRecord::IndentOptional(_) => {
            unreachable!("because `foo` is a valid field; the question mark is not required")
        }

        ETypeRecord::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),
    }
}

fn to_ttag_union_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::ETypeTagUnion<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::ETypeTagUnion;

    let severity = Severity::RuntimeError;

    match *parse_problem {
        ETypeTagUnion::Open(pos) => match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
            Next::Keyword(keyword) => {
                let surroundings = Region::new(start, pos);
                let region = to_keyword_region(lines.convert_pos(pos), keyword);

                let doc = alloc.stack([
                    alloc.reflow(r"I just started parsing a tag union, but I got stuck on this field name:"),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([
                        alloc.reflow(r"Looks like you are trying to use "),
                        alloc.keyword(keyword),
                        alloc.reflow(" as a tag name, but that is a reserved word. Tag names must start with a uppercase letter."),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "UNFINISHED TAG UNION TYPE".to_string(),
                    severity,
                }
            }
            Next::Other(Some(c)) if c.is_alphabetic() => {
                debug_assert!(c.is_lowercase());

                let surroundings = Region::new(start, pos);
                let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                let doc = alloc.stack([
                    alloc.reflow(
                        r"I am partway through parsing a tag union type, but I got stuck here:",
                    ),
                    alloc.region_with_subregion(
                        lines.convert_region(surroundings),
                        region,
                        severity,
                    ),
                    alloc.reflow(r"I was expecting to see a tag name."),
                    hint_for_tag_name(alloc),
                ]);

                Report {
                    filename,
                    doc,
                    title: "WEIRD TAG NAME".to_string(),
                    severity,
                }
            }
            _ => {
                let surroundings = Region::new(start, pos);
                let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                let doc = alloc.stack([
                    alloc.reflow(r"I just started parsing a tag union type, but I got stuck here:"),
                    alloc.region_with_subregion(
                        lines.convert_region(surroundings),
                        region,
                        severity,
                    ),
                    alloc.concat([
                        alloc.reflow(r"Tag unions look like "),
                        alloc.parser_suggestion("[Many I64, None],"),
                        alloc.reflow(" so I was expecting to see a tag name next."),
                    ]),
                ]);

                Report {
                    filename,
                    doc,
                    title: "UNFINISHED TAG UNION TYPE".to_string(),
                    severity,
                }
            }
        },

        ETypeTagUnion::End(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
                Next::Other(Some(c)) if c.is_alphabetic() => {
                    debug_assert!(c.is_lowercase());

                    let doc = alloc.stack([
                        alloc.reflow(
                            r"I am partway through parsing a tag union type, but I got stuck here:",
                        ),
                        alloc.region_with_subregion(
                            lines.convert_region(surroundings),
                            region,
                            severity,
                        ),
                        alloc.reflow(r"I was expecting to see a tag name."),
                        hint_for_tag_name(alloc),
                    ]);

                    Report {
                        filename,
                        doc,
                        title: "WEIRD TAG NAME".to_string(),
                        severity,
                    }
                }
                _ => {
                    let doc = alloc.stack([
                        alloc.reflow(r"I am partway through parsing a tag union type, but I got stuck here:"),
                        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                        alloc.concat([
                                alloc.reflow(
                                    r"I was expecting to see a closing square bracket before this, so try adding a ",
                                ),
                                alloc.parser_suggestion("]"),
                                alloc.reflow(" and see if that helps?"),
                            ]),
                        ]);

                    Report {
                        filename,
                        doc,
                        title: "UNFINISHED TAG UNION TYPE".to_string(),
                        severity,
                    }
                }
            }
        }

        ETypeTagUnion::Type(tipe, pos) => to_type_report(alloc, lines, filename, tipe, pos),

        ETypeTagUnion::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),
    }
}

fn to_tinparens_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::ETypeInParens<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::ETypeInParens;

    let severity = Severity::RuntimeError;

    match *parse_problem {
        ETypeInParens::Open(pos) => {
            match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
                Next::Keyword(keyword) => {
                    let surroundings = Region::new(start, pos);
                    let region = to_keyword_region(lines.convert_pos(pos), keyword);

                    let doc = alloc.stack([
                    alloc.reflow(r"I just saw an open parenthesis, so I was expecting to see a type next."),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.concat([
                        alloc.reflow(r"Something like "),
                        alloc.parser_suggestion("(List Person)"),
                        alloc.text(" or "),
                        alloc.parser_suggestion("(Result I64 Str)"),
                    ]),
                ]);

                    Report {
                        filename,
                        doc,
                        title: "UNFINISHED PARENTHESES".to_string(),
                        severity,
                    }
                }
                Next::Other(Some(c)) if c.is_alphabetic() => {
                    debug_assert!(c.is_lowercase());

                    let surroundings = Region::new(start, pos);
                    let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                    let doc = alloc.stack([
                    alloc.reflow(
                        r"I am partway through parsing a type in parentheses, but I got stuck here:",
                    ),
                    alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                    alloc.reflow(r"I was expecting to see a tag name."),
                    hint_for_tag_name(alloc),
                ]);

                    Report {
                        filename,
                        doc,
                        title: "WEIRD TAG NAME".to_string(),
                        severity,
                    }
                }
                _ => {
                    let surroundings = Region::new(start, pos);
                    let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                    let doc = alloc.stack([
                        alloc.reflow(
                            r"I just started parsing a type in parentheses, but I got stuck here:",
                        ),
                        alloc.region_with_subregion(
                            lines.convert_region(surroundings),
                            region,
                            severity,
                        ),
                        alloc.concat([
                            alloc.reflow(r"Tag unions look like "),
                            alloc.parser_suggestion("[Many I64, None],"),
                            alloc.reflow(" so I was expecting to see a tag name next."),
                        ]),
                    ]);

                    Report {
                        filename,
                        doc,
                        title: "UNFINISHED PARENTHESES".to_string(),
                        severity,
                    }
                }
            }
        }

        ETypeInParens::Empty(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow("I am partway through parsing a parenthesized type:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"I was expecting to see an expression next."),
                    alloc.reflow(r"Note, Roc doesn't use '()' as a null type."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "EMPTY PARENTHESES".to_string(),
                severity,
            }
        }

        ETypeInParens::End(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
                Next::Other(Some(c)) if c.is_alphabetic() => {
                    debug_assert!(c.is_lowercase());

                    // TODO hint for tuples?
                    let doc = alloc.stack([
                        alloc.reflow(
                            r"I am partway through parsing a type in parentheses, but I got stuck here:",
                        ),
                        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                        alloc.reflow(r"I was expecting to see a tag name."),
                        hint_for_tag_name(alloc),
                    ]);

                    Report {
                        filename,
                        doc,
                        title: "WEIRD TAG NAME".to_string(),
                        severity,
                    }
                }
                _ => {
                    let doc = alloc.stack([
                        alloc.reflow(r"I am partway through parsing a type in parentheses, but I got stuck here:"),
                        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                        alloc.concat([
                                alloc.reflow(
                                    r"I was expecting to see a closing parenthesis before this, so try adding a ",
                                ),
                                alloc.parser_suggestion(")"),
                                alloc.reflow(" and see if that helps?"),
                            ]),
                        ]);

                    Report {
                        filename,
                        doc,
                        title: "UNFINISHED PARENTHESES".to_string(),
                        severity,
                    }
                }
            }
        }

        ETypeInParens::Type(tipe, pos) => to_type_report(alloc, lines, filename, tipe, pos),

        ETypeInParens::IndentOpen(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc
                    .reflow(r"I just started parsing a type in parentheses, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"Tag unions look like "),
                    alloc.parser_suggestion("[Many I64, None],"),
                    alloc.reflow(" so I was expecting to see a tag name next."),
                ]),
                note_for_tag_union_type_indent(alloc),
            ]);

            Report {
                filename,
                doc,
                title: "UNFINISHED PARENTHESES".to_string(),
                severity,
            }
        }

        ETypeInParens::IndentEnd(pos) => {
            match next_line_starts_with_close_parenthesis(alloc.src_lines, lines.convert_pos(pos)) {
                Some(curly_pos) => {
                    let surroundings = LineColumnRegion::new(lines.convert_pos(start), curly_pos);
                    let region = LineColumnRegion::from_pos(curly_pos);

                    let doc = alloc.stack([
                        alloc.reflow(
                            "I am partway through parsing a type in parentheses, but I got stuck here:",
                        ),
                        alloc.region_with_subregion(surroundings, region, severity),
                        alloc.concat([
                            alloc.reflow("I need this parenthesis to be indented more. Try adding more spaces before it!"),
                        ]),
                    ]);

                    Report {
                        filename,
                        doc,
                        title: "NEED MORE INDENTATION".to_string(),
                        severity,
                    }
                }
                None => {
                    let surroundings = Region::new(start, pos);
                    let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

                    let doc = alloc.stack([
                        alloc.reflow(
                            r"I am partway through parsing a type in parentheses, but I got stuck here:",
                        ),
                        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                        alloc.concat([
                            alloc.reflow("I was expecting to see a parenthesis "),
                            alloc.reflow("before this, so try adding a "),
                            alloc.parser_suggestion(")"),
                            alloc.reflow(" and see if that helps?"),
                        ]),
                        note_for_tag_union_type_indent(alloc),
                    ]);

                    Report {
                        filename,
                        doc,
                        title: "UNFINISHED PARENTHESES".to_string(),
                        severity,
                    }
                }
            }
        }

        ETypeInParens::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),
    }
}

fn to_tapply_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::ETypeApply,
    _start: Position,
) -> Report<'a> {
    use roc_parse::parser::ETypeApply;
    let severity = Severity::RuntimeError;

    match *parse_problem {
        ETypeApply::DoubleDot(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I encountered two dots in a row:"),
                alloc.region(region, severity),
                alloc.concat([alloc.reflow("Try removing one of them.")]),
            ]);

            Report {
                filename,
                doc,
                title: "DOUBLE DOT".to_string(),
                severity,
            }
        }
        ETypeApply::TrailingDot(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I encountered a dot with nothing after it:"),
                alloc.region(region, severity),
                alloc.concat([
                    alloc.reflow("Dots are used to refer to a type in a qualified way, like "),
                    alloc.parser_suggestion("Num.I64"),
                    alloc.text(" or "),
                    alloc.parser_suggestion("List.List a"),
                    alloc.reflow(". Try adding a type name next."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "TRAILING DOT".to_string(),
                severity,
            }
        }
        ETypeApply::StartIsNumber(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I encountered a number at the start of a qualified name segment:"),
                alloc.region(region, severity),
                alloc.concat([
                    alloc.reflow("All parts of a qualified type name must start with an uppercase letter, like "),
                    alloc.parser_suggestion("Num.I64"),
                    alloc.text(" or "),
                    alloc.parser_suggestion("List.List a"),
                    alloc.text("."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD QUALIFIED NAME".to_string(),
                severity,
            }
        }
        ETypeApply::StartNotUppercase(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I encountered a lowercase letter at the start of a qualified name segment:"),
                alloc.region(region, severity),
                alloc.concat([
                    alloc.reflow("All parts of a qualified type name must start with an uppercase letter, like "),
                    alloc.parser_suggestion("Num.I64"),
                    alloc.text(" or "),
                    alloc.parser_suggestion("List.List a"),
                    alloc.text("."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD QUALIFIED NAME".to_string(),
                severity,
            }
        }

        ETypeApply::End(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(
                    r"I reached the end of the input file while parsing a qualified type name",
                ),
                alloc.region(region, severity),
            ]);

            Report {
                filename,
                doc,
                title: "END OF FILE".to_string(),
                severity,
            }
        }

        ETypeApply::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),
    }
}

fn to_talias_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::ETypeInlineAlias,
) -> Report<'a> {
    use roc_parse::parser::ETypeInlineAlias;
    let severity = Severity::RuntimeError;

    match *parse_problem {
        ETypeInlineAlias::NotAnAlias(pos) => {
            let region = Region::from_pos(pos);

            let doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("The inline type after this "),
                    alloc.keyword("as"),
                    alloc.reflow(" is not a type alias:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("Inline alias types must start with an uppercase identifier and be followed by zero or more type arguments, like "),
                    alloc.type_str("Point"),
                    alloc.reflow(" or "),
                    alloc.type_str("List a"),
                    alloc.reflow("."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "NOT AN INLINE ALIAS".to_string(),
                severity,
            }
        }
        ETypeInlineAlias::Qualified(pos) => {
            let region = Region::from_pos(pos);

            let doc = alloc.stack([
                alloc.reflow(r"This type alias has a qualified name:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("An alias introduces a new name to the current scope, so it must be unqualified."),
            ]);

            Report {
                filename,
                doc,
                title: "QUALIFIED ALIAS NAME".to_string(),
                severity,
            }
        }
        ETypeInlineAlias::ArgumentNotLowercase(pos) => {
            let region = Region::from_pos(pos);

            let doc = alloc.stack([
                alloc.reflow(r"This alias type argument is not lowercase:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("All type arguments must be lowercase."),
            ]);

            Report {
                filename,
                doc,
                title: "TYPE ARGUMENT NOT LOWERCASE".to_string(),
                severity,
            }
        }
    }
}

fn to_header_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::EHeader<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EHeader;

    let severity = Severity::RuntimeError;

    match parse_problem {
        EHeader::Provides(provides, pos) => {
            to_provides_report(alloc, lines, filename, provides, *pos)
        }

        EHeader::Params(params, pos) => to_params_report(alloc, lines, filename, params, *pos),

        EHeader::Exposes(exposes, pos) => to_exposes_report(alloc, lines, filename, exposes, *pos),

        EHeader::Imports(imports, pos) => to_imports_report(alloc, lines, filename, imports, *pos),

        EHeader::Requires(requires, pos) => {
            to_requires_report(alloc, lines, filename, requires, *pos)
        }

        EHeader::Packages(packages, pos) => {
            to_packages_report(alloc, lines, filename, packages, *pos)
        }

        EHeader::IndentStart(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([alloc.reflow("I may be confused by indentation.")]),
            ]);

            Report {
                filename,
                doc,
                title: "INCOMPLETE HEADER".to_string(),
                severity,
            }
        }

        EHeader::Start(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let is_utf8 = alloc
                .src_lines
                .iter()
                .all(|line| std::str::from_utf8(line.as_bytes()).is_ok());

            let preamble = if is_utf8 {
                vec![
                    alloc.reflow(r"I am expecting a header, but got stuck here:"),
                    alloc.region_with_subregion(
                        lines.convert_region(surroundings),
                        region,
                        severity,
                    ),
                ]
            } else {
                vec![alloc.reflow(r"I am expecting a header, but the file is not UTF-8 encoded.")]
            };

            let doc = alloc.stack(preamble.into_iter().chain([alloc.concat([
                alloc.reflow("I am expecting a module keyword next, one of "),
                alloc.keyword("interface"),
                alloc.reflow(", "),
                alloc.keyword("app"),
                alloc.reflow(", "),
                alloc.keyword("package"),
                alloc.reflow(" or "),
                alloc.keyword("platform"),
                alloc.reflow("."),
            ])]));

            Report {
                filename,
                doc,
                title: "MISSING HEADER".to_string(),
                severity,
            }
        }

        EHeader::ModuleName(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting a module name next, like "),
                    alloc.parser_suggestion("BigNum"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("Main"),
                    alloc.reflow(". Module names must start with an uppercase letter."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD MODULE NAME".to_string(),
                severity,
            }
        }

        EHeader::InconsistentModuleName(region) => {
            let doc = alloc.stack([
                alloc.reflow(
                    r"This module name does not correspond with the file path it is defined in:",
                ),
                alloc.region(lines.convert_region(*region), severity),
                alloc.concat([
                    alloc.reflow("Module names must correspond with the file paths they are defined in. For example, I expect to see "),
                    alloc.parser_suggestion("BigNum"),
                    alloc.reflow(" defined in "),
                    alloc.parser_suggestion("BigNum.roc"),
                    alloc.reflow(", or "),
                    alloc.parser_suggestion("Math.Sin"),
                    alloc.reflow(" defined in "),
                    alloc.parser_suggestion("Math/Sin.roc"),
                    alloc.reflow("."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD MODULE NAME".to_string(),
                severity,
            }
        }

        EHeader::AppName(_, pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting an application name next, like "),
                    alloc.parser_suggestion("app \"main\""),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("app \"editor\""),
                    alloc.reflow(". App names are surrounded by quotation marks."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD APP NAME".to_string(),
                severity,
            }
        }

        EHeader::PackageName(_, pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a package header, but got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting a package name next, like "),
                    alloc.parser_suggestion("\"roc/core\""),
                    alloc.reflow(". Package names must be quoted."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "INVALID PACKAGE NAME".to_string(),
                severity,
            }
        }

        EHeader::PlatformName(_, pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc
                    .reflow(r"I am partway through parsing a platform header, but got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting a platform name next, like "),
                    alloc.parser_suggestion("\"roc/core\""),
                    alloc.reflow(". Platform names must be quoted."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "INVALID PLATFORM NAME".to_string(),
                severity,
            }
        }

        EHeader::Space(error, pos) => to_space_report(alloc, lines, filename, error, *pos),
    }
}

fn to_provides_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::EProvides,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EProvides;

    let severity = Severity::RuntimeError;

    match *parse_problem {
        EProvides::ListEnd(pos) | // TODO: give this its own error message
        EProvides::Identifier(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc
                    .reflow(r"I am partway through parsing a provides list, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([alloc.reflow(
                    "I was expecting a type name, value name or function name next, like",
                )]),
                alloc
                    .parser_suggestion("provides [Animal, default, tame]")
                    .indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD PROVIDES".to_string(),
                severity,
            }
        }

        EProvides::Provides(pos) | EProvides::IndentProvides(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting the "),
                    alloc.keyword("provides"),
                    alloc.reflow(" keyword next, like"),
                ]),
                alloc
                    .parser_suggestion("provides [Animal, default, tame]")
                    .indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD PROVIDES".to_string(),
                severity,
            }
        }

        EProvides::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),

        EProvides::IndentTo(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting the "),
                    alloc.keyword("to"),
                    alloc.reflow(" keyword next, like:"),
                ]),
                alloc
                    .parser_suggestion("to pf")
                    .indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD PROVIDES".to_string(),
                severity,
            }
        }

        EProvides::IndentListStart(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.reflow("I am expecting the platform name next, like:"),
                alloc
                    .parser_suggestion("to pf")
                    .indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD PROVIDES".to_string(),
                severity,
            }
        }
        // If you're adding or changing syntax, please handle the case with a
        // good error message above instead of adding more unhandled cases below.
        EProvides::Open(pos) |
        EProvides::To(pos) |
        EProvides::IndentPackage(pos) |
        EProvides::ListStart(pos) |
        EProvides::Package(_, pos) =>
            to_unhandled_parse_error_report(alloc, lines, filename, format!("{:?}", parse_problem), pos, start),
    }
}

fn to_params_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::EParams<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EParams;

    let severity = Severity::RuntimeError;

    match parse_problem {
        EParams::Pattern(error, pos) => to_precord_report(alloc, lines, filename, error, *pos),

        EParams::BeforeArrow(pos) | EParams::Arrow(pos) | EParams::AfterArrow(pos) => {
            let surroundings = Region::new(start, *pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(*pos));

            let doc = alloc.stack([
                alloc
                    .reflow(r"I am partway through parsing a module header, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting "),
                    alloc.keyword("->"),
                    alloc.reflow(" next, like:"),
                ]),
                alloc
                    .parser_suggestion("module { echo, read } -> [menu]")
                    .indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD MODULE PARAMS".to_string(),
                severity,
            }
        }

        EParams::Space(error, pos) => to_space_report(alloc, lines, filename, error, *pos),
    }
}

fn to_exposes_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::EExposes,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EExposes;

    let severity = Severity::RuntimeError;

    match *parse_problem {
        // TODO: It would be nice to give ListEnd it's own error message.
        // However with how parsing error are currently reported, ListEnd and
        // Identifier are tightly coupled and Identifier is basically never
        // "thrown". Splitting these up to make more specific error messages
        // would require tweaking how parsing works.
        EExposes::ListEnd(pos) | EExposes::Identifier(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("I am partway through parsing an "),
                    alloc.keyword("exposes"),
                    alloc.reflow(" list, but I got stuck here:"),
                ]),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.reflow("I was expecting a type name, value name or function name next, like"),
                alloc.parser_suggestion("[Animal, default, tame]").indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD EXPOSES".to_string(),
                severity,
            }
        }

        EExposes::Exposes(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting the "),
                    alloc.keyword("exposes"),
                    alloc.reflow(" keyword next, like"),
                ]),
                alloc.parser_suggestion("[Animal, default, tame]").indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD EXPOSES".to_string(),
                severity,
            }
        }

        EExposes::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),

        // TODO: Depending on the type of header we're parsing, it would be
        // great to add more context to this error message. For example, if
        // we're parsing a `module` then saying something like:
        //
        //     I was expecting an exposes list like
        //         ...
        //
        //     or module params like
        //         ...
        //
        // would be great. But currently we don't capture the type of header in
        // the parse problem data type, so this isn't possible
        EExposes::ListStart(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I was expecting an "),
                    alloc.keyword("exposes"),
                    alloc.reflow(" list like"),
                ]),
                alloc.parser_suggestion("[Animal, default, tame]").indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD EXPOSES".to_string(),
                severity,
            }
        }

        // If you're adding or changing syntax, please handle the case with a
        // good error message above instead of adding more unhandled cases below.
        EExposes::Open(pos) | EExposes::IndentExposes(pos) | EExposes::IndentListStart(pos) => {
            to_unhandled_parse_error_report(
                alloc,
                lines,
                filename,
                format!("{:?}", parse_problem),
                pos,
                start,
            )
        }
    }
}

fn to_imports_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::EImports,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EImports;

    let severity = Severity::RuntimeError;

    match *parse_problem {
        EImports::Identifier(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a imports list, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([alloc.reflow(
                    "I was expecting a type name, value name or function name next, like ",
                )]),
                alloc
                    .parser_suggestion("imports [Animal, default, tame]")
                    .indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD IMPORTS".to_string(),
                severity,
            }
        }

        EImports::Imports(pos) | EImports::IndentImports(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting the "),
                    alloc.keyword("imports"),
                    alloc.reflow(" keyword next, like"),
                ]),
                alloc
                    .parser_suggestion("imports [Animal, default, tame]")
                    .indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD IMPORTS".to_string(),
                severity,
            }
        }

        EImports::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),

        EImports::ModuleName(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting a module name next, like "),
                    alloc.parser_suggestion("BigNum"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("Main"),
                    alloc.reflow(". Module names must start with an uppercase letter."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD MODULE NAME".to_string(),
                severity,
            }
        }

        EImports::ListEnd(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a imports list, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([alloc.reflow("I am expecting a comma or end of list, like")]),
                alloc.parser_suggestion("imports [Shape, Vector]").indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD IMPORTS".to_string(),
                severity,
            }
        }
        // If you're adding or changing syntax, please handle the case with a
        // good error message above instead of adding more unhandled cases below.
        EImports::Open(pos)
        | EImports::IndentListStart(pos)
        | EImports::IndentListEnd(pos)
        | EImports::ListStart(pos)
        | EImports::ExposingDot(pos)
        | EImports::ShorthandDot(pos)
        | EImports::Shorthand(pos)
        | EImports::IndentSetStart(pos)
        | EImports::SetStart(pos)
        | EImports::SetEnd(pos)
        | EImports::TypedIdent(pos)
        | EImports::AsKeyword(pos)
        | EImports::StrLiteral(pos) => to_unhandled_parse_error_report(
            alloc,
            lines,
            filename,
            format!("{:?}", parse_problem),
            pos,
            start,
        ),
    }
}

fn to_requires_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::ERequires<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::ERequires;

    let severity = Severity::RuntimeError;

    match *parse_problem {
        ERequires::Requires(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting the "),
                    alloc.keyword("requires"),
                    alloc.reflow(" keyword next, like"),
                ]),
                alloc
                    .parser_suggestion("requires { main : Task I64 Str }")
                    .indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "MISSING REQUIRES".to_string(),
                severity,
            }
        }

        ERequires::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),

        ERequires::ListStart(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting the "),
                    alloc.keyword("requires"),
                    alloc.reflow(" keyword next, like"),
                ]),
                alloc
                    .parser_suggestion("requires { main : Task I64 Str }")
                    .indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "MISSING REQUIRES".to_string(),
                severity,
            }
        }

        ERequires::Rigid(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting a list of rigids like "),
                    alloc.keyword("{}"),
                    alloc.reflow(" or "),
                    alloc.keyword("{model=>Model}"),
                    alloc.reflow(" next. A full "),
                    alloc.keyword("requires"),
                    alloc.reflow(" definition looks like"),
                ]),
                alloc
                    .parser_suggestion("requires {model=>Model, msg=>Msg} {main : Task {} []}")
                    .indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "BAD REQUIRES RIGIDS".to_string(),
                severity,
            }
        }

        ERequires::ListEnd(pos) | ERequires::Open(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting a list of type names like "),
                    alloc.keyword("{}"),
                    alloc.reflow(" or "),
                    alloc.keyword("{ Model }"),
                    alloc.reflow(" next. A full "),
                    alloc.keyword("requires"),
                    alloc.reflow(" definition looks like"),
                ]),
                alloc
                    .parser_suggestion("requires { Model, Msg } {main : Task {} []}")
                    .indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "BAD REQUIRES".to_string(),
                severity,
            }
        }
        // If you're adding or changing syntax, please handle the case with a
        // good error message above instead of adding more unhandled cases below.
        ERequires::IndentRequires(pos)
        | ERequires::IndentListStart(pos)
        | ERequires::TypedIdent(_, pos) => to_unhandled_parse_error_report(
            alloc,
            lines,
            filename,
            format!("{:?}", parse_problem),
            pos,
            start,
        ),
    }
}

fn to_packages_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::EPackages,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EPackages;

    let severity = Severity::RuntimeError;

    match *parse_problem {
        EPackages::Packages(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I am partway through parsing a header, but I got stuck here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I am expecting the "),
                    alloc.keyword("packages"),
                    alloc.reflow(" keyword next, like"),
                ]),
                alloc.parser_suggestion("packages {}").indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "MISSING PACKAGES".to_string(),
                severity,
            }
        }
        EPackages::ListEnd(pos) => {
            let surroundings = Region::new(start, pos);
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(
                    r"I am partway through parsing a list of packages, but I got stuck here:",
                ),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([alloc.reflow("I am expecting a comma or end of list, like")]),
                alloc
                    .parser_suggestion("packages { package_name: \"url-or-path\", }")
                    .indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "WEIRD PACKAGES LIST".to_string(),
                severity,
            }
        }

        EPackages::Space(error, pos) => to_space_report(alloc, lines, filename, &error, pos),

        // If you're adding or changing syntax, please handle the case with a
        // good error message above instead of adding more unhandled cases below.
        EPackages::Open(pos)
        | EPackages::IndentPackages(pos)
        | EPackages::ListStart(pos)
        | EPackages::IndentListStart(pos)
        | EPackages::IndentListEnd(pos)
        | EPackages::PackageEntry(_, pos) => to_unhandled_parse_error_report(
            alloc,
            lines,
            filename,
            format!("{:?}", parse_problem),
            pos,
            start,
        ),
    }
}

fn to_space_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    parse_problem: &roc_parse::parser::BadInputError,
    pos: Position,
) -> Report<'a> {
    use roc_parse::parser::BadInputError;

    let severity = Severity::RuntimeError;

    match parse_problem {
        BadInputError::HasTab => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow("I encountered a tab character:"),
                alloc.region(region, severity),
                alloc.reflow(
                    "Tab characters are not allowed in Roc code. Please use spaces instead!",
                ),
            ]);

            Report {
                filename,
                doc,
                title: "TAB CHARACTER".to_string(),
                severity,
            }
        }

        BadInputError::HasAsciiControl => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow("I encountered an ASCII control character:"),
                alloc.region(region, severity),
                alloc.reflow("ASCII control characters are not allowed."),
            ]);

            Report {
                filename,
                doc,
                title: "ASCII CONTROL CHARACTER".to_string(),
                severity,
            }
        }

        BadInputError::HasMisplacedCarriageReturn => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            let doc = alloc.stack([
                alloc.reflow(r"I encountered a stray carriage return (\r):"),
                alloc.region(region, severity),
                alloc.reflow(r"A carriage return (\r) has to be followed by a newline (\n)."),
            ]);

            Report {
                filename,
                doc,
                title: "MISPLACED CARRIAGE RETURN".to_string(),
                severity,
            }
        }

        // If you're adding or changing syntax, please handle the case with a
        // good error message above instead of adding more unhandled cases below.
        BadInputError::BadUtf8 => to_unhandled_parse_error_report(
            alloc,
            lines,
            filename,
            format!("{:?}", parse_problem),
            pos,
            pos,
        ),
    }
}

fn to_ability_def_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    problem: &roc_parse::parser::EAbility<'a>,
    start: Position,
) -> Report<'a> {
    use roc_parse::parser::EAbility;

    match problem {
        EAbility::Space(error, pos) => to_space_report(alloc, lines, filename, error, *pos),
        EAbility::Type(tipe, pos) => to_type_report(alloc, lines, filename, tipe, *pos),
        EAbility::DemandAlignment(over_under_indent, pos) => {
            let over_under_msg = if *over_under_indent > 0 {
                alloc.reflow("indented too much")
            } else {
                alloc.reflow("not indented enough")
            };

            let msg = alloc.concat([
                alloc.reflow("I suspect this line is "),
                over_under_msg,
                alloc.reflow(" (by "),
                alloc.string(over_under_indent.abs().to_string()),
                alloc.reflow(" spaces)"),
            ]);

            to_unfinished_ability_report(alloc, lines, filename, *pos, start, msg)
        }
        EAbility::DemandName(pos) => to_unfinished_ability_report(
            alloc,
            lines,
            filename,
            *pos,
            start,
            alloc.reflow("I was expecting to see a value signature next."),
        ),
        EAbility::DemandColon(pos) => to_unfinished_ability_report(
            alloc,
            lines,
            filename,
            *pos,
            start,
            alloc.concat([
                alloc.reflow("I was expecting to see a "),
                alloc.parser_suggestion(":"),
                alloc.reflow(" annotating the signature of this value next."),
            ]),
        ),
    }
}

fn to_unfinished_ability_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    pos: Position,
    start: Position,
    message: RocDocBuilder<'a>,
) -> Report<'a> {
    let surroundings = Region::new(start, pos);
    let region = LineColumnRegion::from_pos(lines.convert_pos(pos));
    let severity = Severity::RuntimeError;

    let doc = alloc.stack([
        alloc.reflow(r"I was partway through parsing an ability definition, but I got stuck here:"),
        alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
        message,
    ]);

    Report {
        filename,
        doc,
        title: "UNFINISHED ABILITY".to_string(),
        severity,
    }
}

#[derive(Debug)]
enum Next<'a> {
    Keyword(&'a str),
    // Operator(&'a str),
    #[allow(dead_code)]
    Close(&'a str, char),
    Token(&'a str),
    Other(Option<char>),
}

fn what_is_next<'a>(source_lines: &'a [&'a str], pos: LineColumn) -> Next<'a> {
    let row_index = pos.line as usize;
    let col_index = pos.column as usize;
    match source_lines.get(row_index) {
        None => Next::Other(None),
        Some(line) => {
            let chars = &line[col_index..];
            let mut it = chars.chars();

            match roc_parse::keyword::KEYWORDS
                .iter()
                .find(|keyword| starts_with_keyword(chars, keyword))
            {
                Some(keyword) => Next::Keyword(keyword),
                None => match it.next() {
                    None => Next::Other(None),
                    Some(c) => match c {
                        ')' => Next::Close("parenthesis", ')'),
                        ']' => Next::Close("square bracket", ']'),
                        '}' => Next::Close("curly brace", '}'),
                        '-' if it.next() == Some('>') => Next::Token("->"),
                        '=' if it.next() == Some('>') => Next::Token("=>"),
                        // _ if is_symbol(c) => todo!("it's an operator"),
                        _ => Next::Other(Some(c)),
                    },
                },
            }
        }
    }
}

pub fn starts_with_keyword(rest_of_line: &str, keyword: &str) -> bool {
    if let Some(stripped) = rest_of_line.strip_prefix(keyword) {
        match stripped.chars().next() {
            None => true,
            Some(c) => !c.is_alphanumeric(),
        }
    } else {
        false
    }
}

fn next_line_starts_with_close_curly(source_lines: &[&str], pos: LineColumn) -> Option<LineColumn> {
    next_line_starts_with_char(source_lines, pos, '}')
}

fn next_line_starts_with_close_parenthesis(
    source_lines: &[&str],
    pos: LineColumn,
) -> Option<LineColumn> {
    next_line_starts_with_char(source_lines, pos, ')')
}

fn next_line_starts_with_char(
    source_lines: &[&str],
    pos: LineColumn,
    character: char,
) -> Option<LineColumn> {
    match source_lines.get(pos.line as usize + 1) {
        None => None,

        Some(line) => {
            let spaces_dropped = line.trim_start_matches(' ');
            match spaces_dropped.chars().next() {
                Some(c) if c == character => Some(LineColumn {
                    line: pos.line + 1,
                    column: (line.len() - spaces_dropped.len()) as u32,
                }),
                _ => None,
            }
        }
    }
}

fn to_keyword_region(pos: LineColumn, keyword: &str) -> LineColumnRegion {
    LineColumnRegion {
        start: pos,
        end: pos.bump_column(keyword.len() as u32),
    }
}
