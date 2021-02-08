use roc_parse::parser::{Col, ContextItem, ParseProblem, Row, SyntaxError};
use roc_region::all::Region;
use std::path::PathBuf;

use crate::report::{Report, RocDocAllocator, RocDocBuilder};
use ven_pretty::DocAllocator;

fn context<'a>(
    alloc: &'a RocDocAllocator<'a>,
    context_stack: &[ContextItem],
    default: &'a str,
) -> RocDocBuilder<'a> {
    match context_stack.last() {
        Some(context_item) => {
            // assign string to `Attempting`
            use roc_parse::ast::Attempting::*;
            match context_item.context {
                Def => alloc.text("while parsing a definition"),
                _ => {
                    // use the default
                    alloc.text(default)
                }
            }
        }
        None => {
            // use the default
            alloc.text(default)
        }
    }
}

pub fn parse_problem<'a>(
    alloc: &'a RocDocAllocator<'a>,
    filename: PathBuf,
    starting_line: u32,
    parse_problem: ParseProblem<SyntaxError<'a>>,
) -> Report<'a> {
    let line = starting_line + parse_problem.line;
    let region = Region {
        start_line: line,
        end_line: line,
        start_col: parse_problem.column,
        end_col: parse_problem.column + 1,
    };

    let report = |doc| Report {
        filename: filename.clone(),
        doc,
        title: "PARSE PROBLEM".to_string(),
    };

    use SyntaxError::*;
    match parse_problem.problem {
        SyntaxError::ConditionFailed => {
            let doc = alloc.stack(vec![
                alloc.reflow("A condition failed:"),
                alloc.region(region),
            ]);

            Report {
                filename,
                doc,
                title: "PARSE PROBLEM".to_string(),
            }
        }
        SyntaxError::ArgumentsBeforeEquals(region) => {
            let doc = alloc.stack(vec![
                alloc.reflow("Unexpected tokens in front of the `=` symbol:"),
                alloc.region(region),
            ]);

            Report {
                filename,
                doc,
                title: "PARSE PROBLEM".to_string(),
            }
        }
        Unexpected(mut region) => {
            if region.start_col == region.end_col {
                region.end_col += 1;
            }

            let doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("Unexpected token "),
                    // context(alloc, &parse_problem.context_stack, "here"),
                    alloc.text(":"),
                ]),
                alloc.region(region),
            ]);

            report(doc)
        }
        Type(typ) => to_type_report(alloc, filename, starting_line, typ),
        _ => todo!("unhandled parse error: {:?}", parse_problem.problem),
    }
}

fn to_type_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    filename: PathBuf,
    starting_line: u32,
    parse_problem: roc_parse::parser::Type<'a>,
) -> Report<'a> {
    use roc_parse::parser::Type;

    match parse_problem {
        Type::TRecord(record, row, col) => {
            to_trecord_report(alloc, filename, starting_line, record, row, col)
        }
        _ => todo!("unhandled type parse error: {:?}", &parse_problem),
    }
}

fn to_trecord_report<'a>(
    alloc: &'a RocDocAllocator<'a>,
    filename: PathBuf,
    starting_line: u32,
    parse_problem: roc_parse::parser::TRecord<'a>,
    start_row: Row,
    start_col: Col,
) -> Report<'a> {
    use roc_parse::parser::TRecord;

    match parse_problem {
        TRecord::End(row, col) => {
            let surroundings = Region {
                start_col,
                start_line: start_row,
                end_col: col,
                end_line: row,
            };

            let region = Region {
                start_col: col,
                start_line: row,
                end_col: col,
                end_line: row,
            };

            let doc = alloc.stack(vec![
                alloc.reflow("I am partway through parsing a record type, but I got stuck here:"),
                alloc.region_with_subregion(surroundings, region),
                alloc.concat(vec![
                    alloc.reflow(
                        r"I was expecting to see a closing curly brace before this, so try adding a ",
                    ),
                    alloc.parser_suggestion("}"),
                    alloc.reflow(" and see if that helps?"),
                ]),
            ]);

            Report {
                filename: filename.clone(),
                doc,
                title: "UNFINISHED RECORD TYPE".to_string(),
            }
        }

        TRecord::IndentEnd(row, col) => {
            // TODO check whether next character is a `}`
            let surroundings = Region {
                start_col,
                start_line: start_row,
                end_col: col,
                end_line: row,
            };

            let region = Region {
                start_col: col,
                start_line: row,
                end_col: col,
                end_line: row,
            };

            let doc = alloc.stack(vec![
                alloc.reflow("I am partway through parsing a record type, but I got stuck here:"),
                alloc.region_with_subregion(surroundings, region),
                alloc.concat(vec![
                    alloc.reflow(
                        r"I was expecting to see a closing curly brace before this, so try adding a ",
                    ),
                    alloc.parser_suggestion("}"),
                    alloc.reflow(" and see if that helps?"),
                ]),
            ]);

            Report {
                filename: filename.clone(),
                doc,
                title: "UNFINISHED RECORD TYPE".to_string(),
            }
        }

        _ => todo!("unhandled record type parse error: {:?}", &parse_problem),
    }
}
