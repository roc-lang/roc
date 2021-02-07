use roc_parse::parser::{ContextItem, ParseProblem, SyntaxError};
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

pub fn parse_problem<'b>(
    alloc: &'b RocDocAllocator<'b>,
    filename: PathBuf,
    starting_line: u32,
    parse_problem: ParseProblem<SyntaxError>,
) -> Report<'b> {
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
                    todo!(),
                    alloc.text(":"),
                ]),
                alloc.region(region),
            ]);

            report(doc)
        }
        _ => todo!("unhandled parse error: {:?}", parse_problem.problem),
    }
}
