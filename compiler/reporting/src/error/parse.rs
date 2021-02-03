use roc_parse::parser::{Bag, DeadEnd, FailReason, ParseProblem};
use roc_region::all::Region;
use std::path::PathBuf;

use crate::report::{Report, RocDocAllocator};
use ven_pretty::DocAllocator;

pub fn parse_problem<'b>(
    alloc: &'b RocDocAllocator<'b>,
    filename: PathBuf,
    starting_line: u32,
    parse_problem: ParseProblem,
) -> Report<'b> {
    use FailReason::*;

    let line = starting_line + parse_problem.line;
    let region = Region {
        start_line: line,
        end_line: line,
        start_col: parse_problem.column,
        end_col: parse_problem.column,
    };

    match parse_problem.problem {
        ConditionFailed => {
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
        _ => todo!("unhandled parse error: {:?}", parse_problem.problem),
    }
}
