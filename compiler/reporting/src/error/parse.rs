use roc_parse::parser::{Fail, FailReason};
use std::path::PathBuf;

use crate::report::{Report, RocDocAllocator};
use ven_pretty::DocAllocator;

pub fn parse_problem<'b>(
    alloc: &'b RocDocAllocator<'b>,
    filename: PathBuf,
    problem: Fail,
) -> Report<'b> {
    use FailReason::*;

    match problem.reason {
        ArgumentsBeforeEquals => {
            let doc = alloc.text("Unexpected tokens in front of the `=` symbol:");

            Report {
                filename,
                doc,
                title: "PARSE PROBLEM".to_string(),
            }
        }
        other => {
            //
            //    Unexpected(char, Region),
            //    OutdentedTooFar,
            //    ConditionFailed,
            //    LineTooLong(u32 /* which line was too long */),
            //    TooManyLines,
            //    Eof(Region),
            //    InvalidPattern,
            //    ReservedKeyword(Region),
            //    ArgumentsBeforeEquals,
            //}
            todo!("unhandled parse error: {:?}", other)
        }
    }
}
