use std::path::PathBuf;

use roc_problem::Severity;
use roc_region::all::{LineInfo, Region};

use crate::report::{Report, RocDocAllocator};
use ven_pretty::DocAllocator;

pub fn not_supported<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    problem: &'b str,
    region: Region,
) -> Report<'b> {
    let doc = alloc.stack([
        alloc.region(lines.convert_region(region)),
        alloc
            .reflow(problem)
            .append(alloc.reflow(" is not supported in the repl.")),
    ]);

    Report {
        title: "REPL NOT SUPPORTED".to_string(),
        filename,
        doc,
        severity: Severity::Fatal,
    }
}
