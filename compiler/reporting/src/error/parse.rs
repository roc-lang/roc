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
            let surroundings = Region::from_rows_cols(start_row, start_col, row, col);
            let region = Region::from_row_col(row, col);

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
            match next_line_starts_with_close_curly(alloc.src_lines, row - 1) {
                Some((curly_row, curly_col)) => {
                    let surroundings =
                        Region::from_rows_cols(start_row, start_col, curly_row, curly_col);
                    let region = Region::from_row_col(curly_row, curly_col);

                    dbg!(region);

                    let doc = alloc.stack(vec![
                        alloc.reflow(
                            "I am partway through parsing a record type, but I got stuck here:",
                        ),
                        alloc.region_with_subregion(surroundings, region),
                        alloc.concat(vec![
                            alloc.reflow("I need this curly brace to be indented more. Try adding more spaces before it!"),
                        ]),
                    ]);

                    Report {
                        filename: filename.clone(),
                        doc,
                        title: "NEED MORE INDENTATION".to_string(),
                    }
                }
                None => {
                    let surroundings = Region::from_rows_cols(start_row, start_col, row, col);
                    let region = Region::from_row_col(row, col);

                    let doc = alloc.stack(vec![
                        alloc.reflow(
                            r"I am partway through parsing a record type, but I got stuck here:",
                        ),
                        alloc.region_with_subregion(surroundings, region),
                        alloc.concat(vec![
                            alloc.reflow("I was expecting to see a closing curly "),
                            alloc.reflow("brace before this, so try adding a "),
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
            }
        }

        _ => todo!("unhandled record type parse error: {:?}", &parse_problem),
    }
}

enum Next<'a> {
    Keyword(&'a str),
    Operator(&'a str),
    Close(&'a str, char),
    Other(Option<char>),
}

fn what_is_next<'a>(source_lines: &'a [&'a str], row: Row, col: Col) -> Next<'a> {
    match source_lines.get(row as usize) {
        None => Next::Other(None),
        Some(line) => {
            let chars = &line[col as usize..];

            match roc_parse::keyword::KEYWORDS
                .iter()
                .find(|keyword| starts_with_keyword(chars, keyword))
            {
                Some(keyword) => Next::Keyword(keyword),
                None => match chars.chars().nth(0) {
                    None => Next::Other(None),
                    Some(c) => match c {
                        ')' => Next::Close("parenthesis", ')'),
                        ']' => Next::Close("square bracket", ']'),
                        '}' => Next::Close("curly brace", '}'),
                        // _ if is_symbol(c) => todo!("it's an operator"),
                        _ => Next::Other(Some(c)),
                    },
                },
            }
        }
    }
}

fn starts_with_keyword(rest_of_line: &str, keyword: &str) -> bool {
    if rest_of_line.starts_with(keyword) {
        match (&rest_of_line[keyword.len()..]).chars().nth(0) {
            None => true,
            Some(c) => c.is_alphanumeric() || c == '_',
        }
    } else {
        false
    }
}

fn next_line_starts_with_close_curly(source_lines: &[&str], row: Row) -> Option<(Row, Col)> {
    match source_lines.get(row as usize + 1) {
        None => None,

        Some(line) => {
            let spaces_dropped = line.trim_start_matches(' ');
            match spaces_dropped.chars().nth(0) {
                Some('}') => Some((row + 1, (line.len() - spaces_dropped.len()) as u16)),
                _ => None,
            }
        }
    }
}
