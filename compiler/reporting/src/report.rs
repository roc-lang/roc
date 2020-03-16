use crate::report::ReportText::{Batch, Region, Value};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_problem::can::Problem;
use roc_types::pretty_print::content_to_string;
use roc_types::subs::{Content, Subs};
use std::path::PathBuf;

/// A textual report.
pub struct Report {
    pub filename: PathBuf,
    pub text: ReportText,
}

pub fn can_problem(filename: PathBuf, problem: Problem) -> Report {
    let mut texts = Vec::new();

    match problem {
        Problem::UnusedDef(symbol, region) => {
            texts.push(Value(symbol));
            texts.push(plain_text(" is not used anywhere in your code."));
            texts.push(newline());
            texts.push(newline());
            texts.push(Region(region));
            texts.push(newline());
            texts.push(newline());
            texts.push(plain_text("If you didn't intend on using "));
            texts.push(Value(symbol));
            texts.push(plain_text(
                " then remove it so future readers of your code don't wonder why it is there.",
            ));
        }
        _ => {
            panic!("TODO implement others");
        }
    };

    Report {
        filename,
        text: Batch(texts),
    }
}

#[derive(Debug, Clone)]
pub enum ReportText {
    /// A value. Render it qualified unless it was defined in the current module.
    Value(Symbol),

    /// A type. Render it using roc_types::pretty_print for now, but maybe
    /// do something fancier later.
    Type(Content),

    /// Plain text
    Plain(Box<str>),

    /// Emphasized text (might be bold, italics, a different color, etc)
    EmText(Box<str>),

    /// A region in the original source
    Region(roc_region::all::Region),

    /// A URL, which should be rendered as a hyperlink.
    Url(Box<str>),

    /// The documentation for this symbol.
    Docs(Symbol),

    Batch(Vec<ReportText>),
}

pub fn plain_text(str: &str) -> ReportText {
    use ReportText::*;

    Plain(Box::from(str))
}

fn newline() -> ReportText {
    plain_text("\n")
}

impl ReportText {
    /// Render to CI console output, where no colors are available.
    pub fn render_ci(
        self,
        buf: &mut String,
        subs: &mut Subs,
        home: ModuleId,
        src_lines: &[&str],
        interns: &Interns,
    ) {
        use ReportText::*;

        match self {
            Plain(string) => buf.push_str(&string),
            EmText(string) => {
                // Since this is CI, the best we can do for emphasis are asterisks.
                buf.push('*');
                buf.push_str(&string);
                buf.push('*');
            }
            Url(url) => {
                buf.push('<');
                buf.push_str(&url);
                buf.push('>');
            }
            Value(symbol) => {
                if symbol.module_id() == home {
                    // Render it unqualified if it's in the current module.
                    buf.push_str(symbol.ident_string(interns));
                } else {
                    buf.push_str(symbol.module_string(interns));
                    buf.push('.');
                    buf.push_str(symbol.ident_string(interns));
                }
            }
            Type(content) => buf.push_str(content_to_string(content, subs, home, interns).as_str()),
            Region(region) => {
                for i in region.start_line..=region.end_line {
                    buf.push_str(i.to_string().as_str());
                    buf.push_str(" ┆");

                    let line = src_lines[i as usize];

                    if !line.is_empty() {
                        buf.push_str("  ");
                        buf.push_str(src_lines[i as usize]);
                    }

                    if i != region.end_line {
                        buf.push('\n');
                    }
                }
            }
            Docs(_) => {
                panic!("TODO implment docs");
            }
            Batch(report_texts) => {
                for report_text in report_texts {
                    report_text.render_ci(buf, subs, home, src_lines, interns);
                }
            }
        }
    }

    /// Render to a color terminal using ANSI escape sequences
    pub fn render_color_terminal(
        &self,
        _buf: &mut String,
        _subs: &mut Subs,
        _home: ModuleId,
        _src_lines: &[&str],
        _interns: &Interns,
    ) {
        // TODO do the same stuff as render_ci, but with colors via ANSI terminal escape codes!
        // Examples of how to do this are in the source code of https://github.com/rtfeldman/console-print
    }
}
