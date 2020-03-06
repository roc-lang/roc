use crate::pretty_print_types::content_to_string;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_problem::can::Problem;
use roc_region::all::Region;
use roc_types::subs::{Content, Subs};
use std::path::PathBuf;

/// A textual report.
pub struct Report {
    pub filename: PathBuf,
    pub text: ReportText,
}

impl Report {
    pub fn can_problem(_filename: PathBuf, _problem: Problem) -> Self {
        // let text = match problem {
        //     Problem::UnusedDef(symbol, region) => {
        //         panic!("TODO implelment me!");
        //     }
        //     _ => {
        //         panic!("TODO implement others");
        //     }
        // };

        // Report { filename, text }
        panic!("TODO implement me!");
    }
}

#[derive(Debug, Clone)]
pub enum ReportText {
    /// A value. Render it qualified unless it was defined in the current module.
    Value(Symbol),

    /// A type. Render it using pretty_print_types for now, but maybe
    /// do something fancier later.
    Type(Content),

    /// Plain text
    Plain(Box<str>),

    /// Emphasized text (might be bold, italics, a different color, etc)
    EmText(Box<str>),

    /// A region in the original source
    Region(Region),

    /// A URL, which should be rendered as a hyperlink.
    Url(Box<str>),

    /// The documentation for this symbol.
    Docs(Symbol),
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
                panic!(
                    "TODO convert these source lines and this region into a String: {:?}, {:?}",
                    src_lines, region
                );
            }
            Docs(_) => {
                panic!("TODO implment docs");
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
