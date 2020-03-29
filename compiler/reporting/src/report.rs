use crate::report::ReportText::{Batch, Module, Region, Value};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_problem::can::PrecedenceProblem::BothNonAssociative;
use roc_problem::can::Problem;
use roc_types::pretty_print::content_to_string;
use roc_types::subs::{Content, Subs};
use std::path::PathBuf;

/// A textual report.
pub struct Report {
    pub filename: PathBuf,
    pub text: ReportText,
}

pub struct Palette {
    pub primary: Color,
    pub code_block: Color,
    pub variable: Color,
    pub flex_var: Color,
    pub rigid_var: Color,
    pub structure: Color,
    pub alias: Color,
    pub error: Color,
    pub line_number: Color,
    pub gutter_bar: Color,
    pub module_name: Color,
}

#[derive(Copy, Clone)]
pub enum Color {
    White,
    Red,
    Blue,
    Yellow,
    Green,
    Cyan,
    Magenta,
}

pub const TEST_PALETTE: Palette = Palette {
    primary: Color::White,
    code_block: Color::White,
    variable: Color::Blue,
    flex_var: Color::Yellow,
    rigid_var: Color::Yellow,
    structure: Color::Green,
    alias: Color::Yellow,
    error: Color::Red,
    line_number: Color::Cyan,
    gutter_bar: Color::Magenta,
    module_name: Color::Green,
};

impl Color {
    pub fn render(self, str: &str) -> String {
        use Color::*;

        match self {
            Red => red(str),
            White => white(str),
            Blue => blue(str),
            Yellow => yellow(str),
            Green => green(str),
            Cyan => cyan(str),
            Magenta => magenta(str),
        }
    }
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
        Problem::UnusedImport(module_id, region) => {
            texts.push(plain_text("Nothing from "));
            texts.push(Module(module_id));
            texts.push(plain_text(" is used in this module."));
            texts.push(newline());
            texts.push(newline());
            texts.push(Region(region));
            texts.push(newline());
            texts.push(newline());
            texts.push(plain_text("Since "));
            texts.push(Module(module_id));
            texts.push(plain_text(" isn't used, you don't need to import it."));
        }
        Problem::UnusedArgument(closure_symbol, argument_symbol, region) => {
            texts.push(Value(closure_symbol));
            texts.push(plain_text(" doesn't use "));
            texts.push(Value(argument_symbol));
            texts.push(plain_text("."));
            texts.push(newline());
            texts.push(newline());
            texts.push(Region(region));
            texts.push(newline());
            texts.push(newline());
            texts.push(plain_text("If you don't need "));
            texts.push(Value(argument_symbol));
            texts.push(plain_text(
                ", then you can just remove it. However, if you really do need ",
            ));
            texts.push(Value(argument_symbol));
            texts.push(plain_text(" as an argument of "));
            texts.push(Value(closure_symbol));
            texts.push(plain_text(", prefix it with an underscore, like this: \"_"));
            texts.push(Value(argument_symbol));
            texts.push(plain_text("\". Adding an underscore at the start of a variable name is a way of saying that the variable is not used."));
        }
        Problem::PrecedenceProblem(BothNonAssociative(
            _left_symbol,
            _left_bin_op,
            _middle_symbol,
            _right_bin_op,
            _right_symbol,
        )) => panic!("TODO implement precedence problem report"),
        Problem::UnsupportedPattern(_pattern_type, _region) => {
            panic!("TODO implement unsupported pattern report")
        }
        Problem::ShadowingInAnnotation {
            original_region,
            shadow,
        } => {
            let _a = original_region;
            let _b = shadow;
            panic!("TODO implement shadow report")
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

    /// A module,
    Module(ModuleId),

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

    /// Many ReportText that should be concatenated together.
    Batch(Vec<ReportText>),
}

pub fn plain_text(str: &str) -> ReportText {
    use ReportText::*;

    Plain(Box::from(str))
}

pub fn em_text(str: &str) -> ReportText {
    use ReportText::*;

    EmText(Box::from(str))
}

pub fn url(str: &str) -> ReportText {
    use ReportText::*;

    Url(Box::from(str))
}

fn newline() -> ReportText {
    plain_text("\n")
}

pub const RED_CODE: &str = "\u{001b}[31m";
pub const WHITE_CODE: &str = "\u{001b}[37m";
pub const BLUE_CODE: &str = "\u{001b}[34m";
pub const YELLOW_CODE: &str = "\u{001b}[33m";
pub const GREEN_CODE: &str = "\u{001b}[42m";
pub const CYAN_CODE: &str = "\u{001b}[36m";
pub const MAGENTA_CODE: &str = "\u{001b}[35m";

pub const BOLD_CODE: &str = "\u{001b}[1m";

pub const UNDERLINE_CODE: &str = "\u{001b}[4m";

fn code(code_str: &str, str: &str) -> String {
    let mut buf = String::new();

    buf.push_str(code_str);
    buf.push_str(str);
    buf.push_str(RESET_CODE);

    buf
}

pub fn underline(str: &str) -> String {
    code(UNDERLINE_CODE, str)
}

pub fn bold(str: &str) -> String {
    code(BOLD_CODE, str)
}

fn cyan(str: &str) -> String {
    code(CYAN_CODE, str)
}

fn magenta(str: &str) -> String {
    code(MAGENTA_CODE, str)
}

fn green(str: &str) -> String {
    code(GREEN_CODE, str)
}

fn yellow(str: &str) -> String {
    code(YELLOW_CODE, str)
}

fn blue(str: &str) -> String {
    code(BLUE_CODE, str)
}

fn red(str: &str) -> String {
    code(RED_CODE, str)
}

fn white(str: &str) -> String {
    code(WHITE_CODE, str)
}

pub const RESET_CODE: &str = "\u{001b}[0m";

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
            Module(module_id) => {
                buf.push_str(&module_id.name());
            }
            Type(content) => buf.push_str(content_to_string(content, subs, home, interns).as_str()),
            Region(region) => {
                let max_line_number_length = region.end_line.to_string().len();

                for i in region.start_line..=region.end_line {
                    let i_one_indexed = i + 1;

                    let line_number_string = i_one_indexed.to_string();
                    let line_number = line_number_string.as_str();
                    let this_line_number_length = line_number.len();

                    buf.push_str(
                        " ".repeat(max_line_number_length - this_line_number_length)
                            .as_str(),
                    );
                    buf.push_str(line_number);
                    buf.push_str(" ┆");

                    let line = src_lines[i as usize];

                    if !line.trim().is_empty() {
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
        self,
        buf: &mut String,
        subs: &mut Subs,
        home: ModuleId,
        src_lines: &[&str],
        interns: &Interns,
        palette: &Palette,
    ) {
        use ReportText::*;

        match self {
            Plain(string) => {
                buf.push_str(&palette.primary.render(&string));
            }

            EmText(string) => {
                buf.push_str(&bold(&string));
            }
            Url(url) => {
                buf.push_str(&underline(&url));
            }
            Value(symbol) => {
                if symbol.module_id() == home {
                    // Render it unqualified if it's in the current module.
                    buf.push_str(&palette.variable.render(symbol.ident_string(interns)));
                } else {
                    let mut module_str = String::new();

                    module_str.push_str(symbol.module_string(interns));
                    module_str.push('.');
                    module_str.push_str(symbol.ident_string(interns));

                    buf.push_str(&palette.variable.render(&module_str));
                }
            }
            Module(module_id) => {
                buf.push_str(&palette.module_name.render(&module_id.name()));
            }
            Type(content) => match content {
                Content::FlexVar(flex_var) => buf.push_str(&palette.flex_var.render(
                    content_to_string(Content::FlexVar(flex_var), subs, home, interns).as_str(),
                )),
                Content::RigidVar(rigid_var) => buf.push_str(&palette.rigid_var.render(
                    content_to_string(Content::RigidVar(rigid_var), subs, home, interns).as_str(),
                )),
                Content::Structure(structure) => buf.push_str(&palette.structure.render(
                    // TODO give greater specificity to how structures are colored. Empty record colored differently than tags, etc.
                    content_to_string(Content::Structure(structure), subs, home, interns).as_str(),
                )),
                Content::Alias(symbol, vars, var) => buf.push_str(
                    &palette.alias.render(
                        content_to_string(Content::Alias(symbol, vars, var), subs, home, interns)
                            .as_str(),
                    ),
                ),
                Content::Error => {}
            },
            Region(region) => {
                let max_line_number_length = region.end_line.to_string().len();

                for i in region.start_line..=region.end_line {
                    let i_one_indexed = i + 1;

                    let line_number_string = i_one_indexed.to_string();
                    let line_number = line_number_string.as_str();
                    let this_line_number_length = line_number.len();

                    buf.push_str(
                        " ".repeat(max_line_number_length - this_line_number_length)
                            .as_str(),
                    );
                    buf.push_str(&palette.line_number.render(line_number));
                    buf.push_str(&palette.gutter_bar.render(" ┆"));

                    let line = src_lines[i as usize];

                    if !line.trim().is_empty() {
                        buf.push_str("  ");
                        buf.push_str(&palette.code_block.render(src_lines[i as usize]));
                    }

                    if i != region.end_line {
                        buf.push('\n');
                    }
                }
            }
            Batch(report_texts) => {
                for report_text in report_texts {
                    report_text.render_color_terminal(buf, subs, home, src_lines, interns, palette);
                }
            }
            _ => panic!("TODO implement more ReportTexts in render color terminal"),
        }
    }
}
