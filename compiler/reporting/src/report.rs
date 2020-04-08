use crate::report::ReportText::{BinOp, Concat, Module, Region, Value};
use roc_module::ident::TagName;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_problem::can::PrecedenceProblem::BothNonAssociative;
use roc_problem::can::{Problem, RuntimeError};
use roc_types::pretty_print::content_to_string;
use roc_types::subs::{Content, Subs};
use roc_types::types::{write_error_type, ErrorType};
use std::path::PathBuf;

use roc_module::ident::Ident;
use roc_region::all::Located;
use std::fmt;
use ven_pretty::{BoxAllocator, DocAllocator, DocBuilder, Render, RenderAnnotated};

const ADD_ANNOTATIONS: &str = r#"Can more type annotations be added? Type annotations always help me give more specific messages, and I think they could help a lot in this case"#;

/// A textual report.
pub struct Report {
    pub title: String,
    pub filename: PathBuf,
    pub text: ReportText,
}

pub struct Palette<'a> {
    pub primary: &'a str,
    pub code_block: &'a str,
    pub variable: &'a str,
    pub type_variable: &'a str,
    pub structure: &'a str,
    pub alias: &'a str,
    pub error: &'a str,
    pub line_number: &'a str,
    pub gutter_bar: &'a str,
    pub module_name: &'a str,
    pub binop: &'a str,
    pub typo: &'a str,
    pub typo_suggestion: &'a str,
}

pub const DEFAULT_PALETTE: Palette = Palette {
    primary: WHITE_CODE,
    code_block: WHITE_CODE,
    variable: BLUE_CODE,
    type_variable: YELLOW_CODE,
    structure: GREEN_CODE,
    alias: YELLOW_CODE,
    error: RED_CODE,
    line_number: CYAN_CODE,
    gutter_bar: MAGENTA_CODE,
    module_name: GREEN_CODE,
    binop: GREEN_CODE,
    typo: YELLOW_CODE,
    typo_suggestion: GREEN_CODE,
};

pub fn can_problem(filename: PathBuf, problem: Problem) -> Report {
    let mut texts = Vec::new();
    match problem {
        Problem::UnusedDef(symbol, region) => {
            texts.push(Value(symbol));
            texts.push(plain_text(" is not used anywhere in your code."));
            texts.push(Region(region));
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
            texts.push(Region(region));
            texts.push(plain_text("Since "));
            texts.push(Module(module_id));
            texts.push(plain_text(" isn't used, you don't need to import it."));
        }
        Problem::UnusedArgument(closure_symbol, argument_symbol, region) => {
            texts.push(Value(closure_symbol));
            texts.push(plain_text(" doesn't use "));
            texts.push(Value(argument_symbol));
            texts.push(plain_text("."));
            texts.push(Region(region));
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
        Problem::PrecedenceProblem(BothNonAssociative(region, left_bin_op, right_bin_op)) => {
            if left_bin_op.value == right_bin_op.value {
                texts.push(plain_text("Using more than one "));
                texts.push(BinOp(left_bin_op.value));
                texts.push(plain_text(
                    " like this requires parentheses, to clarify how things should be grouped.",
                ))
            } else {
                texts.push(plain_text("Using "));
                texts.push(BinOp(left_bin_op.value));
                texts.push(plain_text(" and "));
                texts.push(BinOp(right_bin_op.value));
                texts.push(plain_text(
                    " together requires parentheses, to clarify how they should be grouped.",
                ))
            }
            texts.push(Region(region));
        }
        Problem::UnsupportedPattern(_pattern_type, _region) => {
            panic!("TODO implement unsupported pattern report")
        }
        Problem::ShadowingInAnnotation {
            original_region,
            shadow,
        } => {
            shadowing_report(&mut texts, original_region, shadow);
        }
        Problem::RuntimeError(runtime_error) => match runtime_error {
            RuntimeError::Shadowing {
                original_region,
                shadow,
            } => {
                shadowing_report(&mut texts, original_region, shadow);
            }

            _ => {
                panic!("TODO implement run time error reporting");
            }
        },
    };

    Report {
        title: "SYNTAX PROBLEM".to_string(),
        filename,
        text: Concat(texts),
    }
}

fn shadowing_report(
    texts: &mut Vec<ReportText>,
    original_region: roc_region::all::Region,
    shadow: Located<Ident>,
) {
    texts.push(name(shadow.value));
    texts.push(plain_text(" is first defined here:"));
    texts.push(Region(original_region));
    texts.push(plain_text("But then it's defined a second time here:"));
    texts.push(Region(shadow.region));
    texts.push(plain_text("Since these variables have the same name, it's easy to use the wrong one on accident. Give one of them a new name."));
}

#[derive(Debug, Clone)]
pub enum ReportText {
    /// A value. Render it qualified unless it was defined in the current module.
    Value(Symbol),

    /// An identifier, should probably be rendered the same way as a symbol.
    Name(Ident),

    /// A module,
    Module(ModuleId),

    /// A type. Render it using roc_types::pretty_print for now, but maybe
    /// do something fancier later.
    Type(Content),

    TypeProblem(crate::type_error::Problem),

    ErrorTypeInline(ErrorType),
    ErrorTypeBlock(Box<ReportText>),

    /// Plain text
    Plain(Box<str>),

    /// Emphasized text (might be bold, italics, a different color, etc)
    EmText(Box<str>),

    /// A global tag rendered as code (e.g. a monospace font, or with backticks around it).
    GlobalTag(Box<str>),

    /// A private tag rendered as code (e.g. a monospace font, or with backticks around it).
    PrivateTag(Symbol),

    /// A record field name rendered as code (e.g. a monospace font, or with backticks around it).
    RecordField(Box<str>),

    /// A language keyword like `if`, rendered as code (e.g. a monospace font, or with backticks around it).
    Keyword(Box<str>),

    /// A region in the original source
    Region(roc_region::all::Region),

    /// A URL, which should be rendered as a hyperlink.
    Url(Box<str>),

    /// The documentation for this symbol.
    Docs(Symbol),

    BinOp(roc_parse::operator::BinOp),

    /// Many ReportText that should be concatenated together.
    Concat(Vec<ReportText>),

    /// Many ReportText that each get separate lines
    Stack(Vec<ReportText>),

    Intersperse {
        separator: Box<ReportText>,
        items: Vec<ReportText>,
    },

    Indent(usize, Box<ReportText>),
}

pub fn plain_text(str: &str) -> ReportText {
    ReportText::Plain(Box::from(str))
}

pub fn name(ident: Ident) -> ReportText {
    ReportText::Name(ident)
}

pub fn em_text(str: &str) -> ReportText {
    ReportText::EmText(Box::from(str))
}

pub fn tag_name_text(tag_name: TagName) -> ReportText {
    match tag_name {
        TagName::Private(symbol) => ReportText::PrivateTag(symbol),
        TagName::Global(uppercase) => global_tag_text(uppercase.as_str()),
    }
}

pub fn private_tag_text(symbol: Symbol) -> ReportText {
    ReportText::PrivateTag(symbol)
}

pub fn global_tag_text(str: &str) -> ReportText {
    ReportText::GlobalTag(Box::from(str))
}

pub fn record_field_text(str: &str) -> ReportText {
    ReportText::RecordField(Box::from(str))
}

pub fn keyword_text(str: &str) -> ReportText {
    ReportText::Keyword(Box::from(str))
}

pub fn error_type_inline(err: ErrorType) -> ReportText {
    ReportText::ErrorTypeInline(err)
}

pub fn error_type_block(err: ReportText) -> ReportText {
    ReportText::ErrorTypeBlock(Box::new(err))
}

pub fn url(str: &str) -> ReportText {
    ReportText::Url(Box::from(str))
}

pub fn concat(values: Vec<ReportText>) -> ReportText {
    ReportText::Concat(values)
}

pub fn separate(values: Vec<ReportText>) -> ReportText {
    // TODO I think this should be a possibly-breaking space
    intersperse(plain_text(" "), values)
}

pub fn intersperse(separator: ReportText, items: Vec<ReportText>) -> ReportText {
    ReportText::Intersperse {
        separator: Box::new(separator),
        items,
    }
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

pub const RESET_CODE: &str = "\u{001b}[0m";

#[derive(Copy, Clone)]
pub enum Annotation {
    Emphasized,
    Url,
    Keyword,
    GlobalTag,
    PrivateTag,
    RecordField,
    TypeVariable,
    Alias,
    Structure,
    Symbol,
    BinOp,
    Error,
    GutterBar,
    LineNumber,
    PlainText,
    CodeBlock,
    TypeBlock,
    Module,
    Typo,
    TypoSuggestion,
    Hint,
}

/// Render with minimal formatting
pub struct CiWrite<W> {
    style_stack: Vec<Annotation>,
    in_type_block: bool,
    upstream: W,
}

impl<W> CiWrite<W> {
    pub fn new(upstream: W) -> CiWrite<W> {
        CiWrite {
            style_stack: vec![],
            in_type_block: false,
            upstream,
        }
    }
}

/// Render with fancy formatting
pub struct ColorWrite<'a, W> {
    style_stack: Vec<Annotation>,
    palette: &'a Palette<'a>,
    upstream: W,
}

impl<'a, W> ColorWrite<'a, W> {
    pub fn new(palette: &'a Palette, upstream: W) -> ColorWrite<'a, W> {
        ColorWrite {
            style_stack: vec![],
            palette,
            upstream,
        }
    }
}

impl<W> Render for CiWrite<W>
where
    W: fmt::Write,
{
    type Error = fmt::Error;

    fn write_str(&mut self, s: &str) -> Result<usize, fmt::Error> {
        self.write_str_all(s).map(|_| s.len())
    }

    fn write_str_all(&mut self, s: &str) -> fmt::Result {
        self.upstream.write_str(s)
    }
}

impl<W> RenderAnnotated<Annotation> for CiWrite<W>
where
    W: fmt::Write,
{
    fn push_annotation(&mut self, annotation: &Annotation) -> Result<(), Self::Error> {
        use Annotation::*;
        match annotation {
            TypeBlock => {
                self.in_type_block = true;
            }
            Emphasized => {
                self.write_str("*")?;
            }
            Url => {
                self.write_str("<")?;
            }
            GlobalTag | PrivateTag | Keyword | RecordField | Symbol | Typo | TypoSuggestion
                if !self.in_type_block =>
            {
                self.write_str("`")?;
            }

            _ => {}
        }
        self.style_stack.push(*annotation);
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        use Annotation::*;

        match self.style_stack.pop() {
            None => {}
            Some(annotation) => match annotation {
                TypeBlock => {
                    self.in_type_block = false;
                }
                Emphasized => {
                    self.write_str("*")?;
                }
                Url => {
                    self.write_str(">")?;
                }
                GlobalTag | PrivateTag | Keyword | RecordField | Symbol | Typo | TypoSuggestion
                    if !self.in_type_block =>
                {
                    self.write_str("`")?;
                }

                _ => {}
            },
        }
        Ok(())
    }
}

impl<'a, W> Render for ColorWrite<'a, W>
where
    W: fmt::Write,
{
    type Error = fmt::Error;

    fn write_str(&mut self, s: &str) -> Result<usize, fmt::Error> {
        self.write_str_all(s).map(|_| s.len())
    }

    fn write_str_all(&mut self, s: &str) -> fmt::Result {
        self.upstream.write_str(s)
    }
}

impl<'a, W> RenderAnnotated<Annotation> for ColorWrite<'a, W>
where
    W: fmt::Write,
{
    fn push_annotation(&mut self, annotation: &Annotation) -> Result<(), Self::Error> {
        use Annotation::*;
        match annotation {
            Emphasized => {
                self.write_str(BOLD_CODE)?;
            }
            Url | Hint => {
                self.write_str(UNDERLINE_CODE)?;
            }
            PlainText => {
                self.write_str(self.palette.primary)?;
            }
            CodeBlock => {
                self.write_str(self.palette.code_block)?;
            }
            TypeVariable => {
                self.write_str(self.palette.type_variable)?;
            }
            Alias => {
                self.write_str(self.palette.alias)?;
            }
            BinOp => {
                self.write_str(self.palette.alias)?;
            }
            Symbol => {
                self.write_str(self.palette.variable)?;
            }
            GutterBar => {
                self.write_str(self.palette.gutter_bar)?;
            }
            Error => {
                self.write_str(self.palette.error)?;
            }
            LineNumber => {
                self.write_str(self.palette.line_number)?;
            }
            Structure => {
                self.write_str(self.palette.structure)?;
            }
            Module => {
                self.write_str(self.palette.module_name)?;
            }
            Typo => {
                self.write_str(self.palette.typo)?;
            }
            TypoSuggestion => {
                self.write_str(self.palette.typo_suggestion)?;
            }
            TypeBlock | GlobalTag | PrivateTag | RecordField | Keyword => { /* nothing yet */ }
        }
        self.style_stack.push(*annotation);
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        use Annotation::*;

        match self.style_stack.pop() {
            None => {}
            Some(annotation) => match annotation {
                Emphasized | Url | TypeVariable | Alias | Symbol | BinOp | Error | GutterBar
                | Typo | TypoSuggestion | Structure | CodeBlock | PlainText | LineNumber | Hint
                | Module => {
                    self.write_str(RESET_CODE)?;
                }

                TypeBlock | GlobalTag | PrivateTag | RecordField | Keyword => { /* nothing yet */ }
            },
        }
        Ok(())
    }
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
        let alloc = BoxAllocator;

        let err_msg = "<buffer is not a utf-8 encoded string>";

        self.pretty::<_>(&alloc, subs, home, src_lines, interns)
            .1
            .render_raw(70, &mut CiWrite::new(buf))
            .expect(err_msg);
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
        let alloc = BoxAllocator;

        let err_msg = "<buffer is not a utf-8 encoded string>";

        self.pretty::<_>(&alloc, subs, home, src_lines, interns)
            .1
            .render_raw(70, &mut ColorWrite::new(palette, buf))
            .expect(err_msg);
    }

    /// General idea: this function puts all the characters in. Any styling (emphasis, colors,
    /// monospace font, etc) is done in the CiWrite and ColorWrite `RenderAnnotated` instances.
    pub fn pretty<'b, D>(
        self,
        alloc: &'b D,
        subs: &mut Subs,
        home: ModuleId,
        src_lines: &'b [&'b str],
        interns: &Interns,
    ) -> DocBuilder<'b, D, Annotation>
    where
        D: DocAllocator<'b, Annotation>,
        D::Doc: Clone,
    {
        use ReportText::*;

        match self {
            Url(url) => alloc.text(url.into_string()).annotate(Annotation::Url),
            Plain(string) => alloc
                .text(string.into_string())
                .annotate(Annotation::PlainText),
            EmText(string) => alloc
                .text(string.into_string())
                .annotate(Annotation::Emphasized),
            Keyword(string) => alloc
                .text(string.into_string())
                .annotate(Annotation::Keyword),
            GlobalTag(string) => alloc
                .text(string.into_string())
                .annotate(Annotation::GlobalTag),
            RecordField(string) => alloc
                .text(format!(".{}", string))
                .annotate(Annotation::RecordField),
            PrivateTag(symbol) => {
                if symbol.module_id() == home {
                    // Render it unqualified if it's in the current module.
                    alloc
                        .text(format!("{}", symbol.ident_string(interns)))
                        .annotate(Annotation::PrivateTag)
                } else {
                    alloc
                        .text(format!(
                            "{}.{}",
                            symbol.module_string(interns),
                            symbol.ident_string(interns),
                        ))
                        .annotate(Annotation::PrivateTag)
                }
            }
            Value(symbol) => {
                if symbol.module_id() == home || symbol.module_id().is_builtin() {
                    // Render it unqualified if it's in the current module or a builtin
                    alloc
                        .text(format!("{}", symbol.ident_string(interns)))
                        .annotate(Annotation::Symbol)
                } else {
                    alloc
                        .text(format!(
                            "{}.{}",
                            symbol.module_string(interns),
                            symbol.ident_string(interns),
                        ))
                        .annotate(Annotation::Symbol)
                }
            }

            Module(module_id) => alloc
                .text(format!("{}", interns.module_name(module_id)))
                .annotate(Annotation::Module),
            Type(content) => match content {
                Content::FlexVar(_) | Content::RigidVar(_) => alloc
                    .text(content_to_string(content, subs, home, interns))
                    .annotate(Annotation::TypeVariable),

                Content::Structure(_) => alloc
                    .text(content_to_string(content, subs, home, interns))
                    .annotate(Annotation::Structure),

                Content::Alias(_, _, _) => alloc
                    .text(content_to_string(content, subs, home, interns))
                    .annotate(Annotation::Alias),

                Content::Error => alloc.text(content_to_string(content, subs, home, interns)),
            },
            ErrorTypeInline(error_type) => alloc
                .nil()
                .append(alloc.hardline())
                .append(
                    alloc
                        .text(write_error_type(home, interns, error_type))
                        .indent(4),
                )
                .append(alloc.hardline()),

            ErrorTypeBlock(error_type) => alloc
                .nil()
                .append(alloc.hardline())
                .append(
                    error_type
                        .pretty(alloc, subs, home, src_lines, interns)
                        .indent(4)
                        .annotate(Annotation::TypeBlock),
                )
                .append(alloc.hardline()),

            Indent(n, nested) => {
                let rest = nested.pretty(alloc, subs, home, src_lines, interns);
                alloc.nil().append(rest).indent(n)
            }
            Docs(_) => {
                panic!("TODO implment docs");
            }
            Concat(report_texts) => alloc.concat(
                report_texts
                    .into_iter()
                    .map(|rep| rep.pretty(alloc, subs, home, src_lines, interns)),
            ),
            Stack(report_texts) => alloc
                .intersperse(
                    report_texts
                        .into_iter()
                        .map(|rep| (rep.pretty(alloc, subs, home, src_lines, interns))),
                    alloc.hardline(),
                )
                .append(alloc.hardline()),
            Intersperse { separator, items } => alloc.intersperse(
                items
                    .into_iter()
                    .map(|rep| (rep.pretty(alloc, subs, home, src_lines, interns)))
                    .collect::<Vec<_>>(),
                separator.pretty(alloc, subs, home, src_lines, interns),
            ),
            BinOp(bin_op) => alloc.text(bin_op.to_string()).annotate(Annotation::BinOp),
            Region(region) => {
                let max_line_number_length = (region.end_line + 1).to_string().len();
                let indent = 2;

                let body = if region.start_line == region.end_line {
                    let i = region.start_line;

                    let line_number_string = (i + 1).to_string();
                    let line_number = line_number_string;
                    let this_line_number_length = line_number.len();

                    let line = src_lines[i as usize];
                    let rest_of_line = if line.trim().is_empty() {
                        alloc.nil()
                    } else {
                        alloc
                            .nil()
                            .append(alloc.text(line).indent(2))
                            .annotate(Annotation::CodeBlock)
                    };

                    let source_line = alloc
                        .line()
                        .append(
                            alloc
                                .text(" ".repeat(max_line_number_length - this_line_number_length)),
                        )
                        .append(alloc.text(line_number).annotate(Annotation::LineNumber))
                        .append(alloc.text(" ┆").annotate(Annotation::GutterBar))
                        .append(rest_of_line);

                    let highlight_line = alloc
                        .line()
                        .append(alloc.text(" ".repeat(max_line_number_length)))
                        .append(alloc.text(" ┆").annotate(Annotation::GutterBar))
                        .append(
                            alloc
                                .text(" ".repeat(region.start_col as usize))
                                .indent(indent),
                        )
                        .append(
                            alloc
                                .text("^".repeat((region.end_col - region.start_col) as usize))
                                .annotate(Annotation::Error),
                        );

                    source_line.append(highlight_line)
                } else {
                    let mut result = alloc.nil();
                    for i in region.start_line..=region.end_line {
                        let line_number_string = (i + 1).to_string();
                        let line_number = line_number_string;
                        let this_line_number_length = line_number.len();

                        let line = src_lines[i as usize];
                        let rest_of_line = if !line.trim().is_empty() {
                            alloc
                                .text(line)
                                .annotate(Annotation::CodeBlock)
                                .indent(indent)
                        } else {
                            alloc.nil()
                        };

                        let source_line =
                            alloc
                                .line()
                                .append(alloc.text(
                                    " ".repeat(max_line_number_length - this_line_number_length),
                                ))
                                .append(alloc.text(line_number).annotate(Annotation::LineNumber))
                                .append(alloc.text(" ┆").annotate(Annotation::GutterBar))
                                .append(alloc.text(">").annotate(Annotation::Error))
                                .append(rest_of_line);

                        result = result.append(source_line);
                    }

                    result
                };
                alloc
                    .nil()
                    .append(alloc.line())
                    .append(body)
                    .append(alloc.line())
                    .append(alloc.line())
            }
            Name(ident) => alloc
                .text(format!("{}", ident.as_inline_str()))
                .annotate(Annotation::Symbol),
            TypeProblem(problem) => Self::type_problem_to_pretty(alloc, home, interns, problem),
        }
    }

    fn type_problem_to_pretty<'b, D>(
        alloc: &'b D,
        home: ModuleId,
        interns: &Interns,
        problem: crate::type_error::Problem,
    ) -> DocBuilder<'b, D, Annotation>
    where
        D: DocAllocator<'b, Annotation>,
        D::Doc: Clone,
    {
        use crate::type_error::suggest;
        use crate::type_error::Problem::*;

        match problem {
            FieldTypo(typo, possibilities) => {
                let suggestions = suggest::sort(typo.as_str(), possibilities);

                match suggestions.get(0) {
                    None => alloc.nil(),
                    Some(nearest) => {
                        let typo_str = format!("{}", typo);
                        let nearest_str = format!("{}", nearest);

                        let found = alloc.text(typo_str).annotate(Annotation::Typo);
                        let suggestion =
                            alloc.text(nearest_str).annotate(Annotation::TypoSuggestion);

                        let hint1 = Self::hint(alloc)
                            .append(alloc.reflow("Seems like a record field typo. Maybe "))
                            .append(found)
                            .append(alloc.reflow(" should be "))
                            .append(suggestion)
                            .append(alloc.text("?"));

                        let hint2 = Self::hint(alloc).append(alloc.reflow(ADD_ANNOTATIONS));

                        hint1
                            .append(alloc.line())
                            .append(alloc.line())
                            .append(hint2)
                    }
                }
            }
            TagTypo(typo, possibilities_tn) => {
                let possibilities = possibilities_tn
                    .into_iter()
                    .map(|tag_name| tag_name.into_string(interns, home))
                    .collect();
                let typo_str = format!("{}", typo.into_string(interns, home));
                let suggestions = suggest::sort(&typo_str, possibilities);

                match suggestions.get(0) {
                    None => alloc.nil(),
                    Some(nearest) => {
                        let nearest_str = format!("{}", nearest);

                        let found = alloc.text(typo_str).annotate(Annotation::Typo);
                        let suggestion =
                            alloc.text(nearest_str).annotate(Annotation::TypoSuggestion);

                        let hint1 = Self::hint(alloc)
                            .append(alloc.reflow("Seems like a tag typo. Maybe "))
                            .append(found)
                            .append(" should be ")
                            .append(suggestion)
                            .append(alloc.text("?"));

                        let hint2 = Self::hint(alloc).append(alloc.reflow(ADD_ANNOTATIONS));

                        hint1
                            .append(alloc.line())
                            .append(alloc.line())
                            .append(hint2)
                    }
                }
            }
            _ => todo!(),
        }
    }

    fn hint<'b, D>(alloc: &'b D) -> DocBuilder<'b, D, Annotation>
    where
        D: DocAllocator<'b, Annotation>,
        D::Doc: Clone,
    {
        alloc
            .text("Hint:")
            .append(alloc.softline())
            .annotate(Annotation::Hint)
    }
}
