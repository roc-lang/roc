use crate::report::ReportText::{BinOp, Concat, Module, Region, Value};
use bumpalo::Bump;
use roc_collections::all::MutSet;
use roc_module::ident::{Lowercase, TagName, Uppercase};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_problem::can::PrecedenceProblem::BothNonAssociative;
use roc_problem::can::{Problem, RuntimeError};
use roc_types::pretty_print::content_to_string;
use roc_types::subs::{Content, Subs};
use roc_types::types::{write_error_type, ErrorType};
use std::path::PathBuf;

use roc_module::ident::Ident;
use std::fmt;
use ven_pretty::{BoxAllocator, DocAllocator, DocBuilder, Render, RenderAnnotated};

const ADD_ANNOTATIONS: &str = r#"Can more type annotations be added? Type annotations always help me give more specific messages, and I think they could help a lot in this case"#;

/// A textual report.
pub struct Report {
    pub title: String,
    pub filename: PathBuf,
    pub text: ReportText,
}

impl Report {
    /// Render to CI console output, where no colors are available.
    pub fn render_ci(
        self,
        buf: &mut String,
        subs: &mut Subs,
        home: ModuleId,
        src_lines: &[&str],
        interns: &Interns,
    ) {
        let arena = Bump::new();
        let alloc = RocDocAllocator::new(&arena, subs, src_lines, home, interns);

        let err_msg = "<buffer is not a utf-8 encoded string>";

        self.pretty(&alloc)
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
        let arena = Bump::new();
        let alloc = RocDocAllocator::new(&arena, subs, src_lines, home, interns);

        let err_msg = "<buffer is not a utf-8 encoded string>";

        self.pretty(&alloc)
            .1
            .render_raw(70, &mut ColorWrite::new(palette, buf))
            .expect(err_msg);
    }

    fn pretty<'b>(self, alloc: &'b RocDocAllocator<'b>) -> RocDocBuilder<'b> {
        if self.title.is_empty() {
            self.text.pretty(alloc)
        } else {
            let header = format!(
                "-- {} {}",
                self.title,
                "-".repeat(80 - self.title.len() - 4)
            );

            alloc.stack(vec![alloc.text(header), self.text.pretty(alloc)])
        }
    }
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
            texts.push(ReportText::RuntimeError(RuntimeError::Shadowing {
                original_region,
                shadow,
            }));
        }
        Problem::RuntimeError(runtime_error) => {
            texts.push(ReportText::RuntimeError(runtime_error));
        }
    };

    Report {
        title: "SYNTAX PROBLEM".to_string(),
        filename,
        text: Concat(texts),
    }
}

fn pretty_runtime_error<'b>(
    alloc: &'b RocDocAllocator<'b>,
    runtime_error: RuntimeError,
) -> RocDocBuilder<'b> {
    match runtime_error {
        RuntimeError::Shadowing {
            original_region,
            shadow,
        } => alloc.stack(vec![
            alloc
                .text("The ")
                .append(alloc.ident(shadow.value))
                .append(alloc.reflow(" name is first defined here:")),
            alloc.region(original_region),
            alloc.reflow("But then it's defined a second time here:"),
            alloc.region(shadow.region),
            alloc.reflow(r#"Since these variables have the same name, it's easy to use the wrong one on accident. Give one of them a new name."#),
        ]),

        RuntimeError::LookupNotInScope(loc_name, options) => {
            not_found(alloc, loc_name.region, &loc_name.value, "value", options)
        }
        other => todo!("TODO implement run time error reporting for {:?}", other),
    }
}

fn not_found<'b>(
    alloc: &'b RocDocAllocator<'b>,
    region: roc_region::all::Region,
    name: &str,
    thing: &str,
    options: MutSet<Box<str>>,
) -> RocDocBuilder<'b> {
    use crate::type_error::suggest;

    let mut suggestions = suggest::sort(name, options.iter().map(|v| v.as_ref()).collect());
    suggestions.truncate(4);

    let default_no = alloc.concat(vec![
        alloc.reflow("Is there an "),
        alloc.keyword("import"),
        alloc.reflow(" or "),
        alloc.keyword("exposing"),
        alloc.reflow(" missing up-top"),
    ]);

    let default_yes = alloc.reflow("these names seem close though:");

    let to_details = |no_suggestion_details, yes_suggestion_details| {
        if suggestions.is_empty() {
            no_suggestion_details
        } else {
            alloc.stack(vec![
                yes_suggestion_details,
                alloc
                    .vcat(suggestions.into_iter().map(|v| alloc.string(v.to_string())))
                    .indent(4),
            ])
        }
    };

    alloc.stack(vec![
        alloc.string(format!("I cannot find a `{}` {}", name, thing)),
        alloc.region(region),
        to_details(default_no, default_yes),
    ])
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
    RuntimeError(RuntimeError),

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

// define custom allocator struct so we can `impl RocDocAllocator` custom helpers
pub struct RocDocAllocator<'a> {
    upstream: BoxAllocator,
    arena: &'a Bump,
    subs: &'a mut Subs,
    home: ModuleId,
    src_lines: &'a [&'a str],
    interns: &'a Interns,
}

type RocDocBuilder<'b> = DocBuilder<'b, RocDocAllocator<'b>, Annotation>;

impl<'a, A> DocAllocator<'a, A> for RocDocAllocator<'a>
where
    A: 'a,
{
    type Doc = ven_pretty::BoxDoc<'a, A>;

    fn alloc(&'a self, doc: ven_pretty::Doc<'a, Self::Doc, A>) -> Self::Doc {
        self.upstream.alloc(doc)
    }

    fn alloc_column_fn(
        &'a self,
        f: impl Fn(usize) -> Self::Doc + 'a,
    ) -> <Self::Doc as ven_pretty::DocPtr<'a, A>>::ColumnFn {
        self.upstream.alloc_column_fn(f)
    }

    fn alloc_width_fn(
        &'a self,
        f: impl Fn(isize) -> Self::Doc + 'a,
    ) -> <Self::Doc as ven_pretty::DocPtr<'a, A>>::WidthFn {
        self.upstream.alloc_width_fn(f)
    }
}

impl<'a> RocDocAllocator<'a> {
    pub fn new(
        arena: &'a Bump,
        subs: &'a mut Subs,
        src_lines: &'a [&'a str],
        home: ModuleId,
        interns: &'a Interns,
    ) -> Self {
        RocDocAllocator {
            upstream: BoxAllocator,
            arena,
            subs,
            home,
            src_lines,
            interns,
        }
    }

    fn vcat<A, I>(&'a self, docs: I) -> DocBuilder<'a, Self, A>
    where
        A: 'a + Clone,
        I: IntoIterator,
        I::Item: Into<ven_pretty::BuildDoc<'a, ven_pretty::BoxDoc<'a, A>, A>>,
    {
        self.intersperse(docs, self.line())
    }

    fn stack<A, I>(&'a self, docs: I) -> DocBuilder<'a, Self, A>
    where
        A: 'a + Clone,
        I: IntoIterator,
        I::Item: Into<ven_pretty::BuildDoc<'a, ven_pretty::BoxDoc<'a, A>, A>>,
    {
        self.intersperse(docs, self.line().append(self.line()))
    }

    pub fn string(&'a self, string: String) -> DocBuilder<'a, Self, Annotation> {
        let x: std::borrow::Cow<'a, str> = string.into();

        self.text(x)
    }

    pub fn keyword(&'a self, string: &'a str) -> DocBuilder<'a, Self, Annotation> {
        self.text(string).annotate(Annotation::Keyword)
    }

    pub fn tag_name(&'a self, tn: TagName) -> DocBuilder<'a, Self, Annotation> {
        match tn {
            TagName::Global(uppercase) => self.global_tag_name(uppercase),
            TagName::Private(symbol) => self.private_tag_name(symbol),
        }
    }

    pub fn symbol_unqualified(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        self.text(format!("{}", symbol.ident_string(self.interns)))
            .annotate(Annotation::Symbol)
    }
    pub fn symbol_foreign_qualified(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        if symbol.module_id() == self.home || symbol.module_id().is_builtin() {
            // Render it unqualified if it's in the current module or a builtin
            self.text(format!("{}", symbol.ident_string(self.interns)))
                .annotate(Annotation::Symbol)
        } else {
            self.text(format!(
                "{}.{}",
                symbol.module_string(self.interns),
                symbol.ident_string(self.interns),
            ))
            .annotate(Annotation::Symbol)
        }
    }
    pub fn symbol_qualified(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        self.text(format!(
            "{}.{}",
            symbol.module_string(self.interns),
            symbol.ident_string(self.interns),
        ))
        .annotate(Annotation::Symbol)
    }

    pub fn private_tag_name(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        if symbol.module_id() == self.home {
            // Render it unqualified if it's in the current module.
            self.text(format!("{}", symbol.ident_string(self.interns)))
                .annotate(Annotation::PrivateTag)
        } else {
            self.text(format!(
                "{}.{}",
                symbol.module_string(self.interns),
                symbol.ident_string(self.interns),
            ))
            .annotate(Annotation::PrivateTag)
        }
    }

    pub fn global_tag_name(&'a self, uppercase: Uppercase) -> DocBuilder<'a, Self, Annotation> {
        self.text(format!("{}", uppercase))
            .annotate(Annotation::GlobalTag)
    }
    pub fn record_field(&'a self, lowercase: Lowercase) -> DocBuilder<'a, Self, Annotation> {
        self.text(format!(".{}", lowercase))
            .annotate(Annotation::RecordField)
    }

    pub fn region(&'a self, region: roc_region::all::Region) -> DocBuilder<'a, Self, Annotation> {
        let max_line_number_length = (region.end_line + 1).to_string().len();
        let indent = 2;

        if region.start_line == region.end_line {
            let i = region.start_line;

            let line_number_string = (i + 1).to_string();
            let line_number = line_number_string;
            let this_line_number_length = line_number.len();

            let line = self.src_lines[i as usize];
            let rest_of_line = if line.trim().is_empty() {
                self.nil()
            } else {
                self.nil()
                    .append(self.text(line).indent(2))
                    .annotate(Annotation::CodeBlock)
            };

            let source_line = self
                .text(" ".repeat(max_line_number_length - this_line_number_length))
                .append(self.text(line_number).annotate(Annotation::LineNumber))
                .append(self.text(" ┆").annotate(Annotation::GutterBar))
                .append(rest_of_line);

            let highlight_line = self
                .line()
                .append(self.text(" ".repeat(max_line_number_length)))
                .append(self.text(" ┆").annotate(Annotation::GutterBar))
                .append(
                    self.text(" ".repeat(region.start_col as usize))
                        .indent(indent),
                )
                .append(
                    self.text("^".repeat((region.end_col - region.start_col) as usize))
                        .annotate(Annotation::Error),
                );

            source_line.append(highlight_line)
        } else {
            let mut result = self.nil();
            for i in region.start_line..=region.end_line {
                let line_number_string = (i + 1).to_string();
                let line_number = line_number_string;
                let this_line_number_length = line_number.len();

                let line = self.src_lines[i as usize];
                let rest_of_line = if !line.trim().is_empty() {
                    self.text(line)
                        .annotate(Annotation::CodeBlock)
                        .indent(indent)
                } else {
                    self.nil()
                };

                let source_line = self
                    .text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(" ┆").annotate(Annotation::GutterBar))
                    .append(self.text(">").annotate(Annotation::Error))
                    .append(rest_of_line);

                result = result.append(source_line);
            }

            result
        }
    }

    pub fn ident(&'a self, ident: Ident) -> DocBuilder<'a, Self, Annotation> {
        self.text(format!("{}", ident.as_inline_str()))
            .annotate(Annotation::Symbol)
    }
}

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
    /// General idea: this function puts all the characters in. Any styling (emphasis, colors,
    /// monospace font, etc) is done in the CiWrite and ColorWrite `RenderAnnotated` instances.
    pub fn pretty<'b>(
        self,
        alloc: &'b RocDocAllocator<'b>,
    ) -> DocBuilder<'b, RocDocAllocator<'b>, Annotation> {
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
            PrivateTag(symbol) => alloc.private_tag_name(symbol),
            Value(symbol) => alloc.symbol_foreign_qualified(symbol),

            Module(module_id) => alloc
                .text(format!("{}", alloc.interns.module_name(module_id)))
                .annotate(Annotation::Module),
            Type(content) => match content {
                Content::FlexVar(_) | Content::RigidVar(_) => alloc
                    .text(content_to_string(
                        content,
                        alloc.subs,
                        alloc.home,
                        alloc.interns,
                    ))
                    .annotate(Annotation::TypeVariable),

                Content::Structure(_) => alloc
                    .text(content_to_string(
                        content,
                        alloc.subs,
                        alloc.home,
                        alloc.interns,
                    ))
                    .annotate(Annotation::Structure),

                Content::Alias(_, _, _) => alloc
                    .text(content_to_string(
                        content,
                        alloc.subs,
                        alloc.home,
                        alloc.interns,
                    ))
                    .annotate(Annotation::Alias),

                Content::Error => alloc.text(content_to_string(
                    content,
                    alloc.subs,
                    alloc.home,
                    alloc.interns,
                )),
            },
            ErrorTypeInline(error_type) => alloc
                .nil()
                .append(alloc.hardline())
                .append(
                    alloc
                        .text(write_error_type(alloc.home, alloc.interns, error_type))
                        .indent(4),
                )
                .append(alloc.hardline()),

            ErrorTypeBlock(error_type) => alloc
                .nil()
                .append(alloc.hardline())
                .append(
                    error_type
                        .pretty(alloc)
                        .indent(4)
                        .annotate(Annotation::TypeBlock),
                )
                .append(alloc.hardline()),

            Indent(n, nested) => {
                let rest = nested.pretty(alloc);
                alloc.nil().append(rest).indent(n)
            }
            Docs(_) => {
                panic!("TODO implment docs");
            }
            Concat(report_texts) => {
                alloc.concat(report_texts.into_iter().map(|rep| rep.pretty(alloc)))
            }
            Stack(report_texts) => alloc
                .intersperse(
                    report_texts.into_iter().map(|rep| (rep.pretty(alloc))),
                    alloc.hardline(),
                )
                .append(alloc.hardline()),
            Intersperse { separator, items } => alloc.intersperse(
                items
                    .into_iter()
                    .map(|rep| (rep.pretty(alloc)))
                    .collect::<Vec<_>>(),
                separator.pretty(alloc),
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

                    let line = alloc.src_lines[i as usize];
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

                        let line = alloc.src_lines[i as usize];
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
            TypeProblem(problem) => Self::type_problem_to_pretty(alloc, problem),
            RuntimeError(problem) => pretty_runtime_error(alloc, problem),
        }
    }

    fn type_problem_to_pretty<'b>(
        alloc: &'b RocDocAllocator<'b>,
        problem: crate::type_error::Problem,
    ) -> DocBuilder<'b, RocDocAllocator<'b>, Annotation> {
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
            FieldsMissing(missing) => match missing.split_last() {
                None => alloc.nil(),
                Some((f1, [])) => Self::hint(alloc)
                    .append(alloc.reflow("Looks like the "))
                    .append(f1.as_str().to_owned())
                    .append(alloc.reflow(" field is missing.")),
                Some((last, init)) => {
                    let separator = alloc.reflow(", ");

                    Self::hint(alloc)
                        .append(alloc.reflow("Looks like the "))
                        .append(
                            alloc
                                .intersperse(init.iter().map(|v| v.as_str().to_owned()), separator),
                        )
                        .append(alloc.reflow(" and "))
                        .append(alloc.text(last.as_str().to_owned()))
                        .append(alloc.reflow(" fields are missing."))
                }
            },
            TagTypo(typo, possibilities_tn) => {
                let possibilities = possibilities_tn
                    .into_iter()
                    .map(|tag_name| tag_name.into_string(alloc.interns, alloc.home))
                    .collect();
                let typo_str = format!("{}", typo.into_string(alloc.interns, alloc.home));
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
            ArityMismatch(found, expected) => {
                let line = if found < expected {
                    format!(
                        "It looks like it takes too few arguments. I was expecting {} more.",
                        expected - found
                    )
                } else {
                    format!(
                        "It looks like it takes too many arguments. I'm seeing {} extra.",
                        found - expected
                    )
                };

                Self::hint(alloc).append(line)
            }

            BadRigidVar(x, tipe) => {
                use ErrorType::*;

                let bad_rigid_var = |name: &str, a_thing| {
                    let text = format!(
                        r#"The type annotation uses the type variable `{}` to say that this definition can produce any type of value. But in the body I see that it will only produce {} of a single specific type. Maybe change the type annotation to be more specific? Maybe change the code to be more general?"#,
                        name, a_thing
                    );

                    Self::hint(alloc).append(alloc.reflow(alloc.arena.alloc(text)))
                };

                let bad_double_rigid = |a, b| {
                    let text = format!(
                        r#"Your type annotation uses {} and {} as separate type variables. Your code seems to be saying they are the same though. Maybe they should be the same your type annotation? Maybe your code uses them in a weird way?"#,
                        a, b
                    );

                    Self::hint(alloc).append(alloc.reflow(alloc.arena.alloc(text)))
                };

                match tipe {
                    Infinite | Error | FlexVar(_) => alloc.nil(),
                    RigidVar(y) => bad_double_rigid(x.as_str(), y.as_str()),
                    Function(_, _) => bad_rigid_var(x.as_str(), "a function value"),
                    Record(_, _) => bad_rigid_var(x.as_str(), "a record value"),
                    TagUnion(_, _) | RecursiveTagUnion(_, _, _) => {
                        bad_rigid_var(x.as_str(), "a tag value")
                    }
                    Alias(symbol, _, _) | Type(symbol, _) => bad_rigid_var(
                        x.as_str(),
                        &format!("a {} value", symbol.ident_string(alloc.interns)),
                    ),
                    Boolean(_) => bad_rigid_var(x.as_str(), "a uniqueness attribute value"),
                }
            }

            _ => todo!(),
        }
    }

    fn hint<'b>(alloc: &'b RocDocAllocator<'b>) -> DocBuilder<'b, RocDocAllocator<'b>, Annotation> {
        alloc
            .text("Hint:")
            .append(alloc.softline())
            .annotate(Annotation::Hint)
    }
}
