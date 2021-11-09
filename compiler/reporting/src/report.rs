use roc_module::ident::Ident;
use roc_module::ident::{Lowercase, ModuleName, TagName, Uppercase};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use std::fmt;
use std::path::PathBuf;
use ven_pretty::{BoxAllocator, DocAllocator, DocBuilder, Render, RenderAnnotated};

pub use crate::error::canonicalize::can_problem;
pub use crate::error::mono::mono_problem;
pub use crate::error::parse::parse_problem;
pub use crate::error::r#type::type_problem;

// const IS_WINDOWS: bool = std::env::consts::OS == "windows";
const IS_WINDOWS: bool = false;

// trick to branch in a const. Can be replaced by an if when that is merged into rustc
const CYCLE_TOP: &str = ["+-----+", "┌─────┐"][(!IS_WINDOWS) as usize];
const CYCLE_LN: &str = ["|     ", "│     "][!IS_WINDOWS as usize];
const CYCLE_MID: &str = ["|     |", "│     ↓"][!IS_WINDOWS as usize];
const CYCLE_END: &str = ["+-<---+", "└─────┘"][!IS_WINDOWS as usize];

const GUTTER_BAR: &str = "│";
const ERROR_UNDERLINE: &str = "^";

/// The number of monospace spaces the gutter bar takes up.
/// (This is not necessarily the same as GUTTER_BAR.len()!)
const GUTTER_BAR_WIDTH: usize = 1;

pub fn cycle<'b>(
    alloc: &'b RocDocAllocator<'b>,
    indent: usize,
    name: RocDocBuilder<'b>,
    names: Vec<RocDocBuilder<'b>>,
) -> RocDocBuilder<'b> {
    let mut lines = Vec::with_capacity(4 + (2 * names.len() - 1));

    lines.push(alloc.text(CYCLE_TOP));

    lines.push(alloc.text(CYCLE_LN).append(name));
    lines.push(alloc.text(CYCLE_MID));

    let mut it = names.into_iter().peekable();

    while let Some(other_name) = it.next() {
        lines.push(alloc.text(CYCLE_LN).append(other_name));

        if it.peek().is_some() {
            lines.push(alloc.text(CYCLE_MID));
        }
    }

    lines.push(alloc.text(CYCLE_END));

    alloc
        .vcat(lines)
        .indent(indent)
        .annotate(Annotation::TypeBlock)
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Severity {
    /// This will cause a runtime error if some code get srun
    /// (e.g. type mismatch, naming error)
    RuntimeError,

    /// This will never cause the code to misbehave,
    /// but should be cleaned up
    /// (e.g. unused def, unused import)
    Warning,
}

/// A textual report.
pub struct Report<'b> {
    pub title: String,
    pub filename: PathBuf,
    pub doc: RocDocBuilder<'b>,
    pub severity: Severity,
}

impl<'b> Report<'b> {
    /// Render to CI console output, where no colors are available.
    pub fn render_ci(self, buf: &'b mut String, alloc: &'b RocDocAllocator<'b>) {
        let err_msg = "<buffer is not a utf-8 encoded string>";

        self.pretty(alloc)
            .1
            .render_raw(70, &mut CiWrite::new(buf))
            .expect(err_msg);
    }

    /// Render to a color terminal using ANSI escape sequences
    pub fn render_color_terminal(
        self,
        buf: &mut String,
        alloc: &'b RocDocAllocator<'b>,
        palette: &'b Palette,
    ) {
        let err_msg = "<buffer is not a utf-8 encoded string>";

        self.pretty(alloc)
            .1
            .render_raw(70, &mut ColorWrite::new(palette, buf))
            .expect(err_msg);
    }

    pub fn pretty(self, alloc: &'b RocDocAllocator<'b>) -> RocDocBuilder<'b> {
        if self.title.is_empty() {
            self.doc
        } else {
            let header = format!(
                "── {} {}",
                self.title,
                "─".repeat(80 - (self.title.len() + 4))
            );

            alloc.stack(vec![
                alloc.text(header).annotate(Annotation::Header),
                self.doc,
            ])
        }
    }

    pub fn horizontal_rule(palette: &'b Palette) -> String {
        format!("{}{}", palette.header, "─".repeat(80))
    }
}

pub struct Palette<'a> {
    pub primary: &'a str,
    pub code_block: &'a str,
    pub keyword: &'a str,
    pub variable: &'a str,
    pub type_variable: &'a str,
    pub structure: &'a str,
    pub alias: &'a str,
    pub error: &'a str,
    pub line_number: &'a str,
    pub header: &'a str,
    pub gutter_bar: &'a str,
    pub module_name: &'a str,
    pub binop: &'a str,
    pub typo: &'a str,
    pub typo_suggestion: &'a str,
    pub parser_suggestion: &'a str,
}

pub const DEFAULT_PALETTE: Palette = Palette {
    primary: WHITE_CODE,
    code_block: WHITE_CODE,
    keyword: GREEN_CODE,
    variable: BLUE_CODE,
    type_variable: YELLOW_CODE,
    structure: GREEN_CODE,
    alias: YELLOW_CODE,
    error: RED_CODE,
    line_number: CYAN_CODE,
    header: CYAN_CODE,
    gutter_bar: CYAN_CODE,
    module_name: GREEN_CODE,
    binop: GREEN_CODE,
    typo: YELLOW_CODE,
    typo_suggestion: GREEN_CODE,
    parser_suggestion: YELLOW_CODE,
};

pub const RED_CODE: &str = "\u{001b}[31m";
pub const GREEN_CODE: &str = "\u{001b}[32m";
pub const YELLOW_CODE: &str = "\u{001b}[33m";
pub const BLUE_CODE: &str = "\u{001b}[34m";
pub const MAGENTA_CODE: &str = "\u{001b}[35m";
pub const CYAN_CODE: &str = "\u{001b}[36m";
pub const WHITE_CODE: &str = "\u{001b}[37m";

pub const BOLD_CODE: &str = "\u{001b}[1m";

pub const UNDERLINE_CODE: &str = "\u{001b}[4m";

pub const RESET_CODE: &str = "\u{001b}[0m";

// define custom allocator struct so we can `impl RocDocAllocator` custom helpers
pub struct RocDocAllocator<'a> {
    upstream: BoxAllocator,
    pub src_lines: &'a [&'a str],
    pub home: ModuleId,
    pub interns: &'a Interns,
}

pub type RocDocBuilder<'b> = DocBuilder<'b, RocDocAllocator<'b>, Annotation>;

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
    pub fn new(src_lines: &'a [&'a str], home: ModuleId, interns: &'a Interns) -> Self {
        RocDocAllocator {
            upstream: BoxAllocator,
            home,
            src_lines,
            interns,
        }
    }

    /// vertical concatenation. Adds a newline between elements
    pub fn vcat<A, I>(&'a self, docs: I) -> DocBuilder<'a, Self, A>
    where
        A: 'a + Clone,
        I: IntoIterator,
        I::Item: Into<ven_pretty::BuildDoc<'a, ven_pretty::BoxDoc<'a, A>, A>>,
    {
        self.intersperse(docs, self.line())
    }

    /// like vcat, but adds a double line break between elements. Visually this means an empty line
    /// between elements.
    pub fn stack<A, I>(&'a self, docs: I) -> DocBuilder<'a, Self, A>
    where
        A: 'a + Clone,
        I: IntoIterator,
        I::Item: Into<ven_pretty::BuildDoc<'a, ven_pretty::BoxDoc<'a, A>, A>>,
    {
        self.intersperse(docs, self.line().append(self.line()))
    }

    /// text from a String. Note that this does not reflow!
    pub fn string(&'a self, string: String) -> DocBuilder<'a, Self, Annotation> {
        let x: std::borrow::Cow<'a, str> = string.into();

        self.text(x)
    }

    pub fn keyword(&'a self, string: &'a str) -> DocBuilder<'a, Self, Annotation> {
        self.text(string).annotate(Annotation::Keyword)
    }

    pub fn parser_suggestion(&'a self, string: &'a str) -> DocBuilder<'a, Self, Annotation> {
        self.text(string).annotate(Annotation::ParserSuggestion)
    }

    pub fn type_str(&'a self, content: &str) -> DocBuilder<'a, Self, Annotation> {
        self.string(content.to_owned()).annotate(Annotation::Alias)
    }

    pub fn type_variable(&'a self, content: Lowercase) -> DocBuilder<'a, Self, Annotation> {
        // currently not annotated
        self.string(content.to_string())
            .annotate(Annotation::TypeVariable)
    }

    pub fn tag_name(&'a self, tn: TagName) -> DocBuilder<'a, Self, Annotation> {
        match tn {
            TagName::Global(uppercase) => self.global_tag_name(uppercase),
            TagName::Private(symbol) => self.private_tag_name(symbol),
            TagName::Closure(_symbol) => unreachable!("closure tags are internal only"),
        }
    }

    pub fn symbol_unqualified(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        self.text(format!("{}", symbol.ident_str(self.interns)))
            .annotate(Annotation::Symbol)
    }
    pub fn symbol_foreign_qualified(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        if symbol.module_id() == self.home || symbol.module_id().is_builtin() {
            // Render it unqualified if it's in the current module or a builtin
            self.text(format!("{}", symbol.ident_str(self.interns)))
                .annotate(Annotation::Symbol)
        } else {
            self.text(format!(
                "{}.{}",
                symbol.module_string(self.interns),
                symbol.ident_str(self.interns),
            ))
            .annotate(Annotation::Symbol)
        }
    }
    pub fn symbol_qualified(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        self.text(format!(
            "{}.{}",
            symbol.module_string(self.interns),
            symbol.ident_str(self.interns),
        ))
        .annotate(Annotation::Symbol)
    }

    pub fn private_tag_name(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        if symbol.module_id() == self.home {
            // Render it unqualified if it's in the current module.
            self.text(format!("{}", symbol.ident_str(self.interns)))
                .annotate(Annotation::PrivateTag)
        } else {
            self.text(format!(
                "{}.{}",
                symbol.module_string(self.interns),
                symbol.ident_str(self.interns),
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

    pub fn module(&'a self, module_id: ModuleId) -> DocBuilder<'a, Self, Annotation> {
        let name = self.interns.module_name(module_id);
        let name = if name.is_empty() {
            // Render the app module as "app"
            "app".to_string()
        } else {
            name.to_string()
        };

        self.text(name).annotate(Annotation::Module)
    }

    pub fn module_name(&'a self, name: ModuleName) -> DocBuilder<'a, Self, Annotation> {
        let name = if name.is_empty() {
            // Render the app module as "app"
            "app".to_string()
        } else {
            name.as_str().to_string()
        };

        self.text(name).annotate(Annotation::Module)
    }

    pub fn binop(
        &'a self,
        content: roc_module::operator::BinOp,
    ) -> DocBuilder<'a, Self, Annotation> {
        self.text(content.to_string()).annotate(Annotation::BinOp)
    }

    /// Turns of backticks/colors in a block
    pub fn type_block(
        &'a self,
        content: DocBuilder<'a, Self, Annotation>,
    ) -> DocBuilder<'a, Self, Annotation> {
        content.annotate(Annotation::TypeBlock).indent(4)
    }

    pub fn tip(&'a self) -> DocBuilder<'a, Self, Annotation> {
        self.text("Tip")
            .annotate(Annotation::Tip)
            .append(":")
            .append(self.softline())
    }

    pub fn note(&'a self, line: &'a str) -> DocBuilder<'a, Self, Annotation> {
        self.text("Note")
            .annotate(Annotation::Tip)
            .append(": ")
            .append(line)
    }

    pub fn hint(&'a self, line: &'a str) -> DocBuilder<'a, Self, Annotation> {
        self.text("Hint")
            .annotate(Annotation::Tip)
            .append(": ")
            .append(line)
    }

    pub fn region_all_the_things(
        &'a self,
        region: roc_region::all::Region,
        sub_region1: roc_region::all::Region,
        sub_region2: roc_region::all::Region,
        error_annotation: Annotation,
    ) -> DocBuilder<'a, Self, Annotation> {
        debug_assert!(region.contains(&sub_region1));
        debug_assert!(region.contains(&sub_region2));

        // if true, the final line of the snippet will be some ^^^ that point to the region where
        // the problem is. Otherwise, the snippet will have a > on the lines that are in the region
        // where the problem is.
        let error_highlight_line = region.start_line == region.end_line;

        let max_line_number_length = (region.end_line + 1).to_string().len();
        let indent = 2;

        let mut result = self.nil();
        for i in region.start_line..=region.end_line {
            let line_number_string = (i + 1).to_string();
            let line_number = line_number_string;
            let this_line_number_length = line_number.len();

            let line = self.src_lines[i as usize];

            let rest_of_line = if !line.trim().is_empty() {
                self.text(line).indent(indent)
            } else {
                self.nil()
            };

            let highlight = !error_highlight_line
                && ((i >= sub_region1.start_line && i <= sub_region1.end_line)
                    || (i >= sub_region2.start_line && i <= sub_region2.end_line));

            let source_line = if highlight {
                self.text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                    .append(self.text(">").annotate(error_annotation))
                    .append(rest_of_line)
            } else if error_highlight_line {
                self.text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                    .append(rest_of_line)
            } else {
                self.text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                    .append(self.text(" "))
                    .append(rest_of_line)
            };

            result = result.append(source_line);

            if i != region.end_line {
                result = result.append(self.line())
            }
        }

        if error_highlight_line {
            let overlapping = sub_region2.start_col < sub_region1.end_col;

            let highlight = if overlapping {
                self.text(
                    ERROR_UNDERLINE.repeat((sub_region2.end_col - sub_region1.start_col) as usize),
                )
            } else {
                let highlight1 =
                    ERROR_UNDERLINE.repeat((sub_region1.end_col - sub_region1.start_col) as usize);
                let highlight2 = if sub_region1 == sub_region2 {
                    "".repeat(0)
                } else {
                    ERROR_UNDERLINE.repeat((sub_region2.end_col - sub_region2.start_col) as usize)
                };
                let in_between = " "
                    .repeat((sub_region2.start_col.saturating_sub(sub_region1.end_col)) as usize);

                self.text(highlight1)
                    .append(self.text(in_between))
                    .append(self.text(highlight2))
            };

            let highlight_line = self
                .line()
                // Omit the gutter bar when we know there are no further
                // line numbers to be printed after this!
                .append(self.text(" ".repeat(max_line_number_length + GUTTER_BAR_WIDTH)))
                .append(if sub_region1.is_empty() && sub_region2.is_empty() {
                    self.nil()
                } else {
                    self.text(" ".repeat(sub_region1.start_col as usize))
                        .indent(indent)
                        .append(highlight)
                        .annotate(error_annotation)
                });

            result = result.append(highlight_line);
        }

        result.annotate(Annotation::CodeBlock)
    }

    pub fn region_with_subregion(
        &'a self,
        region: roc_region::all::Region,
        sub_region: roc_region::all::Region,
    ) -> DocBuilder<'a, Self, Annotation> {
        // debug_assert!(region.contains(&sub_region));

        // If the outer region takes more than 1 full screen (~60 lines), only show the inner region
        if region.end_line - region.start_line > 60 {
            return self.region_with_subregion(sub_region, sub_region);
        }

        // if true, the final line of the snippet will be some ^^^ that point to the region where
        // the problem is. Otherwise, the snippet will have a > on the lines that are in the region
        // where the problem is.
        let error_highlight_line = sub_region.start_line == region.end_line;

        let max_line_number_length = (region.end_line + 1).to_string().len();
        let indent = 2;

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

            let source_line = if !error_highlight_line
                && i >= sub_region.start_line
                && i <= sub_region.end_line
            {
                self.text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                    .append(self.text(">").annotate(Annotation::Error))
                    .append(rest_of_line)
            } else if error_highlight_line {
                self.text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                    .append(rest_of_line)
            } else {
                self.text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                    .append(self.text(" "))
                    .append(rest_of_line)
            };

            result = result.append(source_line);

            if i != region.end_line {
                result = result.append(self.line())
            }
        }

        if error_highlight_line {
            let highlight_text =
                ERROR_UNDERLINE.repeat((sub_region.end_col - sub_region.start_col) as usize);

            let highlight_line = self
                .line()
                // Omit the gutter bar when we know there are no further
                // line numbers to be printed after this!
                .append(self.text(" ".repeat(max_line_number_length + GUTTER_BAR_WIDTH)))
                .append(if highlight_text.is_empty() {
                    self.nil()
                } else {
                    self.text(" ".repeat(sub_region.start_col as usize))
                        .indent(indent)
                        .append(self.text(highlight_text).annotate(Annotation::Error))
                });

            result = result.append(highlight_line);
        }

        result
    }

    pub fn region(&'a self, region: roc_region::all::Region) -> DocBuilder<'a, Self, Annotation> {
        self.region_with_subregion(region, region)
    }

    pub fn region_without_error(
        &'a self,
        region: roc_region::all::Region,
    ) -> DocBuilder<'a, Self, Annotation> {
        let mut result = self.nil();
        for i in region.start_line..=region.end_line {
            let line = if i == region.start_line {
                if i == region.end_line {
                    &self.src_lines[i as usize][region.start_col as usize..region.end_col as usize]
                } else {
                    &self.src_lines[i as usize][region.start_col as usize..]
                }
            } else if i == region.end_line {
                &self.src_lines[i as usize][0..region.end_col as usize]
            } else {
                self.src_lines[i as usize]
            };

            let rest_of_line = if !line.trim().is_empty() {
                self.text(line).annotate(Annotation::CodeBlock)
            } else {
                self.nil()
            };

            result = result.append(rest_of_line);

            if i != region.end_line {
                result = result.append(self.line())
            }
        }

        result.indent(4)
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
    Tip,
    Header,
    ParserSuggestion,
}

/// Render with minimal formatting
pub struct CiWrite<W> {
    style_stack: Vec<Annotation>,
    in_type_block: bool,
    in_code_block: bool,
    upstream: W,
}

impl<W> CiWrite<W> {
    pub fn new(upstream: W) -> CiWrite<W> {
        CiWrite {
            style_stack: vec![],
            in_type_block: false,
            in_code_block: false,
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
            CodeBlock => {
                self.in_code_block = true;
            }
            Emphasized => {
                self.write_str("*")?;
            }
            Url => {
                self.write_str("<")?;
            }
            GlobalTag | PrivateTag | Keyword | RecordField | Symbol | Typo | TypoSuggestion
            | TypeVariable
                if !self.in_type_block && !self.in_code_block =>
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
                CodeBlock => {
                    self.in_code_block = false;
                }
                Emphasized => {
                    self.write_str("*")?;
                }
                Url => {
                    self.write_str(">")?;
                }
                GlobalTag | PrivateTag | Keyword | RecordField | Symbol | Typo | TypoSuggestion
                | TypeVariable
                    if !self.in_type_block && !self.in_code_block =>
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
            Url | Tip => {
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
            Keyword => {
                self.write_str(self.palette.keyword)?;
            }
            GutterBar => {
                self.write_str(self.palette.gutter_bar)?;
            }
            Error => {
                self.write_str(self.palette.error)?;
            }
            Header => {
                self.write_str(self.palette.header)?;
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
            ParserSuggestion => {
                self.write_str(self.palette.parser_suggestion)?;
            }
            TypeBlock | GlobalTag | PrivateTag | RecordField => { /* nothing yet */ }
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
                | Typo | TypoSuggestion | ParserSuggestion | Structure | CodeBlock | PlainText
                | LineNumber | Tip | Module | Header | Keyword => {
                    self.write_str(RESET_CODE)?;
                }

                TypeBlock | GlobalTag | PrivateTag | RecordField => { /* nothing yet */ }
            },
        }
        Ok(())
    }
}
