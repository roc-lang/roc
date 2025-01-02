use roc_module::ident::Ident;
use roc_module::ident::{Lowercase, ModuleName, TagName, Uppercase};
use roc_module::symbol::{Interns, ModuleId, ModuleIds, PQModuleName, PackageQualified, Symbol};
use roc_problem::Severity;
use roc_region::all::LineColumnRegion;
use std::path::{Path, PathBuf};
use std::{fmt, io};
use ven_pretty::{text, BoxAllocator, DocAllocator, DocBuilder, Render, RenderAnnotated};

#[cfg(not(target_family = "wasm"))]
use byte_unit::Byte;
#[cfg(not(target_family = "wasm"))]
use roc_packaging::https::Problem;

pub use crate::error::canonicalize::can_problem;
pub use crate::error::parse::parse_problem;
pub use crate::error::r#type::type_problem;

#[cfg(windows)]
const CYCLE_ELEMENTS: [&str; 4] = ["+-----+", "|     ", "|     |", "+-<---+"];

#[cfg(not(windows))]
const CYCLE_ELEMENTS: [&str; 4] = ["┌─────┐", "│     ", "│     ↓", "└─────┘"];

const CYCLE_TOP: &str = CYCLE_ELEMENTS[0];
const CYCLE_LN: &str = CYCLE_ELEMENTS[1];
const CYCLE_MID: &str = CYCLE_ELEMENTS[2];
const CYCLE_END: &str = CYCLE_ELEMENTS[3];

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

const HEADER_WIDTH: usize = 80;

pub fn pretty_header(title: &str) -> String {
    let title_width = title.len() + 4;
    let header = format!("── {} {}", title, "─".repeat(HEADER_WIDTH - title_width));
    header
}

pub fn pretty_header_with_path(title: &str, path: &Path) -> String {
    let cwd = std::env::current_dir().unwrap();
    let relative_path = match path.strip_prefix(cwd) {
        Ok(p) => p,
        _ => path,
    }
    .to_str()
    .unwrap();

    let additional_path_display = "in";
    let additional_path_display_width = additional_path_display.len() + 1;
    let title_width = title.len() + 4;
    let relative_path_width = relative_path.len() + 1;
    let available_path_width = HEADER_WIDTH - title_width - additional_path_display_width - 1;

    // If path is too long to fit in 80 characters with everything else then truncate it
    let path_width = relative_path_width.min(available_path_width);
    let path_trim = relative_path_width - path_width;
    let path = if path_trim > 0 {
        format!("...{}", &relative_path[(path_trim + 3)..])
    } else {
        relative_path.to_string()
    };

    // ensure path conatians only unix slashes
    let path = path.replace('\\', "/");

    let header = format!(
        "── {} {} {} {}",
        title,
        additional_path_display,
        path,
        "─".repeat(HEADER_WIDTH - (title_width + path_width + additional_path_display_width))
    );

    header
}

#[derive(Clone, Copy, Debug)]
pub enum RenderTarget {
    ColorTerminal,
    Generic,
    LanguageServer,
}

/// A textual report.
pub struct Report<'b> {
    pub title: String,
    pub filename: PathBuf,
    pub doc: RocDocBuilder<'b>,
    pub severity: Severity,
}

impl<'b> Report<'b> {
    pub fn render(
        self,
        target: RenderTarget,
        buf: &'b mut String,
        alloc: &'b RocDocAllocator<'b>,
        palette: &'b Palette,
    ) {
        match target {
            RenderTarget::Generic => self.render_ci(buf, alloc),
            RenderTarget::ColorTerminal => self.render_color_terminal(buf, alloc, palette),
            RenderTarget::LanguageServer => self.render_language_server(buf, alloc),
        }
    }

    /// Render to CI console output, where no colors are available.
    pub fn render_ci(self, buf: &mut String, alloc: &'b RocDocAllocator<'b>) {
        let err_msg = "<buffer is not a utf-8 encoded string>";

        self.pretty(alloc)
            .1
            .render_raw(70, &mut CiWrite::new(buf))
            .expect(err_msg);
    }

    /// Render to a color terminal using ANSI escape sequences,
    /// or to the web REPL, using HTML tags.
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
            let header = if self.filename == PathBuf::from("replfile.roc") {
                crate::report::pretty_header(&self.title)
            } else {
                crate::report::pretty_header_with_path(&self.title, &self.filename)
            };

            alloc.stack([alloc.text(header).annotate(Annotation::Header), self.doc])
        }
    }

    /// Render report for the language server, where the window is narrower.
    /// Path is not included, and the header is not emphasized with "─".
    pub fn render_language_server(self, buf: &mut String, alloc: &'b RocDocAllocator<'b>) {
        let err_msg = "<buffer is not a utf-8 encoded string>";

        alloc
            .stack([alloc.text(self.title), self.doc])
            .1
            .render_raw(60, &mut CiWrite::new(buf))
            .expect(err_msg)
    }

    pub fn horizontal_rule(palette: &'b Palette) -> String {
        format!("{}{}", palette.header, "─".repeat(80))
    }
}

/// This struct is a combination of several things
/// 1. A set of StyleCodes suitable for the environment we're running in (web or terminal)
/// 2. A set of colors we decided to use
/// 3. A mapping from UI elements to the styles we use for them
/// Note: This should really be called Theme! Usually a "palette" is just (2).
#[derive(Debug, Clone, Copy)]
pub struct Palette {
    pub primary: &'static str,
    pub code_block: &'static str,
    pub keyword: &'static str,
    pub ellipsis: &'static str,
    pub variable: &'static str,
    pub type_variable: &'static str,
    pub structure: &'static str,
    pub alias: &'static str,
    pub opaque: &'static str,
    pub error: &'static str,
    pub line_number: &'static str,
    pub header: &'static str,
    pub gutter_bar: &'static str,
    pub module_name: &'static str,
    pub binop: &'static str,
    pub typo: &'static str,
    pub typo_suggestion: &'static str,
    pub parser_suggestion: &'static str,
    pub bold: &'static str,
    pub underline: &'static str,
    pub reset: &'static str,
    pub warning: &'static str,
}

/// Set the default styles for various semantic elements,
/// given a set of StyleCodes for an environment (web or terminal).
const fn default_palette_from_style_codes(codes: StyleCodes) -> Palette {
    Palette {
        primary: codes.white,
        code_block: codes.white,
        keyword: codes.green,
        ellipsis: codes.green,
        variable: codes.cyan,
        type_variable: codes.yellow,
        structure: codes.green,
        alias: codes.yellow,
        opaque: codes.yellow,
        error: codes.red,
        line_number: codes.cyan,
        header: codes.cyan,
        gutter_bar: codes.cyan,
        module_name: codes.green,
        binop: codes.green,
        typo: codes.yellow,
        typo_suggestion: codes.yellow,
        parser_suggestion: codes.yellow,
        bold: codes.bold,
        underline: codes.underline,
        reset: codes.reset,
        warning: codes.yellow,
    }
}

/// Set colorless styles for printing with no color,
/// given a set of StyleCodes for an environment (web or terminal).
const fn no_color_palette_from_style_codes(codes: StyleCodes) -> Palette {
    Palette {
        primary: codes.no_color,
        code_block: codes.no_color,
        keyword: codes.no_color,
        ellipsis: codes.no_color,
        variable: codes.no_color,
        type_variable: codes.no_color,
        structure: codes.no_color,
        alias: codes.no_color,
        opaque: codes.no_color,
        error: codes.no_color,
        line_number: codes.no_color,
        header: codes.no_color,
        gutter_bar: codes.no_color,
        module_name: codes.no_color,
        binop: codes.no_color,
        typo: codes.no_color,
        typo_suggestion: codes.no_color,
        parser_suggestion: codes.no_color,
        bold: codes.no_color,
        underline: codes.no_color,
        reset: codes.no_color,
        warning: codes.no_color,
    }
}

pub const DEFAULT_PALETTE: Palette = default_palette_from_style_codes(ANSI_STYLE_CODES);

pub const DEFAULT_PALETTE_HTML: Palette = default_palette_from_style_codes(HTML_STYLE_CODES);

pub const NO_COLOR_PALETTE: Palette = no_color_palette_from_style_codes(ANSI_STYLE_CODES);

pub const NO_COLOR_PALETTE_HTML: Palette = no_color_palette_from_style_codes(HTML_STYLE_CODES);

/// A machine-readable format for text styles (colors and other styles)
#[derive(Debug, PartialEq)]
pub struct StyleCodes {
    pub red: &'static str,
    pub green: &'static str,
    pub yellow: &'static str,
    pub cyan: &'static str,
    pub white: &'static str,
    pub bold: &'static str,
    pub underline: &'static str,
    pub reset: &'static str,
    pub no_color: &'static str,
}

pub const ANSI_STYLE_CODES: StyleCodes = StyleCodes {
    red: "\u{001b}[1;31m",
    green: "\u{001b}[1;32m",
    yellow: "\u{001b}[1;33m",
    cyan: "\u{001b}[1;36m",
    white: "\u{001b}[37m",
    bold: "\u{001b}[1m",
    underline: "\u{001b}[4m",
    reset: "\u{001b}[0m",
    no_color: "",
};

macro_rules! html_color {
    ($name: expr) => {
        concat!("<span class='color-", $name, "'>")
    };
}

pub const HTML_STYLE_CODES: StyleCodes = StyleCodes {
    red: html_color!("red"),
    green: html_color!("green"),
    yellow: html_color!("yellow"),
    cyan: html_color!("cyan"),
    white: html_color!("white"),
    bold: "<span class='bold'>",
    underline: "<span class='underline'>",
    reset: "</span>",
    no_color: "",
};

// useful for tests
pub fn strip_colors(str: &str) -> String {
    str.replace(ANSI_STYLE_CODES.red, "")
        .replace(ANSI_STYLE_CODES.green, "")
        .replace(ANSI_STYLE_CODES.yellow, "")
        .replace(ANSI_STYLE_CODES.cyan, "")
        .replace(ANSI_STYLE_CODES.white, "")
        .replace(ANSI_STYLE_CODES.bold, "")
        .replace(ANSI_STYLE_CODES.underline, "")
        .replace(ANSI_STYLE_CODES.reset, "")
}

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

    pub fn ellipsis(&'a self) -> DocBuilder<'a, Self, Annotation> {
        self.text("…").annotate(Annotation::Ellipsis)
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
        self.tag(tn.0)
    }

    pub fn symbol_unqualified(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        self.text(symbol.as_str(self.interns))
            .annotate(Annotation::Symbol)
    }
    pub fn symbol_foreign_qualified(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        if symbol.module_id() == self.home || symbol.module_id().is_builtin() {
            // Render it unqualified if it's in the current module or a builtin
            self.text(symbol.as_str(self.interns))
                .annotate(Annotation::Symbol)
        } else {
            text!(
                self,
                "{}.{}",
                symbol.module_string(self.interns),
                symbol.as_str(self.interns),
            )
            .annotate(Annotation::Symbol)
        }
    }
    pub fn symbol_qualified(&'a self, symbol: Symbol) -> DocBuilder<'a, Self, Annotation> {
        text!(
            self,
            "{}.{}",
            symbol.module_string(self.interns),
            symbol.as_str(self.interns),
        )
        .annotate(Annotation::Symbol)
    }

    /// TODO: remove in favor of tag_name
    pub fn tag(&'a self, uppercase: Uppercase) -> DocBuilder<'a, Self, Annotation> {
        text!(self, "{}", uppercase).annotate(Annotation::Tag)
    }

    pub fn opaque_name(&'a self, opaque: Symbol) -> DocBuilder<'a, Self, Annotation> {
        let fmt = if opaque.module_id() == self.home {
            // Render it unqualified if it's in the current module.
            opaque.as_str(self.interns).to_string()
        } else {
            format!(
                "{}.{}",
                opaque.module_string(self.interns),
                opaque.as_str(self.interns),
            )
        };

        self.text(fmt).annotate(Annotation::Opaque)
    }

    pub fn wrapped_opaque_name(&'a self, opaque: Symbol) -> DocBuilder<'a, Self, Annotation> {
        debug_assert_eq!(opaque.module_id(), self.home, "Opaque wrappings can only be defined in the same module they're defined in, but this one is defined elsewhere: {opaque:?}");

        text!(self, "@{}", opaque.as_str(self.interns)).annotate(Annotation::Opaque)
    }

    pub fn record_field(&'a self, lowercase: Lowercase) -> DocBuilder<'a, Self, Annotation> {
        text!(self, ".{}", lowercase).annotate(Annotation::RecordField)
    }

    pub fn tuple_field(&'a self, index: usize) -> DocBuilder<'a, Self, Annotation> {
        text!(self, ".{}", index).annotate(Annotation::TupleElem)
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

    pub fn pq_module_name(&'a self, name: PQModuleName<'a>) -> DocBuilder<'a, Self, Annotation> {
        let name = match name {
            PackageQualified::Unqualified(n) => n.to_string(),
            PackageQualified::Qualified(prefix, n) => format!("{prefix}.{n}"),
        };

        self.text(name).annotate(Annotation::Module)
    }

    pub fn module_name(&'a self, name: ModuleName) -> DocBuilder<'a, Self, Annotation> {
        if name.is_empty() {
            // Render the app module as "app"
            self.text("app")
        } else {
            self.text(name.as_str().to_string())
        }
        .annotate(Annotation::Module)
    }

    pub fn shorthand(&'a self, name: &'a str) -> DocBuilder<'a, Self, Annotation> {
        self.text(name).annotate(Annotation::Shorthand)
    }

    pub fn backwards_arrow(&'a self) -> DocBuilder<'a, Self, Annotation> {
        self.text("<-").annotate(Annotation::BinOp)
    }

    pub fn binop(
        &'a self,
        content: roc_module::called_via::BinOp,
    ) -> DocBuilder<'a, Self, Annotation> {
        self.text(content.to_string()).annotate(Annotation::BinOp)
    }

    pub fn unop(
        &'a self,
        content: roc_module::called_via::UnaryOp,
    ) -> DocBuilder<'a, Self, Annotation> {
        self.text(content.to_string()).annotate(Annotation::UnaryOp)
    }

    pub fn suffix(
        &'a self,
        content: roc_module::called_via::Suffix,
    ) -> DocBuilder<'a, Self, Annotation> {
        self.text(content.to_string()).annotate(Annotation::UnaryOp)
    }

    /// Turns off backticks/colors in a block
    pub fn type_block(
        &'a self,
        content: DocBuilder<'a, Self, Annotation>,
    ) -> DocBuilder<'a, Self, Annotation> {
        content.annotate(Annotation::TypeBlock).indent(4)
    }

    /// Turns off backticks/colors in a block
    pub fn inline_type_block(
        &'a self,
        content: DocBuilder<'a, Self, Annotation>,
    ) -> DocBuilder<'a, Self, Annotation> {
        content.annotate(Annotation::InlineTypeBlock)
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
        region: LineColumnRegion,
        sub_region1: LineColumnRegion,
        sub_region2: LineColumnRegion,
        error_annotation: Annotation,
    ) -> DocBuilder<'a, Self, Annotation> {
        debug_assert!(region.contains(&sub_region1));
        debug_assert!(region.contains(&sub_region2));

        // if true, the final line of the snippet will be some ^^^ that point to the region where
        // the problem is. Otherwise, the snippet will have a > on the lines that are in the region
        // where the problem is.
        let error_highlight_line = region.start().line == region.end().line;

        let max_line_number_length = (region.end().line + 1).to_string().len();
        let indent = 2;

        let mut result = self.nil();
        for i in region.start().line..=region.end().line {
            let line_number_string = (i + 1).to_string();
            let line_number = line_number_string;
            let this_line_number_length = line_number.len();

            let line = self.src_lines[i as usize];
            let is_line_empty = line.trim().is_empty();
            let rest_of_line = if !is_line_empty {
                self.text(line).indent(indent)
            } else {
                self.nil()
            };

            let highlight = !error_highlight_line
                && ((i >= sub_region1.start().line && i <= sub_region1.end().line)
                    || (i >= sub_region2.start().line && i <= sub_region2.end().line));

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
                let up_to_gutter = self
                    .text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar));

                if is_line_empty {
                    // Don't put an trailing space after the gutter
                    up_to_gutter
                } else {
                    up_to_gutter.append(self.text(" ")).append(rest_of_line)
                }
            };

            result = result.append(source_line);

            if i != region.end().line {
                result = result.append(self.line())
            }
        }

        if error_highlight_line {
            let overlapping = sub_region2.start().column < sub_region1.end().column;

            let highlight = if overlapping {
                self.text(
                    ERROR_UNDERLINE
                        .repeat((sub_region2.end().column - sub_region1.start().column) as usize),
                )
            } else {
                let highlight1 = ERROR_UNDERLINE
                    .repeat((sub_region1.end().column - sub_region1.start().column) as usize);
                let highlight2 = if sub_region1 == sub_region2 {
                    "".repeat(0)
                } else {
                    ERROR_UNDERLINE
                        .repeat((sub_region2.end().column - sub_region2.start().column) as usize)
                };
                let in_between = " ".repeat(
                    (sub_region2
                        .start()
                        .column
                        .saturating_sub(sub_region1.end().column)) as usize,
                );

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
                    self.text(" ".repeat(sub_region1.start().column as usize))
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
        region: LineColumnRegion,
        sub_region: LineColumnRegion,
        severity: Severity,
    ) -> DocBuilder<'a, Self, Annotation> {
        // debug_assert!(region.contains(&sub_region));

        // If the outer region takes more than 1 full screen (~60 lines), only show the inner region
        if region.end().line.saturating_sub(region.start().line) > 60 {
            // If the inner region contains the outer region (or if they are the same),
            // attempting this will recurse forever, so don't do that! Instead, give up and
            // accept that this report will take up more than 1 full screen.
            if !sub_region.contains(&region) {
                return self.region_with_subregion(sub_region, sub_region, severity);
            }
        }

        let annotation = match severity {
            Severity::RuntimeError | Severity::Fatal => Annotation::Error,
            Severity::Warning => Annotation::Warning,
        };

        // if true, the final line of the snippet will be some ^^^ that point to the region where
        // the problem is. Otherwise, the snippet will have a > on the lines that are in the region
        // where the problem is.
        let error_highlight_line = sub_region.start().line == region.end().line;

        let max_line_number_length = (region.end().line + 1).to_string().len();
        let indent = 2;

        let mut result = self.nil();
        for i in region.start().line..=region.end().line {
            let line_number_string = (i + 1).to_string();
            let line_number = line_number_string;
            let this_line_number_length = line_number.len();

            // filter out any escape characters for the current line that could mess up the output.
            let line: String = self
                .src_lines
                .get(i as usize)
                .unwrap_or(&"")
                .chars()
                .filter(|&c| !c.is_ascii_control() || c == '\t')
                .collect::<String>();

            let is_line_empty = line.trim().is_empty();
            let rest_of_line = if !is_line_empty {
                self.text(line)
                    .annotate(Annotation::CodeBlock)
                    .indent(indent)
            } else {
                self.nil()
            };

            let source_line = if !error_highlight_line
                && i >= sub_region.start().line
                && i <= sub_region.end().line
            {
                self.text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                    .append(self.text(">").annotate(annotation))
                    .append(rest_of_line)
            } else if error_highlight_line {
                self.text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar))
                    .append(rest_of_line)
            } else {
                let up_to_gutter = self
                    .text(" ".repeat(max_line_number_length - this_line_number_length))
                    .append(self.text(line_number).annotate(Annotation::LineNumber))
                    .append(self.text(GUTTER_BAR).annotate(Annotation::GutterBar));

                if is_line_empty {
                    // Don't put an trailing space after the gutter
                    up_to_gutter
                } else {
                    up_to_gutter.append(self.text(" ")).append(rest_of_line)
                }
            };

            result = result.append(source_line);

            if i != region.end().line {
                result = result.append(self.line())
            }
        }

        if error_highlight_line {
            let highlight_text = ERROR_UNDERLINE
                .repeat((sub_region.end().column - sub_region.start().column) as usize);

            let highlight_line = self
                .line()
                // Omit the gutter bar when we know there are no further
                // line numbers to be printed after this!
                .append(self.text(" ".repeat(max_line_number_length + GUTTER_BAR_WIDTH)))
                .append(if highlight_text.is_empty() {
                    self.nil()
                } else {
                    self.text(" ".repeat(sub_region.start().column as usize))
                        .indent(indent)
                        .append(self.text(highlight_text).annotate(annotation))
                });

            result = result.append(highlight_line);
        }

        result
    }

    pub fn region(
        &'a self,
        region: LineColumnRegion,
        severity: Severity,
    ) -> DocBuilder<'a, Self, Annotation> {
        self.region_with_subregion(region, region, severity)
    }

    pub fn region_without_error(
        &'a self,
        region: LineColumnRegion,
    ) -> DocBuilder<'a, Self, Annotation> {
        let mut result = self.nil();
        for i in region.start().line..=region.end().line {
            let line = if i == region.start().line {
                if i == region.end().line {
                    &self.src_lines[i as usize]
                        [region.start().column as usize..region.end().column as usize]
                } else {
                    &self.src_lines[i as usize][region.start().column as usize..]
                }
            } else if i == region.end().line {
                &self.src_lines[i as usize][0..region.end().column as usize]
            } else {
                self.src_lines[i as usize]
            };

            let rest_of_line = if !line.trim().is_empty() {
                self.text(line).annotate(Annotation::CodeBlock)
            } else {
                self.nil()
            };

            result = result.append(rest_of_line);

            if i != region.end().line {
                result = result.append(self.line())
            }
        }

        result.indent(4)
    }

    pub fn ident(&'a self, ident: Ident) -> DocBuilder<'a, Self, Annotation> {
        text!(self, "{}", ident.as_inline_str()).annotate(Annotation::Symbol)
    }

    pub fn int_literal<I>(&'a self, int: I) -> DocBuilder<'a, Self, Annotation>
    where
        I: ToString,
    {
        let s = int.to_string();

        let is_negative = s.starts_with('-');

        if s.len() < 7 + (is_negative as usize) {
            // If the number is not at least in the millions, return it as-is.
            return self.text(s);
        }

        // Otherwise, let's add numeric separators to make it easier to read.
        let mut result = String::with_capacity(s.len() + s.len() / 3);
        for (idx, c) in s
            .get((is_negative as usize)..)
            .unwrap()
            .chars()
            .rev()
            .enumerate()
        {
            if idx != 0 && idx % 3 == 0 {
                result.push('_');
            }
            result.push(c);
        }
        if is_negative {
            result.push('-');
        }
        self.text(result.chars().rev().collect::<String>())
    }

    pub fn file_path(&'a self, path: &Path) -> DocBuilder<'a, Self, Annotation> {
        let cwd = std::env::current_dir().unwrap();
        let relative_path = path.strip_prefix(cwd).unwrap_or(path).to_str().unwrap();

        self.text(relative_path.to_string())
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Annotation {
    Emphasized,
    Url,
    Keyword,
    Ellipsis,
    Tag,
    RecordField,
    RecordUpdater,
    TupleElem,
    TypeVariable,
    Alias,
    Opaque,
    Structure,
    Symbol,
    BinOp,
    UnaryOp,
    Error,
    GutterBar,
    LineNumber,
    PlainText,
    CodeBlock,
    TypeBlock,
    InlineTypeBlock,
    Module,
    Shorthand,
    Typo,
    TypoSuggestion,
    Tip,
    Header,
    ParserSuggestion,
    Warning,
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
    palette: &'a Palette,
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
            InlineTypeBlock => {
                debug_assert!(!self.in_type_block);
                self.write_str("`")?;
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
            Tag | Keyword | RecordField | Symbol | Typo | TypoSuggestion | TypeVariable
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
                InlineTypeBlock => {
                    debug_assert!(self.in_type_block);
                    self.write_str("`")?;
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
                Tag | Keyword | RecordField | Symbol | Typo | TypoSuggestion | TypeVariable
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
                self.write_str(self.palette.bold)?;
            }
            Url | Tip => {
                self.write_str(self.palette.underline)?;
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
            Opaque => {
                self.write_str(self.palette.alias)?;
            }
            BinOp => {
                self.write_str(self.palette.alias)?;
            }
            UnaryOp => {
                self.write_str(self.palette.alias)?;
            }
            Symbol => {
                self.write_str(self.palette.variable)?;
            }
            Keyword => {
                self.write_str(self.palette.keyword)?;
            }
            Ellipsis => {
                self.write_str(self.palette.ellipsis)?;
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
            Shorthand => {
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
            Warning => {
                self.write_str(self.palette.warning)?;
            }
            TypeBlock | InlineTypeBlock | Tag | RecordField | RecordUpdater | TupleElem => { /* nothing yet */
            }
        }
        self.style_stack.push(*annotation);
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        use Annotation::*;

        match self.style_stack.pop() {
            None => {}
            Some(annotation) => match annotation {
                Emphasized | Url | TypeVariable | Alias | Symbol | BinOp | UnaryOp | Error
                | GutterBar | Ellipsis | Typo | TypoSuggestion | ParserSuggestion | Structure
                | CodeBlock | PlainText | LineNumber | Tip | Module | Shorthand | Header
                | Keyword | Warning => {
                    self.write_str(self.palette.reset)?;
                }

                TypeBlock | InlineTypeBlock | Tag | Opaque | RecordField | RecordUpdater
                | TupleElem => { /* nothing yet */ }
            },
        }
        Ok(())
    }
}

#[cfg(not(target_family = "wasm"))]
pub fn to_https_problem_report_string(
    url: &str,
    https_problem: Problem,
    filename: PathBuf,
) -> String {
    let src_lines: Vec<&str> = Vec::new();

    let mut module_ids = ModuleIds::default();

    let module_id = module_ids.get_or_insert(&"find module name somehow?".into());

    let interns = Interns::default();

    // Report parsing and canonicalization problems
    let alloc = RocDocAllocator::new(&src_lines, module_id, &interns);

    let mut buf = String::new();
    let palette = DEFAULT_PALETTE;
    let report = to_https_problem_report(&alloc, url, https_problem, filename);
    report.render_color_terminal(&mut buf, &alloc, &palette);

    buf
}

#[cfg(not(target_family = "wasm"))]
pub fn to_https_problem_report<'b>(
    alloc: &'b RocDocAllocator<'b>,
    url: &'b str,
    https_problem: Problem,
    filename: PathBuf,
) -> Report<'b> {
    match https_problem {
        Problem::UnsupportedEncoding(not_supported_encoding) => {
            let doc = alloc.stack([
                alloc.reflow(r"I tried to download from this URL:"),
                alloc.string((&url).to_string()).annotate(Annotation::Url).indent(4),
                alloc.concat([
                    alloc.reflow(r"But the server replied with a "),
                    alloc.reflow(r"content encoding").annotate(Annotation::Emphasized),
                    alloc.reflow(r" that I do not understand ("),
                    alloc.string(not_supported_encoding).annotate(Annotation::Emphasized),
                    alloc.reflow(r")."),
                ]),
                alloc.concat([
                    alloc.reflow(r"The supported content encodings are "),
                    alloc.keyword(r"br"),
                    alloc.reflow(r", "),
                    alloc.keyword(r"gzip"),
                    alloc.reflow(r" and "),
                    alloc.keyword(r"deflate"),
                ]),
                alloc.concat([
                    alloc.tip(),
                    alloc.reflow(r"Perhaps you can check if the URL is correctly formed, or if the server is correctly configured."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "UNSUPPORTED ENCODING".to_string(),
                severity: Severity::Fatal,
            }
        }
        Problem::MultipleEncodings(multiple_encodings) => {
            let doc = alloc.stack([
                alloc.reflow(r"I tried to download from this URL:"),
                alloc.string((&url).to_string()).annotate(Annotation::Url).indent(4),
                alloc.concat([
                    alloc.reflow(r"But the server replied with multiple "),
                    alloc.reflow(r"content encodings").annotate(Annotation::Emphasized),
                    alloc.reflow(r": "),
                    alloc.string(multiple_encodings).annotate(Annotation::Emphasized),
                    alloc.reflow(r"."),
                ]),
                alloc.concat([
                    alloc.reflow(r"The supported content encodings are "),
                    alloc.keyword(r"br"),
                    alloc.reflow(r", "),
                    alloc.keyword(r"gzip"),
                    alloc.reflow(r" and "),
                    alloc.keyword(r"deflate"),
                    alloc.reflow(r". However, the server reply can only contain "),
                    alloc.reflow(r"one").annotate(Annotation::Emphasized),
                    alloc.reflow(r"."),
                ]),
                alloc.concat([
                    alloc.tip(),
                    alloc.reflow(r"Perhaps you can check if the URL is correctly formed, or if the server is correctly configured."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "MULTIPLE ENCODINGS".to_string(),
                severity: Severity::Fatal,
            }
        }
        Problem::InvalidContentHash { expected, actual } => {
            let doc = alloc.stack([
                alloc.reflow(r"I was able to download this URL:"),
                alloc.string((&url).to_string()).annotate(Annotation::Url).indent(4),
                alloc.concat([
                    alloc.reflow(r"I use a mechanism to detect if the file might "),
                    alloc.reflow(r"have been tampered with. This could happen if "),
                    alloc.reflow(r"the server or domain have been compromised."),
                ]),
                alloc.concat([
                    alloc.reflow(r"This is the content signature I was "),
                    alloc.reflow(r"expecting").annotate(Annotation::Emphasized),
                    alloc.reflow(r":"),
                ]),
                alloc.string(expected).annotate(Annotation::PlainText).indent(4),
                alloc.concat([
                    alloc.reflow(r"However, this is the content signature I "),
                    alloc.reflow(r"obtained").annotate(Annotation::Emphasized),
                    alloc.reflow(r":"),
                ]),
                alloc.string(actual).annotate(Annotation::PlainText).indent(4),
                alloc.reflow(r"To keep you secure, I will not execute this untrusted code."),
                alloc.concat([
                    alloc.tip(),
                    alloc.reflow(r"Check if the URL is correctly formed and if this is the server you are expecting to connect to."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "INVALID CONTENT HASH".to_string(),
                severity: Severity::Fatal,
            }
        }
        Problem::NotFound => {
            let doc = alloc.stack([
                alloc.reflow(r"I tried to download from this URL:"),
                alloc
                    .string((&url).to_string())
                    .annotate(Annotation::Url)
                    .indent(4),
                alloc.concat([alloc.reflow(r"But the file was not found (404).")]),
                alloc.concat([alloc.tip(), alloc.reflow(r"Is the URL correct?")]),
            ]);
            Report {
                filename,
                doc,
                title: "NOTFOUND".to_string(),
                severity: Severity::Fatal,
            }
        }
        // TODO: The reporting text for IoErr and FsExtraErr could probably be unified
        Problem::IoErr(io_error) => {
            let doc = alloc.stack([
                alloc.reflow(r"I tried to download from this URL:"),
                alloc
                    .string((&url).to_string())
                    .annotate(Annotation::Url)
                    .indent(4),
                alloc.reflow(r"But I encountered an IO (input/output) error:"),
                alloc
                    .string(io_error.to_string())
                    .annotate(Annotation::PlainText)
                    .indent(4),
                // TODO: What should the tip for IO errors be?
                // alloc.concat([
                //     alloc.tip(),
                //     alloc.reflow(r"Check the error message."),
                // ]),
            ]);

            Report {
                filename,
                doc,
                title: "IO ERROR".to_string(),
                severity: Severity::Fatal,
            }
        }
        // TODO: The reporting text for IoErr and FsExtraErr could probably be unified
        Problem::FsExtraErr(fs_extra_error) => {
            let doc = alloc.stack([
                alloc.reflow(r"I tried to download from this URL:"),
                alloc
                    .string((&url).to_string())
                    .annotate(Annotation::Url)
                    .indent(4),
                alloc.reflow(r"But I encountered an IO (input/output) error:"),
                alloc
                    .string(fs_extra_error.to_string())
                    .annotate(Annotation::PlainText)
                    .indent(4),
                // TODO: What should the tip for IO errors be?
                // alloc.concat([
                //     alloc.tip(),
                //     alloc.reflow(r"Check the error message."),
                // ]),
            ]);

            Report {
                filename,
                doc,
                title: "IO ERROR".to_string(),
                severity: Severity::Fatal,
            }
        }
        Problem::HttpErr(reqwest_error) => {
            let doc = alloc.stack([
                alloc.reflow(r"I tried to download from this URL:"),
                alloc
                    .string((&url).to_string())
                    .annotate(Annotation::Url)
                    .indent(4),
                alloc.reflow(r"But I encountered a network error:"),
                alloc
                    .string(reqwest_error.to_string())
                    .annotate(Annotation::PlainText)
                    .indent(4),
                // TODO: What should the tip for HTTP IO errors be?
                // Should we import reqwest and check stuff like
                // reqwest_error.{ is_redirect(), is_status(), is_timeout(), ... } ?
                //
                // alloc.concat([
                //     alloc.tip(),
                //     alloc.reflow(r"Check the error message."),
                // ]),
            ]);

            Report {
                filename,
                doc,
                title: "HTTP ERROR".to_string(),
                severity: Severity::Fatal,
            }
        }
        Problem::InvalidUrl(roc_packaging::https::UrlProblem::InvalidExtensionSuffix(
            invalid_suffix,
        )) => {
            let (suffix_text, annotation_style) = if invalid_suffix.is_empty() {
                (r"empty".to_string(), Annotation::PlainText)
            } else {
                (invalid_suffix, Annotation::Emphasized)
            };

            let doc = alloc.stack([
                alloc.reflow(r"I tried to download from this URL:"),
                alloc
                    .string((&url).to_string())
                    .annotate(Annotation::Url)
                    .indent(4),
                alloc.concat([
                    alloc.reflow(r"However, this file's extension ("),
                    alloc.string(suffix_text).annotate(annotation_style),
                    alloc.reflow(r") is not a supported extension."),
                ]),
                alloc.concat([
                    alloc.reflow(r"The supported extensions are "),
                    alloc.keyword(r".tar"),
                    alloc.reflow(r", "),
                    alloc.keyword(r".tar.gz"),
                    alloc.reflow(r" and "),
                    alloc.keyword(r".tar.br"),
                ]),
                alloc.concat([
                    alloc.tip(),
                    alloc.reflow(r"Check that you have the correct URL for this package/platform."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "INVALID EXTENSION SUFFIX".to_string(),
                severity: Severity::Fatal,
            }
        }
        Problem::InvalidUrl(roc_packaging::https::UrlProblem::MissingTarExt) => {
            let doc = alloc.stack([
                alloc.reflow(r"I tried to download from this URL:"),
                alloc
                    .string((&url).to_string())
                    .annotate(Annotation::Url)
                    .indent(4),
                alloc.concat([
                    alloc.reflow(r"However, this file's extension is not "),
                    alloc.keyword(r".tar"),
                    alloc.reflow(r"."),
                ]),
                alloc.concat([
                    alloc.reflow(r"The supported extensions are "),
                    alloc.keyword(r".tar"),
                    alloc.reflow(r", "),
                    alloc.keyword(r".tar.gz"),
                    alloc.reflow(r" and "),
                    alloc.keyword(r".tar.br"),
                ]),
                alloc.concat([
                    alloc.tip(),
                    alloc.reflow(r"Check that you have the correct URL for this package/platform."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "INVALID EXTENSION".to_string(),
                severity: Severity::Fatal,
            }
        }
        Problem::InvalidUrl(roc_packaging::https::UrlProblem::InvalidFragment(
            invalid_fragment,
        )) => {
            let doc = alloc.stack([
                alloc.reflow(r"I tried to download from this URL:"),
                alloc
                    .string((&url).to_string())
                    .annotate(Annotation::Url)
                    .indent(4),
                alloc.concat([
                    alloc.reflow(r"However, this URL's fragment (the part after #) "),
                    alloc.reflow(r"is not valid. When present, the fragment must point to "),
                    alloc.reflow(r"an existing "),
                    alloc.keyword(r".roc"),
                    alloc.reflow(r" file inside the package. Also, the filename can't be empty, "),
                    alloc.reflow(r"so a fragment of #.roc would also not be valid. This is the "),
                    alloc.reflow(r"invalid fragment I encountered: "),
                ]),
                alloc
                    .string(invalid_fragment)
                    .annotate(Annotation::Emphasized)
                    .indent(4),
                alloc.concat([
                    alloc.tip(),
                    alloc.reflow(r"Check that the fragment points to an existing "),
                    alloc.keyword(r".roc"),
                    alloc.reflow(r" file inside the package. You can download this package "),
                    alloc.reflow(r"and inspect it locally."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "INVALID FRAGMENT".to_string(),
                severity: Severity::Fatal,
            }
        }
        Problem::InvalidUrl(roc_packaging::https::UrlProblem::MissingHash) => {
            let doc = alloc.stack([
                alloc.reflow(r"I tried to download from this URL:"),
                alloc
                    .string((&url).to_string())
                    .annotate(Annotation::Url)
                    .indent(4),
                alloc.concat([
                    alloc.reflow(r"I use a content hash to detect if the file might "),
                    alloc.reflow(r"have been tampered with. This could happen if "),
                    alloc.reflow(r"the server or domain have been compromised."),
                ]),
                alloc.concat([
                    alloc.reflow(r"The way this works is that the name of the file "),
                    alloc.reflow(r"is the BLAKE3 hash of the contents of the "),
                    alloc.reflow(r"file itself. If someone would tamper with the file, "),
                    alloc.reflow(r"I could notify and protect you. However, I could "),
                    alloc.reflow(r"not find the expected hash on the URL above, "),
                    alloc.reflow(r"so I cannot apply this tamper-check."),
                ]),
                alloc.concat([
                    alloc.tip(),
                    alloc
                        .reflow(r"Check that you have the correct URL for this package/platform. "),
                    alloc.reflow(r"Here is an example of how such a hash looks like: "),
                    alloc
                        .string(r"tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk".to_string())
                        .annotate(Annotation::Emphasized),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "MISSING PACKAGE HASH".to_string(),
                severity: Severity::Fatal,
            }
        }
        Problem::InvalidUrl(roc_packaging::https::UrlProblem::MissingHttps) => {
            let doc = alloc.stack([
                alloc.reflow(r"I tried to download from this URL:"),
                alloc
                    .string((&url).to_string())
                    .annotate(Annotation::Url)
                    .indent(4),
                alloc.concat([
                    alloc.reflow(r"For your security, I will only attempt to download "),
                    alloc.reflow(r"files from servers which use the "),
                    alloc.keyword(r"https"),
                    alloc.reflow(r" protocol."),
                ]),
                alloc.concat([
                    alloc.tip(),
                    alloc.reflow(r"Check that you have the correct URL for this package/platform."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "HTTPS MANDATORY".to_string(),
                severity: Severity::Fatal,
            }
        }
        Problem::InvalidUrl(roc_packaging::https::UrlProblem::MisleadingCharacter) => {
            let doc = alloc.stack([
                alloc.reflow(r"I tried to download from this URL:"),
                alloc
                    .string((&url).to_string())
                    .annotate(Annotation::Url)
                    .indent(4),
                alloc.concat([
                    alloc.reflow(r"I have found one or more potentially misleading "),
                    alloc.reflow(r"characters in this URL. Misleading characters are "),
                    alloc.reflow(r"characters that look like others but aren't the same. "),
                    alloc.reflow(r"The following characters are classified as misleading: "),
                    alloc.keyword(r"@"),
                    alloc.reflow(r", "),
                    alloc.keyword("\u{2044}"),
                    alloc.reflow(r" (unicode 2044), "),
                    alloc.keyword("\u{2215}"),
                    alloc.reflow(r" (unicode 2215), "),
                    alloc.keyword("\u{FF0F}"),
                    alloc.reflow(r" (unicode FF0F) and "),
                    alloc.keyword("\u{29F8}"),
                    alloc.reflow(r" (unicode 29F8). "),
                ]),
                alloc.concat([
                    alloc.reflow(r"If you have a use-case for any of these characters we "),
                    alloc.reflow(r"would like to hear about it. Reach out on "),
                    alloc
                        .string(r"https://github.com/roc-lang/roc/issues/5487".to_string())
                        .annotate(Annotation::Url),
                ]),
                alloc.concat([
                    alloc.tip(),
                    alloc.reflow(r"Check that you have the correct URL for this package/platform."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "MISLEADING CHARACTERS".to_string(),
                severity: Severity::Fatal,
            }
        }
        Problem::DownloadTooBig(content_len) => {
            let nice_bytes = Byte::from_bytes(content_len.into())
                .get_appropriate_unit(false)
                .format(3);
            let doc = alloc.stack([
                alloc.reflow(r"I tried to download from this URL:"),
                alloc
                    .string((&url).to_string())
                    .annotate(Annotation::Url)
                    .indent(4),
                    alloc.concat([
                        alloc.reflow(r"But the server stated this file is "),
                        alloc.string(nice_bytes).annotate(Annotation::Keyword),
                        alloc.reflow(r" in size. This is larger that the maximum size I can handle (around 32 GB)."),
                    ]),
                alloc.concat([
                    alloc.tip(),
                    alloc.reflow(r"Check that you have the correct URL for this package/platform. "),
                    alloc.reflow(r"If you do, you should contact the package/platform's author and "),
                    alloc.reflow(r"notify them about this issue."),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "FILE TOO LARGE".to_string(),
                severity: Severity::Fatal,
            }
        }
    }
}

pub fn to_file_problem_report_string(
    filename: PathBuf,
    error: io::ErrorKind,
    has_color: bool,
) -> String {
    let src_lines: Vec<&str> = Vec::new();
    let mut module_ids = ModuleIds::default();
    let module_id = module_ids.get_or_insert(&"find module name somehow?".into());
    let interns = Interns::default();

    // Report parsing and canonicalization problems
    let alloc = RocDocAllocator::new(&src_lines, module_id, &interns);

    let mut buf = String::new();
    let palette = if has_color {
        DEFAULT_PALETTE
    } else {
        NO_COLOR_PALETTE
    };
    let report = to_file_problem_report(&alloc, filename, error);
    report.render_color_terminal(&mut buf, &alloc, &palette);

    buf
}

pub fn to_file_problem_report<'b>(
    alloc: &'b RocDocAllocator<'b>,
    filename: PathBuf,
    error: io::ErrorKind,
) -> Report<'b> {
    let filename_str: String = filename.to_str().unwrap().to_string();
    match error {
        io::ErrorKind::NotFound => {
            let doc = alloc.stack([
                alloc.reflow(r"I am looking for this file, but it's not there:"),
                alloc
                    .string(filename_str)
                    .annotate(Annotation::ParserSuggestion)
                    .indent(4),
                alloc.concat([
                    alloc.reflow(r"Is the file supposed to be there? "),
                    alloc.reflow("Maybe there is a typo in the file name?"),
                ]),
            ]);

            Report {
                filename,
                doc,
                title: "FILE NOT FOUND".to_string(),
                severity: Severity::Fatal,
            }
        }
        io::ErrorKind::PermissionDenied => {
            let doc = alloc.stack([
                alloc.reflow(r"I don't have the required permissions to read this file:"),
                alloc
                    .string(filename_str)
                    .annotate(Annotation::ParserSuggestion)
                    .indent(4),
                alloc
                    .concat([alloc.reflow(r"Is it the right file? Maybe change its permissions?")]),
            ]);

            Report {
                filename,
                doc,
                title: "FILE PERMISSION DENIED".to_string(),
                severity: Severity::Fatal,
            }
        }
        io::ErrorKind::Unsupported => {
            let doc = match filename.extension() {
                Some(ext) => alloc.concat(vec![
                    alloc.reflow(r"I expected a file with extension `.roc` or without extension."),
                    alloc.hardline(),
                    alloc.reflow(r"Instead I received a file with extension `."),
                    alloc.as_string(ext.to_string_lossy()),
                    alloc.as_string("`."),
                ]),
                None => {
                    alloc.stack(vec![
                        alloc.vcat(vec![
                            alloc.reflow(r"I expected a file with either:"),
                            alloc.reflow("- extension `.roc`"),
                            alloc.intersperse(
                                "- no extension and a roc shebang as the first line, e.g. `#!/home/username/bin/roc_nightly/roc`"
                                    .split(char::is_whitespace),
                                alloc.concat(vec![ alloc.hardline(), alloc.text("  ")]).flat_alt(alloc.space()).group()
                            ),
                        ]),
                        alloc.concat(vec![
                            alloc.reflow("The provided file did not start with a shebang `#!` containing the string `roc`. Is "),
                            alloc.as_string(filename.to_string_lossy()),
                            alloc.reflow(" a Roc file?"),
                        ])
                    ])
                }
            };

            Report {
                filename,
                doc,
                title: "NOT A ROC FILE".to_string(),
                severity: Severity::Fatal,
            }
        }
        _ => {
            let error = std::io::Error::from(error);
            let formatted = format!("{error}");
            let doc = alloc.stack([
                alloc.reflow(r"I tried to read this file:"),
                alloc
                    .string(filename_str)
                    .annotate(Annotation::Error)
                    .indent(4),
                alloc.reflow(r"But ran into:"),
                alloc.text(formatted).annotate(Annotation::Error).indent(4),
            ]);

            Report {
                filename,
                doc,
                title: "FILE PROBLEM".to_string(),
                severity: Severity::Fatal,
            }
        }
    }
}
