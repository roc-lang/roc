use std::path::PathBuf;

use bumpalo::Bump;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_parse::ast::Expr;
use roc_region::all::{LineColumnRegion, LineInfo, Region};
use roc_types::{
    subs::{Subs, Variable},
    types::{ErrorType, Polarity},
};

use crate::report::{RenderTarget, RocDocAllocator, RocDocBuilder};

pub struct Renderer<'a> {
    arena: &'a Bump,
    alloc: RocDocAllocator<'a>,
    filename: PathBuf,
    line_info: LineInfo,
    render_target: RenderTarget,
}

impl<'a> Renderer<'a> {
    pub fn new(
        arena: &'a Bump,
        interns: &'a Interns,
        render_target: RenderTarget,
        module_id: ModuleId,
        filename: PathBuf,
        source: &'a str,
    ) -> Self {
        let source_lines = bumpalo::collections::Vec::from_iter_in(source.lines(), arena);
        let line_info = roc_region::all::LineInfo::new(source);

        let alloc = RocDocAllocator::new(source_lines.into_bump_slice(), module_id, interns);

        Self {
            arena,
            alloc,
            line_info,
            filename,
            render_target,
        }
    }

    fn render_expr(&'a self, error_type: ErrorType) -> RocDocBuilder<'a> {
        use crate::error::r#type::error_type_to_doc;

        error_type_to_doc(&self.alloc, error_type)
    }

    fn render_lookup(
        &'a self,
        symbol: Symbol,
        expr: &Expr<'_>,
        error_type: ErrorType,
    ) -> RocDocBuilder<'a> {
        use roc_fmt::annotation::Formattable;

        let mut buf = roc_fmt::Buf::new_in(self.arena);
        expr.format(&mut buf, 0);

        self.alloc.vcat([
            self.alloc
                .symbol_unqualified(symbol)
                .append(" : ")
                .append(self.render_expr(error_type)),
            self.alloc
                .symbol_unqualified(symbol)
                .append(" = ")
                .append(buf.into_bump_str()),
        ])
    }

    fn render_lookups(
        &'a self,
        subs: &mut Subs,
        line_col_region: LineColumnRegion,

        symbols: &[Symbol],
        variables: &[Variable],
        expressions: &[Expr<'_>],
    ) -> RocDocBuilder<'a> {
        use ven_pretty::DocAllocator;

        let it =
            symbols
                .iter()
                .zip(variables)
                .zip(expressions)
                .map(|((symbol, variable), expr)| {
                    let error_type = subs.var_to_error_type(*variable, Polarity::OF_VALUE);
                    self.render_lookup(*symbol, expr, error_type)
                });

        if it.len() > 0 {
            self.alloc.stack([
                self.alloc.text("This expectation failed:"),
                self.alloc.region(line_col_region),
                self.alloc
                    .text("When it failed, these variables had these values:"),
                self.alloc.stack(it),
                self.alloc.text(""), // Blank line at the end
            ])
        } else {
            self.alloc.stack([
                self.alloc.text("This expectation failed:"),
                self.alloc.region(line_col_region),
                self.alloc.text(""), // Blank line at the end
            ])
        }
    }

    fn to_line_col_region(
        &self,
        expect_region: Option<Region>,
        failure_region: Region,
    ) -> LineColumnRegion {
        let display_region = match expect_region {
            Some(expect_region) => {
                if !expect_region.contains(&failure_region) {
                    // this is an expect outside of a toplevel expect,
                    // likely in some function we called
                    failure_region
                } else {
                    Region::across_all([&expect_region, &failure_region])
                }
            }
            None => failure_region,
        };

        self.line_info.convert_region(display_region)
    }

    #[allow(clippy::too_many_arguments)]
    pub fn render_failure<W>(
        &self,
        writer: &mut W,
        subs: &mut Subs,
        symbols: &[Symbol],
        variables: &[Variable],
        expressions: &[Expr<'_>],
        expect_region: Option<Region>,
        failure_region: Region,
    ) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        use crate::report::Report;

        let line_col_region = self.to_line_col_region(expect_region, failure_region);
        let doc = self.render_lookups(subs, line_col_region, symbols, variables, expressions);

        let report = Report {
            title: "EXPECT FAILED".into(),
            doc,
            filename: self.filename.clone(),
            severity: crate::report::Severity::RuntimeError,
        };

        let mut buf = String::new();

        report.render(
            self.render_target,
            &mut buf,
            &self.alloc,
            &crate::report::DEFAULT_PALETTE,
        );

        write!(writer, "{}", buf)
    }

    #[allow(clippy::too_many_arguments)]
    pub fn render_dbg<W>(
        &self,
        writer: &mut W,
        expressions: &[Expr<'_>],
        expect_region: Option<Region>,
        dbg_expr_region: Region,
    ) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        let line_col_region = self.to_line_col_region(expect_region, dbg_expr_region);
        write!(
            writer,
            "\u{001b}[36m[{} {}:{}] \u{001b}[0m",
            self.filename.display(),
            line_col_region.start.line + 1,
            line_col_region.start.column + 1
        )?;

        let expr = expressions[0];

        let mut buf = roc_fmt::Buf::new_in(self.arena);
        {
            use roc_fmt::annotation::Formattable;
            expr.format(&mut buf, 0);
        }

        writeln!(writer, "{}", buf.as_str())
    }

    pub fn render_panic<W>(
        &self,
        writer: &mut W,
        message: &str,
        expect_region: Region,
    ) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        use crate::report::Report;
        use ven_pretty::DocAllocator;

        let line_col_region = self.line_info.convert_region(expect_region);

        let doc = self.alloc.stack([
            self.alloc.text("This expectation crashed while running:"),
            self.alloc.region(line_col_region),
            self.alloc.text("The crash reported this message:"),
            self.alloc.text(message),
        ]);

        let report = Report {
            title: "EXPECT PANICKED".into(),
            doc,
            filename: self.filename.clone(),
            severity: crate::report::Severity::RuntimeError,
        };

        let mut buf = String::new();

        report.render(
            self.render_target,
            &mut buf,
            &self.alloc,
            &crate::report::DEFAULT_PALETTE,
        );

        write!(writer, "{}", buf)
    }
}
