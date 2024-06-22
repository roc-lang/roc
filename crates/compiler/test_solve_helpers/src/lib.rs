use std::{error::Error, io, path::PathBuf};

use bumpalo::Bump;
use lazy_static::lazy_static;
use regex::Regex;
use roc_can::{
    abilities::AbilitiesStore,
    expr::Declarations,
    module::ExposedByModule,
    traverse::{find_declaration, find_symbol_at, find_type_at, FoundSymbol},
};
use roc_derive::SharedDerivedModule;
use roc_late_solve::AbilitiesView;
use roc_load::{FunctionKind, LoadedModule};
use roc_module::symbol::{Interns, ModuleId};
use roc_packaging::cache::RocCacheDir;
use roc_problem::can::Problem;
use roc_region::all::{LineColumn, LineColumnRegion, LineInfo, Region};
use roc_reporting::report::{can_problem, type_problem, RocDocAllocator};
use roc_solve_problem::TypeError;
use roc_types::{
    pretty_print::{name_and_print_var, DebugPrint},
    subs::{instantiate_rigids, Subs, Variable},
};

fn promote_expr_to_module(src: &str) -> String {
    let mut buffer = String::from(indoc::indoc!(
        r#"
        app "test"
            imports []
            provides [main] to "./platform"

        main =
        "#
    ));

    for line in src.lines() {
        // indent the body!
        buffer.push_str("    ");
        buffer.push_str(line);
        buffer.push('\n');
    }

    buffer
}

pub fn run_load_and_infer<'a>(
    src: &str,
    dependencies: impl IntoIterator<Item = (&'a str, &'a str)>,
    no_promote: bool,
    function_kind: FunctionKind,
) -> Result<(LoadedModule, String), std::io::Error> {
    use tempfile::tempdir;

    let arena = &Bump::new();

    let module_src;
    let temp;
    if src.starts_with("app") || no_promote {
        // this is already a module
        module_src = src;
    } else {
        // this is an expression, promote it to a module
        temp = promote_expr_to_module(src);
        module_src = &temp;
    }

    let loaded = {
        let dir = tempdir()?;

        for (file, source) in dependencies {
            std::fs::write(dir.path().join(format!("{file}.roc")), source)?;
        }

        let filename = PathBuf::from("Test.roc");
        let file_path = dir.path().join(filename);
        let result = roc_load::load_and_typecheck_str(
            arena,
            file_path,
            module_src,
            dir.path().to_path_buf(),
            None,
            roc_target::Target::LinuxX64,
            function_kind,
            roc_reporting::report::RenderTarget::Generic,
            RocCacheDir::Disallowed,
            roc_reporting::report::DEFAULT_PALETTE,
        );

        dir.close()?;

        result
    };

    let loaded = loaded.expect("failed to load module");
    Ok((loaded, module_src.to_string()))
}

pub fn format_problems(
    src: &str,
    home: ModuleId,
    interns: &Interns,
    can_problems: Vec<Problem>,
    type_problems: Vec<TypeError>,
) -> (String, String) {
    let filename = PathBuf::from("test.roc");
    let src_lines: Vec<&str> = src.split('\n').collect();
    let lines = LineInfo::new(src);
    let alloc = RocDocAllocator::new(&src_lines, home, interns);

    let mut can_reports = vec![];
    let mut type_reports = vec![];

    for problem in can_problems {
        let report = can_problem(&alloc, &lines, filename.clone(), problem.clone());
        can_reports.push(report.pretty(&alloc));
    }

    for problem in type_problems {
        if let Some(report) = type_problem(&alloc, &lines, filename.clone(), problem.clone()) {
            type_reports.push(report.pretty(&alloc));
        }
    }

    let mut can_reports_buf = String::new();
    let mut type_reports_buf = String::new();
    use roc_reporting::report::CiWrite;
    alloc
        .stack(can_reports)
        .1
        .render_raw(70, &mut CiWrite::new(&mut can_reports_buf))
        .unwrap();
    alloc
        .stack(type_reports)
        .1
        .render_raw(70, &mut CiWrite::new(&mut type_reports_buf))
        .unwrap();

    (can_reports_buf, type_reports_buf)
}

lazy_static! {
    /// Queries of the form
    ///
    /// ```text
    /// ^^^{(directive),*}?
    ///
    /// directive :=
    ///   -\d+   # shift the query left by N columns
    ///   inst   # instantiate the given generic instance
    /// ```
    static ref RE_TYPE_QUERY: Regex =
        Regex::new(r"(?P<where>\^+)(?:\{(?P<directives>.*?)\})?").unwrap();

    static ref RE_DIRECTIVE : Regex =
        Regex::new(r"(?:-(?P<sub>\d+))|(?P<inst>inst)").unwrap();
}

/// Markers of nested query lines, that should be skipped.
pub const MUTLILINE_MARKER: &str = "│";

#[derive(Debug, Clone)]
pub struct TypeQuery {
    query_region: Region,
    /// If true, the query is under a function call, which should be instantiated with the present
    /// value and have its nested queries printed.
    instantiate: bool,
    source: String,
    comment_column: u32,
    source_line_column: LineColumn,
}

/// Parse inference queries in a Roc program.
/// See [RE_TYPE_QUERY].
fn parse_queries(src: &str, line_info: &LineInfo) -> Vec<TypeQuery> {
    let mut queries = vec![];
    let mut consecutive_query_lines = 0;
    for (i, line) in src.lines().enumerate() {
        // If this is a query line, it should start with a comment somewhere before the query
        // lines.
        let comment_column = match line.find('#') {
            Some(i) => i as _,
            None => {
                consecutive_query_lines = 0;
                continue;
            }
        };

        let mut queries_on_line = RE_TYPE_QUERY.captures_iter(line).peekable();

        if queries_on_line.peek().is_none() || line.contains(MUTLILINE_MARKER) {
            consecutive_query_lines = 0;
            continue;
        } else {
            consecutive_query_lines += 1;
        }

        for capture in queries_on_line {
            let source = capture
                .get(0)
                .expect("full capture must always exist")
                .as_str()
                .to_string();

            let wher = capture.name("where").unwrap();

            let mut subtract_col = 0u32;
            let mut instantiate = false;

            if let Some(directives) = capture.name("directives") {
                for directive in directives.as_str().split(',') {
                    let directive = RE_DIRECTIVE
                        .captures(directive)
                        .unwrap_or_else(|| panic!("directive {directive} must match RE_DIRECTIVE"));
                    if let Some(sub) = directive.name("sub") {
                        subtract_col += sub.as_str().parse::<u32>().expect("must be a number");
                    }
                    if directive.name("inst").is_some() {
                        instantiate = true;
                    }
                }
            }

            let (source_start, source_end) = (wher.start() as u32, wher.end() as u32);
            let (query_start, query_end) = (source_start - subtract_col, source_end - subtract_col);

            let source_line_column = LineColumn {
                line: i as u32,
                column: source_start,
            };

            let query_region = {
                let last_line = i as u32 - consecutive_query_lines;
                let query_start_lc = LineColumn {
                    line: last_line,
                    column: query_start,
                };
                let query_end_lc = LineColumn {
                    line: last_line,
                    column: query_end,
                };
                let query_lc_region = LineColumnRegion::new(query_start_lc, query_end_lc);
                line_info.convert_line_column_region(query_lc_region)
            };

            queries.push(TypeQuery {
                query_region,
                source,
                comment_column,
                source_line_column,
                instantiate,
            });
        }
    }
    queries
}

#[derive(Default, Clone, Copy)]
pub struct InferOptions {
    pub allow_errors: bool,
    pub print_can_decls: bool,
    pub print_only_under_alias: bool,
    pub print_ranks: bool,
    pub print_variables: bool,
    pub no_promote: bool,
}

#[derive(Debug)]
pub enum Elaboration {
    Specialization {
        specialized_name: String,
        typ: String,
    },
    Source {
        source: String,
        typ: String,
    },
    Instantiation {
        typ: String,
        source: String,
        offset_line: u32,
        queries_in_instantiation: InferredQueries,
    },
}

#[derive(Debug)]
pub struct InferredQuery {
    pub elaboration: Elaboration,
    /// Where the comment before the query string was written in the source.
    pub comment_column: u32,
    /// Where the query string "^^^" itself was written in the source.
    pub source_line_column: LineColumn,
    /// The content of the query string.
    pub source: String,
}

pub struct Program {
    home: ModuleId,
    interns: Interns,
    declarations: Declarations,
}

impl Program {
    pub fn write_can_decls(&self, writer: &mut impl io::Write) -> io::Result<()> {
        use roc_can::debug::{pretty_write_declarations, PPCtx};
        let ctx = PPCtx {
            home: self.home,
            interns: &self.interns,
            print_lambda_names: true,
        };
        pretty_write_declarations(writer, &ctx, &self.declarations)
    }
}

#[derive(Debug)]
pub struct InferredQueries(Vec<InferredQuery>);

impl InferredQueries {
    /// Returns all inferred queries, sorted by
    ///   - increasing source line
    ///   - on ties, decreasing source column
    pub fn into_sorted(self) -> Vec<InferredQuery> {
        let mut queries = self.0;
        queries.sort_by(|lc1, lc2| {
            let line1 = lc1.source_line_column.line;
            let line2 = lc2.source_line_column.line;
            let col1 = lc1.source_line_column.column;
            let col2 = lc2.source_line_column.column;
            line1.cmp(&line2).then(col2.cmp(&col1))
        });
        queries
    }
}

pub struct InferredProgram {
    program: Program,
    inferred_queries: Vec<InferredQuery>,
}

impl InferredProgram {
    /// Decomposes the program and inferred queries.
    pub fn decompose(self) -> (InferredQueries, Program) {
        let Self {
            program,
            inferred_queries,
        } = self;

        (InferredQueries(inferred_queries), program)
    }
}

pub fn infer_queries<'a>(
    src: &str,
    dependencies: impl IntoIterator<Item = (&'a str, &'a str)>,
    options: InferOptions,
    allow_can_errors: bool,
    function_kind: FunctionKind,
) -> Result<InferredProgram, Box<dyn Error>> {
    let (
        LoadedModule {
            module_id: home,
            mut can_problems,
            mut type_problems,
            mut declarations_by_id,
            mut solved,
            interns,
            abilities_store,
            ..
        },
        src,
    ) = run_load_and_infer(src, dependencies, options.no_promote, function_kind)?;

    let declarations = declarations_by_id.remove(&home).unwrap();
    let subs = solved.inner_mut();

    let can_problems = can_problems.remove(&home).unwrap_or_default();
    let type_problems = type_problems.remove(&home).unwrap_or_default();

    {
        let (can_problems, type_problems) =
            format_problems(&src, home, &interns, can_problems, type_problems);

        if !can_problems.is_empty() && !allow_can_errors {
            return Err(format!("Canonicalization problems: {can_problems}",).into());
        }
        if !type_problems.is_empty() && !options.allow_errors {
            return Err(format!("Type problems: {type_problems}",).into());
        }
    }

    let line_info = LineInfo::new(&src);
    let queries = parse_queries(&src, &line_info);

    let mut inferred_queries = Vec::with_capacity(queries.len());
    let exposed_by_module = ExposedByModule::default();
    let arena = Bump::new();

    let mut ctx = QueryCtx {
        all_queries: &queries,
        arena: &arena,
        source: &src,
        declarations: &declarations,
        subs,
        abilities_store: &abilities_store,
        home,
        interns: &interns,
        line_info,
        derived_module: Default::default(),
        exposed_by_module,
        options,
    };

    for query in queries.iter() {
        let answer = ctx.answer(query)?;
        inferred_queries.push(answer);
    }

    Ok(InferredProgram {
        program: Program {
            home,
            interns,
            declarations,
        },
        inferred_queries,
    })
}

struct QueryCtx<'a> {
    all_queries: &'a [TypeQuery],
    arena: &'a Bump,
    source: &'a str,
    declarations: &'a Declarations,
    subs: &'a mut Subs,
    abilities_store: &'a AbilitiesStore,
    home: ModuleId,
    interns: &'a Interns,
    line_info: LineInfo,
    derived_module: SharedDerivedModule,
    exposed_by_module: ExposedByModule,
    options: InferOptions,
}

impl<'a> QueryCtx<'a> {
    fn answer(&mut self, query: &TypeQuery) -> Result<InferredQuery, Box<dyn Error>> {
        let TypeQuery {
            query_region,
            source,
            comment_column,
            source_line_column,
            instantiate,
        } = query;

        let start = query_region.start().offset;
        let end = query_region.end().offset;
        let text = &self.source[start as usize..end as usize];
        let var = find_type_at(*query_region, self.declarations).ok_or_else(|| {
            format!(
                "No type for {:?} ({:?})!",
                &text,
                self.line_info.convert_region(*query_region)
            )
        })?;

        let snapshot = self.subs.snapshot();

        let type_string = name_and_print_var(
            var,
            self.subs,
            self.home,
            self.interns,
            DebugPrint {
                print_lambda_sets: true,
                print_only_under_alias: self.options.print_only_under_alias,
                print_ranks: self.options.print_ranks,
                print_variables: self.options.print_variables,
                ignore_polarity: true,
                print_weakened_vars: true,
            },
        );

        let elaboration = if *instantiate {
            self.infer_instantiated(var, type_string, *query_region, text)?
        } else {
            self.infer_direct(type_string, *query_region, text)
        };
        self.subs.rollback_to(snapshot);

        Ok(InferredQuery {
            elaboration,
            comment_column: *comment_column,
            source_line_column: *source_line_column,
            source: source.to_string(),
        })
    }

    fn infer_direct(&mut self, typ: String, query_region: Region, text: &str) -> Elaboration {
        match find_symbol_at(query_region, self.declarations, self.abilities_store) {
            Some(found_symbol) => match found_symbol {
                FoundSymbol::Specialization(spec_type, spec_symbol)
                | FoundSymbol::AbilityMember(spec_type, spec_symbol) => {
                    Elaboration::Specialization {
                        specialized_name: format!(
                            "{}#{}({})",
                            spec_type.as_str(self.interns),
                            text,
                            spec_symbol.ident_id().index(),
                        ),
                        typ,
                    }
                }
                FoundSymbol::Symbol(symbol) => Elaboration::Source {
                    source: symbol.as_str(self.interns).to_owned(),
                    typ,
                },
            },
            None => Elaboration::Source {
                source: text.to_owned(),
                typ,
            },
        }
    }

    fn infer_instantiated(
        &mut self,
        var: Variable,
        typ: String,
        query_region: Region,
        text: &str,
    ) -> Result<Elaboration, Box<dyn Error>> {
        let symbol = match find_symbol_at(query_region, self.declarations, self.abilities_store) {
            Some(FoundSymbol::Symbol(symbol) | FoundSymbol::Specialization(_, symbol)) => symbol,
            _ => return Err(format!("No symbol under {text:?}",).into()),
        };

        let def = find_declaration(symbol, self.declarations)
            .ok_or_else(|| format!("No def found for {text:?}"))?;

        let region = def.region();
        let LineColumnRegion { start, end } = self.line_info.convert_region(region);

        let start_pos = self.line_info.convert_line_column(LineColumn {
            line: start.line,
            column: 0,
        });
        let end_pos = self.line_info.convert_line_column(LineColumn {
            line: end.line + 1,
            column: 0,
        });
        let def_region = Region::new(start_pos, end_pos);
        let def_source = &self.source[start_pos.offset as usize..end_pos.offset as usize];

        instantiate_rigids(self.subs, def.var());
        roc_late_solve::unify(
            self.home,
            self.arena,
            self.subs,
            &AbilitiesView::Module(self.abilities_store),
            &self.derived_module,
            &self.exposed_by_module,
            var,
            def.var(),
        )
        .map_err(|_| "does not unify")?;

        let queries_in_instantiation = self
            .all_queries
            .iter()
            .filter(|query| def_region.contains(&query.query_region))
            .map(|query| self.answer(query))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Elaboration::Instantiation {
            typ,
            source: def_source.to_owned(),
            offset_line: start.line,
            queries_in_instantiation: InferredQueries(queries_in_instantiation),
        })
    }
}
