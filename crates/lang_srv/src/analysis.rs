use std::path::{Path, PathBuf};

use bumpalo::Bump;
use roc_can::{abilities::AbilitiesStore, expr::Declarations};
use roc_collections::MutMap;
use roc_fmt::{Ast, Buf};
use roc_load::{CheckedModule, LoadedModule};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_packaging::cache::{self, RocCacheDir};
use roc_parse::parser::SyntaxError;
use roc_region::all::LineInfo;
use roc_reporting::report::RocDocAllocator;
use roc_solve_problem::TypeError;
use roc_types::subs::Subs;
use tower_lsp::lsp_types::{
    Diagnostic, GotoDefinitionResponse, Hover, HoverContents, Location, MarkedString, Position,
    Range, TextEdit, Url,
};

use crate::convert::{
    diag::{IntoLspDiagnostic, ProblemFmt},
    ToRange, ToRocPosition,
};

pub(crate) struct GlobalAnalysis {
    pub documents: Vec<AnalyzedDocument>,
}

impl GlobalAnalysis {
    pub fn new(source_url: Url, source: String) -> GlobalAnalysis {
        let arena = Bump::new();

        let fi = source_url.to_file_path().unwrap();
        let src_dir = find_src_dir(&fi).to_path_buf();
        let line_info = LineInfo::new(&source);

        let loaded = roc_load::load_and_typecheck_str(
            &arena,
            fi,
            &source,
            src_dir,
            roc_target::TargetInfo::default_x86_64(),
            roc_load::FunctionKind::LambdaSet,
            roc_reporting::report::RenderTarget::Generic,
            RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
            roc_reporting::report::DEFAULT_PALETTE,
        );

        let module = match loaded {
            Ok(module) => module,
            Err(problem) => {
                let all_problems = problem
                    .into_lsp_diagnostic(&())
                    .into_iter()
                    .collect::<Vec<_>>();

                let analyzed_document = AnalyzedDocument {
                    url: source_url,
                    line_info,
                    source,
                    module: None,
                    diagnostics: all_problems,
                };

                return GlobalAnalysis {
                    documents: vec![analyzed_document],
                };
            }
        };

        let mut documents = vec![];

        let LoadedModule {
            module_id,
            interns,
            mut can_problems,
            mut type_problems,
            mut declarations_by_id,
            sources,
            mut typechecked,
            solved,
            abilities_store,
            ..
        } = module;

        let mut root_module = Some(RootModule {
            module_id,
            subs: solved.into_inner(),
            abilities_store,
        });

        let mut builder = AnalyzedDocumentBuilder {
            interns: &interns,
            can_problems: &mut can_problems,
            type_problems: &mut type_problems,
            declarations_by_id: &mut declarations_by_id,
            typechecked: &mut typechecked,
            root_module: &mut root_module,
        };

        for (module_id, (path, source)) in sources {
            documents.push(builder.build_document(path, source, module_id));
        }

        GlobalAnalysis { documents }
    }
}

fn find_src_dir(path: &Path) -> &Path {
    path.parent().unwrap_or(path)
}

fn _find_parent_git_repo(path: &Path) -> Option<&Path> {
    let mut path = path;
    loop {
        if path.join(".git").exists() {
            return Some(path);
        }

        path = path.parent()?;
    }
}

struct RootModule {
    module_id: ModuleId,
    subs: Subs,
    abilities_store: AbilitiesStore,
}

struct AnalyzedDocumentBuilder<'a> {
    interns: &'a Interns,
    can_problems: &'a mut MutMap<ModuleId, Vec<roc_problem::can::Problem>>,
    type_problems: &'a mut MutMap<ModuleId, Vec<TypeError>>,
    declarations_by_id: &'a mut MutMap<ModuleId, Declarations>,
    typechecked: &'a mut MutMap<ModuleId, CheckedModule>,
    root_module: &'a mut Option<RootModule>,
}

impl<'a> AnalyzedDocumentBuilder<'a> {
    fn build_document(
        &mut self,
        path: PathBuf,
        source: Box<str>,
        module_id: ModuleId,
    ) -> AnalyzedDocument {
        let subs;
        let abilities;
        let declarations;

        if let Some(m) = self.typechecked.remove(&module_id) {
            subs = m.solved_subs.into_inner();
            abilities = m.abilities_store;
            declarations = m.decls;
        } else {
            let rm = self.root_module.take().unwrap();
            assert!(rm.module_id == module_id);
            subs = rm.subs;
            abilities = rm.abilities_store;
            declarations = self.declarations_by_id.remove(&module_id).unwrap();
        }

        let analyzed_module = AnalyzedModule {
            subs,
            abilities,
            declarations,
            module_id,
            interns: self.interns.clone(),
        };

        let line_info = LineInfo::new(&source);
        let diagnostics = self.build_diagnostics(&path, &source, &line_info, module_id);

        let path = if path.is_relative() {
            // Make it <tmpdir>/path
            let tmpdir = std::env::temp_dir();
            tmpdir.join(path)
        } else {
            path
        };

        AnalyzedDocument {
            url: Url::from_file_path(path).unwrap(),
            line_info,
            source: source.into(),
            module: Some(analyzed_module),
            diagnostics,
        }
    }

    fn build_diagnostics(
        &mut self,
        source_path: &Path,
        source: &str,
        line_info: &LineInfo,
        module_id: ModuleId,
    ) -> Vec<Diagnostic> {
        let lines: Vec<_> = source.lines().collect();

        let alloc = RocDocAllocator::new(&lines, module_id, self.interns);

        let mut all_problems = Vec::new();
        let fmt = ProblemFmt {
            alloc: &alloc,
            line_info,
            path: source_path,
        };

        let can_problems = self.can_problems.remove(&module_id).unwrap_or_default();

        let type_problems = self.type_problems.remove(&module_id).unwrap_or_default();

        for can_problem in can_problems {
            if let Some(diag) = can_problem.into_lsp_diagnostic(&fmt) {
                all_problems.push(diag);
            }
        }

        for type_problem in type_problems {
            if let Some(diag) = type_problem.into_lsp_diagnostic(&fmt) {
                all_problems.push(diag);
            }
        }

        all_problems
    }
}

#[derive(Debug)]
struct AnalyzedModule {
    module_id: ModuleId,
    interns: Interns,
    subs: Subs,
    abilities: AbilitiesStore,
    declarations: Declarations,
}

#[derive(Debug)]
pub(crate) struct AnalyzedDocument {
    url: Url,
    line_info: LineInfo,
    source: String,
    module: Option<AnalyzedModule>,
    diagnostics: Vec<Diagnostic>,
}

impl AnalyzedDocument {
    pub fn url(&self) -> &Url {
        &self.url
    }

    pub fn module_id(&self) -> Option<ModuleId> {
        self.module.as_ref().map(|m| m.module_id)
    }

    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }

    fn module_mut(&mut self) -> Option<&mut AnalyzedModule> {
        self.module.as_mut()
    }

    fn module(&self) -> Option<&AnalyzedModule> {
        self.module.as_ref()
    }

    fn location(&self, range: Range) -> Location {
        Location {
            uri: self.url.clone(),
            range,
        }
    }

    fn whole_document_range(&self) -> Range {
        let line_info = self.line_info();
        let start = Position::new(0, 0);
        let end = Position::new(line_info.num_lines(), 0);
        Range::new(start, end)
    }

    pub fn diagnostics(&mut self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }

    pub fn symbol_at(&self, position: Position) -> Option<Symbol> {
        let line_info = self.line_info();

        let position = position.to_roc_position(line_info);

        let AnalyzedModule {
            declarations,
            abilities,
            ..
        } = self.module()?;

        let found_symbol =
            roc_can::traverse::find_closest_symbol_at(position, declarations, abilities)?;

        Some(found_symbol.implementation_symbol())
    }

    pub fn hover(&mut self, position: Position) -> Option<Hover> {
        let line_info = self.line_info();

        let pos = position.to_roc_position(line_info);

        let AnalyzedModule {
            subs,
            declarations,
            module_id,
            interns,
            ..
        } = self.module_mut()?;

        let (region, var) = roc_can::traverse::find_closest_type_at(pos, declarations)?;

        let snapshot = subs.snapshot();
        let type_str = roc_types::pretty_print::name_and_print_var(
            var,
            subs,
            *module_id,
            interns,
            roc_types::pretty_print::DebugPrint::NOTHING,
        );
        subs.rollback_to(snapshot);

        let range = region.to_range(self.line_info());

        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(type_str)),
            range: Some(range),
        })
    }

    pub fn definition(&self, symbol: Symbol) -> Option<GotoDefinitionResponse> {
        let AnalyzedModule { declarations, .. } = self.module()?;

        let found_declaration = roc_can::traverse::find_declaration(symbol, declarations)?;

        let range = found_declaration.region().to_range(self.line_info());

        Some(GotoDefinitionResponse::Scalar(self.location(range)))
    }

    pub fn format(&self) -> Option<Vec<TextEdit>> {
        let source = &self.source;
        let arena = &Bump::new();
        let ast = parse_all(arena, &self.source).ok()?;
        let mut buf = Buf::new_in(arena);
        fmt_all(&mut buf, &ast);
        let new_source = buf.as_str();

        if source == new_source {
            None
        } else {
            let range = self.whole_document_range();
            let text_edit = TextEdit::new(range, new_source.to_string());
            Some(vec![text_edit])
        }
    }
}

fn parse_all<'a>(arena: &'a Bump, src: &'a str) -> Result<Ast<'a>, SyntaxError<'a>> {
    use roc_parse::{
        module::{module_defs, parse_header},
        parser::Parser,
        state::State,
    };

    let (module, state) = parse_header(arena, State::new(src.as_bytes()))
        .map_err(|e| SyntaxError::Header(e.problem))?;

    let (_, defs, _) = module_defs().parse(arena, state, 0).map_err(|(_, e)| e)?;

    Ok(Ast { module, defs })
}

fn fmt_all<'a>(buf: &mut Buf<'a>, ast: &'a Ast) {
    roc_fmt::module::fmt_module(buf, &ast.module);

    roc_fmt::def::fmt_defs(buf, &ast.defs, 0);

    buf.fmt_end_of_file();
}
