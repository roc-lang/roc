use std::path::{Path, PathBuf};

use bumpalo::Bump;
use roc_can::{abilities::AbilitiesStore, expr::Declarations};
use roc_collections::MutMap;
use roc_load::{CheckedModule, LoadedModule};
use roc_module::symbol::{Interns, ModuleId};
use roc_packaging::cache::{self, RocCacheDir};
use roc_region::all::LineInfo;
use roc_reporting::report::RocDocAllocator;
use roc_solve_problem::TypeError;
use roc_types::subs::Subs;

use tower_lsp::lsp_types::{Diagnostic, SemanticTokenType, Url};

mod analysed_doc;
mod completion;
mod parse_ast;
mod semantic_tokens;
mod tokens;
mod utils;

use crate::convert::diag::{IntoLspDiagnostic, ProblemFmt};

pub(crate) use self::analysed_doc::{AnalyzedDocument, DocInfo};
use self::{analysed_doc::ModuleIdToUrl, tokens::Token};

pub const HIGHLIGHT_TOKENS_LEGEND: &[SemanticTokenType] = Token::LEGEND;

#[derive(Debug, Clone)]
pub(super) struct AnalyzedModule {
    module_id: ModuleId,
    interns: Interns,
    subs: Subs,
    abilities: AbilitiesStore,
    declarations: Declarations,
    // We need this because ModuleIds are not stable between compilations, so a ModuleId visible to
    // one module may not be true global to the language server.
    module_id_to_url: ModuleIdToUrl,
}
#[derive(Debug, Clone)]
pub struct AnalysisResult {
    module: Option<AnalyzedModule>,
    diagnostics: Vec<Diagnostic>,
}

pub(crate) fn global_analysis(doc_info: DocInfo) -> Vec<AnalyzedDocument> {
    let fi = doc_info.url.to_file_path().unwrap();
    let src_dir = find_src_dir(&fi).to_path_buf();

    let arena = Bump::new();
    let loaded = roc_load::load_and_typecheck_str(
        &arena,
        fi,
        &doc_info.source,
        src_dir,
        roc_target::TargetInfo::default_x86_64(),
        roc_load::FunctionKind::LambdaSet,
        roc_reporting::report::RenderTarget::Generic,
        RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
        roc_reporting::report::DEFAULT_PALETTE,
        false,
    );

    let module = match loaded {
        Ok(module) => module,
        Err(problem) => {
            let all_problems = problem
                .into_lsp_diagnostic(&())
                .into_iter()
                .collect::<Vec<_>>();

            let analyzed_document = AnalyzedDocument {
                doc_info,
                analysis_result: AnalysisResult {
                    module: None,
                    diagnostics: all_problems,
                },
            };

            return vec![analyzed_document];
        }
    };

    let mut documents = vec![];

    let LoadedModule {
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
        subs: solved.into_inner(),
        abilities_store,
    });

    let mut builder = AnalyzedDocumentBuilder {
        interns: &interns,
        module_id_to_url: module_id_to_url_from_sources(&sources),
        can_problems: &mut can_problems,
        type_problems: &mut type_problems,
        declarations_by_id: &mut declarations_by_id,
        typechecked: &mut typechecked,
        root_module: &mut root_module,
    };

    for (module_id, (path, source)) in sources {
        documents.push(builder.build_document(path, source, module_id, doc_info.version));
    }

    documents
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

fn module_id_to_url_from_sources(sources: &MutMap<ModuleId, (PathBuf, Box<str>)>) -> ModuleIdToUrl {
    sources
        .iter()
        .map(|(module_id, (path, _))| {
            let url = path_to_url(path);
            (*module_id, url)
        })
        .collect()
}

fn path_to_url(path: &Path) -> Url {
    if path.is_relative() {
        // Make it <tmpdir>/path
        let tmpdir = std::env::temp_dir();
        Url::from_file_path(tmpdir.join(path)).unwrap()
    } else {
        Url::from_file_path(path).unwrap()
    }
}

struct RootModule {
    subs: Subs,
    abilities_store: AbilitiesStore,
}

struct AnalyzedDocumentBuilder<'a> {
    interns: &'a Interns,
    module_id_to_url: ModuleIdToUrl,
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
        version: i32,
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
            module_id_to_url: self.module_id_to_url.clone(),
        };

        let line_info = LineInfo::new(&source);
        let diagnostics = self.build_diagnostics(&path, &source, &line_info, module_id);

        AnalyzedDocument {
            doc_info: DocInfo {
                url: path_to_url(&path),
                line_info,
                source: source.into(),
                version,
            },
            analysis_result: AnalysisResult {
                module: Some(analyzed_module),
                diagnostics,
            },
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
