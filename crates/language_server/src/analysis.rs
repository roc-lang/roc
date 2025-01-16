use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use bumpalo::Bump;

use parking_lot::Mutex;
use roc_can::{abilities::AbilitiesStore, expr::Declarations};
use roc_collections::{MutMap, MutSet, VecMap};
use roc_load::{docs::ModuleDocumentation, CheckedModule, LoadedModule};
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_packaging::cache::{self, RocCacheDir};
use roc_region::all::LineInfo;
use roc_reporting::report::RocDocAllocator;
use roc_solve_problem::TypeError;
use roc_types::subs::{Subs, Variable};

use tower_lsp::lsp_types::{Diagnostic, SemanticTokenType, Url};

mod analysed_doc;
mod annotation_visitor;
mod completion;
mod parse_ast;
mod semantic_tokens;
mod tokens;
mod utils;

use crate::convert::diag::{IntoLspDiagnostic, ProblemFmt};

pub(crate) use self::analysed_doc::{AnalyzedDocument, DocInfo};
use self::{analysed_doc::ModuleIdToUrl, tokens::Token};

pub const HIGHLIGHT_TOKENS_LEGEND: &[SemanticTokenType] = Token::LEGEND;

#[derive(Debug)]
struct ModulesInfo {
    subs_by_module: HashMap<ModuleId, Mutex<Subs>>,
    exposed_by_module: HashMap<ModuleId, Arc<Vec<(Symbol, Variable)>>>,
    docs_by_module: HashMap<ModuleId, ModuleDocumentation>,
}

impl ModulesInfo {
    fn with_subs<F, A>(&self, module_id: &ModuleId, f: F) -> Option<A>
    where
        F: FnOnce(&mut Subs) -> A,
    {
        let subs = self.subs_by_module.get(module_id)?;
        Some(f(&mut subs.lock()))
    }

    fn get_docs(&self, module_id: &ModuleId) -> Option<&ModuleDocumentation> {
        self.docs_by_module.get(module_id)
    }

    fn from_loaded_module(
        exposes: MutMap<ModuleId, Vec<(Symbol, Variable)>>,
        typechecked: &MutMap<ModuleId, CheckedModule>,
        docs_by_module: VecMap<ModuleId, ModuleDocumentation>,
    ) -> ModulesInfo {
        let exposed_by_module = exposes
            .into_iter()
            .map(|(module_id, symbols)| (module_id, Arc::new(symbols)))
            .collect::<HashMap<_, _>>();

        let subs_by_module = typechecked
            .iter()
            .map(|(module_id, checked_module)| {
                (*module_id, checked_module.solved_subs.0.clone().into())
            })
            .collect::<HashMap<_, _>>();

        let docs_by_module = docs_by_module.into_iter().collect();

        ModulesInfo {
            subs_by_module,
            exposed_by_module,
            docs_by_module,
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct AnalyzedModule {
    exposed_imports: Vec<(Symbol, Variable)>,
    imports_by_module: HashMap<ModuleId, Arc<Vec<(Symbol, Variable)>>>,
    module_id: ModuleId,
    interns: Interns,
    subs: Subs,
    abilities: AbilitiesStore,
    declarations: Declarations,
    modules_info: Arc<ModulesInfo>,
    // ModuleIds are not stable between compilations, so a ModuleId visible to
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
        None,
        roc_target::Target::LinuxX64,
        roc_load::FunctionKind::LambdaSet,
        roc_reporting::report::RenderTarget::LanguageServer,
        RocCacheDir::Persistent(cache::roc_cache_packages_dir().as_path()),
        roc_reporting::report::DEFAULT_PALETTE,
    );

    let module = match loaded {
        Ok(module) => module,
        Err(problem) => {
            let all_problems = problem
                .into_lsp_diagnostic(&doc_info.line_info)
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
        exposed_imports,
        mut imports,
        exposes,
        docs_by_module,
        ..
    } = module;

    let mut root_module = Some(RootModule {
        subs: solved.into_inner(),
        abilities_store,
    });

    let exposed_imports = resolve_exposed_imports(exposed_imports, &exposes);

    let modules_info = Arc::new(ModulesInfo::from_loaded_module(
        exposes,
        &typechecked,
        docs_by_module,
    ));

    let mut builder = AnalyzedDocumentBuilder {
        interns: &interns,
        module_id_to_url: module_id_to_url_from_sources(&sources),
        can_problems: &mut can_problems,
        type_problems: &mut type_problems,
        declarations_by_id: &mut declarations_by_id,
        typechecked: &mut typechecked,
        root_module: &mut root_module,
        exposed_imports,
        imports: &mut imports,
        modules_info,
    };

    for (module_id, (path, source)) in sources {
        let doc = builder.build_document(path, source, module_id, doc_info.version);
        documents.push(doc);
    }

    documents
}

/// Take the exposed imports from each module, lookup the symbol within that module's list of
/// exposed symbols and then get the type info for that import.
/// example: `import Foo exposing [bar]`. `bar` is an exposed_import, so we need to lookup its type info.
fn resolve_exposed_imports(
    exposed_imports: MutMap<ModuleId, MutMap<Symbol, roc_region::all::Region>>,
    exposes: &MutMap<ModuleId, Vec<(Symbol, Variable)>>,
) -> HashMap<ModuleId, Vec<(Symbol, Variable)>> {
    let get_exposed_symbol_info = |symbol: &Symbol, module_id: &ModuleId| {
        exposes
            .get(module_id)?
            .iter()
            .find(|(symb, _)| symb == symbol)
    };

    exposed_imports
        .into_iter()
        .map(|(module_id, symbols)| {
            (
                module_id,
                symbols
                    .into_iter()
                    .filter_map(|(symbol, _)| get_exposed_symbol_info(&symbol, &module_id))
                    .cloned()
                    .collect::<Vec<_>>(),
            )
        })
        .collect()
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
    imports: &'a mut MutMap<ModuleId, MutSet<ModuleId>>,
    exposed_imports: HashMap<ModuleId, Vec<(Symbol, Variable)>>,
    modules_info: Arc<ModulesInfo>,
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

        //lookup the type info for each import from the module where it was exposed
        let this_imports = self.imports.remove(&module_id).unwrap_or_default();
        let imports = self.get_symbols_for_imports(this_imports);

        let exposed_imports = self.exposed_imports.remove(&module_id).unwrap_or_default();

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
            exposed_imports,
            imports_by_module: imports,
            subs,
            abilities,
            declarations,
            module_id,
            modules_info: self.modules_info.clone(),
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

    ///Gets the exposed symbols, and type info for each imported module
    fn get_symbols_for_imports(
        &mut self,
        imports: MutSet<ModuleId>,
    ) -> HashMap<ModuleId, Arc<Vec<(Symbol, Variable)>>> {
        imports
            .into_iter()
            .map(|id| {
                (
                    id,
                    self.modules_info
                        .exposed_by_module
                        .get(&id)
                        .unwrap_or(&Arc::new(vec![]))
                        .clone(),
                )
            })
            .collect::<HashMap<_, _>>()
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
