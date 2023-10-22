use std::collections::HashMap;

use bumpalo::Bump;
use roc_can::{abilities::AbilitiesStore, expr::Declarations};
use roc_load::LoadedModule;
use roc_module::symbol::{Interns, ModuleId, Symbol};
use roc_packaging::cache::{self, RocCacheDir};
use roc_region::all::{LineInfo, Region};
use roc_reporting::report::RocDocAllocator;
use roc_types::subs::Subs;
use tower_lsp::lsp_types::{
    Diagnostic, GotoDefinitionResponse, Hover, HoverContents, MarkedString, Position, Range, Url,
};

use crate::convert::{
    diag::{IntoLspDiagnostic, ProblemFmt},
    ToRange, ToRocPosition,
};

pub(crate) enum DocumentChange {
    Modified(Url, String),
    Closed(Url),
}

#[derive(Debug)]
struct Analysis {
    line_info: LineInfo,
    module: Option<LoadedModule>,
    diagnostics: Vec<Diagnostic>,
}

impl Analysis {
    fn new(url: &Url, source: &str, arena: &Bump) -> Self {
        let fi = url.to_file_path().unwrap();
        let src_dir = fi.parent().unwrap().to_path_buf();

        let mut loaded = roc_load::load_and_typecheck_str(
            arena,
            fi,
            source,
            src_dir,
            roc_target::TargetInfo::default_x86_64(),
            roc_load::FunctionKind::LambdaSet,
            roc_reporting::report::RenderTarget::Generic,
            RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
            roc_reporting::report::DEFAULT_PALETTE,
        );

        let line_info = LineInfo::new(source);

        let diagnostics = match loaded.as_mut() {
            Ok(module) => {
                let lines: Vec<_> = source.lines().collect();

                let alloc = RocDocAllocator::new(&lines, module.module_id, &module.interns);

                let mut all_problems = Vec::new();
                let module_path = url.to_file_path().unwrap();
                let fmt = ProblemFmt {
                    alloc: &alloc,
                    line_info: &line_info,
                    path: &module_path,
                };

                for can_problem in module
                    .can_problems
                    .remove(&module.module_id)
                    .unwrap_or_default()
                {
                    if let Some(diag) = can_problem.into_lsp_diagnostic(&fmt) {
                        all_problems.push(diag);
                    }
                }

                for type_problem in module
                    .type_problems
                    .remove(&module.module_id)
                    .unwrap_or_default()
                {
                    if let Some(diag) = type_problem.into_lsp_diagnostic(&fmt) {
                        all_problems.push(diag);
                    }
                }

                all_problems
            }
            Err(problem) => {
                let mut all_problems = vec![];
                all_problems.extend(problem.into_lsp_diagnostic(&()));
                all_problems
            }
        };

        Self {
            line_info: LineInfo::new(source),
            module: loaded.ok(),
            diagnostics,
        }
    }
}

#[derive(Debug)]
struct Document {
    url: Url,
    source: String,

    arena: Bump,

    analysis: Analysis,
}

impl Document {
    fn new(url: Url, source: String) -> Self {
        let arena = Bump::new();
        let analysis = Analysis::new(&url, &source, &arena);

        Self {
            url,
            source,
            arena,
            analysis,
        }
    }

    fn refresh(&mut self, source: String) {
        self.source = source;
        self.analysis = Analysis::new(&self.url, &self.source, &self.arena);
    }

    fn line_info(&self) -> &LineInfo {
        &self.analysis.line_info
    }

    fn module(&mut self) -> Option<&mut LoadedModule> {
        self.analysis.module.as_mut()
    }

    fn diagnostics(&mut self) -> Vec<Diagnostic> {
        self.analysis.diagnostics.clone()
    }

    fn split_module(&mut self) -> Option<SplitModule<'_>> {
        self.module()?.try_into().ok()
    }

    fn symbol_at(&mut self, position: Position) -> Option<Symbol> {
        let line_info = self.line_info();

        let region = Region::from_pos(position.to_roc_position(line_info));

        let SplitModule {
            decls,
            abilities_store,
            ..
        } = self.split_module()?;

        let found_symbol = roc_can::traverse::find_symbol_at(region, decls, abilities_store)?;

        Some(found_symbol.implementation_symbol())
    }

    fn hover(&mut self, position: Position) -> Option<Hover> {
        let line_info = self.line_info();

        let pos = position.to_roc_position(line_info);

        let SplitModule {
            subs,
            decls,
            module_id,
            interns,
            ..
        } = self.split_module()?;

        let (region, var) = roc_can::traverse::find_closest_type_at(pos, decls)?;

        let snapshot = subs.snapshot();
        let type_str = roc_types::pretty_print::name_and_print_var(
            var,
            subs,
            module_id,
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
}

struct SplitModule<'a> {
    subs: &'a mut Subs,
    abilities_store: &'a AbilitiesStore,
    decls: &'a Declarations,
    module_id: ModuleId,
    interns: &'a Interns,
}

impl<'a> TryFrom<&'a mut LoadedModule> for SplitModule<'a> {
    type Error = ();

    fn try_from(module: &'a mut LoadedModule) -> Result<Self, Self::Error> {
        let module_id = module.module_id;
        let interns = &module.interns;
        let subs;
        let abilities_store;
        let decls;
        if let Some(m) = module.typechecked.get_mut(&module.module_id) {
            subs = &mut m.solved_subs;
            abilities_store = &m.abilities_store;
            decls = &m.decls;
        } else if let Some(d) = module.declarations_by_id.get(&module.module_id) {
            subs = &mut module.solved;
            abilities_store = &module.abilities_store;
            decls = d;
        } else {
            return Err(());
        }

        Ok(Self {
            subs: subs.inner_mut(),
            abilities_store,
            decls,
            module_id,
            interns,
        })
    }
}

fn missing_hover(module: &mut LoadedModule, position: Position) -> Option<Hover> {
    Some(Hover {
        contents: HoverContents::Scalar(MarkedString::String(format!(
            "{:?}",
            (module.typechecked.keys().collect::<Vec<_>>())
        ))),
        range: Some(Range::new(
            position,
            Position {
                line: position.line,
                character: position.character + 1,
            },
        )),
    })
}

#[derive(Debug, Default)]
pub(crate) struct Registry {
    documents: HashMap<Url, Document>,
}

impl Registry {
    pub fn apply_change(&mut self, change: DocumentChange) {
        match change {
            DocumentChange::Modified(url, source) => match self.documents.get_mut(&url) {
                Some(document) => document.refresh(source),
                None => {
                    self.documents
                        .insert(url.clone(), Document::new(url, source));
                }
            },
            DocumentChange::Closed(url) => {
                self.documents.remove(&url);
            }
        }
    }

    pub fn diagnostics(&mut self, document: &Url) -> Vec<Diagnostic> {
        self.documents.get_mut(document).unwrap().diagnostics()
    }

    pub fn hover(&mut self, document: &Url, position: Position) -> Option<Hover> {
        self.documents.get_mut(document).unwrap().hover(position)
    }

    pub fn goto_definition(
        &mut self,
        document: &Url,
        position: Position,
    ) -> Option<GotoDefinitionResponse> {
        let symbol = self
            .documents
            .get_mut(document)
            .unwrap()
            .symbol_at(position)?;
        None
    }
}
