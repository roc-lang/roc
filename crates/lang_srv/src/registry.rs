use std::collections::HashMap;

use bumpalo::Bump;
use roc_load::{LoadedModule, LoadingProblem};
use roc_region::all::{LineInfo, Region};
use roc_reporting::report::RocDocAllocator;
use roc_types::subs::Variable;
use tower_lsp::lsp_types::{Diagnostic, Hover, HoverContents, MarkedString, Position, Range, Url};

use crate::convert::{
    diag::{IntoLspDiagnostic, ProblemFmt},
    ToRange, ToRocPosition,
};

pub(crate) enum DocumentChange {
    Modified(Url, String),
    Closed(Url),
}

#[derive(Debug)]
struct Document {
    url: Url,
    source: String,

    arena: Bump,

    // Incrementally updated module, diagnostis, etc.
    line_info: Option<LineInfo>,
    module: Option<Result<LoadedModule, ()>>,
    diagnostics: Option<Vec<Diagnostic>>,
}

impl Document {
    fn new(url: Url, source: String) -> Self {
        Self {
            url,
            source,
            arena: Bump::new(),

            line_info: None,
            module: None,
            diagnostics: None,
        }
    }

    fn prime(&mut self, source: String) {
        self.source = source;
        self.module = None;
        self.diagnostics = None;
    }

    fn prime_line_info(&mut self) {
        if self.line_info.is_none() {
            self.line_info = Some(LineInfo::new(&self.source));
        }
    }

    fn line_info(&self) -> &LineInfo {
        &self.line_info.as_ref().unwrap()
    }

    fn module(&mut self) -> Result<&mut LoadedModule, LoadingProblem<'_>> {
        if let Some(Ok(module)) = &mut self.module {
            // Safety: returning for time self is alive
            return Ok(unsafe { std::mem::transmute(module) });
        }

        let fi = self.url.to_file_path().unwrap();
        let src_dir = fi.parent().unwrap().to_path_buf();

        let loaded = roc_load::load_and_typecheck_str(
            &self.arena,
            fi,
            &self.source,
            src_dir,
            Default::default(),
            roc_target::TargetInfo::default_x86_64(),
            roc_reporting::report::RenderTarget::Generic,
            roc_reporting::report::DEFAULT_PALETTE,
        );

        match loaded {
            Ok(module) => {
                self.module = Some(Ok(module));
                Ok(self.module.as_mut().unwrap().as_mut().unwrap())
            }
            Err(problem) => {
                self.module = Some(Err(()));
                Err(problem)
            }
        }
    }

    fn diagnostics(&mut self) -> Vec<Diagnostic> {
        if let Some(diagnostics) = &self.diagnostics {
            return diagnostics.clone();
        }

        let loaded: Result<&'static mut LoadedModule, LoadingProblem> =
            unsafe { std::mem::transmute(self.module()) };

        let diagnostics = match loaded {
            Ok(module) => {
                let line_info = {
                    self.prime_line_info();
                    self.line_info()
                };
                let lines: Vec<_> = self.source.lines().collect();

                let alloc = RocDocAllocator::new(&lines, module.module_id, &module.interns);

                let mut all_problems = Vec::new();
                let module_path = self.url.to_file_path().unwrap();
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

        self.diagnostics = Some(diagnostics);
        self.diagnostics.as_ref().unwrap().clone()
    }

    fn hover(&mut self, position: Position) -> Option<Hover> {
        let line_info = {
            self.prime_line_info();
            self.line_info()
        };

        let region = position.to_roc_position(line_info);

        let module = match self.module() {
            Ok(module) => module,
            Err(_) => {
                return Some(Hover {
                    contents: HoverContents::Scalar(MarkedString::String("bar".to_owned())),
                    range: Some(Range::new(
                        position,
                        Position {
                            line: position.line,
                            character: position.character + 1,
                        },
                    )),
                })
            }
        };

        let (subs, decls) = match module.typechecked.get_mut(&module.module_id) {
            Some(m) => (&mut m.solved_subs, &m.decls),
            None => match module.declarations_by_id.get(&module.module_id) {
                Some(decls) => (&mut module.solved, decls),
                None => {
                    return Some(Hover {
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
            },
        };
        let (region, var) = roc_can::traverse::find_closest_type_at(region, decls)
            .unwrap_or_else(|| (Region::new(region, region.bump_column(1)), Variable::NULL));

        let subs = subs.inner_mut();
        let snapshot = subs.snapshot();
        let type_str = roc_types::pretty_print::name_and_print_var(
            var,
            subs,
            module.module_id,
            &module.interns,
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

#[derive(Debug, Default)]
pub(crate) struct Registry {
    documents: HashMap<Url, Document>,
}

impl Registry {
    pub fn apply_change(&mut self, change: DocumentChange) {
        match change {
            DocumentChange::Modified(url, source) => match self.documents.get_mut(&url) {
                Some(document) => document.prime(source),
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
}
