use std::collections::HashMap;

use roc_module::symbol::ModuleId;
use tower_lsp::lsp_types::{Diagnostic, GotoDefinitionResponse, Hover, Position, TextEdit, Url};

use crate::analysis::{AnalyzedDocument, GlobalAnalysis};

pub(crate) enum DocumentChange {
    Modified(Url, String),
    Closed(Url),
}

#[derive(Debug, Default)]
pub(crate) struct Registry {
    documents: HashMap<Url, AnalyzedDocument>,
    module_id_to_url: HashMap<ModuleId, Url>,
}

impl Registry {
    pub fn apply_change(&mut self, change: DocumentChange) {
        match change {
            DocumentChange::Modified(url, source) => {
                let GlobalAnalysis { documents } = GlobalAnalysis::new(url, source);

                // Only replace the set of documents and all dependencies that were re-analyzed.
                // Note that this is actually the opposite of what we want - in truth we want to
                // re-evaluate all dependents!
                for document in documents {
                    let url = document.url().clone();
                    let module_id = document.module_id();
                    self.documents.insert(url.clone(), document);
                    if let Some(module_id) = module_id {
                        self.module_id_to_url.insert(module_id, url);
                    }
                }
            }
            DocumentChange::Closed(_url) => {
                // Do nothing.
            }
        }
    }

    fn document_by_url(&mut self, url: &Url) -> Option<&mut AnalyzedDocument> {
        self.documents.get_mut(url)
    }

    fn document_by_module_id(&mut self, module_id: ModuleId) -> Option<&mut AnalyzedDocument> {
        let url = self.module_id_to_url.get(&module_id)?;
        self.documents.get_mut(url)
    }

    pub fn diagnostics(&mut self, url: &Url) -> Vec<Diagnostic> {
        let Some(document) = self.document_by_url(url) else {
            return vec![];
        };
        document.diagnostics()
    }

    pub fn hover(&mut self, url: &Url, position: Position) -> Option<Hover> {
        self.document_by_url(url)?.hover(position)
    }

    pub fn goto_definition(
        &mut self,
        url: &Url,
        position: Position,
    ) -> Option<GotoDefinitionResponse> {
        let symbol = self.document_by_url(url)?.symbol_at(position)?;
        let def_document = self.document_by_module_id(symbol.module_id())?;
        def_document.definition(symbol)
    }

    pub fn formatting(&mut self, url: &Url) -> Option<Vec<TextEdit>> {
        let document = self.document_by_url(url)?;
        document.format()
    }
}
