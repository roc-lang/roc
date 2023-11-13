use std::collections::HashMap;

use tower_lsp::lsp_types::{
    Diagnostic, GotoDefinitionResponse, Hover, Position, SemanticTokensResult, TextEdit, Url,
};

use crate::analysis::{AnalyzedDocument, GlobalAnalysis};

pub(crate) enum DocumentChange {
    Modified(Url, String),
    Closed(Url),
}

#[derive(Debug, Default)]
pub(crate) struct Registry {
    documents: HashMap<Url, AnalyzedDocument>,
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
                    self.documents.insert(url.clone(), document);
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
        let document = self.document_by_url(url)?;
        let symbol = document.symbol_at(position)?;
        let def_document_url = document.module_url(symbol.module_id())?;
        let def_document = self.document_by_url(&def_document_url)?;
        def_document.definition(symbol)
    }

    pub fn formatting(&mut self, url: &Url) -> Option<Vec<TextEdit>> {
        let document = self.document_by_url(url)?;
        document.format()
    }

    pub fn semantic_tokens(&mut self, url: &Url) -> Option<SemanticTokensResult> {
        let document = self.document_by_url(url)?;
        document.semantic_tokens()
    }
}
