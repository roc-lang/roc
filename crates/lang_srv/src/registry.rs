use std::{collections::HashMap, io::Write};

use tower_lsp::lsp_types::{
    CompletionResponse, Diagnostic, GotoDefinitionResponse, Hover, Position, SemanticTokensResult,
    TextEdit, Url,
};

use crate::analysis::{AnalyzedDocument, GlobalAnalysis};

pub(crate) enum DocumentChange {
    Modified(Url, String),
    Closed(Url),
}

#[derive(Debug)]
pub(crate) struct Document {
    latest_document: AnalyzedDocument,
    last_good_document: AnalyzedDocument,
}
#[derive(Debug, Default)]
pub(crate) struct Registry {
    documents: HashMap<Url, Document>,
}

impl Registry {
    fn update_document(&mut self, document: AnalyzedDocument) {
        let url = document.url().clone();

        match self.documents.get_mut(&url) {
            Some(doc) => {
                if document.type_checked() {
                    self.documents.insert(
                        url.clone(),
                        Document {
                            //TODO not sure if i should actually be cloning here?
                            latest_document: document.clone(),
                            last_good_document: document,
                        },
                    );
                } else {
                    //TODO this seems ugly but for now i'll let it slide. shoudl be immutable
                    doc.latest_document = document;
                }
            }
            None => {
                self.documents.insert(
                    url.clone(),
                    Document {
                        latest_document: document.clone(),
                        last_good_document: document,
                    },
                );
            }
        }
    }
    pub fn apply_change(&mut self, change: DocumentChange) {
        match change {
            DocumentChange::Modified(url, source) => {
                let GlobalAnalysis { documents } = GlobalAnalysis::new(url, source);

                // Only replace the set of documents and all dependencies that were re-analyzed.
                // Note that this is actually the opposite of what we want - in truth we want to
                // re-evaluate all dependents!
                for document in documents {
                    self.update_document(document);
                }
            }
            DocumentChange::Closed(_url) => {
                // Do nothing.
            }
        }
    }

    fn document_by_url(&mut self, url: &Url) -> Option<&mut AnalyzedDocument> {
        self.documents.get_mut(url).map(|a| &mut a.latest_document)
    }
    fn document_pair_by_url(&mut self, url: &Url) -> Option<&mut Document> {
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
    pub fn completion_items(
        &mut self,
        url: &Url,
        position: Position,
    ) -> Option<CompletionResponse> {
        let Document {
            latest_document,
            last_good_document,
        } = self.document_pair_by_url(url)?;
        let mut stderr = std::io::stderr();
        writeln!(&mut stderr, "got document");
        let symbol_prefix = latest_document.get_prefix_at_position(position);
        //this strategy probably won't work for record fields
        let completions = last_good_document.completion_items(position, symbol_prefix)?;

        Some(CompletionResponse::Array(completions))
    }
}
