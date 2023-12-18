use std::{collections::HashMap, sync::Arc};

use tokio::sync::{Mutex, MutexGuard, RwLock, RwLockWriteGuard};
use tower_lsp::lsp_types::{
    CompletionResponse, Diagnostic, GotoDefinitionResponse, Hover, Position, SemanticTokensResult,
    TextEdit, Url,
};

use crate::analysis::{AnalyzedDocument, DocInfo};

#[derive(Debug)]
pub(crate) struct LatestDocument {
    pub info: DocInfo,
    analyzed: tokio::sync::RwLock<Option<Arc<AnalyzedDocument>>>,
}
impl LatestDocument {
    pub(crate) async fn get_latest(&self) -> Arc<AnalyzedDocument> {
        self.analyzed.read().await.as_ref().unwrap().clone()
    }
    pub(crate) fn get_lock(&self) -> RwLockWriteGuard<Option<Arc<AnalyzedDocument>>> {
        self.analyzed.blocking_write()
    }
    pub(crate) fn new(doc_info: DocInfo) -> LatestDocument {
        let val = RwLock::new(None);
        LatestDocument {
            info: doc_info,
            analyzed: val,
        }
    }
    pub(crate) fn new_initialised(analyzed: Arc<AnalyzedDocument>) -> LatestDocument {
        LatestDocument {
            info: analyzed.doc_info.clone(),
            analyzed: RwLock::new(Some(analyzed)),
        }
    }
}

#[derive(Debug)]
pub(crate) struct DocumentPair {
    latest_document: Arc<LatestDocument>,
    last_good_document: Arc<AnalyzedDocument>,
}

#[derive(Debug, Default)]
pub(crate) struct Registry {
    documents: Mutex<HashMap<Url, DocumentPair>>,
}

impl Registry {
    pub async fn get_latest_version(&self, url: &Url) -> Option<i32> {
        self.documents
            .lock()
            .await
            .get(&url)
            .map(|x| x.latest_document.info.version)
    }
    fn update_document<'a>(
        documents: &mut MutexGuard<'a, HashMap<Url, DocumentPair>>,
        document: AnalyzedDocument,
    ) {
        let url = document.url().clone();
        let document = Arc::new(document);
        let latest_doc = Arc::new(LatestDocument::new_initialised(document.clone()));
        match documents.get_mut(&url) {
            Some(old_doc) => {
                if document.type_checked() {
                    *old_doc = DocumentPair {
                        latest_document: latest_doc,
                        last_good_document: document,
                    };
                } else {
                    *old_doc = DocumentPair {
                        latest_document: latest_doc,
                        last_good_document: old_doc.last_good_document.clone(),
                    };
                }
            }
            None => {
                documents.insert(
                    url.clone(),
                    DocumentPair {
                        latest_document: latest_doc,
                        last_good_document: document,
                    },
                );
            }
        }
    }

    pub async fn apply_changes<'a>(
        &self,
        analysed_docs: Vec<AnalyzedDocument>,
        mut partial_writer: RwLockWriteGuard<'a, Option<Arc<AnalyzedDocument>>>,
        updating_url: Url,
    ) {
        let mut documents = self.documents.lock().await;
        eprintln!(
            "finised doc analysis updating docs {:?}",
            analysed_docs
                .iter()
                .map(|a| a.doc_info.url.to_string())
                .collect::<Vec<_>>()
        );
        let updates = analysed_docs.into_iter().filter_map(|a| {
            if a.doc_info.url == updating_url {
                *partial_writer = Some(Arc::new(a));
                None
            } else {
                Some(a)
            }
        });

        for document in updates {
            Registry::update_document(&mut documents, document);
        }
    }

    pub async fn apply_doc_info_changes(&self, url: Url, partial: Arc<LatestDocument>) {
        let mut lock = self.documents.lock().await;
        let doc = lock.get_mut(&url);
        match doc {
            Some(a) => {
                eprintln!(
                    "set the docInfo for {:?} to version:{:?}",
                    url.as_str(),
                    partial.info.version
                );

                a.latest_document = partial;
            }

            None => (),
        }
    }

    fn document_info_by_url(&self, url: &Url) -> Option<DocInfo> {
        self.documents
            .blocking_lock()
            .get(url)
            .map(|a| a.latest_document.info.clone())
    }
    async fn latest_document_by_url(&self, url: &Url) -> Option<Arc<AnalyzedDocument>> {
        match self.documents.lock().await.get(url) {
            Some(a) => Some(a.latest_document.get_latest().await),
            None => None,
        }
    }

    pub async fn diagnostics(&self, url: &Url) -> Vec<Diagnostic> {
        let Some( document) = self.latest_document_by_url(url).await else {
            return vec![];
        };
        document.diagnostics()
    }

    pub async fn hover(&self, url: &Url, position: Position) -> Option<Hover> {
        self.latest_document_by_url(url).await?.hover(position)
    }

    pub async fn goto_definition(
        &self,
        url: &Url,
        position: Position,
    ) -> Option<GotoDefinitionResponse> {
        let document = self.latest_document_by_url(url).await?;
        let symbol = document.symbol_at(position)?;
        let def_document_url = document.module_url(symbol.module_id())?;
        let def_document = self.latest_document_by_url(&def_document_url).await?;
        def_document.definition(symbol)
    }

    pub fn formatting(&self, url: &Url) -> Option<Vec<TextEdit>> {
        let document = self.document_info_by_url(url)?;
        document.format()
    }

    pub fn semantic_tokens(&self, url: &Url) -> Option<SemanticTokensResult> {
        let document = self.document_info_by_url(url)?;
        document.semantic_tokens()
    }
    pub async fn completion_items(
        &self,
        url: &Url,
        position: Position,
    ) -> Option<CompletionResponse> {
        let lock = self.documents.lock().await;
        let pair = lock.get(url)?;
        eprintln!("got document");
        let latest_doc_info = &pair.latest_document.info;
        eprintln!("latest version:{:?} ", latest_doc_info.version);

        let completions = pair
            .last_good_document
            .completion_items(position, &latest_doc_info)?;

        Some(CompletionResponse::Array(completions))
    }
}
