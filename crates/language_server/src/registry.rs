use log::{debug, info, trace};

use std::{
    collections::HashMap,
    sync::{Arc, OnceLock},
    time::Duration,
};

use tokio::sync::{Mutex, MutexGuard};

use tower_lsp::lsp_types::{
    CodeActionOrCommand, CodeActionResponse, CompletionResponse, Diagnostic,
    GotoDefinitionResponse, Hover, Position, Range, SemanticTokensResult, TextEdit, Url,
};

use crate::analysis::{AnalyzedDocument, DocInfo};

#[derive(Debug)]
pub(crate) struct DocumentPair {
    info: DocInfo,
    latest_document: OnceLock<Arc<AnalyzedDocument>>,
    last_good_document: Arc<AnalyzedDocument>,
}

impl DocumentPair {
    pub(crate) fn new(
        latest_doc: Arc<AnalyzedDocument>,
        last_good_document: Arc<AnalyzedDocument>,
    ) -> Self {
        Self {
            info: latest_doc.doc_info.clone(),
            latest_document: OnceLock::from(latest_doc),
            last_good_document,
        }
    }
}

#[derive(Debug)]
pub(crate) struct RegistryConfig {
    pub(crate) latest_document_timeout: Duration,
}

impl Default for RegistryConfig {
    fn default() -> Self {
        Self {
            latest_document_timeout: Duration::from_millis(5000),
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct Registry {
    documents: Mutex<HashMap<Url, DocumentPair>>,
    config: RegistryConfig,
}

impl Registry {
    pub(crate) fn new(config: RegistryConfig) -> Self {
        Self {
            documents: Default::default(),
            config,
        }
    }

    pub async fn get_latest_version(&self, url: &Url) -> Option<i32> {
        self.documents.lock().await.get(url).map(|x| x.info.version)
    }

    fn update_document(
        documents: &mut MutexGuard<'_, HashMap<Url, DocumentPair>>,
        document: Arc<AnalyzedDocument>,
        updating_url: &Url,
    ) {
        if &document.doc_info.url == updating_url {
            //Write the newly analysed document into the oncelock that any request requiring the latest document will be waiting on
            if let Some(a) = documents.get_mut(updating_url) {
                // We don't care if this fails because we expect the document to sometimes already be there
                a.latest_document.set(document.clone()).unwrap_or(())
            }
        }

        let url = document.url().clone();
        match documents.get_mut(&url) {
            Some(old_doc) => {
                //If the latest doc_info has a version higher than what we are setting we shouldn't overwrite the document, but we can update the last_good_document if the parse went well
                if old_doc.info.version > document.doc_info.version {
                    if document.type_checked() {
                        *old_doc = DocumentPair {
                            info: old_doc.info.clone(),
                            latest_document: old_doc.latest_document.clone(),
                            last_good_document: document,
                        };
                    }
                } else if document.type_checked() {
                    *old_doc = DocumentPair::new(document.clone(), document);
                } else {
                    debug!(
                        "Document typechecking failed at version {:?}, not updating last_good_document",
                        &document.doc_info.version
                    );
                    *old_doc = DocumentPair::new(document, old_doc.last_good_document.clone());
                }
            }
            None => {
                documents.insert(url.clone(), DocumentPair::new(document.clone(), document));
            }
        }
    }

    pub async fn apply_changes<'a>(&self, analysed_docs: Vec<AnalyzedDocument>, updating_url: Url) {
        let mut documents = self.documents.lock().await;
        debug!(
            "Finished doc analysis for doc: {}",
            updating_url.to_string()
        );

        for document in analysed_docs {
            let document = Arc::new(document);
            Registry::update_document(&mut documents, document, &updating_url);
        }
    }

    pub async fn apply_doc_info_changes(&self, url: Url, info: DocInfo) {
        let mut documents_lock = self.documents.lock().await;
        let doc = documents_lock.get_mut(&url);
        match doc {
            Some(a) => {
                debug!(
                    "Set the docInfo for {:?} to version:{:?}",
                    url.as_str(),
                    info.version
                );
                *a = DocumentPair {
                    info,
                    last_good_document: a.last_good_document.clone(),
                    latest_document: OnceLock::new(),
                };
            }
            None => debug!("So existing docinfo for {:?} ", url.as_str()),
        }
    }

    async fn document_info_by_url(&self, url: &Url) -> Option<DocInfo> {
        self.documents.lock().await.get(url).map(|a| a.info.clone())
    }

    ///Tries to get the latest document from analysis.
    ///Gives up and returns none after 5 seconds.
    async fn latest_document_by_url(&self, url: &Url) -> Option<Arc<AnalyzedDocument>> {
        tokio::time::timeout(self.config.latest_document_timeout, async {
            //TODO: This should really be a condvar that is triggered by the latest being ready, this will do for now though
            loop {
                let docs = self.documents.lock().await;
                if let Some(a) = docs.get(url) {
                    if let Some(a) = a.latest_document.get() {
                        return a.clone();
                    }
                }
                drop(docs);
                tokio::task::yield_now().await;
            }
        })
        .await
        .ok()
    }

    pub async fn diagnostics(&self, url: &Url) -> Vec<Diagnostic> {
        let Some(document) = self.latest_document_by_url(url).await else {
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

    pub async fn formatting(&self, url: &Url) -> Option<Vec<TextEdit>> {
        let document = self.document_info_by_url(url).await?;
        document.format()
    }

    pub async fn semantic_tokens(&self, url: &Url) -> Option<SemanticTokensResult> {
        let document = self.document_info_by_url(url).await?;
        document.semantic_tokens()
    }
    pub async fn completion_items(
        &self,
        url: &Url,
        position: Position,
    ) -> Option<CompletionResponse> {
        trace!("Starting completion ");
        let lock = self.documents.lock().await;
        let pair = lock.get(url)?;

        let latest_doc_info = &pair.info;
        info!(
            "Using document version:{:?} for completion ",
            latest_doc_info.version
        );

        let completions = pair
            .last_good_document
            .completion_items(position, latest_doc_info)?;

        Some(CompletionResponse::Array(completions))
    }

    pub async fn code_actions(&self, url: &Url, range: Range) -> Option<CodeActionResponse> {
        let document = self.latest_document_by_url(url).await?;

        let mut responses = vec![];
        if let Some(edit) = document.annotate(range) {
            responses.push(CodeActionOrCommand::CodeAction(edit));
        }
        Some(responses)
    }
}
