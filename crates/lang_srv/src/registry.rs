use std::{
    cell::OnceCell,
    collections::HashMap,
    future::Future,
    io::{stderr, Write},
    ops::Deref,
    rc::Rc,
    sync::Arc,
};

use tokio::{
    io::AsyncRead,
    sync::{Mutex, MutexGuard, RwLock, RwLockReadGuard, RwLockWriteGuard},
    task::JoinHandle,
};
use tower_lsp::lsp_types::{
    notification::Notification, CompletionResponse, Diagnostic, GotoDefinitionResponse, Hover,
    Position, SemanticTokensResult, TextEdit, Url,
};

use crate::analysis::{global_anal, AnalysisResult, AnalyzedDocument, DocInfo};

pub(crate) enum DocumentChange {
    Modified(Url, String, u32),
    Closed(Url),
}
#[derive(Debug)]
pub(crate) struct LatestDocument {
    info: DocInfo,
    //We can hold this mutex locked during updating while the latest and doc_info are out of sync
    //the lock should be aquired and immediately freed by and task looking to get a copy of info
    //At the top level we will need to store our lock
    latest: tokio::sync::watch::Receiver<Option<Arc<AnalyzedDocument>>>,
    latest_sender: tokio::sync::watch::Sender<Option<Arc<AnalyzedDocument>>>,
}
impl LatestDocument {
    pub(crate) async fn get_latest(&self) -> Arc<AnalyzedDocument> {
        let mut my_reciever = self.latest.clone();

        let a = my_reciever.wait_for(|x| x.is_some()).await.unwrap();
        match a.as_ref() {
            Some(latest) => latest.clone(),
            None => todo!(),
        }
    }
    pub(crate) fn set_latest(&self, latest: Arc<AnalyzedDocument>) {
        self.latest_sender.send(Some(latest)).unwrap()
    }
    pub(crate) fn waiting_for_doc(&self) -> bool {
        self.latest.borrow().is_none()
    }
    pub(crate) fn new(doc_info: DocInfo) -> LatestDocument {
        let chan = tokio::sync::watch::channel(None);
        LatestDocument {
            info: doc_info,
            latest_sender: chan.0,
            latest: chan.1,
        }
    }
    pub(crate) fn new_initialised(doc: Arc<AnalyzedDocument>) -> LatestDocument {
        let info = doc.doc_info.clone();
        let chan = tokio::sync::watch::channel(Some(doc));
        LatestDocument {
            info,
            latest_sender: chan.0,
            latest: chan.1,
        }
    }
}

#[derive(Debug)]
pub(crate) struct DocumentPair {
    latest_document: LatestDocument,
    last_good_document: Arc<AnalyzedDocument>,
}

// pub(crate) fn new(latest: AnalyzedDocument, last_good: AnalyzedDocument) -> DocumentPair {
//     DocumentPair {
//         //TODO not sure if i should actually be cloning here?
//         latest_document: (latest.doc_info, Arc::new(RwLock::new(last_good.clone()))),
//         last_good_document: last_good,
//     }
// }
// pub(crate) fn new_latest_type_checked(latest_doc: AnalyzedDocument) -> DocumentPair {
//     DocumentPair {
//         //TODO not sure if i should actually be cloning here?
//         latest_document: (
//             latest_doc.doc_info.clone(),
//             Arc::new(RwLock::new(latest_doc.clone())),
//         ),
//         last_good_document: latest_doc,
//     }
// }

#[derive(Debug, Default)]
pub(crate) struct Registry {
    documents: Mutex<HashMap<Url, DocumentPair>>,
}

impl Registry {
    fn update_document<'a>(
        documents: &mut MutexGuard<'a, HashMap<Url, DocumentPair>>,
        document: AnalyzedDocument,
    ) {
        let url = document.url().clone();
        let document = Arc::new(document);
        // writeln!(
        //     std::io::stderr(),
        //     "updating doc{:?}. version:{:?}",
        //     &url,
        //     &document.doc_info.version
        // );

        let latest_doc = LatestDocument::new_initialised(document.clone());
        match documents.get_mut(&url) {
            Some(old_doc) => {
                //This is a special case where we know we should
                if old_doc.latest_document.waiting_for_doc() {
                    old_doc.latest_document.set_latest(document.clone());
                }
                if document.type_checked() {
                    *old_doc = DocumentPair {
                        latest_document: latest_doc,
                        last_good_document: document,
                    };
                } else {
                    //TODO this seems ugly but for now i'll let it slide. shoudl be immutable
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

    pub async fn apply_change(&self, analysed_docs: Vec<AnalyzedDocument>) -> () {
        writeln!(
            std::io::stderr(),
            "updated the latest document with docinfo"
        );

        let mut documents = self.documents.lock().await;
        writeln!(
            std::io::stderr(),
            "finised doc analasys updating docs {:?}",
            analysed_docs
                .iter()
                .map(|a| a.doc_info.url.to_string())
                .collect::<Vec<_>>()
        );

        for document in analysed_docs {
            Registry::update_document(&mut documents, document);
        }
    }

    pub async fn apply_doc_info_changes(&self, url: Url, partial: DocInfo) {
        let mut lock = self.documents.lock().await;
        let doc = lock.get_mut(&url);
        match doc {
            Some(a) => {
                writeln!(
                    std::io::stderr(),
                    "set the docInfo to version:{:?}",
                    partial.version
                );

                a.latest_document = LatestDocument::new(partial);
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
        writeln!(stderr(), "got document");
        let latest_doc_info = &pair.latest_document.info;
        writeln!(stderr(), "latest version:{:?} ", latest_doc_info.version);

        let symbol_prefix = latest_doc_info.get_prefix_at_position(position);

        //this strategy probably won't work for record fields
        let completions =
            pair.last_good_document
                .completion_items(position, &latest_doc_info, symbol_prefix)?;

        Some(CompletionResponse::Array(completions))
    }
}
