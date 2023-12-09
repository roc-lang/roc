use analysis::HIGHLIGHT_TOKENS_LEGEND;
use registry::{DocumentChange, Registry};
use std::future::Future;
use std::io::Write;
use std::sync::{Arc, OnceLock};
use std::time::Duration;
use tokio::sync::{oneshot, Mutex, MutexGuard, RwLock};
use tokio::task::{JoinError, JoinHandle};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::request::RegisterCapability;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::analysis::global_anal;

mod analysis;
mod convert;
mod registry;

#[derive(Debug)]
struct RocLs {
    pub inner: Arc<Inner>,
}
#[derive(Debug)]
struct Inner {
    client: Client,
    registry: Registry,
    change_handle: parking_lot::Mutex<(
        tokio::sync::watch::Sender<i32>,
        tokio::sync::watch::Receiver<i32>,
    )>,
    documents_updating: tokio::sync::Semaphore,
}

impl std::panic::RefUnwindSafe for RocLs {}
const SEMLIMIT: u32 = 20;

impl RocLs {
    pub fn new(client: Client) -> Self {
        Self {
            inner: Arc::new(Inner {
                client,
                registry: Registry::default(),
                change_handle: parking_lot::Mutex::new(tokio::sync::watch::channel(0)),
                ///Used for identifying if the intial stage of a document update is complete
                documents_updating: tokio::sync::Semaphore::new(SEMLIMIT as usize),
            }),
        }
    }
    ///Wait for all the semaphores associated with an in-progress document_info update to be released
    async fn wait_for_changes(&self) {
        self.inner.documents_updating.acquire_many(SEMLIMIT).await;
    }
    pub fn capabilities() -> ServerCapabilities {
        let text_document_sync = TextDocumentSyncCapability::Options(
            // TODO: later on make this incremental
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                ..TextDocumentSyncOptions::default()
            },
        );
        let hover_provider = HoverProviderCapability::Simple(true);
        let definition_provider = DefinitionOptions {
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
        };
        let document_formatting_provider = DocumentFormattingOptions {
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
        };
        let semantic_tokens_provider =
            SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
                legend: SemanticTokensLegend {
                    token_types: HIGHLIGHT_TOKENS_LEGEND.into(),
                    token_modifiers: vec![],
                },
                range: None,
                full: Some(SemanticTokensFullOptions::Bool(true)),
            });
        let completion_provider = CompletionOptions {
            resolve_provider: Some(true),
            trigger_characters: Some(vec![".".to_string()]),
            //TODO: what is this?
            all_commit_characters: None,
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
        };
        ServerCapabilities {
            text_document_sync: Some(text_document_sync),
            hover_provider: Some(hover_provider),
            definition_provider: Some(OneOf::Right(definition_provider)),
            document_formatting_provider: Some(OneOf::Right(document_formatting_provider)),
            semantic_tokens_provider: Some(semantic_tokens_provider),
            completion_provider: Some(completion_provider),
            ..ServerCapabilities::default()
        }
    }

    /// Records a document content change.
    async fn change(&self, fi: Url, text: String, version: i32) {
        writeln!(std::io::stderr(), "starting change");
        let updating_doc_info = self.inner.documents_updating.acquire().await.unwrap();
        let mut new_change = {
            let change_handle = self.inner.change_handle.lock();
            //This will cancel any other onging changes in favour of this one
            change_handle
                .0
                .send(version)
                .expect("change_handle disposed, this shouldn't happen");
            let mut watched = change_handle.1.clone();
            drop(watched.borrow_and_update());
            watched
        };
        //We will wait just a tiny amount of time to catch any requests that come in at the exact same time
        tokio::time::sleep(Duration::from_millis(20)).await;
        if (new_change.has_changed().unwrap()) {
            writeln!(std::io::stderr(), "newer task started almost immediately");
            return;
        }
        writeln!(std::io::stderr(), "finished checking for cancellation");
        let (results, partial) = global_anal(fi.clone(), text, version as u32);

        self.inner
            .registry
            .apply_doc_info_changes(fi.clone(), partial)
            .await;
        drop(updating_doc_info);

        writeln!(std::io::stderr(), "finished applying");

        let inner_ref = self.inner.clone();
        let handle: JoinHandle<core::result::Result<&str, JoinError>> =
            tokio::task::spawn(async move {
                let results = tokio::task::spawn_blocking(results).await?;

                inner_ref.registry().apply_change(results).await;
                Ok("okay")
            });

        writeln!(std::io::stderr(), "waiting on analisys or cancel");

        //The analysis task can be cancelled by another change coming in which will update the watched variable
        let cancelled = tokio::select! {
            a=handle=>{
                match a{

                    Err(a)=>
                    {
                        writeln!(std::io::stderr(), "error in task{:?}",a);
                    true},
                    Ok(_)=>false

                }
                },
            _=new_change.changed()=>true
        };
        if cancelled {
            writeln!(std::io::stderr(), "cancelled change");
            return;
        }
        writeln!(std::io::stderr(), "applied_change getting diagnostics");

        //We do this to briefly yeild

        let diagnostics = self.inner.registry().diagnostics(&fi).await;
        writeln!(std::io::stderr(), "applied_change returning diagnostics");

        self.inner
            .client
            .publish_diagnostics(fi, diagnostics, Some(version))
            .await;
    }
}

impl Inner {
    fn registry(&self) -> &Registry {
        &self.registry
    }
    fn registry_write(&mut self) -> &mut Registry {
        &mut self.registry
    }

    async fn close(&self, fi: Url) {
        // self.registry()
        //     .apply_change(DocumentChange::Closed(fi))
        //     .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for RocLs {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: Self::capabilities(),
            ..InitializeResult::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.inner
            .client
            .log_message(MessageType::INFO, "Roc language server initialized.")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let TextDocumentItem {
            uri, text, version, ..
        } = params.text_document;
        self.change(uri, text, version).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let VersionedTextDocumentIdentifier { uri, version, .. } = params.text_document;

        // NOTE: We specify that we expect full-content syncs in the server capabilities,
        // so here we assume the only change passed is a change of the entire document's content.
        let TextDocumentContentChangeEvent { text, .. } =
            params.content_changes.into_iter().next().unwrap();

        self.change(uri, text, version).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let TextDocumentIdentifier { uri } = params.text_document;
        self.inner.close(uri).await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let HoverParams {
            text_document_position_params:
                TextDocumentPositionParams {
                    text_document,
                    position,
                },
            work_done_progress_params: _,
        } = params;

        panic_wrapper_async(|| async {
            self.inner
                .registry()
                .hover(&text_document.uri, position)
                .await
        })
        .await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let GotoDefinitionParams {
            text_document_position_params:
                TextDocumentPositionParams {
                    text_document,
                    position,
                },
            work_done_progress_params: _,
            partial_result_params: _,
        } = params;

        panic_wrapper_async(|| async {
            self.inner
                .registry()
                .goto_definition(&text_document.uri, position)
                .await
        })
        .await
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let DocumentFormattingParams {
            text_document,
            options: _,
            work_done_progress_params: _,
        } = params;

        panic_wrapper_async(|| async { self.inner.registry().formatting(&text_document.uri) }).await
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let SemanticTokensParams {
            text_document,
            work_done_progress_params: _,
            partial_result_params: _,
        } = params;

        panic_wrapper_async(|| async { self.inner.registry().semantic_tokens(&text_document.uri) })
            .await
    }
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let doc = params.text_document_position;
        writeln!(std::io::stderr(), "starting completion");
        writeln!(
            std::io::stderr(),
            "permits::{:?} ",
            self.inner.documents_updating.available_permits()
        );

        //We need to wait untill any changes that were in progress when we requested completion have applied
        self.wait_for_changes().await;
        writeln!(std::io::stderr(), "waited for doc update to get sorted ");

        let res = panic_wrapper_async(|| async {
            self.inner
                .registry()
                .completion_items(&doc.text_document.uri, doc.position)
                .await
        })
        .await;

        writeln!(std::io::stderr(), "finished completion");
        res
    }
}

fn panic_wrapper<T>(f: impl FnOnce() -> Option<T> + std::panic::UnwindSafe) -> Result<Option<T>> {
    match std::panic::catch_unwind(f) {
        Ok(r) => Ok(r),
        Err(_) => Err(tower_lsp::jsonrpc::Error::internal_error()),
    }
}
async fn panic_wrapper_async<Fut, T>(
    f: impl FnOnce() -> Fut + std::panic::UnwindSafe,
) -> Result<Option<T>>
where
    Fut: Future<Output = Option<T>>,
{
    match std::panic::catch_unwind(f) {
        Ok(r) => Ok(r.await),
        Err(_) => Err(tower_lsp::jsonrpc::Error::internal_error()),
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(RocLs::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
