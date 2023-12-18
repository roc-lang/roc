use analysis::HIGHLIGHT_TOKENS_LEGEND;
use registry::Registry;
use std::future::Future;
use tokio::sync::RwLock;

use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::analysis::global_analysis;
use crate::registry::LatestDocument;

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
    registry: RwLock<Registry>,
}

impl std::panic::RefUnwindSafe for RocLs {}

impl RocLs {
    pub fn new(client: Client) -> Self {
        Self {
            inner: Arc::new(Inner {
                client,
                registry: RwLock::new(Registry::default()),
            }),
        }
    }
    ///Wait for all the semaphores associated with an in-progress document_info update to be released
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
        eprintln!("starting change");
        let registry_write_lock = self.inner.registry.write().await;

        eprintln!("finished checking for cancellation");
        let (results, partial) = global_analysis(fi.clone(), text, version);
        let partial_document = Arc::new(LatestDocument::new(partial.clone()));
        let partial_doc_write_lock = partial_document.get_lock();

        registry_write_lock
            .apply_doc_info_changes(fi.clone(), partial_document.clone())
            .await;
        //Now that we've got our new partial document written and we hold the exclusive write_handle to its analysis we can allow other tasks to access the registry and the doc_info inside this partial document
        drop(registry_write_lock);

        eprintln!("finished updating docinfo, starting analysis ",);

        let inner_ref = self.inner.clone();
        let updating_result = async {
            let results = match tokio::task::spawn_blocking(results).await {
                Err(e) => return Err(format!("document analysis failed. reason:{:?}", e)),
                Ok(a) => a,
            };
            let latest_version = inner_ref
                .registry
                .read()
                .await
                .get_latest_version(&fi)
                .await;

            //if this version is not the latest another change must have come in and this analysis is useless
            //if there is no older version we can just proceed with the update
            if let Some(latest_version) = latest_version {
                return Err(format!(
                    "version {0} doesn't match latest: {1} discarding analysis  ",
                    version, latest_version
                ));
            }

            inner_ref
                .registry
                .write()
                .await
                .apply_changes(results, partial_doc_write_lock, fi.clone())
                .await;
            Ok(())
        }
        .await;

        //The analysis task can be cancelled by another change coming in which will update the watched variable
        if let Err(e) = updating_result {
            eprintln!("cancelled change. Reason:{:?}", e);
            return;
        }
        eprintln!("applied_change getting and returning diagnostics");

        let diagnostics = self.inner.registry().await.diagnostics(&fi).await;

        self.inner
            .client
            .publish_diagnostics(fi, diagnostics, Some(version))
            .await;
    }
}

impl Inner {
    fn registry(&self) -> impl Future<Output = tokio::sync::RwLockReadGuard<Registry>> {
        self.registry.read()
    }

    async fn close(&self, _fi: Url) {
        ()
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
                .await
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
                .await
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

        panic_wrapper_async(|| async { self.inner.registry().await.formatting(&text_document.uri) })
            .await
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

        panic_wrapper_async(|| async {
            self.inner
                .registry()
                .await
                .semantic_tokens(&text_document.uri)
        })
        .await
    }
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let doc = params.text_document_position;
        eprintln!("starting completion");

        //We need to wait untill any changes that were in progress when we requested completion have applied
        eprintln!("waited for doc update to get sorted ");

        let res = panic_wrapper_async(|| async {
            self.inner
                .registry()
                .await
                .completion_items(&doc.text_document.uri, doc.position)
                .await
        })
        .await;

        eprintln!("finished completion");
        res
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
