use parking_lot::{Mutex, MutexGuard};
use registry::{DocumentChange, Registry};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod analysis;
mod convert;
mod registry;

#[derive(Debug)]
struct RocLs {
    client: Client,
    registry: Mutex<Registry>,
}

impl std::panic::RefUnwindSafe for RocLs {}

impl RocLs {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            registry: Mutex::new(Registry::default()),
        }
    }

    fn registry(&self) -> MutexGuard<Registry> {
        self.registry.lock()
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

        ServerCapabilities {
            text_document_sync: Some(text_document_sync),
            hover_provider: Some(hover_provider),
            definition_provider: Some(OneOf::Right(definition_provider)),
            ..ServerCapabilities::default()
        }
    }

    /// Records a document content change.
    async fn change(&self, fi: Url, text: String, version: i32) {
        self.registry()
            .apply_change(DocumentChange::Modified(fi.clone(), text));

        let diagnostics = match std::panic::catch_unwind(|| self.registry().diagnostics(&fi)) {
            Ok(ds) => ds,
            Err(_) => return,
        };

        self.client
            .publish_diagnostics(fi, diagnostics, Some(version))
            .await;
    }

    async fn close(&self, fi: Url) {
        self.registry().apply_change(DocumentChange::Closed(fi));
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
        self.client
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
        self.close(uri).await;
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

        panic_wrapper(|| self.registry().hover(&text_document.uri, position))
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

        panic_wrapper(|| {
            self.registry()
                .goto_definition(&text_document.uri, position)
        })
    }
}

fn panic_wrapper<T>(f: impl FnOnce() -> Option<T> + std::panic::UnwindSafe) -> Result<Option<T>> {
    match std::panic::catch_unwind(f) {
        Ok(r) => Ok(r),
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
