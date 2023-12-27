use analysis::HIGHLIGHT_TOKENS_LEGEND;
use log::{debug, trace};
use registry::Registry;
use std::future::Future;

use std::time::Duration;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::analysis::global_analysis;

mod analysis;
mod convert;
mod registry;

#[derive(Debug)]
struct RocLs {
    pub inner: Inner,
    client: Client,
}

///This exists so we can test most of RocLs without anything LSP related
#[derive(Debug)]
struct Inner {
    registry: Registry,
}

impl std::panic::RefUnwindSafe for RocLs {}

impl RocLs {
    pub fn new(client: Client) -> Self {
        Self {
            inner: Inner::new(),
            client,
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
            resolve_provider: Some(false),
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
        let updating_result = self.inner.change(&fi, text, version).await;

        //The analysis task can be cancelled by another change coming in which will update the watched variable
        if let Err(e) = updating_result {
            debug!("cancelled change. Reason:{:?}", e);
            return;
        }
        debug!("applied_change getting and returning diagnostics");

        let diagnostics = self.inner.registry.diagnostics(&fi).await;

        self.client
            .publish_diagnostics(fi, diagnostics, Some(version))
            .await;
    }
}

impl Inner {
    pub fn new() -> Inner {
        Self {
            registry: Registry::default(),
        }
    }

    async fn registry(&self) -> &Registry {
        &self.registry
    }

    async fn close(&self, _fi: Url) {
        ()
    }

    pub async fn change(
        &self,
        fi: &Url,
        text: String,
        version: i32,
    ) -> std::result::Result<(), String> {
        debug!("V{:?}:starting change", version);
        //was write lock

        debug!("V{:?}:change acquired registry lock", version);
        let (results, partial) = global_analysis(fi.clone(), text, version);

        self.registry
            .apply_doc_info_changes(fi.clone(), partial.clone())
            .await;
        //Now that we've got our new partial document written and we hold the exclusive write_handle to its analysis we can allow other tasks to access the registry and the doc_info inside this partial document

        debug!(
            "V{:?}:finished updating docinfo, starting analysis ",
            version
        );

        let inner_ref = self;
        let updating_result = async {
            //This reduces wasted computation by waiting to see if a new change comes in, but does delay the final analysis. Ideally this would be replaced with cancelling the analysis when a new one comes in.
            tokio::time::sleep(Duration::from_millis(100)).await;
            let is_latest = inner_ref
                .registry
                .get_latest_version(fi)
                .await
                .map(|latest| latest == version)
                .unwrap_or(true);
            if !is_latest {
                return Err("Not latest version skipping analysis".to_string());
            }

            let results = match tokio::task::spawn_blocking(results).await {
                Err(e) => return Err(format!("Document analysis failed. reason:{:?}", e)),
                Ok(a) => a,
            };
            let latest_version = inner_ref.registry.get_latest_version(fi).await;

            //if this version is not the latest another change must have come in and this analysis is useless
            //if there is no older version we can just proceed with the update
            if let Some(latest_version) = latest_version {
                if latest_version != version {
                    return Err(format!(
                        "version {0} doesn't match latest: {1} discarding analysis  ",
                        version, latest_version
                    ));
                }
            }
            debug!(
                "V{:?}:finished document analysis applying changes ",
                version
            );

            inner_ref.registry.apply_changes(results, fi.clone()).await;
            Ok(())
        }
        .await;
        debug!("V{:?}:finished document change process", version);
        updating_result
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

        trace!("got did_change");
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
                .registry
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

        panic_wrapper_async(|| async {
            self.inner
                .registry()
                .await
                .formatting(&text_document.uri)
                .await
        })
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
                .await
        })
        .await
    }
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let doc = params.text_document_position;
        trace!("got completion request");
        let res = panic_wrapper_async(|| async {
            self.inner
                .registry
                .completion_items(&doc.text_document.uri, doc.position)
                .await
        })
        .await;
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

#[tokio::main(flavor = "multi_thread")]
async fn main() {
    env_logger::Builder::from_env("ROCLS_LOG").init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(RocLs::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use std::{
        sync::{Once, OnceLock},
        time::Duration,
    };

    use insta::assert_debug_snapshot;
    use tokio::{join, spawn};

    use super::*;
    const DOC_LIT: &str = r#"
app "fizz-buzz"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout,pf.Task.{ Task, await },]
    provides [main] to pf
"#;
    static INIT: Once = Once::new();
    async fn test_setup(doc: String) -> (Inner, Url) {
        INIT.call_once(|| {
            env_logger::builder()
                .is_test(true)
                .filter_level(log::LevelFilter::Trace)
                .init();
        });
        // static INNER_CELL: OnceLock<Inner> = OnceLock::new();
        // INNER_CELL.set(Inner::new()).unwrap();
        // static URL_CELL: OnceLock<Url> = OnceLock::new();
        // URL_CELL.set().unwrap()).unwrap();

        // let inner = INNER_CELL.get().unwrap();
        let url = Url::parse("file:/test.roc").unwrap();

        let inner = Inner::new();
        //setup the file
        inner.change(&url, doc, 0).await.unwrap();
        (inner, url)
    }

    #[tokio::test]
    async fn test_completion_with_changes() {
        let doc = DOC_LIT.to_string()
            + r#"rec=\a,b->{one:{potato:\d->d,leak:59},two:b}
rectest= 
  value= rec 1 2
  va"#;
        let (inner, url) = test_setup(doc.clone()).await;
        static INNER_CELL: OnceLock<Inner> = OnceLock::new();
        INNER_CELL.set(inner).unwrap();
        static URL_CELL: OnceLock<Url> = OnceLock::new();
        URL_CELL.set(url).unwrap();

        let inner = INNER_CELL.get().unwrap();
        let url = URL_CELL.get().unwrap();
        let position = Position::new(8, 8);
        //setup the file
        inner.change(&url, doc.clone(), 1).await.unwrap();

        //apply a sequence of changes back to back
        let a1 = spawn(inner.change(&url, doc.clone() + "l", 2));
        let a2 = spawn(inner.change(&url, doc.clone() + "lu", 3));
        let a3 = spawn(inner.change(&url, doc.clone() + "lue", 4));
        let a4 = spawn(inner.change(&url, doc.clone() + "lue.", 5));
        //start a completion that would only work if all changes have been applied
        let comp = spawn(async move {
            let url2 = url.clone();
            let reg = inner.registry().await;
            reg.completion_items(&url2, position).await
        });
        // Simulate two changes coming in with a slight delay
        let a = spawn(inner.change(&url, doc.clone() + "lue.o", 6));
        tokio::time::sleep(Duration::from_millis(100)).await;
        let rest = spawn(inner.change(&url, doc.clone() + "lue.on", 7));

        let done = join!(a1, a2, a3, a4, comp, a, rest);

        assert_debug_snapshot!(done)
    }
    #[tokio::test]
    async fn test_completion_as_identifier() {
        let suffix = DOC_LIT.to_string()
            + r#"
main =
  when a is
    inn as outer -> "#;
        let (inner, url) = test_setup(suffix.clone()).await;
        //test compltion for outer
        let position = Position::new(8, 21);

        let change = suffix.clone() + "o";
        inner.change(&url, change, 1).await.unwrap();
        let comp1 = inner
            .registry()
            .await
            .completion_items(&url, position)
            .await;

        let c = suffix.clone() + "i";
        inner.change(&url, c, 2).await.unwrap();
        let comp2 = inner
            .registry()
            .await
            .completion_items(&url, position)
            .await;

        let actual = [comp1, comp2];
        assert_debug_snapshot!(actual)
    }

    #[tokio::test]
    async fn test_completion_as_record() {
        let doc = DOC_LIT.to_string()
            + r#"
main =
  when a is
    {one,two} as outer -> "#;
        let (inner, url) = test_setup(doc.clone()).await;
        //test compltion for outer
        let position = Position::new(8, 27);

        let change = doc.clone() + "o";
        inner.change(&url, change, 1).await.unwrap();
        let comp1 = inner
            .registry()
            .await
            .completion_items(&url, position)
            .await;

        let c = doc.clone() + "t";
        inner.change(&url, c, 2).await.unwrap();
        let comp2 = inner
            .registry()
            .await
            .completion_items(&url, position)
            .await;

        let actual = [comp1, comp2];
        assert_debug_snapshot!(actual);
    }
}
