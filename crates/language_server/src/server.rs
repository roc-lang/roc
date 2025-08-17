use analysis::HIGHLIGHT_TOKENS_LEGEND;

use log::{debug, trace};
use registry::{Registry, RegistryConfig};
use std::future::Future;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::time::Duration;

use tower_lsp::jsonrpc::{self, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::analysis::{global_analysis, DocInfo};

mod analysis;
mod convert;
mod registry;

struct RocServer {
    pub state: RocServerState,
    client: Client,
}

struct RocServerConfig {
    pub debounce_ms: Duration,
}

impl Default for RocServerConfig {
    fn default() -> Self {
        Self {
            debounce_ms: Duration::from_millis(100),
        }
    }
}

///This exists so we can test most of RocLs without anything LSP related
struct RocServerState {
    registry: Registry,
    config: RocServerConfig,
}

impl std::panic::RefUnwindSafe for RocServer {}

fn read_env_num(name: &str) -> Option<u64> {
    std::env::var(name)
        .ok()
        .and_then(|a| str::parse::<u64>(&a).ok())
}

impl RocServer {
    pub fn new(client: Client) -> Self {
        let registry_config = RegistryConfig {
            latest_document_timeout: Duration::from_millis(
                read_env_num("ROCLS_LATEST_DOC_TIMEOUT_MS").unwrap_or(5000),
            ),
        };
        let config = RocServerConfig {
            debounce_ms: Duration::from_millis(read_env_num("ROCLS_DEBOUNCE_MS").unwrap_or(100)),
        };
        Self {
            state: RocServerState::new(config, Registry::new(registry_config)),
            client,
        }
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
            resolve_provider: Some(false),
            trigger_characters: Some(vec![".".to_string()]),
            all_commit_characters: None,
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
        };
        let code_action_provider = CodeActionProviderCapability::Simple(true);
        ServerCapabilities {
            text_document_sync: Some(text_document_sync),
            hover_provider: Some(hover_provider),
            definition_provider: Some(OneOf::Right(definition_provider)),
            document_formatting_provider: Some(OneOf::Right(document_formatting_provider)),
            semantic_tokens_provider: Some(semantic_tokens_provider),
            completion_provider: Some(completion_provider),
            code_action_provider: Some(code_action_provider),
            ..ServerCapabilities::default()
        }
    }

    /// Records a document content change.
    async fn change(&self, fi: Url, text: String, version: i32) {
        let updating_result = self.state.change(&fi, text, version).await;

        //The analysis task can be cancelled by another change coming in which will update the watched variable
        if let Err(e) = updating_result {
            debug!("Cancelled change. Reason:{:?}", e);
            return;
        }

        debug!("Applied_changes getting and returning diagnostics");

        let diagnostics = self.state.registry.diagnostics(&fi).await;

        self.client
            .publish_diagnostics(fi, diagnostics, Some(version))
            .await;
    }
}

impl RocServerState {
    pub fn new(config: RocServerConfig, registry: Registry) -> RocServerState {
        Self { config, registry }
    }

    async fn close(&self, _fi: Url) {}

    pub async fn change(
        &self,
        fi: &Url,
        text: String,
        version: i32,
    ) -> std::result::Result<(), String> {
        debug!("V{:?}:starting change", version);
        let doc_info = DocInfo::new(fi.clone(), text, version);

        self.registry
            .apply_doc_info_changes(fi.clone(), doc_info.clone())
            .await;

        debug!(
            "V{:?}:finished updating docinfo, starting analysis ",
            version
        );

        let inner_ref = self;
        let updating_result = async {
            //This reduces wasted computation by waiting to allow a new change to come in and update the version before we check, but does delay the final analysis. Ideally this would be replaced with cancelling the analysis when a new one comes in.
            tokio::time::sleep(self.config.debounce_ms).await;
            let is_latest = inner_ref
                .registry
                .get_latest_version(fi)
                .await
                .map(|latest| latest == version)
                .unwrap_or(true);
            if !is_latest {
                return Err("Not latest version skipping analysis".to_string());
            }

            let results = match tokio::time::timeout(
                Duration::from_secs(60),
                tokio::task::spawn_blocking(|| catch_unwind(|| global_analysis(doc_info))),
            )
            .await
            {
                Err(e) => {
                    return Err(format!(
                        "Document analysis thread timeout out after: {:?}",
                        e
                    ))
                }
                Ok(Err(e)) => {
                    return Err(format!("Document analysis thread failed. reason:{:?}", e))
                }
                Ok(Ok(res)) => {
                    res.map_err(|err| format!("Document analysis panicked with: {:?}", err))?
                }
            };
            let latest_version = inner_ref.registry.get_latest_version(fi).await;

            //if this version is not the latest another change must have come in and this analysis is useless
            //if there is no older version we can just proceed with the update
            if let Some(latest_version) = latest_version {
                if latest_version != version {
                    return Err(format!(
                        "Version {0} doesn't match latest: {1} discarding analysis",
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
impl LanguageServer for RocServer {
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
        let _res = unwind_async(self.change(uri, text, version)).await;
        if let Err(e) = _res {
            self.client.log_message(MessageType::ERROR, e.message).await
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let VersionedTextDocumentIdentifier { uri, version, .. } = params.text_document;

        // NOTE: We specify that we expect full-content syncs in the server capabilities,
        // so here we assume the only change passed is a change of the entire document's content.
        let TextDocumentContentChangeEvent { text, .. } = params
            .content_changes
            .into_iter()
            .last()
            .expect("textDocument change event had no changes ");

        let _res = unwind_async(self.change(uri, text, version)).await;
        if let Err(e) = _res {
            self.client.log_message(MessageType::ERROR, e.message).await
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let TextDocumentIdentifier { uri } = params.text_document;
        self.state.close(uri).await;
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

        unwind_async(self.state.registry.hover(&text_document.uri, position)).await
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

        unwind_async(
            self.state
                .registry
                .goto_definition(&text_document.uri, position),
        )
        .await
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let DocumentFormattingParams {
            text_document,
            options: _,
            work_done_progress_params: _,
        } = params;

        unwind_async(self.state.registry.formatting(&text_document.uri)).await
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

        unwind_async(self.state.registry.semantic_tokens(&text_document.uri)).await
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let doc = params.text_document_position;
        trace!("Got completion request.");

        unwind_async(
            self.state
                .registry
                .completion_items(&doc.text_document.uri, doc.position),
        )
        .await
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let CodeActionParams {
            text_document,
            range,
            context: _,
            partial_result_params: _,
            work_done_progress_params: _,
        } = params;

        unwind_async(self.state.registry.code_actions(&text_document.uri, range)).await
    }
}

async fn unwind_async<Fut, T>(future: Fut) -> tower_lsp::jsonrpc::Result<T>
where
    Fut: Future<Output = T>,
{
    let result = { futures::FutureExt::catch_unwind(AssertUnwindSafe(future)).await };

    match result {
        Ok(a) => tower_lsp::jsonrpc::Result::Ok(a),

        Err(err) => tower_lsp::jsonrpc::Result::Err(jsonrpc::Error {
            code: jsonrpc::ErrorCode::InternalError,
            message: format!("{:?}", err),
            data: None,
        }),
    }
}

fn main() {
    env_logger::Builder::from_env("ROCLS_LOG").init();

    // Tokio uses a smaller stack size for threads by default,
    // this can lead to stack overflows that don't show up with the roc release bin!
    let stack_size = 8 * 1024 * 1024; // 8MB

    // Build a custom Tokio runtime with configured thread stack size
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .thread_stack_size(stack_size)
        .enable_all()
        .build()
        .expect("Failed to create Tokio runtime");

    // Run the async main function on our custom runtime
    runtime.block_on(async_main());
}

async fn async_main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(RocServer::new);
    use roc_error_macros::set_panic_not_exit;
    set_panic_not_exit(true);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use std::sync::Once;

    use expect_test::expect;
    use indoc::indoc;
    use log::info;

    use super::*;

    fn completion_resp_to_strings(
        resp: CompletionResponse,
    ) -> Vec<(String, Option<Documentation>)> {
        match resp {
            CompletionResponse::Array(list) => list.into_iter(),
            CompletionResponse::List(list) => list.items.into_iter(),
        }
        .map(|item| (item.label, item.documentation))
        .collect::<Vec<_>>()
    }

    /// gets completion and returns only the label and docs for each completion
    async fn get_basic_completion_info(
        reg: &Registry,
        url: &Url,
        position: Position,
    ) -> Option<Vec<(String, Option<Documentation>)>> {
        reg.completion_items(url, position)
            .await
            .map(completion_resp_to_strings)
    }

    /// gets completion and returns only the label for each completion
    fn comp_labels(
        completions: Option<Vec<(String, Option<Documentation>)>>,
    ) -> Option<Vec<String>> {
        completions.map(|list| list.into_iter().map(|(labels, _)| labels).collect())
    }

    const DOC_LIT: &str = indoc! {r#"
        interface Test
          exposes []
          imports []
        "#};

    static INIT: Once = Once::new();

    async fn test_setup(doc: String) -> (RocServerState, Url) {
        INIT.call_once(|| {
            env_logger::builder()
                .is_test(true)
                .filter_level(log::LevelFilter::Debug)
                .init();
        });
        info!("Doc is:\n{0}", doc);
        let url = Url::parse("file:/Test.roc").unwrap();

        let inner = RocServerState::new(RocServerConfig::default(), Registry::default());
        // setup the file
        inner.change(&url, doc, 0).await.unwrap();
        (inner, url)
    }

    /// Runs a basic completion and returns the response
    async fn completion_test(
        initial: &str,
        addition: &str,
        position: Position,
    ) -> Option<Vec<(String, Option<Documentation>)>> {
        let doc = DOC_LIT.to_string() + initial;
        let (inner, url) = test_setup(doc.clone()).await;
        let registry = &inner.registry;

        let change = doc.clone() + addition;
        info!("doc is:\n{0}", change);

        inner.change(&url, change, 1).await.unwrap();

        get_basic_completion_info(registry, &url, position).await
    }

    async fn completion_test_labels(
        initial: &str,
        addition: &str,
        position: Position,
    ) -> Option<Vec<String>> {
        comp_labels(completion_test(initial, addition, position).await)
    }

    /// Test that completion works properly when we apply an "as" pattern to an identifier
    #[tokio::test]
    async fn test_completion_as_identifier() {
        let suffix = DOC_LIT.to_string()
            + indoc! {r#"
            main =
              when a is
                inn as outer ->
                  "#};

        let (inner, url) = test_setup(suffix.clone()).await;
        let position = Position::new(6, 7);
        let registry = &inner.registry;

        let change = suffix.clone() + "o";
        inner.change(&url, change, 1).await.unwrap();
        let comp1 = comp_labels(get_basic_completion_info(registry, &url, position).await);

        let c = suffix.clone() + "i";
        inner.change(&url, c, 2).await.unwrap();
        let comp2 = comp_labels(get_basic_completion_info(registry, &url, position).await);

        let actual = [comp1, comp2];

        expect![[r#"
            [
                Some(
                    [
                        "outer",
                    ],
                ),
                Some(
                    [
                        "inn",
                        "outer",
                    ],
                ),
            ]
        "#]]
        .assert_debug_eq(&actual)
    }

    /// Tests that completion works properly when we apply an "as" pattern to a record.
    #[tokio::test]
    async fn test_completion_as_record() {
        let doc = DOC_LIT.to_string()
            + indoc! {r#"
            main =
              when a is
                {one,two} as outer ->
                  "#};

        let (inner, url) = test_setup(doc.clone()).await;
        let position = Position::new(6, 7);
        let reg = &inner.registry;

        let change = doc.clone() + "o";
        inner.change(&url, change, 1).await.unwrap();
        let comp1 = comp_labels(get_basic_completion_info(reg, &url, position).await);

        let c = doc.clone() + "t";
        inner.change(&url, c, 2).await.unwrap();
        let comp2 = comp_labels(get_basic_completion_info(reg, &url, position).await);
        let actual = [comp1, comp2];

        expect![[r#"
            [
                Some(
                    [
                        "one",
                        "two",
                        "outer",
                    ],
                ),
                Some(
                    [
                        "one",
                        "two",
                        "outer",
                    ],
                ),
            ]
        "#]]
        .assert_debug_eq(&actual);
    }

    /// Test that completion works properly when we apply an "as" pattern to a record
    #[tokio::test]
    async fn test_completion_fun_params() {
        let actual = completion_test_labels(
            indoc! {r"
            main = \param1, param2 ->
              "},
            "par",
            Position::new(4, 3),
        )
        .await;

        expect![[r#"
            Some(
                [
                    "param1",
                    "param2",
                ],
            )
        "#]]
        .assert_debug_eq(&actual);
    }

    #[tokio::test]
    async fn test_completion_closure() {
        let actual = completion_test_labels(
            indoc! {r"
            main = [] |> List.map \ param1 , param2->
              "},
            "par",
            Position::new(4, 3),
        )
        .await;
        expect![[r#"
            Some(
                [
                    "param1",
                    "param2",
                ],
            )
        "#]]
        .assert_debug_eq(&actual);
    }

    #[tokio::test]
    async fn test_completion_with_docs() {
        let actual = completion_test(
            indoc! {r"
            ## This is the main function
            main = mai
              "},
            "par",
            Position::new(4, 10),
        )
        .await;

        expect![[r#"
            Some(
                [
                    (
                        "main",
                        Some(
                            MarkupContent(
                                MarkupContent {
                                    kind: Markdown,
                                    value: "This is the main function",
                                },
                            ),
                        ),
                    ),
                ],
            )
        "#]]
        .assert_debug_eq(&actual);
    }

    #[tokio::test]
    async fn test_completion_on_utf8() {
        let actual = completion_test(
            indoc! {r"
            main =
              "},
            "ç",
            Position::new(4, 3),
        )
        .await;

        expect![[r#"
            Some(
                [
                    (
                        "main",
                        None,
                    ),
                ],
            )
        "#]]
        .assert_debug_eq(&actual);
    }

    async fn code_action_edits(doc: String, position: Position, name: &str) -> Vec<TextEdit> {
        let (inner, url) = test_setup(doc.clone()).await;
        let registry = &inner.registry;

        let actions = registry
            .code_actions(&url, Range::new(position, position))
            .await
            .unwrap();

        actions
            .into_iter()
            .find_map(|either| match either {
                CodeActionOrCommand::CodeAction(action) if name == action.title => Some(action),
                _ => None,
            })
            .expect("Code action not present")
            .edit
            .expect("Code action does not have an associated edit")
            .changes
            .expect("Edit does not have any changes")
            .get(&url)
            .expect("Edit does not have changes for this file")
            .clone()
    }

    #[tokio::test]
    async fn test_annotate_single() {
        let edit = code_action_edits(
            DOC_LIT.to_string() + r#"main = "Hello, world!""#,
            Position::new(3, 2),
            "Add signature",
        )
        .await;

        expect![[r#"
            [
                TextEdit {
                    range: Range {
                        start: Position {
                            line: 3,
                            character: 0,
                        },
                        end: Position {
                            line: 3,
                            character: 0,
                        },
                    },
                    new_text: "main : Str\n",
                },
            ]
        "#]]
        .assert_debug_eq(&edit);
    }

    #[tokio::test]
    async fn test_annotate_top_level() {
        let edit = code_action_edits(
            DOC_LIT.to_string()
                + indoc! {r#"
                other = \_ ->
                    "Something else?"

                main =
                    other {}
            "#},
            Position::new(5, 0),
            "Add top-level signatures",
        )
        .await;

        expect![[r#"
            [
                TextEdit {
                    range: Range {
                        start: Position {
                            line: 3,
                            character: 0,
                        },
                        end: Position {
                            line: 3,
                            character: 0,
                        },
                    },
                    new_text: "other : * -> Str\n",
                },
                TextEdit {
                    range: Range {
                        start: Position {
                            line: 6,
                            character: 0,
                        },
                        end: Position {
                            line: 6,
                            character: 0,
                        },
                    },
                    new_text: "main : Str\n",
                },
            ]
        "#]]
        .assert_debug_eq(&edit);
    }

    #[tokio::test]
    async fn test_annotate_inner() {
        let edit = code_action_edits(
            DOC_LIT.to_string()
                + indoc! {r#"
                main =
                    start = 10
                    fib start 0 1

                fib = \n, a, b ->
                    if n == 0 then
                        a
                    else
                        fib (n - 1) b (a + b)
            "#},
            Position::new(4, 8),
            "Add signature",
        )
        .await;

        expect![[r#"
            [
                TextEdit {
                    range: Range {
                        start: Position {
                            line: 4,
                            character: 0,
                        },
                        end: Position {
                            line: 4,
                            character: 0,
                        },
                    },
                    new_text: "    start : Num *\n",
                },
            ]
        "#]]
        .assert_debug_eq(&edit);
    }
}
