//! - Store global state
//! - The main loop
//! - Job/Request scheduling
//! - many Request handlers defined here. Except for the major ones which are in `src/features`

const Server = @This();

const std = @import("std");
const zig_builtin = @import("builtin");
const build_options = @import("build_options");
const Config = @import("Config.zig");
const configuration = @import("configuration.zig");
const DocumentStore = @import("DocumentStore.zig");
const lsp = @import("lsp");
const types = lsp.types;
const Analyser = @import("analysis.zig");
const offsets = @import("offsets.zig");
const tracy = @import("tracy");
const diff = @import("diff.zig");
const Uri = @import("Uri.zig");
const InternPool = @import("analyser/analyser.zig").InternPool;
const DiagnosticsCollection = @import("DiagnosticsCollection.zig");
const build_runner_shared = @import("build_runner/shared.zig");

const signature_help = @import("features/signature_help.zig");
const references = @import("features/references.zig");
const semantic_tokens = @import("features/semantic_tokens.zig");
const inlay_hints = @import("features/inlay_hints.zig");
const code_actions = @import("features/code_actions.zig");
const folding_range = @import("features/folding_range.zig");
const document_symbol = @import("features/document_symbol.zig");
const completions = @import("features/completions.zig");
const goto = @import("features/goto.zig");
const hover_handler = @import("features/hover.zig");
const selection_range = @import("features/selection_range.zig");
const diagnostics_gen = @import("features/diagnostics.zig");

const BuildOnSave = diagnostics_gen.BuildOnSave;
const BuildOnSaveSupport = build_runner_shared.BuildOnSaveSupport;

const log = std.log.scoped(.server);

// public fields
io: std.Io,
allocator: std.mem.Allocator,
config_manager: configuration.Manager,
document_store: DocumentStore,
transport: ?*lsp.Transport = null,
offset_encoding: offsets.Encoding = .@"utf-16",
status: Status = .uninitialized,

// private fields
thread_pool: std.Thread.Pool,
wait_group: std.Thread.WaitGroup = .{},
ip: InternPool = undefined,
/// avoid Zig deadlocking when spawning multiple `zig ast-check` processes at the same time.
/// See https://github.com/ziglang/zig/issues/16369
zig_ast_check_lock: std.Thread.Mutex = .{},
/// Stores messages that should be displayed with `window/showMessage` once the server has been initialized.
pending_show_messages: std.ArrayList(types.window.ShowMessageParams) = .empty,
client_capabilities: ClientCapabilities = .{},
diagnostics_collection: DiagnosticsCollection,
workspaces: std.ArrayList(Workspace) = .empty,

// Code was based off of https://github.com/andersfr/zig-lsp/blob/master/server.zig

const ClientCapabilities = struct {
    supports_snippets: bool = false,
    supports_apply_edits: bool = false,
    supports_will_save_wait_until: bool = false,
    supports_publish_diagnostics: bool = false,
    supports_code_action_fixall: bool = false,
    supports_semantic_tokens_overlapping: bool = false,
    hover_supports_md: bool = false,
    signature_help_supports_md: bool = false,
    completion_doc_supports_md: bool = false,
    supports_completion_insert_replace_support: bool = false,
    /// deprecated can be marked through the `CompletionItem.deprecated` field
    supports_completion_deprecated_old: bool = false,
    /// deprecated can be marked through the `CompletionItem.tags` field
    supports_completion_deprecated_tag: bool = false,
    label_details_support: bool = false,
    /// The client supports `workspace/configuration` requests.
    supports_configuration: bool = false,
    /// The client supports dynamically registering for the `workspace/didChangeConfiguration` notification.
    supports_workspace_did_change_configuration_dynamic_registration: bool = false,
    /// The client supports dynamically registering for the `workspace/didChangeWatchedFiles` notification.
    supports_workspace_did_change_watched_files: bool = false,
    supports_textDocument_definition_linkSupport: bool = false,
    /// The detail entries for big structs such as std.zig.CrossTarget were
    /// bricking the preview window in Sublime Text.
    /// https://github.com/zigtools/zls/pull/261
    max_detail_length: u32 = 1024 * 1024,
    client_name: ?[]const u8 = null,

    fn deinit(self: *ClientCapabilities, allocator: std.mem.Allocator) void {
        if (self.client_name) |name| allocator.free(name);
        self.* = undefined;
    }
};

pub const Error = error{
    OutOfMemory,
    ParseError,
    InvalidRequest,
    MethodNotFound,
    InvalidParams,
    InternalError,
    /// Error code indicating that a server received a notification or
    /// request before the server has received the `initialize` request.
    ServerNotInitialized,
    /// A request failed but it was syntactically correct, e.g the
    /// method name was known and the parameters were valid. The error
    /// message should contain human readable information about why
    /// the request failed.
    ///
    /// @since 3.17.0
    RequestFailed,
    /// The server cancelled the request. This error code should
    /// only be used for requests that explicitly support being
    /// server cancellable.
    ///
    /// @since 3.17.0
    ServerCancelled,
    /// The server detected that the content of a document got
    /// modified outside normal conditions. A server should
    /// NOT send this error code if it detects a content change
    /// in it unprocessed messages. The result even computed
    /// on an older state might still be useful for the client.
    ///
    /// If a client decides that a result is not of any use anymore
    /// the client should cancel the request.
    ContentModified,
    /// The client has canceled a request and a server as detected
    /// the cancel.
    RequestCancelled,
};

pub const Status = enum {
    /// the server has not received a `initialize` request
    uninitialized,
    /// the server has received a `initialize` request and is awaiting the `initialized` notification
    initializing,
    /// the server has been initialized and is ready to received requests
    initialized,
    /// the server has been shutdown and can't handle any more requests
    shutdown,
    /// the server is received a `exit` notification and has been shutdown
    exiting_success,
    /// the server is received a `exit` notification but has not been shutdown
    exiting_failure,
};

fn sendToClientResponse(server: *Server, id: lsp.JsonRPCMessage.ID, result: anytype) error{OutOfMemory}![]u8 {
    const tracy_zone = tracy.traceNamed(@src(), "sendToClientResponse(" ++ @typeName(@TypeOf(result)) ++ ")");
    defer tracy_zone.end();

    // TODO validate result type is a possible response
    // TODO validate response is from a client to server request
    // TODO validate result type

    const response: lsp.TypedJsonRPCResponse(@TypeOf(result)) = .{
        .id = id,
        .result_or_error = .{ .result = result },
    };
    return try sendToClientInternal(server.allocator, server.transport, response);
}

fn sendToClientRequest(server: *Server, id: lsp.JsonRPCMessage.ID, method: []const u8, params: anytype) error{OutOfMemory}![]u8 {
    const tracy_zone = tracy.traceNamed(@src(), "sendToClientRequest(" ++ @typeName(@TypeOf(params)) ++ ")");
    defer tracy_zone.end();

    // TODO validate method is a request
    // TODO validate method is server to client
    // TODO validate params type

    const request: lsp.TypedJsonRPCRequest(@TypeOf(params)) = .{
        .id = id,
        .method = method,
        .params = params,
    };
    return try sendToClientInternal(server.allocator, server.transport, request);
}

fn sendToClientNotification(server: *Server, method: []const u8, params: anytype) error{OutOfMemory}![]u8 {
    const tracy_zone = tracy.traceNamed(@src(), "sendToClientRequest(" ++ @typeName(@TypeOf(params)) ++ ")");
    defer tracy_zone.end();

    // TODO validate method is a notification
    // TODO validate method is server to client
    // TODO validate params type

    const notification: lsp.TypedJsonRPCNotification(@TypeOf(params)) = .{
        .method = method,
        .params = params,
    };
    return try sendToClientInternal(server.allocator, server.transport, notification);
}

fn sendToClientResponseError(server: *Server, id: lsp.JsonRPCMessage.ID, err: lsp.JsonRPCMessage.Response.Error) error{OutOfMemory}![]u8 {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const response: lsp.JsonRPCMessage = .{
        .response = .{ .id = id, .result_or_error = .{ .@"error" = err } },
    };

    return try sendToClientInternal(server.allocator, server.transport, response);
}

fn sendToClientInternal(allocator: std.mem.Allocator, transport: ?*lsp.Transport, message: anytype) error{OutOfMemory}![]u8 {
    const message_stringified = try std.json.Stringify.valueAlloc(allocator, message, .{
        .emit_null_optional_fields = false,
    });
    errdefer allocator.free(message_stringified);

    if (transport) |t| {
        const tracy_zone = tracy.traceNamed(@src(), "Transport.writeJsonMessage");
        defer tracy_zone.end();

        t.writeJsonMessage(message_stringified) catch |err| {
            log.err("failed to write message: {}", .{err});
        };
    }

    return message_stringified;
}

/// Send a `window/showMessage` notification to the client that will display a message in the user interface.
pub fn showMessage(
    server: *Server,
    message_type: types.window.MessageType,
    comptime fmt: []const u8,
    args: anytype,
) void {
    var message = std.fmt.allocPrint(server.allocator, fmt, args) catch return;
    defer server.allocator.free(message);
    switch (message_type) {
        .Error => log.err("{s}", .{message}),
        .Warning => log.warn("{s}", .{message}),
        .Info => log.info("{s}", .{message}),
        .Log, .Debug => log.debug("{s}", .{message}),
        _ => log.debug("{s}", .{message}),
    }
    switch (server.status) {
        .uninitialized => {
            server.pending_show_messages.ensureUnusedCapacity(server.allocator, 1) catch return;
            server.pending_show_messages.appendAssumeCapacity(.{
                .type = message_type,
                .message = message,
            });
            message = "";
            return;
        },
        .initializing,
        .initialized,
        => {},
        .shutdown,
        .exiting_success,
        .exiting_failure,
        => return,
    }
    if (server.sendToClientNotification("window/showMessage", types.window.ShowMessageParams{
        .type = message_type,
        .message = message,
    })) |json_message| {
        server.allocator.free(json_message);
    } else |err| {
        log.warn("failed to show message: {}", .{err});
    }
}

pub fn initAnalyser(server: *Server, arena: std.mem.Allocator, handle: ?*DocumentStore.Handle) Analyser {
    return .init(
        server.allocator,
        arena,
        &server.document_store,
        &server.ip,
        handle,
    );
}

/// If `force_autofix` is enabled, implement autofix without relying on a `source.fixall` code action.
pub fn autofixWorkaround(server: *Server) enum {
    /// Autofix is implemented using `textDocument/willSaveWaitUntil`.
    will_save_wait_until,
    /// Autofix is implemented by send a `workspace/applyEdit` request after receiving a `textDocument/didSave` notification.
    on_save,
    /// No workaround implementation of autofix is possible.
    unavailable,
    /// The `force_autofix` config option is disabled.
    none,
} {
    if (!server.config_manager.config.force_autofix) return .none;
    if (server.client_capabilities.supports_will_save_wait_until) return .will_save_wait_until;
    if (server.client_capabilities.supports_apply_edits) return .on_save;
    return .unavailable;
}

/// caller owns returned memory.
fn autofix(server: *Server, arena: std.mem.Allocator, handle: *DocumentStore.Handle) error{OutOfMemory}!std.ArrayList(types.TextEdit) {
    if (handle.tree.errors.len != 0) return .empty;
    if (handle.tree.mode == .zon) return .empty;

    var error_bundle = try diagnostics_gen.getAstCheckDiagnostics(server, handle);
    defer error_bundle.deinit(server.allocator);
    if (error_bundle.errorMessageCount() == 0) return .empty;

    var analyser = server.initAnalyser(arena, handle);
    defer analyser.deinit();

    var builder: code_actions.Builder = .{
        .arena = arena,
        .analyser = &analyser,
        .handle = handle,
        .offset_encoding = server.offset_encoding,
        .only_kinds = .init(.{
            .@"source.fixAll" = true,
        }),
    };

    try builder.generateCodeAction(error_bundle);
    for (builder.actions.items) |action| {
        std.debug.assert(action.kind.?.eql(.@"source.fixAll")); // We request only source.fixall code actions
    }

    defer builder.fixall_text_edits = .empty;
    return builder.fixall_text_edits;
}

fn generateDiagnostics(server: *Server, handle: *DocumentStore.Handle) void {
    if (!server.client_capabilities.supports_publish_diagnostics) return;
    const do = struct {
        fn do(param_server: *Server, param_handle: *DocumentStore.Handle) void {
            diagnostics_gen.generateDiagnostics(param_server, param_handle) catch |err| switch (err) {
                error.OutOfMemory => {},
            };
        }
    }.do;
    server.thread_pool.spawnWg(&server.wait_group, do, .{ server, handle });
}

fn initializeHandler(server: *Server, arena: std.mem.Allocator, request: types.InitializeParams) Error!types.InitializeResult {
    var support_full_semantic_tokens = true;

    if (request.clientInfo) |clientInfo| {
        server.client_capabilities.client_name = try server.allocator.dupe(u8, clientInfo.name);

        if (std.mem.startsWith(u8, clientInfo.name, "Visual Studio Code") or
            std.mem.startsWith(u8, clientInfo.name, "VSCodium") or
            std.mem.startsWith(u8, clientInfo.name, "Code - OSS"))
        {
            // VS Code doesn't really utilize `textDocument/semanticTokens/range`.
            // This will cause some visual artifacts when scrolling through the
            // document quickly but will considerably improve performance
            // especially on large files.
            support_full_semantic_tokens = false;
        } else if (std.mem.eql(u8, clientInfo.name, "Sublime Text LSP")) {
            server.client_capabilities.max_detail_length = 256;
        } else if (std.mem.startsWith(u8, clientInfo.name, "emacs")) {
            // Assumes that `emacs` means `emacs-lsp/lsp-mode`. Eglot uses `Eglot`.
        }
    }

    if (request.capabilities.general) |general| {
        if (general.positionEncodings) |position_encodings| {
            server.offset_encoding = outer: for (position_encodings) |encoding| {
                switch (encoding) {
                    .@"utf-8" => break :outer .@"utf-8",
                    .@"utf-16" => break :outer .@"utf-16",
                    .@"utf-32" => break :outer .@"utf-32",
                    .custom_value => {},
                }
            } else server.offset_encoding;
        }
    }
    server.diagnostics_collection.offset_encoding = server.offset_encoding;

    if (request.capabilities.textDocument) |textDocument| {
        server.client_capabilities.supports_publish_diagnostics = textDocument.publishDiagnostics != null;
        if (textDocument.hover) |hover| {
            if (hover.contentFormat) |content_format| {
                for (content_format) |format| {
                    if (format == .plaintext) {
                        break;
                    }
                    if (format == .markdown) {
                        server.client_capabilities.hover_supports_md = true;
                        break;
                    }
                }
            }
        }
        if (textDocument.completion) |completion| {
            if (completion.completionItem) |completionItem| {
                server.client_capabilities.label_details_support = completionItem.labelDetailsSupport orelse false;
                server.client_capabilities.supports_snippets = completionItem.snippetSupport orelse false;
                server.client_capabilities.supports_completion_deprecated_old = completionItem.deprecatedSupport orelse false;
                server.client_capabilities.supports_completion_insert_replace_support = completionItem.insertReplaceSupport orelse false;
                if (completionItem.tagSupport) |tagSupport| {
                    for (tagSupport.valueSet) |tag| {
                        switch (tag) {
                            .Deprecated => {
                                server.client_capabilities.supports_completion_deprecated_tag = true;
                                break;
                            },
                            _ => {},
                        }
                    }
                }
                if (completionItem.documentationFormat) |documentation_format| {
                    for (documentation_format) |format| {
                        if (format == .plaintext) {
                            break;
                        }
                        if (format == .markdown) {
                            server.client_capabilities.completion_doc_supports_md = true;
                            break;
                        }
                    }
                }
            }
        }
        if (textDocument.synchronization) |synchronization| {
            server.client_capabilities.supports_will_save_wait_until = synchronization.willSaveWaitUntil orelse false;
        }
        if (textDocument.definition) |definition| {
            server.client_capabilities.supports_textDocument_definition_linkSupport = definition.linkSupport orelse false;
        }
        if (textDocument.signatureHelp) |signature_help_capabilities| {
            if (signature_help_capabilities.signatureInformation) |signature_information| {
                if (signature_information.documentationFormat) |content_format| {
                    for (content_format) |format| {
                        if (format == .plaintext) {
                            break;
                        }
                        if (format == .markdown) {
                            server.client_capabilities.signature_help_supports_md = true;
                            break;
                        }
                    }
                }
            }
        }
        if (textDocument.semanticTokens) |semanticTokens| {
            server.client_capabilities.supports_semantic_tokens_overlapping = semanticTokens.overlappingTokenSupport orelse false;
        }
    }

    if (request.capabilities.window) |window| {
        if (window.workDoneProgress) |wdp| {
            server.document_store.lsp_capabilities.supports_work_done_progress = wdp;
        }
    }

    if (request.capabilities.workspace) |workspace| {
        server.client_capabilities.supports_apply_edits = workspace.applyEdit orelse false;
        server.client_capabilities.supports_configuration = workspace.configuration orelse false;
        if (workspace.didChangeConfiguration) |did_change| {
            if (did_change.dynamicRegistration orelse false) {
                server.client_capabilities.supports_workspace_did_change_configuration_dynamic_registration = true;
            }
        }
        if (workspace.didChangeWatchedFiles) |did_change| {
            if (did_change.dynamicRegistration orelse false) {
                server.client_capabilities.supports_workspace_did_change_watched_files = true;
            }
        }
        if (workspace.semanticTokens) |workspace_semantic_tokens| {
            server.document_store.lsp_capabilities.supports_semantic_tokens_refresh = workspace_semantic_tokens.refreshSupport orelse false;
        }
        if (workspace.inlayHint) |inlay_hint| {
            server.document_store.lsp_capabilities.supports_inlay_hints_refresh = inlay_hint.refreshSupport orelse false;
        }
    }

    if (request.clientInfo) |clientInfo| {
        log.info("Client Info:      {s} ({s})", .{ clientInfo.name, clientInfo.version orelse "unknown version" });
    }
    log.debug("Offset Encoding:  '{t}'", .{server.offset_encoding});

    if (request.workspaceFolders) |workspace_folders| {
        for (workspace_folders) |src| {
            const uri = Uri.parse(arena, src.uri) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => return error.InvalidParams,
            };
            try server.addWorkspace(uri);
        }
    }

    server.status = .initializing;

    {
        for (server.pending_show_messages.items) |params| {
            if (server.sendToClientNotification("window/showMessage", params)) |json_message| {
                server.allocator.free(json_message);
            } else |err| {
                log.warn("failed to show message: {}", .{err});
            }
        }
        for (server.pending_show_messages.items) |params| server.allocator.free(params.message);
        server.pending_show_messages.clearAndFree(server.allocator);
    }

    if (request.initializationOptions) |initialization_options| {
        if (std.json.parseFromValueLeaky(configuration.UnresolvedConfig, arena, initialization_options, .{
            .ignore_unknown_fields = true,
        })) |*new_cfg| {
            try server.config_manager.setConfiguration(.lsp_initialization, new_cfg);
            if (server.client_capabilities.supports_configuration) {
                // Do not resolve configuration until we received `workspace/configuration`.
            } else {
                try server.resolveConfiguration();
            }
        } else |err| {
            log.err("failed to read initialization_options: {}", .{err});
        }
    }

    return .{
        .serverInfo = .{
            .name = "zls",
            .version = build_options.version_string,
        },
        .capabilities = .{
            .positionEncoding = switch (server.offset_encoding) {
                .@"utf-8" => .@"utf-8",
                .@"utf-16" => .@"utf-16",
                .@"utf-32" => .@"utf-32",
            },
            .signatureHelpProvider = .{
                .triggerCharacters = &.{"("},
                .retriggerCharacters = &.{","},
            },
            .textDocumentSync = .{
                .text_document_sync_options = .{
                    .openClose = true,
                    .change = .Incremental,
                    .save = .{ .bool = true },
                    .willSaveWaitUntil = true,
                },
            },
            .renameProvider = .{
                .rename_options = .{ .prepareProvider = true },
            },
            .completionProvider = .{
                .resolveProvider = false,
                .triggerCharacters = &.{ ".", ":", "@", "]", "\"", "/" },
                .completionItem = .{ .labelDetailsSupport = true },
            },
            .documentHighlightProvider = .{ .bool = true },
            .hoverProvider = .{ .bool = true },
            .codeActionProvider = .{ .code_action_options = .{ .codeActionKinds = code_actions.supported_code_actions } },
            .declarationProvider = .{ .bool = true },
            .definitionProvider = .{ .bool = true },
            .typeDefinitionProvider = .{ .bool = true },
            .implementationProvider = .{ .bool = false },
            .referencesProvider = .{ .bool = true },
            .documentSymbolProvider = .{ .bool = true },
            .colorProvider = .{ .bool = false },
            .documentFormattingProvider = .{ .bool = true },
            .documentRangeFormattingProvider = .{ .bool = false },
            .foldingRangeProvider = .{ .bool = true },
            .selectionRangeProvider = .{ .bool = true },
            .workspaceSymbolProvider = .{ .bool = false },
            .workspace = .{
                .workspaceFolders = .{
                    .supported = true,
                    .changeNotifications = .{ .bool = true },
                },
            },
            .semanticTokensProvider = .{
                .semantic_tokens_options = .{
                    .full = .{ .bool = support_full_semantic_tokens },
                    .range = .{ .bool = true },
                    .legend = .{
                        .tokenTypes = std.meta.fieldNames(semantic_tokens.TokenType),
                        .tokenModifiers = std.meta.fieldNames(semantic_tokens.TokenModifiers),
                    },
                },
            },
            .inlayHintProvider = .{ .bool = true },
        },
    };
}

fn initializedHandler(server: *Server, arena: std.mem.Allocator, notification: types.InitializedParams) Error!void {
    _ = notification;

    if (server.status != .initializing) {
        log.warn("received a initialized notification but the server has not send a initialize request!", .{});
    }

    server.status = .initialized;

    if (server.client_capabilities.supports_configuration and
        server.client_capabilities.supports_workspace_did_change_configuration_dynamic_registration)
    {
        try server.registerCapability("workspace/didChangeConfiguration", null);
    }

    if (server.client_capabilities.supports_workspace_did_change_watched_files) {
        // `{ "watchers": [ { "globPattern": "**/*.{zig,zon}" } ] }`
        var watcher: std.json.ObjectMap = .init(arena);
        try watcher.putNoClobber("globPattern", .{ .string = "**/*.{zig,zon}" });
        var watchers_arr: std.json.Array = try .initCapacity(arena, 1);
        watchers_arr.appendAssumeCapacity(.{ .object = watcher });
        var fs_watcher_obj: std.json.ObjectMap = .init(arena);
        try fs_watcher_obj.putNoClobber("watchers", .{ .array = watchers_arr });
        const json_val: std.json.Value = .{ .object = fs_watcher_obj };

        try server.registerCapability("workspace/didChangeWatchedFiles", json_val);
    }

    if (server.client_capabilities.supports_configuration) {
        // We defer calling `server.resolveConfiguration()` until after workspace configuration has been received.
        try server.requestConfiguration();
    } else {
        // The client does not support the `workspace/configuration` (pull model) request
        // and it is unknown whether the client will use the
        // `workspace/didChangeConfiguration` (push model) notification instead.
        // In case they don't, we resolve configuration early and re-resolve if push model is used.
        try server.resolveConfiguration();
    }

    if (std.crypto.random.intRangeLessThan(usize, 0, 32768) == 0) {
        server.showMessage(.Warning, "HELP ME, I AM STUCK INSIDE AN LSP!", .{});
    }
}

fn shutdownHandler(server: *Server, _: std.mem.Allocator, _: void) Error!?void {
    defer server.status = .shutdown;
    if (server.status != .initialized) return error.InvalidRequest; // received a shutdown request but the server is not initialized!
}

fn exitHandler(server: *Server, _: std.mem.Allocator, _: void) Error!void {
    server.status = switch (server.status) {
        .initialized => .exiting_failure,
        .shutdown => .exiting_success,
        else => unreachable,
    };
}

fn registerCapability(server: *Server, method: []const u8, registersOptions: ?types.LSPAny) Error!void {
    const id = try std.fmt.allocPrint(server.allocator, "register-{s}", .{method});
    defer server.allocator.free(id);

    log.debug("Dynamically registering method '{s}'", .{method});

    const json_message = try server.sendToClientRequest(
        .{ .string = id },
        "client/registerCapability",
        types.Registration.Params{ .registrations = &.{
            .{
                .id = id,
                .method = method,
                .registerOptions = registersOptions,
            },
        } },
    );
    server.allocator.free(json_message);
}

/// Request configuration options with the `workspace/configuration` request.
fn requestConfiguration(server: *Server) Error!void {
    const configuration_items: [1]types.workspace.configuration.Item = .{
        .{
            .section = "zls",
            .scopeUri = if (server.workspaces.items.len == 1) server.workspaces.items[0].uri.raw else null,
        },
    };

    const json_message = try server.sendToClientRequest(
        .{ .string = "i_haz_configuration" },
        "workspace/configuration",
        types.workspace.configuration.Params{
            .items = &configuration_items,
        },
    );
    server.allocator.free(json_message);
}

/// Handle the response of the `workspace/configuration` request.
fn handleConfiguration(server: *Server, json: std.json.Value) error{OutOfMemory}!void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const result: std.json.Value = switch (json) {
        .array => |arr| blk: {
            if (arr.items.len != 1) {
                log.err("Response to 'workspace/configuration' expects an array of size 1 but received {d}", .{arr.items.len});
                break :blk null;
            }
            break :blk switch (arr.items[0]) {
                .object => arr.items[0],
                .null => null,
                else => {
                    log.err("Response to 'workspace/configuration' expects an array of objects but got an array of {t}.", .{json});
                    break :blk null;
                },
            };
        },
        else => blk: {
            log.err("Response to 'workspace/configuration' expects an array but received {t}", .{json});
            break :blk null;
        },
    } orelse {
        try server.resolveConfiguration();
        return;
    };

    var arena_allocator: std.heap.ArenaAllocator = .init(server.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    var new_config = std.json.parseFromValueLeaky(
        configuration.UnresolvedConfig,
        arena,
        result,
        .{ .ignore_unknown_fields = true },
    ) catch |err| {
        log.err("Failed to parse response from 'workspace/configuration': {}", .{err});
        try server.resolveConfiguration();
        return;
    };

    const maybe_root_dir: ?[]const u8 = dir: {
        if (server.workspaces.items.len != 1) break :dir null;
        const workspace = server.workspaces.items[0];
        break :dir workspace.uri.toFsPath(arena) catch |err| {
            log.err("failed to parse root uri for workspace {s}: {}", .{ workspace.uri.raw, err });
            break :dir null;
        };
    };

    inline for (configuration.file_system_config_options) |file_config| {
        const field: *?[]const u8 = &@field(new_config, file_config.name);
        if (field.*) |maybe_relative| resolve: {
            if (maybe_relative.len == 0) break :resolve;
            if (std.fs.path.isAbsolute(maybe_relative)) break :resolve;

            const root_dir = maybe_root_dir orelse {
                log.err("relative path only supported for {s} with exactly one workspace", .{file_config.name});
                break;
            };

            const absolute = try std.fs.path.resolve(arena, &.{
                root_dir, maybe_relative,
            });

            field.* = absolute;
        }
    }

    try server.config_manager.setConfiguration(.lsp_configuration, &new_config);
    try server.resolveConfiguration();
}

const Workspace = struct {
    uri: Uri,
    build_on_save: if (BuildOnSaveSupport.isSupportedComptime()) ?BuildOnSave else void,
    build_on_save_mode: if (BuildOnSaveSupport.isSupportedComptime()) ?enum { watch, manual } else void,

    fn init(server: *Server, uri: Uri) error{OutOfMemory}!Workspace {
        const duped_uri = try uri.dupe(server.allocator);
        errdefer duped_uri.deinit(server.allocator);

        return .{
            .uri = duped_uri,
            .build_on_save = if (BuildOnSaveSupport.isSupportedComptime()) null else {},
            .build_on_save_mode = if (BuildOnSaveSupport.isSupportedComptime()) null else {},
        };
    }

    fn deinit(workspace: *Workspace, allocator: std.mem.Allocator) void {
        if (BuildOnSaveSupport.isSupportedComptime()) {
            if (workspace.build_on_save) |*build_on_save| build_on_save.deinit();
        }
        workspace.uri.deinit(allocator);
    }

    fn sendManualWatchUpdate(workspace: *Workspace) void {
        comptime std.debug.assert(BuildOnSaveSupport.isSupportedComptime());

        const build_on_save = if (workspace.build_on_save) |*build_on_save| build_on_save else return;
        const mode = workspace.build_on_save_mode orelse return;
        if (mode != .manual) return;

        build_on_save.sendManualWatchUpdate();
    }

    fn refreshBuildOnSave(workspace: *Workspace, args: struct {
        server: *Server,
        /// Whether the build on save process should be restarted if it is already running.
        restart: bool,
    }) error{OutOfMemory}!void {
        comptime std.debug.assert(BuildOnSaveSupport.isSupportedComptime());

        const config = &args.server.config_manager.config;

        if (args.server.config_manager.zig_exe) |zig_exe| {
            workspace.build_on_save_mode = switch (BuildOnSaveSupport.isSupportedRuntime(zig_exe.version)) {
                .supported => .watch,
                // If if build on save has been explicitly enabled, fallback to the implementation with manual updates
                else => if (config.enable_build_on_save orelse false) .manual else null,
            };
        } else {
            workspace.build_on_save_mode = null;
        }

        const build_on_save_supported = workspace.build_on_save_mode != null;
        const build_on_save_wanted = config.enable_build_on_save orelse true;
        const enable = build_on_save_supported and build_on_save_wanted;

        if (workspace.build_on_save) |*build_on_save| {
            if (enable and !args.restart) return;
            log.debug("stopped Build-On-Save for '{s}'", .{workspace.uri.raw});
            build_on_save.deinit();
            workspace.build_on_save = null;
        }

        if (!enable) return;

        const zig_exe_path = config.zig_exe_path orelse return;
        const zig_lib_path = config.zig_lib_path orelse return;
        const build_runner_path = config.build_runner_path orelse return;

        const workspace_path = workspace.uri.toFsPath(args.server.allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.UnsupportedScheme => return,
        };
        defer args.server.allocator.free(workspace_path);

        std.debug.assert(workspace.build_on_save == null);
        workspace.build_on_save = BuildOnSave.init(.{
            .io = args.server.io,
            .allocator = args.server.allocator,
            .workspace_path = workspace_path,
            .build_on_save_args = config.build_on_save_args,
            .check_step_only = config.enable_build_on_save == null,
            .zig_exe_path = zig_exe_path,
            .zig_lib_path = zig_lib_path,
            .build_runner_path = build_runner_path,
            .collection = &args.server.diagnostics_collection,
        }) catch |err| {
            log.err("failed to initialize Build-On-Save for '{s}': {}", .{ workspace.uri.raw, err });
            return;
        };

        log.info("trying to start Build-On-Save for '{s}'", .{workspace.uri.raw});
    }
};

fn addWorkspace(server: *Server, uri: Uri) error{OutOfMemory}!void {
    try server.workspaces.ensureUnusedCapacity(server.allocator, 1);
    server.workspaces.appendAssumeCapacity(try Workspace.init(server, uri));
    log.info("added Workspace Folder: {s}", .{uri.raw});

    if (BuildOnSaveSupport.isSupportedComptime() and
        // Don't initialize build on save until initialization finished.
        // If the client supports the `workspace/configuration` request, wait
        // until we have received workspace configuration from the server.
        (server.status == .initialized and !server.client_capabilities.supports_configuration))
    {
        try server.workspaces.items[server.workspaces.items.len - 1].refreshBuildOnSave(.{
            .server = server,
            .restart = false,
        });
    }
}

fn removeWorkspace(server: *Server, uri: Uri) void {
    for (server.workspaces.items, 0..) |workspace, i| {
        if (workspace.uri.eql(uri)) {
            var removed_workspace = server.workspaces.swapRemove(i);
            removed_workspace.deinit(server.allocator);
            log.info("removed Workspace Folder: {s}", .{uri.raw});
            break;
        }
    } else {
        log.warn("could not remove Workspace Folder: {s}", .{uri.raw});
    }
}

fn didChangeWatchedFilesHandler(server: *Server, arena: std.mem.Allocator, notification: types.workspace.did_change_watched_files.Params) Error!void {
    var updated_files: usize = 0;
    for (notification.changes) |change| {
        const uri = Uri.parse(arena, change.uri) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.InvalidParams,
        };
        const file_extension = std.fs.path.extension(uri.raw);
        if (!std.mem.eql(u8, file_extension, ".zig") and !std.mem.eql(u8, file_extension, ".zon")) continue;

        switch (change.type) {
            .Created, .Changed, .Deleted => |kind| {
                const did_update_file = try server.document_store.refreshDocumentFromFileSystem(uri, kind == .Deleted);
                updated_files += @intFromBool(did_update_file);
            },
            else => {},
        }
    }
    if (updated_files != 0) {
        log.debug("updated {d} watched file(s)", .{updated_files});
    }
}

fn didChangeWorkspaceFoldersHandler(server: *Server, arena: std.mem.Allocator, notification: types.workspace.folders.DidChangeParams) Error!void {
    for (notification.event.added) |folder| {
        const uri = Uri.parse(arena, folder.uri) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.InvalidParams,
        };
        try server.addWorkspace(uri);
    }

    for (notification.event.removed) |folder| {
        const uri = Uri.parse(arena, folder.uri) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.InvalidParams,
        };
        server.removeWorkspace(uri);
    }
}

fn didChangeConfigurationHandler(server: *Server, arena: std.mem.Allocator, notification: types.workspace.configuration.did_change.Params) Error!void {
    const settings = switch (notification.settings) {
        .null => {
            if (server.client_capabilities.supports_configuration and
                server.client_capabilities.supports_workspace_did_change_configuration_dynamic_registration)
            {
                // The client has informed us that the configuration options have
                // changed. The will request them with `workspace/configuration`.
                try server.requestConfiguration();
            }
            return;
        },
        .object => |object| blk: {
            if (server.client_capabilities.supports_configuration and
                server.client_capabilities.supports_workspace_did_change_configuration_dynamic_registration)
            {
                log.debug("Ignoring 'workspace/didChangeConfiguration' notification in favor of 'workspace/configuration'", .{});
                try server.requestConfiguration();
                return;
            }
            break :blk object.get("zls") orelse notification.settings;
        },
        else => notification.settings, // We will definitely fail to parse this
    };

    const new_config = std.json.parseFromValueLeaky(
        configuration.UnresolvedConfig,
        arena,
        settings,
        .{ .ignore_unknown_fields = true },
    ) catch |err| {
        log.err("failed to parse 'workspace/didChangeConfiguration' response: {}", .{err});
        return error.ParseError;
    };

    try server.config_manager.setConfiguration(.lsp_configuration, &new_config);
    try server.resolveConfiguration();
}

pub fn resolveConfiguration(server: *Server) error{OutOfMemory}!void {
    var result = try server.config_manager.resolveConfiguration(server.allocator);
    defer result.deinit(server.allocator);

    for (result.messages) |msg| {
        server.showMessage(.Error, "{s}", .{msg});
    }

    inline for (std.meta.fields(Config)) |field| {
        if (@field(result.did_change, field.name)) {
            const new_value = @field(server.config_manager.config, field.name);
            log.info("Set config option '{s}' to {f}", .{ field.name, std.json.fmt(new_value, .{}) });
        }
    }

    const new_zig_exe_path: bool = result.did_change.zig_exe_path;
    const new_zig_lib_path: bool = result.did_change.zig_lib_path;
    const new_build_runner_path: bool = result.did_change.build_runner_path;
    const new_enable_build_on_save: bool = result.did_change.enable_build_on_save;
    const new_build_on_save_args: bool = result.did_change.build_on_save_args;
    const new_force_autofix: bool = result.did_change.force_autofix;

    server.document_store.config = createDocumentStoreConfig(&server.config_manager);

    if (BuildOnSaveSupport.isSupportedComptime() and
        // If the client supports the `workspace/configuration` request, defer
        // build on save initialization until after we have received workspace
        // configuration from the server
        (!server.client_capabilities.supports_configuration or server.status == .initialized))
    {
        const should_restart =
            new_zig_exe_path or
            new_zig_lib_path or
            new_build_runner_path or
            new_enable_build_on_save or
            new_build_on_save_args;

        for (server.workspaces.items) |*workspace| {
            try workspace.refreshBuildOnSave(.{
                .server = server,
                .restart = should_restart,
            });
        }
    }

    if (DocumentStore.supports_build_system) {
        if (new_zig_exe_path or new_zig_lib_path or new_build_runner_path) {
            for (server.document_store.build_files.keys()) |build_file_uri| {
                server.document_store.invalidateBuildFile(build_file_uri);
            }
        }

        if (new_zig_exe_path or new_zig_lib_path) {
            for (server.document_store.cimports.values()) |*cimport| {
                cimport.deinit(server.document_store.allocator);
            }
            server.document_store.cimports.clearAndFree(server.document_store.allocator);
        }
    }

    if (server.status == .initialized and
        (new_zig_exe_path or new_zig_lib_path) and
        server.client_capabilities.supports_publish_diagnostics)
    {
        for (server.document_store.handles.values()) |handle| {
            if (!handle.isLspSynced()) continue;
            server.generateDiagnostics(handle);
        }
    }

    // <---------------------------------------------------------->
    //  don't modify config options after here, only show messages
    // <---------------------------------------------------------->

    check: {
        if (!std.process.can_spawn) break :check;
        if (server.status != .initialized) break :check;

        // TODO there should a way to suppress this message
        if (server.config_manager.zig_exe == null) {
            server.showMessage(.Warning, "zig executable could not be found", .{});
        } else if (server.config_manager.zig_lib_dir == null) {
            server.showMessage(.Warning, "zig standard library directory could not be resolved", .{});
        }
    }

    check: {
        if (server.status != .initialized) break :check;

        switch (server.config_manager.build_runner_supported) {
            .yes, .no_dont_error => break :check,
            .no => {},
        }

        const zig_version = server.config_manager.zig_exe.?.version;
        const zls_version = build_options.version;

        const zig_version_is_tagged = zig_version.pre == null and zig_version.build == null;
        const zls_version_is_tagged = zls_version.pre == null and zls_version.build == null;

        if (zig_version_is_tagged) {
            server.showMessage(
                .Warning,
                "ZLS '{f}' does not support Zig '{f}'. A ZLS '{}.{}' release should be used instead.",
                .{ zls_version, zig_version, zig_version.major, zig_version.minor },
            );
        } else if (zls_version_is_tagged) {
            server.showMessage(
                .Warning,
                "ZLS '{f}' should be used with a Zig '{}.{}' release but found Zig '{f}'.",
                .{ zls_version, zls_version.major, zls_version.minor, zig_version },
            );
        } else {
            server.showMessage(
                .Warning,
                "ZLS '{f}' requires at least Zig '{s}' but got Zig '{f}'. Update Zig to avoid unexpected behavior.",
                .{ zls_version, build_options.minimum_runtime_zig_version_string, zig_version },
            );
        }
    }

    if (server.config_manager.config.enable_build_on_save orelse false) {
        if (!BuildOnSaveSupport.isSupportedComptime()) {
            // This message is not very helpful but it relatively uncommon to happen anyway.
            log.info("'enable_build_on_save' is ignored because build on save is not supported by this ZLS build", .{});
        } else if (server.status == .initialized and (server.config_manager.config.zig_exe_path == null or server.config_manager.zig_lib_dir == null)) {
            log.warn("'enable_build_on_save' is ignored because Zig could not be found", .{});
        } else if (!server.client_capabilities.supports_publish_diagnostics) {
            log.warn("'enable_build_on_save' is ignored because it is not supported by {s}", .{server.client_capabilities.client_name orelse "your editor"});
        } else if (server.status == .initialized and server.config_manager.build_runner_supported == .no and server.config_manager.config.build_runner_path == null) {
            log.warn("'enable_build_on_save' is ignored because no build runner is available", .{});
        } else if (server.status == .initialized and server.config_manager.zig_exe != null) {
            switch (BuildOnSaveSupport.isSupportedRuntime(server.config_manager.zig_exe.?.version)) {
                .supported => {},
                .invalid_linux_kernel_version => |*utsname_release| log.warn("Build-On-Save cannot run in watch mode because the Linux version '{s}' could not be parsed", .{std.mem.sliceTo(utsname_release, 0)}),
                .unsupported_linux_kernel_version => |kernel_version| log.warn("Build-On-Save cannot run in watch mode because it is not supported by Linux '{f}' (requires at least {f})", .{ kernel_version, BuildOnSaveSupport.minimum_linux_version }),
                .unsupported_zig_version => log.warn("Build-On-Save cannot run in watch mode because it is not supported on {t} by Zig {f} (requires at least {f})", .{ zig_builtin.os.tag, server.resolved_config.zig_runtime_version.?, BuildOnSaveSupport.minimum_zig_version }),
                .unsupported_os => log.warn("Build-On-Save cannot run in watch mode because it is not supported on {t}", .{zig_builtin.os.tag}),
            }
        }
    }

    if (new_force_autofix) {
        switch (server.autofixWorkaround()) {
            .none => {},
            .unavailable => {
                log.warn("`force_autofix` is ignored because it is not supported by {s}", .{server.client_capabilities.client_name orelse "your editor"});
            },
            .on_save, .will_save_wait_until => |workaround| {
                log.info("Autofix workaround enabled: '{t}'", .{workaround});
            },
        }
    }
}

fn createDocumentStoreConfig(config_manager: *const configuration.Manager) DocumentStore.Config {
    return .{
        .zig_exe_path = config_manager.config.zig_exe_path,
        .zig_lib_dir = config_manager.zig_lib_dir,
        .build_runner_path = config_manager.config.build_runner_path,
        .builtin_path = config_manager.config.builtin_path,
        .global_cache_dir = config_manager.global_cache_dir,
    };
}

fn openDocumentHandler(server: *Server, arena: std.mem.Allocator, notification: types.TextDocument.DidOpenParams) Error!void {
    if (notification.textDocument.text.len > DocumentStore.max_document_size) {
        log.err("open document '{s}' failed: text size ({d}) is above maximum length ({d})", .{
            notification.textDocument.uri,
            notification.textDocument.text.len,
            DocumentStore.max_document_size,
        });
        return error.InternalError;
    }

    const document_uri = Uri.parse(arena, notification.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    try server.document_store.openLspSyncedDocument(document_uri, notification.textDocument.text);
    server.generateDiagnostics(server.document_store.getHandle(document_uri).?);
}

fn changeDocumentHandler(server: *Server, arena: std.mem.Allocator, notification: types.TextDocument.DidChangeParams) Error!void {
    if (notification.contentChanges.len == 0) return;
    const document_uri = Uri.parse(arena, notification.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    const handle = server.document_store.getHandle(document_uri) orelse return;

    const new_text = try diff.applyContentChanges(server.allocator, handle.tree.source, notification.contentChanges, server.offset_encoding);

    if (new_text.len > DocumentStore.max_document_size) {
        log.err("change document '{s}' failed: text size ({d}) is above maximum length ({d})", .{
            document_uri.raw,
            new_text.len,
            DocumentStore.max_document_size,
        });
        server.allocator.free(new_text);
        return error.InternalError;
    }

    try server.document_store.refreshLspSyncedDocument(handle.uri, new_text);
    server.generateDiagnostics(handle);
}

fn saveDocumentHandler(server: *Server, arena: std.mem.Allocator, notification: types.TextDocument.DidSaveParams) Error!void {
    const document_uri = Uri.parse(arena, notification.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };

    if (std.process.can_spawn and DocumentStore.isBuildFile(document_uri)) {
        server.document_store.invalidateBuildFile(document_uri);
    }

    if (server.autofixWorkaround() == .on_save) {
        const handle = server.document_store.getHandle(document_uri) orelse return;
        var text_edits = try server.autofix(arena, handle);

        var workspace_edit: types.WorkspaceEdit = .{ .changes = .{} };
        try workspace_edit.changes.?.map.putNoClobber(arena, document_uri.raw, try text_edits.toOwnedSlice(arena));

        const json_message = try server.sendToClientRequest(
            .{ .string = "apply_edit" },
            "workspace/applyEdit",
            types.workspace.apply_workspace_edit.Params{
                .label = "autofix",
                .edit = workspace_edit,
            },
        );
        server.allocator.free(json_message);
    }

    if (BuildOnSaveSupport.isSupportedComptime()) {
        for (server.workspaces.items) |*workspace| {
            workspace.sendManualWatchUpdate();
        }
    }
}

fn closeDocumentHandler(server: *Server, arena: std.mem.Allocator, notification: types.TextDocument.DidCloseParams) Error!void {
    const document_uri = Uri.parse(arena, notification.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    server.document_store.closeLspSyncedDocument(document_uri);

    if (server.client_capabilities.supports_publish_diagnostics) {
        server.diagnostics_collection.clearSingleDocumentDiagnostics(document_uri);
        server.diagnostics_collection.publishDiagnostics() catch |err| {
            std.log.err("failed to publish diagnostics: {}", .{err});
        };
    }
}

fn willSaveWaitUntilHandler(server: *Server, arena: std.mem.Allocator, request: types.TextDocument.WillSaveParams) Error!?[]types.TextEdit {
    if (server.autofixWorkaround() != .will_save_wait_until) return null;

    switch (request.reason) {
        .Manual => {},
        .AfterDelay,
        .FocusOut,
        => return null,
        _ => return null,
    }

    const document_uri = Uri.parse(arena, request.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    const handle = server.document_store.getHandle(document_uri) orelse return null;

    var text_edits = try server.autofix(arena, handle);

    return try text_edits.toOwnedSlice(arena);
}

fn semanticTokensFullHandler(server: *Server, arena: std.mem.Allocator, request: types.semantic_tokens.Params) Error!?types.semantic_tokens.Result {
    if (server.config_manager.config.semantic_tokens == .none) return null;

    const document_uri = Uri.parse(arena, request.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    const handle = server.document_store.getHandle(document_uri) orelse return null;

    // Workaround: The Ast on .zon files is unusable when an error occurred on the root expr
    if (handle.tree.mode == .zon and handle.tree.errors.len > 0) return null;

    var analyser = server.initAnalyser(arena, handle);
    defer analyser.deinit();
    // semantic tokens can be quite expensive to compute on large files
    // and disabling callsite references can help with bringing the cost down.
    analyser.collect_callsite_references = false;

    return try semantic_tokens.writeSemanticTokens(
        arena,
        &analyser,
        handle,
        null,
        server.offset_encoding,
        server.config_manager.config.semantic_tokens == .partial,
        server.client_capabilities.supports_semantic_tokens_overlapping,
    );
}

fn semanticTokensRangeHandler(server: *Server, arena: std.mem.Allocator, request: types.semantic_tokens.Params.Range) Error!?types.semantic_tokens.Result {
    if (server.config_manager.config.semantic_tokens == .none) return null;

    const document_uri = Uri.parse(arena, request.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    const handle = server.document_store.getHandle(document_uri) orelse return null;

    // Workaround: The Ast on .zon files is unusable when an error occurred on the root expr
    if (handle.tree.mode == .zon and handle.tree.errors.len > 0) return null;

    const loc = offsets.rangeToLoc(handle.tree.source, request.range, server.offset_encoding);

    var analyser = server.initAnalyser(arena, handle);
    defer analyser.deinit();
    // semantic tokens can be quite expensive to compute on large files
    // and disabling callsite references can help with bringing the cost down.
    analyser.collect_callsite_references = false;

    return try semantic_tokens.writeSemanticTokens(
        arena,
        &analyser,
        handle,
        loc,
        server.offset_encoding,
        server.config_manager.config.semantic_tokens == .partial,
        server.client_capabilities.supports_semantic_tokens_overlapping,
    );
}

fn completionHandler(server: *Server, arena: std.mem.Allocator, request: types.completion.Params) Error!?types.completion.Result {
    const document_uri = Uri.parse(arena, request.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    const handle = server.document_store.getHandle(document_uri) orelse return null;
    if (handle.tree.mode == .zon) return null;

    const source_index = offsets.positionToIndex(handle.tree.source, request.position, server.offset_encoding);

    var analyser = server.initAnalyser(arena, handle);
    defer analyser.deinit();

    return .{
        .completion_list = try completions.completionAtIndex(server, &analyser, arena, handle, source_index) orelse return null,
    };
}

fn signatureHelpHandler(server: *Server, arena: std.mem.Allocator, request: types.SignatureHelp.Params) Error!?types.SignatureHelp {
    const document_uri = Uri.parse(arena, request.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    const handle = server.document_store.getHandle(document_uri) orelse return null;
    if (handle.tree.mode == .zon) return null;

    const source_index = offsets.positionToIndex(handle.tree.source, request.position, server.offset_encoding);

    const markup_kind: types.MarkupKind = if (server.client_capabilities.signature_help_supports_md) .markdown else .plaintext;

    var analyser = server.initAnalyser(arena, handle);
    defer analyser.deinit();

    const signature_info = (try signature_help.getSignatureInfo(
        &analyser,
        arena,
        handle,
        source_index,
        markup_kind,
    )) orelse return null;

    var signatures = try arena.alloc(types.SignatureHelp.Signature, 1);
    signatures[0] = signature_info;

    return .{
        .signatures = signatures,
        .activeSignature = 0,
        .activeParameter = signature_info.activeParameter,
    };
}

fn gotoDefinitionHandler(
    server: *Server,
    arena: std.mem.Allocator,
    request: types.Definition.Params,
) Error!?types.Definition.Result {
    return goto.gotoHandler(server, arena, .definition, request);
}

fn gotoTypeDefinitionHandler(server: *Server, arena: std.mem.Allocator, request: types.type_definition.Params) Error!?types.Definition.Result {
    return try goto.gotoHandler(server, arena, .type_definition, .{
        .textDocument = request.textDocument,
        .position = request.position,
        .workDoneToken = request.workDoneToken,
        .partialResultToken = request.partialResultToken,
    });
}

fn gotoImplementationHandler(server: *Server, arena: std.mem.Allocator, request: types.implementation.Params) Error!?types.Definition.Result {
    return try goto.gotoHandler(server, arena, .definition, .{
        .textDocument = request.textDocument,
        .position = request.position,
        .workDoneToken = request.workDoneToken,
        .partialResultToken = request.partialResultToken,
    });
}

fn gotoDeclarationHandler(server: *Server, arena: std.mem.Allocator, request: types.declaration.Params) Error!?types.Definition.Result {
    return try goto.gotoHandler(server, arena, .declaration, .{
        .textDocument = request.textDocument,
        .position = request.position,
        .workDoneToken = request.workDoneToken,
        .partialResultToken = request.partialResultToken,
    });
}

fn hoverHandler(server: *Server, arena: std.mem.Allocator, request: types.Hover.Params) Error!?types.Hover {
    const document_uri = Uri.parse(arena, request.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    const handle = server.document_store.getHandle(document_uri) orelse return null;
    if (handle.tree.mode == .zon) return null;
    const source_index = offsets.positionToIndex(handle.tree.source, request.position, server.offset_encoding);

    const markup_kind: types.MarkupKind = if (server.client_capabilities.hover_supports_md) .markdown else .plaintext;

    var analyser = server.initAnalyser(arena, handle);
    defer analyser.deinit();

    return hover_handler.hover(
        &analyser,
        arena,
        handle,
        source_index,
        markup_kind,
        server.offset_encoding,
    );
}

fn documentSymbolsHandler(server: *Server, arena: std.mem.Allocator, request: types.DocumentSymbol.Params) Error!lsp.ResultType("textDocument/documentSymbol") {
    const document_uri = Uri.parse(arena, request.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    const handle = server.document_store.getHandle(document_uri) orelse return null;
    if (handle.tree.mode == .zon) return null;
    return .{
        .document_symbols = try document_symbol.getDocumentSymbols(arena, &handle.tree, server.offset_encoding),
    };
}

fn formattingHandler(server: *Server, arena: std.mem.Allocator, request: types.document_formatting.Params) Error!?[]types.TextEdit {
    const document_uri = Uri.parse(arena, request.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    const handle = server.document_store.getHandle(document_uri) orelse return null;

    if (handle.tree.errors.len != 0) return null;

    const formatted = try handle.tree.renderAlloc(arena);

    if (std.mem.eql(u8, handle.tree.source, formatted)) return null;

    const text_edits = try diff.edits(arena, handle.tree.source, formatted, server.offset_encoding);
    return text_edits.items;
}

fn renameHandler(server: *Server, arena: std.mem.Allocator, request: types.rename.Params) Error!?types.WorkspaceEdit {
    const response = try references.referencesHandler(server, arena, .{ .rename = request });
    return if (response) |rep| rep.rename else null;
}

fn prepareRenameHandler(server: *Server, arena: std.mem.Allocator, request: types.prepare_rename.Params) Error!?types.prepare_rename.Result {
    const document_uri = Uri.parse(arena, request.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    const handle = server.document_store.getHandle(document_uri) orelse return null;
    const source_index = offsets.positionToIndex(handle.tree.source, request.position, server.offset_encoding);
    const name_loc = Analyser.identifierLocFromIndex(&handle.tree, source_index) orelse return null;
    const name = offsets.locToSlice(handle.tree.source, name_loc);
    return .{
        .prepare_rename_placeholder = .{
            .range = offsets.locToRange(handle.tree.source, name_loc, server.offset_encoding),
            .placeholder = name,
        },
    };
}

fn referencesHandler(server: *Server, arena: std.mem.Allocator, request: types.reference.Params) Error!?[]types.Location {
    const response = try references.referencesHandler(server, arena, .{ .references = request });
    return if (response) |rep| rep.references else null;
}

fn documentHighlightHandler(server: *Server, arena: std.mem.Allocator, request: types.DocumentHighlight.Params) Error!?[]types.DocumentHighlight {
    const response = try references.referencesHandler(server, arena, .{ .highlight = request });
    return if (response) |rep| rep.highlight else null;
}

fn inlayHintHandler(server: *Server, arena: std.mem.Allocator, request: types.InlayHint.Params) Error!?[]types.InlayHint {
    const document_uri = Uri.parse(arena, request.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    const handle = server.document_store.getHandle(document_uri) orelse return null;
    if (handle.tree.mode == .zon) return null;

    // The Language Server Specification does not provide a client capabilities that allows the client to specify the MarkupKind of inlay hints.
    const hover_kind: types.MarkupKind = if (server.client_capabilities.hover_supports_md) .markdown else .plaintext;
    const loc = offsets.rangeToLoc(handle.tree.source, request.range, server.offset_encoding);

    var analyser = server.initAnalyser(arena, handle);
    defer analyser.deinit();

    return try inlay_hints.writeRangeInlayHint(
        arena,
        &server.config_manager.config,
        &analyser,
        handle,
        loc,
        hover_kind,
        server.offset_encoding,
    );
}

fn codeActionHandler(server: *Server, arena: std.mem.Allocator, request: types.CodeAction.Params) Error!?[]const lsp.types.CodeAction.Result {
    const document_uri = Uri.parse(arena, request.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    const handle = server.document_store.getHandle(document_uri) orelse return null;

    // as of right now, only ast-check errors may get a code action
    if (handle.tree.errors.len != 0) return null;
    if (handle.tree.mode == .zon) return null;

    var error_bundle = try diagnostics_gen.getAstCheckDiagnostics(server, handle);
    defer error_bundle.deinit(server.allocator);

    var analyser = server.initAnalyser(arena, handle);
    defer analyser.deinit();

    const only_kinds = if (request.context.only) |kinds| blk: {
        var set: std.EnumSet(std.meta.Tag(types.CodeAction.Kind)) = .initEmpty();
        for (kinds) |kind| {
            set.setPresent(kind, true);
        }
        break :blk set;
    } else null;

    var builder: code_actions.Builder = .{
        .arena = arena,
        .analyser = &analyser,
        .handle = handle,
        .offset_encoding = server.offset_encoding,
        .only_kinds = only_kinds,
    };

    try builder.generateCodeAction(error_bundle);
    try builder.generateCodeActionsInRange(request.range);

    const result = try arena.alloc(types.CodeAction.Result, builder.actions.items.len);
    for (builder.actions.items, result) |action, *out| {
        out.* = .{ .code_action = action };
    }

    return result;
}

fn foldingRangeHandler(server: *Server, arena: std.mem.Allocator, request: types.FoldingRange.Params) Error!?[]types.FoldingRange {
    const document_uri = Uri.parse(arena, request.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    const handle = server.document_store.getHandle(document_uri) orelse return null;

    return try folding_range.generateFoldingRanges(arena, &handle.tree, server.offset_encoding);
}

fn selectionRangeHandler(server: *Server, arena: std.mem.Allocator, request: types.SelectionRange.Params) Error!?[]types.SelectionRange {
    const document_uri = Uri.parse(arena, request.textDocument.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.InvalidParams,
    };
    const handle = server.document_store.getHandle(document_uri) orelse return null;

    return try selection_range.generateSelectionRanges(arena, handle, request.positions, server.offset_encoding);
}

const HandledRequestParams = union(enum) {
    initialize: types.InitializeParams,
    shutdown,
    @"textDocument/willSaveWaitUntil": types.TextDocument.WillSaveParams,
    @"textDocument/semanticTokens/full": types.semantic_tokens.Params,
    @"textDocument/semanticTokens/range": types.semantic_tokens.Params.Range,
    @"textDocument/inlayHint": types.InlayHint.Params,
    @"textDocument/completion": types.completion.Params,
    @"textDocument/signatureHelp": types.SignatureHelp.Params,
    @"textDocument/definition": types.Definition.Params,
    @"textDocument/typeDefinition": types.type_definition.Params,
    @"textDocument/implementation": types.implementation.Params,
    @"textDocument/declaration": types.declaration.Params,
    @"textDocument/hover": types.Hover.Params,
    @"textDocument/documentSymbol": types.DocumentSymbol.Params,
    @"textDocument/formatting": types.document_formatting.Params,
    @"textDocument/rename": types.rename.Params,
    @"textDocument/prepareRename": types.prepare_rename.Params,
    @"textDocument/references": types.reference.Params,
    @"textDocument/documentHighlight": types.DocumentHighlight.Params,
    @"textDocument/codeAction": types.CodeAction.Params,
    @"textDocument/foldingRange": types.FoldingRange.Params,
    @"textDocument/selectionRange": types.SelectionRange.Params,
    other: lsp.MethodWithParams,
};

const HandledNotificationParams = union(enum) {
    initialized: types.InitializedParams,
    exit,
    @"textDocument/didOpen": types.TextDocument.DidOpenParams,
    @"textDocument/didChange": types.TextDocument.DidChangeParams,
    @"textDocument/didSave": types.TextDocument.DidSaveParams,
    @"textDocument/didClose": types.TextDocument.DidCloseParams,
    @"workspace/didChangeWatchedFiles": types.workspace.did_change_watched_files.Params,
    @"workspace/didChangeWorkspaceFolders": types.workspace.folders.DidChangeParams,
    @"workspace/didChangeConfiguration": types.workspace.configuration.did_change.Params,
    other: lsp.MethodWithParams,
};

const Message = lsp.Message(HandledRequestParams, HandledNotificationParams, .{});

fn isBlockingMessage(msg: Message) bool {
    switch (msg) {
        .request => |request| switch (request.params) {
            .initialize,
            .shutdown,
            => return true,
            .@"textDocument/willSaveWaitUntil",
            .@"textDocument/semanticTokens/full",
            .@"textDocument/semanticTokens/range",
            .@"textDocument/inlayHint",
            .@"textDocument/completion",
            .@"textDocument/signatureHelp",
            .@"textDocument/definition",
            .@"textDocument/typeDefinition",
            .@"textDocument/implementation",
            .@"textDocument/declaration",
            .@"textDocument/hover",
            .@"textDocument/documentSymbol",
            .@"textDocument/formatting",
            .@"textDocument/rename",
            .@"textDocument/prepareRename",
            .@"textDocument/references",
            .@"textDocument/documentHighlight",
            .@"textDocument/codeAction",
            .@"textDocument/foldingRange",
            .@"textDocument/selectionRange",
            => return false,
            .other => return false,
        },
        .notification => |notification| switch (notification.params) {
            .initialized,
            .exit,
            .@"textDocument/didOpen",
            .@"textDocument/didChange",
            .@"textDocument/didSave",
            .@"textDocument/didClose",
            .@"workspace/didChangeWatchedFiles",
            .@"workspace/didChangeWorkspaceFolders",
            .@"workspace/didChangeConfiguration",
            => return true,
            .other => return false,
        },
        .response => return true,
    }
}

pub const CreateOptions = struct {
    /// Must support `concurrent` unless the ZLS module is in single_threaded mode.
    io: std.Io,
    /// Must be thread-safe unless the ZLS module is in single_threaded mode.
    allocator: std.mem.Allocator,
    /// Must be set when running `loop`. Controls how the server will send and receive messages.
    transport: ?*lsp.Transport,
    /// The `global_cache_path` will not be resolve automatically.
    config: ?*const Config,
    config_manager: ?configuration.Manager = null,
    max_thread_count: usize = 4, // what is a good value here?
};

pub fn create(options: CreateOptions) (std.mem.Allocator.Error || std.Thread.SpawnError)!*Server {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const allocator = options.allocator;
    const io = options.io;

    const server = try allocator.create(Server);
    errdefer allocator.destroy(server);

    server.* = .{
        .io = io,
        .allocator = allocator,
        .config_manager = options.config_manager orelse .init(allocator),
        .document_store = .{
            .io = io,
            .allocator = allocator,
            .config = undefined, // set below
            .thread_pool = &server.thread_pool,
            .diagnostics_collection = &server.diagnostics_collection,
        },
        .thread_pool = undefined, // set below
        .diagnostics_collection = .{ .allocator = allocator },
    };
    server.document_store.config = createDocumentStoreConfig(&server.config_manager);

    try server.thread_pool.init(.{
        .allocator = allocator,
        .n_jobs = @min(4, std.Thread.getCpuCount() catch 1), // what is a good value here?
    });
    errdefer server.thread_pool.deinit();

    server.ip = try InternPool.init(allocator);
    errdefer server.ip.deinit(allocator);

    if (options.transport) |transport| {
        server.setTransport(transport);
    }
    if (options.config) |config| {
        try server.config_manager.setConfiguration2(.frontend, config);
    }

    return server;
}

pub fn destroy(server: *Server) void {
    server.thread_pool.deinit();
    server.document_store.deinit();
    server.ip.deinit(server.allocator);
    for (server.workspaces.items) |*workspace| workspace.deinit(server.allocator);
    server.workspaces.deinit(server.allocator);
    server.diagnostics_collection.deinit();
    server.client_capabilities.deinit(server.allocator);
    server.config_manager.deinit();
    for (server.pending_show_messages.items) |params| server.allocator.free(params.message);
    server.pending_show_messages.deinit(server.allocator);
    server.allocator.destroy(server);
}

pub fn setTransport(server: *Server, transport: *lsp.Transport) void {
    server.transport = transport;
    server.diagnostics_collection.transport = transport;
    server.document_store.transport = transport;
}

pub fn keepRunning(server: Server) bool {
    switch (server.status) {
        .exiting_success, .exiting_failure => return false,
        else => return true,
    }
}

/// The main loop of ZLS
pub fn loop(server: *Server) !void {
    std.debug.assert(server.transport != null);
    while (server.keepRunning()) {
        const json_message = try server.transport.?.readJsonMessage(server.allocator);
        defer server.allocator.free(json_message);

        var arena_allocator: std.heap.ArenaAllocator = .init(server.allocator);
        errdefer arena_allocator.deinit();

        const message = message: {
            const tracy_zone = tracy.traceNamed(@src(), "Message.parse");
            defer tracy_zone.end();
            break :message Message.parseFromSliceLeaky(
                arena_allocator.allocator(),
                json_message,
                .{ .ignore_unknown_fields = true, .max_value_len = null, .allocate = .alloc_always },
            ) catch return error.ParseError;
        };

        errdefer comptime unreachable;

        if (zig_builtin.single_threaded) {
            server.processMessageReportError(arena_allocator.state, message);
            continue;
        }

        if (isBlockingMessage(message)) {
            server.thread_pool.waitAndWork(&server.wait_group);
            server.wait_group.reset();
            server.processMessageReportError(arena_allocator.state, message);
        } else {
            server.thread_pool.spawnWg(&server.wait_group, processMessageReportError, .{ server, arena_allocator.state, message });
        }
    }
}

pub fn sendJsonMessageSync(server: *Server, json_message: []const u8) Error!?[]u8 {
    const parsed_message = Message.parseFromSlice(
        server.allocator,
        json_message,
        .{ .ignore_unknown_fields = true, .max_value_len = null, .allocate = .alloc_always },
    ) catch return error.ParseError;
    defer parsed_message.deinit();
    return try server.processMessage(parsed_message.arena.allocator(), parsed_message.value);
}

pub fn sendRequestSync(server: *Server, arena: std.mem.Allocator, comptime method: []const u8, params: lsp.ParamsType(method)) Error!lsp.ResultType(method) {
    comptime std.debug.assert(lsp.isRequestMethod(method));
    const tracy_zone = tracy.traceNamed(@src(), "sendRequestSync(" ++ method ++ ")");
    defer tracy_zone.end();
    tracy_zone.setName(method);

    const Params = std.meta.Tag(HandledRequestParams);
    if (!@hasField(Params, method)) return null;

    return switch (@field(Params, method)) {
        .initialize => try server.initializeHandler(arena, params),
        .shutdown => try server.shutdownHandler(arena, params),
        .@"textDocument/willSaveWaitUntil" => try server.willSaveWaitUntilHandler(arena, params),
        .@"textDocument/semanticTokens/full" => try server.semanticTokensFullHandler(arena, params),
        .@"textDocument/semanticTokens/range" => try server.semanticTokensRangeHandler(arena, params),
        .@"textDocument/inlayHint" => try server.inlayHintHandler(arena, params),
        .@"textDocument/completion" => try server.completionHandler(arena, params),
        .@"textDocument/signatureHelp" => try server.signatureHelpHandler(arena, params),
        .@"textDocument/definition" => try server.gotoDefinitionHandler(arena, params),
        .@"textDocument/typeDefinition" => try server.gotoTypeDefinitionHandler(arena, params),
        .@"textDocument/implementation" => try server.gotoImplementationHandler(arena, params),
        .@"textDocument/declaration" => try server.gotoDeclarationHandler(arena, params),
        .@"textDocument/hover" => try server.hoverHandler(arena, params),
        .@"textDocument/documentSymbol" => try server.documentSymbolsHandler(arena, params),
        .@"textDocument/formatting" => try server.formattingHandler(arena, params),
        .@"textDocument/rename" => try server.renameHandler(arena, params),
        .@"textDocument/prepareRename" => try server.prepareRenameHandler(arena, params),
        .@"textDocument/references" => try server.referencesHandler(arena, params),
        .@"textDocument/documentHighlight" => try server.documentHighlightHandler(arena, params),
        .@"textDocument/codeAction" => try server.codeActionHandler(arena, params),
        .@"textDocument/foldingRange" => try server.foldingRangeHandler(arena, params),
        .@"textDocument/selectionRange" => try server.selectionRangeHandler(arena, params),
        .other => return null,
    };
}

pub fn sendNotificationSync(server: *Server, arena: std.mem.Allocator, comptime method: []const u8, params: lsp.ParamsType(method)) Error!void {
    comptime std.debug.assert(lsp.isNotificationMethod(method));
    const tracy_zone = tracy.traceNamed(@src(), "sendNotificationSync(" ++ method ++ ")");
    defer tracy_zone.end();
    tracy_zone.setName(method);

    const Params = std.meta.Tag(HandledNotificationParams);
    if (!@hasField(Params, method)) return null;

    return switch (@field(Params, method)) {
        .initialized => try server.initializedHandler(arena, params),
        .exit => try server.exitHandler(arena, params),
        .@"textDocument/didOpen" => try server.openDocumentHandler(arena, params),
        .@"textDocument/didChange" => try server.changeDocumentHandler(arena, params),
        .@"textDocument/didSave" => try server.saveDocumentHandler(arena, params),
        .@"textDocument/didClose" => try server.closeDocumentHandler(arena, params),
        .@"workspace/didChangeWatchedFiles" => try server.didChangeWatchedFilesHandler(arena, params),
        .@"workspace/didChangeWorkspaceFolders" => try server.didChangeWorkspaceFoldersHandler(arena, params),
        .@"workspace/didChangeConfiguration" => try server.didChangeConfigurationHandler(arena, params),
        .other => {},
    };
}

pub fn sendMessageSync(server: *Server, arena: std.mem.Allocator, comptime method: []const u8, params: lsp.ParamsType(method)) Error!lsp.ResultType(method) {
    comptime std.debug.assert(lsp.isRequestMethod(method) or lsp.isNotificationMethod(method));

    if (comptime lsp.isRequestMethod(method)) {
        return try server.sendRequestSync(arena, method, params);
    } else if (comptime lsp.isNotificationMethod(method)) {
        return try server.sendNotificationSync(arena, method, params);
    } else unreachable;
}

fn processMessage(server: *Server, arena: std.mem.Allocator, message: Message) Error!?[]u8 {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    try server.validateMessage(message);

    switch (message) {
        .request => |request| switch (request.params) {
            .other => return try server.sendToClientResponse(request.id, @as(?void, null)),
            inline else => |params, method| {
                const result = try server.sendRequestSync(arena, @tagName(method), params);
                return try server.sendToClientResponse(request.id, result);
            },
        },
        .notification => |notification| switch (notification.params) {
            .other => {},
            inline else => |params, method| try server.sendNotificationSync(arena, @tagName(method), params),
        },
        .response => |response| try server.handleResponse(response),
    }
    return null;
}

fn processMessageReportError(server: *Server, arena_state: std.heap.ArenaAllocator.State, message: Message) void {
    var arena_allocator = arena_state.promote(server.allocator);
    defer arena_allocator.deinit();

    if (server.processMessage(arena_allocator.allocator(), message)) |json_message| {
        server.allocator.free(json_message orelse return);
    } else |err| {
        log.err("failed to process {f}: {}", .{ fmtMessage(message), err });
        if (@errorReturnTrace()) |trace| {
            std.debug.dumpStackTrace(trace);
        }

        switch (message) {
            .request => |request| {
                const json_message = server.sendToClientResponseError(request.id, .{
                    .code = @enumFromInt(switch (err) {
                        error.OutOfMemory => @intFromEnum(types.ErrorCodes.InternalError),
                        error.ParseError => @intFromEnum(types.ErrorCodes.ParseError),
                        error.InvalidRequest => @intFromEnum(types.ErrorCodes.InvalidRequest),
                        error.MethodNotFound => @intFromEnum(types.ErrorCodes.MethodNotFound),
                        error.InvalidParams => @intFromEnum(types.ErrorCodes.InvalidParams),
                        error.InternalError => @intFromEnum(types.ErrorCodes.InternalError),
                        error.ServerNotInitialized => @intFromEnum(types.ErrorCodes.ServerNotInitialized),
                        error.RequestFailed => @intFromEnum(types.LSPErrorCodes.RequestFailed),
                        error.ServerCancelled => @intFromEnum(types.LSPErrorCodes.ServerCancelled),
                        error.ContentModified => @intFromEnum(types.LSPErrorCodes.ContentModified),
                        error.RequestCancelled => @intFromEnum(types.LSPErrorCodes.RequestCancelled),
                    }),
                    .message = @errorName(err),
                }) catch return;
                server.allocator.free(json_message);
            },
            .notification, .response => return,
        }
    }
}

fn validateMessage(server: *const Server, message: Message) Error!void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const method = switch (message) {
        .request => |request| switch (request.params) {
            .other => |info| info.method,
            else => @tagName(request.params),
        },
        .notification => |notification| switch (notification.params) {
            .other => |info| info.method,
            else => @tagName(notification.params),
        },
        .response => return, // validation happens in `handleResponse`
    };

    // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#dollarRequests
    if (message == .request and std.mem.startsWith(u8, method, "$/")) return error.MethodNotFound;
    if (message == .notification and std.mem.startsWith(u8, method, "$/")) return;

    switch (server.status) {
        .uninitialized => blk: {
            if (std.mem.eql(u8, method, "initialize")) break :blk;
            if (std.mem.eql(u8, method, "exit")) break :blk;

            return error.ServerNotInitialized; // server received a request before being initialized!
        },
        .initializing => blk: {
            if (std.mem.eql(u8, method, "initialized")) break :blk;
            if (std.mem.eql(u8, method, "$/progress")) break :blk;

            return error.InvalidRequest; // server received a request during initialization!
        },
        .initialized => {},
        .shutdown => blk: {
            if (std.mem.eql(u8, method, "exit")) break :blk;

            return error.InvalidRequest; // server received a request after shutdown!
        },
        .exiting_success,
        .exiting_failure,
        => unreachable,
    }
}

fn handleResponse(server: *Server, response: lsp.JsonRPCMessage.Response) Error!void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    if (response.id == null) {
        log.warn("received response from client without id!", .{});
        return;
    }

    const id: []const u8 = switch (response.id.?) {
        .string => |id| id,
        .number => |id| {
            log.warn("received response from client with id '{d}' that has no handler!", .{id});
            return;
        },
    };

    const result = switch (response.result_or_error) {
        .result => |result| result,
        .@"error" => |err| {
            log.err("Error response for '{s}': {}, {s}", .{ id, err.code, err.message });
            if (std.mem.eql(u8, id, "i_haz_configuration")) {
                try server.resolveConfiguration();
            }
            return;
        },
    };

    if (std.mem.eql(u8, id, "semantic_tokens_refresh")) {
        //
    } else if (std.mem.eql(u8, id, "inlay_hints_refresh")) {
        //
    } else if (std.mem.eql(u8, id, "progress")) {
        //
    } else if (std.mem.startsWith(u8, id, "register")) {
        //
    } else if (std.mem.eql(u8, id, "apply_edit")) {
        //
    } else if (std.mem.eql(u8, id, "i_haz_configuration")) {
        try server.handleConfiguration(result orelse .null);
    } else {
        log.warn("received response from client with id '{s}' that has no handler!", .{id});
    }
}

fn formatMessage(message: Message, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    switch (message) {
        .request => |request| try writer.print("request-{f}-{t}", .{ std.json.fmt(request.id, .{}), request.params }),
        .notification => |notification| try writer.print("notification-{t}", .{notification.params}),
        .response => |response| try writer.print("response-{f}", .{std.json.fmt(response.id, .{})}),
    }
}

fn fmtMessage(message: Message) std.fmt.Alt(Message, formatMessage) {
    return .{ .data = message };
}
