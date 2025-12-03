# Roc LSP
The Roc compiler can now expose an experimental Language Server Protocol (LSP) endpoint
written in Zig as part of the Rust to Zig rewrite.

## Current state
The experimental LSP currently only holds the scaffolding for the incoming implementation.
It doesn't provide any features yet, but it does connect to your editor, detect file change
and store the buffer in memory. 
The following request have been handled :
- `initialize`
- `shutdown`
The following notifications have been handled :
- `initialized`
- `exit`
- `didOpen` (stores the buffer into a `StringHashMap`, but doesn't do any action on it)
- `didChange` (same as `didOpen`, but also supports incremental changes)


## How to implement new LSP capabilities
The core functionalities of the LSP have been implemented in a way so that `transport.zig` and
`protocol.zig` shouldn't have to be modified as more capabilities are added. When handling a new
LSP method, like `textDocument/completion` for example, the handler should be added in the `handlers`
directory and its call should be added either in `request` (if it expects a response) or `notification`
(if it doesn't expect a response). `textDocument/completion` for example would go here :
```zig
const request_handlers = std.StaticStringMap(HandlerPtr).initComptime(.{
    .{ "initialize", &InitializeHandler.call },
    .{ "shutdown", &ShutdownHandler.call },
    .{ "textDocument/completion", &CompletionHandler.call },
});
```
When adding a new capability, if the server is ready to support it, you need to add the capabilities to
the `capabilities.zig` file for the `initialize` response to tell the client the capabilities is available :
```zig
pub fn buildCapabilities() ServerCapabilities {
    return .{
        .textDocumentSync = .{
            .openClose = true,
            .change = @intFromEnum(ServerCapabilities.TextDocumentSyncKind.incremental),
        },
    };
}
```
Here we tell the client that `textDocumentSync` is available in accordance to the LSP specifications data
structure. The `Server` struct holds the state, meaning in has the knowledge of the project files, the
documentation, the type inference, the syntax, etc. Every handler has access to it. These points of knowledge
are ideally separated in different fields of the server. For example, the opened buffer and other desired files
are stored in a `DocumentStore` which is a struct containing a `StringHashMap`, accessible through the `Server`.

## Starting the server
Build the Roc toolchain and run:
```bash
roc experimental-lsp
```
Normally you would just let your editor handle this.

## Debugging
Because the LSP takes control of the standard input and output, an optional flag was added.
```bash
roc experimental-lsp --debug-transport
```

Passing the `--debug-transport` flag will create a log file in your OS tmp folder (`/tmp` on Unix
systems). A mirror of the raw JSON-RPC traffic will be appended to the log file. Watching the file 
will allow a user to see incoming and outgoing message between the server and the editor
```bash
tail -f /tmp/roc-lsp-debug.log 
---
[1763992681773] OUT (128 bytes)
{"jsonrpc":"2.0","id":1,"result":{"capabilities":{"positionEncoding":"utf-16"},"serverInfo":{"name":"roc-lsp","version":"0.1"}}}
---
[1763992681828] IN (52 bytes)
{"jsonrpc":"2.0","method":"initialized","params":{}}
---
```

Additional debug channels can be enabled with `--debug-build`, `--debug-syntax`, and `--debug-server`
which log build environment activity, syntax/type checking, and server lifecycle details respectively
to the same temporary log file.

## Editor examples

### Neovim (lua + nvim-lspconfig)
The plan is to eventually just add the lsp to the Mason package manager, but for the meantime a manual
setup is possible. Your config might vary, but it'll look something like this:
```lua
local lspconfig = require("lspconfig")
local configs = require("lspconfig.configs")

if not configs.roc_lsp then
  configs.roc_lsp = {
    default_config = {
      name = 'roc_lsp',
      cmd = { '/path/to/roc', 'experimental-lsp', '--debug-transport' },
      filetypes = { 'roc' },
      root_markers = { 'main.roc', 'app.roc' },
      single_file_support = true,
    },
  }
end

lspconfig.roc_lsp.setup({
  capabilities = require("cmp_nvim_lsp").default_capabilities(),
})

-- If using Mason, you might have to remove the ensure_installed check
ensure_installed = vim.tbl_filter(function(name)
  return name ~= 'roc_lsp'
end, ensure_installed)
```

### VS Code

TODO

### Zed

TODO
