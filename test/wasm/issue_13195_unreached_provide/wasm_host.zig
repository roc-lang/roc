//! A wasm platform host whose only export reaches one of the platform's two
//! provides. The other provide calls a hosted function this host leaves to
//! the embedder, so the linked module must hold neither that provide nor an
//! import of the hosted function.

extern fn roc_used() callconv(.c) u64;

export fn run() callconv(.c) u64 {
    return roc_used();
}
