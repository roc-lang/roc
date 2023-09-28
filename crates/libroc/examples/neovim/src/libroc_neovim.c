// NOTE: neovim can more efficiently access this via C, because
// Lua lets you load a dylib and then keep it open. In contrast,
// VimScript does one dlopen per C function call, which actually
// would be ok for perf because if you dlopen multiple times per
// process, the OS should cache basically everything and the subsequent
// ones should be very cheap (and the same with DLLs on Windows)...except
// vimscript's libcall() also immediately calls dlclose on it when it's
// done, thereby negating all of that and making it expensive again.
// Hence, neovim. In Vim, this can be done if Vim was compiled with
// Python extensions, which is nonstandard. (Neovim doesn't ship with
// Python extensions.) That can be someone else's separate project.
//
// Unfortunately, vim's APIs only provide a way to access
// buffers by line; there's no way to get a single contiguous
// array of bytes. However, what we can do is:
// - check to see if each line happens to end right before the next one's address
// - if so, then we can actually just point to the first one
// - if not, then we have to copy each of the lines individually into Src64
