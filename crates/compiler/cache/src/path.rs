/// A string used to represent paths outside the `roc` process, that can be UTF-8 (e.g. UNIX)
/// or UTF-16 (e.g. Windows, browsers, VS Code extensions).
pub struct Path {
    #[cfg(unix)]
    elems: [u8],

    #[cfg(windows)]
    elems: [u16],
}
