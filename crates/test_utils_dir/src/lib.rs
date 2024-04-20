/**
 * Creates a temporary empty directory that gets deleted when this goes out of scope.
 */
use std::path::PathBuf;
pub struct TmpDir {
    path: std::path::PathBuf,
}

impl TmpDir {
    pub fn new(dir: &str) -> TmpDir {
        let path = std::path::Path::new(dir);
        // ensure_empty_dir will fail if the dir doesn't already exist
        std::fs::create_dir_all(path).unwrap();
        remove_dir_all::ensure_empty_dir(path).unwrap();

        let mut pathbuf = std::path::PathBuf::new();
        pathbuf.push(path);
        TmpDir { path: pathbuf }
    }

    pub fn path(&self) -> &std::path::Path {
        self.path.as_path()
    }
}

impl Drop for TmpDir {
    fn drop(&mut self) {
        // we "discard" the Result because there is no problem when a dir was already removed before we call remove_dir_all
        let _ = remove_dir_all::remove_dir_all(&self.path);
    }
}

pub fn workspace_root() -> PathBuf {
    let root = std::env::var("ROC_WORKSPACE_DIR").expect("Can't find the ROC_WORKSPACE_DIR variable expected to be set in .cargo/config.toml. Are you running tests outside of cargo?");
    PathBuf::from(root)
}
