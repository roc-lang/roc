use std::{
    env, fs,
    path::{Path, PathBuf},
};

use tar::Archive;

use crate::https::{self, PackageMetadata, Problem};

const MAX_DOWNLOAD_BYTES: u64 = 32 * 1_000_000_000; // GB
const TARBALL_BUFFER_SIZE: usize = 16 * 1_000_000; // MB

/// Look in the given cache dir to see if we already have an entry for the given URL. If we do,
/// return its info. If we don't already have it, then:
///
/// - Download and decompress the compressed tarball from the given URL
/// - Verify its bytes against the hash in the URL
/// - Extract the tarball's contents into the appropriate cache directory
///
/// Returns the path to the installed package (which will be in the cache dir somewhere), as well
/// as the requested root module filename (optionally specified via the URL fragment).
pub fn install_package<'a>(
    roc_cache_dir: &Path,
    url: &'a str,
) -> Result<(PathBuf, Option<&'a str>), Problem> {
    let metadata = PackageMetadata::try_from(url).map_err(Problem::UrlProblem)?;
    let dest_dir = path_inside_cache(
        roc_cache_dir,
        metadata.cache_subfolder,
        metadata.content_hash,
    );

    // If it exists already, we assume it has the correct contents (it's a cache, after all!)
    // and don't download anything.
    if !dest_dir.exists() {
        // Download the tarball into memory and verify it. Early return if it fails verification.
        let tarball_bytes = {
            let mut buf = Vec::with_capacity(TARBALL_BUFFER_SIZE);

            https::download_and_verify(url, metadata.content_hash, &mut buf, MAX_DOWNLOAD_BYTES)?;

            buf
        };

        // Create the destination directory and unpack the tarball into it.
        fs::create_dir_all(&dest_dir).map_err(Problem::IoErr)?;
        Archive::new(tarball_bytes.as_slice())
            .unpack(&dest_dir)
            .map_err(Problem::IoErr)?;

        // The package's files are now in the cache. We're done!
    }

    Ok((dest_dir, metadata.root_module_filename))
}

fn path_inside_cache(roc_cache_dir: &Path, cache_subfolder: &str, content_hash: &str) -> PathBuf {
    roc_cache_dir.join(cache_subfolder).join(content_hash)
}

#[cfg(windows)]
// e.g. the "Roc" in %APPDATA%\\Roc
const ROC_CACHE_DIR_NAME: &str = "Roc";

#[cfg(unix)]
// e.g. the "roc" in ~/.cache/roc
const ROC_CACHE_DIR_NAME: &str = "roc";

/// This looks up environment variables, so it should ideally be called once and then cached!
///
/// Returns a path of the form cache_dir_path.join(ROC_CACHE_DIR_NAME) where cache_dir_path is:
/// - The XDG_CACHE_HOME environment varaible, if it's set.
/// - Otherwise, ~/.cache on UNIX and %APPDATA% on Windows.
///
/// ROC_CACHE_DIR_NAME is "roc" on UNIX and "Roc" on Windows.
///
/// So ~/.cache/roc will be typical on UNIX, and %APPDATA%\\Roc will be typical on Windows.
///
/// Returns None if XDG_CACHE_HOME is not set, and also we can't determine the home directory
/// (or if %APPDATA% is missing on Windows) on this system.
#[cfg(not(test))]
pub fn roc_cache_dir(roc_version: &str) -> Option<PathBuf> {
    // Respect XDG, if the system appears to be using it.
    // https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
    match env::var_os("XDG_CACHE_HOME") {
        Some(xdg_cache_home) => Some(
            Path::new(&xdg_cache_home)
                .join(ROC_CACHE_DIR_NAME)
                .join(roc_version),
        ),
        None => {
            #[cfg(windows)]
            {
                // e.g. %APPDATA%\\Roc
                Some(Path::new(&env::var_os("APPDATA")?).join(ROC_CACHE_DIR_NAME))
            }

            #[cfg(unix)]
            {
                // e.g. $HOME/.cache/roc
                Some(
                    Path::new(&env::var_os("HOME")?)
                        .join(".cache")
                        .join(ROC_CACHE_DIR_NAME),
                )
            }
        }
    }
}

#[cfg(test)]
pub fn roc_cache_dir(_: &str) -> Option<PathBuf> {
    // Always use a tempdir during tests. We never want to write to the user's
    // actual cache directory during tests!
    Some(tempfile::temp_dir().unwrap())
}
