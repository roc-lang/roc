#[cfg(not(target_family = "wasm"))]
use crate::https::{self, PackageMetadata, Problem};
use roc_error_macros::internal_error;
use std::{
    fs,
    path::{Path, PathBuf},
};

const MAX_DOWNLOAD_BYTES: u64 = 32 * 1_000_000_000; // GB

#[derive(Copy, Clone, Debug)]
pub enum RocCacheDir<'a> {
    /// Normal scenario: reading from the user's cache dir on disk
    Persistent(&'a Path),
    /// For build.rs and tests where we never want to be downloading anything - yell loudly if we try!
    Disallowed,
    /// For tests only; we don't want to write to the real cache during a test!
    #[cfg(test)]
    Temp(&'a tempfile::TempDir),
}

/// Accepts either a path to the Roc cache dir, or else a TempDir. If a TempDir, always download
/// into that dir. If the cache dir on the filesystem, then look into it to see if we already
/// have an entry for the given URL. If we do, return its info. If we don't already have it, then:
///
/// - Download and decompress the compressed tarball from the given URL
/// - Verify its bytes against the hash in the URL
/// - Extract the tarball's contents into the appropriate cache directory
///
/// Returns the path to the installed package (which will be in the cache dir somewhere), as well
/// as the requested root module filename (optionally specified via the URL fragment).
#[cfg(not(target_family = "wasm"))]
pub fn install_package<'a>(
    roc_cache_dir: RocCacheDir<'_>,
    url: &'a str,
) -> Result<(PathBuf, Option<&'a str>), Problem> {
    let PackageMetadata {
        cache_subdir,
        content_hash,
        root_module_filename,
    } = PackageMetadata::try_from(url).map_err(Problem::InvalidUrl)?;

    match roc_cache_dir {
        RocCacheDir::Persistent(cache_dir) => {
            // e.g. ~/.cache/roc/example.com/roc-packages/
            let parent_dir = cache_dir.join(cache_subdir);
            // e.g. ~/.cache/roc/example.com/roc-packages/jDRlAFAA3738vu3-vMpLUoyxtA86Z7CaZneoOKrihbE
            let dest_dir = parent_dir.join(content_hash);

            if dest_dir.exists() {
                // If the cache dir exists already, we assume it has the correct contents
                // (it's a cache, after all!) and return without downloading anything.
                Ok((dest_dir, root_module_filename))
            } else {
                // Download into a tempdir; only move it to dest_dir if hash verification passes.
                println!(
                    "Downloading \u{001b}[36m{url}\u{001b}[0m\n    into {}\n",
                    cache_dir.display()
                );
                let tempdir = tempfile::tempdir().map_err(Problem::IoErr)?;
                let tempdir_path = tempdir.path();
                let downloaded_hash =
                    https::download_and_hash(url, tempdir_path, MAX_DOWNLOAD_BYTES)?;

                // Download the tarball into memory and verify it.
                // The tarball name is the hash of its contents.
                if downloaded_hash == content_hash {
                    // Now that we've verified the hash, rename the tempdir to the real dir.

                    // Create the destination dir's parent dir, since it may not exist yet.
                    fs::create_dir_all(parent_dir).map_err(Problem::IoErr)?;

                    // This should be super cheap - just an inode change.
                    fs::rename(tempdir_path, &dest_dir).map_err(Problem::IoErr)?;

                    // The package's files are now in the cache. We're done!
                    Ok((dest_dir, root_module_filename))
                } else {
                    Err(Problem::InvalidContentHash {
                        expected: content_hash.to_string(),
                        actual: downloaded_hash,
                    })
                }
            }
        }
        RocCacheDir::Disallowed => {
            internal_error!(
                "Tried to download a package ({:?}) via RocCacheDir::Disallowed - which was explicitly used in order to disallow downloading packages in the current context!",
                url
            )
        }
        #[cfg(test)]
        RocCacheDir::Temp(temp_dir) => Ok((temp_dir.path().to_path_buf(), None)),
    }
}

#[cfg(windows)]
// e.g. the "Roc" in %APPDATA%\\Roc
const ROC_CACHE_DIR_NAME: &str = "Roc";

#[cfg(not(windows))]
// e.g. the "roc" in ~/.cache/roc
const ROC_CACHE_DIR_NAME: &str = "roc";

/// This looks up environment variables, so it should ideally be called once and then cached!
///
/// Returns a path of the form cache_dir_path.join(ROC_CACHE_DIR_NAME).join("packages")
/// where cache_dir_path is:
/// - The XDG_CACHE_HOME environment varaible, if it's set.
/// - Otherwise, ~/.cache on UNIX and %APPDATA% on Windows.
///
/// ROC_CACHE_DIR_NAME is "roc" on UNIX and "Roc" on Windows.
///
/// So ~/.cache/roc will be typical on UNIX, and %APPDATA%\\Roc will be typical on Windows.
///
/// Returns None if XDG_CACHE_HOME is not set, and also we can't determine the home directory
/// (or if %APPDATA% is missing on Windows) on this system.
#[cfg(not(target_family = "wasm"))]
pub fn roc_cache_dir() -> PathBuf {
    use std::{env, process};

    const PACKAGES_DIR_NAME: &str = "packages";

    // Respect XDG, if the system appears to be using it.
    // https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
    match env::var_os("XDG_CACHE_HOME") {
        Some(xdg_cache_home) => Path::new(&xdg_cache_home)
            .join(ROC_CACHE_DIR_NAME)
            .join(PACKAGES_DIR_NAME),
        None => {
            #[cfg(windows)]
            {
                // e.g. %APPDATA%\\Roc
                if let Some(appdata) =
                    // CSIDL_APPDATA is the same as APPDATA, according to:
                    // https://learn.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables
                    env::var_os("APPDATA").or_else(|| env::var_os("CSIDL_APPDATA"))
                {
                    Path::new(&appdata)
                        .join(ROC_CACHE_DIR_NAME)
                        .join(PACKAGES_DIR_NAME)
                } else {
                    eprintln!("roc needs either the %APPDATA% or else the %XDG_CACHE_HOME% environment variables set. Please set one of these environment variables and re-run roc!");
                    process::exit(1);
                }
            }

            #[cfg(unix)]
            {
                // e.g. $HOME/.cache/roc
                if let Some(home) = env::var_os("HOME") {
                    Path::new(&home)
                        .join(".cache")
                        .join(ROC_CACHE_DIR_NAME)
                        .join(PACKAGES_DIR_NAME)
                } else {
                    eprintln!("roc needs either the $HOME or else the $XDG_CACHE_HOME environment variables set. Please set one of these environment variables and re-run roc!");
                    process::exit(1);
                }
            }
        }
    }
}

/// WASI doesn't have a home directory, so just make the cache dir in the current directory
/// https://github.com/WebAssembly/wasi-filesystem/issues/59
#[cfg(target_family = "wasm")]
pub fn roc_cache_dir() -> PathBuf {
    PathBuf::from(".cache").join(ROC_CACHE_DIR_NAME)
}
