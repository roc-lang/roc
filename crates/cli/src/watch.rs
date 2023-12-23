use crossbeam::channel::{bounded, RecvError, Sender};
use roc_load::{ChannelProblem, LoadingProblem};
use roc_reporting::cli::Problems;
use std::{
    path::Path,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::Duration,
};

pub fn with_watch<'a>(
    func: impl Fn(Sender<()>) -> Result<(), LoadingProblem<'a>>,
) -> Result<(), LoadingProblem<'a>> {
    let (watch_tx, watch_rx) = bounded(64);
    let rebuild_needed = Arc::new(AtomicBool::new(false));

    // We explicitly extend the lifetime of the watcher to the end of the function.
    // Otherwise, it gets dropped too soon and stops watching the filesystem!
    #[cfg(not(target_family = "wasm"))]
    let opt_watcher;

    // --watch is not supported on wasm; there is no filesystem to watch!
    #[cfg(not(target_family = "wasm"))]
    {
        opt_watcher = match init_watcher(watch_tx.clone(), Arc::clone(&rebuild_needed)) {
            Ok(watcher) => Some(watcher),
            Err(err) => {
                return Err(LoadingProblem::InitWatchFailed(err.to_string()));
            }
        }
    }

    loop {
        match func(watch_tx.clone()) {
            Ok(()) => {
                // Do nothing; the function should have reported everything.
            }
            Err(loading_problem) => todo!("report loading problem {:?}", loading_problem),
        }

        if !(*rebuild_needed).load(Ordering::Relaxed) {
            // We didn't get a request for a rebuild yet (meaning the build finished
            // before the file changed on the filesystem; this is the most common case),
            // so block until we get a request for a rebuild.
            match watch_rx.recv() {
                Ok(_) => todo!("time to rebuild!"),
                Err(RecvError) => {
                    return Err(LoadingProblem::ChannelProblem(
                        ChannelProblem::ChannelDisconnected,
                    ))
                }
            }
        }
    }

    // Artificially extend the lifetime of watch_tx so it doesn't get dropped and disconnected.
    let _ = watch_tx;

    // Artificially extend the lifetime of the watcher so it doesn't stop watching prematurely.
    #[cfg(not(target_family = "wasm"))]
    let _ = opt_watcher;

    Ok(())
}

// Watching is not supported on wasm; there is no filesystem to watch!
#[cfg(not(target_family = "wasm"))]
fn init_watcher<'a>(
    watch_tx: Sender<()>,
    rebuild_needed: Arc<AtomicBool>,
) -> notify::Result<notify::RecommendedWatcher> {
    use notify::{RecursiveMode, Watcher};
    use std::{ffi::OsStr, os::unix::ffi::OsStrExt, sync::Arc};

    let mut watcher =
        notify::recommended_watcher(move |res: notify::Result<notify::Event>| match res {
            Ok(event) => {
                // We only care about changes to .roc files
                if event
                    .paths
                    .iter()
                    .any(|path| path.extension() == Some(&OsStr::from_bytes(b"roc")))
                {
                    rebuild_needed.store(true, Ordering::Relaxed);
                    // Since a .roc file changed, we need to abort the current build
                    // and rebuild.
                    watch_tx.send(()).unwrap();
                }
            }
            Err(err) => eprintln!("Error watching filesystem for changes: {:?}", err),
        })?;

    // Watch the directory the root module is in, plus all of its descendant directories.
    // TODO: instead of watching "." we should watch the path to the root module.
    // TODO: the way `notify` does this is much less efficient than what we want. We should
    // replace it with something that gives us OS events, which we can filter ourselves.
    watcher.watch(Path::new("."), RecursiveMode::Recursive)?;

    Ok(watcher)
}
