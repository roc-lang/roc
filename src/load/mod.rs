use crate::can::symbol::Symbol;
use crate::can::expr::Expr;
use crate::can::pattern::Pattern;
use crate::can::canonicalize_module_defs;
use crate::collections::{SendSet, ImMap};
use crate::module::ModuleName;
use crate::parse::ast::{self, Attempting, ExposesEntry, ImportsEntry};
use crate::parse::module::{self, module_defs};
use crate::parse::parser::{Fail, Parser, State};
use crate::region::{Located, Region};
use im::Vector;
use bumpalo::Bump;
use tokio::fs::read_to_string;
use tokio::sync::mpsc::{self, Sender, Receiver};
use std::io;
use std::path::{Path, PathBuf};
use crate::can::module::Module;
use futures::future::join_all;

pub struct Loaded {
    pub requested_module: LoadedModule,
    pub deps: Deps,
}

#[derive(Debug, Clone)]
struct Env {
    pub src_dir: PathBuf
}

#[derive(Debug)]
pub enum BuildProblem<'a> {
    FileNotFound(&'a Path),
}

type Deps = SendSet<Box<str>>;

#[derive(Debug, PartialEq)]
pub enum LoadedModule {
    Valid(Module),
    FileProblem(io::ErrorKind),
    ParsingFailed(Fail),
}

pub async fn load<'a>(src_dir: PathBuf, filename: PathBuf) -> Loaded {
    let env = Env { src_dir: src_dir.clone() };
    let (tx, mut rx): (Sender<Deps>, Receiver<Deps>) = mpsc::channel(1024);

    let main_tx = tx.clone();
    let handle = tokio::spawn(async move {
        load_filename(&env, &filename, main_tx).await
    });

    let requested_module = handle.await.expect("Unable to load requested module.");
    let mut other_modules = Vec::new();
    let mut all_deps = SendSet::default();

    // Get a fresh env, since the previous one has been consumed
    let env = Env { src_dir };
    // At first, 1 module is pending (namely the `filename` one).
    let mut pending = 1;

    while let Some(module_deps) = rx.recv().await {
        let deps_to_load = module_deps.relative_complement(all_deps.clone());

        // We just loaded 1 module, and gained deps_to_load more
        pending = pending + deps_to_load.len() - 1;

        // Record that these are loaded *before* spawning threads to load them.
        all_deps = all_deps.union(deps_to_load.clone());

        let loaded_modules = join_all(deps_to_load.into_iter().map(|dep|{
            let env = env.clone();
            let tx = tx.clone();

            tokio::spawn(async move {
                load_module(&env, dep, tx).await
            })
        })).await;

        for module in loaded_modules {
            other_modules.push(module.expect("Unable to load dependent module"));
        }

        // Once we've run out of pending modules to process, we're done!
        if pending == 0 {
            break;
        }
    }

    Loaded { requested_module, deps: all_deps }
}

/// The long-term plan is for the loading process to work like this, starting from main.roc:
///
/// 1. Open the file.
/// 2. Parse its header.
/// 3. For each of its imports, repeat this process starting with step 1.
/// 4. Once a given import is finished parsing, we can process that import.
/// 5. Processing an import entails checking what we want to import against what it exposes.
/// 6. If anything we want to import unqualified is not exposed, record a problem.
/// 7. Add everything we were able to import unqualified to the module's default scope.
/// 8. Once all imports have been processed for this module, canonicalize it.
///
/// This would ideally be done using a parallel work-stealing scheduler like tokio_threadpool.
/// However, a prerequisite of this is that we are able to canonicalize in parallel!
///
/// To canonicalize in parallel, we want to be able to generate Variables in parallel,
/// which currently would require a Mutex on Subs. We can avoid that Mutex in one of two ways.
///
/// One way would be to give each thread in a thread pool a "starting id" -
/// distributed into (usize::MAX / n) ranges.  For example, if there are 2 threads,
/// the first thread gets to start at id 0, and the second thread starts at
/// id (usize::MAX / 2). That way both of them can increment in parallel without colliding.
/// (If we have 1024 threads running at once, on a 64-bit system, we still have
/// over 1 quadrillion Variables per thread. Seems like enough.)
/// However, to support that, we need to change Subs to be able to look up arbitrary IDs,
/// instead of being backed by a flat Vec where each Variable is a direct array index.
///
/// A strategy I like better, which should be slightly slower for canonicalization
/// (which is likely I/O bound anyway since it'll be happening concurrently with file reads),
/// but *much* faster for unification, is to give each thread a shared AtomicUsize which
/// they each call .fetch_add(1) on to get a fresh ID. Atomic increment is a bit slower than
/// regular increment, but it means afterwards unification (which I'm not yet sure how to
/// parallelize) no longer needs to use a hashing function to get the contents of each ID;
/// the IDs will already correspond directly to array indices like they do in the status quo.
///
/// Separately, if we use that strategy, there's probably another optimization opportunity:
/// instead of instantiating fresh structs with mk_fresh_var(), ensure that the default of
/// each struct will be all 0s in memory. That way, after we've distributed all the IDs,
/// we can do one single Vec resize (to zeroed memory) and they're all instantly ready to go.
///
/// Anyway, that'll all take awhile; for now, we'll do this in a synchronous, blocking way.

/// Resolve a module's list of imports, creating a Scope map for use in the
/// module's canonicalization.
///
/// If a given import has not been loaded yet, load it too.
async fn load_module(env: &Env, module_name: Box<str>, tx: Sender<Deps>) -> LoadedModule {
    // 1. Convert module_name to filename, using src_dir.
    // 2. Open that file for reading. (If there's a problem, record it and bail.)
    // 3. Read the whole file into a string. (In the future, we can read just the header.)
    // 4. Parse the header.
    // 5. Use the parsed header to load more modules as necessary.
    // 6. Now that all the headers have been parsed, parse the bodies too.
    // 7. Once all the bodies have been parsed, canonicalize beginning with the leaves.

    let mut filename = PathBuf::new();

    filename.push(env.src_dir.clone());

    // Convert dots in module name to directories
    for part in module_name.split('.') {
        filename.push(part);
    }

    // End with .roc
    filename.set_extension("roc");

    load_filename(env, &filename, tx).await
}

async fn load_filename(env: &Env, filename: &Path, tx: Sender<Deps>) -> LoadedModule {
    match read_to_string(filename).await {
        Ok(src) => {
            let arena = Bump::new();
            // TODO instead of env.arena.alloc(src), we should create a new buffer
            // in the arena as a Vec<'a, u8> and call .as_mut_slice() on it to
            // get a (&mut [u8]) which can be passed to io::Read::read directly
            // instead of using read_to_string. This way, we avoid both heap-allocating
            // the String (which read_to_string does) and also re-allocating it
            // in the arena after read_to_string completes.
            let state = State::new(&src, Attempting::Module);

            let answer = match module::module().parse(&arena, state) {
                Ok((ast::Module::Interface { header }, state)) => {
                    let declared_name: Box<str> = header.name.value.as_str().into();

                    // TODO check to see if declared_name is consistent with filename.
                    // If it isn't, report a problem!

                    let mut scope = ImMap::default();
                    let mut deps = SendSet::default();

                    for loc_entry in header.imports {
                        deps.insert(load_import(env, loc_entry.region, &loc_entry.value, &mut scope));
                    }

                    tokio::spawn(async move {
                        let mut tx = tx;

                        // Send the deps to the main thread for processing,
                        // then continue on to parsing and canonicalizing defs.
                        tx.send(deps).await.unwrap();
                    });

                    let defs = parse_and_canonicalize_defs(&arena,  state, declared_name.clone(), &mut scope);
                    let module = Module { name: Some(declared_name), defs };

                    LoadedModule::Valid(module)
                }
                Ok((ast::Module::App { header }, state)) => {
                    let mut scope = ImMap::default();
                    let mut deps = SendSet::default();

                    for loc_entry in header.imports {
                        deps.insert(load_import(env, loc_entry.region, &loc_entry.value, &mut scope));
                    }

                    tokio::spawn(async move {
                        let mut tx = tx;

                        // Send the deps to the main thread for processing,
                        // then continue on to parsing and canonicalizing defs.
                        tx.send(deps).await.unwrap();
                    });

                    // The app module has no declared name. Pass it as "".
                    let defs = parse_and_canonicalize_defs(&arena, state, "".into(), &mut scope);
                    let module = Module { name: None, defs };

                    LoadedModule::Valid(module)
                }
                Err((fail, _)) => LoadedModule::ParsingFailed(fail),
            };

            answer
        }
        Err(err) => LoadedModule::FileProblem(err.kind()),
    }
}

fn parse_and_canonicalize_defs(arena: &Bump, state: State<'_>, home: Box<str>, scope: &mut ImMap<Box<str>, (Symbol, Region)>) -> Vector<(Located<Pattern>, Located<Expr>)> {
    let (parsed_defs, _) = module_defs().parse(arena, state).expect("TODO gracefully handle parse error on module defs");

    canonicalize_module_defs(arena, parsed_defs, home, scope)
}

fn load_import(
    env: &Env,
    region: Region,
    entry: &ImportsEntry<'_>,
    scope: &mut ImMap<Box<str>, (Symbol, Region)>,
) -> Box<str> {
    use crate::parse::ast::ImportsEntry::*;

    match entry {
        Module(module_name, exposes) => {
            for loc_entry in exposes {
                let (key, value) = expose(*module_name, &loc_entry.value, loc_entry.region);

                scope.insert(key, value);
            }

            module_name.as_str().into()
        }

        SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => {
            // Ignore spaces.
            load_import(env, region, *sub_entry, scope)
        }
    }
}

fn expose(
    module_name: ModuleName<'_>,
    entry: &ExposesEntry<'_>,
    region: Region,
)->  (Box<str>, (Symbol, Region)){
    use crate::parse::ast::ExposesEntry::*;

    match entry {
        Ident(ident) => {
            // Since this value is exposed, add it to our module's default scope.
            let symbol = Symbol::from_module(&module_name, &ident);

            (ident.as_str().into(), (symbol, region))
        }
        SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => {
            // Ignore spaces.
            expose(module_name, *sub_entry, region)
        }
    }
}
