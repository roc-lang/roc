use crate::can::symbol::Symbol;
use crate::collections::{ImMap, MutMap};
use crate::ident::UnqualifiedIdent;
use crate::module::ModuleName;
use crate::parse::ast::{Attempting, Def, ExposesEntry, ImportsEntry, Module};
use crate::parse::module;
use crate::parse::parser::{Fail, Parser, State};
use crate::region::{Located, Region};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use std::future::Future;
use std::io;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use tokio::fs::read_to_string;

pub struct Loaded<'a> {
    pub requested_header: LoadedHeader<'a>,
    pub dependent_headers: ImMap<ModuleName<'a>, LoadedHeader<'a>>,
    pub defs: MutMap<ModuleName<'a>, Result<Vec<'a, Located<Def<'a>>>, Fail>>,
}

struct Env<'a> {
    pub arena: &'a Bump,
    pub src_dir: &'a Path,
    pub queue: Queue<'a>,
}

type Queue<'a> = MutMap<ModuleName<'a>, State<'a>>;

#[derive(Debug)]
pub enum BuildProblem<'a> {
    FileNotFound(&'a Path),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LoadedHeader<'a> {
    Valid {
        scope: ImMap<UnqualifiedIdent<'a>, (Symbol, Region)>,
    },
    FileProblem(io::ErrorKind),
    ParsingFailed(Fail),
}

pub async fn load<'a>(arena: &'a Bump, src_dir: &'a Path, filename: &Path) -> Loaded<'a> {
    let env = Env {
        arena,
        src_dir,
        queue: MutMap::default(),
    };


    /// TODO proof of concept:
    ///
    /// set up a job queue, and load *all* modules using that.
    /// after each one loads, maintain a cache of "we've already started loading this"
    /// so subsequent ones don't need to enqueue redundantly - 
    /// but also check again before running a fresh load!
    /// Also, use a similar (maybe even the same?) queue for parsing defs in parallel

    let (requested_header, dependent_headers) = load_filename(&env, filename).await;
    let mut defs = MutMap::default();

    // for (module_name, state) in env.queue {
    //     let loaded_defs = match module::module_defs().parse(arena, state) {
    //         Ok((defs, _)) => Ok(defs),
    //         Err((fail, _)) => Err(fail),
    //     };

    //     defs.insert(module_name, loaded_defs);
    // }

    Loaded {
        requested_header,
        dependent_headers,
        defs,
    }
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
async fn load_module<'a>(env: &'a Env<'a>, 
    loaded_headers: ImMap<ModuleName<'a>, LoadedHeader<'a>>,
                         module_name: &ModuleName<'a>) -> (LoadedHeader<'a>, ImMap<ModuleName<'a>, LoadedHeader<'a>>)  {
    // 1. Convert module_name to filename, using src_dir.
    // 2. Open that file for reading. (If there's a problem, record it and bail.)
    // 3. Read the whole file into a string. (In the future, we can read just the header.)
    // 4. Parse the header.
    // 5. Use the parsed header to load more modules as necessary.
    // 6. Now that all the headers have been parsed, parse the bodies too.
    // 7. Once all the bodies have been parsed, canonicalize beginning with the leaves.

    let mut filename = PathBuf::new();

    filename.push(env.src_dir);

    // Convert dots in module name to directories
    for part in module_name.as_str().split('.') {
        filename.push(part);
    }

    // End with .roc
    filename.set_extension("roc");

    load_filename(env, loaded_headers,&filename).await
}

async fn load_filename<'a, 'p>(env: &'a Env<'a>, 
                               
    loaded_headers: ImMap<ModuleName<'a>, LoadedHeader<'a>>,
                               filename: &Path) -> (LoadedHeader<'a>, ImMap<ModuleName<'a>, LoadedHeader<'a>>) {
    let imports = match read_to_string(filename).await {
        Ok(src) => {
            // TODO instead of env.arena.alloc(src), we should create a new buffer
            // in the arena as a Vec<'a, u8> and call .as_mut_slice() on it to
            // get a (&mut [u8]) which can be passed to io::Read::read directly
            // instead of using read_to_string. This way, we avoid both heap-allocating
            // the String (which read_to_string does) and also re-allocating it
            // in the arena after read_to_string completes.
            let state = State::new(env.arena.alloc(src), Attempting::Module);

            match module::module().parse(env.arena, state) {
                Ok((Module::Interface { header }, state)) => {
                    // Enqueue the defs parsing job for background processing.
                    // env.queue.insert(header.name.value, state);
                   
                    header.imports
                }
                Ok((Module::App { header }, state)) => {
                    // Enqueue the defs parsing job for background processing.
                    // The app module has a module name of ""
                    // env.queue.insert(ModuleName::new(""), state);
                    
                    header.imports
                }
                Err((fail, _)) => {
                    return LoadedHeader::ParsingFailed(fail);
                }
            }
        }
        Err(err) => return LoadedHeader::FileProblem(err.kind()),
    };

    let mut scope = ImMap::default();
    let mut headers = ImMap::default();

    for loc_entry in imports {
        let (new_scope, opt_header) = 
            load_import(env, loc_entry.region, loaded_headers, env.arena.alloc(loc_entry.value)).await;

        scope = scope.union(new_scope);

        if let Some((module_name, loaded_header)) = opt_header {
            headers.insert(module_name, loaded_header);
        }
    }

    (LoadedHeader::Valid { scope }, headers)
} 

type Scope<'a>= ImMap<UnqualifiedIdent<'a>, (Symbol, Region)>;

fn load_import<'a>(
    env: &'a Env<'a>,
    region: Region,
    loaded_headers: ImMap<ModuleName<'a>, LoadedHeader<'a>>,
    entry: &'a ImportsEntry<'a>,
) -> Pin<Box<dyn Future<Output = (Scope<'a>, Option<(ModuleName<'a>, LoadedHeader<'a>)>)> + 'a>> {
    Box::pin(async move {
        use crate::parse::ast::ImportsEntry::*;

        match entry {
            Module(module_name, exposes) => {
                // If we haven't already loaded the module, load it!
                let new_header = if !loaded_headers.contains_key(&module_name) {
                    let loaded = load_module(env, loaded_headers, module_name).await;

                    Some((*module_name, loaded))
                } else {
                    None
                };

                let mut scope = ImMap::default();

                for loc_entry in exposes {
                    expose(*module_name, &loc_entry.value, loc_entry.region, &mut scope)
                }

                (scope, new_header)
            }

            SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => {
                // Ignore spaces.
                load_import(env, region, *sub_entry).await
            }
        }
    })
}

fn expose<'a>(
    module_name: ModuleName<'_>,
    entry: &ExposesEntry<'a>,
    region: Region,
    scope: &mut ImMap<UnqualifiedIdent<'a>, (Symbol, Region)>,
) {
    use crate::parse::ast::ExposesEntry::*;

    match entry {
        Ident(ident) => {
            // Since this value is exposed, add it to our module's default scope.
            let symbol = Symbol::from_module(&module_name, &ident);

            scope.insert(ident.clone(), (symbol, region));
        }
        SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => {
            // Ignore spaces.
            expose(module_name, *sub_entry, region, scope)
        }
    }
}
