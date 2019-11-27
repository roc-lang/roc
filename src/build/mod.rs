use bumpalo::collections::Vec;
use bumpalo::Bump;
use can::symbol::Symbol;
use collections::{ImMap, MutMap};
use ident::UnqualifiedIdent;
use module::ModuleName;
use parse::ast::{Attempting, Def, ExposesEntry, ImportsEntry, Module};
use parse::module;
use parse::parser::{Fail, Parser, State};
use region::{Located, Region};
use std::fs::read_to_string;
use std::io;
use std::path::{Path, PathBuf};

struct Env<'a> {
    pub arena: &'a Bump,
    pub src_dir: &'a Path,
    pub problems: &'a mut Vec<'a, BuildProblem<'a>>,
    pub loaded_headers: &'a mut MutMap<ModuleName<'a>, LoadedHeader<'a>>,
    pub queue: Queue<'a>,
}

type Queue<'a> = MutMap<ModuleName<'a>, State<'a>>;

pub enum BuildProblem<'a> {
    FileNotFound(&'a Path),
}

pub enum LoadedHeader<'a> {
    Valid {
        scope: ImMap<UnqualifiedIdent<'a>, (Symbol, Region)>,
    },
    FileProblem(io::Error),
    ParsingFailed(Fail),
}

pub fn build<'a>(
    arena: &'a Bump,
    src_dir: &'a Path,
    filename: &'a Path,
    problems: &'a mut Vec<'a, BuildProblem<'a>>,
    loaded_headers: &'a mut MutMap<ModuleName<'a>, LoadedHeader<'a>>,
    loaded_defs: &'a mut MutMap<ModuleName<'a>, Result<Vec<'a, Located<Def<'a>>>, Fail>>,
) -> LoadedHeader<'a> {
    let mut env = Env {
        arena,
        src_dir,
        problems,
        loaded_headers,
        queue: MutMap::default(),
    };

    let answer = load_filename(&mut env, filename);

    for (module_name, state) in env.queue {
        let defs = match module::module_defs().parse(arena, state) {
            Ok((defs, _)) => Ok(defs),
            Err((fail, _)) => Err(fail),
        };

        loaded_defs.insert(module_name, defs);
    }

    answer
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
/// This would need to be done using a work-stealing scheduler like tokio_threadpool.
/// However, a prerequisite of this is that we are able to canonicalize in parallel.
/// Otherwise, there isn't much benefit to doing things in parallel.
///
/// To canonicalize in parallel, we want to be able to generate Variables in parallel,
/// which currently would require a Mutex on Subs. We can avoid that Mutex if we can
/// give each thread in a thread pool a "starting id" - distributed into (usize::MAX / n)
/// ranges. For example, if there are 2 threads, the first thread gets to start at id 0,
/// and the second thread starts at id (usize::MAX / 2). That way both of them can increment
/// in parallel without colliding. (If we have 1024 threads running at once, on a 64-bit
/// system, we still have over 1 quadrillion Variables per thread. Seems like enough.)
///
/// However, to support *that*, we need to change Subs to be backed by an actual HashMap
/// instead of a flat Vec where the Variables are direct indices. Once that's done,
/// we can canonicalize in parallel without needing mutexes for Subs.
///
/// Anyway, for now, we'll do this in a synchronous, blocking way.

/// Resolve a module's list of imports, creating a Scope map for use in the
/// module's canonicalization.
///
/// If a given import has not been loaded yet, load it too.
fn load_module<'a>(env: &mut Env<'a>, module_name: &ModuleName<'a>) -> LoadedHeader<'a> {
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
    filename.push(".roc");

    load_filename(env, &filename)
}

fn load_filename<'a>(env: &mut Env<'a>, filename: &Path) -> LoadedHeader<'a> {
    match read_to_string(filename) {
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
                    let mut scope = ImMap::default();

                    // Enqueue the defs parsing job for background processing.
                    env.queue.insert(header.name.value, state);

                    for loc_entry in header.imports {
                        load_import(env, loc_entry.region, &loc_entry.value, &mut scope);
                    }

                    LoadedHeader::Valid { scope }
                }
                Ok((Module::App { header }, state)) => {
                    let mut scope = ImMap::default();

                    // Enqueue the defs parsing job for background processing.
                    // The app module has a module name of ""
                    env.queue.insert(ModuleName::new(""), state);

                    for loc_entry in header.imports {
                        load_import(env, loc_entry.region, &loc_entry.value, &mut scope);
                    }

                    LoadedHeader::Valid { scope }
                }
                Err((fail, _)) => LoadedHeader::ParsingFailed(fail),
            }
        }
        Err(err) => LoadedHeader::FileProblem(err),
    }
}

fn load_import<'a>(
    env: &mut Env<'a>,
    region: Region,
    entry: &ImportsEntry<'a>,
    scope: &mut ImMap<UnqualifiedIdent<'a>, (Symbol, Region)>,
) {
    use parse::ast::ImportsEntry::*;

    match entry {
        Module(module_name, exposes) => {
            // If we haven't already loaded the module, load it!
            if !env.loaded_headers.contains_key(&module_name) {
                let loaded = load_module(env, module_name);

                env.loaded_headers.insert(*module_name, loaded);
            }

            for loc_entry in exposes {
                expose(*module_name, &loc_entry.value, loc_entry.region, scope)
            }
        }

        SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => {
            // Ignore spaces.
            load_import(env, region, *sub_entry, scope)
        }
    }
}

fn expose<'a>(
    module_name: ModuleName<'_>,
    entry: &ExposesEntry<'a>,
    region: Region,
    scope: &mut ImMap<UnqualifiedIdent<'a>, (Symbol, Region)>,
) {
    use parse::ast::ExposesEntry::*;

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
