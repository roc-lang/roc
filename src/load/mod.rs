use crate::builtins;
use crate::can;
use crate::can::def::Declaration;
use crate::can::ident::{Ident, Lowercase, ModuleName};
use crate::can::module::{canonicalize_module_defs, ModuleOutput};
use crate::collections::{default_hasher, insert_all, MutMap, MutSet, SendMap};
use crate::constrain::module::{
    constrain_imported_aliases, constrain_imported_values, constrain_module, load_builtin_aliases,
    Import,
};
use crate::module::symbol::{IdentIds, Interns, ModuleId, ModuleIds, Symbol};
use crate::parse::ast::{self, Attempting, ExposesEntry, ImportsEntry, InterfaceHeader};
use crate::parse::module::{self, module_defs};
use crate::parse::parser::{Fail, Parser, State};
use crate::region::{Located, Region};
use crate::solve::{self, ExposedModuleTypes, Solved, SolvedType, SubsByModule};
use crate::subs::{Subs, VarStore, Variable};
use crate::types::{self, Alias, Constraint};
use bumpalo::Bump;
use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use tokio::sync::mpsc;
use tokio::task::spawn_blocking;

/// Filename extension for normal Roc modules
const ROC_FILE_EXTENSION: &str = "roc";

/// The . in between module names like Foo.Bar.Baz
const MODULE_SEPARATOR: char = '.';

#[derive(Debug)]
pub struct Module {
    pub module_id: ModuleId,
    pub declarations: Vec<Declaration>,
    pub exposed_imports: MutMap<Symbol, Variable>,
    pub exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
    pub references: MutSet<Symbol>,
    pub aliases: MutMap<Symbol, Alias>,
    pub rigid_variables: MutMap<Lowercase, Variable>,
    pub imported_modules: MutSet<ModuleId>
}

#[derive(Debug)]
pub struct LoadedModule {
    pub module_id: ModuleId,
    pub interns: Interns,
    pub solved: Solved<Subs>,
    pub can_problems: Vec<can::problem::Problem>,
    pub type_problems: Vec<types::Problem>,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
struct Env {
    pub src_dir: PathBuf,
}

#[derive(Debug)]
pub enum BuildProblem<'a> {
    FileNotFound(&'a Path),
}

#[derive(Debug)]
struct ModuleHeader {
    module_id: ModuleId,
    module_name: ModuleName,
    exposed_ident_ids: IdentIds,
    deps_by_name: MutMap<ModuleName, ModuleId>,
    imported_modules: MutSet<ModuleId>,
    exposes: Vec<Symbol>,
    exposed_imports: MutMap<Ident, (Symbol, Region)>,
    src: Box<str>,
}

#[derive(Debug)]
enum Msg {
    Header(ModuleHeader),
    Constrained {
        module: Module,
        constraint: Constraint,
        ident_ids: IdentIds,
        problems: Vec<can::problem::Problem>,
        var_store: VarStore,
    },
    Solved {
        module_id: ModuleId,
        solved_types: MutMap<Symbol, SolvedType>,
        aliases: MutMap<Symbol, Alias>,
        subs: Arc<Solved<Subs>>,
        problems: Vec<types::Problem>,
    },
}

#[derive(Debug)]
pub enum LoadingProblem {
    FileProblem {
        filename: PathBuf,
        error: io::ErrorKind,
    },
    ParsingFailed {
        filename: PathBuf,
        fail: Fail,
    },
    MsgChannelDied,
}

enum MaybeShared<'a, 'b, A, B> {
    Shared(Arc<Mutex<A>>, Arc<Mutex<B>>),
    Unique(&'a mut A, &'b mut B),
}

type SharedModules<'a, 'b> = MaybeShared<'a, 'b, ModuleIds, IdentIdsByModule>;
type IdentIdsByModule = MutMap<ModuleId, IdentIds>;

type MsgSender = mpsc::Sender<Msg>;
type MsgReceiver = mpsc::Receiver<Msg>;

/// The loading process works like this, starting from the given filename (e.g. "main.roc"):
///
/// 1. Open the file.
/// 2. Parse the module's header.
/// 3. For each of its imports, send a message on the channel to the coordinator thread, which
///    will repeat this process to load that module - starting with step 1.
/// 4. Add everything we were able to import unqualified to the module's default scope.
/// 5. Parse the module's defs.
/// 6. Canonicalize the module.
/// 7. Before type checking, block on waiting for type checking to complete on all imports.
///    (Since Roc doesn't allow cyclic dependencies, this ctypeot deadlock.)
/// 8. Type check the module and create type annotations for its top-level declarations.
/// 9. Report the completed type annotation to the coordinator thread, so other modules
///    that are waiting in step 7 can unblock.
///
/// The loaded_modules argument specifies which modules have already been loaded.
/// It typically contains *at least* the standard modules, but is empty when loading
/// the standard modules themselves.
#[allow(clippy::cognitive_complexity)]
pub async fn load<'a>(
    src_dir: PathBuf,
    filename: PathBuf,
    mut exposed_types: SubsByModule,
) -> Result<LoadedModule, LoadingProblem> {
    use self::MaybeShared::*;

    let mut type_problems = Vec::new();
    let mut can_problems = Vec::new();
    let env = Env {
        src_dir: src_dir.clone(),
    };

    let (msg_tx, mut msg_rx): (MsgSender, MsgReceiver) = mpsc::channel(1024);
    let mut module_ids = ModuleIds::default();
    let mut root_exposed_ident_ids: IdentIdsByModule = IdentIds::exposed_builtins(0);

    // This is the "final" list of IdentIds, after canonicalization and constraint gen
    // have completed for a given module.
    let mut constrained_ident_ids = IdentIds::exposed_builtins(0);
    let mut headers_parsed = MutSet::default();

    // Load the root module synchronously; we can't proceed until we have its id.
    let root_id = load_filename(
        filename,
        msg_tx.clone(),
        Unique(&mut module_ids, &mut root_exposed_ident_ids),
    )?;

    headers_parsed.insert(root_id);

    // From now on, these will be used by multiple threads; time to make an Arc<Mutex<_>>!
    let arc_modules = Arc::new(Mutex::new(module_ids));
    let ident_ids_by_module: Arc<Mutex<IdentIdsByModule>> =
        Arc::new(Mutex::new(root_exposed_ident_ids));

    // All the dependent modules we've already begun loading -
    // meaning we should never kick off another load_module on them!
    let mut loading_started: MutSet<ModuleId> = MutSet::default();

    // If the root module we're compiling happens to be an interface,
    // it's possible that something else will import it. That will
    // necessarily cause a cyclic import error, but in the meantime
    // we still shouldn't load it.
    loading_started.insert(root_id);

    // The declarations we'll ultimately be returning
    let mut declarations_by_id: MutMap<ModuleId, Vec<Declaration>> = MutMap::default();

    let mut exposed_symbols_by_module: MutMap<ModuleId, MutSet<Symbol>> = MutMap::default();

    // Modules which are waiting for certain headers to be parsed
    let mut waiting_for_headers: MutMap<ModuleId, MutSet<ModuleId>> = MutMap::default();

    // When the key ModuleId gets solved, iterate through each of the given modules
    // and remove that ModuleId from the appropriate waiting_for_headers entry.
    // If the relevant module's waiting_for_headers entry is now empty, canonicalize the module.
    let mut header_listeners: MutMap<ModuleId, Vec<ModuleId>> = MutMap::default();

    let mut unparsed_modules: MutMap<ModuleId, ModuleHeader> = MutMap::default();

    // Modules which are waiting for certain deps to be solved
    let mut waiting_for_solve: MutMap<ModuleId, MutSet<ModuleId>> = MutMap::default();

    // When the key ModuleId gets solved, iterate through each of the given modules
    // and remove that ModuleId from the appropriate waiting_for_solve entry.
    // If the relevant module's waiting_for_solve entry is now empty, solve the module.
    let mut solve_listeners: MutMap<ModuleId, Vec<ModuleId>> = MutMap::default();

    let mut unsolved_modules: MutMap<ModuleId, (Module, Constraint, VarStore)> = MutMap::default();
    let builtins = builtins::types();

    // TODO can be removed I think
    let vars_by_symbol = SendMap::default();

    // Parse and canonicalize the module's deps
    while let Some(msg) = msg_rx.recv().await {
        use self::Msg::*;

        match msg {
            Header(header) => {
                let home = header.module_id;
                let deps_by_name = &header.deps_by_name;
                let mut headers_needed =
                    HashSet::with_capacity_and_hasher(deps_by_name.len(), default_hasher());

                headers_parsed.insert(home);

                for dep_id in deps_by_name.values() {
                    if !headers_parsed.contains(&dep_id) {
                        headers_needed.insert(*dep_id);
                    }
                }

                // This was a dependency. Write it down and keep processing messaages.
                let mut exposed_symbols: MutSet<Symbol> =
                    HashSet::with_capacity_and_hasher(header.exposes.len(), default_hasher());

                // TODO can we avoid this loop by storing them as a Set in Header to begin with?
                for symbol in header.exposes.iter() {
                    exposed_symbols.insert(*symbol);
                }

                debug_assert!(!exposed_symbols_by_module.contains_key(&home));
                exposed_symbols_by_module.insert(home, exposed_symbols);

                // Notify all the listeners that headers are now available for this module.
                if let Some(listeners) = header_listeners.remove(&home) {
                    for listener_id in listeners {
                        // This listener is longer waiting for this module,
                        // because this module's headers are now available!
                        let waiting_for = waiting_for_headers
                            .get_mut(&listener_id)
                            .expect("Unable to find module ID in waiting_for_headers");

                        waiting_for.remove(&home);

                        // If it's no longer waiting for anything else, solve it.
                        if waiting_for.is_empty() {
                            let header = unparsed_modules
                                .remove(&listener_id)
                                .expect("Could not find listener ID in unparsed_modules");

                            let exposed_symbols = exposed_symbols_by_module
                                .remove(&listener_id)
                                .expect("Could not find listener ID in exposed_symbols_by_module");

                            spawn_parse_and_constrain(
                                header,
                                Arc::clone(&arc_modules),
                                Arc::clone(&ident_ids_by_module),
                                &exposed_types,
                                exposed_symbols.clone(),
                                &mut waiting_for_solve,
                                msg_tx.clone(),
                            )
                        }
                    }
                }

                // If any of our deps weren't loaded before, start loading them.
                for (dep_name, dep_id) in deps_by_name.iter() {
                    if !loading_started.contains(&dep_id) {
                        // Record that we've started loading the module *before*
                        // we actually start loading it.
                        loading_started.insert(*dep_id);

                        let env = env.clone();
                        let msg_tx = msg_tx.clone();
                        let dep_name = dep_name.clone();

                        // Provide mutexes of ModuleIds and IdentIds by module,
                        // so other modules can populate them as they load.
                        let shared =
                            Shared(Arc::clone(&arc_modules), Arc::clone(&ident_ids_by_module));

                        // Start loading this module in the background.
                        spawn_blocking(move || load_module(env, dep_name, msg_tx, shared));
                    }
                }

                if headers_needed.is_empty() {
                    let exposed_symbols = exposed_symbols_by_module
                        .remove(&home)
                        .expect("Could not find listener ID in exposed_symbols_by_module");

                    spawn_parse_and_constrain(
                        header,
                        Arc::clone(&arc_modules),
                        Arc::clone(&ident_ids_by_module),
                        &exposed_types,
                        exposed_symbols,
                        &mut waiting_for_solve,
                        msg_tx.clone(),
                    )
                } else {
                    // We will have to wait for our deps' headers to be parsed,
                    // so we can access their IdentId, which we need for canonicalization.
                    debug_assert!(!unparsed_modules.contains_key(&home));
                    unparsed_modules.insert(home, header);

                    // Register a listener with each of these.
                    for dep_id in headers_needed.iter() {
                        let listeners = header_listeners
                            .entry(*dep_id)
                            .or_insert_with(|| Vec::with_capacity(1));

                        (*listeners).push(home);
                    }

                    debug_assert!(!waiting_for_headers.contains_key(&home));
                    waiting_for_headers.insert(home, headers_needed);
                }
            }
            Constrained {
                module,
                ident_ids,
                constraint,
                problems,
                var_store,
            } => {
                can_problems.extend(problems);

                let module_id = module.module_id;
                let waiting_for = waiting_for_solve.get_mut(&module_id).unwrap_or_else(|| {
                    panic!(
                        "Could not find module ID {:?} in waiting_for_solve",
                        module_id
                    )
                });

                // Record the final IdentIds
                debug_assert!(!constrained_ident_ids.contains_key(&module_id));
                constrained_ident_ids.insert(module_id, ident_ids);

                // It's possible that some modules have been solved since
                // we began waiting for them. Remove those from waiting_for,
                // because we no longer need to wait for them!
                waiting_for.retain(|id| !exposed_types.contains_key(id));

                if waiting_for.is_empty() {
                    // All of our dependencies have already been solved. Great!
                    // That means we can proceed directly to solving.
                    solve_module(
                        module,
                        constraint,
                        var_store,
                        msg_tx.clone(),
                        &mut exposed_types,
                        &mut declarations_by_id,
                        &builtins,
                        vars_by_symbol.clone(),
                    );
                } else {
                    // We will have to wait for our dependencies to be solved.
                    debug_assert!(!unsolved_modules.contains_key(&module_id));
                    unsolved_modules.insert(module_id, (module, constraint, var_store));

                    // Register a listener with each of these.
                    for dep_id in waiting_for.iter() {
                        let listeners = solve_listeners
                            .entry(*dep_id)
                            .or_insert_with(|| Vec::with_capacity(1));

                        (*listeners).push(module_id);
                    }
                }
            }
            Solved {
                module_id,
                solved_types,
                subs,
                problems,
                aliases,
                ..
            } => {
                type_problems.extend(problems);

                if module_id == root_id {
                    // Once we've solved the originally requested module, we're done!
                    msg_rx.close();

                    let solved = Arc::try_unwrap(subs).unwrap_or_else(|_| {
                        panic!("There were still outstanding Arc references to Solved<Subs>")
                    });

                    let module_ids = Arc::try_unwrap(arc_modules)
                        .unwrap_or_else(|_| {
                            panic!("There were still outstanding Arc references to module_ids")
                        })
                        .into_inner()
                        .expect("Unwrapping mutex for module_ids");

                    let declarations = declarations_by_id
                        .remove(&module_id)
                        .expect("declarations_by_id was missing root module_id entry");

                    let interns = Interns {
                        module_ids,
                        all_ident_ids: constrained_ident_ids,
                    };

                    return Ok(LoadedModule {
                        module_id: root_id,
                        interns,
                        solved,
                        can_problems,
                        type_problems,
                        declarations,
                    });
                } else {
                    // This was a dependency. Write it down and keep processing messages.
                    debug_assert!(!exposed_types.contains_key(&module_id));
                    exposed_types
                        .insert(module_id, ExposedModuleTypes::Valid(solved_types, aliases));

                    // Notify all the listeners that this solved.
                    if let Some(listeners) = solve_listeners.remove(&module_id) {
                        for listener_id in listeners {
                            // This listener is longer waiting for this module,
                            // because this module has now been solved!
                            let waiting_for = waiting_for_solve
                                .get_mut(&listener_id)
                                .expect("Unable to find module ID in waiting_for_solve");

                            waiting_for.remove(&module_id);

                            // If it's no longer waiting for anything else, solve it.
                            if waiting_for.is_empty() {
                                let (module, constraint, var_store) = unsolved_modules
                                    .remove(&listener_id)
                                    .expect("Could not find listener ID in unsolved_modules");
                                let home = module.module_id;
                                let unused_modules = solve_module(
                                    module,
                                    constraint,
                                    var_store,
                                    msg_tx.clone(),
                                    &mut exposed_types,
                                    &mut declarations_by_id,
                                    &builtins,
                                    vars_by_symbol.clone(),
                                );

                                for _unused_module_id in unused_modules.iter() {
                                    panic!("TODO gracefully report unused imports for {:?}, namely {:?}", home, unused_modules);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // The msg_rx receiver closed unexpectedly before we finished solving everything
    Err(LoadingProblem::MsgChannelDied)
}

/// Load a module by its module name, rather than by its filename
fn load_module(
    env: Env,
    module_name: ModuleName,
    msg_tx: MsgSender,
    module_ids: SharedModules<'_, '_>,
) -> Result<ModuleId, LoadingProblem> {
    let mut filename = PathBuf::new();

    filename.push(env.src_dir);

    // Convert dots in module name to directories
    for part in module_name.as_str().split(MODULE_SEPARATOR) {
        filename.push(part);
    }

    // End with .roc
    filename.set_extension(ROC_FILE_EXTENSION);

    load_filename(filename, msg_tx, module_ids)
}

/// Load a module by its filename
fn load_filename(
    filename: PathBuf,
    msg_tx: MsgSender,
    module_ids: SharedModules<'_, '_>,
) -> Result<ModuleId, LoadingProblem> {
    match read_to_string(&filename) {
        Ok(src) => {
            let arena = Bump::new();
            let state = State::new(&src, Attempting::Module);

            // TODO figure out if there's a way to address this clippy error
            // without introducing a borrow error. ("let and return" is literally
            // what the borrow checker suggested using here to fix the problem, so...)
            #[allow(clippy::let_and_return)]
            let answer = match module::header().parse(&arena, state) {
                Ok((ast::Module::Interface { header }, state)) => {
                    let module_id = send_interface_header(header, state, module_ids, msg_tx);

                    Ok(module_id)
                }
                Ok((ast::Module::App { .. }, _)) => {
                    panic!("TODO finish loading an App module");
                }
                Err((fail, _)) => Err(LoadingProblem::ParsingFailed { filename, fail }),
            };

            answer
        }
        Err(err) => Err(LoadingProblem::FileProblem {
            filename,
            error: err.kind(),
        }),
    }
}

fn send_interface_header<'a>(
    header: InterfaceHeader<'a>,
    state: State<'a>,
    shared_modules: SharedModules<'_, '_>,
    msg_tx: MsgSender,
) -> ModuleId {
    use MaybeShared::*;

    let declared_name: ModuleName = header.name.value.as_str().into();

    // TODO check to see if declared_name is consistent with filename.
    // If it isn't, report a problem!

    let mut imports: Vec<(ModuleName, Vec<Ident>, Region)> =
        Vec::with_capacity(header.imports.len());
    let mut imported_modules: MutSet<ModuleId> = MutSet::default();
    let mut scope_size = 0;

    for loc_entry in header.imports {
        let (module_name, exposed) = exposed_from_import(&loc_entry.value);

        scope_size += exposed.len();

        imports.push((module_name, exposed, loc_entry.region));
    }

    let num_exposes = header.exposes.len();
    let mut deps_by_name: MutMap<ModuleName, ModuleId> =
        HashMap::with_capacity_and_hasher(num_exposes, default_hasher());
    let mut exposes: Vec<Symbol> = Vec::with_capacity(num_exposes);

    // Make sure the module_ids has ModuleIds for all our deps,
    // then record those ModuleIds in can_module_ids for later.
    let mut scope: MutMap<Ident, (Symbol, Region)> =
        HashMap::with_capacity_and_hasher(scope_size, default_hasher());
    let home: ModuleId;

    let ident_ids = match shared_modules {
        Shared(arc_module_ids, arc_ident_ids) => {
            // Lock just long enough to perform the minimal operations necessary.
            let mut module_ids = (*arc_module_ids).lock().expect("Failed to acquire lock for interning module IDs, presumably because a thread panicked.");
            let mut ident_ids_by_module = (*arc_ident_ids).lock().expect("Failed to acquire lock for interning ident IDs, presumably because a thread panicked.");

            home = module_ids.get_or_insert(&declared_name.as_inline_str());

            // Ensure this module has an entry in the exposed_ident_ids map.
            ident_ids_by_module
                .entry(home)
                .or_insert_with(IdentIds::default);

            // This can't possibly fail, because we just ensured it
            // has an entry with this key.
            let ident_ids = ident_ids_by_module.get_mut(&home).unwrap();
            let mut imports_to_expose = Vec::with_capacity(imports.len());

            // For each of our imports, add an entry to deps_by_name
            //
            // e.g. for `imports [ Foo.{ bar } ]`, add `Foo` to deps_by_name
            for (module_name, exposed, region) in imports.into_iter() {
                let cloned_module_name = module_name.clone();
                let module_id = module_ids.get_or_insert(&module_name.into());

                deps_by_name.insert(cloned_module_name, module_id);

                imported_modules.insert(module_id);

                imports_to_expose.push((module_id, exposed, region));
            }

            // For each of our imports, add any exposed values to scope.
            //
            // e.g. for `imports [ Foo.{ bar } ]`, add `bar` to scope.
            for (module_id, exposed, region) in imports_to_expose.into_iter() {
                if !exposed.is_empty() {
                    add_exposed_to_scope(module_id, &mut scope, exposed, ident_ids, region);
                }
            }

            // Generate IdentIds entries for all values this module exposes.
            // This way, when we encounter them in Defs later, they already
            // have an IdentIds entry.
            //
            // We must *not* add them to scope yet, or else the Defs will
            // incorrectly think they're shadowing them!
            for loc_exposed in header.exposes.iter() {
                let ident_id = ident_ids.add(loc_exposed.value.as_str().into());
                let symbol = Symbol::new(home, ident_id);

                exposes.push(symbol);
            }

            if cfg!(debug_assertions) {
                home.register_debug_idents(&ident_ids);
            }

            ident_ids.clone()
        }
        Unique(module_ids, ident_ids_by_module) => {
            // If this is the original file the user loaded,
            // then we already have a mutable reference,
            // and won't need to pay locking costs.
            home = module_ids.get_or_insert(declared_name.as_inline_str());

            let mut ident_ids = IdentIds::default();

            // For each of our imports, add it to deps_by_name,
            // and also add any exposed values to scope.
            //
            // e.g. for `imports [ Foo.{ bar } ]`, add `Foo` to deps_by_name and `bar` to scope.
            for (module_name, exposed, region) in imports.into_iter() {
                let module_id = module_ids.get_or_insert(&module_name.clone().into());

                deps_by_name.insert(module_name, module_id);

                imported_modules.insert(module_id);

                if !exposed.is_empty() {
                    add_exposed_to_scope(module_id, &mut scope, exposed, &mut ident_ids, region);
                }
            }

            // Generate IdentIds entries for all values this module exposes.
            // This way, when we encounter them in Defs later, they already
            // have an IdentIds entry.
            //
            // We must *not* add them to scope yet, or else the Defs will
            // incorrectly think they're shadowing them!
            for loc_exposed in header.exposes.iter() {
                let ident_id = ident_ids.add(loc_exposed.value.as_str().into());
                let symbol = Symbol::new(home, ident_id);

                exposes.push(symbol);
            }

            if cfg!(debug_assertions) {
                home.register_debug_idents(&ident_ids);
            }

            // Record this entry into ident_ids_by_module. It should not
            // have been recorded previously, since this was Unique.
            debug_assert!(!ident_ids_by_module.contains_key(&home));
            ident_ids_by_module.insert(home, ident_ids.clone());

            ident_ids
        }
    };

    // Box up the input &str for transfer over the wire.
    // We'll need this in order to continue parsing later.
    let src: Box<str> = state.input.to_string().into();

    // Send the deps to the coordinator thread for processing,
    // then continue on to parsing and canonicalizing defs.
    //
    // We always need to send these, even if deps is empty,
    // because the coordinator thread needs to receive this message
    // to decrement its "pending" count.
    let mut tx = msg_tx;

    tokio::spawn(async move {
        // Send the header the main thread for processing,
        tx.send(Msg::Header(ModuleHeader {
            module_id: home,
            exposed_ident_ids: ident_ids,
            module_name: declared_name,
            imported_modules,
            deps_by_name,
            exposes,
            src,
            exposed_imports: scope,
        }))
        .await
        .unwrap_or_else(|_| panic!("Failed to send Header message for module ID: {:?}", home));
    });

    home
}

fn add_exposed_to_scope(
    module_id: ModuleId,
    scope: &mut MutMap<Ident, (Symbol, Region)>,
    exposed: Vec<Ident>,
    ident_ids: &mut IdentIds,
    region: Region,
) {
    for ident in exposed {
        // Since this value is exposed, add it to our module's default scope.
        debug_assert!(!scope.contains_key(&ident.clone()));

        let ident_id = ident_ids.add(ident.clone().into());
        let symbol = Symbol::new(module_id, ident_id);

        scope.insert(ident, (symbol, region));
    }
}

// TODO trim down these arguments - possibly by moving Constraint into Module
#[allow(clippy::too_many_arguments)]
fn solve_module(
    module: Module,
    constraint: Constraint,
    var_store: VarStore,
    msg_tx: MsgSender,
    exposed_types: &mut SubsByModule,
    declarations_by_id: &mut MutMap<ModuleId, Vec<Declaration>>,
    builtins: &MutMap<Symbol, (SolvedType, Region)>,
    mut vars_by_symbol: SendMap<Symbol, Variable>,
) -> MutSet<ModuleId> /* returs a set of unused imports */ {
    let home = module.module_id;
    let mut imported_symbols = Vec::with_capacity(module.references.len());
    let mut aliases = MutMap::default();
    let mut unused_imports = module.imported_modules; // We'll remove these as we encounter them.


    // Translate referenced symbols into constraints
    for &symbol in module.references.iter() {
        let module_id = symbol.module_id();

        // We used this one, so clearly it is not unused!
        unused_imports.remove(&module_id);

        if module_id.is_builtin() {
            // For builtin modules, we create imports from the
            // hardcoded builtin map.
            match builtins.get(&symbol) {
                Some((solved_type, region)) => {
                    let loc_symbol = Located {
                        value: symbol,
                        region: *region,
                    };

                    imported_symbols.push(Import {
                        loc_symbol,
                        solved_type,
                    });
                } None => {
                    panic!("Could not find {:?} in builtins {:?}", symbol, builtins)
                }
            }
        } else if module_id != home {
            // We already have constraints for our own symbols.
            let region = Region::zero(); // TODO this should be the region where this symbol was declared in its home module. Look that up!
            let loc_symbol = Located {
                value: symbol,
                region,
            };

            match exposed_types.get(&module_id) {
                Some(ExposedModuleTypes::Valid(solved_types, new_aliases)) => {
                    let solved_type = solved_types.get(&symbol).unwrap_or_else(|| {
                        panic!(
                            "Could not find {:?} in solved_types {:?}",
                            loc_symbol.value, solved_types
                        )
                    });

                    // TODO should this be a union?
                    for (k, v) in new_aliases.clone() {
                        aliases.insert(k, v);
                    }

                    imported_symbols.push(Import {
                        loc_symbol,
                        solved_type,
                    });
                }
                Some(ExposedModuleTypes::Invalid) => {
                    // If that module was invalid, use True constraints
                    // for everything imported from it.
                    imported_symbols.push(Import {
                        loc_symbol,
                        solved_type: &SolvedType::Erroneous(types::Problem::InvalidModule),
                    });
                }
                None => {
                    panic!(
                        "Could not find module {:?} in exposed_types {:?}",
                        module_id, exposed_types
                    );
                }
            }
        }
    }

    // Wrap the existing module constraint in these imported constraints.
    let constraint = constrain_imported_values(imported_symbols, constraint, &var_store);
    let constraint = constrain_imported_aliases(aliases, constraint, &var_store);
    let constraint = load_builtin_aliases(&builtins::aliases(), constraint, &var_store);

    // All the exposed imports should be available in the solver's vars_by_symbol
    for (symbol, expr_var) in module.exposed_imports.iter() {
        vars_by_symbol.insert(*symbol, *expr_var);
    }

    // All the top-level defs should also be available in vars_by_symbol
    for decl in &module.declarations {
        use Declaration::*;

        match decl {
            Declare(def) => {
                insert_all(&mut vars_by_symbol, def.pattern_vars.clone().into_iter());
            }
            DeclareRec(defs) => {
                for def in defs {
                    insert_all(&mut vars_by_symbol, def.pattern_vars.clone().into_iter());
                }
            }
            InvalidCycle(_, _) => panic!("TODO handle invalid cycles"),
        }
    }

    declarations_by_id.insert(home, module.declarations);

    let exposed_vars_by_symbol: Vec<(Symbol, Variable)> = module.exposed_vars_by_symbol;

    let mut aliases = MutMap::default();

    for (symbol, alias) in module.aliases.iter() {
        aliases.insert(*symbol, alias.clone());
    }

    let env = solve::Env {
        vars_by_symbol,
        aliases: module.aliases,
    };

    let mut subs = Subs::new(var_store.into());
    for (var, name) in module.rigid_variables {
        subs.rigid_var(name, var);
    }

    // Start solving this module in the background.
    spawn_blocking(move || {
        // Now that the module is parsed, canonicalized, and constrained,
        // we need to type check it.
        let mut problems = Vec::new();

        // Run the solver to populate Subs.
        let (solved_subs, solved_env) = solve::run(&env, &mut problems, subs, &constraint);

        let mut solved_types = MutMap::default();

        for (symbol, alias) in solved_env.aliases.into_iter() {
            let mut args = Vec::with_capacity(alias.vars.len());

            for Located {
                value: (name, _), ..
            } in alias.vars.iter()
            {
                args.push(name.clone());
            }

            let solved_type = SolvedType::from_type(&solved_subs, alias.typ);
            let solved_alias = SolvedType::Alias(symbol, args, Box::new(solved_type));

            solved_types.insert(symbol, solved_alias);
        }

        // exposed_vars_by_symbol contains the Variables for all the Symbols
        // this module exposes. We want to convert those into flat SolvedType
        // annotations which are decoupled from our Subs, because that's how
        // other modules will generate constraints for imported values
        // within the context of their own Subs.
        for (symbol, var) in exposed_vars_by_symbol {
            let solved_type = SolvedType::new(&solved_subs, var);

            solved_types.insert(symbol, solved_type);
        }

        tokio::spawn(async move {
            let mut tx = msg_tx;

            // Send the subs to the main thread for processing,
            tx.send(Msg::Solved {
                module_id: home,
                subs: Arc::new(solved_subs),
                solved_types,
                problems,
                aliases,
            })
            .await
            .unwrap_or_else(|_| panic!("Failed to send Solved message"));
        });
    });

    unused_imports
}

fn spawn_parse_and_constrain(
    header: ModuleHeader,
    module_ids: Arc<Mutex<ModuleIds>>,
    ident_ids_by_module: Arc<Mutex<IdentIdsByModule>>,
    exposed_types: &SubsByModule,
    exposed_symbols: MutSet<Symbol>,
    waiting_for_solve: &mut MutMap<ModuleId, MutSet<ModuleId>>,
    msg_tx: MsgSender,
) {
    let module_id = header.module_id;
    let deps_by_name = &header.deps_by_name;
    let num_deps = deps_by_name.len();
    let mut dep_idents: IdentIdsByModule = IdentIds::exposed_builtins(num_deps);

    {
        let ident_ids_by_module = (*ident_ids_by_module).lock().expect(
            "Failed to acquire lock for interning ident IDs, presumably because a thread panicked.",
        );

        // Populate dep_idents with each of their IdentIds,
        // which we'll need during canonicalization to translate
        // identifier strings into IdentIds, which we need to build Symbols.
        // We only include the modules we care about (the ones we import).
        //
        // At the end of this loop, dep_idents contains all the information to
        // resolve a symbol from another module: if it's in here, that means
        // we have both imported the module and the ident was exported by that mdoule.
        for dep_id in header.deps_by_name.values() {
            // We already verified that these are all present,
            // so unwrapping should always succeed here.
            let idents = ident_ids_by_module.get(&dep_id).unwrap();

            dep_idents.insert(*dep_id, idents.clone());
        }
    }

    // Once this step has completed, the next thing we'll need
    // is solving. Register the modules we'll need to have been
    // solved before we can solve.
    let mut solve_needed = HashSet::with_capacity_and_hasher(num_deps, default_hasher());

    for dep_id in deps_by_name.values() {
        if !exposed_types.contains_key(dep_id) {
            solve_needed.insert(*dep_id);
        }
    }

    waiting_for_solve.insert(module_id, solve_needed);

    let module_ids = {
        (*module_ids).lock().expect(
            "Failed to acquire lock for obtaining module IDs, presumably because a thread panicked.",
        ).clone()
    };

    // Now that we have waiting_for_solve populated, continue parsing,
    // canonicalizing, and constraining the module.
    spawn_blocking(move || {
        parse_and_constrain(header, module_ids, dep_idents, exposed_symbols, msg_tx);
    });
}

/// Parse the module, canonicalize it, and generate constraints for it.
fn parse_and_constrain(
    header: ModuleHeader,
    module_ids: ModuleIds,
    dep_idents: IdentIdsByModule,
    exposed_symbols: MutSet<Symbol>,
    msg_tx: MsgSender,
) {
    let module_id = header.module_id;
    let var_store = VarStore::default();
    let arena = Bump::new();
    let state = State::new(&header.src, Attempting::Module);

    let (parsed_defs, _) = module_defs()
        .parse(&arena, state)
        .expect("TODO gracefully handle parse error on module defs");

    let (module, ident_ids, constraint, problems) = match canonicalize_module_defs(
        &arena,
        parsed_defs,
        module_id,
        &module_ids,
        header.exposed_ident_ids,
        dep_idents,
        header.exposed_imports,
        exposed_symbols,
        &var_store,
    ) {
        Ok(ModuleOutput {
            declarations,
            exposed_imports,
            lookups,
            ident_ids,
            exposed_vars_by_symbol,
            references,
            aliases,
            rigid_variables,
            problems,
            ..
        }) => {
            let constraint = constrain_module(module_id, &declarations, lookups);
            let module = Module {
                module_id,
                declarations,
                exposed_imports,
                exposed_vars_by_symbol,
                references,
                aliases,
                rigid_variables,
                imported_modules: header.imported_modules
            };

            (module, ident_ids, constraint, problems)
        }
        Err(_runtime_error) => {
            panic!("TODO gracefully handle module canonicalization error");
        }
    };

    tokio::spawn(async move {
        let mut tx = msg_tx;

        // Send the constraint to the main thread for processing.
        tx.send(Msg::Constrained {
            module,
            ident_ids,
            constraint,
            problems,
            var_store,
        })
        .await
        .unwrap_or_else(|_| panic!("Failed to send Constrained message"));
    });
}

fn exposed_from_import(entry: &ImportsEntry<'_>) -> (ModuleName, Vec<Ident>) {
    use crate::parse::ast::ImportsEntry::*;

    match entry {
        Module(module_name, exposes) => {
            let mut exposed = Vec::with_capacity(exposes.len());

            for loc_entry in exposes {
                exposed.push(ident_from_exposed(&loc_entry.value));
            }

            (module_name.as_str().into(), exposed)
        }

        SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => {
            // Ignore spaces.
            exposed_from_import(*sub_entry)
        }
    }
}

fn ident_from_exposed(entry: &ExposesEntry<'_>) -> Ident {
    use crate::parse::ast::ExposesEntry::*;

    match entry {
        Ident(ident) => (*ident).into(),
        SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => ident_from_exposed(sub_entry),
    }
}
