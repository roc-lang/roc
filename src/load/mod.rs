use crate::can::def::Declaration;
use crate::can::ident::ModuleName;
use crate::can::module::{canonicalize_module_defs, Module, ModuleOutput};
use crate::can::scope::Scope;
use crate::module::symbol::Symbol;
use crate::collections::{default_hasher, insert_all, ImMap, MutMap, MutSet, SendMap};
use crate::constrain::module::constrain_module;
use crate::ident::Ident;
use crate::module::header;
use crate::module::symbol::{IdentId, IdentIds, ModuleId, ModuleIds};
use crate::parse::ast::{self, Attempting, ExposesEntry, ImportsEntry, InterfaceHeader};
use crate::parse::module::{self, module_defs};
use crate::parse::parser::{Fail, Parser, State};
use crate::region::Region;
use crate::solve::{self, ModuleSubs, Solved};
use crate::subs::{Subs, VarStore, Variable};
use crate::types::{Constraint, Problem};
use bumpalo::Bump;
use inlinable_string::InlinableString;
use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use tokio::sync::mpsc;
use tokio::task::spawn_blocking;

/// TODO change solve::SubsByModule to be this
type SubsByModule = MutMap<ModuleId, ModuleSubs>;

/// Filename extension for normal Roc modules
const ROC_FILE_EXTENSION: &str = "roc";

/// The . in between module names like Foo.Bar.Baz
const MODULE_SEPARATOR: char = '.';

pub struct LoadedModule {
    pub module_id: ModuleId,
    pub module_ids: ModuleIds,
    pub solved: Solved<Subs>,
    pub problems: Vec<Problem>,
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
    deps_by_id: Vec<(ModuleId, ModuleName)>,
    exposes: Vec<InlinableString>,
    scope: MutMap<Ident, (Symbol, Region)>,
    src: Box<str>,
}

#[derive(Debug)]
enum Msg {
    Header(ModuleHeader),
    Constrained {
        module: Module,
        constraint: Constraint,
        next_var: Variable,
    },
    Solved {
        module_id: ModuleId,
        subs: Arc<Solved<Subs>>,
        problems: Vec<Problem>,
        new_vars_by_symbol: SendMap<Symbol, Variable>,
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

enum MaybeShared<'a, T> {
    Shared(Arc<Mutex<T>>),
    Unique(&'a mut T),
}

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
    subs_by_module: &mut SubsByModule,
) -> Result<LoadedModule, LoadingProblem> {
    use self::MaybeShared::*;

    let mut all_problems = Vec::new();
    let env = Env {
        src_dir: src_dir.clone(),
    };

    let (msg_tx, mut msg_rx): (MsgSender, MsgReceiver) = mpsc::channel(1024);
    let mut module_ids = ModuleIds::default();

    // Load the root module synchronously; we can't proceed until we have its id.
    let root_id = load_filename(&env, filename, msg_tx.clone(), Unique(&mut module_ids))?;

    // From now on, this will be used by multiple threads; time to make it an Arc<Mutex<_>>!
    let module_ids = Arc::new(Mutex::new(module_ids));

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

    let mut exposed_idents_by_module: MutMap<ModuleId, Arc<(MutSet<IdentId>, IdentIds)>> =
        MutMap::default();

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

    let mut unsolved_modules: MutMap<ModuleId, (Module, Constraint, Variable)> = MutMap::default();
    let mut vars_by_symbol = SendMap::default();

    // Parse and canonicalize the module's deps
    while let Some(msg) = msg_rx.recv().await {
        use self::Msg::*;

        match msg {
            Header(header) => {
                let mut headers_needed =
                    HashSet::with_capacity_and_hasher(header.deps_by_id.len(), default_hasher());

                for (dep_id, _) in header.deps_by_id.iter() {
                    if !exposed_idents_by_module.contains_key(&dep_id) {
                        headers_needed.insert(*dep_id);
                    }
                }

                // This was a dependency. Write it down and keep processing messaages.
                let mut ident_ids = IdentIds::default();
                let mut exposed_ids = MutSet::default();

                for ident in header.exposes.iter() {
                    let exposed_id = ident_ids.get_or_insert_id(&ident);

                    exposed_ids.insert(exposed_id);
                }

                let module_id = header.module_id;

                debug_assert!(!exposed_idents_by_module.contains_key(&module_id));
                exposed_idents_by_module.insert(module_id, Arc::new((exposed_ids, ident_ids)));

                // Notify all the listeners that headers are now available for this.
                if let Some(listeners) = header_listeners.remove(&module_id) {
                    for listener_id in listeners {
                        // This listener is longer waiting for this module,
                        // because this module's headers are now available!
                        let waiting_for = waiting_for_headers
                            .get_mut(&listener_id)
                            .expect("Unable to find module ID in waiting_for_headers");

                        waiting_for.remove(&module_id);

                        // If it's no longer waiting for anything else, solve it.
                        if waiting_for.is_empty() {
                            let header = unparsed_modules
                                .remove(&listener_id)
                                .expect("Could not find listener ID in unparsed_modules");

                            begin_parse_and_constrain(
                                header,
                                &exposed_idents_by_module,
                                subs_by_module,
                                &mut waiting_for_solve,
                                msg_tx.clone(),
                            )
                        }
                    }
                }

                // If any of our deps weren't loaded before, start loading them.
                for (dep_id, dep_name) in header.deps_by_id.iter() {
                    if !loading_started.contains(&dep_id) {
                        // Record that we've started loading the module *before*
                        // we actually start loading it.
                        loading_started.insert(*dep_id);

                        let env = env.clone();
                        let msg_tx = msg_tx.clone();
                        let shared_module_ids = Shared(Arc::clone(&module_ids));
                        let dep_name = dep_name.clone();

                        // Start loading this module in the background.
                        spawn_blocking(move || {
                            load_module(env, dep_name, msg_tx, shared_module_ids)
                        });
                    }
                }

                if headers_needed.is_empty() {
                    begin_parse_and_constrain(
                        header,
                        &exposed_idents_by_module,
                        subs_by_module,
                        &mut waiting_for_solve,
                        msg_tx.clone(),
                    )
                } else {
                    // We will have to wait for our deps' headers to be parsed,
                    // so we can access their IdentId, which we need for canonicalization.
                    debug_assert!(!unparsed_modules.contains_key(&module_id));
                    unparsed_modules.insert(module_id, header);

                    // Register a listener with each of these.
                    for dep_id in headers_needed.iter() {
                        let listeners = header_listeners
                            .entry(*dep_id)
                            .or_insert_with(|| Vec::with_capacity(1));

                        (*listeners).push(module_id);
                    }

                    debug_assert!(!waiting_for_headers.contains_key(&module_id));
                    waiting_for_headers.insert(module_id, headers_needed);
                }
            }
            Constrained {
                module,
                constraint,
                next_var,
            } => {
                let module_id = module.module_id;
                let waiting_for = waiting_for_solve.get_mut(&module_id).unwrap_or_else(|| {
                    panic!(
                        "Could not find module ID {:?} in waiting_for_solve",
                        module_id
                    )
                });

                // It's possible that some modules have been solved since
                // we began waiting for them. Remove those from waiting_for,
                // because we no longer need to wait for them!
                waiting_for.retain(|id| !subs_by_module.contains_key(id));

                if waiting_for.is_empty() {
                    // All of our dependencies have already been solved. Great!
                    // That means we can proceed directly to solving.
                    solve_module(
                        module,
                        constraint,
                        next_var,
                        msg_tx.clone(),
                        subs_by_module,
                        Arc::clone(&module_ids),
                        &mut declarations_by_id,
                        vars_by_symbol.clone(),
                    );
                } else {
                    // We will have to wait for our dependencies to be solved.
                    debug_assert!(!unsolved_modules.contains_key(&module_id));
                    unsolved_modules.insert(module_id, (module, constraint, next_var));

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
                subs,
                problems,
                new_vars_by_symbol,
            } => {
                all_problems.extend(problems);

                if module_id == root_id {
                    // Once we've solved the originally requested module, we're done!
                    msg_rx.close();

                    let module_ids = Arc::try_unwrap(module_ids)
                        .unwrap_or_else(|_| {
                            panic!("There were still outstanding Arc references to module_ids")
                        })
                        .into_inner()
                        .expect("Unwrapping mutex for module_ids");

                    let solved = Arc::try_unwrap(subs).unwrap_or_else(|_| {
                        panic!("There were still outstanding Arc references to Solved<Subs>")
                    });

                    let declarations = declarations_by_id
                        .remove(&module_id)
                        .expect("declarations_by_id was missing root module_id entry");

                    return Ok(LoadedModule {
                        module_id: root_id,
                        module_ids,
                        solved,
                        problems: all_problems,
                        declarations,
                    });
                } else {
                    // This was a dependency. Write it down and keep processing messaages.
                    vars_by_symbol = vars_by_symbol.union(new_vars_by_symbol);

                    debug_assert!(!subs_by_module.contains_key(&module_id));
                    subs_by_module.insert(module_id, ModuleSubs::Valid(subs));

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
                                let (module, constraint, next_var) = unsolved_modules
                                    .remove(&listener_id)
                                    .expect("Could not find listener ID in unsolved_modules");

                                solve_module(
                                    module,
                                    constraint,
                                    next_var,
                                    msg_tx.clone(),
                                    subs_by_module,
                                    Arc::clone(&module_ids),
                                    &mut declarations_by_id,
                                    vars_by_symbol.clone(),
                                );
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
    module_ids: MaybeShared<'_, ModuleIds>,
) -> Result<ModuleId, LoadingProblem> {
    let mut filename = PathBuf::new();

    filename.push(env.src_dir.clone());

    // Convert dots in module name to directories
    for part in module_name.as_str().split(MODULE_SEPARATOR) {
        filename.push(part);
    }

    // End with .roc
    filename.set_extension(ROC_FILE_EXTENSION);

    load_filename(&env, filename, msg_tx, module_ids)
}

/// Load a module by its filename
fn load_filename(
    env: &Env,
    filename: PathBuf,
    msg_tx: MsgSender,
    module_ids: MaybeShared<'_, ModuleIds>,
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
                    let module_id = send_interface_header(env, header, state, module_ids, msg_tx);

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
    env: &Env,
    header: InterfaceHeader<'a>,
    state: State<'a>,
    module_ids: MaybeShared<'_, ModuleIds>,
    msg_tx: MsgSender,
) -> ModuleId {
    use MaybeShared::*;

    let declared_name: ModuleName = header.name.value.as_str().into();

    // TODO check to see if declared_name is consistent with filename.
    // If it isn't, report a problem!

    let mut scope_from_imports = MutMap::default();
    let mut deps = Vec::with_capacity(header.imports.len());

    for loc_entry in header.imports {
        deps.push(load_import(
            env,
            loc_entry.region,
            &loc_entry.value,
            &mut scope_from_imports,
        ));
    }

    // Canonicalization will use this map to resolve names into
    // ModuleIds without having to wait for a lock every time
    // it looks something up. We know the module will only
    // be able to reference modules it has imported, so this
    // list should cover every valid lookup it performs.
    let mut can_module_ids: MutMap<ModuleName, ModuleId> =
        HashMap::with_capacity_and_hasher(deps.len(), default_hasher());
    let mut deps_by_id: Vec<(ModuleId, ModuleName)> = Vec::with_capacity(header.exposes.len());
    let mut exposes = Vec::with_capacity(header.exposes.len());

    for loc_exposed in header.exposes.iter() {
        exposes.push(loc_exposed.value.as_str().into());
    }

    // Make sure the module_ids has ModuleIds for all our deps,
    // then record those ModuleIds in can_module_ids for later.
    let module_id;

    match module_ids {
        Shared(arc) => {
            // Lock just long enough to perform these operations.
            let mut unlocked = (*arc).lock().expect("Failed to acquire lock for interning module IDs, presumably because a thread panicked.");

            module_id = unlocked.get_or_insert_id(declared_name.as_inline_str());

            for dep in deps.iter() {
                let id = unlocked.get_or_insert_id(dep.into());

                can_module_ids.insert(dep.clone(), id);
                deps_by_id.push((id, dep.clone()));
            }
        }

        Unique(mut_ref) => {
            // If this is the original file the user loaded,
            // then we already have a mutable reference,
            // and won't need to pay locking costs.
            module_id = mut_ref.get_or_insert_id(declared_name.as_inline_str());

            for dep in deps.iter() {
                let id = mut_ref.get_or_insert_id(dep.into());

                can_module_ids.insert(dep.clone(), id);
                deps_by_id.push((id, dep.clone()));
            }
        }
    }

    // Insert our own module_id into the can_module_ids.
    // (It's already in the shared module_ids, but we insert
    // this after the lock has expired.)
    can_module_ids.insert(declared_name.clone(), module_id);

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
            module_id,
            module_name: declared_name,
            deps_by_id,
            exposes,
            src,
            scope: scope_from_imports,
        }))
        .await
        .unwrap_or_else(|_| {
            panic!(
                "Failed to send Header message for module ID: {:?}",
                module_id
            )
        });
    });

    module_id
}

// TODO trim down these arguments - possibly by moving Constraint into Module
#[allow(clippy::too_many_arguments)]
fn solve_module(
    module: Module,
    constraint: Constraint,
    next_var: Variable,
    msg_tx: MsgSender,
    subs_by_module: &mut SubsByModule,
    module_ids: Arc<Mutex<ModuleIds>>,
    declarations_by_id: &mut MutMap<ModuleId, Vec<Declaration>>,
    mut vars_by_symbol: SendMap<Symbol, Variable>,
) {
    // TODO change solve::run to expect SubsById to be keyed on ModuleId instead of doing this
    // conversion!
    let subs_by_module = {
        let mut converted = MutMap::default();
        let unlocked = (*module_ids).lock().expect("Could not lock module_ids");

        for (module_id, v) in subs_by_module {
            let module_name: ModuleName = unlocked
                .get_name(*module_id)
                .unwrap_or_else(|| {
                    panic!(
                        "Could not find module name for {:?} in {:?}",
                        module_id, unlocked
                    )
                })
                .clone()
                .into();

            converted.insert(module_name, v.clone());
        }

        converted
    };

    // All the exposed imports should be available in the solver's vars_by_symbol
    for (symbol, expr_var) in im::HashMap::clone(&module.exposed_imports) {
        vars_by_symbol.insert(symbol, expr_var);
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

    let module_id = module.module_id;

    declarations_by_id.insert(module.module_id, module.declarations);

    // Start solving this module in the background.
    spawn_blocking(move || {
        // Now that the module is parsed, canonicalized, and constrained,
        // we just need to type check it.
        //
        // We'll use a fresh Subs for this, because we're starting from
        // other modules' Subs plus the variables we've generated during
        // our own canonicalization.
        let subs = Subs::new(next_var);

        let mut problems = Vec::new();

        // Run the solver to populate Subs.
        let (solved, new_vars_by_symbol) = solve::run(
            &vars_by_symbol,
            subs_by_module,
            &mut problems,
            subs,
            &constraint,
        );

        tokio::spawn(async move {
            let mut tx = msg_tx;

            // Send the subs to the main thread for processing,
            tx.send(Msg::Solved {
                module_id,
                subs: Arc::new(solved),
                new_vars_by_symbol,
                problems,
            })
            .await
            .unwrap_or_else(|_| panic!("Failed to send Solved message"));
        });
    });
}

/// Parse the module, canonicalize it, and generate constraints for it.
fn parse_and_constrain(
    header: ModuleHeader,
    _idents_by_module: MutMap<ModuleId, Arc<(MutSet<IdentId>, IdentIds)>>,
    msg_tx: MsgSender,
) {
    let mut im_scope = ImMap::default();

    for (k, v) in header.scope {
        im_scope.insert(k, v);
    }

    let module_name = header.module_name;
    let module_id = header.module_id;
    let scope_prefix = format!("{}.", module_name.as_str()).into();
    let mut scope = Scope::new(module_name.clone(), scope_prefix, im_scope);

    let var_store = VarStore::default();
    let arena = Bump::new();
    let state = State::new(&header.src, Attempting::Module);

    let (parsed_defs, _) = module_defs()
        .parse(&arena, state)
        .expect("TODO gracefully handle parse error on module defs");

    let (declarations, exposed_imports, constraint) = match canonicalize_module_defs(
        &arena,
        parsed_defs,
        module_name.clone(),
        header.exposes,
        &mut scope,
        &var_store,
    ) {
        Ok(ModuleOutput {
            declarations,
            exposed_imports,
            lookups,
        }) => {
            let constraint = constrain_module(module_name, &declarations, lookups);

            (declarations, exposed_imports, constraint)
        }
        Err(_runtime_error) => {
            panic!("TODO gracefully handle module canonicalization error");
        }
    };

    let next_var = var_store.into();
    let module = Module {
        module_id,
        declarations,
        exposed_imports,
    };

    tokio::spawn(async move {
        let mut tx = msg_tx;

        // Send the constraint to the main thread for processing.
        tx.send(Msg::Constrained {
            module,
            constraint,
            next_var,
        })
        .await
        .unwrap_or_else(|_| panic!("Failed to send Constrained message"));
    });
}

fn load_import(
    env: &Env,
    region: Region,
    entry: &ImportsEntry<'_>,
    scope: &mut MutMap<Ident, (Symbol, Region)>,
) -> ModuleName {
    use crate::parse::ast::ImportsEntry::*;

    match entry {
        Module(module_name, exposes) => {
            for loc_entry in exposes {
                let (key, value) = expose(*module_name, &loc_entry.value, loc_entry.region);

                scope.insert(Ident::Unqualified(key.as_str().into()), value);
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
    module_name: header::ModuleName<'_>,
    entry: &ExposesEntry<'_>,
    region: Region,
) -> (ModuleName, (Symbol, Region)) {
    use crate::parse::ast::ExposesEntry::*;

    match entry {
        Ident(ident) => {
            // Since this value is exposed, add it to our module's default scope.
            let symbol = Symbol::from_module(&module_name, ident);

            ((*ident).into(), (symbol, region))
        }
        SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => {
            // Ignore spaces.
            expose(module_name, *sub_entry, region)
        }
    }
}

fn begin_parse_and_constrain(
    header: ModuleHeader,
    exposed_idents_by_module: &MutMap<ModuleId, Arc<(MutSet<IdentId>, IdentIds)>>,
    subs_by_module: &SubsByModule,
    waiting_for_solve: &mut MutMap<ModuleId, MutSet<ModuleId>>,
    msg_tx: MsgSender,
) {
    let module_id = header.module_id;
    let deps_by_id = &header.deps_by_id;
    let num_deps = deps_by_id.len();
    let mut dep_idents = HashMap::with_capacity_and_hasher(num_deps, default_hasher());

    // Populate dep_idents with each of their IdentIds,
    // which we'll need during canonicalization to translate
    // identifier strings into IdentIds, which we need to build Symbols.
    for (dep_id, _) in header.deps_by_id.iter() {
        // We already verified that these are all present,
        // so unwrapping should always succeed here.
        let idents = exposed_idents_by_module.get(&dep_id).unwrap();

        dep_idents.insert(*dep_id, Arc::clone(idents));
    }

    // Once this step has completed, the next thing we'll need
    // is solving. Register the modules we'll need to have been
    // solved before we can solve.
    let mut solve_needed = HashSet::with_capacity_and_hasher(num_deps, default_hasher());

    for (dep_id, _) in deps_by_id.iter() {
        if !subs_by_module.contains_key(dep_id) {
            solve_needed.insert(*dep_id);
        }
    }

    waiting_for_solve.insert(module_id, solve_needed);

    // Now that we have waiting_for_solve populated, continue parsing,
    // canonicalizing, and constraining the module.
    spawn_blocking(move || {
        parse_and_constrain(header, dep_idents, msg_tx);
    });
}
