use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::{ModuleId, PackageQualified};

use std::collections::hash_map::Entry;

/// NOTE the order of definition of the phases is used by the ord instance
/// make sure they are ordered from first to last!
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub enum Phase {
    LoadHeader,
    Parse,
    CanonicalizeAndConstrain,
    SolveTypes,
    FindSpecializations,
    MakeSpecializations,
}

/// NOTE keep up to date manually, from ParseAndGenerateConstraints to the highest phase we support
const PHASES: [Phase; 6] = [
    Phase::LoadHeader,
    Phase::Parse,
    Phase::CanonicalizeAndConstrain,
    Phase::SolveTypes,
    Phase::FindSpecializations,
    Phase::MakeSpecializations,
];

#[derive(Debug)]
enum Status {
    NotStarted,
    Pending,
    Done,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Job<'a> {
    Step(ModuleId, Phase),
    ResolveShorthand(&'a str),
}

#[derive(Default, Debug)]
struct MakeSpecializationInfo {
    /// Modules to make specializations for after they are made for this module
    succ: MutSet<ModuleId>,
    /// Whether this module depends on specializations being made for another module
    has_pred: bool,
}

#[derive(Debug)]
struct MakeSpecializationsDependents(MutMap<ModuleId, MakeSpecializationInfo>);

impl MakeSpecializationsDependents {
    /// Gets the info entry for a module, or creates a default one.
    fn entry(&mut self, module_id: ModuleId) -> &mut MakeSpecializationInfo {
        self.0.entry(module_id).or_default()
    }

    fn mark_has_pred(&mut self, module_id: ModuleId) {
        self.entry(module_id).has_pred = true;
    }

    fn add_succ(&mut self, module_id: ModuleId, succ: impl IntoIterator<Item = ModuleId>) {
        // Add make specialization dependents
        let entry = self.entry(module_id);
        debug_assert!(
            entry.succ.is_empty(),
            "already added successors for this module"
        );

        entry.succ.extend(succ.into_iter());

        // The module for derives implicitly depends on every other module
        entry.succ.insert(ModuleId::DERIVED_GEN);
    }
}

impl Default for MakeSpecializationsDependents {
    fn default() -> Self {
        let mut map: MutMap<ModuleId, MakeSpecializationInfo> = Default::default();

        // The module for derives is always at the base as the last module to specialize
        map.insert(
            ModuleId::DERIVED_GEN,
            MakeSpecializationInfo {
                succ: Default::default(),
                // NB: invariant - the derived module depends on every other module, and
                // work can never be initiated for just the derived module!
                has_pred: true,
            },
        );

        Self(map)
    }
}

#[derive(Debug)]
pub struct Dependencies<'a> {
    waiting_for: MutMap<Job<'a>, MutSet<Job<'a>>>,
    notifies: MutMap<Job<'a>, MutSet<Job<'a>>>,
    status: MutMap<Job<'a>, Status>,

    make_specializations_dependents: MakeSpecializationsDependents,
}

impl<'a> Dependencies<'a> {
    pub fn new(goal_phase: Phase) -> Self {
        let mut deps = Self {
            waiting_for: Default::default(),
            notifies: Default::default(),
            status: Default::default(),
            make_specializations_dependents: Default::default(),
        };

        if goal_phase >= Phase::MakeSpecializations {
            // Module for deriving is always implicitly loaded into the work graph, but it only
            // comes into play for make specializations.
            deps.add_to_status_for_phase(ModuleId::DERIVED_GEN, Phase::MakeSpecializations);
        }

        deps
    }

    /// Add all the dependencies for a module, return (module, phase) pairs that can make progress
    pub fn add_module(
        &mut self,
        module_id: ModuleId,
        dependencies: &MutSet<PackageQualified<'a, ModuleId>>,
        goal_phase: Phase,
    ) -> MutSet<(ModuleId, Phase)> {
        use Phase::*;

        let mut output = MutSet::default();

        for dep in dependencies.iter() {
            let has_package_dependency = self.add_package_dependency(dep, Phase::LoadHeader);

            let dep = *dep.as_inner();

            if !has_package_dependency {
                // loading can start immediately on this dependency
                output.insert((dep, Phase::LoadHeader));
            }

            // to parse and generate constraints, the headers of all dependencies must be loaded!
            // otherwise, we don't know whether an imported symbol is actually exposed
            self.add_dependency_help(module_id, dep, Phase::Parse, Phase::LoadHeader);

            // to canonicalize a module, all its dependencies must be canonicalized
            self.add_dependency(module_id, dep, Phase::CanonicalizeAndConstrain);

            // to typecheck a module, all its dependencies must be type checked already
            self.add_dependency(module_id, dep, Phase::SolveTypes);

            if goal_phase >= FindSpecializations {
                self.add_dependency(module_id, dep, Phase::FindSpecializations);
            }

            if goal_phase >= MakeSpecializations {
                self.add_dependency(dep, module_id, Phase::MakeSpecializations);
                // The module for derives implicitly depends on every other module
                self.add_dependency(ModuleId::DERIVED_GEN, module_id, Phase::MakeSpecializations);

                // `dep` depends on `module_id` making specializations first
                self.make_specializations_dependents.mark_has_pred(dep);
            }
        }

        if goal_phase >= MakeSpecializations {
            // Add make specialization dependents
            self.make_specializations_dependents
                .add_succ(module_id, dependencies.iter().map(|dep| *dep.as_inner()));
        }

        // add dependencies for self
        // phase i + 1 of a file always depends on phase i being completed
        {
            let mut i = 0;
            while PHASES[i] < goal_phase {
                self.add_dependency_help(module_id, module_id, PHASES[i + 1], PHASES[i]);
                i += 1;
            }
        }

        self.add_to_status_for_all_phases(module_id, goal_phase);

        output
    }

    /// Adds a status for the given module for exactly one phase.
    fn add_to_status_for_phase(&mut self, module_id: ModuleId, phase: Phase) {
        if let Entry::Vacant(entry) = self.status.entry(Job::Step(module_id, phase)) {
            entry.insert(Status::NotStarted);
        }
    }

    /// Adds a status for the given module for all phases up to and including the goal phase.
    fn add_to_status_for_all_phases(&mut self, module_id: ModuleId, goal_phase: Phase) {
        for phase in PHASES.iter() {
            if *phase > goal_phase {
                break;
            }

            self.add_to_status_for_phase(module_id, *phase);
        }
    }

    /// Propagate a notification, return (module, phase) pairs that can make progress
    pub fn notify(&mut self, module_id: ModuleId, phase: Phase) -> MutSet<(ModuleId, Phase)> {
        self.notify_help(Job::Step(module_id, phase))
    }

    /// Propagate a notification, return (module, phase) pairs that can make progress
    pub fn notify_package(&mut self, shorthand: &'a str) -> MutSet<(ModuleId, Phase)> {
        self.notify_help(Job::ResolveShorthand(shorthand))
    }

    fn notify_help(&mut self, key: Job<'a>) -> MutSet<(ModuleId, Phase)> {
        self.status.insert(key.clone(), Status::Done);

        let mut output = MutSet::default();

        if let Some(to_notify) = self.notifies.get(&key) {
            for notify_key in to_notify {
                let mut is_empty = false;
                if let Some(waiting_for_pairs) = self.waiting_for.get_mut(notify_key) {
                    waiting_for_pairs.remove(&key);
                    is_empty = waiting_for_pairs.is_empty();
                }

                if is_empty {
                    self.waiting_for.remove(notify_key);

                    if let Job::Step(module, phase) = *notify_key {
                        output.insert((module, phase));
                    }
                }
            }
        }

        self.notifies.remove(&key);

        output
    }

    fn add_package_dependency(
        &mut self,
        module: &PackageQualified<'a, ModuleId>,
        next_phase: Phase,
    ) -> bool {
        match module {
            PackageQualified::Unqualified(_) => {
                // no dependency, we can just start loading the file
                false
            }
            PackageQualified::Qualified(shorthand, module_id) => {
                let job = Job::ResolveShorthand(shorthand);
                let next_step = Job::Step(*module_id, next_phase);
                match self.status.get(&job) {
                    None | Some(Status::NotStarted) | Some(Status::Pending) => {
                        // this shorthand is not resolved, add a dependency
                        {
                            let entry = self
                                .waiting_for
                                .entry(next_step.clone())
                                .or_insert_with(Default::default);

                            entry.insert(job.clone());
                        }

                        {
                            let entry = self.notifies.entry(job).or_insert_with(Default::default);

                            entry.insert(next_step);
                        }

                        true
                    }
                    Some(Status::Done) => {
                        // shorthand is resolved; no dependency
                        false
                    }
                }
            }
        }
    }

    /// A waits for B, and B will notify A when it completes the phase
    fn add_dependency(&mut self, a: ModuleId, b: ModuleId, phase: Phase) {
        self.add_dependency_help(a, b, phase, phase);
    }

    /// phase_a of module a is waiting for phase_b of module_b
    fn add_dependency_help(&mut self, a: ModuleId, b: ModuleId, phase_a: Phase, phase_b: Phase) {
        // no need to wait if the dependency is already done!
        if let Some(Status::Done) = self.status.get(&Job::Step(b, phase_b)) {
            return;
        }

        let key = Job::Step(a, phase_a);
        let value = Job::Step(b, phase_b);
        match self.waiting_for.get_mut(&key) {
            Some(existing) => {
                existing.insert(value);
            }
            None => {
                let mut set = MutSet::default();
                set.insert(value);
                self.waiting_for.insert(key, set);
            }
        }

        let key = Job::Step(b, phase_b);
        let value = Job::Step(a, phase_a);
        match self.notifies.get_mut(&key) {
            Some(existing) => {
                existing.insert(value);
            }
            None => {
                let mut set = MutSet::default();
                set.insert(value);
                self.notifies.insert(key, set);
            }
        }
    }

    pub fn solved_all(&self) -> bool {
        debug_assert_eq!(self.notifies.is_empty(), self.waiting_for.is_empty());

        for status in self.status.values() {
            match status {
                Status::Done => {
                    continue;
                }
                _ => {
                    return false;
                }
            }
        }

        true
    }

    pub fn prepare_start_phase(&mut self, module_id: ModuleId, phase: Phase) -> PrepareStartPhase {
        match self.status.get_mut(&Job::Step(module_id, phase)) {
            Some(current @ Status::NotStarted) => {
                // start this phase!
                *current = Status::Pending;
                PrepareStartPhase::Continue
            }
            Some(Status::Pending) => {
                // don't start this task again!
                PrepareStartPhase::Done
            }
            Some(Status::Done) => {
                // don't start this task again, but tell those waiting for it they can continue
                let new = self.notify(module_id, phase);

                PrepareStartPhase::Recurse(new)
            }
            None => match phase {
                Phase::LoadHeader => {
                    // this is fine, mark header loading as pending
                    self.status
                        .insert(Job::Step(module_id, Phase::LoadHeader), Status::Pending);

                    PrepareStartPhase::Continue
                }
                _ => unreachable!(
                    "Pair {:?} is not in dependencies.status, that should never happen!",
                    (module_id, phase)
                ),
            },
        }
    }

    /// Load the entire "make specializations" dependency graph and start from the top.
    pub fn reload_make_specialization_pass(&mut self) -> MutSet<(ModuleId, Phase)> {
        let mut output = MutSet::default();

        let mut make_specializations_dependents = MakeSpecializationsDependents::default();
        let default_make_specializations_dependents_len = make_specializations_dependents.0.len();
        std::mem::swap(
            &mut self.make_specializations_dependents,
            &mut make_specializations_dependents,
        );

        for (&module, _) in make_specializations_dependents.0.iter() {
            let job = Job::Step(module, Phase::MakeSpecializations);
            let status = self.status.get_mut(&job).unwrap();
            debug_assert!(
                matches!(status, Status::Done),
                "all previous make specializations should be done before reloading"
            );
            *status = Status::NotStarted;
        }

        // `add_dependency` borrows self as mut so we move `make_specializations_dependents` out
        // for our local use. `add_dependency` should never grow the make specializations
        // dependency graph.
        for (&module, MakeSpecializationInfo { succ, has_pred }) in
            make_specializations_dependents.0.iter()
        {
            for &dependent in succ {
                self.add_dependency(dependent, module, Phase::MakeSpecializations);
            }

            self.add_to_status_for_phase(module, Phase::MakeSpecializations);
            if !has_pred {
                output.insert((module, Phase::MakeSpecializations));
            }
        }

        std::mem::swap(
            &mut self.make_specializations_dependents,
            &mut make_specializations_dependents,
        );
        debug_assert_eq!(
            make_specializations_dependents.0.len(),
            default_make_specializations_dependents_len,
            "more modules were added to the graph: {:?}",
            make_specializations_dependents
        );

        output
    }
}

pub enum PrepareStartPhase {
    Continue,
    Done,
    Recurse(MutSet<(ModuleId, Phase)>),
}
