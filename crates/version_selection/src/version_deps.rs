use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::hash::Hash;

pub struct Deps<Pkg, Ver> {
    exact_versions: HashMap<Pkg, Ver>,
    indirect_deps: HashMap<(Pkg, Ver), HashMap<Pkg, Ver>>,
    exclusions: HashMap<Pkg, HashSet<Ver>>,
}

impl<Pkg: Ord + Clone + Copy + Eq + Hash, Ver: Ord + Clone + Copy + Eq + Hash> Deps<Pkg, Ver> {
    /// Root modules always require exact versions for all their dependencies.
    pub fn from_root(
        root_deps: impl IntoIterator<Item = (Pkg, Ver)>,
        exclusions: impl IntoIterator<Item = (Pkg, Ver)>,
    ) -> Self {
        let exact_versions = root_deps.into_iter().collect();
        let mut exclusion_map = HashMap::default();

        for (pkg, version) in exclusions {
            exclusion_map
                .entry(pkg)
                .and_modify(|set: &mut HashSet<Ver>| {
                    set.insert(version);
                })
                .or_insert_with(|| std::iter::once(version).collect());
        }

        Self {
            exact_versions,
            exclusions: exclusion_map,
            indirect_deps: HashMap::default(),
        }
    }

    /// Record that a package depends on (at least) these versions of other packages.
    /// (If we've already recorded this package, this is a no-op and we don't use
    /// the iterator.)
    pub fn pkg_depends_on(
        &mut self,
        pkg: Pkg,
        ver: Ver,
        deps: impl IntoIterator<Item = (Pkg, Ver)>,
    ) {
        self.indirect_deps
            .entry((pkg, ver))
            .or_insert_with(|| deps.into_iter().collect());
    }

    /// Consume this and return an exact version to use for each package in the dep tree.
    ///
    /// Solving can fail, because the root's direct dependencies must solve to exactly those
    /// versions. This can cause conflicts with indirect dependencies, which may also require
    /// those same packages, but higher versions than what the root wants. Excluded package
    /// versions can also lead solving to fail.
    ///
    /// If solving fails, this returns a Set of the packages that couldn't be solved.
    /// (Detecting dependency cycles should be done separately.)
    pub fn select_versions(self) -> Result<HashMap<Pkg, Ver>, HashSet<Pkg>> {
        use std::collections::{BTreeMap, BTreeSet, VecDeque};

        // Use BTreeMap and BTreeSet for deterministic behavior
        let mut selected_versions: BTreeMap<Pkg, Ver> = BTreeMap::new();
        let mut failed_pkgs: BTreeSet<Pkg> = BTreeSet::new();

        // Initialize the pending queue with the exact_versions
        let mut pending: VecDeque<(Pkg, Ver)> = VecDeque::new();

        for (pkg, ver) in &self.exact_versions {
            selected_versions.insert(pkg.clone(), ver.clone());
            pending.push_back((pkg.clone(), ver.clone()));
        }

        let mut processed: BTreeSet<(Pkg, Ver)> = BTreeSet::new();

        while let Some((pkg, ver)) = pending.pop_front() {
            // Check for exclusions
            if let Some(excluded_versions) = self.exclusions.get(&pkg) {
                if excluded_versions.contains(&ver) {
                    failed_pkgs.insert(pkg.clone());
                    continue;
                }
            }

            // Mark this (pkg, ver) as processed
            if !processed.insert((pkg.clone(), ver.clone())) {
                continue;
            }

            // Get dependencies of (pkg, ver)
            if let Some(deps) = self.indirect_deps.get(&(pkg.clone(), ver.clone())) {
                for (dep_pkg, dep_ver) in deps {
                    // Check for exclusions
                    if let Some(excluded_versions) = self.exclusions.get(dep_pkg) {
                        if excluded_versions.contains(dep_ver) {
                            failed_pkgs.insert(dep_pkg.clone());
                            continue;
                        }
                    }

                    let selected_ver = selected_versions.get(dep_pkg);
                    if let Some(selected_ver) = selected_ver {
                        if dep_ver > selected_ver {
                            // Update to higher version
                            selected_versions.insert(dep_pkg.clone(), dep_ver.clone());
                            pending.push_back((dep_pkg.clone(), dep_ver.clone()));
                        }
                    } else {
                        // Select this version
                        selected_versions.insert(dep_pkg.clone(), dep_ver.clone());
                        pending.push_back((dep_pkg.clone(), dep_ver.clone()));
                    }
                }
            }
        }

        // Check if selected_versions satisfy exact_versions
        for (pkg, ver) in &self.exact_versions {
            if selected_versions.get(pkg) != Some(ver) {
                failed_pkgs.insert(pkg.clone());
            }
        }

        if failed_pkgs.is_empty() {
            Ok(selected_versions.into_iter().collect())
        } else {
            Err(failed_pkgs.into_iter().collect())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_root() {
        let deps = Deps::from_root([("a", 1), ("b", 2)], []);
        assert_eq!(deps.exact_versions.len(), 2);
        assert_eq!(deps.exact_versions["a"], 1);
        assert_eq!(deps.exact_versions["b"], 2);
    }

    #[test]
    fn test_pkg_depends_on() {
        let mut deps = Deps::from_root([("a", 1)], []);
        deps.pkg_depends_on("a", 1, [("b", 2), ("c", 3)]);
        deps.pkg_depends_on("b", 2, [("d", 4)]);

        assert_eq!(deps.indirect_deps.len(), 2);
        assert_eq!(deps.indirect_deps[&("a", 1)].len(), 2);
        assert_eq!(deps.indirect_deps[&("b", 2)].len(), 1);
    }

    #[test]
    fn test_select_versions_simple() {
        let mut deps = Deps::from_root([("a", 1)], []);
        deps.pkg_depends_on("a", 1, [("b", 2)]);
        let result = deps.select_versions().unwrap();

        assert_eq!(result.len(), 2);
        assert_eq!(result["a"], 1);
        assert_eq!(result["b"], 2);
    }

    #[test]
    fn test_select_versions_incompatible() {
        let mut deps = Deps::from_root([("a", 1), ("b", 2)], []);
        deps.pkg_depends_on("a", 1, [("b", 3)]);
        let bad_pkgs = deps.select_versions().unwrap_err();

        assert_eq!(bad_pkgs, std::iter::once("b").collect());
    }

    #[test]
    fn test_select_versions_multiple_incompatibilities() {
        let mut deps = Deps::from_root([("a", 1), ("b", 2), ("c", 3)], []);
        deps.pkg_depends_on("a", 1, [("b", 3), ("c", 4)]);
        deps.pkg_depends_on("b", 2, [("c", 1)]);
        deps.pkg_depends_on("c", 3, [("d", 4)]);
        let bad_pkgs = deps.select_versions().unwrap_err();

        assert_eq!(bad_pkgs, ["b", "c"].into_iter().collect())
    }

    #[test]
    fn test_select_versions_complex_scenario() {
        let mut deps = Deps::from_root([("a", 1), ("b", 1)], []);
        deps.pkg_depends_on("a", 1, [("c", 2), ("d", 1)]);
        deps.pkg_depends_on("b", 1, [("c", 3), ("e", 1)]); // means we'll select c@3
        deps.pkg_depends_on("c", 2, [("f", 4)]); // won't be selected bc we select c@3
        deps.pkg_depends_on("c", 3, [("f", 2), ("g", 1)]); // will be selected
        deps.pkg_depends_on("d", 1, [("h", 1)]); // will be selected bc a@1 depends on it
        deps.pkg_depends_on("e", 1, [("h", 2)]); // will be selected bc b@1 depends on it
        deps.pkg_depends_on("f", 2, []); // will be selected bc c@3 depends on it
        deps.pkg_depends_on("f", 4, []); // won't be selected bc c@2 depends on it, but we don't end up selecting c@2
        deps.pkg_depends_on("g", 1, []); // will be selected bc c@3 depends on it
        deps.pkg_depends_on("h", 1, []); // won't be selected bc d@1 depends on it but e@1 depends on v2
        deps.pkg_depends_on("h", 2, []); // will be selected bc e@1 depends on it
        let result = deps.select_versions().unwrap();

        assert_eq!(result["a"], 1);
        assert_eq!(result["b"], 1);
        assert_eq!(result["c"], 3);
        assert_eq!(result["d"], 1);
        assert_eq!(result["e"], 1);
        assert_eq!(result["f"], 2);
        assert_eq!(result["g"], 1);
        assert_eq!(result["h"], 2);
        assert_eq!(result.len(), 8);
    }

    #[test]
    fn test_select_versions_with_multiple_paths() {
        let mut deps = Deps::from_root([("a", 1)], []);
        deps.pkg_depends_on("a", 1, [("b", 1), ("c", 1)]);
        deps.pkg_depends_on("b", 1, [("d", 2)]);
        deps.pkg_depends_on("c", 1, [("d", 3)]);
        let result = deps.select_versions().unwrap();

        assert_eq!(result["a"], 1);
        assert_eq!(result["b"], 1);
        assert_eq!(result["c"], 1);
        assert_eq!(result["d"], 3);
        assert_eq!(result.len(), 4);
    }

    #[test]
    fn test_select_versions_circular_dependency() {
        let mut deps = Deps::from_root([("a", 1)], []);
        deps.pkg_depends_on("a", 1, [("b", 1)]);
        deps.pkg_depends_on("b", 1, [("a", 1)]);
        let result = deps.select_versions().unwrap();

        assert_eq!(result["a"], 1);
        assert_eq!(result["b"], 1);
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn test_select_versions_empty() {
        let deps: Deps<&'static str, i32> = Deps::from_root([], []);
        let result = deps.select_versions().unwrap();

        assert_eq!(result.len(), 0);
    }

    #[test]
    fn test_backtracking() {
        let mut deps = Deps::from_root([("foo", 1), ("bar", 1)], []);
        // Here, our `foo` dependency leads us to select v2 of baz.
        deps.pkg_depends_on("foo", 1, [("baz", 2)]);
        // If we only consider `foo`, we would select v2 of baz, and therefore v3 of `other`.
        // However, later on `bar` will select v3 of baz, meaning we ultimately select v1 of `other` instead.
        deps.pkg_depends_on("baz", 2, [("other", 3)]);
        deps.pkg_depends_on("baz", 3, [("other", 1)]);
        // Here, our `bar` dependency leads us to select v3 of baz.
        deps.pkg_depends_on("bar", 1, [("baz", 3)]);
        let result = deps.select_versions().unwrap();

        assert_eq!(result["foo"], 1);
        assert_eq!(result["bar"], 1);
        assert_eq!(result["baz"], 3);
        assert_eq!(result["other"], 1);
        assert_eq!(result.len(), 4);
    }

    #[test]
    fn test_consistent_higher_version_selection() {
        let mut deps = Deps::from_root([("a", 1), ("b", 1)], []);
        deps.pkg_depends_on("a", 1, [("c", 2)]);
        deps.pkg_depends_on("b", 1, [("c", 3)]);
        let result = deps.select_versions().unwrap();

        assert_eq!(result["a"], 1);
        assert_eq!(result["b"], 1);
        assert_eq!(result["c"], 3);
    }

    #[test]
    fn test_multiple_higher_versions_consistency() {
        let mut deps = Deps::from_root([("a", 1), ("b", 1), ("c", 1)], []);
        deps.pkg_depends_on("a", 1, [("d", 2)]);
        deps.pkg_depends_on("b", 1, [("d", 3)]);
        deps.pkg_depends_on("c", 1, [("d", 4)]);
        let result = deps.select_versions().unwrap();

        assert_eq!(result["a"], 1);
        assert_eq!(result["b"], 1);
        assert_eq!(result["c"], 1);
        assert_eq!(result["d"], 4);
    }
}
