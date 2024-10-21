use std::collections::{BTreeSet, HashMap, HashSet};
use std::hash::Hash;

use roc_collections::{VecMap, VecSet};

pub struct Deps<Pkg, Ver> {
    exact_versions: VecMap<Pkg, Ver>,
    versions: HashMap<Pkg, BTreeSet<Ver>>,
    indirect_deps: HashMap<(Pkg, Ver), VecMap<Pkg, Ver>>,
    exclusions: VecSet<(Pkg, Ver)>,
}

impl<Pkg: Ord + Clone + Copy + Eq + Hash, Ver: Ord + Clone + Copy + Eq + Hash> Deps<Pkg, Ver> {
    /// Root modules always require exact versions for all their dependencies.
    pub fn from_root(
        root_deps: impl IntoIterator<Item = (Pkg, Ver)>,
        exclusions: impl IntoIterator<Item = (Pkg, Ver)>,
    ) -> Result<Self, VecSet<Pkg>> {
        let exclusions: VecSet<(Pkg, Ver)> = exclusions.into_iter().collect();
        let root_deps = root_deps.into_iter();
        let mut exact_versions = VecMap::with_capacity(root_deps.size_hint().0);

        // Verify that no package-versions are both required and excluded.
        // If any are, error out before attempting anything else.
        let mut excluded_direct_deps = VecSet::default();

        for (pkg, ver) in root_deps {
            if exclusions.contains(&(pkg, ver)) {
                // Package version is excluded; we're going to return early.
                excluded_direct_deps.insert(pkg);
            } else {
                exact_versions.insert(pkg, ver);
            }
        }

        if excluded_direct_deps.is_empty() {
            Ok(Self {
                exact_versions,
                exclusions,
                indirect_deps: HashMap::default(),
            })
        } else {
            Err(excluded_direct_deps)
        }
    }

    fn is_excluded(&self, pkg: Pkg, ver: Ver) -> bool {
        self.exclusions.contains(&(pkg, ver))
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
        // Excluded package/version combinations don't even get recorded. It's like they don't exist!
        if !self.is_excluded(pkg, ver) {
            self.indirect_deps
                .entry((pkg, ver))
                .or_insert_with(|| deps.into_iter().collect());
        }
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
    pub fn select_versions(self) -> Result<VecMap<Pkg, Ver>, VecSet<Pkg>> {
        // Map of package to its required version. Use VecMap for deterministic ordering.
        // Initialize required_versions with exact_versions
        let mut required_versions: VecMap<Pkg, Ver> = self.exact_versions.clone();

        // Packages to process, using VecSet for deterministic ordering
        let mut packages_to_process: VecSet<Pkg> = required_versions.keys().copied().collect();

        // Keep track of whether we've updated a package's version
        let mut updated_packages: VecSet<Pkg> = VecSet::default();

        // First, go through and delete all the unreachable dependencies.
        // For example, if `foo@1` depends on `bar@2`, but `bar@2` is excluded,
        // see if we can find a `bar@3` to use instead, etc. If we ultimately
        // find a version of `bar` that `foo@1` can depend on, update `foo@1` to
        // depend on that one. Otherwise, remove `foo@1` from the deps, because
        // we can't use it, and then go recursively do this to everyone who depends
        // on `foo@1` because we can't use them either unless they can find a `foo@2`
        // or something to depend on.
        let todo = ();

        while let Some(pkg) = packages_to_process.pop() {
            let ver = *required_versions.get(&pkg).unwrap();

            // Get dependencies of this package-version
            if let Some(deps) = self.indirect_deps.get(&(pkg, ver)) {
                for (&dep_pkg, &dep_ver) in deps.iter() {
                    // This package-version is excluded. Try to find a higher one that isn't!
                    if self.is_excluded(dep_pkg, dep_ver) {
                        let todo = ();
                        // TODO if it was excluded, go find in our versions list the next version
                        // of this package that isn't excluded. (We can pre-prune that one to not
                        // include excluded ones.)
                        // If we can't find one, that means this
                    }

                    // See if dep_pkg is in required_versions
                    if let Some(&existing_ver) = required_versions.get(&dep_pkg) {
                        if dep_ver > existing_ver {
                            // Use the higher version
                            required_versions.insert(dep_pkg, dep_ver);
                            packages_to_process.insert(dep_pkg);
                            updated_packages.insert(dep_pkg);
                        }

                        // (If the existing version was already higher or equal, do nothing.)
                    } else {
                        // First time we see dep_pkg
                        required_versions.insert(dep_pkg, dep_ver);
                        packages_to_process.insert(dep_pkg);
                        updated_packages.insert(dep_pkg);
                    }
                }
            } else {
                // We couldnd't find an entry in indirect_deps for this
            }
        }

        // After processing, check if required_versions satisfy the exact_versions
        let mut failed_pkgs = HashSet::new();
        for (pkg, ver) in self.exact_versions.iter() {
            if required_versions.get(pkg) != Some(ver) {
                failed_pkgs.insert(pkg.clone());
            }
        }

        if failed_pkgs.is_empty() {
            Ok(required_versions.into_iter().collect())
        } else {
            Err(failed_pkgs)
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
        assert_eq!(deps.exact_versions.get(&"a"), Some(&1));
        assert_eq!(deps.exact_versions.get(&"b"), Some(&2));
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
