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
    /// Note that some packages may return multiple versions. This can happen when a
    /// dependency requires a higher version of another package than the version the
    /// root requires exactly.
    ///
    /// For example, let's say the root depends on package `foo` version (exactly) 1.2,
    /// but another package the root indirectly depends on needs at least `foo` 1.3,
    /// and a third such package needs `foo` 1.4. In that case, we would select
    /// 1.2 as the "root version" and 1.4 as the "higher version." (So, the tuple
    /// associated with the `foo` package would be (1.2, Some(1.4)).)
    ///
    /// This would mean that all packages which depend on a version of `foo` greater
    /// than 1.2 would get 1.4, and all other packages would get `foo` 1.2.
    ///
    /// This means that builds always successfully solve.
    pub fn select_versions(self) -> HashMap<Pkg, (Ver, Option<Ver>)> {
        let mut answer = self.exact_versions;
        let mut to_process: Vec<(Pkg, Ver)> =
            answer.iter().map(|(pkg, (ver, _))| (*pkg, *ver)).collect();

        while let Some((pkg, ver)) = to_process.pop() {
            if let Some(deps) = self.indirect_deps.get(&(pkg, ver)) {
                for (dep_pkg, dep_ver) in deps {
                    let dep_ver = *dep_ver;
                    match answer.entry(dep_pkg.clone()) {
                        Entry::Vacant(e) => {
                            e.insert((dep_ver, None));
                            to_process.push((dep_pkg.clone(), dep_ver));
                        }
                        Entry::Occupied(mut e) => {
                            let (root_ver, higher_ver) = e.get_mut();
                            if dep_ver > *root_ver {
                                match higher_ver {
                                    Some(hv) if dep_ver > *hv => *hv = dep_ver,
                                    None => *higher_ver = Some(dep_ver),
                                    _ => {}
                                }
                                to_process.push((dep_pkg.clone(), dep_ver));
                            }
                        }
                    }
                }
            }
        }

        answer
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
