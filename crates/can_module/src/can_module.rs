use bumpalo::{collections::Vec, Bump};
use roc_scope::{DeclScope, LowercaseId, ModuleId, TopLevelScope, UppercaseId};

pub struct Module<'a, Region, LcStr, Path> {
    packages: Vec<'a, (LcStr, Path)>,
}

/// Parsing will provide these.
pub struct Pkg<LcStr, Path, Region> {
    shorthand: (LcStr, Region),
    path: (Path, Region),
    is_platform: bool,
}

pub enum Problem<Region> {
    /// Includes the Region of the entire packages section.
    AppWithoutPlatform,
    /// Includes the Region of the `platform` keyword encountered.
    /// (We report 1 of these Problems per extra `platform` keyword.)
    MultiplePlatforms(Region),
}

impl<'a, Region: Copy, LcStr, Path> Module<'a, Region, LcStr, Path> {
    pub fn app(
        arena: &'a Bump,
        // Used if we need to report that no packages were labeled as the `platform`
        packages_region: Region,
        parsed_provides: impl ExactSizeIterator<Item = (Region, LcStr)>,
        parsed_packages: impl ExactSizeIterator<Item = Pkg<LcStr, Path, Region>>,
    ) -> (Self, Vec<'a, Problem<Region>>) {
        let mut problems = Vec::new_in(arena);
        let mut packages = Vec::with_capacity_in(parsed_packages.len(), arena);
        let mut platform = None;

        for pkg in parsed_packages {
            if pkg.is_platform {
                if platform.is_none() {
                    platform = Some((pkg.shorthand.0, pkg.path.0));
                } else {
                    // Report 1 Problem per extra `platform` keyword encountered.
                    // We could try to collapse all the extras into 1 report,
                    // but more than 2 extras should come up so rarely
                    // that it doesn't seem worth the complexity.
                    problems.push(Problem::MultiplePlatforms(pkg.region))
                }
            } else {
                packages.push((pkg.shorthand.0, pkg.path.0));
            }
        }

        if platform.is_none() {
            problems.push(Problem::AppWithoutPlatform(packages_region));
        }
    }

    /// Canonicalize the top-level defs
    fn can_tl()

    // Hosted {
    //     name: ModuleName<'a>,
    //     exposes: &'a [Loc<ExposedName<'a>>],
    //     generates: UppercaseIdent<'a>,
    //     generates_with: &'a [Loc<ExposedName<'a>>],
    // },
    // /// Only created during canonicalization, never actually parsed from source
    // Builtin {
    //     name: ModuleName<'a>,
    //     exposes: &'a [Loc<ExposedName<'a>>],
    //     generates_with: &'a [Symbol],
    // },
    // Package {
    //     /// usually something other than `pf`
    //     config_shorthand: &'a str,
    //     exposes: &'a [Loc<ModuleName<'a>>],
    //     exposes_ids: &'a [ModuleId],
    // },
    // Platform {
    //     opt_app_module_id: Option<ModuleId>,
    //     /// the name and type scheme of the main function (required by the platform)
    //     /// (type scheme is currently unused)
    //     provides: &'a [(Loc<ExposedName<'a>>, Loc<TypedIdent<'a>>)],
    //     requires: &'a [Loc<TypedIdent<'a>>],
    //     requires_types: &'a [Loc<UppercaseIdent<'a>>],
    //     exposes: &'a [Loc<ModuleName<'a>>],
    //     exposes_ids: &'a [ModuleId],

    //     /// usually `pf`
    //     config_shorthand: &'a str,
    // },
    // Module {
    //     name: ModuleName<'a>,
    //     exposes: &'a [Loc<ExposedName<'a>>],
    // },
}
