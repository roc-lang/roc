use core::ops::Index;

use bumpalo::{collections::Vec, Bump};
use roc_scope::{DeclScope, LowercaseId, ModuleId, TopLevelScope, UppercaseId};

/// TODO replace this with the real Vec2 that stores 1 length as u32 etc. (We can even do u16 len.)
pub type Vec2<'a, A, B> = Vec<'a, (A, B)>;
/// TODO replace this with the real Vec3 that stores 1 length as u32 etc. (We can even do u16 len.)
pub type Vec3<'a, A, B, C> = Vec<'a, (A, B, C)>;

pub enum Problem<Region> {
    /// Includes the Region of the entire packages section.
    AppWithoutPlatform,
    /// Includes the Region of the first `platform` keyword encountered, plus the erroneous one.
    /// (We report 1 of these Problems per extra `platform` keyword.)
    MultiplePlatforms { original: Region, duplicate: Region },
    /// A package shorthand was repeated.
    DuplicateShorthand { original: Region, duplicate: Region },
}

pub enum ModType<'a, Shorthand, PkgPath, ModName, Lc, Region> {
    App {
        /// This may not be an app module, or if it is, the user may not have specified a platform.
        /// (That would be an error, but that error shouldn't block proceeding to type-checking!)
        platform: Option<PkgIdx>,
        packages: Vec3<'a, Shorthand, PkgPath, Option<Version>>,
        /// What an app module provides to the platform. This should never be empty for app modules,
        /// but should always be empty for other modules.
        provides: Vec<'a, Lc>,
    },
    Mod {
        exposes_lc: Vec<'a, Lc>,
        exposes_uc: Vec<'a, Lc>,
    },
    Package {
        exposed_modules: Vec<'a, ModName>,
        packages: Vec3<'a, Shorthand, PkgPath, Option<Version>>,
    },
    Platform {
        exposed_modules: Vec<'a, ModName>,
        packages: Vec3<'a, Shorthand, PkgPath, Option<Version>>,
        requires: Vec<'a, Lc>,
    },
}

/// Index into Mod::modules_imported
pub struct ModIdx(u16);

/// Index into Mod::packages
pub struct PkgIdx(u16);

impl PkgIdx {
    fn to_index(&self) -> usize {
        self.0 as usize
    }
}

pub struct Mod<'a, Shorthand, Lc, Uc, ModName, PkgPath, Region> {
    pub mod_type: ModType<'a, Shorthand, PkgPath, ModName, Lc, Region>,
    // We don't know if these lookups and imports are actually exposed.
    // Canonicalization doesn't have enough information to verify them; a later step will do that.
    pub modules_imported: Vec2<'a, PkgIdx, ModName>,
    pub lc_lookups_into_other_modules: Vec2<'a, ModIdx, Vec<'a, Lc>>,
    pub uc_lookups_into_other_modules: Vec2<'a, ModIdx, Vec<'a, Uc>>,
}

pub struct ParsedPkg<Shorthand, Path, Region> {
    shorthand: (Shorthand, Region),
    /// We don't ask the parser for Region here because we don't use it.
    path: Path,
    /// We don't ask the parser for Region here because we don't use it.
    version: Option<Version>,
    /// The region for the `platform` keyword, if there is one.
    platform_region: Region,
}

/// TODO move this into roc_version
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub struct Version {
    pub major: u16,
    pub minor: u16,
    pub patch: u16,
}

impl Ord for Version {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use core::cmp::Ordering::Equal;

        match self.major.cmp(&other.major) {
            Equal => match self.minor.cmp(&other.minor) {
                Equal => self.patch.cmp(&other.patch),
                _ => other,
            },
            other => other,
        }
    }
}

impl<'a, Shorthand, Lc, Uc, ModName, PkgPath, Region: Copy>
    Mod<'a, Shorthand, Lc, Uc, ModName, PkgPath, Region>
{
    pub fn app(
        arena: &'a Bump,
        // The region for the `app` keyword
        region: Region,
        // The module's name (relevant for self-qualified lookups)
        name: ModName,
        // Module params
        params: impl ExactSizeIterator<Item = Lc>,
        // The region for the entire packages section. Used if we need to report that no packages were labeled `platform`
        packages_region: Region,
        // Note: parsing already should have verified all the paths (and kicked off loading for them) and
        // reported any errors if they were in an invalid format. We assume all the paths are valid at this point!
        parsed_packages: impl ExactSizeIterator<Item = ParsedPkg<Shorthand, PkgPath, Region>>,
        // What the app module provides to the platform. These should all be lowercase at this point;
        // if any were uppercase, parsing should have already reported errors for those and discarded them.
        parsed_provides: impl ExactSizeIterator<Item = (Region, Lc)>,
    ) -> (Self, Vec<'a, Problem<Region>>) {
        let mut problems = Vec::new_in(arena);
        let mut packages = Vec::with_capacity_in(parsed_packages.len(), arena);
        let mut pkg_shorthand_regions = Vec::with_capacity_in(parsed_packages.len(), arena);
        let mut platform = None;
        let mut scope = TopLevelScope::from_params(
            arena,
            name,
            region,
            params,
            |original_region, duplicate_region| {
                problems.push(Problem::DuplicateModuleParam {
                    original: original_region,
                    duplicate: duplicate_region,
                })
            },
        );

        for (index, pkg) in parsed_packages.enumerate() {
            // If this is a platform, set our current platform to be it.
            // (Or, if we already have one, report an error for having specified multiple platforms!)
            if let Some(platform_region) = pkg.platform_region {
                match platform {
                    None => {
                        platform = Some((PkgIdx(index as u16), platform_region));
                    }
                    Some((_, original_region)) => {
                        // Report 1 Problem per extra `platform` keyword encountered.
                        // We could try to collapse all the extras into 1 report,
                        // but more than 2 extras should come up so rarely
                        // that it doesn't seem worth the complexity.
                        problems.push(Problem::MultiplePlatforms {
                            original: original_region,
                            duplicate: pkg.region,
                        })
                    }
                }
            }

            let (shorthand, region) = pkg.shorthand;

            check_for_duplicate_shorthands(
                arena,
                shorthand,
                region,
                packages.iter().map(|(sh, _, _)| *sh),
                pkg_shorthand_regions.iter().copied(),
                &mut problems,
            );

            // Note that the shorthands in this list may not be unique!
            // We report an error if they aren't, but we still record them all.
            // This lets us iterate over all the packages later.
            packages.push((shorthand, pkg.path, pkg.version));
            pkg_shorthand_regions.push(region);
        }

        // Report an error if there's still no platform at this point.
        let platform = match platform {
            Some((pkg_idx, _)) => {
                // Discard the region; we have no reason to save it!
                Some(pkg_idx)
            }
            None => {
                problems.push(Problem::AppWithoutPlatform(packages_region));
                None
            }
        };

        // Check for duplicate `provides` entries.
        let mut provides = Vec::with_capacity_in(parsed_provides.len(), arena);

        {
            let mut provides_regions = Vec::with_capacity_in(parsed_provides.len(), arena);

            for (lc, region) in parsed_provides {
                for (idx, original) in provides.iter().enumerate() {
                    if lc == original {
                        let original_region = provides_regions[idx];

                        problems.push(Problem::DuplicateProvides {
                            original,
                            duplicate: region,
                        })
                    } else {
                        provides.push(lc)
                    }
                }
            }
        }

        (
            Self {
                mod_type: ModType::App {
                    platform,
                    packages,
                    provides,
                },
                modules_imported: todo!(), // TODO populate by calling Self::can_tl
                lc_lookups_into_other_modules: todo!(), // TODO populate by calling Self::can_tl
                uc_lookups_into_other_modules: todo!(), // TODO populate by calling Self::can_tl
            },
            problems,
        )
    }

    pub fn package(
        arena: &'a Bump,
        // Note: parsing already should have verified all the paths (and kicked off loading for them) and
        // reported any errors if they were in an invalid format. We assume all the paths are valid at this point!
        parsed_packages: impl ExactSizeIterator<Item = ParsedPkg<Shorthand, PkgPath, Region>>,
    ) -> (Self, Vec<'a, Problem<Region>>) {
        let mut problems = Vec::new_in(arena);
        let mut packages = Vec::with_capacity_in(parsed_packages.len(), arena);
        let mut pkg_shorthand_regions = Vec::with_capacity_in(parsed_packages.len(), arena);

        for (index, pkg) in parsed_packages.enumerate() {
            let (shorthand, region) = pkg.shorthand;

            check_for_duplicate_shorthands(
                arena,
                shorthand,
                region,
                packages.iter().map(|(sh, _, _)| *sh),
                pkg_shorthand_regions.iter().copied(),
                &mut problems,
            );

            // Note that the shorthands in this list may not be unique!
            // We report an error if they aren't, but we still record them all.
            // This lets us iterate over all the packages later.
            packages.push((shorthand, pkg.path, pkg.version));
            pkg_shorthand_regions.push(region);
        }

        (
            Self {
                mod_type: ModType::Package {
                    packages,
                    exposed_modules: todo!(),
                },
                modules_imported: todo!(), // TODO populate by calling Self::can_tl
                lc_lookups_into_other_modules: todo!(), // TODO populate by calling Self::can_tl
                uc_lookups_into_other_modules: todo!(), // TODO populate by calling Self::can_tl
            },
            problems,
        )
    }

    /// Canonicalize the top-level defs
    fn can_tl() {}

    // Hosted {
    //     name: ModName<'a>,
    //     exposes: &'a [Loc<ExposedName<'a>>],
    //     generates: UppercaseIdent<'a>,
    //     generates_with: &'a [Loc<ExposedName<'a>>],
    // },
    // /// Only created during canonicalization, never actually parsed from source
    // Builtin {
    //     name: ModName<'a>,
    //     exposes: &'a [Loc<ExposedName<'a>>],
    //     generates_with: &'a [Symbol],
    // },
    // Package {
    //     /// usually something other than `pf`
    //     config_shorthand: &'a str,
    //     exposes: &'a [Loc<ModName<'a>>],
    //     exposes_ids: &'a [ModId],
    // },
    // Platform {
    //     opt_app_module_id: Option<ModId>,
    //     /// the name and type scheme of the main function (required by the platform)
    //     /// (type scheme is currently unused)
    //     provides: &'a [(Loc<ExposedName<'a>>, Loc<TypedIdent<'a>>)],
    //     requires: &'a [Loc<TypedIdent<'a>>],
    //     requires_types: &'a [Loc<UppercaseIdent<'a>>],
    //     exposes: &'a [Loc<ModName<'a>>],
    //     exposes_ids: &'a [ModId],

    //     /// usually `pf`
    //     config_shorthand: &'a str,
    // },
    // Mod {
    //     name: ModName<'a>,
    //     exposes: &'a [Loc<ExposedName<'a>>],
    // },
}

fn check_for_duplicate_shorthands<'a, Shorthand, Region: Copy>(
    _arena: &'a Bump,
    shorthand: Shorthand,
    region: Region,
    shorthands: impl Iterator<Item = Shorthand>,
    shorthand_regions: impl Index<usize>,
    problems: &mut Vec<'a, Problem<Region>>,
) {
    // Check for duplicate shorthands. Shorthands must be unique!
    for (idx, existing_shorthand) in shorthands.enumerate() {
        if existing_shorthand == shorthand {
            // Safety: pkg_shorthand_regions has the same length as packages, so this will always be in-bounds.
            let original = shorthand_regions.index(idx as usize);

            problems.push(Problem::DuplicateShorthand {
                original,
                duplicate: *region,
            });
        }
    }
}
