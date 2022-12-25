use crate::blankspace::space0_e;
use crate::ident::lowercase_ident;
use crate::parser::{optional, then};
use crate::parser::{specialize, word1, Parser};
use crate::string_literal;
use roc_ast2::{
    EPackageEntry, EPackageName, ExposedName, ModuleName, PackageEntry, PackageName, Spaced,
    StrLiteral, To, TypedIdent, UppercaseIdent,
};
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::Loc;
use std::fmt::Debug;

impl<'a> HeaderType<'a> {
    pub fn exposed_or_provided_values(&'a self) -> &'a [Loc<ExposedName<'a>>] {
        match self {
            HeaderType::App {
                provides: exposes, ..
            }
            | HeaderType::Hosted { exposes, .. }
            | HeaderType::Builtin { exposes, .. }
            | HeaderType::Interface { exposes, .. } => exposes,
            HeaderType::Platform { .. } | HeaderType::Package { .. } => &[],
        }
    }
}

#[derive(Debug)]
pub enum HeaderType<'a> {
    App {
        output_name: StrLiteral<'a>,
        provides: &'a [Loc<ExposedName<'a>>],
        to_platform: To<'a>,
    },
    Hosted {
        name: ModuleName<'a>,
        exposes: &'a [Loc<ExposedName<'a>>],
        generates: UppercaseIdent<'a>,
        generates_with: &'a [Loc<ExposedName<'a>>],
    },
    /// Only created during canonicalization, never actually parsed from source
    Builtin {
        name: ModuleName<'a>,
        exposes: &'a [Loc<ExposedName<'a>>],
        generates_with: &'a [Symbol],
    },
    Package {
        /// usually something other than `pf`
        config_shorthand: &'a str,
        exposes: &'a [Loc<ModuleName<'a>>],
    },
    Platform {
        opt_app_module_id: Option<ModuleId>,
        /// the name and type scheme of the main function (required by the platform)
        /// (type scheme is currently unused)
        provides: &'a [(Loc<ExposedName<'a>>, Loc<TypedIdent<'a>>)],
        requires: &'a [Loc<TypedIdent<'a>>],
        requires_types: &'a [Loc<UppercaseIdent<'a>>],
        exposes: &'a [Loc<ModuleName<'a>>],

        /// usually `pf`
        config_shorthand: &'a str,
    },
    Interface {
        name: ModuleName<'a>,
        exposes: &'a [Loc<ExposedName<'a>>],
    },
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Version<'a> {
    Exact(&'a str),
    Range {
        min: &'a str,
        min_comparison: VersionComparison,
        max: &'a str,
        max_comparison: VersionComparison,
    },
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum VersionComparison {
    AllowsEqual,
    DisallowsEqual,
}

pub fn package_entry<'a>() -> impl Parser<'a, Spaced<'a, PackageEntry<'a>>, EPackageEntry<'a>> {
    map!(
        // You may optionally have a package shorthand,
        // e.g. "uc" in `uc: roc/unicode 1.0.0`
        //
        // (Indirect dependencies don't have a shorthand.)
        and!(
            optional(and!(
                skip_second!(
                    specialize(|_, pos| EPackageEntry::Shorthand(pos), lowercase_ident()),
                    word1(b':', EPackageEntry::Colon)
                ),
                space0_e(EPackageEntry::IndentPackage)
            )),
            loc!(specialize(EPackageEntry::BadPackage, package_name()))
        ),
        move |(opt_shorthand, package_or_path)| {
            let entry = match opt_shorthand {
                Some((shorthand, spaces_after_shorthand)) => PackageEntry {
                    shorthand,
                    spaces_after_shorthand,
                    package_name: package_or_path,
                },
                None => PackageEntry {
                    shorthand: "",
                    spaces_after_shorthand: &[],
                    package_name: package_or_path,
                },
            };

            Spaced::Item(entry)
        }
    )
}

pub fn package_name<'a>() -> impl Parser<'a, PackageName<'a>, EPackageName<'a>> {
    then(
        loc!(specialize(EPackageName::BadPath, string_literal::parse())),
        move |_arena, state, progress, text| match text.value {
            StrLiteral::PlainLine(text) => Ok((progress, PackageName(text), state)),
            StrLiteral::Line(_) => Err((progress, EPackageName::Escapes(text.region.start()))),
            StrLiteral::Block(_) => Err((progress, EPackageName::Multiline(text.region.start()))),
        },
    )
}
