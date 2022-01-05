use crate::ast::{Collection, CommentOrNewline, Spaced, StrLiteral, TypeAnnotation};
use crate::blankspace::space0_e;
use crate::ident::lowercase_ident;
use crate::parser::Progress::*;
use crate::parser::{specialize, word1, EPackageEntry, EPackageName, Parser};
use crate::state::State;
use crate::string_literal;
use bumpalo::collections::Vec;
use roc_region::all::Loc;

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

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct PackageName<'a>(pub &'a str);

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct ModuleName<'a>(&'a str);

impl<'a> From<ModuleName<'a>> for &'a str {
    fn from(name: ModuleName<'a>) -> Self {
        name.0
    }
}

impl<'a> ModuleName<'a> {
    pub fn new(name: &'a str) -> Self {
        ModuleName(name)
    }

    pub fn as_str(&'a self) -> &'a str {
        self.0
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct ExposedName<'a>(&'a str);

impl<'a> From<ExposedName<'a>> for &'a str {
    fn from(name: ExposedName<'a>) -> Self {
        name.0
    }
}

impl<'a> ExposedName<'a> {
    pub fn new(name: &'a str) -> Self {
        ExposedName(name)
    }

    pub fn as_str(&'a self) -> &'a str {
        self.0
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceHeader<'a> {
    pub name: Loc<ModuleName<'a>>,
    pub exposes: Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    pub imports: Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>,

    // Potential comments and newlines - these will typically all be empty.
    pub before_header: &'a [CommentOrNewline<'a>],
    pub after_interface_keyword: &'a [CommentOrNewline<'a>],
    pub before_exposes: &'a [CommentOrNewline<'a>],
    pub after_exposes: &'a [CommentOrNewline<'a>],
    pub before_imports: &'a [CommentOrNewline<'a>],
    pub after_imports: &'a [CommentOrNewline<'a>],
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum To<'a> {
    ExistingPackage(&'a str),
    NewPackage(PackageName<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AppHeader<'a> {
    pub name: Loc<StrLiteral<'a>>,
    pub packages: Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>,
    pub imports: Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>,
    pub provides: Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    pub to: Loc<To<'a>>,

    // Potential comments and newlines - these will typically all be empty.
    pub before_header: &'a [CommentOrNewline<'a>],
    pub after_app_keyword: &'a [CommentOrNewline<'a>],
    pub before_packages: &'a [CommentOrNewline<'a>],
    pub after_packages: &'a [CommentOrNewline<'a>],
    pub before_imports: &'a [CommentOrNewline<'a>],
    pub after_imports: &'a [CommentOrNewline<'a>],
    pub before_provides: &'a [CommentOrNewline<'a>],
    pub after_provides: &'a [CommentOrNewline<'a>],
    pub before_to: &'a [CommentOrNewline<'a>],
    pub after_to: &'a [CommentOrNewline<'a>],
}

#[derive(Clone, Debug, PartialEq)]
pub struct PackageHeader<'a> {
    pub name: Loc<PackageName<'a>>,
    pub exposes: Vec<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    pub packages: Vec<'a, (Loc<&'a str>, Loc<PackageName<'a>>)>,
    pub imports: Vec<'a, Loc<ImportsEntry<'a>>>,

    // Potential comments and newlines - these will typically all be empty.
    pub before_header: &'a [CommentOrNewline<'a>],
    pub after_package_keyword: &'a [CommentOrNewline<'a>],
    pub before_exposes: &'a [CommentOrNewline<'a>],
    pub after_exposes: &'a [CommentOrNewline<'a>],
    pub before_packages: &'a [CommentOrNewline<'a>],
    pub after_packages: &'a [CommentOrNewline<'a>],
    pub before_imports: &'a [CommentOrNewline<'a>],
    pub after_imports: &'a [CommentOrNewline<'a>],
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct PlatformRigid<'a> {
    pub rigid: &'a str,
    pub alias: &'a str,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlatformRequires<'a> {
    pub rigids: Collection<'a, Loc<Spaced<'a, PlatformRigid<'a>>>>,
    pub signature: Loc<Spaced<'a, TypedIdent<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlatformHeader<'a> {
    pub name: Loc<PackageName<'a>>,
    pub requires: PlatformRequires<'a>,
    pub exposes: Collection<'a, Loc<Spaced<'a, ModuleName<'a>>>>,
    pub packages: Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>,
    pub imports: Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>,
    pub provides: Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    pub effects: Effects<'a>,

    // Potential comments and newlines - these will typically all be empty.
    pub before_header: &'a [CommentOrNewline<'a>],
    pub after_platform_keyword: &'a [CommentOrNewline<'a>],
    pub before_requires: &'a [CommentOrNewline<'a>],
    pub after_requires: &'a [CommentOrNewline<'a>],
    pub before_exposes: &'a [CommentOrNewline<'a>],
    pub after_exposes: &'a [CommentOrNewline<'a>],
    pub before_packages: &'a [CommentOrNewline<'a>],
    pub after_packages: &'a [CommentOrNewline<'a>],
    pub before_imports: &'a [CommentOrNewline<'a>],
    pub after_imports: &'a [CommentOrNewline<'a>],
    pub before_provides: &'a [CommentOrNewline<'a>],
    pub after_provides: &'a [CommentOrNewline<'a>],
}

/// e.g. fx.Effects
#[derive(Clone, Debug, PartialEq)]
pub struct Effects<'a> {
    pub spaces_before_effects_keyword: &'a [CommentOrNewline<'a>],
    pub spaces_after_effects_keyword: &'a [CommentOrNewline<'a>],
    pub spaces_after_type_name: &'a [CommentOrNewline<'a>],
    pub effect_shortname: &'a str,
    pub effect_type_name: &'a str,
    pub entries: Collection<'a, Loc<Spaced<'a, TypedIdent<'a>>>>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ImportsEntry<'a> {
    /// e.g. `Task` or `Task.{ Task, after }`
    Module(
        ModuleName<'a>,
        Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    ),

    /// e.g. `pf.Task` or `pf.Task.{ after }` or `pf.{ Task.{ Task, after } }`
    Package(
        &'a str,
        ModuleName<'a>,
        Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    ),
}

/// e.g.
///
/// printLine : Str -> Effect {}
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TypedIdent<'a> {
    pub ident: Loc<&'a str>,
    pub spaces_before_colon: &'a [CommentOrNewline<'a>],
    pub ann: Loc<TypeAnnotation<'a>>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct PackageEntry<'a> {
    pub shorthand: &'a str,
    pub spaces_after_shorthand: &'a [CommentOrNewline<'a>],
    pub package_name: Loc<PackageName<'a>>,
}

pub fn package_entry<'a>() -> impl Parser<'a, Spaced<'a, PackageEntry<'a>>, EPackageEntry<'a>> {
    move |arena, state| {
        // You may optionally have a package shorthand,
        // e.g. "uc" in `uc: roc/unicode 1.0.0`
        //
        // (Indirect dependencies don't have a shorthand.)
        let min_indent = 1;

        let (_, opt_shorthand, state) = maybe!(and!(
            skip_second!(
                specialize(|_, pos| EPackageEntry::Shorthand(pos), lowercase_ident()),
                word1(b':', EPackageEntry::Colon)
            ),
            space0_e(
                min_indent,
                EPackageEntry::Space,
                EPackageEntry::IndentPackage
            )
        ))
        .parse(arena, state)?;

        let (_, package_or_path, state) =
            loc!(specialize(EPackageEntry::BadPackage, package_name())).parse(arena, state)?;

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

        Ok((MadeProgress, Spaced::Item(entry), state))
    }
}

pub fn package_name<'a>() -> impl Parser<'a, PackageName<'a>, EPackageName<'a>> {
    move |arena, state: State<'a>| {
        let pos = state.pos();
        specialize(EPackageName::BadPath, string_literal::parse())
            .parse(arena, state)
            .and_then(|(progress, text, next_state)| match text {
                StrLiteral::PlainLine(text) => Ok((progress, PackageName(text), next_state)),
                StrLiteral::Line(_) => Err((progress, EPackageName::Escapes(pos), next_state)),
                StrLiteral::Block(_) => Err((progress, EPackageName::Multiline(pos), next_state)),
            })
    }
}
