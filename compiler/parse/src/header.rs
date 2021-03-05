use crate::blankspace::space0;
use crate::ident::lowercase_ident;
use crate::module::package_name;
use crate::parser::{ascii_char, optional, Either, Parser, Progress::*, State, SyntaxError};
use crate::string_literal;
use crate::{
    ast::{CommentOrNewline, Spaceable, StrLiteral, TypeAnnotation},
    parser::specialize,
};
use bumpalo::collections::Vec;
use inlinable_string::InlinableString;
use roc_region::all::Loc;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct PackageName<'a> {
    pub account: &'a str,
    pub pkg: &'a str,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
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

#[derive(Clone, PartialEq, Debug)]
pub enum PackageOrPath<'a> {
    Package(PackageName<'a>, Version<'a>),
    Path(StrLiteral<'a>),
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct ModuleName<'a>(&'a str);

impl<'a> Into<&'a str> for ModuleName<'a> {
    fn into(self) -> &'a str {
        self.0
    }
}

impl<'a> Into<InlinableString> for ModuleName<'a> {
    fn into(self) -> InlinableString {
        self.0.into()
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

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceHeader<'a> {
    pub name: Loc<ModuleName<'a>>,
    pub exposes: Vec<'a, Loc<ExposesEntry<'a, &'a str>>>,
    pub imports: Vec<'a, Loc<ImportsEntry<'a>>>,

    // Potential comments and newlines - these will typically all be empty.
    pub after_interface_keyword: &'a [CommentOrNewline<'a>],
    pub before_exposes: &'a [CommentOrNewline<'a>],
    pub after_exposes: &'a [CommentOrNewline<'a>],
    pub before_imports: &'a [CommentOrNewline<'a>],
    pub after_imports: &'a [CommentOrNewline<'a>],
}

#[derive(Clone, Debug, PartialEq)]
pub enum To<'a> {
    ExistingPackage(&'a str),
    NewPackage(PackageOrPath<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AppHeader<'a> {
    pub name: Loc<StrLiteral<'a>>,
    pub packages: Vec<'a, Loc<PackageEntry<'a>>>,
    pub imports: Vec<'a, Loc<ImportsEntry<'a>>>,
    pub provides: Vec<'a, Loc<ExposesEntry<'a, &'a str>>>,
    pub to: Loc<To<'a>>,

    // Potential comments and newlines - these will typically all be empty.
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
    pub exposes: Vec<'a, Loc<ExposesEntry<'a, &'a str>>>,
    pub packages: Vec<'a, (Loc<&'a str>, Loc<PackageOrPath<'a>>)>,
    pub imports: Vec<'a, Loc<ImportsEntry<'a>>>,

    // Potential comments and newlines - these will typically all be empty.
    pub after_package_keyword: &'a [CommentOrNewline<'a>],
    pub before_exposes: &'a [CommentOrNewline<'a>],
    pub after_exposes: &'a [CommentOrNewline<'a>],
    pub before_packages: &'a [CommentOrNewline<'a>],
    pub after_packages: &'a [CommentOrNewline<'a>],
    pub before_imports: &'a [CommentOrNewline<'a>],
    pub after_imports: &'a [CommentOrNewline<'a>],
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlatformHeader<'a> {
    pub name: Loc<PackageName<'a>>,
    pub requires: Vec<'a, Loc<TypedIdent<'a>>>,
    pub exposes: Vec<'a, Loc<ExposesEntry<'a, ModuleName<'a>>>>,
    pub packages: Vec<'a, Loc<PackageEntry<'a>>>,
    pub imports: Vec<'a, Loc<ImportsEntry<'a>>>,
    pub provides: Vec<'a, Loc<ExposesEntry<'a, &'a str>>>,
    pub effects: Effects<'a>,

    // Potential comments and newlines - these will typically all be empty.
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
    pub entries: &'a [Loc<TypedIdent<'a>>],
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExposesEntry<'a, T> {
    /// e.g. `Task`
    Exposed(T),

    // Spaces
    SpaceBefore(&'a ExposesEntry<'a, T>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a ExposesEntry<'a, T>, &'a [CommentOrNewline<'a>]),
}

impl<'a, T> Spaceable<'a> for ExposesEntry<'a, T> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        ExposesEntry::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        ExposesEntry::SpaceAfter(self, spaces)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ImportsEntry<'a> {
    /// e.g. `Task` or `Task.{ Task, after }`
    Module(ModuleName<'a>, Vec<'a, Loc<ExposesEntry<'a, &'a str>>>),

    /// e.g. `base.Task` or `base.Task.{ after }` or `base.{ Task.{ Task, after } }`
    Package(
        &'a str,
        ModuleName<'a>,
        Vec<'a, Loc<ExposesEntry<'a, &'a str>>>,
    ),

    // Spaces
    SpaceBefore(&'a ImportsEntry<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a ImportsEntry<'a>, &'a [CommentOrNewline<'a>]),
}

impl<'a> ExposesEntry<'a, &'a str> {
    pub fn as_str(&'a self) -> &'a str {
        use ExposesEntry::*;

        match self {
            Exposed(string) => string,
            SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => sub_entry.as_str(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedIdent<'a> {
    /// e.g.
    ///
    /// printLine : Str -> Effect {}
    Entry {
        ident: Loc<&'a str>,
        spaces_before_colon: &'a [CommentOrNewline<'a>],
        ann: Loc<TypeAnnotation<'a>>,
    },

    // Spaces
    SpaceBefore(&'a TypedIdent<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a TypedIdent<'a>, &'a [CommentOrNewline<'a>]),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PackageEntry<'a> {
    Entry {
        shorthand: &'a str,
        spaces_after_shorthand: &'a [CommentOrNewline<'a>],
        package_or_path: Loc<PackageOrPath<'a>>,
    },

    // Spaces
    SpaceBefore(&'a PackageEntry<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a PackageEntry<'a>, &'a [CommentOrNewline<'a>]),
}

impl<'a> Spaceable<'a> for PackageEntry<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        PackageEntry::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        PackageEntry::SpaceAfter(self, spaces)
    }
}

pub fn package_entry<'a>() -> impl Parser<'a, PackageEntry<'a>, SyntaxError<'a>> {
    move |arena, state| {
        // You may optionally have a package shorthand,
        // e.g. "uc" in `uc: roc/unicode 1.0.0`
        //
        // (Indirect dependencies don't have a shorthand.)
        let (_, opt_shorthand, state) = optional(and!(
            skip_second!(lowercase_ident(), ascii_char(b':')),
            space0(1)
        ))
        .parse(arena, state)?;
        let (_, package_or_path, state) = loc!(package_or_path()).parse(arena, state)?;

        let entry = match opt_shorthand {
            Some((shorthand, spaces_after_shorthand)) => PackageEntry::Entry {
                shorthand,
                spaces_after_shorthand,
                package_or_path,
            },
            None => PackageEntry::Entry {
                shorthand: "",
                spaces_after_shorthand: &[],
                package_or_path,
            },
        };

        Ok((MadeProgress, entry, state))
    }
}

pub fn package_or_path<'a>() -> impl Parser<'a, PackageOrPath<'a>, SyntaxError<'a>> {
    map!(
        either!(
            specialize(
                |e, r, c| SyntaxError::Expr(crate::parser::EExpr::Str(e, r, c)),
                string_literal::parse()
            ),
            and!(
                package_name(),
                skip_first!(one_or_more!(ascii_char(b' ')), package_version())
            )
        ),
        |answer| {
            match answer {
                Either::First(str_literal) => PackageOrPath::Path(str_literal),
                Either::Second((name, version)) => PackageOrPath::Package(name, version),
            }
        }
    )
}

fn package_version<'a>() -> impl Parser<'a, Version<'a>, SyntaxError<'a>> {
    move |_, _| todo!("TODO parse package version")
}
