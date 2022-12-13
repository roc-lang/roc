use crate::ast::{Collection, CommentOrNewline, Spaced, Spaces, StrLiteral, TypeAnnotation};
use crate::blankspace::space0_e;
use crate::ident::{lowercase_ident, UppercaseIdent};
use crate::parser::{optional, then};
use crate::parser::{specialize, word1, EPackageEntry, EPackagePath, Parser};
use crate::string_literal;
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::Loc;
use std::fmt::Debug;

#[derive(Debug)]
pub enum HeaderType<'a> {
    App {
        output_name: StrLiteral<'a>,
        to_platform: To<'a>,
    },
    Hosted {
        name: ModuleName<'a>,
        generates: UppercaseIdent<'a>,
        generates_with: &'a [Loc<ExposedName<'a>>],
    },
    /// Only created during canonicalization, never actually parsed from source
    Builtin {
        name: ModuleName<'a>,
        generates_with: &'a [Symbol],
    },
    Package {
        /// usually something other than `pf`
        config_shorthand: &'a str,
    },
    Platform {
        opt_app_module_id: Option<ModuleId>,
        /// the name and type scheme of the main function (required by the platform)
        /// (type scheme is currently unused)
        provides: &'a [(Loc<ExposedName<'a>>, Loc<TypedIdent<'a>>)],
        requires: &'a [Loc<TypedIdent<'a>>],
        requires_types: &'a [Loc<UppercaseIdent<'a>>],

        /// usually `pf`
        config_shorthand: &'a str,
    },
    Interface {
        name: ModuleName<'a>,
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

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct PackagePath<'a>(&'a str);

impl<'a> PackagePath<'a> {
    pub fn to_str(self) -> &'a str {
        self.0
    }

    pub fn as_str(&self) -> &'a str {
        self.0
    }
}

impl<'a> From<PackagePath<'a>> for &'a str {
    fn from(name: PackagePath<'a>) -> &'a str {
        name.0
    }
}

impl<'a> From<&'a str> for PackagePath<'a> {
    fn from(string: &'a str) -> Self {
        Self(string)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct ModuleName<'a>(&'a str);

impl<'a> From<ModuleName<'a>> for &'a str {
    fn from(name: ModuleName<'a>) -> Self {
        name.0
    }
}

impl<'a> ModuleName<'a> {
    pub const fn new(name: &'a str) -> Self {
        ModuleName(name)
    }

    pub const fn as_str(&'a self) -> &'a str {
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
    pub const fn new(name: &'a str) -> Self {
        ExposedName(name)
    }

    pub fn as_str(&'a self) -> &'a str {
        self.0
    }
}

pub trait Keyword: Copy + Clone + Debug {
    const KEYWORD: &'static str;
}

macro_rules! keywords {
    ($($name:ident => $string:expr),* $(,)?) => {
        $(
            #[derive(Copy, Clone, PartialEq, Eq, Debug)]
            pub struct $name;

            impl Keyword for $name {
                const KEYWORD: &'static str = $string;
            }
        )*
    }
}

keywords! {
    ExposesKeyword => "exposes",
    ImportsKeyword => "imports",
    WithKeyword => "with",
    GeneratesKeyword => "generates",
    PackageKeyword => "package",
    PackagesKeyword => "packages",
    RequiresKeyword => "requires",
    ProvidesKeyword => "provides",
    ToKeyword => "to",
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KeywordItem<'a, K, V> {
    pub keyword: Spaces<'a, K>,
    pub item: V,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceHeader<'a> {
    pub before_name: &'a [CommentOrNewline<'a>],
    pub name: Loc<ModuleName<'a>>,

    pub exposes: KeywordItem<'a, ExposesKeyword, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,
    pub imports: KeywordItem<'a, ImportsKeyword, Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HostedHeader<'a> {
    pub before_name: &'a [CommentOrNewline<'a>],
    pub name: Loc<ModuleName<'a>>,
    pub exposes: KeywordItem<'a, ExposesKeyword, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,

    pub imports: KeywordItem<'a, ImportsKeyword, Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>>,

    pub generates: KeywordItem<'a, GeneratesKeyword, UppercaseIdent<'a>>,
    pub generates_with:
        KeywordItem<'a, WithKeyword, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum To<'a> {
    ExistingPackage(&'a str),
    NewPackage(PackagePath<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AppHeader<'a> {
    pub before_name: &'a [CommentOrNewline<'a>],
    pub name: Loc<StrLiteral<'a>>,

    pub packages:
        Option<KeywordItem<'a, PackagesKeyword, Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>>>,
    pub imports:
        Option<KeywordItem<'a, ImportsKeyword, Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>>>,
    pub provides: ProvidesTo<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProvidesTo<'a> {
    pub provides_keyword: Spaces<'a, ProvidesKeyword>,
    pub entries: Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    pub types: Option<Collection<'a, Loc<Spaced<'a, UppercaseIdent<'a>>>>>,

    pub to_keyword: Spaces<'a, ToKeyword>,
    pub to: Loc<To<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PackageHeader<'a> {
    pub before_name: &'a [CommentOrNewline<'a>],
    pub name: Loc<PackagePath<'a>>,

    pub exposes: KeywordItem<'a, ExposesKeyword, Collection<'a, Loc<Spaced<'a, ModuleName<'a>>>>>,
    pub packages:
        KeywordItem<'a, PackagesKeyword, Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlatformRequires<'a> {
    pub rigids: Collection<'a, Loc<Spaced<'a, UppercaseIdent<'a>>>>,
    pub signature: Loc<Spaced<'a, TypedIdent<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlatformHeader<'a> {
    pub before_name: &'a [CommentOrNewline<'a>],
    pub name: Loc<PackagePath<'a>>,

    pub requires: KeywordItem<'a, RequiresKeyword, PlatformRequires<'a>>,
    pub exposes: KeywordItem<'a, ExposesKeyword, Collection<'a, Loc<Spaced<'a, ModuleName<'a>>>>>,
    pub packages:
        KeywordItem<'a, PackagesKeyword, Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>>,
    pub imports: KeywordItem<'a, ImportsKeyword, Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>>,
    pub provides:
        KeywordItem<'a, ProvidesKeyword, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct PackageEntry<'a> {
    pub shorthand: &'a str,
    pub spaces_after_shorthand: &'a [CommentOrNewline<'a>],
    pub package_path: Loc<PackagePath<'a>>,
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
            loc!(specialize(EPackageEntry::BadPackage, package_path()))
        ),
        move |(opt_shorthand, package_or_path)| {
            let entry = match opt_shorthand {
                Some((shorthand, spaces_after_shorthand)) => PackageEntry {
                    shorthand,
                    spaces_after_shorthand,
                    package_path: package_or_path,
                },
                None => PackageEntry {
                    shorthand: "",
                    spaces_after_shorthand: &[],
                    package_path: package_or_path,
                },
            };

            Spaced::Item(entry)
        }
    )
}

pub fn package_path<'a>() -> impl Parser<'a, PackagePath<'a>, EPackagePath<'a>> {
    then(
        loc!(specialize(EPackagePath::BadPath, string_literal::parse())),
        move |_arena, state, progress, text| match text.value {
            StrLiteral::PlainLine(text) => Ok((progress, PackagePath(text), state)),
            StrLiteral::Line(_) => Err((progress, EPackagePath::Escapes(text.region.start()))),
            StrLiteral::Block(_) => Err((progress, EPackagePath::Multiline(text.region.start()))),
        },
    )
}
