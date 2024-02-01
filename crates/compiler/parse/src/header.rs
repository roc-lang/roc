use crate::ast::{
    Collection, CommentOrNewline, Malformed, Spaced, Spaces, StrLiteral, TypeAnnotation,
};
use crate::blankspace::space0_e;
use crate::expr::merge_spaces;
use crate::ident::{lowercase_ident, UppercaseIdent};
use crate::parser::{optional, then};
use crate::parser::{specialize, word1, EPackageEntry, EPackageName, Parser};
use crate::string_literal;
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
    pub fn to_string(&'a self) -> &str {
        match self {
            HeaderType::App { .. } => "app",
            HeaderType::Hosted { .. } => "hosted",
            HeaderType::Builtin { .. } => "builtin",
            HeaderType::Package { .. } => "package",
            HeaderType::Platform { .. } => "platform",
            HeaderType::Interface { .. } => "interface",
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
        exposes_ids: &'a [ModuleId],
    },
    Platform {
        opt_app_module_id: Option<ModuleId>,
        /// the name and type scheme of the main function (required by the platform)
        /// (type scheme is currently unused)
        provides: &'a [(Loc<ExposedName<'a>>, Loc<TypedIdent<'a>>)],
        requires: &'a [Loc<TypedIdent<'a>>],
        requires_types: &'a [Loc<UppercaseIdent<'a>>],
        exposes: &'a [Loc<ModuleName<'a>>],
        exposes_ids: &'a [ModuleId],

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

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct PackageName<'a>(&'a str);

impl<'a> PackageName<'a> {
    pub fn to_str(self) -> &'a str {
        self.0
    }

    pub fn as_str(&self) -> &'a str {
        self.0
    }
}

impl<'a> From<PackageName<'a>> for &'a str {
    fn from(name: PackageName<'a>) -> &'a str {
        name.0
    }
}

impl<'a> From<&'a str> for PackageName<'a> {
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
    WithKeyword => "with",
    GeneratesKeyword => "generates",
    PackageKeyword => "package",
    PackagesKeyword => "packages",
    RequiresKeyword => "requires",
    ProvidesKeyword => "provides",
    ToKeyword => "to",
    // No longer supported, but still parsed so we can format it away
    ImportsKeyword => "imports",
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct KeywordItem<'a, K, V> {
    pub keyword: Spaces<'a, K>,
    pub item: V,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceHeader<'a> {
    pub before_name: &'a [CommentOrNewline<'a>],
    pub name: Loc<ModuleName<'a>>,

    pub exposes: KeywordItem<'a, ExposesKeyword, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,
    pub imports: Option<Loc<KeywordItem<'a, ImportsKeyword, ImportsCollection<'a>>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HostedHeader<'a> {
    pub before_name: &'a [CommentOrNewline<'a>],
    pub name: Loc<ModuleName<'a>>,
    pub exposes: KeywordItem<'a, ExposesKeyword, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,

    pub imports: Option<Loc<KeywordItem<'a, ImportsKeyword, ImportsCollection<'a>>>>,

    pub generates: KeywordItem<'a, GeneratesKeyword, UppercaseIdent<'a>>,
    pub generates_with:
        KeywordItem<'a, WithKeyword, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum To<'a> {
    ExistingPackage(&'a str),
    NewPackage(PackageName<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AppHeader<'a> {
    pub before_name: &'a [CommentOrNewline<'a>],
    pub name: Loc<StrLiteral<'a>>,

    pub packages:
        Option<KeywordItem<'a, PackagesKeyword, Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>>>,
    pub imports: Option<Loc<KeywordItem<'a, ImportsKeyword, ImportsCollection<'a>>>>,
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
    pub name: Loc<PackageName<'a>>,

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
    pub name: Loc<PackageName<'a>>,

    pub requires: KeywordItem<'a, RequiresKeyword, PlatformRequires<'a>>,
    pub exposes: KeywordItem<'a, ExposesKeyword, Collection<'a, Loc<Spaced<'a, ModuleName<'a>>>>>,
    pub packages:
        KeywordItem<'a, PackagesKeyword, Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>>,
    pub imports: Option<Loc<KeywordItem<'a, ImportsKeyword, ImportsCollection<'a>>>>,
    pub provides:
        KeywordItem<'a, ProvidesKeyword, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,
}

pub type ImportsCollection<'a> = Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>;

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

    /// e.g "path/to/my/file.txt" as myFile : Str
    IngestedFile(StrLiteral<'a>, Spaced<'a, TypedIdent<'a>>),
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
    pub package_name: Loc<PackageName<'a>>,
}

pub fn package_entry<'a>() -> impl Parser<'a, Spaced<'a, PackageEntry<'a>>, EPackageEntry<'a>> {
    map_with_arena!(
        // You may optionally have a package shorthand,
        // e.g. "uc" in `uc: roc/unicode 1.0.0`
        //
        // (Indirect dependencies don't have a shorthand.)
        and!(
            optional(and!(
                skip_second!(
                    and!(
                        specialize(|_, pos| EPackageEntry::Shorthand(pos), lowercase_ident()),
                        space0_e(EPackageEntry::IndentPackage)
                    ),
                    word1(b':', EPackageEntry::Colon)
                ),
                space0_e(EPackageEntry::IndentPackage)
            )),
            loc!(specialize(EPackageEntry::BadPackage, package_name()))
        ),
        move |arena, (opt_shorthand, package_or_path)| {
            let entry = match opt_shorthand {
                Some(((shorthand, spaces_before_colon), spaces_after_colon)) => PackageEntry {
                    shorthand,
                    spaces_after_shorthand: merge_spaces(
                        arena,
                        spaces_before_colon,
                        spaces_after_colon,
                    ),
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
        loc!(specialize(
            EPackageName::BadPath,
            string_literal::parse_str_literal()
        )),
        move |_arena, state, progress, text| match text.value {
            StrLiteral::PlainLine(text) => Ok((progress, PackageName(text), state)),
            StrLiteral::Line(_) => Err((progress, EPackageName::Escapes(text.region.start()))),
            StrLiteral::Block(_) => Err((progress, EPackageName::Multiline(text.region.start()))),
        },
    )
}

impl<'a, K, V> Malformed for KeywordItem<'a, K, V>
where
    K: Malformed,
    V: Malformed,
{
    fn is_malformed(&self) -> bool {
        self.keyword.is_malformed() || self.item.is_malformed()
    }
}

impl<'a> Malformed for InterfaceHeader<'a> {
    fn is_malformed(&self) -> bool {
        false
    }
}

impl<'a> Malformed for HostedHeader<'a> {
    fn is_malformed(&self) -> bool {
        false
    }
}

impl<'a> Malformed for AppHeader<'a> {
    fn is_malformed(&self) -> bool {
        self.name.is_malformed()
    }
}

impl<'a> Malformed for PackageHeader<'a> {
    fn is_malformed(&self) -> bool {
        false
    }
}

impl<'a> Malformed for PlatformRequires<'a> {
    fn is_malformed(&self) -> bool {
        self.signature.is_malformed()
    }
}

impl<'a> Malformed for PlatformHeader<'a> {
    fn is_malformed(&self) -> bool {
        false
    }
}

impl<'a> Malformed for TypedIdent<'a> {
    fn is_malformed(&self) -> bool {
        self.ann.is_malformed()
    }
}
