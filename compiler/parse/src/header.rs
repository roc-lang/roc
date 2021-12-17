use crate::ast::{Collection, CommentOrNewline, Spaced, StrLiteral, TypeAnnotation};
use crate::blankspace::space0_e;
use crate::ident::lowercase_ident;
use crate::parser::Progress::{self, *};
use crate::parser::{
    specialize, word1, EPackageEntry, EPackageName, EPackageOrPath, Parser, State,
};
use crate::string_literal;
use bumpalo::collections::Vec;
use roc_region::all::Loc;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct PackageName<'a> {
    pub account: &'a str,
    pub pkg: &'a str,
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

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum PackageOrPath<'a> {
    Package(PackageName<'a>, Version<'a>),
    Path(StrLiteral<'a>),
}

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
    NewPackage(PackageOrPath<'a>),
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
    pub packages: Vec<'a, (Loc<&'a str>, Loc<PackageOrPath<'a>>)>,
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
    pub package_or_path: Loc<PackageOrPath<'a>>,
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
                specialize(|_, r, c| EPackageEntry::Shorthand(r, c), lowercase_ident()),
                word1(b':', EPackageEntry::Colon)
            ),
            space0_e(
                min_indent,
                EPackageEntry::Space,
                EPackageEntry::IndentPackageOrPath
            )
        ))
        .parse(arena, state)?;

        let (_, package_or_path, state) = loc!(specialize(
            EPackageEntry::BadPackageOrPath,
            package_or_path()
        ))
        .parse(arena, state)?;

        let entry = match opt_shorthand {
            Some((shorthand, spaces_after_shorthand)) => PackageEntry {
                shorthand,
                spaces_after_shorthand,
                package_or_path,
            },
            None => PackageEntry {
                shorthand: "",
                spaces_after_shorthand: &[],
                package_or_path,
            },
        };

        Ok((MadeProgress, Spaced::Item(entry), state))
    }
}

pub fn package_or_path<'a>() -> impl Parser<'a, PackageOrPath<'a>, EPackageOrPath<'a>> {
    one_of![
        map!(
            specialize(EPackageOrPath::BadPath, string_literal::parse()),
            PackageOrPath::Path
        ),
        map!(
            and!(
                specialize(EPackageOrPath::BadPackage, package_name()),
                skip_first!(skip_spaces(), package_version())
            ),
            |(name, version)| { PackageOrPath::Package(name, version) }
        )
    ]
}

fn skip_spaces<'a, T>() -> impl Parser<'a, (), T>
where
    T: 'a,
{
    |_, mut state: State<'a>| {
        let mut chomped = 0;
        let mut it = state.bytes.iter();

        while let Some(b' ') = it.next() {
            chomped += 1;
        }

        if chomped == 0 {
            Ok((NoProgress, (), state))
        } else {
            state.column += chomped;
            state.bytes = it.as_slice();

            Ok((MadeProgress, (), state))
        }
    }
}

fn package_version<'a, T>() -> impl Parser<'a, Version<'a>, T>
where
    T: 'a,
{
    move |_, _| todo!("TODO parse package version")
}

#[inline(always)]
pub fn package_name<'a>() -> impl Parser<'a, PackageName<'a>, EPackageName> {
    use encode_unicode::CharExt;
    // e.g. rtfeldman/blah
    //
    // Package names and accounts can be capitalized and can contain dashes.
    // They cannot contain underscores or other special characters.
    // They must be ASCII.

    |_, mut state: State<'a>| match chomp_package_part(state.bytes) {
        Err(progress) => Err((
            progress,
            EPackageName::Account(state.line, state.column),
            state,
        )),
        Ok(account) => {
            let mut chomped = account.len();
            if let Ok(('/', width)) = char::from_utf8_slice_start(&state.bytes[chomped..]) {
                chomped += width;
                match chomp_package_part(&state.bytes[chomped..]) {
                    Err(progress) => Err((
                        progress,
                        EPackageName::Pkg(state.line, state.column + chomped as u16),
                        state,
                    )),
                    Ok(pkg) => {
                        chomped += pkg.len();

                        state.column += chomped as u16;
                        state.bytes = &state.bytes[chomped..];

                        let value = PackageName { account, pkg };
                        Ok((MadeProgress, value, state))
                    }
                }
            } else {
                Err((
                    MadeProgress,
                    EPackageName::MissingSlash(state.line, state.column + chomped as u16),
                    state,
                ))
            }
        }
    }
}

fn chomp_package_part(buffer: &[u8]) -> Result<&str, Progress> {
    use encode_unicode::CharExt;

    let mut chomped = 0;

    while let Ok((ch, width)) = char::from_utf8_slice_start(&buffer[chomped..]) {
        if ch == '-' || ch.is_ascii_alphanumeric() {
            chomped += width;
        } else {
            // we're done
            break;
        }
    }

    if chomped == 0 {
        Err(Progress::NoProgress)
    } else {
        let name = unsafe { std::str::from_utf8_unchecked(&buffer[..chomped]) };

        Ok(name)
    }
}
