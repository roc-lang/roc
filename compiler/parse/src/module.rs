use crate::ast::{Collection, CommentOrNewline, Def, Module};
use crate::blankspace::{space0_around_ee, space0_before_e, space0_e};
use crate::header::{
    package_entry, package_name, package_or_path, AppHeader, Effects, ExposesEntry, ImportsEntry,
    InterfaceHeader, ModuleName, PackageEntry, PlatformHeader, PlatformRequires, PlatformRigid, To,
    TypedIdent,
};
use crate::ident::{lowercase_ident, unqualified_ident, uppercase_ident};
use crate::parser::Progress::{self, *};
use crate::parser::{
    backtrackable, specialize, word1, word2, Col, EEffects, EExposes, EHeader, EImports, EPackages,
    EProvides, ERequires, ETypedIdent, Parser, Row, State, SyntaxError,
};
use crate::string_literal;
use crate::type_annotation;
use bumpalo::collections::Vec;
use roc_region::all::Located;

fn end_of_file<'a>() -> impl Parser<'a, (), SyntaxError<'a>> {
    |_arena, state: State<'a>| {
        if state.has_reached_end() {
            Ok((NoProgress, (), state))
        } else {
            Err((
                NoProgress,
                SyntaxError::NotEndOfFile(state.line, state.column),
                state,
            ))
        }
    }
}

#[inline(always)]
pub fn module_defs<'a>() -> impl Parser<'a, Vec<'a, Located<Def<'a>>>, SyntaxError<'a>> {
    // force that we parse until the end of the input
    let min_indent = 0;
    skip_second!(
        specialize(
            |e, _, _| SyntaxError::Expr(e),
            crate::expr::defs(min_indent),
        ),
        end_of_file()
    )
}

pub fn parse_header<'a>(
    arena: &'a bumpalo::Bump,
    state: State<'a>,
) -> Result<(Module<'a>, State<'a>), EHeader<'a>> {
    match header().parse(arena, state) {
        Ok((_, module, state)) => Ok((module, state)),
        Err((_, fail, _)) => Err(fail),
    }
}

fn header<'a>() -> impl Parser<'a, Module<'a>, EHeader<'a>> {
    use crate::parser::keyword_e;

    one_of![
        map!(
            and!(
                space0_e(0, EHeader::Space, EHeader::IndentStart),
                skip_first!(keyword_e("app", EHeader::Start), app_header())
            ),
            |(spaces, mut header): (&'a [CommentOrNewline], AppHeader<'a>)| {
                header.before_header = spaces;

                Module::App { header }
            }
        ),
        map!(
            and!(
                space0_e(0, EHeader::Space, EHeader::IndentStart),
                skip_first!(keyword_e("platform", EHeader::Start), platform_header())
            ),
            |(spaces, mut header): (&'a [CommentOrNewline], PlatformHeader<'a>)| {
                header.before_header = spaces;

                Module::Platform { header }
            }
        ),
        map!(
            and!(
                space0_e(0, EHeader::Space, EHeader::IndentStart),
                skip_first!(keyword_e("interface", EHeader::Start), interface_header())
            ),
            |(spaces, mut header): (&'a [CommentOrNewline], InterfaceHeader<'a>)| {
                header.before_header = spaces;

                Module::Interface { header }
            }
        )
    ]
}

#[inline(always)]
fn interface_header<'a>() -> impl Parser<'a, InterfaceHeader<'a>, EHeader<'a>> {
    |arena, state| {
        let min_indent = 1;

        let (_, after_interface_keyword, state) =
            space0_e(min_indent, EHeader::Space, EHeader::IndentStart).parse(arena, state)?;
        let (_, name, state) = loc!(module_name_help(EHeader::ModuleName)).parse(arena, state)?;

        let (_, ((before_exposes, after_exposes), exposes), state) =
            specialize(EHeader::Exposes, exposes_values()).parse(arena, state)?;
        let (_, ((before_imports, after_imports), imports), state) =
            specialize(EHeader::Imports, imports()).parse(arena, state)?;

        let header = InterfaceHeader {
            name,
            exposes,
            imports,
            before_header: &[] as &[_],
            after_interface_keyword,
            before_exposes,
            after_exposes,
            before_imports,
            after_imports,
        };

        Ok((MadeProgress, header, state))
    }
}

fn chomp_module_name(buffer: &[u8]) -> Result<&str, Progress> {
    use encode_unicode::CharExt;

    let mut chomped = 0;

    if let Ok((first_letter, width)) = char::from_utf8_slice_start(&buffer[chomped..]) {
        if first_letter.is_uppercase() {
            chomped += width;
        } else {
            return Err(Progress::NoProgress);
        }
    }

    while let Ok((ch, width)) = char::from_utf8_slice_start(&buffer[chomped..]) {
        // After the first character, only these are allowed:
        //
        // * Unicode alphabetic chars - you might include `鹏` if that's clear to your readers
        // * ASCII digits - e.g. `1` but not `¾`, both of which pass .is_numeric()
        // * A '.' separating module parts
        if ch.is_alphabetic() || ch.is_ascii_digit() {
            chomped += width;
        } else if ch == '.' {
            chomped += width;

            if let Ok((first_letter, width)) = char::from_utf8_slice_start(&buffer[chomped..]) {
                if first_letter.is_uppercase() {
                    chomped += width;
                } else if first_letter == '{' {
                    // the .{ starting a `Foo.{ bar, baz }` importing clauses
                    chomped -= width;
                    break;
                } else {
                    return Err(Progress::MadeProgress);
                }
            }
        } else {
            // we're done
            break;
        }
    }

    let name = unsafe { std::str::from_utf8_unchecked(&buffer[..chomped]) };

    Ok(name)
}

#[inline(always)]
fn module_name<'a>() -> impl Parser<'a, ModuleName<'a>, ()> {
    |_, mut state: State<'a>| match chomp_module_name(state.bytes) {
        Ok(name) => {
            let width = name.len();
            state.column += width as u16;
            state.bytes = &state.bytes[width..];

            Ok((MadeProgress, ModuleName::new(name), state))
        }
        Err(progress) => Err((progress, (), state)),
    }
}

#[inline(always)]
fn app_header<'a>() -> impl Parser<'a, AppHeader<'a>, EHeader<'a>> {
    |arena, state| {
        let min_indent = 1;

        let (_, after_app_keyword, state) =
            space0_e(min_indent, EHeader::Space, EHeader::IndentStart).parse(arena, state)?;
        let (_, name, state) = loc!(crate::parser::specialize(
            EHeader::AppName,
            string_literal::parse()
        ))
        .parse(arena, state)?;

        let (_, opt_pkgs, state) =
            maybe!(specialize(EHeader::Packages, packages())).parse(arena, state)?;
        let (_, opt_imports, state) =
            maybe!(specialize(EHeader::Imports, imports())).parse(arena, state)?;
        let (_, provides, state) =
            specialize(EHeader::Provides, provides_to()).parse(arena, state)?;

        let (before_packages, after_packages, packages) = match opt_pkgs {
            Some(pkgs) => {
                let pkgs: Packages<'a> = pkgs; // rustc must be told the type here

                (
                    pkgs.before_packages_keyword,
                    pkgs.after_packages_keyword,
                    pkgs.entries,
                )
            }
            None => (&[] as _, &[] as _, Collection::empty()),
        };

        // rustc must be told the type here
        #[allow(clippy::type_complexity)]
        let opt_imports: Option<(
            (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
            Collection<'a, Located<ImportsEntry<'a>>>,
        )> = opt_imports;

        let ((before_imports, after_imports), imports) =
            opt_imports.unwrap_or_else(|| ((&[] as _, &[] as _), Collection::empty()));
        let provides: ProvidesTo<'a> = provides; // rustc must be told the type here

        let header = AppHeader {
            name,
            packages,
            imports,
            provides: provides.entries,
            to: provides.to,
            before_header: &[] as &[_],
            after_app_keyword,
            before_packages,
            after_packages,
            before_imports,
            after_imports,
            before_provides: provides.before_provides_keyword,
            after_provides: provides.after_provides_keyword,
            before_to: provides.before_to_keyword,
            after_to: provides.after_to_keyword,
        };

        Ok((MadeProgress, header, state))
    }
}

#[inline(always)]
fn platform_header<'a>() -> impl Parser<'a, PlatformHeader<'a>, EHeader<'a>> {
    |arena, state| {
        let min_indent = 1;

        let (_, after_platform_keyword, state) =
            space0_e(min_indent, EHeader::Space, EHeader::IndentStart).parse(arena, state)?;
        let (_, name, state) =
            loc!(specialize(EHeader::PlatformName, package_name())).parse(arena, state)?;

        let (_, ((before_requires, after_requires), requires), state) =
            specialize(EHeader::Requires, requires()).parse(arena, state)?;

        let (_, ((before_exposes, after_exposes), exposes), state) =
            specialize(EHeader::Exposes, exposes_modules()).parse(arena, state)?;

        let (_, packages, state) = specialize(EHeader::Packages, packages()).parse(arena, state)?;

        let (_, ((before_imports, after_imports), imports), state) =
            specialize(EHeader::Imports, imports()).parse(arena, state)?;

        let (_, ((before_provides, after_provides), provides), state) =
            specialize(EHeader::Provides, provides_without_to()).parse(arena, state)?;

        let (_, effects, state) = specialize(EHeader::Effects, effects()).parse(arena, state)?;

        let header = PlatformHeader {
            name,
            requires,
            exposes,
            packages: packages.entries,
            imports,
            provides,
            effects,
            before_header: &[] as &[_],
            after_platform_keyword,
            before_requires,
            after_requires,
            before_exposes,
            after_exposes,
            before_packages: packages.before_packages_keyword,
            after_packages: packages.after_packages_keyword,
            before_imports,
            after_imports,
            before_provides,
            after_provides,
        };

        Ok((MadeProgress, header, state))
    }
}

#[derive(Debug)]
struct ProvidesTo<'a> {
    entries: Collection<'a, Located<ExposesEntry<'a, &'a str>>>,
    to: Located<To<'a>>,

    before_provides_keyword: &'a [CommentOrNewline<'a>],
    after_provides_keyword: &'a [CommentOrNewline<'a>],
    before_to_keyword: &'a [CommentOrNewline<'a>],
    after_to_keyword: &'a [CommentOrNewline<'a>],
}

fn provides_to_package<'a>() -> impl Parser<'a, To<'a>, EProvides<'a>> {
    one_of![
        specialize(
            |_, r, c| EProvides::Identifier(r, c),
            map!(lowercase_ident(), To::ExistingPackage)
        ),
        specialize(EProvides::Package, map!(package_or_path(), To::NewPackage))
    ]
}

#[inline(always)]
fn provides_to<'a>() -> impl Parser<'a, ProvidesTo<'a>, EProvides<'a>> {
    let min_indent = 1;

    map!(
        and!(
            provides_without_to(),
            and!(
                spaces_around_keyword(
                    min_indent,
                    "to",
                    EProvides::To,
                    EProvides::Space,
                    EProvides::IndentTo,
                    EProvides::IndentListStart
                ),
                loc!(provides_to_package())
            )
        ),
        |(
            ((before_provides_keyword, after_provides_keyword), entries),
            ((before_to_keyword, after_to_keyword), to),
        )| {
            ProvidesTo {
                entries,
                to,
                before_provides_keyword,
                after_provides_keyword,
                before_to_keyword,
                after_to_keyword,
            }
        }
    )
}

#[inline(always)]
fn provides_without_to<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Collection<'a, Located<ExposesEntry<'a, &'a str>>>,
    ),
    EProvides<'a>,
> {
    let min_indent = 1;
    and!(
        spaces_around_keyword(
            min_indent,
            "provides",
            EProvides::Provides,
            EProvides::Space,
            EProvides::IndentProvides,
            EProvides::IndentListStart
        ),
        collection_trailing_sep_e!(
            word1(b'[', EProvides::ListStart),
            exposes_entry(EProvides::Identifier),
            word1(b',', EProvides::ListEnd),
            word1(b']', EProvides::ListEnd),
            min_indent,
            EProvides::Open,
            EProvides::Space,
            EProvides::IndentListEnd,
            ExposesEntry::SpaceBefore
        )
    )
}

fn exposes_entry<'a, F, E>(
    to_expectation: F,
) -> impl Parser<'a, Located<ExposesEntry<'a, &'a str>>, E>
where
    F: Fn(crate::parser::Row, crate::parser::Col) -> E,
    F: Copy,
    E: 'a,
{
    loc!(map!(
        specialize(|_, r, c| to_expectation(r, c), unqualified_ident()),
        ExposesEntry::Exposed
    ))
}

#[inline(always)]
fn requires<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        PlatformRequires<'a>,
    ),
    ERequires<'a>,
> {
    let min_indent = 0;
    and!(
        spaces_around_keyword(
            min_indent,
            "requires",
            ERequires::Requires,
            ERequires::Space,
            ERequires::IndentRequires,
            ERequires::IndentListStart
        ),
        platform_requires()
    )
}

#[inline(always)]
fn platform_requires<'a>() -> impl Parser<'a, PlatformRequires<'a>, ERequires<'a>> {
    map!(
        and!(
            skip_second!(
                requires_rigids(0),
                space0_e(0, ERequires::Space, ERequires::ListStart)
            ),
            requires_typed_ident()
        ),
        |(rigids, signature)| { PlatformRequires { rigids, signature } }
    )
}

#[inline(always)]
fn requires_rigids<'a>(
    min_indent: u16,
) -> impl Parser<'a, Collection<'a, Located<PlatformRigid<'a>>>, ERequires<'a>> {
    collection_trailing_sep_e!(
        word1(b'{', ERequires::ListStart),
        specialize(|_, r, c| ERequires::Rigid(r, c), loc!(requires_rigid())),
        word1(b',', ERequires::ListEnd),
        word1(b'}', ERequires::ListEnd),
        min_indent,
        ERequires::Open,
        ERequires::Space,
        ERequires::IndentListEnd,
        PlatformRigid::SpaceBefore
    )
}

#[inline(always)]
fn requires_rigid<'a>() -> impl Parser<'a, PlatformRigid<'a>, ()> {
    map!(
        and!(
            lowercase_ident(),
            skip_first!(word2(b'=', b'>', |_, _| ()), uppercase_ident())
        ),
        |(rigid, alias)| PlatformRigid::Entry { rigid, alias }
    )
}

#[inline(always)]
fn requires_typed_ident<'a>() -> impl Parser<'a, Located<TypedIdent<'a>>, ERequires<'a>> {
    skip_first!(
        word1(b'{', ERequires::ListStart),
        skip_second!(
            space0_around_ee(
                specialize(ERequires::TypedIdent, loc!(typed_ident()),),
                0,
                ERequires::Space,
                ERequires::ListStart,
                ERequires::ListEnd
            ),
            word1(b'}', ERequires::ListStart)
        )
    )
}

#[inline(always)]
fn exposes_values<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Collection<'a, Located<ExposesEntry<'a, &'a str>>>,
    ),
    EExposes,
> {
    let min_indent = 1;

    and!(
        spaces_around_keyword(
            min_indent,
            "exposes",
            EExposes::Exposes,
            EExposes::Space,
            EExposes::IndentExposes,
            EExposes::IndentListStart
        ),
        collection_trailing_sep_e!(
            word1(b'[', EExposes::ListStart),
            exposes_entry(EExposes::Identifier),
            word1(b',', EExposes::ListEnd),
            word1(b']', EExposes::ListEnd),
            min_indent,
            EExposes::Open,
            EExposes::Space,
            EExposes::IndentListEnd,
            ExposesEntry::SpaceBefore
        )
    )
}

fn spaces_around_keyword<'a, E>(
    min_indent: u16,
    keyword: &'static str,
    expectation: fn(Row, Col) -> E,
    space_problem: fn(crate::parser::BadInputError, Row, Col) -> E,
    indent_problem1: fn(Row, Col) -> E,
    indent_problem2: fn(Row, Col) -> E,
) -> impl Parser<'a, (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]), E>
where
    E: 'a,
{
    and!(
        skip_second!(
            backtrackable(space0_e(min_indent, space_problem, indent_problem1)),
            crate::parser::keyword_e(keyword, expectation)
        ),
        space0_e(min_indent, space_problem, indent_problem2)
    )
}

#[inline(always)]
fn exposes_modules<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Collection<'a, Located<ExposesEntry<'a, ModuleName<'a>>>>,
    ),
    EExposes,
> {
    let min_indent = 1;

    and!(
        spaces_around_keyword(
            min_indent,
            "exposes",
            EExposes::Exposes,
            EExposes::Space,
            EExposes::IndentExposes,
            EExposes::IndentListStart
        ),
        collection_trailing_sep_e!(
            word1(b'[', EExposes::ListStart),
            exposes_module(EExposes::Identifier),
            word1(b',', EExposes::ListEnd),
            word1(b']', EExposes::ListEnd),
            min_indent,
            EExposes::Open,
            EExposes::Space,
            EExposes::IndentListEnd,
            ExposesEntry::SpaceBefore
        )
    )
}

fn exposes_module<'a, F, E>(
    to_expectation: F,
) -> impl Parser<'a, Located<ExposesEntry<'a, ModuleName<'a>>>, E>
where
    F: Fn(crate::parser::Row, crate::parser::Col) -> E,
    F: Copy,
    E: 'a,
{
    loc!(map!(
        specialize(|_, r, c| to_expectation(r, c), module_name()),
        ExposesEntry::Exposed
    ))
}

#[derive(Debug)]
struct Packages<'a> {
    entries: Collection<'a, Located<PackageEntry<'a>>>,
    before_packages_keyword: &'a [CommentOrNewline<'a>],
    after_packages_keyword: &'a [CommentOrNewline<'a>],
}

#[inline(always)]
fn packages<'a>() -> impl Parser<'a, Packages<'a>, EPackages<'a>> {
    let min_indent = 1;

    map!(
        and!(
            spaces_around_keyword(
                min_indent,
                "packages",
                EPackages::Packages,
                EPackages::Space,
                EPackages::IndentPackages,
                EPackages::IndentListStart
            ),
            collection_trailing_sep_e!(
                word1(b'{', EPackages::ListStart),
                specialize(EPackages::PackageEntry, loc!(package_entry())),
                word1(b',', EPackages::ListEnd),
                word1(b'}', EPackages::ListEnd),
                min_indent,
                EPackages::Open,
                EPackages::Space,
                EPackages::IndentListEnd,
                PackageEntry::SpaceBefore
            )
        ),
        |((before_packages_keyword, after_packages_keyword), entries): (
            (_, _),
            Collection<'a, _>
        )| {
            Packages {
                entries,
                before_packages_keyword,
                after_packages_keyword,
            }
        }
    )
}

#[inline(always)]
fn imports<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Collection<'a, Located<ImportsEntry<'a>>>,
    ),
    EImports,
> {
    let min_indent = 1;

    and!(
        spaces_around_keyword(
            min_indent,
            "imports",
            EImports::Imports,
            EImports::Space,
            EImports::IndentImports,
            EImports::IndentListStart
        ),
        collection_trailing_sep_e!(
            word1(b'[', EImports::ListStart),
            loc!(imports_entry()),
            word1(b',', EImports::ListEnd),
            word1(b']', EImports::ListEnd),
            min_indent,
            EImports::Open,
            EImports::Space,
            EImports::IndentListEnd,
            ImportsEntry::SpaceBefore
        )
    )
}

#[inline(always)]
fn effects<'a>() -> impl Parser<'a, Effects<'a>, EEffects<'a>> {
    move |arena, state| {
        let min_indent = 1;

        let (_, (spaces_before_effects_keyword, spaces_after_effects_keyword), state) =
            spaces_around_keyword(
                min_indent,
                "effects",
                EEffects::Effects,
                EEffects::Space,
                EEffects::IndentEffects,
                EEffects::IndentListStart,
            )
            .parse(arena, state)?;

        // e.g. `fx.`
        let (_, type_shortname, state) = skip_second!(
            specialize(|_, r, c| EEffects::Shorthand(r, c), lowercase_ident()),
            word1(b'.', EEffects::ShorthandDot)
        )
        .parse(arena, state)?;

        // the type name, e.g. Effects
        let (_, (type_name, spaces_after_type_name), state) = and!(
            specialize(|_, r, c| EEffects::TypeName(r, c), uppercase_ident()),
            space0_e(min_indent, EEffects::Space, EEffects::IndentListStart)
        )
        .parse(arena, state)?;
        let (_, entries, state) = collection_trailing_sep_e!(
            word1(b'{', EEffects::ListStart),
            specialize(EEffects::TypedIdent, loc!(typed_ident())),
            word1(b',', EEffects::ListEnd),
            word1(b'}', EEffects::ListEnd),
            min_indent,
            EEffects::Open,
            EEffects::Space,
            EEffects::IndentListEnd,
            TypedIdent::SpaceBefore
        )
        .parse(arena, state)?;

        Ok((
            MadeProgress,
            Effects {
                spaces_before_effects_keyword,
                spaces_after_effects_keyword,
                spaces_after_type_name,
                effect_shortname: type_shortname,
                effect_type_name: type_name,
                entries: entries.items,
            },
            state,
        ))
    }
}

#[inline(always)]
fn typed_ident<'a>() -> impl Parser<'a, TypedIdent<'a>, ETypedIdent<'a>> {
    // e.g.
    //
    // printLine : Str -> Effect {}
    let min_indent = 0;

    map!(
        and!(
            and!(
                loc!(specialize(
                    |_, r, c| ETypedIdent::Identifier(r, c),
                    lowercase_ident()
                )),
                space0_e(min_indent, ETypedIdent::Space, ETypedIdent::IndentHasType)
            ),
            skip_first!(
                word1(b':', ETypedIdent::HasType),
                space0_before_e(
                    specialize(ETypedIdent::Type, type_annotation::located_help(min_indent)),
                    min_indent,
                    ETypedIdent::Space,
                    ETypedIdent::IndentType,
                )
            )
        ),
        |((ident, spaces_before_colon), ann)| {
            TypedIdent::Entry {
                ident,
                spaces_before_colon,
                ann,
            }
        }
    )
}

fn shortname<'a>() -> impl Parser<'a, &'a str, EImports> {
    specialize(|_, r, c| EImports::Shorthand(r, c), lowercase_ident())
}

fn module_name_help<'a, F, E>(to_expectation: F) -> impl Parser<'a, ModuleName<'a>, E>
where
    F: Fn(crate::parser::Row, crate::parser::Col) -> E,
    E: 'a,
    F: 'a,
{
    specialize(move |_, r, c| to_expectation(r, c), module_name())
}

#[inline(always)]
fn imports_entry<'a>() -> impl Parser<'a, ImportsEntry<'a>, EImports> {
    let min_indent = 1;

    type Temp<'a> = (
        (Option<&'a str>, ModuleName<'a>),
        Option<Collection<'a, Located<ExposesEntry<'a, &'a str>>>>,
    );

    map_with_arena!(
        and!(
            and!(
                // e.g. `base.`
                maybe!(skip_second!(
                    shortname(),
                    word1(b'.', EImports::ShorthandDot)
                )),
                // e.g. `Task`
                module_name_help(EImports::ModuleName)
            ),
            // e.g. `.{ Task, after}`
            maybe!(skip_first!(
                word1(b'.', EImports::ExposingDot),
                collection_trailing_sep_e!(
                    word1(b'{', EImports::SetStart),
                    exposes_entry(EImports::Identifier),
                    word1(b',', EImports::SetEnd),
                    word1(b'}', EImports::SetEnd),
                    min_indent,
                    EImports::Open,
                    EImports::Space,
                    EImports::IndentSetEnd,
                    ExposesEntry::SpaceBefore
                )
            ))
        ),
        |_arena, ((opt_shortname, module_name), opt_values): Temp<'a>| {
            let exposed_values = opt_values.unwrap_or_else(Collection::empty);

            match opt_shortname {
                Some(shortname) => ImportsEntry::Package(shortname, module_name, exposed_values),

                None => ImportsEntry::Module(module_name, exposed_values),
            }
        }
    )
}
