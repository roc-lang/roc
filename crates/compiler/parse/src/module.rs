use crate::ast::{Collection, CommentOrNewline, Defs, Module, Spaced};
use crate::blankspace::{space0_around_ee, space0_before_e, space0_e};
use crate::header::{
    package_entry, package_name, AppHeader, ExposedName, HostedHeader, ImportsEntry,
    InterfaceHeader, ModuleName, PackageEntry, PlatformHeader, PlatformRequires, To, TypedIdent,
};
use crate::ident::{self, lowercase_ident, unqualified_ident, uppercase, UppercaseIdent};
use crate::parser::Progress::{self, *};
use crate::parser::{
    backtrackable, increment_min_indent, optional, reset_min_indent, specialize, word1, EExposes,
    EGenerates, EGeneratesWith, EHeader, EImports, EPackages, EProvides, ERequires, ETypedIdent,
    Parser, SourceError, SpaceProblem, SyntaxError,
};
use crate::state::State;
use crate::string_literal;
use crate::type_annotation;
use roc_region::all::{Loc, Position};

fn end_of_file<'a>() -> impl Parser<'a, (), SyntaxError<'a>> {
    |_arena, state: State<'a>, _min_indent: u32| {
        if state.has_reached_end() {
            Ok((NoProgress, (), state))
        } else {
            Err((NoProgress, SyntaxError::NotEndOfFile(state.pos())))
        }
    }
}

#[inline(always)]
pub fn module_defs<'a>() -> impl Parser<'a, Defs<'a>, SyntaxError<'a>> {
    skip_second!(
        specialize(SyntaxError::Expr, crate::expr::toplevel_defs(),),
        end_of_file()
    )
}

pub fn parse_header<'a>(
    arena: &'a bumpalo::Bump,
    state: State<'a>,
) -> Result<(Module<'a>, State<'a>), SourceError<'a, EHeader<'a>>> {
    let min_indent = 0;
    match header().parse(arena, state.clone(), min_indent) {
        Ok((_, module, state)) => Ok((module, state)),
        Err((_, fail)) => Err(SourceError::new(fail, &state)),
    }
}

fn header<'a>() -> impl Parser<'a, Module<'a>, EHeader<'a>> {
    use crate::parser::keyword_e;

    type Clos<'b> = Box<(dyn FnOnce(&'b [CommentOrNewline]) -> Module<'b> + 'b)>;

    map!(
        and!(
            space0_e(EHeader::IndentStart),
            one_of![
                map!(
                    skip_first!(
                        keyword_e("interface", EHeader::Start),
                        increment_min_indent(interface_header())
                    ),
                    |mut header: InterfaceHeader<'a>| -> Clos<'a> {
                        Box::new(|spaces| {
                            header.before_header = spaces;
                            Module::Interface { header }
                        })
                    }
                ),
                map!(
                    skip_first!(
                        keyword_e("app", EHeader::Start),
                        increment_min_indent(app_header())
                    ),
                    |mut header: AppHeader<'a>| -> Clos<'a> {
                        Box::new(|spaces| {
                            header.before_header = spaces;
                            Module::App { header }
                        })
                    }
                ),
                map!(
                    skip_first!(
                        keyword_e("platform", EHeader::Start),
                        increment_min_indent(platform_header())
                    ),
                    |mut header: PlatformHeader<'a>| -> Clos<'a> {
                        Box::new(|spaces| {
                            header.before_header = spaces;
                            Module::Platform { header }
                        })
                    }
                ),
                map!(
                    skip_first!(
                        keyword_e("hosted", EHeader::Start),
                        increment_min_indent(hosted_header())
                    ),
                    |mut header: HostedHeader<'a>| -> Clos<'a> {
                        Box::new(|spaces| {
                            header.before_header = spaces;
                            Module::Hosted { header }
                        })
                    }
                )
            ]
        ),
        |(spaces, make_header): (&'a [CommentOrNewline], Clos<'a>)| { make_header(spaces) }
    )
}

#[inline(always)]
fn interface_header<'a>() -> impl Parser<'a, InterfaceHeader<'a>, EHeader<'a>> {
    |arena, state, min_indent: u32| {
        let (_, after_interface_keyword, state) =
            space0_e(EHeader::IndentStart).parse(arena, state, min_indent)?;
        let (_, name, state) =
            loc!(module_name_help(EHeader::ModuleName)).parse(arena, state, min_indent)?;

        let (_, ((before_exposes, after_exposes), exposes), state) =
            specialize(EHeader::Exposes, exposes_values()).parse(arena, state, min_indent)?;
        let (_, ((before_imports, after_imports), imports), state) =
            specialize(EHeader::Imports, imports()).parse(arena, state, min_indent)?;

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

#[inline(always)]
fn hosted_header<'a>() -> impl Parser<'a, HostedHeader<'a>, EHeader<'a>> {
    |arena, state, min_indent: u32| {
        let (_, after_hosted_keyword, state) =
            space0_e(EHeader::IndentStart).parse(arena, state, min_indent)?;
        let (_, name, state) =
            loc!(module_name_help(EHeader::ModuleName)).parse(arena, state, min_indent)?;

        let (_, ((before_exposes, after_exposes), exposes), state) =
            specialize(EHeader::Exposes, exposes_values()).parse(arena, state, min_indent)?;
        let (_, ((before_imports, after_imports), imports), state) =
            specialize(EHeader::Imports, imports()).parse(arena, state, min_indent)?;
        let (_, ((before_generates, after_generates), generates), state) =
            specialize(EHeader::Generates, generates()).parse(arena, state, min_indent)?;
        let (_, ((before_with, after_with), generates_with), state) =
            specialize(EHeader::GeneratesWith, generates_with()).parse(arena, state, min_indent)?;

        let header = HostedHeader {
            name,
            exposes,
            imports,
            generates,
            generates_with,
            before_header: &[] as &[_],
            after_hosted_keyword,
            before_exposes,
            after_exposes,
            before_imports,
            after_imports,
            before_generates,
            after_generates,
            before_with,
            after_with,
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
    |_, mut state: State<'a>, _min_indent: u32| match chomp_module_name(state.bytes()) {
        Ok(name) => {
            let width = name.len();
            state = state.advance(width);

            Ok((MadeProgress, ModuleName::new(name), state))
        }
        Err(progress) => Err((progress, ())),
    }
}

#[inline(always)]
fn app_header<'a>() -> impl Parser<'a, AppHeader<'a>, EHeader<'a>> {
    |arena, state, min_indent: u32| {
        let (_, after_app_keyword, state) =
            space0_e(EHeader::IndentStart).parse(arena, state, min_indent)?;
        let (_, name, state) = loc!(crate::parser::specialize(
            EHeader::AppName,
            string_literal::parse()
        ))
        .parse(arena, state, min_indent)?;

        let (_, opt_pkgs, state) =
            maybe!(specialize(EHeader::Packages, packages())).parse(arena, state, min_indent)?;
        let (_, opt_imports, state) =
            maybe!(specialize(EHeader::Imports, imports())).parse(arena, state, min_indent)?;
        let (_, provides, state) =
            specialize(EHeader::Provides, provides_to()).parse(arena, state, min_indent)?;

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
            Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>,
        )> = opt_imports;

        let ((before_imports, after_imports), imports) =
            opt_imports.unwrap_or_else(|| ((&[] as _, &[] as _), Collection::empty()));
        let provides: ProvidesTo<'a> = provides; // rustc must be told the type here

        let header = AppHeader {
            name,
            packages,
            imports,
            provides: provides.entries,
            provides_types: provides.types,
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
    |arena, state, min_indent: u32| {
        let (_, after_platform_keyword, state) =
            space0_e(EHeader::IndentStart).parse(arena, state, min_indent)?;
        let (_, name, state) = loc!(specialize(EHeader::PlatformName, package_name()))
            .parse(arena, state, min_indent)?;

        let (_, ((before_requires, after_requires), requires), state) =
            specialize(EHeader::Requires, requires()).parse(arena, state, min_indent)?;

        let (_, ((before_exposes, after_exposes), exposes), state) =
            specialize(EHeader::Exposes, exposes_modules()).parse(arena, state, min_indent)?;

        let (_, packages, state) =
            specialize(EHeader::Packages, packages()).parse(arena, state, min_indent)?;

        let (_, ((before_imports, after_imports), imports), state) =
            specialize(EHeader::Imports, imports()).parse(arena, state, min_indent)?;

        let (_, ((before_provides, after_provides), (provides, _provides_type)), state) =
            specialize(EHeader::Provides, provides_without_to()).parse(arena, state, min_indent)?;

        let header = PlatformHeader {
            name,
            requires,
            exposes,
            packages: packages.entries,
            imports,
            provides,
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
    entries: Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    types: Option<Collection<'a, Loc<Spaced<'a, UppercaseIdent<'a>>>>>,
    to: Loc<To<'a>>,

    before_provides_keyword: &'a [CommentOrNewline<'a>],
    after_provides_keyword: &'a [CommentOrNewline<'a>],
    before_to_keyword: &'a [CommentOrNewline<'a>],
    after_to_keyword: &'a [CommentOrNewline<'a>],
}

fn provides_to_package<'a>() -> impl Parser<'a, To<'a>, EProvides<'a>> {
    one_of![
        specialize(
            |_, pos| EProvides::Identifier(pos),
            map!(lowercase_ident(), To::ExistingPackage)
        ),
        specialize(EProvides::Package, map!(package_name(), To::NewPackage))
    ]
}

#[inline(always)]
fn provides_to<'a>() -> impl Parser<'a, ProvidesTo<'a>, EProvides<'a>> {
    map!(
        and!(
            provides_without_to(),
            and!(
                spaces_around_keyword(
                    "to",
                    EProvides::To,
                    EProvides::IndentTo,
                    EProvides::IndentListStart
                ),
                loc!(provides_to_package())
            )
        ),
        |(
            ((before_provides_keyword, after_provides_keyword), (entries, provides_types)),
            ((before_to_keyword, after_to_keyword), to),
        )| {
            ProvidesTo {
                entries,
                types: provides_types,
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
        (
            Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
            Option<Collection<'a, Loc<Spaced<'a, UppercaseIdent<'a>>>>>,
        ),
    ),
    EProvides<'a>,
> {
    and!(
        spaces_around_keyword(
            "provides",
            EProvides::Provides,
            EProvides::IndentProvides,
            EProvides::IndentListStart
        ),
        and!(
            collection_trailing_sep_e!(
                word1(b'[', EProvides::ListStart),
                exposes_entry(EProvides::Identifier),
                word1(b',', EProvides::ListEnd),
                word1(b']', EProvides::ListEnd),
                EProvides::IndentListEnd,
                Spaced::SpaceBefore
            ),
            // Optionally
            optional(provides_types())
        )
    )
}

#[inline(always)]
fn provides_types<'a>(
) -> impl Parser<'a, Collection<'a, Loc<Spaced<'a, UppercaseIdent<'a>>>>, EProvides<'a>> {
    skip_first!(
        // We only support spaces here, not newlines, because this is not intended
        // to be the design forever. Someday it will hopefully work like Elm,
        // where platform authors can provide functions like Browser.sandbox which
        // present an API based on ordinary-looking type variables.
        zero_or_more!(word1(
            b' ',
            // HACK: If this errors, EProvides::Provides is not an accurate reflection
            // of what went wrong. However, this is both skipped and zero_or_more,
            // so this error should never be visible to anyone in practice!
            EProvides::Provides
        )),
        collection_trailing_sep_e!(
            word1(b'{', EProvides::ListStart),
            provides_type_entry(EProvides::Identifier),
            word1(b',', EProvides::ListEnd),
            word1(b'}', EProvides::ListEnd),
            EProvides::IndentListEnd,
            Spaced::SpaceBefore
        )
    )
}

fn provides_type_entry<'a, F, E>(
    to_expectation: F,
) -> impl Parser<'a, Loc<Spaced<'a, UppercaseIdent<'a>>>, E>
where
    F: Fn(Position) -> E,
    F: Copy,
    E: 'a,
{
    loc!(map!(
        specialize(|_, pos| to_expectation(pos), ident::uppercase()),
        Spaced::Item
    ))
}

fn exposes_entry<'a, F, E>(
    to_expectation: F,
) -> impl Parser<'a, Loc<Spaced<'a, ExposedName<'a>>>, E>
where
    F: Fn(Position) -> E,
    F: Copy,
    E: 'a,
{
    loc!(map!(
        specialize(|_, pos| to_expectation(pos), unqualified_ident()),
        |n| Spaced::Item(ExposedName::new(n))
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
    and!(
        spaces_around_keyword(
            "requires",
            ERequires::Requires,
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
            skip_second!(requires_rigids(), space0_e(ERequires::ListStart)),
            requires_typed_ident()
        ),
        |(rigids, signature)| { PlatformRequires { rigids, signature } }
    )
}

#[inline(always)]
fn requires_rigids<'a>(
) -> impl Parser<'a, Collection<'a, Loc<Spaced<'a, UppercaseIdent<'a>>>>, ERequires<'a>> {
    collection_trailing_sep_e!(
        word1(b'{', ERequires::ListStart),
        specialize(
            |_, pos| ERequires::Rigid(pos),
            loc!(map!(ident::uppercase(), Spaced::Item))
        ),
        word1(b',', ERequires::ListEnd),
        word1(b'}', ERequires::ListEnd),
        ERequires::IndentListEnd,
        Spaced::SpaceBefore
    )
}

#[inline(always)]
fn requires_typed_ident<'a>() -> impl Parser<'a, Loc<Spaced<'a, TypedIdent<'a>>>, ERequires<'a>> {
    skip_first!(
        word1(b'{', ERequires::ListStart),
        skip_second!(
            reset_min_indent(space0_around_ee(
                specialize(ERequires::TypedIdent, loc!(typed_ident()),),
                ERequires::ListStart,
                ERequires::ListEnd
            )),
            word1(b'}', ERequires::ListStart)
        )
    )
}

#[inline(always)]
fn exposes_values<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    ),
    EExposes,
> {
    and!(
        spaces_around_keyword(
            "exposes",
            EExposes::Exposes,
            EExposes::IndentExposes,
            EExposes::IndentListStart
        ),
        collection_trailing_sep_e!(
            word1(b'[', EExposes::ListStart),
            exposes_entry(EExposes::Identifier),
            word1(b',', EExposes::ListEnd),
            word1(b']', EExposes::ListEnd),
            EExposes::IndentListEnd,
            Spaced::SpaceBefore
        )
    )
}

fn spaces_around_keyword<'a, E>(
    keyword: &'static str,
    expectation: fn(Position) -> E,
    indent_problem1: fn(Position) -> E,
    indent_problem2: fn(Position) -> E,
) -> impl Parser<'a, (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]), E>
where
    E: 'a + SpaceProblem,
{
    and!(
        skip_second!(
            backtrackable(space0_e(indent_problem1)),
            crate::parser::keyword_e(keyword, expectation)
        ),
        space0_e(indent_problem2)
    )
}

#[inline(always)]
fn exposes_modules<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Collection<'a, Loc<Spaced<'a, ModuleName<'a>>>>,
    ),
    EExposes,
> {
    and!(
        spaces_around_keyword(
            "exposes",
            EExposes::Exposes,
            EExposes::IndentExposes,
            EExposes::IndentListStart
        ),
        collection_trailing_sep_e!(
            word1(b'[', EExposes::ListStart),
            exposes_module(EExposes::Identifier),
            word1(b',', EExposes::ListEnd),
            word1(b']', EExposes::ListEnd),
            EExposes::IndentListEnd,
            Spaced::SpaceBefore
        )
    )
}

fn exposes_module<'a, F, E>(
    to_expectation: F,
) -> impl Parser<'a, Loc<Spaced<'a, ModuleName<'a>>>, E>
where
    F: Fn(Position) -> E,
    F: Copy,
    E: 'a,
{
    loc!(map!(
        specialize(|_, pos| to_expectation(pos), module_name()),
        Spaced::Item
    ))
}

#[derive(Debug)]
struct Packages<'a> {
    entries: Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>,
    before_packages_keyword: &'a [CommentOrNewline<'a>],
    after_packages_keyword: &'a [CommentOrNewline<'a>],
}

#[inline(always)]
fn packages<'a>() -> impl Parser<'a, Packages<'a>, EPackages<'a>> {
    map!(
        and!(
            spaces_around_keyword(
                "packages",
                EPackages::Packages,
                EPackages::IndentPackages,
                EPackages::IndentListStart
            ),
            collection_trailing_sep_e!(
                word1(b'{', EPackages::ListStart),
                specialize(EPackages::PackageEntry, loc!(package_entry())),
                word1(b',', EPackages::ListEnd),
                word1(b'}', EPackages::ListEnd),
                EPackages::IndentListEnd,
                Spaced::SpaceBefore
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
fn generates<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        UppercaseIdent<'a>,
    ),
    EGenerates,
> {
    and!(
        spaces_around_keyword(
            "generates",
            EGenerates::Generates,
            EGenerates::IndentGenerates,
            EGenerates::IndentTypeStart
        ),
        specialize(|(), pos| EGenerates::Identifier(pos), uppercase())
    )
}

#[inline(always)]
fn generates_with<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    ),
    EGeneratesWith,
> {
    and!(
        spaces_around_keyword(
            "with",
            EGeneratesWith::With,
            EGeneratesWith::IndentWith,
            EGeneratesWith::IndentListStart
        ),
        collection_trailing_sep_e!(
            word1(b'[', EGeneratesWith::ListStart),
            exposes_entry(EGeneratesWith::Identifier),
            word1(b',', EGeneratesWith::ListEnd),
            word1(b']', EGeneratesWith::ListEnd),
            EGeneratesWith::IndentListEnd,
            Spaced::SpaceBefore
        )
    )
}

#[inline(always)]
fn imports<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>,
    ),
    EImports,
> {
    and!(
        spaces_around_keyword(
            "imports",
            EImports::Imports,
            EImports::IndentImports,
            EImports::IndentListStart
        ),
        collection_trailing_sep_e!(
            word1(b'[', EImports::ListStart),
            loc!(imports_entry()),
            word1(b',', EImports::ListEnd),
            word1(b']', EImports::ListEnd),
            EImports::IndentListEnd,
            Spaced::SpaceBefore
        )
    )
}

#[inline(always)]
fn typed_ident<'a>() -> impl Parser<'a, Spaced<'a, TypedIdent<'a>>, ETypedIdent<'a>> {
    // e.g.
    //
    // printLine : Str -> Effect {}
    map!(
        and!(
            and!(
                loc!(specialize(
                    |_, pos| ETypedIdent::Identifier(pos),
                    lowercase_ident()
                )),
                space0_e(ETypedIdent::IndentHasType)
            ),
            skip_first!(
                word1(b':', ETypedIdent::HasType),
                space0_before_e(
                    specialize(
                        ETypedIdent::Type,
                        reset_min_indent(type_annotation::located(true))
                    ),
                    ETypedIdent::IndentType,
                )
            )
        ),
        |((ident, spaces_before_colon), ann)| {
            Spaced::Item(TypedIdent {
                ident,
                spaces_before_colon,
                ann,
            })
        }
    )
}

fn shortname<'a>() -> impl Parser<'a, &'a str, EImports> {
    specialize(|_, pos| EImports::Shorthand(pos), lowercase_ident())
}

fn module_name_help<'a, F, E>(to_expectation: F) -> impl Parser<'a, ModuleName<'a>, E>
where
    F: Fn(Position) -> E,
    E: 'a,
    F: 'a,
{
    specialize(move |_, pos| to_expectation(pos), module_name())
}

#[inline(always)]
fn imports_entry<'a>() -> impl Parser<'a, Spaced<'a, ImportsEntry<'a>>, EImports> {
    type Temp<'a> = (
        (Option<&'a str>, ModuleName<'a>),
        Option<Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,
    );

    map_with_arena!(
        and!(
            and!(
                // e.g. `pf.`
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
                    EImports::IndentSetEnd,
                    Spaced::SpaceBefore
                )
            ))
        ),
        |_arena, ((opt_shortname, module_name), opt_values): Temp<'a>| {
            let exposed_values = opt_values.unwrap_or_else(Collection::empty);

            let entry = match opt_shortname {
                Some(shortname) => ImportsEntry::Package(shortname, module_name, exposed_values),

                None => ImportsEntry::Module(module_name, exposed_values),
            };

            Spaced::Item(entry)
        }
    )
}
