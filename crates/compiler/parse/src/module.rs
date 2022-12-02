use crate::ast::{Collection, Defs, Header, Module, Spaced, Spaces};
use crate::blankspace::{space0_around_ee, space0_before_e, space0_e};
use crate::header::{
    package_entry, package_name, AppHeader, ExposedName, ExposesKeyword, GeneratesKeyword,
    HostedHeader, ImportsEntry, ImportsKeyword, InterfaceHeader, Keyword, KeywordItem, ModuleName,
    PackageEntry, PackageHeader, PackagesKeyword, PlatformHeader, PlatformRequires,
    ProvidesKeyword, ProvidesTo, RequiresKeyword, To, ToKeyword, TypedIdent, WithKeyword,
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

    record!(Module {
        comments: space0_e(EHeader::IndentStart),
        header: one_of![
            map!(
                skip_first!(
                    keyword_e("interface", EHeader::Start),
                    increment_min_indent(interface_header())
                ),
                Header::Interface
            ),
            map!(
                skip_first!(
                    keyword_e("app", EHeader::Start),
                    increment_min_indent(app_header())
                ),
                Header::App
            ),
            map!(
                skip_first!(
                    keyword_e("package", EHeader::Start),
                    increment_min_indent(package_header())
                ),
                Header::Package
            ),
            map!(
                skip_first!(
                    keyword_e("platform", EHeader::Start),
                    increment_min_indent(platform_header())
                ),
                Header::Platform
            ),
            map!(
                skip_first!(
                    keyword_e("hosted", EHeader::Start),
                    increment_min_indent(hosted_header())
                ),
                Header::Hosted
            ),
        ]
    })
}

#[inline(always)]
fn interface_header<'a>() -> impl Parser<'a, InterfaceHeader<'a>, EHeader<'a>> {
    record!(InterfaceHeader {
        before_name: space0_e(EHeader::IndentStart),
        name: loc!(module_name_help(EHeader::ModuleName)),
        exposes: specialize(EHeader::Exposes, exposes_values()),
        imports: specialize(EHeader::Imports, imports()),
    })
    .trace("interface_header")
}

#[inline(always)]
fn hosted_header<'a>() -> impl Parser<'a, HostedHeader<'a>, EHeader<'a>> {
    record!(HostedHeader {
        before_name: space0_e(EHeader::IndentStart),
        name: loc!(module_name_help(EHeader::ModuleName)),
        exposes: specialize(EHeader::Exposes, exposes_values()),
        imports: specialize(EHeader::Imports, imports()),
        generates: specialize(EHeader::Generates, generates()),
        generates_with: specialize(EHeader::GeneratesWith, generates_with()),
    })
    .trace("hosted_header")
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
    record!(AppHeader {
        before_name: space0_e(EHeader::IndentStart),
        name: loc!(crate::parser::specialize(
            EHeader::AppName,
            string_literal::parse()
        )),
        packages: optional(specialize(EHeader::Packages, packages())),
        imports: optional(specialize(EHeader::Imports, imports())),
        provides: specialize(EHeader::Provides, provides_to()),
    })
    .trace("app_header")
}

#[inline(always)]
fn package_header<'a>() -> impl Parser<'a, PackageHeader<'a>, EHeader<'a>> {
    record!(PackageHeader {
        before_name: space0_e(EHeader::IndentStart),
        name: loc!(specialize(EHeader::PackageName, package_name())),
        exposes: specialize(EHeader::Exposes, exposes_modules()),
        packages: specialize(EHeader::Packages, packages()),
        imports: specialize(EHeader::Imports, imports()),
    })
    .trace("package_header")
}

#[inline(always)]
fn platform_header<'a>() -> impl Parser<'a, PlatformHeader<'a>, EHeader<'a>> {
    record!(PlatformHeader {
        before_name: space0_e(EHeader::IndentStart),
        name: loc!(specialize(EHeader::PlatformName, package_name())),
        requires: specialize(EHeader::Requires, requires()),
        exposes: specialize(EHeader::Exposes, exposes_modules()),
        packages: specialize(EHeader::Packages, packages()),
        imports: specialize(EHeader::Imports, imports()),
        provides: specialize(EHeader::Provides, provides_exposed()),
    })
    .trace("platform_header")
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
    record!(ProvidesTo {
        provides_keyword: spaces_around_keyword(
            ProvidesKeyword,
            EProvides::Provides,
            EProvides::IndentProvides,
            EProvides::IndentListStart
        ),
        entries: collection_trailing_sep_e!(
            word1(b'[', EProvides::ListStart),
            exposes_entry(EProvides::Identifier),
            word1(b',', EProvides::ListEnd),
            word1(b']', EProvides::ListEnd),
            EProvides::IndentListEnd,
            Spaced::SpaceBefore
        ),
        types: optional(backtrackable(provides_types())),
        to_keyword: spaces_around_keyword(
            ToKeyword,
            EProvides::To,
            EProvides::IndentTo,
            EProvides::IndentListStart
        ),
        to: loc!(provides_to_package()),
    })
    .trace("provides_to")
}

fn provides_exposed<'a>() -> impl Parser<
    'a,
    KeywordItem<'a, ProvidesKeyword, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,
    EProvides<'a>,
> {
    record!(KeywordItem {
        keyword: spaces_around_keyword(
            ProvidesKeyword,
            EProvides::Provides,
            EProvides::IndentProvides,
            EProvides::IndentListStart
        ),
        item: collection_trailing_sep_e!(
            word1(b'[', EProvides::ListStart),
            exposes_entry(EProvides::Identifier),
            word1(b',', EProvides::ListEnd),
            word1(b']', EProvides::ListEnd),
            EProvides::IndentListEnd,
            Spaced::SpaceBefore
        ),
    })
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
fn requires<'a>(
) -> impl Parser<'a, KeywordItem<'a, RequiresKeyword, PlatformRequires<'a>>, ERequires<'a>> {
    record!(KeywordItem {
        keyword: spaces_around_keyword(
            RequiresKeyword,
            ERequires::Requires,
            ERequires::IndentRequires,
            ERequires::IndentListStart
        ),
        item: platform_requires(),
    })
}

#[inline(always)]
fn platform_requires<'a>() -> impl Parser<'a, PlatformRequires<'a>, ERequires<'a>> {
    record!(PlatformRequires {
        rigids: skip_second!(requires_rigids(), space0_e(ERequires::ListStart)),
        signature: requires_typed_ident()
    })
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
    KeywordItem<'a, ExposesKeyword, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,
    EExposes,
> {
    record!(KeywordItem {
        keyword: spaces_around_keyword(
            ExposesKeyword,
            EExposes::Exposes,
            EExposes::IndentExposes,
            EExposes::IndentListStart
        ),
        item: collection_trailing_sep_e!(
            word1(b'[', EExposes::ListStart),
            exposes_entry(EExposes::Identifier),
            word1(b',', EExposes::ListEnd),
            word1(b']', EExposes::ListEnd),
            EExposes::IndentListEnd,
            Spaced::SpaceBefore
        )
    })
}

fn spaces_around_keyword<'a, K: Keyword, E>(
    keyword_item: K,
    expectation: fn(Position) -> E,
    indent_problem1: fn(Position) -> E,
    indent_problem2: fn(Position) -> E,
) -> impl Parser<'a, Spaces<'a, K>, E>
where
    E: 'a + SpaceProblem,
{
    map!(
        and!(
            skip_second!(
                backtrackable(space0_e(indent_problem1)),
                crate::parser::keyword_e(K::KEYWORD, expectation)
            ),
            space0_e(indent_problem2)
        ),
        |(before, after)| {
            Spaces {
                before,
                item: keyword_item,
                after,
            }
        }
    )
}

#[inline(always)]
fn exposes_modules<'a>() -> impl Parser<
    'a,
    KeywordItem<'a, ExposesKeyword, Collection<'a, Loc<Spaced<'a, ModuleName<'a>>>>>,
    EExposes,
> {
    record!(KeywordItem {
        keyword: spaces_around_keyword(
            ExposesKeyword,
            EExposes::Exposes,
            EExposes::IndentExposes,
            EExposes::IndentListStart
        ),
        item: collection_trailing_sep_e!(
            word1(b'[', EExposes::ListStart),
            exposes_module(EExposes::Identifier),
            word1(b',', EExposes::ListEnd),
            word1(b']', EExposes::ListEnd),
            EExposes::IndentListEnd,
            Spaced::SpaceBefore
        ),
    })
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

#[inline(always)]
fn packages<'a>() -> impl Parser<
    'a,
    KeywordItem<'a, PackagesKeyword, Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>>,
    EPackages<'a>,
> {
    record!(KeywordItem {
        keyword: spaces_around_keyword(
            PackagesKeyword,
            EPackages::Packages,
            EPackages::IndentPackages,
            EPackages::IndentListStart
        ),
        item: collection_trailing_sep_e!(
            word1(b'{', EPackages::ListStart),
            specialize(EPackages::PackageEntry, loc!(package_entry())),
            word1(b',', EPackages::ListEnd),
            word1(b'}', EPackages::ListEnd),
            EPackages::IndentListEnd,
            Spaced::SpaceBefore
        )
    })
}

#[inline(always)]
fn generates<'a>(
) -> impl Parser<'a, KeywordItem<'a, GeneratesKeyword, UppercaseIdent<'a>>, EGenerates> {
    record!(KeywordItem {
        keyword: spaces_around_keyword(
            GeneratesKeyword,
            EGenerates::Generates,
            EGenerates::IndentGenerates,
            EGenerates::IndentTypeStart
        ),
        item: specialize(|(), pos| EGenerates::Identifier(pos), uppercase())
    })
}

#[inline(always)]
fn generates_with<'a>() -> impl Parser<
    'a,
    KeywordItem<'a, WithKeyword, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,
    EGeneratesWith,
> {
    record!(KeywordItem {
        keyword: spaces_around_keyword(
            WithKeyword,
            EGeneratesWith::With,
            EGeneratesWith::IndentWith,
            EGeneratesWith::IndentListStart
        ),
        item: collection_trailing_sep_e!(
            word1(b'[', EGeneratesWith::ListStart),
            exposes_entry(EGeneratesWith::Identifier),
            word1(b',', EGeneratesWith::ListEnd),
            word1(b']', EGeneratesWith::ListEnd),
            EGeneratesWith::IndentListEnd,
            Spaced::SpaceBefore
        )
    })
}

#[inline(always)]
fn imports<'a>() -> impl Parser<
    'a,
    KeywordItem<'a, ImportsKeyword, Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>>,
    EImports,
> {
    record!(KeywordItem {
        keyword: spaces_around_keyword(
            ImportsKeyword,
            EImports::Imports,
            EImports::IndentImports,
            EImports::IndentListStart
        ),
        item: collection_trailing_sep_e!(
            word1(b'[', EImports::ListStart),
            loc!(imports_entry()),
            word1(b',', EImports::ListEnd),
            word1(b']', EImports::ListEnd),
            EImports::IndentListEnd,
            Spaced::SpaceBefore
        )
    })
    .trace("imports")
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
                optional(backtrackable(skip_second!(
                    shortname(),
                    word1(b'.', EImports::ShorthandDot)
                ))),
                // e.g. `Task`
                module_name_help(EImports::ModuleName)
            ),
            // e.g. `.{ Task, after}`
            optional(skip_first!(
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
    .trace("imports_entry")
}
