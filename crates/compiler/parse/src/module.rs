use crate::ast::{Collection, CommentOrNewline, Defs, Header, Module, Spaced, Spaces};
use crate::blankspace::{space0_around_ee, space0_before_e, space0_e};
use crate::header::{
    package_entry, package_name, AppHeader, ExposedName, ExposesKeyword, GeneratesKeyword,
    HostedHeader, ImportsEntry, ImportsKeyword, ImportsKeywordItem, Keyword, KeywordItem,
    ModuleHeader, ModuleName, PackageEntry, PackageHeader, PackagesKeyword, PlatformHeader,
    PlatformRequires, ProvidesKeyword, ProvidesTo, RequiresKeyword, To, ToKeyword, TypedIdent,
    WithKeyword,
};
use crate::ident::{self, lowercase_ident, unqualified_ident, uppercase, UppercaseIdent};
use crate::parser::Progress::{self, *};
use crate::parser::{
    backtrackable, byte, increment_min_indent, optional, reset_min_indent, specialize_err,
    two_bytes, EExposes, EGenerates, EGeneratesWith, EHeader, EImports, EPackages, EProvides,
    ERequires, ETypedIdent, Parser, SourceError, SpaceProblem, SyntaxError,
};
use crate::state::State;
use crate::string_literal::{self, parse_str_literal};
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

pub fn parse_module_defs<'a>(
    arena: &'a bumpalo::Bump,
    state: State<'a>,
    defs: Defs<'a>,
) -> Result<Defs<'a>, SyntaxError<'a>> {
    let min_indent = 0;
    match crate::expr::parse_top_level_defs(arena, state.clone(), defs) {
        Ok((_, defs, state)) => match end_of_file().parse(arena, state, min_indent) {
            Ok(_) => Ok(defs),
            Err((_, fail)) => Err(fail),
        },
        Err((_, fail)) => Err(SyntaxError::Expr(fail, state.pos())),
    }
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

pub fn header<'a>() -> impl Parser<'a, Module<'a>, EHeader<'a>> {
    use crate::parser::keyword;

    record!(Module {
        comments: space0_e(EHeader::IndentStart),
        header: one_of![
            map!(
                skip_first!(
                    keyword("module", EHeader::Start),
                    increment_min_indent(module_header())
                ),
                Header::Module
            ),
            // Old headers
            map!(
                skip_first!(
                    keyword("interface", EHeader::Start),
                    increment_min_indent(interface_header())
                ),
                Header::Module
            ),
            map!(
                skip_first!(
                    keyword("app", EHeader::Start),
                    increment_min_indent(app_header())
                ),
                Header::App
            ),
            map!(
                skip_first!(
                    keyword("package", EHeader::Start),
                    increment_min_indent(package_header())
                ),
                Header::Package
            ),
            map!(
                skip_first!(
                    keyword("platform", EHeader::Start),
                    increment_min_indent(platform_header())
                ),
                Header::Platform
            ),
            map!(
                skip_first!(
                    keyword("hosted", EHeader::Start),
                    increment_min_indent(hosted_header())
                ),
                Header::Hosted
            ),
        ]
    })
}

#[inline(always)]
fn module_header<'a>() -> impl Parser<'a, ModuleHeader<'a>, EHeader<'a>> {
    record!(ModuleHeader {
        before_exposes: space0_e(EHeader::IndentStart),
        exposes: specialize_err(EHeader::Exposes, exposes_list()),
        interface_imports: succeed!(None)
    })
    .trace("module_header")
}

/// Parse old interface headers so we can format them into module headers
#[inline(always)]
fn interface_header<'a>() -> impl Parser<'a, ModuleHeader<'a>, EHeader<'a>> {
    use bumpalo::collections::Vec;

    let before_exposes = map_with_arena!(
        and!(
            skip_second!(
                space0_e(EHeader::IndentStart),
                loc!(module_name_help(EHeader::ModuleName))
            ),
            specialize_err(EHeader::Exposes, exposes_kw())
        ),
        |arena: &'a bumpalo::Bump,
         (before_name, kw): (&'a [CommentOrNewline<'a>], Spaces<'a, ExposesKeyword>)| {
            let mut combined: Vec<CommentOrNewline> =
                Vec::with_capacity_in(before_name.len() + kw.before.len() + kw.after.len(), arena);
            combined.extend(before_name);
            combined.extend(kw.before);
            combined.extend(kw.after);
            arena.alloc(combined)
        }
    );

    record!(ModuleHeader {
        before_exposes: before_exposes,
        exposes: specialize_err(EHeader::Exposes, exposes_list()).trace("exposes_list"),
        interface_imports: map!(
            specialize_err(EHeader::Imports, imports()),
            imports_none_if_empty
        )
        .trace("imports"),
    })
    .trace("interface_header")
}

fn imports_none_if_empty(value: ImportsKeywordItem<'_>) -> Option<ImportsKeywordItem<'_>> {
    if value.item.is_empty() {
        None
    } else {
        Some(value)
    }
}

#[inline(always)]
fn hosted_header<'a>() -> impl Parser<'a, HostedHeader<'a>, EHeader<'a>> {
    record!(HostedHeader {
        before_name: space0_e(EHeader::IndentStart),
        name: loc!(module_name_help(EHeader::ModuleName)),
        exposes: specialize_err(EHeader::Exposes, exposes_values_kw()),
        imports: specialize_err(EHeader::Imports, imports()),
        generates: specialize_err(EHeader::Generates, generates()),
        generates_with: specialize_err(EHeader::GeneratesWith, generates_with()),
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
        name: loc!(crate::parser::specialize_err(
            EHeader::AppName,
            string_literal::parse_str_literal()
        )),
        packages: optional(specialize_err(EHeader::Packages, packages())),
        imports: optional(specialize_err(EHeader::Imports, imports())),
        provides: specialize_err(EHeader::Provides, provides_to()),
    })
    .trace("app_header")
}

#[inline(always)]
fn package_header<'a>() -> impl Parser<'a, PackageHeader<'a>, EHeader<'a>> {
    record!(PackageHeader {
        before_name: space0_e(EHeader::IndentStart),
        name: loc!(specialize_err(EHeader::PackageName, package_name())),
        exposes: specialize_err(EHeader::Exposes, exposes_modules()),
        packages: specialize_err(EHeader::Packages, packages()),
    })
    .trace("package_header")
}

#[inline(always)]
fn platform_header<'a>() -> impl Parser<'a, PlatformHeader<'a>, EHeader<'a>> {
    record!(PlatformHeader {
        before_name: space0_e(EHeader::IndentStart),
        name: loc!(specialize_err(EHeader::PlatformName, package_name())),
        requires: specialize_err(EHeader::Requires, requires()),
        exposes: specialize_err(EHeader::Exposes, exposes_modules()),
        packages: specialize_err(EHeader::Packages, packages()),
        imports: specialize_err(EHeader::Imports, imports()),
        provides: specialize_err(EHeader::Provides, provides_exposed()),
    })
    .trace("platform_header")
}

fn provides_to_package<'a>() -> impl Parser<'a, To<'a>, EProvides<'a>> {
    one_of![
        specialize_err(
            |_, pos| EProvides::Identifier(pos),
            map!(lowercase_ident(), To::ExistingPackage)
        ),
        specialize_err(EProvides::Package, map!(package_name(), To::NewPackage))
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
            byte(b'[', EProvides::ListStart),
            exposes_entry(EProvides::Identifier),
            byte(b',', EProvides::ListEnd),
            byte(b']', EProvides::ListEnd),
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
            byte(b'[', EProvides::ListStart),
            exposes_entry(EProvides::Identifier),
            byte(b',', EProvides::ListEnd),
            byte(b']', EProvides::ListEnd),
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
        zero_or_more!(byte(
            b' ',
            // HACK: If this errors, EProvides::Provides is not an accurate reflection
            // of what went wrong. However, this is both skipped and zero_or_more,
            // so this error should never be visible to anyone in practice!
            EProvides::Provides
        )),
        collection_trailing_sep_e!(
            byte(b'{', EProvides::ListStart),
            provides_type_entry(EProvides::Identifier),
            byte(b',', EProvides::ListEnd),
            byte(b'}', EProvides::ListEnd),
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
        specialize_err(|_, pos| to_expectation(pos), ident::uppercase()),
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
        specialize_err(|_, pos| to_expectation(pos), unqualified_ident()),
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
        byte(b'{', ERequires::ListStart),
        specialize_err(
            |_, pos| ERequires::Rigid(pos),
            loc!(map!(ident::uppercase(), Spaced::Item))
        ),
        byte(b',', ERequires::ListEnd),
        byte(b'}', ERequires::ListEnd),
        Spaced::SpaceBefore
    )
}

#[inline(always)]
fn requires_typed_ident<'a>() -> impl Parser<'a, Loc<Spaced<'a, TypedIdent<'a>>>, ERequires<'a>> {
    skip_first!(
        byte(b'{', ERequires::ListStart),
        skip_second!(
            reset_min_indent(space0_around_ee(
                specialize_err(ERequires::TypedIdent, loc!(typed_ident()),),
                ERequires::ListStart,
                ERequires::ListEnd
            )),
            byte(b'}', ERequires::ListStart)
        )
    )
}

#[inline(always)]
fn exposes_values_kw<'a>() -> impl Parser<
    'a,
    KeywordItem<'a, ExposesKeyword, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,
    EExposes,
> {
    record!(KeywordItem {
        keyword: exposes_kw(),
        item: exposes_list()
    })
}

#[inline(always)]
fn exposes_kw<'a>() -> impl Parser<'a, Spaces<'a, ExposesKeyword>, EExposes> {
    spaces_around_keyword(
        ExposesKeyword,
        EExposes::Exposes,
        EExposes::IndentExposes,
        EExposes::IndentListStart,
    )
}

#[inline(always)]
fn exposes_list<'a>() -> impl Parser<'a, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>, EExposes>
{
    collection_trailing_sep_e!(
        byte(b'[', EExposes::ListStart),
        exposes_entry(EExposes::Identifier),
        byte(b',', EExposes::ListEnd),
        byte(b']', EExposes::ListEnd),
        Spaced::SpaceBefore
    )
}

pub fn spaces_around_keyword<'a, K: Keyword, E>(
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
                crate::parser::keyword(K::KEYWORD, expectation)
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
            byte(b'[', EExposes::ListStart),
            exposes_module(EExposes::Identifier),
            byte(b',', EExposes::ListEnd),
            byte(b']', EExposes::ListEnd),
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
        specialize_err(|_, pos| to_expectation(pos), module_name()),
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
            byte(b'{', EPackages::ListStart),
            specialize_err(EPackages::PackageEntry, loc!(package_entry())),
            byte(b',', EPackages::ListEnd),
            byte(b'}', EPackages::ListEnd),
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
        item: specialize_err(|(), pos| EGenerates::Identifier(pos), uppercase())
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
            byte(b'[', EGeneratesWith::ListStart),
            exposes_entry(EGeneratesWith::Identifier),
            byte(b',', EGeneratesWith::ListEnd),
            byte(b']', EGeneratesWith::ListEnd),
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
            byte(b'[', EImports::ListStart),
            loc!(imports_entry()),
            byte(b',', EImports::ListEnd),
            byte(b']', EImports::ListEnd),
            Spaced::SpaceBefore
        )
    })
    .trace("imports")
}

#[inline(always)]
pub fn typed_ident<'a>() -> impl Parser<'a, Spaced<'a, TypedIdent<'a>>, ETypedIdent<'a>> {
    // e.g.
    //
    // printLine : Str -> Effect {}
    map!(
        and!(
            and!(
                loc!(specialize_err(
                    |_, pos| ETypedIdent::Identifier(pos),
                    lowercase_ident()
                )),
                space0_e(ETypedIdent::IndentHasType)
            ),
            skip_first!(
                byte(b':', ETypedIdent::HasType),
                space0_before_e(
                    specialize_err(
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
    specialize_err(|_, pos| EImports::Shorthand(pos), lowercase_ident())
}

pub fn module_name_help<'a, F, E>(to_expectation: F) -> impl Parser<'a, ModuleName<'a>, E>
where
    F: Fn(Position) -> E,
    E: 'a,
    F: 'a,
{
    specialize_err(move |_, pos| to_expectation(pos), module_name())
}

#[inline(always)]
fn imports_entry<'a>() -> impl Parser<'a, Spaced<'a, ImportsEntry<'a>>, EImports> {
    type Temp<'a> = (
        (Option<&'a str>, ModuleName<'a>),
        Option<Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,
    );

    one_of!(
        map!(
            and!(
                and!(
                    // e.g. `pf.`
                    optional(backtrackable(skip_second!(
                        shortname(),
                        byte(b'.', EImports::ShorthandDot)
                    ))),
                    // e.g. `Task`
                    module_name_help(EImports::ModuleName)
                ),
                // e.g. `.{ Task, after}`
                optional(skip_first!(
                    byte(b'.', EImports::ExposingDot),
                    collection_trailing_sep_e!(
                        byte(b'{', EImports::SetStart),
                        exposes_entry(EImports::Identifier),
                        byte(b',', EImports::SetEnd),
                        byte(b'}', EImports::SetEnd),
                        Spaced::SpaceBefore
                    )
                ))
            ),
            |((opt_shortname, module_name), opt_values): Temp<'a>| {
                let exposed_values = opt_values.unwrap_or_else(Collection::empty);

                let entry = match opt_shortname {
                    Some(shortname) => {
                        ImportsEntry::Package(shortname, module_name, exposed_values)
                    }

                    None => ImportsEntry::Module(module_name, exposed_values),
                };

                Spaced::Item(entry)
            }
        )
        .trace("normal_import"),
        map!(
            and!(
                and!(
                    // e.g. "filename"
                    // TODO: str literal allows for multiline strings. We probably don't want that for file names.
                    specialize_err(|_, pos| EImports::StrLiteral(pos), parse_str_literal()),
                    // e.g. as
                    and!(
                        and!(
                            space0_e(EImports::AsKeyword),
                            two_bytes(b'a', b's', EImports::AsKeyword)
                        ),
                        space0_e(EImports::AsKeyword)
                    )
                ),
                // e.g. file : Str
                specialize_err(|_, pos| EImports::TypedIdent(pos), typed_ident())
            ),
            |((file_name, _), typed_ident)| {
                // TODO: look at blacking block strings during parsing.
                Spaced::Item(ImportsEntry::IngestedFile(file_name, typed_ident))
            }
        )
        .trace("ingest_file_import")
    )
    .trace("imports_entry")
}
