use std::fmt::Debug;

use crate::ast::{
    Collection, CommentOrNewline, Defs, Header, Malformed, Pattern, Spaced, Spaces, SpacesBefore,
    StrLiteral, TypeAnnotation,
};
use crate::blankspace::{space0_before_e, space0_e};
use crate::expr::merge_spaces;
use crate::ident::{self, lowercase_ident, unqualified_ident, UppercaseIdent};
use crate::parser::Progress::{self, *};
use crate::parser::{
    and, backtrackable, byte, collection_trailing_sep_e, increment_min_indent, loc, map,
    map_with_arena, optional, reset_min_indent, skip_first, skip_second, specialize_err, succeed,
    then, two_bytes, zero_or_more, EExposes, EHeader, EImports, EPackageEntry, EPackageName,
    EPackages, EParams, EProvides, ERequires, ETypedIdent, Parser, SourceError, SpaceProblem,
    SyntaxError,
};
use crate::pattern::record_pattern_fields;
use crate::state::State;
use crate::string_literal::{self, parse_str_literal};
use crate::type_annotation;
use roc_module::ident::IdentSuffix;
use roc_module::symbol::ModuleId;
use roc_region::all::{Loc, Position, Region};

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
) -> Result<(SpacesBefore<'a, Header<'a>>, State<'a>), SourceError<'a, EHeader<'a>>> {
    let min_indent = 0;
    match header().parse(arena, state.clone(), min_indent) {
        Ok((_, module, state)) => Ok((module, state)),
        Err((_, fail)) => Err(SourceError::new(fail, &state)),
    }
}

pub fn header<'a>() -> impl Parser<'a, SpacesBefore<'a, Header<'a>>, EHeader<'a>> {
    use crate::parser::keyword;

    record!(SpacesBefore {
        before: space0_e(EHeader::IndentStart),
        item: one_of![
            map(
                skip_first(
                    keyword("module", EHeader::Start),
                    increment_min_indent(module_header())
                ),
                Header::Module
            ),
            map(
                skip_first(
                    keyword("interface", EHeader::Start),
                    increment_min_indent(interface_header())
                ),
                Header::Module
            ),
            map(
                skip_first(
                    keyword("app", EHeader::Start),
                    increment_min_indent(one_of![app_header(), old_app_header()])
                ),
                Header::App
            ),
            map(
                skip_first(
                    keyword("package", EHeader::Start),
                    increment_min_indent(one_of![package_header(), old_package_header()])
                ),
                Header::Package
            ),
            map(
                skip_first(
                    keyword("platform", EHeader::Start),
                    increment_min_indent(platform_header())
                ),
                Header::Platform
            ),
            map(
                skip_first(
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
        after_keyword: space0_e(EHeader::IndentStart),
        params: optional(specialize_err(EHeader::Params, module_params())),
        exposes: specialize_err(EHeader::Exposes, exposes_list()),
        interface_imports: succeed(None)
    })
    .trace("module_header")
}

fn module_params<'a>() -> impl Parser<'a, ModuleParams<'a>, EParams<'a>> {
    record!(ModuleParams {
        pattern: specialize_err(EParams::Pattern, loc(record_pattern_fields())),
        before_arrow: skip_second(
            space0_e(EParams::BeforeArrow),
            loc(two_bytes(b'-', b'>', EParams::Arrow))
        ),
        after_arrow: space0_e(EParams::AfterArrow),
    })
}

// TODO does this need to be a macro?
macro_rules! merge_n_spaces {
    ($arena:expr, $($slice:expr),*) => {
        {
            let mut merged = bumpalo::collections::Vec::with_capacity_in(0 $(+ $slice.len())*, $arena);
            $(merged.extend_from_slice($slice);)*
            merged.into_bump_slice()
        }
    };
}

/// Parse old interface headers so we can format them into module headers
#[inline(always)]
fn interface_header<'a>() -> impl Parser<'a, ModuleHeader<'a>, EHeader<'a>> {
    let after_keyword = map_with_arena(
        and(
            skip_second(
                space0_e(EHeader::IndentStart),
                loc(module_name_help(EHeader::ModuleName)),
            ),
            specialize_err(EHeader::Exposes, exposes_kw()),
        ),
        |arena: &'a bumpalo::Bump,
         (before_name, kw): (&'a [CommentOrNewline<'a>], Spaces<'a, ExposesKeyword>)| {
            merge_n_spaces!(arena, before_name, kw.before, kw.after)
        },
    );

    record!(ModuleHeader {
        after_keyword: after_keyword,
        params: succeed(None),
        exposes: specialize_err(EHeader::Exposes, exposes_list()).trace("exposes_list"),
        interface_imports: map(
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
        name: loc(module_name_help(EHeader::ModuleName)),
        exposes: specialize_err(EHeader::Exposes, exposes_values_kw()),
        imports: specialize_err(EHeader::Imports, imports()),
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
        before_provides: space0_e(EHeader::IndentStart),
        provides: specialize_err(EHeader::Exposes, exposes_list()),
        before_packages: space0_e(EHeader::IndentStart),
        packages: specialize_err(EHeader::Packages, loc(packages_collection())),
        old_imports: succeed(None),
        old_provides_to_new_package: succeed(None),
    })
    .trace("app_header")
}

struct OldAppHeader<'a> {
    pub before_name: &'a [CommentOrNewline<'a>],
    pub packages: Option<Loc<OldAppPackages<'a>>>,
    pub imports: Option<KeywordItem<'a, ImportsKeyword, ImportsCollection<'a>>>,
    pub provides: ProvidesTo<'a>,
}

type OldAppPackages<'a> =
    KeywordItem<'a, PackagesKeyword, Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>>;

#[inline(always)]
fn old_app_header<'a>() -> impl Parser<'a, AppHeader<'a>, EHeader<'a>> {
    let old = record!(OldAppHeader {
        before_name: skip_second(
            space0_e(EHeader::IndentStart),
            loc(crate::parser::specialize_err(
                EHeader::AppName,
                string_literal::parse_str_literal()
            ))
        ),
        packages: optional(specialize_err(EHeader::Packages, loc(packages()))),
        imports: optional(specialize_err(EHeader::Imports, imports())),
        provides: specialize_err(EHeader::Provides, provides_to()),
    });

    map_with_arena(old, |arena: &'a bumpalo::Bump, old: OldAppHeader<'a>| {
        let mut before_packages: &'a [CommentOrNewline] = &[];

        let packages = match old.packages {
            Some(packages) => {
                before_packages = merge_spaces(
                    arena,
                    packages.value.keyword.before,
                    packages.value.keyword.after,
                );

                if let To::ExistingPackage(platform_shorthand) = old.provides.to.value {
                    packages.map(|coll| {
                        coll.item.map_items(arena, |loc_spaced_pkg| {
                            if loc_spaced_pkg.value.item().shorthand == platform_shorthand {
                                loc_spaced_pkg.map(|spaced_pkg| {
                                    spaced_pkg.map(arena, |pkg| {
                                        let mut new_pkg = *pkg;
                                        new_pkg.platform_marker = Some(merge_spaces(
                                            arena,
                                            old.provides.to_keyword.before,
                                            old.provides.to_keyword.after,
                                        ));
                                        new_pkg
                                    })
                                })
                            } else {
                                *loc_spaced_pkg
                            }
                        })
                    })
                } else {
                    packages.map(|kw| kw.item)
                }
            }
            None => Loc {
                region: Region::zero(),
                value: Collection::empty(),
            },
        };

        let provides = match old.provides.types {
            Some(types) => {
                let mut combined_items = bumpalo::collections::Vec::with_capacity_in(
                    old.provides.entries.items.len() + types.items.len(),
                    arena,
                );

                combined_items.extend_from_slice(old.provides.entries.items);

                for loc_spaced_type_ident in types.items {
                    combined_items.push(loc_spaced_type_ident.map(|spaced_type_ident| {
                        spaced_type_ident.map(arena, |type_ident| {
                            ExposedName::new(From::from(*type_ident))
                        })
                    }));
                }

                let value_comments = old.provides.entries.final_comments();
                let type_comments = types.final_comments();

                let mut combined_comments = bumpalo::collections::Vec::with_capacity_in(
                    value_comments.len() + type_comments.len(),
                    arena,
                );
                combined_comments.extend_from_slice(value_comments);
                combined_comments.extend_from_slice(type_comments);

                Collection::with_items_and_comments(
                    arena,
                    combined_items.into_bump_slice(),
                    combined_comments.into_bump_slice(),
                )
            }
            None => old.provides.entries,
        };

        AppHeader {
            before_provides: merge_spaces(
                arena,
                old.before_name,
                old.provides.provides_keyword.before,
            ),
            provides,
            before_packages: merge_spaces(
                arena,
                before_packages,
                old.provides.provides_keyword.after,
            ),
            packages,
            old_imports: old.imports.and_then(imports_none_if_empty),
            old_provides_to_new_package: match old.provides.to.value {
                To::NewPackage(new_pkg) => Some(new_pkg),
                To::ExistingPackage(_) => None,
            },
        }
    })
}

#[inline(always)]
fn package_header<'a>() -> impl Parser<'a, PackageHeader<'a>, EHeader<'a>> {
    record!(PackageHeader {
        before_exposes: space0_e(EHeader::IndentStart),
        exposes: specialize_err(EHeader::Exposes, exposes_module_collection()),
        before_packages: space0_e(EHeader::IndentStart),
        packages: specialize_err(EHeader::Packages, loc(packages_collection())),
    })
    .trace("package_header")
}

#[derive(Debug, Clone, PartialEq)]
struct OldPackageHeader<'a> {
    before_name: &'a [CommentOrNewline<'a>],
    exposes: KeywordItem<'a, ExposesKeyword, Collection<'a, Loc<Spaced<'a, ModuleName<'a>>>>>,
    packages:
        Loc<KeywordItem<'a, PackagesKeyword, Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>>>,
}

#[inline(always)]
fn old_package_header<'a>() -> impl Parser<'a, PackageHeader<'a>, EHeader<'a>> {
    map_with_arena(
        record!(OldPackageHeader {
            before_name: skip_second(
                space0_e(EHeader::IndentStart),
                specialize_err(EHeader::PackageName, package_name())
            ),
            exposes: specialize_err(EHeader::Exposes, exposes_modules()),
            packages: specialize_err(EHeader::Packages, loc(packages())),
        }),
        |arena: &'a bumpalo::Bump, old: OldPackageHeader<'a>| {
            let before_exposes = merge_n_spaces!(
                arena,
                old.before_name,
                old.exposes.keyword.before,
                old.exposes.keyword.after
            );
            let before_packages = merge_spaces(
                arena,
                old.packages.value.keyword.before,
                old.packages.value.keyword.after,
            );

            PackageHeader {
                before_exposes,
                exposes: old.exposes.item,
                before_packages,
                packages: old.packages.map(|kw| kw.item),
            }
        },
    )
    .trace("old_package_header")
}

#[inline(always)]
fn platform_header<'a>() -> impl Parser<'a, PlatformHeader<'a>, EHeader<'a>> {
    record!(PlatformHeader {
        before_name: space0_e(EHeader::IndentStart),
        name: loc(specialize_err(EHeader::PlatformName, package_name())),
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
            map(lowercase_ident(), To::ExistingPackage)
        ),
        specialize_err(EProvides::Package, map(package_name(), To::NewPackage))
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
        entries: collection_trailing_sep_e(
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
        to: loc(provides_to_package()),
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
        item: collection_trailing_sep_e(
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
    skip_first(
        // We only support spaces here, not newlines, because this is not intended
        // to be the design forever. Someday it will hopefully work like Elm,
        // where platform authors can provide functions like Browser.sandbox which
        // present an API based on ordinary-looking type variables.
        zero_or_more(byte(
            b' ',
            // HACK: If this errors, EProvides::Provides is not an accurate reflection
            // of what went wrong. However, this is both skipped and zero_or_more,
            // so this error should never be visible to anyone in practice!
            EProvides::Provides,
        )),
        collection_trailing_sep_e(
            byte(b'{', EProvides::ListStart),
            provides_type_entry(EProvides::Identifier),
            byte(b',', EProvides::ListEnd),
            byte(b'}', EProvides::ListEnd),
            Spaced::SpaceBefore,
        ),
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
    loc(map(
        specialize_err(move |_, pos| to_expectation(pos), ident::uppercase()),
        Spaced::Item,
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
    loc(map(
        specialize_err(move |_, pos| to_expectation(pos), unqualified_ident()),
        |n| Spaced::Item(ExposedName::new(n)),
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
        rigids: skip_second(requires_rigids(), space0_e(ERequires::ListStart)),
        signatures: requires_typed_ident()
    })
}

#[inline(always)]
fn requires_rigids<'a>(
) -> impl Parser<'a, Collection<'a, Loc<Spaced<'a, UppercaseIdent<'a>>>>, ERequires<'a>> {
    collection_trailing_sep_e(
        byte(b'{', ERequires::ListStart),
        specialize_err(
            |_, pos| ERequires::Rigid(pos),
            loc(map(ident::uppercase(), Spaced::Item)),
        ),
        byte(b',', ERequires::ListEnd),
        byte(b'}', ERequires::ListEnd),
        Spaced::SpaceBefore,
    )
}

#[inline(always)]
fn requires_typed_ident<'a>(
) -> impl Parser<'a, Collection<'a, Loc<Spaced<'a, TypedIdent<'a>>>>, ERequires<'a>> {
    reset_min_indent(collection_trailing_sep_e(
        byte(b'{', ERequires::ListStart),
        specialize_err(ERequires::TypedIdent, loc(typed_ident())),
        byte(b',', ERequires::ListEnd),
        byte(b'}', ERequires::ListEnd),
        Spaced::SpaceBefore,
    ))
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
    collection_trailing_sep_e(
        byte(b'[', EExposes::ListStart),
        exposes_entry(EExposes::Identifier),
        byte(b',', EExposes::ListEnd),
        byte(b']', EExposes::ListEnd),
        Spaced::SpaceBefore,
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
    map(
        and(
            skip_second(
                // parse any leading space before the keyword
                backtrackable(space0_e(indent_problem1)),
                // parse the keyword
                crate::parser::keyword(K::KEYWORD, expectation),
            ),
            // parse the trailing space
            space0_e(indent_problem2),
        ),
        move |(before, after)| Spaces {
            before,
            item: keyword_item,
            after,
        },
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
        item: exposes_module_collection(),
    })
}

fn exposes_module_collection<'a>(
) -> impl Parser<'a, Collection<'a, Loc<Spaced<'a, ModuleName<'a>>>>, EExposes> {
    collection_trailing_sep_e(
        byte(b'[', EExposes::ListStart),
        exposes_module(EExposes::Identifier),
        byte(b',', EExposes::ListEnd),
        byte(b']', EExposes::ListEnd),
        Spaced::SpaceBefore,
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
    loc(map(
        specialize_err(move |_, pos| to_expectation(pos), module_name()),
        Spaced::Item,
    ))
}

#[inline(always)]
fn packages<'a>() -> impl Parser<
    'a,
    KeywordItem<'a, PackagesKeyword, Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>>,
    EPackages<'a>,
> {
    record!(KeywordItem {
        keyword: packages_kw(),
        item: packages_collection()
    })
}

#[inline(always)]
fn packages_kw<'a>() -> impl Parser<'a, Spaces<'a, PackagesKeyword>, EPackages<'a>> {
    spaces_around_keyword(
        PackagesKeyword,
        EPackages::Packages,
        EPackages::IndentPackages,
        EPackages::IndentListStart,
    )
}

#[inline(always)]
fn packages_collection<'a>(
) -> impl Parser<'a, Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>, EPackages<'a>> {
    collection_trailing_sep_e(
        byte(b'{', EPackages::ListStart),
        specialize_err(EPackages::PackageEntry, loc(package_entry())),
        byte(b',', EPackages::ListEnd),
        byte(b'}', EPackages::ListEnd),
        Spaced::SpaceBefore,
    )
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
        item: collection_trailing_sep_e(
            byte(b'[', EImports::ListStart),
            loc(imports_entry()),
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
    map(
        and(
            and(
                loc(specialize_err(
                    |_, pos| ETypedIdent::Identifier(pos),
                    lowercase_ident(),
                )),
                space0_e(ETypedIdent::IndentHasType),
            ),
            skip_first(
                byte(b':', ETypedIdent::HasType),
                space0_before_e(
                    specialize_err(
                        ETypedIdent::Type,
                        reset_min_indent(type_annotation::located(true)),
                    ),
                    ETypedIdent::IndentType,
                ),
            ),
        ),
        |((ident, spaces_before_colon), ann)| {
            Spaced::Item(TypedIdent {
                ident,
                spaces_before_colon,
                ann,
            })
        },
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

    let spaced_import = |((opt_shortname, module_name), opt_values): Temp<'a>| {
        let exposed_values = opt_values.unwrap_or_else(Collection::empty);

        let entry = match opt_shortname {
            Some(shortname) => ImportsEntry::Package(shortname, module_name, exposed_values),

            None => ImportsEntry::Module(module_name, exposed_values),
        };

        Spaced::Item(entry)
    };

    one_of!(
        map(
            and(
                and(
                    // e.g. `pf.`
                    optional(backtrackable(skip_second(
                        shortname(),
                        byte(b'.', EImports::ShorthandDot)
                    ))),
                    // e.g. `Task`
                    module_name_help(EImports::ModuleName)
                ),
                // e.g. `.{ Task, after}`
                optional(skip_first(
                    byte(b'.', EImports::ExposingDot),
                    collection_trailing_sep_e(
                        byte(b'{', EImports::SetStart),
                        exposes_entry(EImports::Identifier),
                        byte(b',', EImports::SetEnd),
                        byte(b'}', EImports::SetEnd),
                        Spaced::SpaceBefore
                    )
                ))
            ),
            spaced_import
        )
        .trace("normal_import"),
        map(
            and(
                and(
                    // e.g. "filename"
                    // TODO: str literal allows for multiline strings. We probably don't want that for file names.
                    specialize_err(|_, pos| EImports::StrLiteral(pos), parse_str_literal()),
                    // e.g. as
                    and(
                        and(
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

impl<'a> HeaderType<'a> {
    pub fn exposed_or_provided_values(&'a self) -> &'a [Loc<ExposedName<'a>>] {
        match self {
            HeaderType::App {
                provides: exposes, ..
            }
            | HeaderType::Hosted { exposes, .. }
            | HeaderType::Builtin { exposes, .. }
            | HeaderType::Module { exposes, .. } => exposes,
            HeaderType::Platform { .. } | HeaderType::Package { .. } => &[],
        }
    }
    pub fn to_string(&self) -> &'static str {
        match self {
            HeaderType::App { .. } => "app",
            HeaderType::Hosted { .. } => "hosted",
            HeaderType::Builtin { .. } => "builtin",
            HeaderType::Package { .. } => "package",
            HeaderType::Platform { .. } => "platform",
            HeaderType::Module { .. } => "module",
        }
    }
}

#[derive(Debug)]
pub enum HeaderType<'a> {
    App {
        provides: &'a [Loc<ExposedName<'a>>],
        to_platform: To<'a>,
    },
    Hosted {
        name: ModuleName<'a>,
        exposes: &'a [Loc<ExposedName<'a>>],
    },
    /// Only created during canonicalization, never actually parsed from source
    Builtin {
        name: ModuleName<'a>,
        exposes: &'a [Loc<ExposedName<'a>>],
        opt_params: Option<ModuleParams<'a>>,
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
        provides: &'a [Loc<ExposedName<'a>>],
        requires: &'a [Loc<TypedIdent<'a>>],
        requires_types: &'a [Loc<UppercaseIdent<'a>>],
        exposes: &'a [Loc<ModuleName<'a>>],
        exposes_ids: &'a [ModuleId],

        /// usually `pf`
        config_shorthand: &'a str,
    },
    Module {
        name: ModuleName<'a>,
        exposes: &'a [Loc<ExposedName<'a>>],
        opt_params: Option<ModuleParams<'a>>,
    },
}

impl<'a> HeaderType<'a> {
    pub fn get_name(self) -> Option<&'a str> {
        match self {
            Self::Module { name, .. } | Self::Builtin { name, .. } | Self::Hosted { name, .. } => {
                Some(name.into())
            }
            Self::Platform {
                config_shorthand: name,
                ..
            }
            | Self::Package {
                config_shorthand: name,
                ..
            } => Some(name),
            Self::App { .. } => {
                //TODO:Eli This can be removed once module params is implemented and app names are no longer strings
                None
            }
        }
    }

    pub fn get_params(&self) -> &Option<ModuleParams<'a>> {
        match self {
            Self::Module {
                opt_params,
                name: _,
                exposes: _,
            }
            | Self::Builtin {
                opt_params,
                name: _,
                exposes: _,
            } => opt_params,
            Self::App {
                provides: _,
                to_platform: _,
            }
            | Self::Package {
                config_shorthand: _,
                exposes: _,
                exposes_ids: _,
            }
            | Self::Hosted {
                name: _,
                exposes: _,
            }
            | Self::Platform {
                opt_app_module_id: _,
                provides: _,
                requires: _,
                requires_types: _,
                exposes: _,
                exposes_ids: _,
                config_shorthand: _,
            } => &None,
        }
    }

    pub fn to_maybe_builtin(self, module_id: ModuleId) -> Self {
        match self {
            HeaderType::Module {
                name,
                exposes,
                opt_params,
            } if module_id.is_builtin() => HeaderType::Builtin {
                name,
                exposes,
                opt_params,
            },
            _ => self,
        }
    }
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

impl<'a> From<ModuleName<'a>> for roc_module::ident::ModuleName {
    fn from(name: ModuleName<'a>) -> Self {
        name.0.into()
    }
}

impl<'a> ModuleName<'a> {
    const MODULE_SEPARATOR: char = '.';

    pub const fn new(name: &'a str) -> Self {
        ModuleName(name)
    }

    pub const fn as_str(&self) -> &'a str {
        self.0
    }

    pub fn parts(&'a self) -> impl DoubleEndedIterator<Item = &'a str> {
        self.0.split(Self::MODULE_SEPARATOR)
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

    pub fn is_effectful_fn(&self) -> bool {
        IdentSuffix::from_name(self.0).is_bang()
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
    PackageKeyword => "package",
    PackagesKeyword => "packages",
    RequiresKeyword => "requires",
    ProvidesKeyword => "provides",
    ToKeyword => "to",
    PlatformKeyword => "platform",
    // Deprecated
    ImportsKeyword => "imports",
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct KeywordItem<'a, K, V> {
    pub keyword: Spaces<'a, K>,
    pub item: V,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModuleHeader<'a> {
    pub after_keyword: &'a [CommentOrNewline<'a>],
    pub params: Option<ModuleParams<'a>>,
    pub exposes: Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,

    // Keeping this so we can format old interface header into module headers
    pub interface_imports: Option<KeywordItem<'a, ImportsKeyword, ImportsCollection<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModuleParams<'a> {
    pub pattern: Loc<Collection<'a, Loc<Pattern<'a>>>>,
    pub before_arrow: &'a [CommentOrNewline<'a>],
    pub after_arrow: &'a [CommentOrNewline<'a>],
}

pub type ImportsKeywordItem<'a> = KeywordItem<'a, ImportsKeyword, ImportsCollection<'a>>;
pub type ImportsCollection<'a> = Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>;

#[derive(Clone, Debug, PartialEq)]
pub struct HostedHeader<'a> {
    pub before_name: &'a [CommentOrNewline<'a>],
    pub name: Loc<ModuleName<'a>>,
    pub exposes: KeywordItem<'a, ExposesKeyword, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>>,

    pub imports: KeywordItem<'a, ImportsKeyword, Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum To<'a> {
    ExistingPackage(&'a str),
    NewPackage(PackageName<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AppHeader<'a> {
    pub before_provides: &'a [CommentOrNewline<'a>],
    pub provides: Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    pub before_packages: &'a [CommentOrNewline<'a>],
    pub packages: Loc<Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>>,
    // Old header pieces
    pub old_imports: Option<KeywordItem<'a, ImportsKeyword, ImportsCollection<'a>>>,
    pub old_provides_to_new_package: Option<PackageName<'a>>,
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
    pub before_exposes: &'a [CommentOrNewline<'a>],
    pub exposes: Collection<'a, Loc<Spaced<'a, ModuleName<'a>>>>,
    pub before_packages: &'a [CommentOrNewline<'a>],
    pub packages: Loc<Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlatformRequires<'a> {
    pub rigids: Collection<'a, Loc<Spaced<'a, UppercaseIdent<'a>>>>,
    pub signatures: Collection<'a, Loc<Spaced<'a, TypedIdent<'a>>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlatformHeader<'a> {
    pub before_name: &'a [CommentOrNewline<'a>],
    pub name: Loc<PackageName<'a>>,

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
    /// e.g. `Hello` or `Hello exposing [hello]` see roc-lang.org/examples/MultipleRocFiles/README.html
    Module(
        ModuleName<'a>,
        Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    ),

    /// e.g. `pf.Stdout` or `pf.Stdout exposing [line]`
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
/// printLine : Str -> Result {} *
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
    pub platform_marker: Option<&'a [CommentOrNewline<'a>]>,
    pub package_name: Loc<PackageName<'a>>,
}

pub fn package_entry<'a>() -> impl Parser<'a, Spaced<'a, PackageEntry<'a>>, EPackageEntry<'a>> {
    map_with_arena(
        // You may optionally have a package shorthand,
        // e.g. "uc" in `uc: roc/unicode 1.0.0`
        //
        // (Indirect dependencies don't have a shorthand.)
        and(
            optional(and(
                skip_second(
                    and(
                        specialize_err(|_, pos| EPackageEntry::Shorthand(pos), lowercase_ident()),
                        space0_e(EPackageEntry::IndentPackage),
                    ),
                    byte(b':', EPackageEntry::Colon),
                ),
                space0_e(EPackageEntry::IndentPackage),
            )),
            and(
                optional(skip_first(
                    crate::parser::keyword(crate::keyword::PLATFORM, EPackageEntry::Platform),
                    space0_e(EPackageEntry::IndentPackage),
                )),
                loc(specialize_err(EPackageEntry::BadPackage, package_name())),
            ),
        ),
        move |arena, (opt_shorthand, (platform_marker, package_or_path))| {
            let entry = match opt_shorthand {
                Some(((shorthand, spaces_before_colon), spaces_after_colon)) => PackageEntry {
                    shorthand,
                    spaces_after_shorthand: merge_spaces(
                        arena,
                        spaces_before_colon,
                        spaces_after_colon,
                    ),
                    platform_marker,
                    package_name: package_or_path,
                },
                None => PackageEntry {
                    shorthand: "",
                    spaces_after_shorthand: &[],
                    platform_marker,
                    package_name: package_or_path,
                },
            };

            Spaced::Item(entry)
        },
    )
}

pub fn package_name<'a>() -> impl Parser<'a, PackageName<'a>, EPackageName<'a>> {
    then(
        loc(specialize_err(
            EPackageName::BadPath,
            string_literal::parse_str_literal(),
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

impl<'a> Malformed for ModuleHeader<'a> {
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
        false
    }
}

impl<'a> Malformed for PackageHeader<'a> {
    fn is_malformed(&self) -> bool {
        false
    }
}

impl<'a> Malformed for PlatformRequires<'a> {
    fn is_malformed(&self) -> bool {
        self.signatures.items.iter().any(|x| x.is_malformed())
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
