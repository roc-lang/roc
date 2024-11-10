use std::fmt::Debug;

use crate::ast::{
    Collection, CommentOrNewline, Defs, Header, Malformed, Pattern, Spaced, Spaces, SpacesBefore,
    StrLiteral, TypeAnnotation,
};
use crate::blankspace::{eat_nc_check, space0_e, SpacedBuilder};
use crate::expr::merge_spaces;
use crate::ident::{self, parse_anycase_ident, parse_lowercase_ident, UppercaseIdent};
use crate::parser::Progress::{self, *};
use crate::parser::{
    at_keyword, byte, collection_inner, collection_trailing_sep_e, loc, reset_min_indent,
    skip_first, skip_second, specialize_err, succeed, zero_or_more, EExposes, EHeader, EImports,
    EPackageEntry, EPackageName, EPackages, EParams, EProvides, ERequires, ETypedIdent,
    ParseResult, Parser, SourceError, SpaceProblem, SyntaxError,
};
use crate::pattern::parse_record_pattern_fields;
use crate::state::State;
use crate::string_literal::{self, parse_str_literal};
use crate::type_annotation::{type_expr, SKIP_PARSING_SPACES_BEFORE, TRAILING_COMMA_VALID};
use roc_module::ident::IdentSuffix;
use roc_module::symbol::ModuleId;
use roc_region::all::{Loc, Position, Region};

pub fn parse_module_defs<'a>(
    arena: &'a bumpalo::Bump,
    state: State<'a>,
    defs: Defs<'a>,
) -> Result<Defs<'a>, SyntaxError<'a>> {
    match crate::expr::parse_top_level_defs(arena, state.clone(), defs) {
        Ok((_, defs, state)) => {
            if state.has_reached_end() {
                Ok(defs)
            } else {
                Err(SyntaxError::NotEndOfFile(state.pos()))
            }
        }
        Err((_, fail)) => Err(SyntaxError::Expr(fail, state.pos())),
    }
}

pub fn parse_header<'a>(
    arena: &'a bumpalo::Bump,
    state: State<'a>,
) -> Result<(SpacesBefore<'a, Header<'a>>, State<'a>), SourceError<'a, EHeader<'a>>> {
    match header().parse(arena, state.clone(), 0) {
        Ok((_, module, state)) => Ok((module, state)),
        Err((_, fail)) => Err(SourceError::new(fail, &state)),
    }
}

pub fn header<'a>() -> impl Parser<'a, SpacesBefore<'a, Header<'a>>, EHeader<'a>> {
    move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
        let (_, before, state) =
            eat_nc_check(EHeader::IndentStart, arena, state, min_indent, false)?;

        let inc_indent = min_indent + 1;

        if at_keyword("module", &state) {
            let state = state.advance("module".len());
            let (_, out, state) = parse_module_header(arena, state, inc_indent)?;
            let item = Header::Module(out);
            return Ok((MadeProgress, SpacesBefore { before, item }, state));
        }

        if at_keyword("interface", &state) {
            let state = state.advance("interface".len());
            let (_, out, state) = interface_header().parse(arena, state, inc_indent)?;
            let item = Header::Module(out);
            return Ok((MadeProgress, SpacesBefore { before, item }, state));
        }

        if at_keyword("app", &state) {
            let state = state.advance("app".len());
            let (_, out, state) = match app_header().parse(arena, state.clone(), inc_indent) {
                Ok(ok) => ok,
                Err((MadeProgress, fail)) => return Err((MadeProgress, fail)),
                Err((NoProgress, _)) => old_app_header().parse(arena, state, inc_indent)?,
            };
            let item = Header::App(out);
            return Ok((MadeProgress, SpacesBefore { before, item }, state));
        }

        if at_keyword("package", &state) {
            let state = state.advance("package".len());
            let (_, out, state) = match package_header().parse(arena, state.clone(), inc_indent) {
                Ok(ok) => ok,
                Err((MadeProgress, fail)) => return Err((MadeProgress, fail)),
                Err((NoProgress, _)) => old_package_header().parse(arena, state, inc_indent)?,
            };
            let item = Header::Package(out);
            return Ok((MadeProgress, SpacesBefore { before, item }, state));
        }

        if at_keyword("platform", &state) {
            let state = state.advance("platform".len());
            let (_, out, state) = platform_header().parse(arena, state, inc_indent)?;
            let item = Header::Platform(out);
            return Ok((MadeProgress, SpacesBefore { before, item }, state));
        }

        if at_keyword("hosted", &state) {
            let state = state.advance("hosted".len());
            let (_, out, state) = hosted_header().parse(arena, state, inc_indent)?;
            let item = Header::Hosted(out);
            return Ok((MadeProgress, SpacesBefore { before, item }, state));
        }

        Err((NoProgress, EHeader::Start(state.pos())))
    }
}

fn parse_module_header<'a>(
    arena: &'a bumpalo::Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, ModuleHeader<'a>, EHeader<'a>> {
    let (_, after_keyword, state) =
        eat_nc_check(EHeader::IndentStart, arena, state, min_indent, false)?;

    let params_pos = state.pos();
    let (params, state) = match parse_module_params(arena, state.clone(), min_indent) {
        Ok((_, out, state)) => (Some(out), state),
        Err((NoProgress, _)) => (None, state),
        Err((p, fail)) => return Err((p, EHeader::Params(fail, params_pos))),
    };

    let exposes_pos = state.pos();
    let (exposes, state) = match exposes_list().parse(arena, state, min_indent) {
        Ok((_, out, state)) => (out, state),
        Err((p, fail)) => return Err((p, EHeader::Exposes(fail, exposes_pos))),
    };

    let header = ModuleHeader {
        after_keyword,
        params,
        exposes,
        interface_imports: None,
    };
    Ok((MadeProgress, header, state))
}

fn parse_module_params<'a>(
    arena: &'a bumpalo::Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, ModuleParams<'a>, EParams<'a>> {
    let start = state.pos();

    let (pattern, state) = match parse_record_pattern_fields(arena, state) {
        Ok((_, fields, state)) => (Loc::pos(start, state.pos(), fields), state),
        Err((p, fail)) => {
            return Err((p, EParams::Pattern(fail, start)));
        }
    };

    let (_, before_arrow, state) =
        eat_nc_check(EParams::BeforeArrow, arena, state, min_indent, false)?;

    if !state.bytes().starts_with(b"->") {
        return Err((MadeProgress, EParams::Arrow(state.pos())));
    }
    let state = state.advance(2);

    let (_, after_arrow, state) =
        eat_nc_check(EParams::AfterArrow, arena, state, min_indent, false)?;

    let params = ModuleParams {
        pattern,
        before_arrow,
        after_arrow,
    };
    Ok((MadeProgress, params, state))
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
fn interface_header<'a>() -> impl Parser<'a, ModuleHeader<'a>, EHeader<'a>> {
    let after_keyword_p = |arena, state: State<'a>, min_indent: u32| {
        let (p, before_name, state) =
            eat_nc_check(EHeader::IndentStart, arena, state, min_indent, false)?;

        let name_pos = state.pos();
        let state = match parse_module_name(state) {
            Ok((_, _, state)) => state,
            Err((p2, _)) => return Err((p2.or(p), EHeader::ModuleName(name_pos))),
        };

        let kw_pos = state.pos();
        let (_, kw, state) = match exposes_kw().parse(arena, state, min_indent) {
            Ok(ok) => ok,
            Err((p, fail)) => return Err((p, EHeader::Exposes(fail, kw_pos))),
        };
        let out = merge_n_spaces!(arena, before_name, kw.before, kw.after);
        Ok((MadeProgress, out, state))
    };

    record!(ModuleHeader {
        after_keyword: after_keyword_p,
        params: succeed(None),
        exposes: specialize_err(EHeader::Exposes, exposes_list()).trace("exposes_list"),
        interface_imports: imports_option(),
    })
    .trace("interface_header")
}

fn imports_option<'a>() -> impl Parser<'a, Option<ImportsKeywordItem<'a>>, EHeader<'a>> {
    move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
        let start = state.pos();
        match imports().parse(arena, state, min_indent) {
            Ok((p, out, state)) => {
                let out = if out.item.is_empty() { None } else { Some(out) };
                Ok((p, out, state))
            }
            Err((p, fail)) => Err((p, EHeader::Imports(fail, start))),
        }
    }
}

fn hosted_header<'a>() -> impl Parser<'a, HostedHeader<'a>, EHeader<'a>> {
    move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
        let (_, before_name, state) =
            eat_nc_check(EHeader::IndentStart, arena, state, min_indent, false)?;

        let name_pos = state.pos();
        let (_, name, state) = match parse_module_name(state) {
            Ok(ok) => ok,
            Err((p, _)) => return Err((p, EHeader::ModuleName(name_pos))),
        };
        let name = Loc::pos(name_pos, state.pos(), name);

        let exposes_pos = state.pos();
        let (_, exposes, state) = match record!(KeywordItem {
            keyword: exposes_kw(),
            item: exposes_list()
        })
        .parse(arena, state, min_indent)
        {
            Ok(ok) => ok,
            Err((p, fail)) => return Err((p, EHeader::Exposes(fail, exposes_pos))),
        };

        let imports_pos = state.pos();
        let (_, imports, state) = match imports().parse(arena, state, min_indent) {
            Ok(ok) => ok,
            Err((p, fail)) => return Err((p, EHeader::Imports(fail, imports_pos))),
        };

        let header = HostedHeader {
            before_name,
            name,
            exposes,
            imports,
        };
        Ok((MadeProgress, header, state))
    }
}

pub(crate) fn chomp_module_name(buffer: &[u8]) -> Result<&str, Progress> {
    use encode_unicode::CharExt;
    match char::from_utf8_slice_start(buffer) {
        Ok((ch, mut chomped)) if ch.is_uppercase() => {
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

                    if let Ok((next, width)) = char::from_utf8_slice_start(&buffer[chomped..]) {
                        if next.is_uppercase() {
                            chomped += width;
                        } else if next == '{' {
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
        _ => Err(Progress::NoProgress),
    }
}

#[inline(always)]
pub(crate) fn parse_module_name(state: State<'_>) -> ParseResult<'_, ModuleName<'_>, ()> {
    match chomp_module_name(state.bytes()) {
        Ok(name) => {
            let state = state.advance(name.len());
            Ok((MadeProgress, ModuleName::new(name), state))
        }
        Err(p) => Err((p, ())),
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
        packages: move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
            let olds = state.clone();
            match packages().parse(arena, state, min_indent) {
                Ok((p, out, state)) => Ok((p, Some(Loc::pos(olds.pos(), state.pos(), out)), state)),
                Err((NoProgress, _)) => Ok((NoProgress, None, olds)),
                Err((_, fail)) => Err((MadeProgress, EHeader::Packages(fail, olds.pos()))),
            }
        },
        imports: move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
            let olds = state.clone();
            match imports().parse(arena, state, min_indent) {
                Ok((p, out, state)) => Ok((p, Some(out), state)),
                Err((NoProgress, _)) => Ok((NoProgress, None, olds)),
                Err((_, fail)) => Err((MadeProgress, EHeader::Imports(fail, olds.pos()))),
            }
        },
        provides: specialize_err(EHeader::Provides, provides_to()),
    });

    move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| match old
        .parse(arena, state, min_indent)
    {
        Ok((p, old, state)) => {
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

            let out = AppHeader {
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
                old_imports: old
                    .imports
                    .and_then(|x| if x.item.is_empty() { None } else { Some(x) }),
                old_provides_to_new_package: match old.provides.to.value {
                    To::NewPackage(new_pkg) => Some(new_pkg),
                    To::ExistingPackage(_) => None,
                },
            };

            Ok((p, out, state))
        }
        Err(err) => Err(err),
    }
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
    move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| match record!(
        OldPackageHeader {
            before_name: skip_second(
                space0_e(EHeader::IndentStart),
                specialize_err(EHeader::PackageName, package_name())
            ),
            exposes: specialize_err(EHeader::Exposes, exposes_modules()),
            packages: specialize_err(EHeader::Packages, loc(packages())),
        }
    )
    .parse(arena, state, min_indent)
    {
        Ok((p, old, state)) => {
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

            let old = PackageHeader {
                before_exposes,
                exposes: old.exposes.item,
                before_packages,
                packages: old.packages.map(|kw| kw.item),
            };

            Ok((p, old, state))
        }
        Err(err) => Err(err),
    }
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

fn provides_to_package<'a>(
    arena: &'a bumpalo::Bump,
    state: State<'a>,
    min_indent: u32,
) -> ParseResult<'a, To<'a>, EProvides<'a>> {
    let pos = state.pos();
    match parse_lowercase_ident(state.clone()) {
        Ok((p, out, state)) => Ok((p, To::ExistingPackage(out), state)),
        Err((MadeProgress, _)) => Err((MadeProgress, EProvides::Identifier(pos))),
        Err(_) => match package_name().parse(arena, state, min_indent) {
            Ok((p, out, state)) => Ok((p, To::NewPackage(out), state)),
            Err((p, fail)) => Err((p, EProvides::Package(fail, pos))),
        },
    }
}

fn provides_to<'a>() -> impl Parser<'a, ProvidesTo<'a>, EProvides<'a>> {
    (move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
        let (_, provides_keyword, state) = spaces_around_keyword(
            ProvidesKeyword,
            EProvides::Provides,
            EProvides::IndentProvides,
            EProvides::IndentListStart,
        )
        .parse(arena, state, min_indent)?;

        let (_, entries, state) = collection_trailing_sep_e(
            byte(b'[', EProvides::ListStart),
            exposes_entry(EProvides::Identifier),
            byte(b']', EProvides::ListEnd),
            Spaced::SpaceBefore,
        )
        .parse(arena, state, min_indent)?;

        let (types, state) = match provides_types().parse(arena, state.clone(), min_indent) {
            Ok((_, out, state)) => (Some(out), state),
            _ => (None, state),
        };

        let (_, to_keyword, state) = spaces_around_keyword(
            ToKeyword,
            EProvides::To,
            EProvides::IndentTo,
            EProvides::IndentListStart,
        )
        .parse(arena, state, min_indent)?;

        let to_pos = state.pos();
        let (_, to, state) = provides_to_package(arena, state, min_indent)?;
        let to = Loc::pos(to_pos, state.pos(), to);

        let provides_to = ProvidesTo {
            provides_keyword,
            entries,
            types,
            to_keyword,
            to,
        };
        Ok((MadeProgress, provides_to, state))
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
            byte(b']', EProvides::ListEnd),
            Spaced::SpaceBefore
        ),
    })
}

fn provides_types<'a>(
) -> impl Parser<'a, Collection<'a, Loc<Spaced<'a, UppercaseIdent<'a>>>>, EProvides<'a>> {
    let elem_p = move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
        let start = state.pos();
        match ident::uppercase().parse(arena, state, min_indent) {
            Ok(ok) => Ok(ok),
            Err((p, _)) => Err((p, EProvides::Identifier(start))),
        }
    };

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
        skip_first(
            byte(b'{', EProvides::ListStart),
            skip_second(
                reset_min_indent(collection_inner(elem_p, Spaced::SpaceBefore)),
                byte(b'}', EProvides::ListEnd),
            ),
        ),
    )
}

fn exposes_entry<'a, F, E>(
    to_expectation: F,
) -> impl Parser<'a, Loc<Spaced<'a, ExposedName<'a>>>, E>
where
    F: Fn(Position) -> E,
    F: Copy,
    E: 'a,
{
    move |_: &'a bumpalo::Bump, state: State<'a>, _: u32| {
        let ident_pos = state.pos();
        match parse_anycase_ident(state) {
            Ok((p, ident, state)) => {
                let ident = Spaced::Item(ExposedName::new(ident));
                let ident = Loc::pos(ident_pos, state.pos(), ident);
                Ok((p, ident, state))
            }
            Err((p, _)) => Err((p, to_expectation(ident_pos))),
        }
    }
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
        specialize_err(|_, pos| ERequires::Rigid(pos), ident::uppercase()),
        byte(b'}', ERequires::ListEnd),
        Spaced::SpaceBefore,
    )
}

#[inline(always)]
fn requires_typed_ident<'a>(
) -> impl Parser<'a, Collection<'a, Loc<Spaced<'a, TypedIdent<'a>>>>, ERequires<'a>> {
    skip_first(
        byte(b'{', ERequires::ListStart),
        skip_second(
            reset_min_indent(collection_inner(
                specialize_err(ERequires::TypedIdent, loc(typed_ident())),
                Spaced::SpaceBefore,
            )),
            byte(b'}', ERequires::ListEnd),
        ),
    )
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

fn exposes_list<'a>() -> impl Parser<'a, Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>, EExposes>
{
    move |arena: &'a bumpalo::Bump, state: State<'a>, _: u32| {
        if state.bytes().first() != Some(&b'[') {
            return Err((NoProgress, EExposes::ListStart(state.pos())));
        }
        let state = state.inc();

        let (entries, state) =
            match collection_inner(exposes_entry(EExposes::Identifier), Spaced::SpaceBefore)
                .parse(arena, state, 0)
            {
                Ok((_, out, state)) => (out, state),
                Err((_, fail)) => return Err((MadeProgress, fail)),
            };

        if state.bytes().first() != Some(&b']') {
            return Err((MadeProgress, EExposes::ListEnd(state.pos())));
        }
        let state = state.inc();

        Ok((MadeProgress, entries, state))
    }
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
    move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
        let (before, state) = match eat_nc_check(indent_problem1, arena, state, min_indent, false) {
            Ok((_, sp, state)) => (sp, state),
            Err((_, fail)) => return Err((NoProgress, fail)),
        };

        if !at_keyword(K::KEYWORD, &state) {
            return Err((NoProgress, expectation(state.pos())));
        }
        let state = state.advance(K::KEYWORD.len());

        let (_, after, state) = eat_nc_check(indent_problem2, arena, state, min_indent, false)?;

        let spaced_keyword = Spaces {
            before,
            item: keyword_item,
            after,
        };
        Ok((MadeProgress, spaced_keyword, state))
    }
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
        exposes_module(),
        byte(b']', EExposes::ListEnd),
        Spaced::SpaceBefore,
    )
}

fn exposes_module<'a>() -> impl Parser<'a, Loc<Spaced<'a, ModuleName<'a>>>, EExposes> {
    move |_: &'a bumpalo::Bump, state: State<'a>, _: u32| {
        let start = state.pos();
        match parse_module_name(state) {
            Ok((p, out, state)) => Ok((p, Loc::pos(start, state.pos(), Spaced::Item(out)), state)),
            Err((p, _)) => Err((p, EExposes::Identifier(start))),
        }
    }
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
            byte(b']', EImports::ListEnd),
            Spaced::SpaceBefore
        )
    })
    .trace("imports")
}

/// e.g. printLine : Str -> Effect {}
pub fn typed_ident<'a>() -> impl Parser<'a, Spaced<'a, TypedIdent<'a>>, ETypedIdent<'a>> {
    move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
        let start = state.pos();

        let (ident, state) = match parse_lowercase_ident(state) {
            Ok((_, out, state)) => (Loc::pos(start, state.pos(), out), state),
            Err((p, _)) => return Err((p, ETypedIdent::Identifier(start))),
        };

        let (_, spaces_before_colon, state) =
            eat_nc_check(ETypedIdent::IndentHasType, arena, state, min_indent, true)?;

        if state.bytes().first() != Some(&b':') {
            return Err((MadeProgress, ETypedIdent::HasType(state.pos())));
        }
        let state = state.inc();

        let (_, spaces_after_colon, state) =
            eat_nc_check(ETypedIdent::IndentType, arena, state, min_indent, true)?;

        let ann_pos = state.pos();
        match type_expr(TRAILING_COMMA_VALID | SKIP_PARSING_SPACES_BEFORE).parse(arena, state, 0) {
            Ok((_, ann, state)) => {
                let typed_ident = Spaced::Item(TypedIdent {
                    ident,
                    spaces_before_colon,
                    ann: ann.spaced_before(arena, spaces_after_colon),
                });
                Ok((MadeProgress, typed_ident, state))
            }
            Err((_, fail)) => Err((MadeProgress, ETypedIdent::Type(fail, ann_pos))),
        }
    }
}

fn imports_entry<'a>() -> impl Parser<'a, Spaced<'a, ImportsEntry<'a>>, EImports> {
    move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
        let p_name_module_name = move |_: &'a bumpalo::Bump, state: State<'a>, _: u32| {
            let (name, state) = match parse_lowercase_ident(state.clone()) {
                Ok((_, name, state)) => {
                    if state.bytes().first() == Some(&b'.') {
                        (Some(name), state.inc())
                    } else {
                        (None, state)
                    }
                }
                Err(_) => (None, state),
            };

            let module_name_pos = state.pos();
            match parse_module_name(state) {
                Ok((p, module_name, state)) => Ok((p, (name, module_name), state)),
                Err((p, _)) => Err((p, EImports::ModuleName(module_name_pos))),
            }
        };

        // e.g. `.{ Task, after}`
        let p_opt_values = move |arena: &'a bumpalo::Bump, state: State<'a>, _: u32| {
            if state.bytes().first() != Some(&b'.') {
                return Ok((NoProgress, Collection::empty(), state));
            }
            let state = state.inc();

            if state.bytes().first() != Some(&b'{') {
                return Err((MadeProgress, EImports::SetStart(state.pos())));
            }
            let state = state.inc();

            let elem_p = exposes_entry(EImports::Identifier);
            let (_, opt_values, state) =
                match collection_inner(elem_p, Spaced::SpaceBefore).parse(arena, state, 0) {
                    Ok(ok) => ok,
                    Err((_, fail)) => return Err((MadeProgress, fail)),
                };

            if state.bytes().first() != Some(&b'}') {
                return Err((MadeProgress, EImports::SetEnd(state.pos())));
            }
            let state = state.inc();
            Ok((MadeProgress, opt_values, state))
        };

        match p_name_module_name.parse(arena, state.clone(), min_indent) {
            Err((NoProgress, _)) => { /*goto below */ }
            Err(err) => return Err(err),
            Ok((_, (opt_shortname, module_name), state)) => {
                match p_opt_values.parse(arena, state, min_indent) {
                    Err((_, fail)) => return Err((MadeProgress, fail)),
                    Ok((_, opt_values, state)) => {
                        let entry = match opt_shortname {
                            Some(shortname) => {
                                ImportsEntry::Package(shortname, module_name, opt_values)
                            }
                            None => ImportsEntry::Module(module_name, opt_values),
                        };
                        return Ok((MadeProgress, Spaced::Item(entry), state));
                    }
                }
            }
        };

        // e.g. "filename"
        // TODO: str literal allows for multiline strings. We probably don't want that for file names.
        let file_name_pos = state.pos();
        let (_, file_name, state) = match parse_str_literal().parse(arena, state, min_indent) {
            Ok(ok) => ok,
            Err((p, _)) => return Err((p, EImports::StrLiteral(file_name_pos))),
        };

        let (.., state) = eat_nc_check(EImports::AsKeyword, arena, state, min_indent, true)?;
        if !state.bytes().starts_with(b"as") {
            return Err((MadeProgress, EImports::AsKeyword(state.pos())));
        }
        let state = state.advance(2);
        let (.., state) = eat_nc_check(EImports::AsKeyword, arena, state, min_indent, true)?;

        // e.g. file : Str
        let typed_ident_pos = state.pos();
        let (_, typed_ident, state) = match typed_ident().parse(arena, state, min_indent) {
            Ok(ok) => ok,
            Err(_) => return Err((MadeProgress, EImports::TypedIdent(typed_ident_pos))),
        };

        // TODO: look at blacking block strings during parsing.
        let entry = Spaced::Item(ImportsEntry::IngestedFile(file_name, typed_ident));
        Ok((MadeProgress, entry, state))
    }
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
    pub fn to_string(&'a self) -> &str {
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
    move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
        let shorthand_p = move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
            let ident_pos = state.pos();
            let (_, ident, state) = match parse_lowercase_ident(state.clone()) {
                Ok(ok) => ok,
                Err((NoProgress, _)) => return Ok((NoProgress, None, state)),
                Err(_) => return Err((MadeProgress, EPackageEntry::Shorthand(ident_pos))),
            };

            let (_, spaces_before_colon, state) =
                eat_nc_check(EPackageEntry::IndentPackage, arena, state, min_indent, true)?;

            if state.bytes().first() != Some(&b':') {
                return Err((MadeProgress, EPackageEntry::Colon(state.pos())));
            }
            let state = state.inc();

            let (_, spaces_after_colon, state) =
                eat_nc_check(EPackageEntry::IndentPackage, arena, state, min_indent, true)?;

            let out = ((ident, spaces_before_colon), spaces_after_colon);
            Ok((MadeProgress, Some(out), state))
        };

        let plat_parser = move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
            let olds = state.clone();
            let (p, sp, state) = if at_keyword(crate::keyword::PLATFORM, &state) {
                let state = state.advance(crate::keyword::PLATFORM.len());
                let (_, sp, state) =
                    eat_nc_check(EPackageEntry::IndentPackage, arena, state, min_indent, true)?;
                (MadeProgress, Some(sp), state)
            } else {
                (NoProgress, None, olds)
            };
            let name_pos = state.pos();
            match package_name().parse(arena, state, min_indent) {
                Ok((p2, name, state)) => {
                    Ok((p2.or(p), (sp, Loc::pos(name_pos, state.pos(), name)), state))
                }
                Err((p2, fail)) => Err((p2.or(p), EPackageEntry::BadPackage(fail, name_pos))),
            }
        };

        let (p, opt_shorthand, state) = shorthand_p.parse(arena, state, min_indent)?;

        let (_, (platform_marker, package_or_path), state) =
            match plat_parser.parse(arena, state, min_indent) {
                Ok(ok) => ok,
                Err((p2, fail)) => return Err((p2.or(p), fail)),
            };

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

        Ok((MadeProgress, Spaced::Item(entry), state))
    }
}

pub fn package_name<'a>() -> impl Parser<'a, PackageName<'a>, EPackageName<'a>> {
    move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
        let name_pos = state.pos();
        match string_literal::parse_str_literal().parse(arena, state, min_indent) {
            Ok((p, name, state)) => match name {
                StrLiteral::PlainLine(text) => Ok((p, PackageName(text), state)),
                StrLiteral::Line(_) => Err((p, EPackageName::Escapes(name_pos))),
                StrLiteral::Block(_) => Err((p, EPackageName::Multiline(name_pos))),
            },
            Err((p, fail)) => Err((p, EPackageName::BadPath(fail, name_pos))),
        }
    }
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
