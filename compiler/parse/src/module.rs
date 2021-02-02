use crate::ast::{Attempting, CommentOrNewline, Def, Module};
use crate::blankspace::{space0, space0_around, space0_before, space1};
use crate::expr::def;
use crate::header::{
    package_entry, package_or_path, AppHeader, Effects, ExposesEntry, ImportsEntry,
    InterfaceHeader, ModuleName, PackageEntry, PackageName, PackageOrPath, PlatformHeader, To,
    TypedIdent,
};
use crate::ident::{lowercase_ident, unqualified_ident, uppercase_ident};
use crate::parser::Progress::{self, *};
use crate::parser::{
    self, ascii_char, ascii_string, backtrackable, end_of_file, fail_when_progress, loc, optional,
    peek_utf8_char, peek_utf8_char_at, unexpected, unexpected_eof, Either, ParseResult, Parser,
    State,
};
use crate::string_literal;
use crate::type_annotation;
use bumpalo::collections::{String, Vec};
use bumpalo::Bump;
use roc_region::all::Located;

pub fn header<'a>() -> impl Parser<'a, Module<'a>> {
    one_of!(interface_module(), app_module(), platform_module())
}

#[inline(always)]
fn app_module<'a>() -> impl Parser<'a, Module<'a>> {
    map!(app_header(), |header| { Module::App { header } })
}

#[inline(always)]
fn platform_module<'a>() -> impl Parser<'a, Module<'a>> {
    map!(platform_header(), |header| { Module::Platform { header } })
}

#[inline(always)]
fn interface_module<'a>() -> impl Parser<'a, Module<'a>> {
    map!(interface_header(), |header| {
        Module::Interface { header }
    })
}

#[inline(always)]
pub fn interface_header<'a>() -> impl Parser<'a, InterfaceHeader<'a>> {
    parser::map(
        and!(
            skip_first!(
                ascii_string("interface"),
                and!(space1(1), loc!(module_name()))
            ),
            and!(exposes_values(), imports())
        ),
        |(
            (after_interface_keyword, name),
            (
                ((before_exposes, after_exposes), exposes),
                ((before_imports, after_imports), imports),
            ),
        )| {
            InterfaceHeader {
                name,
                exposes,
                imports,
                after_interface_keyword,
                before_exposes,
                after_exposes,
                before_imports,
                after_imports,
            }
        },
    )
}

#[inline(always)]
pub fn package_name<'a>() -> impl Parser<'a, PackageName<'a>> {
    // e.g. rtfeldman/blah
    //
    // Package names and accounts can be capitalized and can contain dashes.
    // They cannot contain underscores or other special characters.
    // They must be ASCII.

    map!(
        and!(
            parse_package_part,
            skip_first!(ascii_char(b'/'), parse_package_part)
        ),
        |(account, pkg)| { PackageName { account, pkg } }
    )
}

pub fn parse_package_part<'a>(arena: &'a Bump, mut state: State<'a>) -> ParseResult<'a, &'a str> {
    let mut part_buf = String::new_in(arena); // The current "part" (parts are dot-separated.)

    while !state.bytes.is_empty() {
        match peek_utf8_char(&state) {
            Ok((ch, bytes_parsed)) => {
                if ch == '-' || ch.is_ascii_alphanumeric() {
                    part_buf.push(ch);

                    state = state.advance_without_indenting(bytes_parsed)?;
                } else {
                    let progress = Progress::progress_when(!part_buf.is_empty());
                    return Ok((progress, part_buf.into_bump_str(), state));
                }
            }
            Err(reason) => {
                let progress = Progress::progress_when(!part_buf.is_empty());
                return state.fail(progress, reason);
            }
        }
    }

    Err(unexpected_eof(0, state.attempting, state))
}

#[inline(always)]
pub fn module_name<'a>() -> impl Parser<'a, ModuleName<'a>> {
    move |arena, mut state: State<'a>| {
        match peek_utf8_char(&state) {
            Ok((first_letter, bytes_parsed)) => {
                if !first_letter.is_uppercase() {
                    return Err(unexpected(0, state, Attempting::Module));
                };

                let mut buf = String::with_capacity_in(4, arena);

                buf.push(first_letter);

                state = state.advance_without_indenting(bytes_parsed)?;

                while !state.bytes.is_empty() {
                    match peek_utf8_char(&state) {
                        Ok((ch, bytes_parsed)) => {
                            // After the first character, only these are allowed:
                            //
                            // * Unicode alphabetic chars - you might include `鹏` if that's clear to your readers
                            // * ASCII digits - e.g. `1` but not `¾`, both of which pass .is_numeric()
                            // * A '.' separating module parts
                            if ch.is_alphabetic() || ch.is_ascii_digit() {
                                state = state.advance_without_indenting(bytes_parsed)?;

                                buf.push(ch);
                            } else if ch == '.' {
                                match peek_utf8_char_at(&state, 1) {
                                    Ok((next, next_bytes_parsed)) => {
                                        if next.is_uppercase() {
                                            // If we hit another uppercase letter, keep going!
                                            buf.push('.');
                                            buf.push(next);

                                            state = state.advance_without_indenting(
                                                bytes_parsed + next_bytes_parsed,
                                            )?;
                                        } else {
                                            // We have finished parsing the module name.
                                            //
                                            // There may be an identifier after this '.',
                                            // e.g. "baz" in `Foo.Bar.baz`
                                            return Ok((
                                                MadeProgress,
                                                ModuleName::new(buf.into_bump_str()),
                                                state,
                                            ));
                                        }
                                    }
                                    Err(reason) => return state.fail(MadeProgress, reason),
                                }
                            } else {
                                // This is the end of the module name. We're done!
                                break;
                            }
                        }
                        Err(reason) => return state.fail(MadeProgress, reason),
                    }
                }

                Ok((MadeProgress, ModuleName::new(buf.into_bump_str()), state))
            }
            Err(reason) => state.fail(MadeProgress, reason),
        }
    }
}

#[inline(always)]
pub fn app_header<'a>() -> impl Parser<'a, AppHeader<'a>> {
    map_with_arena!(
        and!(
            skip_first!(
                ascii_string("app"),
                and!(space1(1), loc!(string_literal::parse()))
            ),
            and!(
                optional(packages()),
                and!(optional(imports()), provides_to())
            )
        ),
        |arena, ((after_app_keyword, name), (opt_pkgs, (opt_imports, provides)))| {
            let (before_packages, after_packages, package_entries) = match opt_pkgs {
                Some(pkgs) => {
                    let pkgs: Packages<'a> = pkgs; // rustc must be told the type here

                    (
                        pkgs.before_packages_keyword,
                        pkgs.after_packages_keyword,
                        pkgs.entries,
                    )
                }
                None => (&[] as _, &[] as _, Vec::new_in(arena)),
            };

            // rustc must be told the type here
            #[allow(clippy::type_complexity)]
            let opt_imports: Option<(
                (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
                Vec<'a, Located<ImportsEntry<'a>>>,
            )> = opt_imports;

            let ((before_imports, after_imports), imports) =
                opt_imports.unwrap_or_else(|| ((&[] as _, &[] as _), Vec::new_in(arena)));
            let provides: ProvidesTo<'a> = provides; // rustc must be told the type here

            AppHeader {
                name,
                packages: package_entries,
                imports,
                provides: provides.entries,
                to: provides.to,
                after_app_keyword,
                before_packages,
                after_packages,
                before_imports,
                after_imports,
                before_provides: provides.before_provides_keyword,
                after_provides: provides.after_provides_keyword,
                before_to: provides.before_to_keyword,
                after_to: provides.after_to_keyword,
            }
        }
    )
}

#[inline(always)]
pub fn platform_header<'a>() -> impl Parser<'a, PlatformHeader<'a>> {
    parser::map(
        and!(
            skip_first!(
                ascii_string("platform"),
                and!(space1(1), loc!(package_name()))
            ),
            and!(
                and!(
                    and!(requires(), and!(exposes_modules(), packages())),
                    and!(imports(), provides_without_to())
                ),
                effects()
            )
        ),
        |(
            (after_platform_keyword, name),
            (
                (
                    (
                        ((before_requires, after_requires), requires),
                        (((before_exposes, after_exposes), exposes), packages),
                    ),
                    (
                        ((before_imports, after_imports), imports),
                        ((before_provides, after_provides), provides),
                    ),
                ),
                effects,
            ),
        )| {
            PlatformHeader {
                name,
                requires,
                exposes,
                packages: packages.entries,
                imports,
                provides,
                effects,
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
            }
        },
    )
}

#[inline(always)]
pub fn module_defs<'a>() -> impl Parser<'a, Vec<'a, Located<Def<'a>>>> {
    move |a: &'a Bump, s: State<'a>| {
        // this parses just the defs
        let defs = zero_or_more!(space0_around(loc(debug!(def(0))), 0));

        // let result = skip_second!(defs, end_of_file()).parse(a, s);
        let result = defs.parse(a, s);

        result
    }
}

struct ProvidesTo<'a> {
    entries: Vec<'a, Located<ExposesEntry<'a, &'a str>>>,
    to: Located<To<'a>>,

    before_provides_keyword: &'a [CommentOrNewline<'a>],
    after_provides_keyword: &'a [CommentOrNewline<'a>],
    before_to_keyword: &'a [CommentOrNewline<'a>],
    after_to_keyword: &'a [CommentOrNewline<'a>],
}

#[inline(always)]
fn provides_to<'a>() -> impl Parser<'a, ProvidesTo<'a>> {
    map!(
        and!(
            and!(
                skip_second!(backtrackable(space1(1)), ascii_string("provides")),
                space1(1)
            ),
            and!(
                collection!(
                    ascii_char(b'['),
                    loc!(map!(unqualified_ident(), ExposesEntry::Exposed)),
                    ascii_char(b','),
                    ascii_char(b']'),
                    1
                ),
                and!(
                    space1(1),
                    skip_first!(
                        ascii_string("to"),
                        and!(
                            space1(1),
                            loc!(either!(lowercase_ident(), package_or_path()))
                        )
                    )
                )
            )
        ),
        |(
            (before_provides_keyword, after_provides_keyword),
            (entries, (before_to_keyword, (after_to_keyword, loc_to))),
        )| {
            let loc_to: Located<Either<&'a str, PackageOrPath<'a>>> = loc_to;
            let to_val = match loc_to.value {
                Either::First(pkg) => To::ExistingPackage(pkg),
                Either::Second(pkg) => To::NewPackage(pkg),
            };
            let to = Located {
                value: to_val,
                region: loc_to.region,
            };

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
        Vec<'a, Located<ExposesEntry<'a, &'a str>>>,
    ),
> {
    and!(
        and!(skip_second!(space1(1), ascii_string("provides")), space1(1)),
        collection!(
            ascii_char(b'['),
            loc!(map!(unqualified_ident(), ExposesEntry::Exposed)),
            ascii_char(b','),
            ascii_char(b']'),
            1
        )
    )
}

#[inline(always)]
fn requires<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Vec<'a, Located<TypedIdent<'a>>>,
    ),
> {
    and!(
        and!(skip_second!(space1(1), ascii_string("requires")), space1(1)),
        collection!(
            ascii_char(b'{'),
            loc!(typed_ident()),
            ascii_char(b','),
            ascii_char(b'}'),
            1
        )
    )
}

#[inline(always)]
fn exposes_values<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Vec<'a, Located<ExposesEntry<'a, &'a str>>>,
    ),
> {
    and!(
        and!(skip_second!(space1(1), ascii_string("exposes")), space1(1)),
        collection!(
            ascii_char(b'['),
            loc!(map!(unqualified_ident(), ExposesEntry::Exposed)),
            ascii_char(b','),
            ascii_char(b']'),
            1
        )
    )
}

#[inline(always)]
fn exposes_modules<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Vec<'a, Located<ExposesEntry<'a, ModuleName<'a>>>>,
    ),
> {
    and!(
        and!(skip_second!(space1(1), ascii_string("exposes")), space1(1)),
        collection!(
            ascii_char(b'['),
            loc!(map!(module_name(), ExposesEntry::Exposed)),
            ascii_char(b','),
            ascii_char(b']'),
            1
        )
    )
}

#[derive(Debug)]
struct Packages<'a> {
    entries: Vec<'a, Located<PackageEntry<'a>>>,

    before_packages_keyword: &'a [CommentOrNewline<'a>],
    after_packages_keyword: &'a [CommentOrNewline<'a>],
}

#[inline(always)]
fn packages<'a>() -> impl Parser<'a, Packages<'a>> {
    map!(
        and!(
            and!(
                skip_second!(backtrackable(space1(1)), ascii_string("packages")),
                space1(1)
            ),
            collection!(
                ascii_char(b'{'),
                loc!(package_entry()),
                ascii_char(b','),
                ascii_char(b'}'),
                1
            )
        ),
        |((before_packages_keyword, after_packages_keyword), entries)| {
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
        Vec<'a, Located<ImportsEntry<'a>>>,
    ),
> {
    and!(
        and!(
            skip_second!(backtrackable(space1(1)), ascii_string("imports")),
            space1(1)
        ),
        collection!(
            ascii_char(b'['),
            loc!(imports_entry()),
            ascii_char(b','),
            ascii_char(b']'),
            1
        )
    )
}

#[inline(always)]
fn effects<'a>() -> impl Parser<'a, Effects<'a>> {
    move |arena, state| {
        let (_, spaces_before_effects_keyword, state) =
            skip_second!(space1(0), ascii_string("effects")).parse(arena, state)?;
        let (_, spaces_after_effects_keyword, state) = space1(0).parse(arena, state)?;

        // e.g. `fx.`
        let (_, type_shortname, state) =
            skip_second!(lowercase_ident(), ascii_char(b'.')).parse(arena, state)?;

        let (_, (type_name, spaces_after_type_name), state) =
            and!(uppercase_ident(), space1(0)).parse(arena, state)?;
        let (_, entries, state) = collection!(
            ascii_char(b'{'),
            loc!(typed_ident()),
            ascii_char(b','),
            ascii_char(b'}'),
            1
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
                entries: entries.into_bump_slice(),
            },
            state,
        ))
    }
}

#[inline(always)]
fn typed_ident<'a>() -> impl Parser<'a, TypedIdent<'a>> {
    move |arena, state| {
        // You must have a field name, e.g. "email"
        let (_, ident, state) = loc!(lowercase_ident()).parse(arena, state)?;

        let (_, spaces_before_colon, state) = space0(0).parse(arena, state)?;

        let (_, ann, state) = skip_first!(
            ascii_char(b':'),
            space0_before(type_annotation::located(0), 0)
        )
        .parse(arena, state)?;

        // e.g.
        //
        // printLine : Str -> Effect {}

        Ok((
            MadeProgress,
            TypedIdent::Entry {
                ident,
                spaces_before_colon,
                ann,
            },
            state,
        ))
    }
}

#[inline(always)]
#[allow(clippy::type_complexity)]
fn imports_entry<'a>() -> impl Parser<'a, ImportsEntry<'a>> {
    map_with_arena!(
        and!(
            and!(
                // e.g. `base.`
                optional(skip_second!(lowercase_ident(), ascii_char(b'.'))),
                // e.g. `Task`
                module_name()
            ),
            // e.g. `.{ Task, after}`
            optional(skip_first!(
                ascii_char(b'.'),
                collection!(
                    ascii_char(b'{'),
                    loc!(map!(unqualified_ident(), ExposesEntry::Exposed)),
                    ascii_char(b','),
                    ascii_char(b'}'),
                    1
                )
            ))
        ),
        |arena,
         ((opt_shortname, module_name), opt_values): (
            (Option<&'a str>, ModuleName<'a>),
            Option<Vec<'a, Located<ExposesEntry<'a, &'a str>>>>
        )| {
            let exposed_values = opt_values.unwrap_or_else(|| Vec::new_in(arena));

            match opt_shortname {
                Some(shortname) => ImportsEntry::Package(shortname, module_name, exposed_values),

                None => ImportsEntry::Module(module_name, exposed_values),
            }
        }
    )
}
