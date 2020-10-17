use crate::ast::{
    AppHeader, Attempting, CommentOrNewline, Def, EffectsEntry, ExposesEntry, ImportsEntry,
    InterfaceHeader, Module, PlatformHeader,
};
use crate::blankspace::{space0_around, space1};
use crate::expr::def;
use crate::header::{ModuleName, PackageName};
use crate::ident::unqualified_ident;
use crate::parser::{
    self, ascii_char, ascii_string, loc, optional, peek_utf8_char, peek_utf8_char_at, unexpected,
    unexpected_eof, ParseResult, Parser, State,
};
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
            and!(exposes(), imports())
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
            move |arena, state| parse_package_part(arena, state),
            skip_first!(ascii_char('/'), move |arena, state| parse_package_part(
                arena, state
            ))
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
                    return Ok((part_buf.into_bump_str(), state));
                }
            }
            Err(reason) => return state.fail(reason),
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
                                                ModuleName::new(buf.into_bump_str()),
                                                state,
                                            ));
                                        }
                                    }
                                    Err(reason) => return state.fail(reason),
                                }
                            } else {
                                // This is the end of the module name. We're done!
                                break;
                            }
                        }
                        Err(reason) => return state.fail(reason),
                    }
                }

                Ok((ModuleName::new(buf.into_bump_str()), state))
            }
            Err(reason) => state.fail(reason),
        }
    }
}

#[inline(always)]
fn app_header<'a>() -> impl Parser<'a, AppHeader<'a>> {
    parser::map(
        and!(
            skip_first!(ascii_string("app"), and!(space1(1), loc!(module_name()))),
            and!(provides(), imports())
        ),
        |(
            (after_app_keyword, name),
            (
                ((before_provides, after_provides), provides),
                ((before_imports, after_imports), imports),
            ),
        )| {
            AppHeader {
                name,
                provides,
                imports,
                after_app_keyword,
                before_provides,
                after_provides,
                before_imports,
                after_imports,
            }
        },
    )
}

#[inline(always)]
fn platform_header<'a>() -> impl Parser<'a, PlatformHeader<'a>> {
    parser::map(
        and!(
            skip_first!(
                ascii_string("platform"),
                and!(space1(1), loc!(package_name()))
            ),
            and!(provides(), and!(requires(), and!(imports(), effects())))
        ),
        |(
            (after_platform_keyword, name),
            (
                ((before_provides, after_provides), provides),
                (
                    ((before_requires, after_requires), requires),
                    (
                        ((before_imports, after_imports), imports),
                        ((before_effects, after_effects), effects),
                    ),
                ),
            ),
        )| {
            PlatformHeader {
                name,
                provides,
                requires,
                imports,
                effects,
                after_platform_keyword,
                before_provides,
                after_provides,
                before_requires,
                after_requires,
                before_imports,
                after_imports,
                before_effects,
                after_effects,
            }
        },
    )
}

#[inline(always)]
pub fn module_defs<'a>() -> impl Parser<'a, Vec<'a, Located<Def<'a>>>> {
    zero_or_more!(space0_around(loc(def(0)), 0))
}

#[inline(always)]
fn provides<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Vec<'a, Located<ExposesEntry<'a>>>,
    ),
> {
    and!(
        and!(skip_second!(space1(1), ascii_string("provides")), space1(1)),
        collection!(
            ascii_char('['),
            loc!(exposes_entry()),
            ascii_char(','),
            ascii_char(']'),
            1
        )
    )
}

#[inline(always)]
fn requires<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Vec<'a, Located<ExposesEntry<'a>>>,
    ),
> {
    and!(
        and!(skip_second!(space1(1), ascii_string("requires")), space1(1)),
        collection!(
            ascii_char('['),
            loc!(exposes_entry()),
            ascii_char(','),
            ascii_char(']'),
            1
        )
    )
}

#[inline(always)]
fn exposes<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Vec<'a, Located<ExposesEntry<'a>>>,
    ),
> {
    and!(
        and!(skip_second!(space1(1), ascii_string("exposes")), space1(1)),
        collection!(
            ascii_char('['),
            loc!(exposes_entry()),
            ascii_char(','),
            ascii_char(']'),
            1
        )
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
        and!(skip_second!(space1(1), ascii_string("imports")), space1(1)),
        collection!(
            ascii_char('['),
            loc!(imports_entry()),
            ascii_char(','),
            ascii_char(']'),
            1
        )
    )
}

#[inline(always)]
fn effects<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Vec<'a, Located<EffectsEntry<'a>>>,
    ),
> {
    and!(
        and!(skip_second!(space1(1), ascii_string("effects")), space1(1)),
        collection!(
            ascii_char('{'),
            loc!(effects_entry()),
            ascii_char(','),
            ascii_char('}'),
            1
        )
    )
}

#[inline(always)]
fn effects_entry<'a>() -> impl Parser<'a, EffectsEntry<'a>> {
    // e.g.
    //
    // printLine : Str -> Effect {}
    map!(
        and!(loc(unqualified_ident()), type_annotation::located(0)),
        |(ident, ann)| { EffectsEntry::Effect { ident, ann } }
    )
}

#[inline(always)]
fn exposes_entry<'a>() -> impl Parser<'a, ExposesEntry<'a>> {
    map!(unqualified_ident(), ExposesEntry::Ident)
}

#[inline(always)]
fn imports_entry<'a>() -> impl Parser<'a, ImportsEntry<'a>> {
    map_with_arena!(
        and!(
            // e.g. `Task`
            module_name(),
            // e.g. `.{ Task, after}`
            optional(skip_first!(
                ascii_char('.'),
                collection!(
                    ascii_char('{'),
                    loc!(exposes_entry()),
                    ascii_char(','),
                    ascii_char('}'),
                    1
                )
            ))
        ),
        |arena,
         (module_name, opt_values): (
            ModuleName<'a>,
            Option<Vec<'a, Located<ExposesEntry<'a>>>>
        )| {
            let exposed_values = opt_values.unwrap_or_else(|| Vec::new_in(arena));

            ImportsEntry::Module(module_name, exposed_values)
        }
    )
}
