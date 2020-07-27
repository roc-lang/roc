use crate::ast::{
    AppHeader, Attempting, CommentOrNewline, Def, ExposesEntry, ImportsEntry, InterfaceHeader,
    Module,
};
use crate::blankspace::{space0_around, space1};
use crate::expr::def;
use crate::header::ModuleName;
use crate::ident::unqualified_ident;
use crate::parser::{
    self, ascii_char, ascii_string, loc, optional, peek_utf8_char, peek_utf8_char_at, unexpected,
    Parser, State,
};
use bumpalo::collections::{String, Vec};
use roc_region::all::Located;

pub fn header<'a>() -> impl Parser<'a, Module<'a>> {
    one_of!(interface_module(), app_module())
}

#[inline(always)]
pub fn interface_module<'a>() -> impl Parser<'a, Module<'a>> {
    map!(interface_header(), |header| {
        Module::Interface { header }
    })
}

#[inline(always)]
pub fn app_module<'a>() -> impl Parser<'a, Module<'a>> {
    map!(app_header(), |header| { Module::App { header } })
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
            (after_interface, name),
            (
                ((before_exposes, after_exposes), exposes),
                ((before_imports, after_imports), imports),
            ),
        )| {
            InterfaceHeader {
                name,
                exposes,
                imports,
                after_interface,
                before_exposes,
                after_exposes,
                before_imports,
                after_imports,
            }
        },
    )
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
            (after_interface, name),
            (
                ((before_provides, after_provides), provides),
                ((before_imports, after_imports), imports),
            ),
        )| {
            AppHeader {
                name,
                provides,
                imports,
                after_interface,
                before_provides,
                after_provides,
                before_imports,
                after_imports,
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
