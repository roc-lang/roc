use bumpalo::collections::{String, Vec};
use module::ModuleName;
use parse::ast::{
    AppHeader, Attempting, CommentOrNewline, Def, ExposesEntry, ImportsEntry, InterfaceHeader,
    Module,
};
use parse::blankspace::{space1, space1_around};
use parse::ident::unqualified_ident;
use parse::parse;
use parse::parser::{self, char, loc, optional, string, unexpected, unexpected_eof, Parser, State};
use region::Located;

pub fn module<'a>() -> impl Parser<'a, Module<'a>> {
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
            skip_first!(string("interface"), and!(space1(1), loc!(module_name()))),
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
    move |arena, state: State<'a>| {
        let mut chars = state.input.chars();

        let first_letter = match chars.next() {
            Some(first_char) => {
                // Module names must all be uppercase
                if first_char.is_uppercase() {
                    first_char
                } else {
                    return Err(unexpected(
                        first_char,
                        0,
                        state,
                        Attempting::RecordFieldLabel,
                    ));
                }
            }
            None => {
                return Err(unexpected_eof(0, Attempting::Identifier, state));
            }
        };

        let mut buf = String::with_capacity_in(1, arena);

        buf.push(first_letter);

        while let Some(ch) = chars.next() {
            // After the first character, only these are allowed:
            //
            // * Unicode alphabetic chars - you might include `鹏` if that's clear to your readers
            // * ASCII digits - e.g. `1` but not `¾`, both of which pass .is_numeric()
            // * A '.' separating module parts
            if ch.is_alphabetic() || ch.is_ascii_digit() {
                buf.push(ch);
            } else if ch == '.' {
                match chars.next() {
                    Some(next) => {
                        if next.is_uppercase() {
                            // If we hit another uppercase letter, keep going!
                            buf.push('.');
                            buf.push(next);
                        } else {
                            // We have finished parsing the module name.
                            //
                            // There may be an identifier after this '.',
                            // e.g. "baz" in `Foo.Bar.baz`
                            break;
                        }
                    }
                    None => {
                        // A module name can't end with a '.'
                        return Err(unexpected_eof(0, Attempting::Identifier, state));
                    }
                }
            } else {
                // This is the end of the module name. We're done!
                break;
            }
        }

        let chars_parsed = buf.len();

        Ok((
            ModuleName::new(buf.into_bump_str()),
            state.advance_without_indenting(chars_parsed)?,
        ))
    }
}

#[inline(always)]
fn app_header<'a>() -> impl Parser<'a, AppHeader<'a>> {
    move |_, _| {
        panic!("TODO parse app header");
    }
}

#[inline(always)]
pub fn module_defs<'a>() -> impl Parser<'a, Vec<'a, Located<Def<'a>>>> {
    zero_or_more!(space1_around(loc(parse::def(0)), 0))
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
        and!(skip_second!(space1(1), string("exposes")), space1(1)),
        collection!(char('['), loc!(exposes_entry()), char(','), char(']'), 1)
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
        and!(skip_second!(space1(1), string("imports")), space1(1)),
        collection!(char('['), loc!(imports_entry()), char(','), char(']'), 1)
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
                char('.'),
                collection!(char('{'), loc!(exposes_entry()), char(','), char('}'), 1)
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
