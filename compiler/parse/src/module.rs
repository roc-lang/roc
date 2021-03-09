use crate::ast::{Attempting, CommentOrNewline, Def, Module};
use crate::blankspace::{space0, space0_around, space0_before, space0_before_e, space0_e, space1};
use crate::expr::def;
use crate::header::{
    package_entry, package_or_path, AppHeader, Effects, ExposesEntry, ImportsEntry,
    InterfaceHeader, ModuleName, PackageEntry, PackageName, PlatformHeader, To, TypedIdent,
};
use crate::ident::{lowercase_ident, unqualified_ident, uppercase_ident};
use crate::parser::Progress::{self, *};
use crate::parser::{
    self, ascii_char, ascii_string, backtrackable, end_of_file, loc, optional, peek_utf8_char,
    peek_utf8_char_at, specialize, unexpected, unexpected_eof, word1, Col, EExposes, EHeader,
    EImports, EProvides, ERequires, ETypedIdent, ParseResult, Parser, Row, State, SyntaxError,
};
use crate::string_literal;
use crate::type_annotation;
use bumpalo::collections::{String, Vec};
use bumpalo::Bump;
use roc_region::all::Located;

pub fn header<'a>() -> impl Parser<'a, Module<'a>, SyntaxError<'a>> {
    one_of![
        map!(
            skip_first!(debug!(ascii_string("app")), app_header()),
            |header| { Module::App { header } }
        ),
        map!(
            skip_first!(ascii_string("platform"), platform_header()),
            |header| { Module::Platform { header } }
        ),
        map!(
            skip_first!(debug!(ascii_string("interface")), interface_header()),
            |header| { Module::Interface { header } }
        )
    ]
}

#[inline(always)]
fn interface_header<'a>() -> impl Parser<'a, InterfaceHeader<'a>, SyntaxError<'a>> {
    specialize(|e, _, _| SyntaxError::Header(e), interface_header_help())
}

#[inline(always)]
fn interface_header_help<'a>() -> impl Parser<'a, InterfaceHeader<'a>, EHeader<'a>> {
    |arena, state| {
        let min_indent = 1;

        let (_, after_interface_keyword, state) =
            space0_e(min_indent, EHeader::Space, EHeader::IndentStart).parse(arena, state)?;
        let (_, name, state) = loc!(module_name_help(EHeader::ModuleName)).parse(arena, state)?;

        let (_, ((before_exposes, after_exposes), exposes), state) =
            specialize(EHeader::Exposes, exposes_values_help()).parse(arena, state)?;
        let (_, ((before_imports, after_imports), imports), state) =
            specialize(EHeader::Imports, imports_help()).parse(arena, state)?;

        let header = InterfaceHeader {
            name,
            exposes,
            imports,
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
pub fn package_name<'a>() -> impl Parser<'a, PackageName<'a>, SyntaxError<'a>> {
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

fn parse_package_part<'a>(
    arena: &'a Bump,
    mut state: State<'a>,
) -> ParseResult<'a, &'a str, SyntaxError<'a>> {
    use encode_unicode::CharExt;

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
                return state.fail(arena, progress, reason);
            }
        }
    }

    Err(unexpected_eof(arena, state, 0))
}

#[inline(always)]
pub fn module_name<'a>() -> impl Parser<'a, ModuleName<'a>, SyntaxError<'a>> {
    move |arena, mut state: State<'a>| {
        match peek_utf8_char(&state) {
            Ok((first_letter, bytes_parsed)) => {
                if !first_letter.is_uppercase() {
                    return Err(unexpected(0, Attempting::Module, state));
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
                                    Err(reason) => return state.fail(arena, MadeProgress, reason),
                                }
                            } else {
                                // This is the end of the module name. We're done!
                                break;
                            }
                        }
                        Err(reason) => return state.fail(arena, MadeProgress, reason),
                    }
                }

                Ok((MadeProgress, ModuleName::new(buf.into_bump_str()), state))
            }
            Err(reason) => state.fail(arena, MadeProgress, reason),
        }
    }
}

#[inline(always)]
fn app_header<'a>() -> impl Parser<'a, AppHeader<'a>, SyntaxError<'a>> {
    |arena, state| {
        let (_, after_app_keyword, state) = space1(1).parse(arena, state)?;
        let (_, name, state) = loc!(crate::parser::specialize(
            |e, r, c| SyntaxError::Expr(crate::parser::EExpr::Str(e, r, c)),
            string_literal::parse()
        ))
        .parse(arena, state)?;

        let (_, opt_pkgs, state) = maybe!(packages()).parse(arena, state)?;
        let (_, opt_imports, state) = maybe!(imports()).parse(arena, state)?;
        let (_, provides, state) = provides_to().parse(arena, state)?;

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

        let header = AppHeader {
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
        };

        Ok((MadeProgress, header, state))
    }
}

#[inline(always)]
fn platform_header<'a>() -> impl Parser<'a, PlatformHeader<'a>, SyntaxError<'a>> {
    |arena, state| {
        let (_, after_platform_keyword, state) = space1(1).parse(arena, state)?;
        let (_, name, state) = loc!(package_name()).parse(arena, state)?;

        let (_, ((before_requires, after_requires), requires), state) =
            requires().parse(arena, state)?;

        let (_, ((before_exposes, after_exposes), exposes), state) =
            exposes_modules().parse(arena, state)?;

        let (_, packages, state) = packages().parse(arena, state)?;

        let (_, ((before_imports, after_imports), imports), state) =
            imports().parse(arena, state)?;

        let (_, ((before_provides, after_provides), provides), state) =
            provides_without_to().parse(arena, state)?;

        let (_, effects, state) = effects().parse(arena, state)?;

        let header = PlatformHeader {
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
        };

        Ok((MadeProgress, header, state))
    }
}

#[inline(always)]
pub fn module_defs<'a>() -> impl Parser<'a, Vec<'a, Located<Def<'a>>>, SyntaxError<'a>> {
    // force that we pare until the end of the input
    skip_second!(zero_or_more!(space0_around(loc(def(0)), 0)), end_of_file())
}
#[derive(Debug)]
struct ProvidesTo<'a> {
    entries: Vec<'a, Located<ExposesEntry<'a, &'a str>>>,
    to: Located<To<'a>>,

    before_provides_keyword: &'a [CommentOrNewline<'a>],
    after_provides_keyword: &'a [CommentOrNewline<'a>],
    before_to_keyword: &'a [CommentOrNewline<'a>],
    after_to_keyword: &'a [CommentOrNewline<'a>],
}

#[inline(always)]
fn provides_to<'a>() -> impl Parser<'a, ProvidesTo<'a>, SyntaxError<'a>> {
    specialize(
        |e, r, c| SyntaxError::Header(EHeader::Provides(e, r, c)),
        provides_to_help(),
    )
}

fn provides_to_package<'a>() -> impl Parser<'a, To<'a>, SyntaxError<'a>> {
    one_of![
        map!(lowercase_ident(), To::ExistingPackage),
        map!(package_or_path(), To::NewPackage)
    ]
}

#[inline(always)]
fn provides_to_help<'a>() -> impl Parser<'a, ProvidesTo<'a>, EProvides> {
    let min_indent = 1;

    map!(
        and!(
            provides_without_to_help(),
            and!(
                spaces_around_keyword(
                    min_indent,
                    "to",
                    EProvides::To,
                    EProvides::Space,
                    EProvides::IndentTo,
                    EProvides::IndentListStart
                ),
                loc!(specialize(
                    |_, r, c| EProvides::Package(r, c),
                    provides_to_package()
                ))
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
        Vec<'a, Located<ExposesEntry<'a, &'a str>>>,
    ),
    SyntaxError<'a>,
> {
    specialize(
        |e, r, c| SyntaxError::Header(EHeader::Provides(e, r, c)),
        provides_without_to_help(),
    )
}

#[inline(always)]
fn provides_without_to_help<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Vec<'a, Located<ExposesEntry<'a, &'a str>>>,
    ),
    EProvides,
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
        collection_e!(
            word1(b'[', EProvides::ListStart),
            exposes_entry(EProvides::Identifier),
            word1(b',', EProvides::ListEnd),
            word1(b']', EProvides::ListEnd),
            min_indent,
            EProvides::Space,
            EProvides::IndentListEnd
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
        Vec<'a, Located<TypedIdent<'a>>>,
    ),
    SyntaxError<'a>,
> {
    specialize(
        |e, r, c| SyntaxError::Header(EHeader::Requires(e, r, c)),
        requires_help(),
    )
}

#[inline(always)]
fn requires_help<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Vec<'a, Located<TypedIdent<'a>>>,
    ),
    ERequires<'a>,
> {
    let min_indent = 1;
    and!(
        spaces_around_keyword(
            min_indent,
            "requires",
            ERequires::Requires,
            ERequires::Space,
            ERequires::IndentRequires,
            ERequires::IndentListStart
        ),
        collection_e!(
            word1(b'{', ERequires::ListStart),
            specialize(ERequires::TypedIdent, loc!(typed_ident_help())),
            word1(b',', ERequires::ListEnd),
            word1(b'}', ERequires::ListEnd),
            min_indent,
            ERequires::Space,
            ERequires::IndentListEnd
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
    SyntaxError<'a>,
> {
    specialize(
        |e, r, c| SyntaxError::Header(EHeader::Exposes(e, r, c)),
        exposes_values_help(),
    )
}

#[inline(always)]
fn exposes_values_help<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Vec<'a, Located<ExposesEntry<'a, &'a str>>>,
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
        collection_e!(
            word1(b'[', EExposes::ListStart),
            exposes_entry(EExposes::Identifier),
            word1(b',', EExposes::ListEnd),
            word1(b']', EExposes::ListEnd),
            min_indent,
            EExposes::Space,
            EExposes::IndentListEnd
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
    SyntaxError<'a>,
> {
    specialize(
        |e, r, c| SyntaxError::Header(EHeader::Exposes(e, r, c)),
        exposes_modules_help(),
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
fn exposes_modules_help<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Vec<'a, Located<ExposesEntry<'a, ModuleName<'a>>>>,
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
        collection_e!(
            word1(b'[', EExposes::ListStart),
            exposes_module(EExposes::Identifier),
            word1(b',', EExposes::ListEnd),
            word1(b']', EExposes::ListEnd),
            min_indent,
            EExposes::Space,
            EExposes::IndentListEnd
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
    entries: Vec<'a, Located<PackageEntry<'a>>>,

    before_packages_keyword: &'a [CommentOrNewline<'a>],
    after_packages_keyword: &'a [CommentOrNewline<'a>],
}

#[inline(always)]
fn packages<'a>() -> impl Parser<'a, Packages<'a>, SyntaxError<'a>> {
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
    SyntaxError<'a>,
> {
    specialize(
        |e, r, c| SyntaxError::Header(EHeader::Imports(e, r, c)),
        imports_help(),
    )
}

#[inline(always)]
fn imports_help<'a>() -> impl Parser<
    'a,
    (
        (&'a [CommentOrNewline<'a>], &'a [CommentOrNewline<'a>]),
        Vec<'a, Located<ImportsEntry<'a>>>,
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
        collection_e!(
            word1(b'[', EImports::ListStart),
            loc!(imports_entry()),
            word1(b',', EImports::ListEnd),
            word1(b']', EImports::ListEnd),
            min_indent,
            EImports::Space,
            EImports::IndentListEnd
        )
    )
}

#[inline(always)]
fn effects<'a>() -> impl Parser<'a, Effects<'a>, SyntaxError<'a>> {
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
fn typed_ident<'a>() -> impl Parser<'a, TypedIdent<'a>, SyntaxError<'a>> {
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

fn typed_ident_help<'a>() -> impl Parser<'a, TypedIdent<'a>, ETypedIdent<'a>> {
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
    specialize(|_, r, c| EImports::Shortname(r, c), lowercase_ident())
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
                collection_e!(
                    word1(b'{', EImports::SetStart),
                    exposes_entry(EImports::Identifier),
                    word1(b',', EImports::SetEnd),
                    word1(b'}', EImports::SetEnd),
                    min_indent,
                    EImports::Space,
                    EImports::IndentSetEnd
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
