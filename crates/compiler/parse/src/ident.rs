use crate::ast::TryTarget;
use crate::keyword::is_allowed_identifier;
use crate::parser::Progress::{self, *};
use crate::parser::{BadInputError, EExpr, ParseResult, Parser};
use crate::state::State;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::{Position, Region};

/// A tag, for example. Must start with an uppercase letter
/// and then contain only letters and numbers afterwards - no dots allowed!
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct UppercaseIdent<'a>(&'a str);

impl<'a> From<&'a str> for UppercaseIdent<'a> {
    fn from(string: &'a str) -> Self {
        UppercaseIdent(string)
    }
}

impl<'a> From<UppercaseIdent<'a>> for &'a str {
    fn from(ident: UppercaseIdent<'a>) -> Self {
        ident.0
    }
}

impl<'a> From<&'a UppercaseIdent<'a>> for &'a str {
    fn from(ident: &'a UppercaseIdent<'a>) -> Self {
        ident.0
    }
}

/// The parser accepts all of these in any position where any one of them could
/// appear. This way, canonicalization can give more helpful error messages like
/// "you can't redefine this tag!" if you wrote `Foo = ...` or
/// "you can only define unqualified constants" if you wrote `Foo.bar = ...`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ident<'a> {
    /// Foo or Bar
    Tag(&'a str),
    /// @Foo or @Bar
    OpaqueRef(&'a str),
    /// foo or foo.bar or Foo.Bar.baz.qux
    Access {
        module_name: &'a str,
        parts: &'a [Accessor<'a>],
    },
    /// `.foo { foo: 42 }` or `.1 (1, 2, 3)`
    AccessorFunction(Accessor<'a>),
    /// `&foo { foo: 42 } 3`
    RecordUpdaterFunction(&'a str),
    /// .Foo or foo. or something like foo.Bar
    Malformed(&'a str, BadIdent),
}

/// This could be:
///
/// * A record field, e.g. "email" in `.email` or in `email:`
/// * A named pattern match, e.g. "foo" in `foo =` or `foo ->` or `\foo ->`
pub fn lowercase_ident<'a>() -> impl Parser<'a, &'a str, ()> {
    move |_, state: State<'a>, _min_indent: u32| match chomp_lowercase_part(state.bytes()) {
        Err(progress) => Err((progress, ())),
        Ok(ident) => {
            if !is_allowed_identifier(ident) {
                Err((NoProgress, ()))
            } else {
                let width = ident.len();
                Ok((MadeProgress, ident, state.advance(width)))
            }
        }
    }
}

/// This is a tuple accessor, e.g. "1" in `.1`
pub fn integer_ident<'a>() -> impl Parser<'a, &'a str, ()> {
    move |_, state: State<'a>, _min_indent: u32| match chomp_integer_part(state.bytes()) {
        Err(progress) => Err((progress, ())),
        Ok(ident) => {
            let width = ident.len();
            Ok((MadeProgress, ident, state.advance(width)))
        }
    }
}

/// Like `lowercase_ident`, but returns an error with MadeProgress if the
/// identifier is a keyword.
pub fn lowercase_ident_keyword_e<'a>() -> impl Parser<'a, &'a str, ()> {
    move |_, state: State<'a>, _min_indent: u32| match chomp_lowercase_part(state.bytes()) {
        Err(progress) => Err((progress, ())),
        Ok(ident) => {
            if !is_allowed_identifier(ident) {
                Err((MadeProgress, ()))
            } else {
                let width = ident.len();
                Ok((MadeProgress, ident, state.advance(width)))
            }
        }
    }
}

pub fn tag_name<'a>() -> impl Parser<'a, &'a str, ()> {
    move |arena, state: State<'a>, min_indent: u32| {
        uppercase_ident().parse(arena, state, min_indent)
    }
}

/// This could be:
///
/// * A module name
/// * A type name
/// * A tag
pub fn uppercase<'a>() -> impl Parser<'a, UppercaseIdent<'a>, ()> {
    move |_, state: State<'a>, _min_indent: u32| match chomp_uppercase_part(state.bytes()) {
        Err(progress) => Err((progress, ())),
        Ok(ident) => {
            let width = ident.len();
            Ok((MadeProgress, ident.into(), state.advance(width)))
        }
    }
}

/// This could be:
///
/// * A module name
/// * A type name
/// * A tag
pub fn uppercase_ident<'a>() -> impl Parser<'a, &'a str, ()> {
    move |_, state: State<'a>, _min_indent: u32| match chomp_uppercase_part(state.bytes()) {
        Err(progress) => Err((progress, ())),
        Ok(ident) => {
            let width = ident.len();
            Ok((MadeProgress, ident, state.advance(width)))
        }
    }
}

pub fn unqualified_ident<'a>() -> impl Parser<'a, &'a str, ()> {
    move |_, state: State<'a>, _min_indent: u32| match chomp_anycase_part(state.bytes()) {
        Err(progress) => Err((progress, ())),
        Ok(ident) => {
            if !is_allowed_identifier(ident) {
                Err((MadeProgress, ()))
            } else {
                let width = ident.len();
                Ok((MadeProgress, ident, state.advance(width)))
            }
        }
    }
}

macro_rules! advance_state {
    ($state:expr, $n:expr) => {
        Ok($state.advance($n))
    };
}

pub fn parse_ident<'a>(
    arena: &'a Bump,
    state: State<'a>,
    _min_indent: u32,
) -> ParseResult<'a, Ident<'a>, EExpr<'a>> {
    let initial = state.clone();

    match chomp_identifier_chain(arena, state.bytes(), state.pos()) {
        Ok((width, ident)) => {
            let state = advance_state!(state, width as usize)?;
            if let Ident::Access { module_name, parts } = ident {
                if module_name.is_empty() {
                    if let Some(Accessor::RecordField(ident)) = parts.first() {
                        if !is_allowed_identifier(ident) {
                            return Err((NoProgress, EExpr::Start(initial.pos())));
                        }
                    }
                }

                return Ok((MadeProgress, Ident::Access { module_name, parts }, state));
            }

            Ok((MadeProgress, ident, state))
        }
        Err((0, _)) => Err((NoProgress, EExpr::Start(state.pos()))),
        Err((width, fail)) => match fail {
            BadIdent::Start(pos) => Err((NoProgress, EExpr::Start(pos))),
            BadIdent::Space(e, pos) => Err((NoProgress, EExpr::Space(e, pos))),
            _ => malformed_identifier(
                initial.bytes(),
                fail,
                advance_state!(state, width as usize)?,
            ),
        },
    }
}

fn malformed_identifier<'a>(
    initial_bytes: &'a [u8],
    problem: BadIdent,
    mut state: State<'a>,
) -> ParseResult<'a, Ident<'a>, EExpr<'a>> {
    let chomped = chomp_malformed(state.bytes());
    let delta = initial_bytes.len() - state.bytes().len();
    let parsed_str = unsafe { std::str::from_utf8_unchecked(&initial_bytes[..chomped + delta]) };

    state = state.advance(chomped);

    Ok((MadeProgress, Ident::Malformed(parsed_str, problem), state))
}

/// skip forward to the next non-identifier character
pub fn chomp_malformed(bytes: &[u8]) -> usize {
    use encode_unicode::CharExt;
    let mut chomped = 0;
    while let Ok((ch, width)) = char::from_utf8_slice_start(&bytes[chomped..]) {
        // We can't use ch.is_alphanumeric() here because that passes for
        // things that are "numeric" but not ASCII digits, like `¾`
        if ch == '.' || ch == '_' || ch.is_alphabetic() || ch.is_ascii_digit() {
            chomped += width;
            continue;
        } else {
            break;
        }
    }

    chomped
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BadIdent {
    Start(Position),
    Space(BadInputError, Position),

    UnderscoreAlone(Position),
    UnderscoreInMiddle(Position),
    TooManyUnderscores(Position),
    UnderscoreAtStart {
        position: Position,
        /// If this variable was already declared in a pattern (e.g. \_x -> _x),
        /// then this is where it was declared.
        declaration_region: Option<Region>,
    },
    QualifiedTag(Position),
    WeirdAccessor(Position),
    WeirdDotAccess(Position),
    WeirdDotQualified(Position),
    StrayDot(Position),
    StrayAmpersand(Position),
    BadOpaqueRef(Position),
    QualifiedTupleAccessor(Position),
}

fn is_alnum(ch: char) -> bool {
    ch.is_alphabetic() || ch.is_ascii_digit()
}

fn chomp_lowercase_part(buffer: &[u8]) -> Result<&str, Progress> {
    chomp_part(
        char::is_lowercase,
        is_plausible_ident_continue,
        true,
        buffer,
    )
}

fn chomp_uppercase_part(buffer: &[u8]) -> Result<&str, Progress> {
    chomp_part(
        char::is_uppercase,
        is_plausible_ident_continue,
        false,
        buffer,
    )
}

fn chomp_anycase_part(buffer: &[u8]) -> Result<&str, Progress> {
    use encode_unicode::CharExt;

    let allow_bang =
        char::from_utf8_slice_start(buffer).map_or(false, |(leading, _)| leading.is_lowercase());

    chomp_part(
        char::is_alphabetic,
        is_plausible_ident_continue,
        allow_bang,
        buffer,
    )
}

fn chomp_integer_part(buffer: &[u8]) -> Result<&str, Progress> {
    chomp_part(
        |ch| char::is_ascii_digit(&ch),
        |ch| char::is_ascii_digit(&ch),
        false,
        buffer,
    )
}

fn is_plausible_ident_continue(ch: char) -> bool {
    ch == '_' || is_alnum(ch)
}

#[inline(always)]
fn chomp_part<F, G>(
    leading_is_good: F,
    rest_is_good: G,
    allow_bang: bool,
    buffer: &[u8],
) -> Result<&str, Progress>
where
    F: Fn(char) -> bool,
    G: Fn(char) -> bool,
{
    use encode_unicode::CharExt;

    let mut chomped = 0;

    if let Ok((ch, width)) = char::from_utf8_slice_start(&buffer[chomped..]) {
        if leading_is_good(ch) {
            chomped += width;
        } else {
            return Err(NoProgress);
        }
    }

    while let Ok((ch, width)) = char::from_utf8_slice_start(&buffer[chomped..]) {
        if rest_is_good(ch) {
            chomped += width;
        } else if allow_bang && ch == '!' {
            chomped += width;
            break;
        } else {
            // we're done
            break;
        }
    }

    if let Ok((next, _width)) = char::from_utf8_slice_start(&buffer[chomped..]) {
        // This would mean we have e.g.:
        // * identifier followed by a _
        // * an integer followed by an alphabetic char
        if is_plausible_ident_continue(next) {
            return Err(NoProgress);
        }
    }

    if chomped == 0 {
        Err(NoProgress)
    } else {
        let name = unsafe { std::str::from_utf8_unchecked(&buffer[..chomped]) };

        Ok(name)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Accessor<'a> {
    RecordField(&'a str),
    TupleIndex(&'a str),
}

impl<'a> Accessor<'a> {
    pub fn len(&self) -> usize {
        match self {
            Accessor::RecordField(name) => name.len(),
            Accessor::TupleIndex(name) => name.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() > 0
    }

    pub fn as_inner(&self) -> &'a str {
        match self {
            Accessor::RecordField(name) => name,
            Accessor::TupleIndex(name) => name,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Suffix<'a> {
    Accessor(Accessor<'a>),
    TrySuffix(TryTarget),
}

/// a `.foo` or `.1` accessor function
fn chomp_accessor(buffer: &[u8], pos: Position) -> Result<Accessor, BadIdent> {
    // assumes the leading `.` has been chomped already
    use encode_unicode::CharExt;

    match chomp_lowercase_part(buffer) {
        Ok(name) => {
            let chomped = name.len();

            if let Ok(('.', _)) = char::from_utf8_slice_start(&buffer[chomped..]) {
                Err(BadIdent::WeirdAccessor(pos))
            } else {
                Ok(Accessor::RecordField(name))
            }
        }
        Err(_) => {
            match chomp_integer_part(buffer) {
                Ok(name) => {
                    let chomped = name.len();

                    if let Ok(('.', _)) = char::from_utf8_slice_start(&buffer[chomped..]) {
                        Err(BadIdent::WeirdAccessor(pos))
                    } else {
                        Ok(Accessor::TupleIndex(name))
                    }
                }
                Err(_) => {
                    // we've already made progress with the initial `.`
                    Err(BadIdent::StrayDot(pos.bump_column(1)))
                }
            }
        }
    }
}

/// a `&foo` record updater function
fn chomp_record_updater(buffer: &[u8], pos: Position) -> Result<&str, BadIdent> {
    // assumes the leading `&` has been chomped already
    match chomp_lowercase_part(buffer) {
        Ok(name) => Ok(name),
        Err(_) => {
            // we've already made progress with the initial `&`
            Err(BadIdent::StrayAmpersand(pos.bump_column(1)))
        }
    }
}

/// a `@Token` opaque
fn chomp_opaque_ref(buffer: &[u8], pos: Position) -> Result<&str, BadIdent> {
    // assumes the leading `@` has NOT been chomped already
    debug_assert_eq!(buffer.first(), Some(&b'@'));
    use encode_unicode::CharExt;

    let bad_ident = BadIdent::BadOpaqueRef;

    match chomp_uppercase_part(&buffer[1..]) {
        Ok(name) => {
            let width = 1 + name.len();

            if let Ok(('.', _)) = char::from_utf8_slice_start(&buffer[width..]) {
                Err(bad_ident(pos.bump_column(width as u32)))
            } else {
                let value = unsafe { std::str::from_utf8_unchecked(&buffer[..width]) };
                if value.contains('_') {
                    // we don't allow underscores in the middle of a type identifier
                    // but still parse them (and generate a malformed identifier)
                    // to give good error messages for this case
                    Err(BadIdent::UnderscoreInMiddle(pos.bump_column(width as u32)))
                } else {
                    Ok(value)
                }
            }
        }
        Err(_) => Err(bad_ident(pos.bump_column(1))),
    }
}

fn chomp_identifier_chain<'a>(
    arena: &'a Bump,
    buffer: &'a [u8],
    pos: Position,
) -> Result<(u32, Ident<'a>), (u32, BadIdent)> {
    use encode_unicode::CharExt;

    let first_is_uppercase;
    let mut chomped = 0;

    match char::from_utf8_slice_start(&buffer[chomped..]) {
        Ok((ch, width)) => match ch {
            '.' => match chomp_accessor(&buffer[1..], pos) {
                Ok(accessor) => {
                    let bytes_parsed = 1 + accessor.len();
                    return Ok((bytes_parsed as u32, Ident::AccessorFunction(accessor)));
                }
                Err(fail) => return Err((1, fail)),
            },
            '&' => match chomp_record_updater(&buffer[1..], pos) {
                Ok(updater) => {
                    let bytes_parsed = 1 + updater.len();
                    return Ok((bytes_parsed as u32, Ident::RecordUpdaterFunction(updater)));
                }
                // return 0 bytes consumed on failure to allow parsing &&
                Err(fail) => return Err((0, fail)),
            },
            '@' => match chomp_opaque_ref(buffer, pos) {
                Ok(tagname) => {
                    let bytes_parsed = tagname.len();

                    let ident = Ident::OpaqueRef;

                    return Ok((bytes_parsed as u32, ident(tagname)));
                }
                Err(fail) => return Err((1, fail)),
            },
            c if c.is_alphabetic() => {
                // fall through
                chomped += width;
                first_is_uppercase = c.is_uppercase();
            }
            _ => {
                return Err((0, BadIdent::Start(pos)));
            }
        },
        Err(_) => return Err((0, BadIdent::Start(pos))),
    }

    while let Ok((ch, width)) = char::from_utf8_slice_start(&buffer[chomped..]) {
        if ch.is_alphabetic() || ch.is_ascii_digit() || ch == '_' {
            chomped += width;
        } else if ch == '!' && !first_is_uppercase {
            chomped += width;

            let value = unsafe { std::str::from_utf8_unchecked(&buffer[..chomped]) };
            let ident = Ident::Access {
                module_name: "",
                parts: arena.alloc([Accessor::RecordField(value)]),
            };

            return Ok((chomped as u32, ident));
        } else {
            // we're done
            break;
        }
    }

    if let Ok(('.', _)) = char::from_utf8_slice_start(&buffer[chomped..]) {
        let module_name = if first_is_uppercase {
            match chomp_module_chain(&buffer[chomped..]) {
                Ok(width) => {
                    chomped += width as usize;
                    unsafe { std::str::from_utf8_unchecked(&buffer[..chomped]) }
                }
                Err(MadeProgress) => todo!(),
                Err(NoProgress) => unsafe { std::str::from_utf8_unchecked(&buffer[..chomped]) },
            }
        } else {
            ""
        };

        let mut parts = Vec::with_capacity_in(4, arena);

        if !first_is_uppercase {
            let first_part = unsafe { std::str::from_utf8_unchecked(&buffer[..chomped]) };
            parts.push(Accessor::RecordField(first_part));
        }

        match chomp_access_chain(&buffer[chomped..], &mut parts) {
            Ok(width) => {
                if matches!(parts[0], Accessor::TupleIndex(_)) && first_is_uppercase {
                    return Err((
                        chomped as u32,
                        BadIdent::QualifiedTupleAccessor(pos.bump_column(chomped as u32)),
                    ));
                }

                chomped += width as usize;

                let ident = Ident::Access {
                    module_name,
                    parts: parts.into_bump_slice(),
                };

                Ok((chomped as u32, ident))
            }
            Err(0) if !module_name.is_empty() => Err((
                chomped as u32,
                BadIdent::QualifiedTag(pos.bump_column(chomped as u32)),
            )),
            Err(1) if parts.is_empty() => Err((
                chomped as u32 + 1,
                BadIdent::WeirdDotQualified(pos.bump_column(chomped as u32 + 1)),
            )),
            Err(width) => Err((
                chomped as u32 + width,
                BadIdent::WeirdDotAccess(pos.bump_column(chomped as u32 + width)),
            )),
        }
    } else if first_is_uppercase {
        // just one segment, starting with an uppercase letter; that's a tag
        let value = unsafe { std::str::from_utf8_unchecked(&buffer[..chomped]) };
        if value.contains('_') {
            // we don't allow underscores in the middle of a tag identifier
            // but still parse them (and generate a malformed identifier)
            // to give good error messages for this case
            Err((
                chomped as u32,
                BadIdent::UnderscoreInMiddle(pos.bump_column(chomped as u32)),
            ))
        } else {
            Ok((chomped as u32, Ident::Tag(value)))
        }
    } else {
        // just one segment, starting with a lowercase letter; that's a normal identifier
        let value = unsafe { std::str::from_utf8_unchecked(&buffer[..chomped]) };

        let ident = Ident::Access {
            module_name: "",
            parts: arena.alloc([Accessor::RecordField(value)]),
        };

        Ok((chomped as u32, ident))
    }
}

fn chomp_module_chain(buffer: &[u8]) -> Result<u32, Progress> {
    let mut chomped = 0;

    while let Some(b'.') = buffer.get(chomped) {
        match &buffer.get(chomped + 1..) {
            Some(slice) => match chomp_uppercase_part(slice) {
                Ok(name) => {
                    chomped += name.len() + 1;
                }
                Err(MadeProgress) => return Err(MadeProgress),
                Err(NoProgress) => break,
            },
            None => return Err(MadeProgress),
        }
    }

    if chomped == 0 {
        Err(NoProgress)
    } else {
        Ok(chomped as u32)
    }
}

pub fn concrete_type<'a>() -> impl Parser<'a, (&'a str, &'a str), ()> {
    move |_, state: State<'a>, _min_indent: u32| match chomp_concrete_type(state.bytes()) {
        Err(progress) => Err((progress, ())),
        Ok((module_name, type_name, width)) => {
            Ok((MadeProgress, (module_name, type_name), state.advance(width)))
        }
    }
}

// parse a type name like `Result` or `Result.Result`
fn chomp_concrete_type(buffer: &[u8]) -> Result<(&str, &str, usize), Progress> {
    let first = crate::ident::chomp_uppercase_part(buffer)?;

    if let Some(b'.') = buffer.get(first.len()) {
        match crate::ident::chomp_module_chain(&buffer[first.len()..]) {
            Err(_) => Err(MadeProgress),
            Ok(rest) => {
                let width = first.len() + rest as usize;

                // we must explicitly check here for a trailing `.`
                if let Some(b'.') = buffer.get(width) {
                    return Err(MadeProgress);
                }

                let slice = &buffer[..width];

                match slice.iter().rev().position(|c| *c == b'.') {
                    None => Ok(("", first, first.len())),
                    Some(rev_index) => {
                        let index = slice.len() - rev_index;
                        let module_name =
                            unsafe { std::str::from_utf8_unchecked(&slice[..index - 1]) };
                        let type_name = unsafe { std::str::from_utf8_unchecked(&slice[index..]) };

                        Ok((module_name, type_name, width))
                    }
                }
            }
        }
    } else {
        Ok(("", first, first.len()))
    }
}

fn chomp_access_chain<'a>(buffer: &'a [u8], parts: &mut Vec<'a, Accessor<'a>>) -> Result<u32, u32> {
    let mut chomped = 0;

    while let Some(b'.') = buffer.get(chomped) {
        match &buffer.get(chomped + 1..) {
            Some(slice) => match chomp_lowercase_part(slice) {
                Ok(name) => {
                    let value = unsafe {
                        std::str::from_utf8_unchecked(
                            &buffer[chomped + 1..chomped + 1 + name.len()],
                        )
                    };
                    parts.push(Accessor::RecordField(value));

                    chomped += name.len() + 1;
                }
                Err(_) => match chomp_integer_part(slice) {
                    Ok(name) => {
                        let value = unsafe {
                            std::str::from_utf8_unchecked(
                                &buffer[chomped + 1..chomped + 1 + name.len()],
                            )
                        };
                        parts.push(Accessor::TupleIndex(value));

                        chomped += name.len() + 1;
                    }
                    Err(_) => return Err(chomped as u32 + 1),
                },
            },
            None => return Err(chomped as u32 + 1),
        }
    }

    if chomped == 0 {
        Err(0)
    } else {
        Ok(chomped as u32)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_ident_parses<'a>(arena: &'a Bump, ident: &str, expected: Ident<'a>) {
        let s = State::new(ident.as_bytes());
        let (_, id, _) = parse_ident(arena, s, 0).unwrap();
        assert_eq!(id, expected);
    }

    fn assert_ident_parses_tag(arena: &Bump, ident: &str) {
        assert_ident_parses(arena, ident, Ident::Tag(ident));
    }
    fn assert_ident_parses_opaque(arena: &Bump, ident: &str) {
        assert_ident_parses(arena, ident, Ident::OpaqueRef(ident));
    }
    fn assert_ident_parses_simple_access(arena: &Bump, ident: &str) {
        assert_ident_parses(
            arena,
            ident,
            Ident::Access {
                module_name: "",
                parts: arena.alloc([Accessor::RecordField(ident)]),
            },
        );
    }

    fn assert_ident_parses_malformed(arena: &Bump, ident: &str, pos: Position) {
        assert_ident_parses(
            arena,
            ident,
            Ident::Malformed(ident, BadIdent::UnderscoreInMiddle(pos)),
        );
    }

    #[test]
    fn test_parse_ident_lowercase_camel() {
        let arena = Bump::new();
        assert_ident_parses_simple_access(&arena, "hello");
        assert_ident_parses_simple_access(&arena, "hello23");
        assert_ident_parses_simple_access(&arena, "helloWorld");
        assert_ident_parses_simple_access(&arena, "helloWorld23");
        assert_ident_parses_simple_access(&arena, "helloWorldThisIsQuiteATag");
        assert_ident_parses_simple_access(&arena, "helloWorldThisIsQuiteATag_");
        assert_ident_parses_simple_access(&arena, "helloworldthisisquiteatag_");
        assert_ident_parses_simple_access(&arena, "helloWorldThisIsQuiteATag23");
        assert_ident_parses_simple_access(&arena, "helloWorldThisIsQuiteATag23_");
        assert_ident_parses_simple_access(&arena, "helloworldthisisquiteatag23_");
    }

    #[test]
    fn test_parse_ident_lowercase_snake() {
        let arena = Bump::new();
        assert_ident_parses_simple_access(&arena, "hello_world");
        assert_ident_parses_simple_access(&arena, "hello_world23");
        assert_ident_parses_simple_access(&arena, "hello_world_this_is_quite_a_var");
        assert_ident_parses_simple_access(&arena, "hello_world_this_is_quite_a_var_");
        assert_ident_parses_simple_access(&arena, "hello_world_this_is_quite_a_var23");
        assert_ident_parses_simple_access(&arena, "hello_world_this_is_quite_a_var23_");
    }

    #[test]
    fn test_parse_tag_camel() {
        let arena = Bump::new();
        assert_ident_parses_tag(&arena, "Hello");
        assert_ident_parses_tag(&arena, "Hello23");
        assert_ident_parses_tag(&arena, "HelloWorld");
        assert_ident_parses_tag(&arena, "HelloWorld23");
        assert_ident_parses_tag(&arena, "HelloWorldThisIsQuiteATag");
        assert_ident_parses_tag(&arena, "HelloWorldThisIsQuiteATag23");
    }

    #[test]
    fn test_parse_tag_snake_is_malformed() {
        let arena = Bump::new();
        assert_ident_parses_malformed(&arena, "Hello_World", Position { offset: 11 });
        assert_ident_parses_malformed(&arena, "Hello_World23", Position { offset: 13 });
        assert_ident_parses_malformed(
            &arena,
            "Hello_World_This_Is_Quite_A_Tag",
            Position { offset: 31 },
        );
        assert_ident_parses_malformed(
            &arena,
            "Hello_World_This_Is_Quite_A_Tag23",
            Position { offset: 33 },
        );
    }

    #[test]
    fn test_parse_opaque_ref_camel() {
        let arena = Bump::new();
        assert_ident_parses_opaque(&arena, "@Hello");
        assert_ident_parses_opaque(&arena, "@Hello23");
        assert_ident_parses_opaque(&arena, "@HelloWorld");
        assert_ident_parses_opaque(&arena, "@HelloWorld23");
        assert_ident_parses_opaque(&arena, "@HelloWorldThisIsQuiteARef");
        assert_ident_parses_opaque(&arena, "@HelloWorldThisIsQuiteARef23");
    }

    #[test]
    fn test_parse_opaque_ref_snake_is_malformed() {
        let arena = Bump::new();
        assert_ident_parses_malformed(&arena, "@Hello_World", Position { offset: 12 });
        assert_ident_parses_malformed(&arena, "@Hello_World23", Position { offset: 14 });
        assert_ident_parses_malformed(
            &arena,
            "@Hello_World_This_Is_Quite_A_Ref",
            Position { offset: 32 },
        );
        assert_ident_parses_malformed(
            &arena,
            "@Hello_World_This_Is_Quite_A_Ref23",
            Position { offset: 34 },
        );
    }
}
