use crate::ast::{Spaced, TryTarget};
use crate::keyword::is_keyword;
use crate::parser::Progress::{self, *};
use crate::parser::{EExpr, ParseResult, Parser};
use crate::state::State;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::{Loc, Position, Region};

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
    /// lowercase identifier, e.g. x, foo, or bar in \bar
    Plain(&'a str),
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

pub fn lowercase_ident<'a>() -> impl Parser<'a, &'a str, ()> {
    move |_, state: State<'a>, _: u32| parse_lowercase_ident(state)
}

/// This could be:
///
/// * A record field, e.g. "email" in `.email` or in `email:`
/// * A named pattern match, e.g. "foo" in `foo =` or `foo ->` or `\foo ->`
#[inline(always)]
pub fn parse_lowercase_ident(state: State<'_>) -> ParseResult<'_, &str, ()> {
    match chomp_lowercase_part(state.bytes()) {
        Err(p) => Err((p, ())),
        Ok(ident) => {
            if is_keyword(ident) {
                Err((NoProgress, ()))
            } else {
                Ok((MadeProgress, ident, state.advance(ident.len())))
            }
        }
    }
}

/// This could be:
///
/// * A module name
/// * A type name
/// * A tag
pub fn uppercase<'a>() -> impl Parser<'a, Loc<Spaced<'a, UppercaseIdent<'a>>>, ()> {
    move |_, state: State<'a>, _: u32| {
        let start = state.pos();
        match chomp_uppercase_part(state.bytes()) {
            Err(p) => Err((p, ())),
            Ok(ident) => {
                let state = state.advance(ident.len());
                let ident = Loc::pos(start, state.pos(), Spaced::Item(ident.into()));
                Ok((MadeProgress, ident, state))
            }
        }
    }
}

pub fn parse_unqualified_ident(state: State<'_>) -> ParseResult<'_, &str, ()> {
    match chomp_anycase_part(state.bytes()) {
        Err(p) => Err((p, ())),
        Ok(ident) => {
            if is_keyword(ident) {
                Err((MadeProgress, ()))
            } else {
                Ok((MadeProgress, ident, state.advance(ident.len())))
            }
        }
    }
}

pub(crate) fn malformed_ident<'a>(
    initial_bytes: &'a [u8],
    problem: BadIdent,
    state: State<'a>,
) -> (Ident<'a>, State<'a>) {
    let chomped = chomp_malformed(state.bytes());
    let delta = initial_bytes.len() - state.bytes().len();
    let parsed_str = unsafe { std::str::from_utf8_unchecked(&initial_bytes[..chomped + delta]) };
    let ident = Ident::Malformed(parsed_str, problem);
    (ident, state.advance(chomped))
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

    UnderscoreAlone(Position),
    UnderscoreInMiddle(Position),
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

pub(crate) fn chomp_lowercase_part(buffer: &[u8]) -> Result<&str, Progress> {
    chomp_part(char::is_lowercase, buffer)
}

pub(crate) fn chomp_uppercase_part(buffer: &[u8]) -> Result<&str, Progress> {
    chomp_part(char::is_uppercase, buffer)
}

fn chomp_anycase_part(buffer: &[u8]) -> Result<&str, Progress> {
    chomp_part(char::is_alphabetic, buffer)
}

pub(crate) fn chomp_integer_part(buffer: &[u8]) -> Result<&str, Progress> {
    use encode_unicode::CharExt;

    let mut chomped = 0;
    if let Ok((ch, width)) = char::from_utf8_slice_start(&buffer[chomped..]) {
        if ch.is_ascii_digit() {
            chomped += width;
        } else {
            return Err(NoProgress);
        }
    }

    while let Ok((ch, width)) = char::from_utf8_slice_start(&buffer[chomped..]) {
        if ch.is_ascii_digit() {
            chomped += width;
        } else {
            if ch == '_' || ch.is_alphabetic() {
                return Err(NoProgress);
            }
            break;
        }
    }

    if chomped == 0 {
        Err(NoProgress)
    } else {
        let name = unsafe { std::str::from_utf8_unchecked(&buffer[..chomped]) };
        Ok(name)
    }
}

#[inline(always)]
fn chomp_part<F>(leading_is_good: F, buffer: &[u8]) -> Result<&str, Progress>
where
    F: Fn(char) -> bool,
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
        if ch.is_alphabetic() || ch.is_ascii_digit() {
            chomped += width;
        } else {
            if ch == '_' {
                return Err(NoProgress);
            }
            break;
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

    match chomp_uppercase_part(&buffer[1..]) {
        Ok(name) => {
            let width = 1 + name.len();
            if let Ok(('.', _)) = char::from_utf8_slice_start(&buffer[width..]) {
                Err(BadIdent::BadOpaqueRef(pos.bump_offset(width)))
            } else {
                let value = unsafe { std::str::from_utf8_unchecked(&buffer[..width]) };
                Ok(value)
            }
        }
        Err(_) => Err(BadIdent::BadOpaqueRef(pos.bump_column(1))),
    }
}

/// This is a helper function for parsing function args.
/// The rules for (-) are special-cased, and they come up in function args.
///
/// They work like this:
///
/// x - y  # "x minus y"
/// x-y    # "x minus y"
/// x- y   # "x minus y" (probably written in a rush)
/// x -y   # "call x, passing (-y)"
///
/// Since operators have higher precedence than function application,
/// any time we encounter a '-' it is unary iff it is both preceded by spaces
/// and is *not* followed by a whitespace character.

/// When we parse an ident like `foo ` it could be any of these:
///
/// 1. A standalone variable with trailing whitespace (e.g. because an operator is next)
/// 2. The beginning of a function call (e.g. `foo bar baz`)
/// 3. The beginning of a definition (e.g. `foo =`)
/// 4. The beginning of a type annotation (e.g. `foo :`)
/// 5. A reserved keyword (e.g. `if ` or `when `), meaning we should do something else.
pub fn parse_ident<'a>(arena: &'a Bump, state: State<'a>) -> ParseResult<'a, Ident<'a>, EExpr<'a>> {
    use encode_unicode::CharExt;

    let start = state.pos();
    let bytes = state.bytes();
    let first_is_uppercase;
    let mut chomped = 0;

    // chomp the first character and depending on it decide on the rest
    match char::from_utf8_slice_start(&bytes[chomped..]) {
        Ok((ch, width)) => match ch {
            '.' => {
                return match chomp_accessor(&bytes[1..], start) {
                    Ok(accessor) => {
                        let state = state.advance(1 + accessor.len());
                        Ok((MadeProgress, Ident::AccessorFunction(accessor), state))
                    }
                    Err(fail) => {
                        let (ident, state) = malformed_ident(bytes, fail, state.inc());
                        Ok((MadeProgress, ident, state))
                    }
                }
            }
            '&' => {
                return match chomp_record_updater(&bytes[1..], start) {
                    Ok(updater) => {
                        let state = state.advance(1 + updater.len());
                        Ok((MadeProgress, Ident::RecordUpdaterFunction(updater), state))
                    }
                    // return NoProgress to allow parsing &&
                    Err(_) => Err((NoProgress, EExpr::Start(start))),
                };
            }
            '@' => {
                return match chomp_opaque_ref(bytes, start) {
                    Ok(tagname) => {
                        let state = state.advance(tagname.len());
                        Ok((MadeProgress, Ident::OpaqueRef(tagname), state))
                    }
                    Err(fail) => {
                        let (ident, state) = malformed_ident(bytes, fail, state.inc());
                        Ok((MadeProgress, ident, state))
                    }
                }
            }
            c if c.is_alphabetic() => {
                // fall through
                chomped += width;
                first_is_uppercase = c.is_uppercase();
            }
            _ => return Err((NoProgress, EExpr::Start(start))),
        },
        Err(_) => return Err((NoProgress, EExpr::Start(start))),
    }

    loop {
        match char::from_utf8_slice_start(&bytes[chomped..]) {
            Ok(('.', _)) => {
                let module_name = if first_is_uppercase {
                    match chomp_module_chain(&bytes[chomped..]) {
                        Ok(width) => {
                            chomped += width;
                            unsafe { std::str::from_utf8_unchecked(&bytes[..chomped]) }
                        }
                        Err(MadeProgress) => todo!(), // todo: @wip @ask this is original todo, can I write the test to expose it?
                        Err(NoProgress) => unsafe {
                            std::str::from_utf8_unchecked(&bytes[..chomped])
                        },
                    }
                } else {
                    ""
                };

                let mut parts = Vec::with_capacity_in(4, arena);

                if !first_is_uppercase {
                    let first_part = unsafe { std::str::from_utf8_unchecked(&bytes[..chomped]) };
                    if module_name.is_empty() && is_keyword(first_part) {
                        return Err((NoProgress, EExpr::Start(start)));
                    }
                    parts.push(Accessor::RecordField(first_part));
                }

                return match chomp_access_chain(&bytes[chomped..], &mut parts) {
                    Ok(width) => {
                        if first_is_uppercase && matches!(parts[0], Accessor::TupleIndex(_)) {
                            let fail = BadIdent::QualifiedTupleAccessor(start.bump_offset(chomped));
                            let (ident, state) =
                                malformed_ident(bytes, fail, state.advance(chomped));
                            return Ok((MadeProgress, ident, state));
                        }

                        let parts = parts.into_bump_slice();
                        let ident = Ident::Access { module_name, parts };
                        Ok((MadeProgress, ident, state.advance(chomped + width)))
                    }
                    Err(width) => {
                        let fail = match width {
                            0 if !module_name.is_empty() => {
                                BadIdent::QualifiedTag(start.bump_offset(chomped))
                            }
                            1 if parts.is_empty() => {
                                BadIdent::WeirdDotQualified(start.bump_offset(chomped + 1))
                            }
                            _ => BadIdent::WeirdDotAccess(start.bump_offset(chomped + width)),
                        };
                        let (ident, state) =
                            malformed_ident(bytes, fail, state.advance(chomped + width));
                        return Ok((MadeProgress, ident, state));
                    }
                };
            }
            Ok(('_', _)) => {
                // we don't allow underscores in the middle of an identifier
                // but still parse them (and generate a malformed identifier)
                // to give good error messages for this case
                let fail = BadIdent::UnderscoreInMiddle(start.bump_offset(chomped + 1));
                let (ident, state) = malformed_ident(bytes, fail, state.advance(chomped + 1));
                return Ok((MadeProgress, ident, state));
            }
            Ok((ch, width)) if ch.is_alphabetic() || ch.is_ascii_digit() => {
                // continue the parsing loop
                chomped += width;
            }
            _ => {
                let value = unsafe { std::str::from_utf8_unchecked(&bytes[..chomped]) };
                if first_is_uppercase {
                    return Ok((MadeProgress, Ident::Tag(value), state.advance(chomped)));
                } else {
                    if is_keyword(value) {
                        return Err((NoProgress, EExpr::Start(start)));
                    }
                    return Ok((MadeProgress, Ident::Plain(value), state.advance(chomped)));
                }
            }
        }
    }
}

fn chomp_module_chain(buffer: &[u8]) -> Result<usize, Progress> {
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
        Ok(chomped)
    }
}

// parse a type name like `Result` or `Result.Result`
pub(crate) fn chomp_concrete_type(buffer: &[u8]) -> Result<(&str, &str, usize), Progress> {
    let first = crate::ident::chomp_uppercase_part(buffer)?;

    if let Some(b'.') = buffer.get(first.len()) {
        match crate::ident::chomp_module_chain(&buffer[first.len()..]) {
            Err(_) => Err(MadeProgress),
            Ok(rest) => {
                let width = first.len() + rest;

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

pub(crate) fn chomp_access_chain<'a>(
    buffer: &'a [u8],
    parts: &mut Vec<'a, Accessor<'a>>,
) -> Result<usize, usize> {
    let mut chomped = 0;
    while let Some(b'.') = buffer.get(chomped) {
        let next = chomped + 1;
        match &buffer.get(next..) {
            Some(slice) => match chomp_lowercase_part(slice) {
                Ok(name) => {
                    let value =
                        unsafe { std::str::from_utf8_unchecked(&buffer[next..next + name.len()]) };
                    parts.push(Accessor::RecordField(value));
                    chomped = next + name.len();
                }
                Err(_) => match chomp_integer_part(slice) {
                    Ok(name) => {
                        let value = unsafe {
                            std::str::from_utf8_unchecked(&buffer[next..next + name.len()])
                        };
                        parts.push(Accessor::TupleIndex(value));
                        chomped = next + name.len();
                    }
                    Err(_) => return Err(next),
                },
            },
            None => return Err(next),
        }
    }

    if chomped == 0 {
        Err(0)
    } else {
        Ok(chomped)
    }
}
