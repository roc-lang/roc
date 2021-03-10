use crate::ast::Attempting;
use crate::keyword;
use crate::parser::Progress::{self, *};
use crate::parser::{
    peek_utf8_char, unexpected, BadInputError, Col, EExpr, ParseResult, Parser, Row, State,
    SyntaxError,
};
use bumpalo::collections::string::String;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::Region;

/// The parser accepts all of these in any position where any one of them could
/// appear. This way, canonicalization can give more helpful error messages like
/// "you can't redefine this tag!" if you wrote `Foo = ...` or
/// "you can only define unqualified constants" if you wrote `Foo.bar = ...`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ident<'a> {
    /// Foo or Bar
    GlobalTag(&'a str),
    /// @Foo or @Bar
    PrivateTag(&'a str),
    /// foo or foo.bar or Foo.Bar.baz.qux
    Access {
        module_name: &'a str,
        parts: &'a [&'a str],
    },
    /// .foo
    AccessorFunction(&'a str),
    /// .Foo or foo. or something like foo.Bar
    Malformed(&'a str, BadIdent),
}

impl<'a> Ident<'a> {
    pub fn len(&self) -> usize {
        use self::Ident::*;

        match self {
            GlobalTag(string) | PrivateTag(string) => string.len(),
            Access { module_name, parts } => {
                let mut len = if module_name.is_empty() {
                    0
                } else {
                    module_name.len() + 1
                    // +1 for the dot
                };

                for part in parts.iter() {
                    len += part.len() + 1 // +1 for the dot
                }

                len - 1
            }
            AccessorFunction(string) => string.len(),
            Malformed(string, _) => string.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

fn chomp_identifier<'a, F>(pred: F, buffer: &[u8]) -> Result<&str, Progress>
where
    F: Fn(char) -> bool,
{
    use encode_unicode::CharExt;

    let mut chomped = 0;

    match char::from_utf8_slice_start(&buffer[chomped..]) {
        Ok((ch, width)) if pred(ch) => {
            chomped += width;
        }
        _ => {
            // no parse
            return Err(Progress::NoProgress);
        }
    }

    while let Ok((ch, width)) = char::from_utf8_slice_start(&buffer[chomped..]) {
        // After the first character, only these are allowed:
        //
        // * Unicode alphabetic chars - you might include `鹏` if that's clear to your readers
        // * ASCII digits - e.g. `1` but not `¾`, both of which pass .is_numeric()
        // * A ':' indicating the end of the field
        if ch.is_alphabetic() || ch.is_ascii_digit() {
            chomped += width;
        } else {
            // we're done
            break;
        }
    }

    let name = unsafe { std::str::from_utf8_unchecked(&buffer[..chomped]) };

    Ok(name)
}

fn global_tag_or_ident<'a, F>(pred: F) -> impl Parser<'a, &'a str, SyntaxError<'a>>
where
    F: Fn(char) -> bool,
{
    move |arena, mut state: State<'a>| {
        // pred will determine if this is a tag or ident (based on capitalization)
        let (first_letter, bytes_parsed) = match peek_utf8_char(&state) {
            Ok((first_letter, bytes_parsed)) => {
                if !pred(first_letter) {
                    return Err(unexpected(0, Attempting::RecordFieldLabel, state));
                }

                (first_letter, bytes_parsed)
            }
            Err(reason) => return state.fail(arena, NoProgress, reason),
        };

        let mut buf = String::with_capacity_in(1, arena);

        buf.push(first_letter);

        state = state.advance_without_indenting(bytes_parsed)?;

        while !state.bytes.is_empty() {
            match peek_utf8_char(&state) {
                Ok((ch, bytes_parsed)) => {
                    // After the first character, only these are allowed:
                    //
                    // * Unicode alphabetic chars - you might include `鹏` if that's clear to your readers
                    // * ASCII digits - e.g. `1` but not `¾`, both of which pass .is_numeric()
                    // * A ':' indicating the end of the field
                    if ch.is_alphabetic() || ch.is_ascii_digit() {
                        buf.push(ch);

                        state = state.advance_without_indenting(bytes_parsed)?;
                    } else {
                        // This is the end of the field. We're done!
                        break;
                    }
                }
                Err(reason) => return state.fail(arena, MadeProgress, reason),
            };
        }

        Ok((MadeProgress, buf.into_bump_str(), state))
    }
}

/// This could be:
///
/// * A record field, e.g. "email" in `.email` or in `email:`
/// * A named pattern match, e.g. "foo" in `foo =` or `foo ->` or `\foo ->`
pub fn lowercase_ident<'a>() -> impl Parser<'a, &'a str, SyntaxError<'a>> {
    move |arena, state: State<'a>| {
        let (progress, ident, state) =
            global_tag_or_ident(|first_char| first_char.is_lowercase()).parse(arena, state)?;

        // to parse a valid ident, progress must be made
        debug_assert_eq!(progress, MadeProgress);

        if (ident == keyword::IF)
            || (ident == keyword::THEN)
            || (ident == keyword::ELSE)
            || (ident == keyword::WHEN)
            || (ident == keyword::IS)
            || (ident == keyword::AS)
        {
            // TODO Calculate the correct region based on state
            let region = Region::zero();
            Err((MadeProgress, SyntaxError::ReservedKeyword(region), state))
        } else {
            Ok((MadeProgress, ident, state))
        }
    }
}

/// This could be:
///
/// * A module name
/// * A type name
/// * A global tag
pub fn uppercase_ident<'a>() -> impl Parser<'a, &'a str, SyntaxError<'a>> {
    global_tag_or_ident(|first_char| first_char.is_uppercase())
}

pub fn unqualified_ident<'a>() -> impl Parser<'a, &'a str, SyntaxError<'a>> {
    global_tag_or_ident(|first_char| first_char.is_alphabetic())
}

pub fn join_module_parts<'a>(arena: &'a Bump, module_parts: &[&str]) -> &'a str {
    let capacity = module_parts.len() * 3; // Module parts tend to be 3+ characters.
    let mut buf = String::with_capacity_in(capacity, arena);
    let mut any_parts_added = false;

    for part in module_parts {
        if any_parts_added {
            buf.push('.');
        } else {
            any_parts_added = true;
        }

        buf.push_str(part);
    }

    buf.into_bump_str()
}

macro_rules! advance_state {
    ($state:expr, $n:expr) => {
        $state.advance_without_indenting_ee($n, |r, c| {
            BadIdent::Space(crate::parser::BadInputError::LineTooLong, r, c)
        })
    };
}

pub fn parse_ident_help<'a>(
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Ident<'a>, EExpr<'a>> {
    let initial = state.clone();

    match parse_ident_help_help(arena, state) {
        Ok((progress, ident, state)) => {
            if let Ident::Access { module_name, parts } = ident {
                if module_name.is_empty() {
                    if let Some(first) = parts.first() {
                        for keyword in crate::keyword::KEYWORDS.iter() {
                            if first == keyword {
                                return Err((
                                    NoProgress,
                                    EExpr::Start(initial.line, initial.column),
                                    initial,
                                ));
                            }
                        }
                    }
                }
            }

            Ok((progress, ident, state))
        }
        Err((NoProgress, _, state)) => {
            Err((NoProgress, EExpr::Start(state.line, state.column), state))
        }
        Err((MadeProgress, fail, state)) => match fail {
            BadIdent::Start(r, c) => Err((NoProgress, EExpr::Start(r, c), state)),
            BadIdent::Space(e, r, c) => Err((NoProgress, EExpr::Space(e, r, c), state)),
            _ => malformed_identifier(initial.bytes, fail, arena, state),
        },
    }
}

fn malformed_identifier<'a>(
    initial_bytes: &'a [u8],
    problem: BadIdent,
    _arena: &'a Bump,
    mut state: State<'a>,
) -> ParseResult<'a, Ident<'a>, EExpr<'a>> {
    // skip forward to the next non-identifier character
    while !state.bytes.is_empty() {
        match peek_utf8_char(&state) {
            Ok((ch, bytes_parsed)) => {
                // We can't use ch.is_alphanumeric() here because that passes for
                // things that are "numeric" but not ASCII digits, like `¾`
                if ch == '.' || ch == '_' || ch.is_alphabetic() || ch.is_ascii_digit() {
                    state = state.advance_without_indenting_ee(bytes_parsed, |r, c| {
                        EExpr::Space(crate::parser::BadInputError::LineTooLong, r, c)
                    })?;
                    continue;
                } else {
                    break;
                }
            }
            Err(_reason) => {
                break;
            }
        }
    }

    let parsed = &initial_bytes[..(initial_bytes.len() - state.bytes.len())];

    let parsed_str = unsafe { std::str::from_utf8_unchecked(parsed) };

    Ok((MadeProgress, Ident::Malformed(parsed_str, problem), state))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BadIdent {
    Start(Row, Col),
    Space(BadInputError, Row, Col),
    Underscore(Row, Col),
    QualifiedTag(Row, Col),
    PrivateTagNotUppercase(Row, Col),
    PartStartsWithNumber(Row, Col),
    WeirdAccessor(Row, Col),
    PrivateTagFieldAccess(Row, Col),

    WeirdDotAccess(Row, Col),
    WeirdDotQualified(Row, Col),
    DoubleDot(Row, Col),
    StrayDot(Row, Col),
    StrayAt(Row, Col),
    BadPrivateTag(Row, Col),
}

fn chomp_lowercase_part(buffer: &[u8]) -> Result<&str, Progress> {
    chomp_part(|c: char| c.is_lowercase(), buffer)
}

fn chomp_uppercase_part(buffer: &[u8]) -> Result<&str, Progress> {
    chomp_part(|c: char| c.is_uppercase(), buffer)
}

#[inline(always)]
fn chomp_part<F>(leading_is_good: F, buffer: &[u8]) -> Result<&str, Progress>
where
    F: Fn(char) -> bool,
{
    // assumes the leading `.` has been chomped already
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
            // we're done
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

/// a `.foo` accessor function
fn chomp_accessor(buffer: &[u8], row: Row, col: Col) -> Result<&str, BadIdent> {
    // assumes the leading `.` has been chomped already
    use encode_unicode::CharExt;

    match chomp_lowercase_part(buffer) {
        Ok(name) => {
            let chomped = name.len();

            if let Ok(('.', _)) = char::from_utf8_slice_start(&buffer[chomped..]) {
                Err(BadIdent::WeirdAccessor(row, col))
            } else {
                Ok(name)
            }
        }
        Err(_) => {
            // we've already made progress with the initial `.`
            Err(BadIdent::StrayDot(row, col + 1))
        }
    }
}

/// a `@Token` private tag
fn chomp_private_tag(buffer: &[u8], row: Row, col: Col) -> Result<&str, BadIdent> {
    // assumes the leading `@` has NOT been chomped already
    debug_assert_eq!(buffer.get(0), Some(&b'@'));
    use encode_unicode::CharExt;

    match chomp_uppercase_part(&buffer[1..]) {
        Ok(name) => {
            let chomped = 1 + name.len();

            if let Ok(('.', _)) = char::from_utf8_slice_start(&buffer[chomped..]) {
                Err(BadIdent::BadPrivateTag(row, col + chomped as u16))
            } else {
                Ok(name)
            }
        }
        Err(_) => Err(BadIdent::BadPrivateTag(row, col + 1)),
    }
}

fn chomp_identifier_chain<'a>(
    arena: &'a Bump,
    buffer: &'a [u8],
    row: Row,
    col: Col,
) -> Result<(u16, Ident<'a>), (u16, BadIdent)> {
    use encode_unicode::CharExt;

    let first_is_uppercase;
    let mut chomped = 0;

    match char::from_utf8_slice_start(&buffer[chomped..]) {
        Ok((ch, width)) => match ch {
            '.' => match chomp_accessor(&buffer[1..], row, col) {
                Ok(accessor) => {
                    let bytes_parsed = 1 + accessor.len();

                    return Ok((bytes_parsed as u16, Ident::AccessorFunction(accessor)));
                }
                Err(fail) => return Err((1, fail)),
            },
            '@' => match chomp_private_tag(buffer, row, col) {
                Ok(tagname) => {
                    let bytes_parsed = tagname.len();

                    return Ok((bytes_parsed as u16, Ident::PrivateTag(tagname)));
                }
                Err(fail) => return Err((1, fail)),
            },
            c if c.is_alphabetic() => {
                // fall through
                chomped += width;
                first_is_uppercase = c.is_uppercase();
            }
            _ => {
                return Err((0, BadIdent::Start(row, col)));
            }
        },
        Err(_) => return Err((0, BadIdent::Start(row, col))),
    }

    while let Ok((ch, width)) = char::from_utf8_slice_start(&buffer[chomped..]) {
        if ch.is_alphabetic() || ch.is_ascii_digit() {
            chomped += width;
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
            parts.push(first_part);
        }

        match chomp_access_chain(&buffer[chomped..], &mut parts) {
            Ok(width) => {
                chomped += width as usize;

                let ident = Ident::Access {
                    module_name,
                    parts: parts.into_bump_slice(),
                };

                Ok((chomped as u16, ident))
            }
            Err(0) if !module_name.is_empty() => Err((
                chomped as u16,
                BadIdent::QualifiedTag(row, chomped as u16 + col),
            )),
            Err(1) if parts.is_empty() => Err((
                chomped as u16 + 1,
                BadIdent::WeirdDotQualified(row, chomped as u16 + col + 1),
            )),
            Err(width) => Err((
                chomped as u16 + width,
                BadIdent::WeirdDotAccess(row, chomped as u16 + col + width),
            )),
        }
    } else if let Ok(('_', _)) = char::from_utf8_slice_start(&buffer[chomped..]) {
        // we don't allow underscores in the middle of an identifier
        // but still parse them (and generate a malformed identifier)
        // to give good error messages for this case
        Err((
            chomped as u16 + 1,
            BadIdent::Underscore(row, col + chomped as u16 + 1),
        ))
    } else if first_is_uppercase {
        // just one segment, starting with an uppercase letter; that's a global tag
        let value = unsafe { std::str::from_utf8_unchecked(&buffer[..chomped]) };
        Ok((chomped as u16, Ident::GlobalTag(value)))
    } else {
        // just one segment, starting with a lowercase letter; that's a normal identifier
        let value = unsafe { std::str::from_utf8_unchecked(&buffer[..chomped]) };
        let ident = Ident::Access {
            module_name: "",
            parts: arena.alloc([value]),
        };
        Ok((chomped as u16, ident))
    }
}

fn chomp_module_chain<'a>(buffer: &'a [u8]) -> Result<u16, Progress> {
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
        Ok(chomped as u16)
    }
}

fn chomp_access_chain<'a>(buffer: &'a [u8], parts: &mut Vec<'a, &'a str>) -> Result<u16, u16> {
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
                    parts.push(value);

                    chomped += name.len() + 1;
                }
                Err(_) => return Err(chomped as u16 + 1),
            },
            None => return Err(chomped as u16 + 1),
        }
    }

    dbg!(&parts);

    let value = unsafe { std::str::from_utf8_unchecked(&buffer[chomped..]) };

    dbg!(value);

    if chomped == 0 {
        Err(0)
    } else {
        Ok(chomped as u16)
    }
}

fn parse_ident_help_help<'a>(
    arena: &'a Bump,
    mut state: State<'a>,
) -> ParseResult<'a, Ident<'a>, BadIdent> {
    match chomp_identifier_chain(arena, state.bytes, state.line, state.column) {
        Ok((width, ident)) => {
            state = advance_state!(state, width as usize)?;
            return Ok((MadeProgress, ident, state));
        }
        Err((0, fail)) => {
            return Err((NoProgress, fail, state));
        }
        Err((width, fail)) => {
            state = advance_state!(state, width as usize)?;
            return Err((MadeProgress, fail, state));
        }
    }

    let mut part_buf = String::new_in(arena); // The current "part" (parts are dot-separated.)
    let mut noncapitalized_parts: Vec<&'a str> = Vec::new_in(arena);
    let mut is_capitalized;
    let is_accessor_fn;

    let bytes = state.bytes;
    let mut chomped_capitalized = 0;
    let mut cparts = 0;

    let mut chomped_part_buf = 0;
    let mut chomped = 0;

    // Identifiers and accessor functions must start with either a letter or a dot.
    // If this starts with neither, it must be something else!
    match peek_utf8_char(&state) {
        Ok((first_ch, bytes_parsed)) => {
            if first_ch.is_alphabetic() {
                part_buf.push(first_ch);
                chomped_part_buf += bytes_parsed;

                is_capitalized = first_ch.is_uppercase();
                is_accessor_fn = false;

                state = advance_state!(state, bytes_parsed)?;
            } else if first_ch == '.' {
                match chomp_accessor(&state.bytes[1..], state.line, state.column) {
                    Ok(accessor) => {
                        let bytes_parsed = 1 + accessor.len();

                        state = advance_state!(state, bytes_parsed)?;

                        return Ok((MadeProgress, Ident::AccessorFunction(accessor), state));
                    }
                    Err(fail) => return Err((MadeProgress, fail, state)),
                }
            } else if first_ch == '@' {
                match chomp_private_tag(state.bytes, state.line, state.column) {
                    Ok(tagname) => {
                        let bytes_parsed = 1 + tagname.len();

                        state = advance_state!(state, bytes_parsed)?;

                        return Ok((MadeProgress, Ident::PrivateTag(tagname), state));
                    }
                    Err(fail) => {
                        state = advance_state!(state, 1)?;
                        return Err((MadeProgress, fail, state));
                    }
                }
            } else {
                return Err((NoProgress, BadIdent::Start(state.line, state.column), state));
            }
        }
        Err(_reason) => {
            return Err((NoProgress, BadIdent::Start(state.line, state.column), state));
        }
    }

    while !state.bytes.is_empty() {
        match peek_utf8_char(&state) {
            Ok((ch, width)) => {
                // After the first character, only these are allowed:
                //
                // * Unicode alphabetic chars - you might name a variable `鹏` if that's clear to your readers
                // * ASCII digits - e.g. `1` but not `¾`, both of which pass .is_numeric()
                // * A dot ('.')
                if ch.is_alphabetic() {
                    if part_buf.is_empty() {
                        // Capitalization is determined by the first character in the part.
                        is_capitalized = ch.is_uppercase();
                    }

                    part_buf.push(ch);
                    chomped_part_buf += width;
                } else if ch.is_ascii_digit() {
                    // Parts may not start with numbers!
                    if part_buf.is_empty() {
                        return Err((
                            MadeProgress,
                            BadIdent::PartStartsWithNumber(state.line, state.column),
                            state,
                        ));
                    }

                    part_buf.push(ch);
                    chomped_part_buf += width;
                } else if ch == '.' {
                    // There are two posssible errors here:
                    //
                    // 1. Having two consecutive dots is an error.
                    // 2. Having capitalized parts after noncapitalized (e.g. `foo.Bar`) is an error.
                    if part_buf.is_empty() {
                        return Err((
                            MadeProgress,
                            BadIdent::DoubleDot(state.line, state.column),
                            state,
                        ));
                    }

                    if is_capitalized && !noncapitalized_parts.is_empty() {
                        return Err((
                            MadeProgress,
                            BadIdent::WeirdDotQualified(state.line, state.column),
                            state,
                        ));
                    }

                    if is_capitalized {
                        chomped_capitalized +=
                            chomped_part_buf + (chomped_capitalized != 0) as usize;
                        cparts += 1;
                    } else {
                        let value = unsafe {
                            std::str::from_utf8_unchecked(
                                &bytes[chomped..chomped + chomped_part_buf],
                            )
                        };
                        noncapitalized_parts.push(value);
                    }

                    // Now that we've recorded the contents of the current buffer, reset it.
                    part_buf = String::new_in(arena);
                    chomped += chomped_part_buf + 1;
                    chomped_part_buf = 0;
                } else if ch == '_' {
                    // we don't allow underscores in the middle of an identifier
                    // but still parse them (and generate a malformed identifier)
                    // to give good error messages for this case
                    state = advance_state!(state, width)?;
                    return Err((
                        MadeProgress,
                        BadIdent::Underscore(state.line, state.column),
                        state,
                    ));
                } else {
                    // This must be the end of the identifier. We're done!

                    break;
                }

                state = advance_state!(state, width)?;
            }
            Err(_reason) => {
                //
                return Err((
                    MadeProgress,
                    BadIdent::Start(state.line, state.column),
                    state,
                ));
            }
        }
    }

    if chomped_part_buf == 0 {
        // We probably had a trailing dot, e.g. `Foo.bar.` - this is malformed!
        //
        // This condition might also occur if we encounter a malformed accessor like `.|`
        //
        // If we made it this far and don't have a next_char, then necessarily
        // we have consumed a '.' char previously.
        let fail = if noncapitalized_parts.is_empty() {
            if cparts == 0 {
                BadIdent::StrayDot(state.line, state.column)
            } else {
                BadIdent::WeirdDotQualified(state.line, state.column)
            }
        } else {
            BadIdent::WeirdDotAccess(state.line, state.column)
        };

        return Err((MadeProgress, fail, state));
    }

    // Record the final parts.
    if is_capitalized {
        chomped_capitalized += chomped_part_buf + (chomped_capitalized != 0) as usize;
        cparts += 1;
    } else {
        let value =
            unsafe { std::str::from_utf8_unchecked(&bytes[chomped..chomped + chomped_part_buf]) };
        noncapitalized_parts.push(value);
    }

    let answer = if is_accessor_fn {
        // Handle accessor functions first because they have the strictest requirements.
        // Accessor functions may have exactly 1 noncapitalized part, and no capitalzed parts.
        if cparts == 0 && noncapitalized_parts.len() == 1 {
            // an accessor starts with a `.`, but we drop that from the name
            let value = unsafe {
                std::str::from_utf8_unchecked(&bytes[1 + chomped..1 + chomped + chomped_part_buf])
            };

            Ident::AccessorFunction(value)
        } else {
            return Err((
                MadeProgress,
                BadIdent::WeirdAccessor(state.line, state.column),
                state,
            ));
        }
    } else if noncapitalized_parts.is_empty() {
        // We have capitalized parts only, so this must be a tag.
        match cparts {
            0 => {
                // We had neither capitalized nor noncapitalized parts,
                // yet we made it this far. The only explanation is that this was
                // a stray '.' drifting through the cosmos.
                return Err((
                    MadeProgress,
                    BadIdent::StrayDot(state.line, state.column),
                    state,
                ));
            }
            1 => {
                let chomped = chomped_capitalized;
                let value = unsafe { std::str::from_utf8_unchecked(&bytes[..chomped]) };
                Ident::GlobalTag(value)
            }
            _ => {
                // This is a qualified tag, which is not allowed!
                return Err((
                    MadeProgress,
                    BadIdent::QualifiedTag(state.line, state.column),
                    state,
                ));
            }
        }
    } else {
        // We have multiple noncapitalized parts, so this must be field access.
        let module_name = if cparts == 0 {
            ""
        } else {
            let chomped = chomped_capitalized;
            unsafe { std::str::from_utf8_unchecked(&bytes[..chomped]) }
        };

        Ident::Access {
            module_name,
            parts: noncapitalized_parts.into_bump_slice(),
        }
    };

    Ok((Progress::MadeProgress, answer, state))
}
