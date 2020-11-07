use crate::ast::Attempting;
use crate::keyword;
use crate::parser::{peek_utf8_char, unexpected, Fail, FailReason, ParseResult, Parser, State};
use bumpalo::collections::string::String;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_collections::all::arena_join;
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
    Malformed(&'a str),
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
            Malformed(string) => string.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// Parse an identifier into a string.
///
/// This is separate from the `ident` Parser because string interpolation
/// wants to use it this way.
///
/// By design, this does not check for reserved keywords like "if", "else", etc.
/// Sometimes we may want to check for those later in the process, and give
/// more contextually-aware error messages than "unexpected `if`" or the like.
#[inline(always)]
pub fn parse_ident<'a>(
    arena: &'a Bump,
    mut state: State<'a>,
) -> ParseResult<'a, (Ident<'a>, Option<char>)> {
    let mut part_buf = String::new_in(arena); // The current "part" (parts are dot-separated.)
    let mut capitalized_parts: Vec<&'a str> = Vec::new_in(arena);
    let mut noncapitalized_parts: Vec<&'a str> = Vec::new_in(arena);
    let mut is_capitalized;
    let is_accessor_fn;
    let mut is_private_tag = false;

    // Identifiers and accessor functions must start with either a letter or a dot.
    // If this starts with neither, it must be something else!
    match peek_utf8_char(&state) {
        Ok((first_ch, bytes_parsed)) => {
            if first_ch.is_alphabetic() {
                part_buf.push(first_ch);

                is_capitalized = first_ch.is_uppercase();
                is_accessor_fn = false;

                state = state.advance_without_indenting(bytes_parsed)?;
            } else if first_ch == '.' {
                is_capitalized = false;
                is_accessor_fn = true;

                state = state.advance_without_indenting(bytes_parsed)?;
            } else if first_ch == '@' {
                state = state.advance_without_indenting(bytes_parsed)?;

                // '@' must always be followed by a capital letter!
                match peek_utf8_char(&state) {
                    Ok((next_ch, next_bytes_parsed)) => {
                        if next_ch.is_uppercase() {
                            state = state.advance_without_indenting(next_bytes_parsed)?;

                            part_buf.push('@');
                            part_buf.push(next_ch);

                            is_private_tag = true;
                            is_capitalized = true;
                            is_accessor_fn = false;
                        } else {
                            return Err(unexpected(
                                bytes_parsed + next_bytes_parsed,
                                state,
                                Attempting::Identifier,
                            ));
                        }
                    }
                    Err(reason) => return state.fail(reason),
                }
            } else {
                return Err(unexpected(0, state, Attempting::Identifier));
            }
        }
        Err(reason) => return state.fail(reason),
    }

    while !state.bytes.is_empty() {
        match peek_utf8_char(&state) {
            Ok((ch, bytes_parsed)) => {
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
                } else if ch.is_ascii_digit() {
                    // Parts may not start with numbers!
                    if part_buf.is_empty() {
                        return malformed(
                            Some(ch),
                            arena,
                            state,
                            capitalized_parts,
                            noncapitalized_parts,
                        );
                    }

                    part_buf.push(ch);
                } else if ch == '.' {
                    // There are two posssible errors here:
                    //
                    // 1. Having two consecutive dots is an error.
                    // 2. Having capitalized parts after noncapitalized (e.g. `foo.Bar`) is an error.
                    if part_buf.is_empty() || (is_capitalized && !noncapitalized_parts.is_empty()) {
                        return malformed(
                            Some(ch),
                            arena,
                            state,
                            capitalized_parts,
                            noncapitalized_parts,
                        );
                    }

                    if is_capitalized {
                        capitalized_parts.push(part_buf.into_bump_str());
                    } else {
                        noncapitalized_parts.push(part_buf.into_bump_str());
                    }

                    // Now that we've recorded the contents of the current buffer, reset it.
                    part_buf = String::new_in(arena);
                } else {
                    // This must be the end of the identifier. We're done!

                    break;
                }

                state = state.advance_without_indenting(bytes_parsed)?;
            }
            Err(reason) => return state.fail(reason),
        }
    }

    if part_buf.is_empty() {
        // We probably had a trailing dot, e.g. `Foo.bar.` - this is malformed!
        //
        // This condition might also occur if we encounter a malformed accessor like `.|`
        //
        // If we made it this far and don't have a next_char, then necessarily
        // we have consumed a '.' char previously.
        return malformed(
            Some('.'),
            arena,
            state,
            capitalized_parts,
            noncapitalized_parts,
        );
    }

    // Record the final parts.
    if is_capitalized {
        capitalized_parts.push(part_buf.into_bump_str());
    } else {
        noncapitalized_parts.push(part_buf.into_bump_str());
    }

    let answer = if is_accessor_fn {
        // Handle accessor functions first because they have the strictest requirements.
        // Accessor functions may have exactly 1 noncapitalized part, and no capitalzed parts.
        if capitalized_parts.is_empty() && noncapitalized_parts.len() == 1 && !is_private_tag {
            let value = noncapitalized_parts.iter().next().unwrap();

            Ident::AccessorFunction(value)
        } else {
            return malformed(None, arena, state, capitalized_parts, noncapitalized_parts);
        }
    } else if noncapitalized_parts.is_empty() {
        // We have capitalized parts only, so this must be a tag.
        match capitalized_parts.first() {
            Some(value) => {
                if capitalized_parts.len() == 1 {
                    if is_private_tag {
                        Ident::PrivateTag(value)
                    } else {
                        Ident::GlobalTag(value)
                    }
                } else {
                    // This is a qualified tag, which is not allowed!
                    return malformed(None, arena, state, capitalized_parts, noncapitalized_parts);
                }
            }
            None => {
                // We had neither capitalized nor noncapitalized parts,
                // yet we made it this far. The only explanation is that this was
                // a stray '.' drifting through the cosmos.
                return Err(unexpected(1, state, Attempting::Identifier));
            }
        }
    } else if is_private_tag {
        // This is qualified field access with an '@' in front, which does not make sense!
        return malformed(None, arena, state, capitalized_parts, noncapitalized_parts);
    } else {
        // We have multiple noncapitalized parts, so this must be field access.
        Ident::Access {
            module_name: join_module_parts(arena, capitalized_parts.into_bump_slice()),
            parts: noncapitalized_parts.into_bump_slice(),
        }
    };

    Ok(((answer, None), state))
}

fn malformed<'a>(
    opt_bad_char: Option<char>,
    arena: &'a Bump,
    mut state: State<'a>,
    capitalized_parts: Vec<&'a str>,
    noncapitalized_parts: Vec<&'a str>,
) -> ParseResult<'a, (Ident<'a>, Option<char>)> {
    // Reconstruct the original string that we've been parsing.
    let mut full_string = String::new_in(arena);

    full_string
        .push_str(arena_join(arena, &mut capitalized_parts.into_iter(), ".").into_bump_str());
    full_string
        .push_str(arena_join(arena, &mut noncapitalized_parts.into_iter(), ".").into_bump_str());

    if let Some(bad_char) = opt_bad_char {
        full_string.push(bad_char);
    }

    // Consume the remaining chars in the identifier.
    let mut next_char = None;

    while !state.bytes.is_empty() {
        match peek_utf8_char(&state) {
            Ok((ch, bytes_parsed)) => {
                // We can't use ch.is_alphanumeric() here because that passes for
                // things that are "numeric" but not ASCII digits, like `¾`
                if ch == '.' || ch.is_alphabetic() || ch.is_ascii_digit() {
                    full_string.push(ch);
                } else {
                    next_char = Some(ch);

                    break;
                }

                state = state.advance_without_indenting(bytes_parsed)?;
            }
            Err(reason) => return state.fail(reason),
        }
    }

    Ok((
        (Ident::Malformed(full_string.into_bump_str()), next_char),
        state,
    ))
}

pub fn ident<'a>() -> impl Parser<'a, Ident<'a>> {
    move |arena: &'a Bump, state: State<'a>| {
        // Discard next_char; we don't need it.
        let ((string, _), state) = parse_ident(arena, state)?;

        Ok((string, state))
    }
}

pub fn global_tag_or_ident<'a, F>(pred: F) -> impl Parser<'a, &'a str>
where
    F: Fn(char) -> bool,
{
    move |arena, mut state: State<'a>| {
        // pred will determine if this is a tag or ident (based on capitalization)
        let (first_letter, bytes_parsed) = match peek_utf8_char(&state) {
            Ok((first_letter, bytes_parsed)) => {
                if !pred(first_letter) {
                    return Err(unexpected(0, state, Attempting::RecordFieldLabel));
                }

                (first_letter, bytes_parsed)
            }
            Err(reason) => return state.fail(reason),
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
                Err(reason) => return state.fail(reason),
            };
        }

        Ok((buf.into_bump_str(), state))
    }
}

/// This could be:
///
/// * A record field, e.g. "email" in `.email` or in `email:`
/// * A named pattern match, e.g. "foo" in `foo =` or `foo ->` or `\foo ->`
pub fn lowercase_ident<'a>() -> impl Parser<'a, &'a str> {
    move |arena, state| {
        let (ident, state) =
            global_tag_or_ident(|first_char| first_char.is_lowercase()).parse(arena, state)?;

        if (ident == keyword::IF)
            || (ident == keyword::THEN)
            || (ident == keyword::ELSE)
            || (ident == keyword::WHEN)
            || (ident == keyword::IS)
            || (ident == keyword::AS)
        {
            // TODO Calculate the correct region based on state
            let region = Region::zero();
            Err((
                Fail {
                    reason: FailReason::ReservedKeyword(region),
                    attempting: Attempting::Identifier,
                },
                state,
            ))
        } else {
            Ok((ident, state))
        }
    }
}

/// This could be:
///
/// * A module name
/// * A type name
/// * A global tag
pub fn uppercase_ident<'a>() -> impl Parser<'a, &'a str> {
    global_tag_or_ident(|first_char| first_char.is_uppercase())
}

pub fn unqualified_ident<'a>() -> impl Parser<'a, &'a str> {
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
