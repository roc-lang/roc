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
    Malformed(&'a [IdentProblem]),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum IdentPart {
    AtCapitalized,
    Capitalized,
    Uncapitalized,
    Malformed(IdentProblem),
    /// Could be two dots back to back, or a dot at the front (accessor function)
    Empty,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum IdentProblem {
    ContainedUnderscore(usize),
    AtSignInMiddle(usize),
    UncapitalizedAt(usize),
    PartBeginsWithNumber(usize),
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

    fn from_parts(arena: &'a Bump, parts: &[(IdentPart, &'a str)]) -> Self {
        // Validate parts. Valid patterns include:
        //
        // Package qualified lookup, e.g. (json.Foo.Bar.baz)
        // Package qualified field access, e.g. (json.Foo.Bar.baz.blah)
        // Module qualified lookup, e.g. (Foo.Bar.baz)
        // Module qualified field access, e.g. (Foo.Bar.baz.blah)
        // Field access, e.g. (baz.blah)
        // Lookup, e.g. (blah)
        // Global tag, e.g. (Foo)
        // Private tag, e.g. (@Foo)
        let mut iter = parts.into_iter().peekable();
        let mut module_parts = bumpalo::collections::Vec::new_in(arena);
        let mut field_access_parts = bumpalo::collections::Vec::new_in(arena);
        let lookup_name: Option<&str>;
        let package_part: Option<&str>;

        // Start by examining the first ident, because that rules out
        // a lot of cases.
        match iter.next().unwrap() {
            (IdentPart::AtCapitalized, part) => {
                // This is a private tag, so it should only have one part!
                if iter.peek().is_none() {
                    let variant_var = var_store.fresh();
                    let ext_var = var_store.fresh();
                    let tag_ident = env.ident_ids.get_or_insert(&(*tag).into());
                    let symbol = Symbol::new(env.home, tag_ident);

                    return (
                        Tag {
                            name: TagName::Private(symbol),
                            arguments: vec![],
                            variant_var,
                            ext_var,
                        },
                        Output::default(),
                    );
                } else {
                    todo!("return malformed");
                }
            }
            (IdentPart::Capitalized, part) => {
                if iter.peek().is_none() {
                    // This must be a global tag, because it's one
                    // uppercase part and that's it.
                    let variant_var = var_store.fresh();
                    let ext_var = var_store.fresh();

                    (
                        Tag {
                            name: TagName::Global((*tag).into()),
                            arguments: vec![],
                            variant_var,
                            ext_var,
                        },
                        Output::default(),
                    )
                } else {
                    module_parts.push(part);
                }
            }
            (IdentPart::Uncapitalized, part) => {
                if iter.peek().is_none() {
                    // This must be a lookup, because it's one
                    // lowercase part and that's it.
                    return canonicalize_lookup(
                        env,
                        scope,
                        package_part,
                        module_parts,
                        ident,
                        region,
                    );
                } else {
                    // From here, it could be a module, or a field access on a lookup.
                    // We can tell which based on whether the next part
                    // is capitalized.
                    match iter.next() {
                        IdentPart::AtUppercase(part) => {
                            todo!("malformed");
                        }
                        IdentPart::Uppercase(part) => {
                            if iter.peek().is_some() {
                                package_part = None;
                                lookup_name = None;
                                module_parts.push(part);
                            } else {
                                // e.g. (json.Foo)
                                todo!("malformed");
                            }
                        }
                        IdentPart::Lowercase(field_part) => {
                            // This must be field access on a lookup.
                            package_part = None;
                            lookup_name = Some(part);
                            field_access_parts.push(field_part);
                        }
                        IdentPart::Malformed(part) => {
                            todo!("malformed");
                        }
                    }
                }
            }
            (IdentPart::Malformed(problem), part) => {
                todo!("malformed");
            }
        }

        for ident_part in iter {
            match ident_part {
                IdentPart::AtCapitalized(part) => {
                    todo!("malformed");
                }
                IdentPart::Capitalized(part) => {
                    if lookup_name.is_none() {
                        module_parts.push(part);
                    } else {
                        todo!("malformed");
                    }
                }
                IdentPart::Uncapitalized(part) => {
                    if lookup_name.is_none() {
                        if iter.peek().is_none() {
                            // This must be a fully qualified lookup, because
                            // we didn't have a lookup name already, and there
                            // are no more parts after this.
                            return canonicalize_lookup(
                                env,
                                scope,
                                package_part,
                                module_parts,
                                ident,
                                region,
                            );
                        }

                        // This is now our lookup name. There will be
                        // field accesses after this.
                        lookup_name = Some(part);
                    } else {
                        // We already have a lookup name, so this must
                        // be a field access on it.
                        field_access_parts.push(part);
                    }
                }
                IdentPart::Malformed(part) => {
                    todo!("malformed");
                }
            }
        }

        // Now we're left with a lookup and 1+ field accesses on it.
        // First, canonicalize the lookup, then wrap it in however many
        // Access expressions we need.
        let (mut loc_expr, output) =
            canonicalize_lookup(env, scope, package_part, module_parts, ident, region);

        for field in field_access_parts {
            let expr = canonicalize_field_access(loc_expr, field, var_store);

            loc_expr = todo!("re-wrap the expr in the right Region.");
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
                        return malformed(
                            None,
                            arena,
                            state,
                            capitalized_parts,
                            noncapitalized_parts,
                        );
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

        (loc_expr.value, output)
    }
}

/// Parse an identifier into a &[IdentPart] slice.
///
/// This is separate from the `ident` Parser because string interpolation
/// wants to use it this way.
///
/// By design, this does not check for reserved keywords like "if", "else", etc.
/// Sometimes we may want to check for those later in the process, and give
/// more contextually-aware error messages than "unexpected `if`" or the like.
#[inline(always)]
pub fn parse_ident_parts<'a>(
    arena: &'a Bump,
    mut state: State<'a>,
) -> ParseResult<'a, &'a [(IdentPart, &'a str)]> {
    let mut parts = Vec::new_in(arena);
    let mut part_buf = String::new_in(arena); // The current "part" (parts are dot-separated.)
    let mut part_type = IdentPart::Empty;
    let mut original_state_bytes_len = state.bytes.len();

    // After the first character, only these are allowed:
    //
    // * Unicode alphabetic chars - you might name a variable `鹏` if that's clear to your readers
    // * ASCII digits - e.g. `1` but not `¾`, both of which pass .is_numeric()
    // * A dot ('.')
    //
    // The first character doesn't allow ASCII digits either.
    while !state.bytes.is_empty() {
        match peek_utf8_char(&state) {
            Ok((ch, mut bytes_parsed)) => {
                if ch.is_alphabetic() {
                    // Capitalization is determined by the first character in the part.
                    if part_buf.is_empty() {
                        part_type = if ch.is_uppercase() {
                            IdentPart::Capitalized
                        } else {
                            IdentPart::Uncapitalized
                        };
                    }

                    part_buf.push(ch);
                } else if ch.is_ascii_digit() {
                    if part_buf.is_empty() {
                        // The very first character in the identifier may not be a number!
                        // If we see a number, this isn't an identifier after all.
                        if parts.is_empty() {
                            return Err((
                                Fail {
                                    attempting: state.attempting,
                                    reason: unexpected,
                                },
                                state,
                            ));
                        } else {
                            let index = original_state_bytes_len - state.bytes.len();

                            // Parts may not start with numbers, so this
                            // is malformed.
                            let problem = IdentProblem::PartBeginsWithNumber(index);

                            part_type = IdentPart::Malformed(problem);
                        }
                    }

                    part_buf.push(ch);
                } else if ch == '.' {
                    // We've finished the part! Record it and reset for the next one.
                    parts.push((part_type, part_buf.into_bump_str()));

                    part_type = IdentPart::Empty;
                    part_buf = String::new_in(arena);
                } else if ch == '@' {
                    // This is AtCapitalized if it's both at the beginning
                    // of the part, and is followed by an uppercase letter.
                    // Otherwise, it's malformed!
                    match peek_utf8_char(&state) {
                        Ok((next_ch, next_bytes_parsed))
                            if part_buf.is_empty() && next_ch.is_uppercase() =>
                        {
                            bytes_parsed += next_bytes_parsed;

                            part_type = IdentPart::AtCapitalized;
                        }
                        Ok((next_ch, next_bytes_parsed))
                            if part_buf.is_empty() && !next_ch.is_uppercase() =>
                        {
                            bytes_parsed += next_bytes_parsed;

                            let index = original_state_bytes_len - state.bytes.len();
                            let problem = IdentProblem::UncapitalizedAt(index);

                            part_type = IdentPart::Malformed(problem);
                        }
                        Ok((next_ch, next_bytes_parsed)) if !part_buf.is_empty() => {
                            bytes_parsed += next_bytes_parsed;

                            let index = original_state_bytes_len - state.bytes.len();
                            let problem = IdentProblem::AtSignInMiddle(index);

                            part_type = IdentPart::Malformed(problem);
                        }
                        Err(reason) => {
                            state = state.advance_without_indenting(bytes_parsed)?;

                            return Err((
                                Fail {
                                    attempting: state.attempting,
                                    reason,
                                },
                                state,
                            ));
                        }
                    }
                } else if ch == '_' {
                    // This is definitely malformed, but we can give a special
                    // error message about how idents can't have underscores
                    // in them (which is something many languages allow).
                    let index = original_state_bytes_len - state.bytes.len();
                    let problem = IdentProblem::ContainedUnderscore(index);

                    part_type = IdentPart::Malformed(problem);
                } else {
                    // This must be the end of the identifier. We're done!

                    break;
                }

                state = state.advance_without_indenting(bytes_parsed)?;
            }
            Err(reason) => return state.fail(reason),
        }
    }

    // Record the final part.
    parts.push((part_type, part_buf.into_bump_str()));

    Ok((parts, state))
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
