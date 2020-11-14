use crate::ast::Attempting;
use crate::keyword;
use crate::parser::{peek_utf8_char, unexpected, Fail, FailReason, ParseResult, Parser, State};
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
    /// foo.bar
    Access(&'a str, &'a Ident<'a>),
    /// Foo.Bar.baz or foo.Bar.Baz.blah
    Lookup {
        module_name: &'a str,
        var_name: &'a str,
    },
    /// .foo
    AccessorFunction(&'a str),
    /// .Foo or foo. or something like foo.Bar
    Malformed(&'a [(usize, IdentProblem)]),
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
pub enum IdentProblem {
    ContainedUnderscore,
    AtSignInMiddle,
    UncapitalizedAt,
    PartBeginsWithNumber,
    DotAfterPrivateTag,
    CapitalizedAfterVar,
    QualifiedTag,
    AccessorFunctionCapitalized,
    /// Multiple dots e.g. (foo..bar) or, for accessor functions, perhaps (..foo)
    MultipleDots,
    /// Too many parts! e.g. (.foo.bar)
    AccessorFunctionMultipleFields,
    /// e.g. (.) - that's not an operator in Roc!
    StandaloneDot,
}

impl<'a> Ident<'a> {
    pub fn len(&self) -> usize {
        use self::Ident::*;

        match self {
            GlobalTag(string) | PrivateTag(string) => string.len(),
            Access(field_name, child) => {
                // +1 for the dot
                field_name.len() + child.len() + 1
            }
            Lookup {
                module_name,
                var_name,
            } => {
                if module_name.is_empty() {
                    var_name.len()
                } else {
                    // +1 for the dot
                    var_name.len() + module_name.len() + 1
                }
            }
            AccessorFunction(string) => string.len(),
            Malformed(string) => string.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline(always)]
    fn from_parts<I: Iterator<Item = (IdentPart, &'a str)>>(arena: &'a Bump, parts: I) -> Self {
        use IdentProblem::*;

        // Validate parts. Valid patterns include:
        //
        // Global tag, e.g. (Foo)
        // Private tag, e.g. (@Foo)
        // Lookup, e.g. (blah)
        // Field access, e.g. (baz.blah)
        // Module qualified lookup, e.g. (Foo.Bar.baz)
        // Package qualified lookup, e.g. (json.Foo.Bar.baz)
        // Module qualified field access, e.g. (Foo.Bar.baz.blah)
        // Package qualified field access, e.g. (json.Foo.Bar.baz.blah)
        let mut iter = parts.into_iter().peekable();
        let mut fields: Vec<'_, &str> = bumpalo::collections::Vec::new_in(arena);
        let mut module_name: String<'_> = bumpalo::collections::String::new_in(arena);
        let mut var_name = "";
        let mut package_name: Option<&str> = None;

        macro_rules! malformed {
            ($problem:expr) => {
                {
                    let mut offset = if module_name.is_empty() {
                        var_name.len()
                    } else {
                        // +1 for the dot
                        var_name.len() + module_name.len() + 1
                    };

                    for part in fields.iter().chain(package_name.iter()) {
                        // +1 for the dot
                        offset += part.len() + 1;
                    }

                    let problems = bumpalo::vec![in arena; (offset, $problem)];

                    Ident::Malformed(problems.into_bump_slice())
                }
            }
        }

        // Start by examining the first part, because there are lots
        // of special cases on the first one!
        match iter.next().unwrap() {
            (IdentPart::AtCapitalized, part_str) => {
                // This is a private tag, so it should only have one part!
                if iter.peek().is_none() {
                    return Ident::PrivateTag(part_str);
                } else {
                    return malformed!(DotAfterPrivateTag);
                }
            }
            (IdentPart::Capitalized, part_str) => {
                if iter.peek().is_none() {
                    // This must be a global tag, because it's one
                    // uppercase part and that's it.
                    return Ident::GlobalTag(part_str);
                } else {
                    // This must be a module name.
                    // We don't yet know what var_name is, so leave it as "".
                    module_name.push('.');
                    module_name.push_str(part_str);
                }
            }
            (IdentPart::Uncapitalized, part_str) => {
                // So far this looks like an unqualified lookup.
                // Later it may turn out to be a package qualifier instead.
                var_name = part_str;
            }
            (IdentPart::Malformed(problem), _) => {
                return malformed!(problem);
            }
            (IdentPart::Empty, _) => {
                // This means a dot at the front, which is an accessor function.
                match iter.next() {
                    // An accessor function should have one uncapitalized
                    // field followed by no other parts.
                    Some((IdentPart::Uncapitalized, field_name)) => match iter.next() {
                        None => {
                            return Ident::AccessorFunction(field_name);
                        }
                        Some(_) => {
                            // Too many parts! e.g. (.foo.bar)
                            return malformed!(IdentProblem::AccessorFunctionMultipleFields);
                        }
                    },
                    Some((IdentPart::Capitalized, _)) | Some((IdentPart::AtCapitalized, _)) => {
                        // Capitalized accessor function, e.g. (.Foo) or (.@Foo)
                        return malformed!(IdentProblem::AccessorFunctionCapitalized);
                    }
                    Some((IdentPart::Empty, _)) => {
                        // Multiple dots in front, e.g. (..foo)
                        return malformed!(IdentProblem::MultipleDots);
                    }
                    Some((IdentPart::Malformed(problem), _)) => {
                        return malformed!(problem);
                    }
                    None => {
                        // e.g. (.) - that's not an operator in Roc!
                        return malformed!(IdentProblem::StandaloneDot);
                    }
                }
            }
        }

        while let Some((part_type, part_str)) = iter.next() {
            match part_type {
                IdentPart::AtCapitalized => {
                    // e.g. `Foo.@Bar`
                    return malformed!(QualifiedTag);
                }
                IdentPart::Capitalized => {
                    // Any capitalized parts after the first part
                    // are definitely module parts.
                    module_name.push('.');
                    module_name.push_str(part_str);
                }
                IdentPart::Uncapitalized => {
                    var_name = if module_name.is_empty() {
                        part_str
                    } else {
                        debug_assert!(package_name.is_none());

                        // We already had a var_name, and then there was a
                        // module name after it, so actually what appeared
                        // to be a var name was a package name after all.
                        package_name = Some(var_name);

                        part_str
                    };

                    // It's invalid to have anything other than field accesses
                    // after the value!
                    while let Some((part_type, part_str)) = iter.next() {
                        match part_type {
                            IdentPart::Uncapitalized => {
                                fields.push(part_str);
                            }
                            IdentPart::AtCapitalized => {
                                return malformed!(AtSignInMiddle);
                            }
                            IdentPart::Capitalized => {
                                return malformed!(CapitalizedAfterVar);
                            }
                            IdentPart::Malformed(problem) => {
                                return malformed!(problem);
                            }
                            IdentPart::Empty => {
                                // Multiple dots, e.g. (foo..bar)
                                return malformed!(IdentProblem::MultipleDots);
                            }
                        }
                    }
                }
                IdentPart::Empty => {
                    // Multiple dots, e.g. (foo..bar)
                    return malformed!(IdentProblem::MultipleDots);
                }
                IdentPart::Malformed(problem) => {
                    return malformed!(problem);
                }
            }
        }

        // e.g. (Foo.Bar.Baz)
        if var_name.is_empty() {
            debug_assert!(!module_name.is_empty());

            return malformed!(QualifiedTag);
        }

        let mut answer = Ident::Lookup {
            module_name: module_name.into_bump_str(),
            var_name,
        };

        // Wrap the answer in field accesses as necessary
        for field in fields {
            answer = Ident::Access(field, arena.alloc(answer));
        }

        answer
    }
}

/// Parse an identifier into a &[IdentPart] slice.
///
/// By design, this does not check for reserved keywords like "if", "else", etc.
/// Sometimes we may want to check for those later in the process, and give
/// more contextually-aware error messages than "unexpected `if`" or the like.
#[inline(always)]
fn parse_ident<'a>(arena: &'a Bump, mut state: State<'a>) -> ParseResult<'a, Ident<'a>> {
    let mut parts = Vec::new_in(arena);
    let mut part_buf = String::new_in(arena); // The current "part" (parts are dot-separated.)
    let mut part_type = IdentPart::Empty;

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
                            return Err(unexpected(0, state.attempting, state));
                        } else {
                            // Parts may not start with numbers, so this
                            // is malformed.
                            part_type = IdentPart::Malformed(IdentProblem::PartBeginsWithNumber);
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

                            part_type = IdentPart::Malformed(IdentProblem::UncapitalizedAt);
                        }
                        Ok((_, next_bytes_parsed)) => {
                            bytes_parsed += next_bytes_parsed;

                            part_type = IdentPart::Malformed(IdentProblem::AtSignInMiddle);
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
                    part_type = IdentPart::Malformed(IdentProblem::ContainedUnderscore);
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

    Ok((Ident::from_parts(arena, parts.into_iter()), state))
}

pub fn ident<'a>() -> impl Parser<'a, Ident<'a>> {
    move |arena: &'a Bump, state: State<'a>| parse_ident(arena, state)
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
                    return Err(unexpected(0, Attempting::RecordFieldLabel, state));
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
