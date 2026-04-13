## Composable validators.
## Imports Util for string helpers.

import Util

Validator := [
    NotEmpty(Str),
    MinLength(Str, U64),
    MaxLength(Str, U64),
    MatchesAny(Str, List(Str)),
].{

    ## Validate that a string is not empty (after trimming).
    not_empty : Str -> Validator
    not_empty = |field_name| NotEmpty(field_name)

    ## Validate that a string has at least n characters.
    min_length : Str, U64 -> Validator
    min_length = |field_name, min| MinLength(field_name, min)

    ## Validate that a string has at most n characters.
    max_length : Str, U64 -> Validator
    max_length = |field_name, max| MaxLength(field_name, max)

    ## Validate that a string matches one of the allowed values.
    matches_any : Str, List(Str) -> Validator
    matches_any = |field_name, allowed| MatchesAny(field_name, allowed)

    ## Run a single validator on a value.
    run : Validator, Str -> Try(Str, Str)
    run = |validator, val| {
        # Pre-compute values to avoid calling imported functions inside match branches
        trimmed_val = Util.trim_all([val])
        is_trimmed_empty = trimmed_val == [""]
        val_len = val.to_utf8().len()

        match validator {
            NotEmpty(name) =>
                if is_trimmed_empty
                    Err("${name} must not be empty")
                else
                    Ok(val)

            MinLength(name, min) =>
                if val_len >= min
                    Ok(val)
                else
                    Err("${name} must be at least ${min.to_str()} characters")

            MaxLength(name, max) =>
                if val_len <= max
                    Ok(val)
                else
                    Err("${name} must be at most ${max.to_str()} characters")

            MatchesAny(name, allowed) =>
                if allowed.keep_if(|s| s != "").contains(val)
                    Ok(val)
                else
                    Err("${name} must be one of: ${Util.join_with(allowed.keep_if(|s| s != ""), ", ")}")
        }
    }

    ## Run multiple validators on the same value, collecting all errors.
    run_all : List(Validator), Str -> Try(Str, List(Str))
    run_all = |validators, val| {
        errors =
            validators.fold([], |acc, validator|
                match run(validator, val) {
                    Ok(_) => acc
                    Err(msg) => acc.append(msg)
                })

        if errors.is_empty()
            Ok(val)
        else
            Err(errors)
    }
}

# Tests

expect Validator.run(Validator.not_empty("name"), "Alice") == Ok("Alice")
expect Validator.run(Validator.not_empty("name"), "  ") == Err("name must not be empty")

expect Validator.run(Validator.min_length("pass", 8), "longpassword") == Ok("longpassword")
expect Validator.run(Validator.min_length("pass", 8), "short") == Err("pass must be at least 8 characters")

expect Validator.run(Validator.max_length("tag", 5), "hi") == Ok("hi")
expect Validator.run(Validator.max_length("tag", 5), "toolong") == Err("tag must be at most 5 characters")

expect Validator.run(Validator.matches_any("role", ["admin", "user"]), "admin") == Ok("admin")
expect Validator.run(Validator.matches_any("role", ["admin", "user"]), "guest") == Err("role must be one of: admin, user")

expect Validator.run_all([Validator.not_empty("x"), Validator.min_length("x", 3)], "ab") == Err(["x must be at least 3 characters"])
expect Validator.run_all([Validator.not_empty("x"), Validator.min_length("x", 3)], "abc") == Ok("abc")
