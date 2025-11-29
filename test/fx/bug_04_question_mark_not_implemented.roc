app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Bug 4: The `?` operator
# Previously showed: "This feature is not yet implemented: canonicalize suffix_single_question expression"
# Now it works!

# Helper function that can return an error
get_greeting : {} -> Try(Str, [ListWasEmpty])
get_greeting = |{}| {
    first = List.first(["hello"])?
    Ok(first)
}

main! = || {
    match get_greeting({}) {
        Ok(greeting) => Stdout.line!(greeting)
        Err(ListWasEmpty) => Stdout.line!("List was empty!")
    }
}
