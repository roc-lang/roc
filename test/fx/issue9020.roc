app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Reproduces issue 9020: compiler crashes with "Error evaluating: NotNumeric"
# when using multiple ? operators with arithmetic inside a closure,
# applied via fold to a dynamically created list.

main! = || {
    # Key ingredients for the bug:
    # 1. Local closure f with multiple ? operators and arithmetic
    # 2. Dynamically created list via .map()
    # 3. fold with match on f's result
    # 4. Piping result to another function
    f = |digits| Ok(List.first(digits)? + List.first(digits)?)
    vs = ["a"].map(|s| s.to_utf8().map(|c| c.to_u64()))
    _result = vs.fold(
        [],
        |acc, v| {
            match f(v) {
                Ok(u) => acc.append(u)
                Err(_) => acc
            }
        },
    )->sum()
    Stdout.line!("Done")
}

sum : List(U64) -> U64
sum = |nums| nums.len()
