app [main!] { pf: platform "../fx-open/platform/main.roc" }

import pf.Stdout

# `Stream.next!`'s published result row is implicitly open, exactly like
# `Iter.next`'s, although its body reads the step function out of a record
# pattern: a match with an extra `Other` branch widens it (design.md "Row
# Subsumption"). `Other` sorts between `One` and `Skip`, so a result numbered
# by the step row alone shows up as a wrong branch.

describe! : Stream(U64) => Str
describe! = |source| match source.next!() {
    One({ item, .. }) => "One(${item.to_str()})"
    Skip({ rest }) =>
        match rest.next!() {
            One({ item, .. }) => "Skip then One(${item.to_str()})"
            Skip(_) => "Skip then Skip"
            Done => "Skip then Done"
            Other => "Skip then Other"
        }
    Done => "Done"
    Other => "Other"
}

main! = |_args| {
    Stdout.line!(describe!(Stream.custom(3, Unknown, |n| if n < 4 Ok((n, n + 1)) else Err(NoMore))))
    Stdout.line!(describe!([1, 5].iter().keep_if(|n| n > 1).stream()))
    Stdout.line!(describe!([].iter().stream()))
    Ok({})
}
