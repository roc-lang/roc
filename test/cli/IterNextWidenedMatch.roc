# `Iter.next`'s published result row `[One(..), Skip(..), Done]` is implicitly
# open, so a match with an extra `Other` branch widens it (design.md "Row
# Subsumption"). A compiler-generated iterator's `next` is requested at that
# wider row while its step closure keeps the iterator's own closed row, which
# the body's row coercion joins in (design.md "Closed Result Rows At Template
# Boundaries"). `Other` sorts between `One` and `Skip`, so a result numbered by
# the step row alone shows up as a wrong branch, on both backends.
IterNextWidenedMatch := {}

counted : U64 -> Iter(U64)
counted = |limit|
    Iter.custom(
        0,
        Unknown,
        |n| if n < limit Ok((n, n + 1)) else Err(NoMore),
    )

method_call : U64 -> Str
method_call = |limit| match counted(limit).next() {
    One({ item, .. }) => "One(${item.to_str()})"
    Skip(_) => "Skip"
    Done => "Done"
    Other => "Other"
}

direct_call : U64 -> Str
direct_call = |limit| match Iter.next(counted(limit)) {
    One({ item, .. }) => "One(${item.to_str()})"
    Skip(_) => "Skip"
    Done => "Done"
    Other => "Other"
}

from_list : List(Str) -> Str
from_list = |items| match items.iter().next() {
    One({ item, .. }) => "One(${item})"
    Skip(_) => "Skip"
    Done => "Done"
    Other => "Other"
}

kept_after_skip : List(U64) -> Str
kept_after_skip = |items| match items.iter().keep_if(|n| n > 1).next() {
    One({ item, .. }) => "One(${item.to_str()})"
    Skip({ rest }) =>
        match rest.next() {
            One({ item, .. }) => "Skip then One(${item.to_str()})"
            Skip(_) => "Skip then Skip"
            Done => "Skip then Done"
            Other => "Skip then Other"
        }
    Done => "Done"
    Other => "Other"
}

constant = match Iter.custom(0, Unknown, |_| Err(NoMore)).next() {
    One(_) => "One"
    Skip(_) => "Skip"
    Done => "Done"
    Other => "Other"
}

expect method_call(3) == "One(0)"
expect method_call(0) == "Done"
expect direct_call(3) == "One(0)"
expect direct_call(0) == "Done"
expect from_list(["a", "b"]) == "One(a)"
expect from_list([]) == "Done"
expect kept_after_skip([1, 5]) == "Skip then One(5)"
expect kept_after_skip([7]) == "One(7)"
expect kept_after_skip([1]) == "Skip then Done"
expect constant == "Done"
