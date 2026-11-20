TryQuestionMapped := {}

# `? Tag` and `? |e| ...` wrap a narrower error row into the result's.

parse : Str -> Try(U64, [BadNum])
parse = |s| if s == "" Err(BadNum) else Ok(7)

fwd : Try(U64, [BadNum]) -> Try(U64, [BadNum])
fwd = |t| t

use : Str -> Try(U64, [Wrapped([BadNum]), Again([BadNum]), Empty])
use = |s| {
    x = parse(s) ? Wrapped
    y = fwd(if s == "z" Err(BadNum) else Ok(x)) ? |e| Again(e)
    if y == 0 Err(Empty) else Ok(y + 1)
}

show : Try(U64, [Wrapped([BadNum]), Again([BadNum]), Empty]) -> Str
show = |r| match r { Ok(n) => "Ok(${n.to_str()})", Err(Wrapped(BadNum)) => "Wrapped", Err(Again(BadNum)) => "Again", Err(Empty) => "Empty" }

expect show(use("")) == "Wrapped"
expect show(use("z")) == "Again"
expect show(use("a")) == "Ok(8)"
