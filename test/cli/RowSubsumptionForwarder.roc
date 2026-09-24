# Row subsumption for a NON-hosted forwarder (design.md "Row Subsumption").
# Each `fwd*` below returns a closed value it was handed instead of
# constructing its result, so its body closes its annotated result row; each
# caller widens that row anyway. The definition keeps one narrow
# representation and the widened use is served by the result-row widening
# adapter, which re-tags the narrow result into the caller's row.
#
# Every widened row lists tags that sort AROUND the declared ones, so the
# declared and requested discriminants differ for every tag and a missing or
# misordered re-tag shows up as a wrong answer through `show*`.
RowSubsumptionForwarder := {}

# The direct result row: declared `[B(Str), D]` numbers `B` 0 and `D` 1, the
# widened `[A, B(Str), C, D]` numbers them 1 and 3.
fwd_direct : [B(Str), D] -> [B(Str), D]
fwd_direct = |t| t

wide_direct : [B(Str), D] -> [A, B(Str), C, D]
wide_direct = |t| fwd_direct(t)

show_direct : [A, B(Str), C, D] -> Str
show_direct = |v| match v { A => "A", B(s) => "B(${s})", C => "C", D => "D" }

expect show_direct(wide_direct(B("x"))) == "B(x)"
expect show_direct(wide_direct(D)) == "D"

# The error row of a `Try` standing as the result: `NotFound` is 0 in the
# declared row and 1 in the widened `[Gone, NotFound]`.
fwd_try : Try(Str, [NotFound]) -> Try(Str, [NotFound])
fwd_try = |t| t

wide_try : Try(Str, [NotFound]) -> Try(Str, [Gone, NotFound])
wide_try = |t| fwd_try(t)

show_try : Try(Str, [Gone, NotFound]) -> Str
show_try = |v| match v { Ok(s) => "Ok(${s})", Err(Gone) => "Gone", Err(NotFound) => "NotFound" }

expect show_try(wide_try(Err(NotFound))) == "NotFound"
expect show_try(wide_try(Ok("y"))) == "Ok(y)"

# The same forwarder with its signature NAMED through an alias of the whole
# function type, which must be usable exactly like the inline spelling.
# `Extra` sorts between `Err` and `Ok`.
Status : [Ok(Str), Err(Str)]

Fwd : Status -> Status

fwd_alias : Fwd
fwd_alias = |s| s

wide_alias : Status -> [Ok(Str), Err(Str), Extra]
wide_alias = |s| fwd_alias(s)

show_alias : [Ok(Str), Err(Str), Extra] -> Str
show_alias = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show_alias(wide_alias(Ok("a"))) == "Ok(a)"
expect show_alias(wide_alias(Err("b"))) == "Err(b)"
