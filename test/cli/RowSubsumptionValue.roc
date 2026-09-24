# Row subsumption for an annotated top-level VALUE (design.md "Row
# Subsumption"). Each `v*` below forwards a closed row out of a nominal field
# instead of constructing it, so its body closes its annotated row; every use
# below widens that row anyway. The constant keeps one representation at its
# declared row and each widened use restores it there and re-tags it into its
# own row.
#
# Every widened row lists tags that sort AROUND the declared ones, so the
# declared and requested discriminants differ for every tag and a missing or
# misordered re-tag shows up as a wrong answer through `show*`.
RowSubsumptionValue := {}

import RowSubsumptionValueLib

Closed := { d : [B(Str), D], e : [B(Str), D], r : Try(Str, [NotFound]), o : Try(Str, [NotFound]) }

source : Closed
source = { d: B("x"), e: D, r: Err(NotFound), o: Ok("y") }

# The value's own root row: declared `[B(Str), D]` numbers `B` 0 and `D` 1,
# the widened `[A, B(Str), C, D]` numbers them 1 and 3.
vd : [B(Str), D]
vd = source.d

ve : [B(Str), D]
ve = source.e

show_direct : [A, B(Str), C, D] -> Str
show_direct = |v| match v { A => "A", B(s) => "B(${s})", C => "C", D => "D" }

expect show_direct(vd) == "B(x)"
expect show_direct(ve) == "D"

# The error row of the `Try` standing as the value's root: `NotFound` is 0 in
# the declared row and 1 in the widened `[Gone, NotFound]`.
vr : Try(Str, [NotFound])
vr = source.r

vo : Try(Str, [NotFound])
vo = source.o

show_try : Try(Str, [Gone, NotFound]) -> Str
show_try = |v| match v { Ok(s) => "Ok(${s})", Err(Gone) => "Gone", Err(NotFound) => "NotFound" }

expect show_try(vr) == "NotFound"
expect show_try(vo) == "Ok(y)"

# A value alias of a coerced value is itself coerced, and a constant whose
# body is a widened use stores the wider value.
alias : [B(Str), D]
alias = vd

wide : [A, B(Str), C, D]
wide = alias

expect show_direct(alias) == "B(x)"
expect show_direct(wide) == "B(x)"

# A use at exactly the declared row.
show_narrow : [B(Str), D] -> Str
show_narrow = |v| match v { B(s) => "B(${s})", D => "D" }

expect show_narrow(vd) == "B(x)"

# The error row is forwarded, so it is coerced, while the ok row is
# constructed, so it is quantified: a sealed-row constant whose uses widen
# either row.
sealed_err : Try([X], [NotFound])
sealed_err = source.r.map_ok(|_| X)

sealed_ok : Try([X], [NotFound])
sealed_ok = source.o.map_ok(|_| X)

show_sealed : Try([W, X], [Gone, NotFound]) -> Str
show_sealed = |t| match t { Ok(W) => "W", Ok(X) => "X", Err(Gone) => "Gone", Err(NotFound) => "NotFound" }

show_sealed_exact : Try([X], [NotFound]) -> Str
show_sealed_exact = |t| match t { Ok(X) => "X", Err(NotFound) => "NotFound" }

expect show_sealed(sealed_err) == "NotFound"
expect show_sealed(sealed_ok) == "X"
expect show_sealed_exact(sealed_err) == "NotFound"

# A value imported from another module, widened by this one.
expect show_direct(RowSubsumptionValueLib.lib_value) == "B(lib)"
