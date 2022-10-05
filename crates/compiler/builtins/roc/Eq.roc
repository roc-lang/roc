interface Eq
  exposes [
    Eq,
    isEq,
    isNotEq,
    structuralEq,
  ]
  imports [
    Bool,
  ]

## A type that can be compared for total equality.
##
## Total equality means that all values of the type can be compared to each
## other, and two values `a`, `b` are identical if and only if `isEq a b` is
## `Bool.true`.
##
## Not all types support total equality. For example, an [F32] or [F64] can
## be a `NaN` ([not a number](https://en.wikipedia.org/wiki/NaN)), and the [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754)
## floating point standard specifies that two `NaN`s are never equal to each other.
Eq has
  ## Returns `Bool.true` if the two values are equal, and `Bool.false` otherwise.
  ##
  ## `a == b` is shorthand for `Eq.isEq a b`.
  ##
  ## When `isEq` is derived by the Roc compiler, values are compared via
  ## structural equality. Structural equality works as follows:
  ##
  ## 1. Tags are equal if they have the same tag name, and also their contents (if any) are equal.
  ## 2. Records are equal if all their fields are equal.
  ## 3. Collections ([Str], [List], [Dict], and [Set]) are equal if they are the same length, and also all their corresponding elements are equal.
  ## 4. [Num](Num#Num) values are equal if their numbers are equal, with one exception: if both arguments to `isEq` are *NaN*, then `isEq` returns `Bool.false`. See `Num.isNaN` for more about *NaN*.
  ## 5. Functions can never be compared for structural equality. Roc cannot derive `isEq` for types that contain functions!
  isEq : a, a -> Bool | a has Eq

## Calls [isEq] on the given values, then calls [not] on the result.
##
## `a != b` is shorthand for `Eq.isNotEq a b`.
isNotEq : a, a -> Bool | a has Eq
isNotEq = \a, b -> Bool.not (isEq a b)

# INTERNAL COMPILER USE ONLY: used to lower calls to `isEq` to structural
# equality via the `Eq` low-level for derived types.
structuralEq : a, a -> Bool
