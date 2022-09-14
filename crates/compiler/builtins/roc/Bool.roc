interface Bool
    exposes [Bool, true, false, and, or, not, isEq, isNotEq]
    imports []

Bool := [True, False]

true : Bool
true = @Bool True

false : Bool
false = @Bool False

## Returns `True` when given `True` and `True`, and `False` when either argument is `False`.
##
## `a && b` is shorthand for `Bool.and a b`
##
## >>> True && True
##
## >>> True && False
##
## >>> False && True
##
## >>> False && False
##
## ## Performance Notes
##
## In some languages, `&&` and `||` are special-cased in the compiler to skip
## evaluating the expression after the operator under certain circumstances.
## For example, in some languages, `enablePets && likesDogs user` would compile
## to the equivalent of:
##
##     if enablePets then
##         likesDogs user
##     else
##         False
##
## In Roc, however, `&&` and `||` are not special. They work the same way as
## other functions. Conditionals like `if` and `when` have a performance cost,
## and sometimes calling a function like `likesDogs user` can be faster across
## the board than doing an `if` to decide whether to skip calling it.
##
## (Naturally, if you expect the `if` to improve performance, you can always add
## one explicitly!)
and : Bool, Bool -> Bool

## Returns `True` when given `True` for either argument, and `False` only when given `False` and `False`.
##
## `a || b` is shorthand for `Bool.or a b`.
##
## >>> True || True
##
## >>> True || False
##
## >>> False || True
##
## >>> False || False
##
## ## Performance Notes
##
## In some languages, `&&` and `||` are special-cased in the compiler to skip
## evaluating the expression after the operator under certain circumstances.
## In Roc, this is not the case. See the performance notes for [Bool.and] for details.
or : Bool, Bool -> Bool
# xor : Bool, Bool -> Bool # currently unimplemented
## Returns `False` when given `True`, and vice versa.
not : Bool -> Bool

## Returns `True` if the two values are *structurally equal*, and `False` otherwise.
##
## `a == b` is shorthand for `Bool.isEq a b`
##
## Structural equality works as follows:
##
## 1. Tags are equal if they have the same tag name, and also their contents (if any) are equal.
## 2. Records are equal if all their fields are equal.
## 3. Collections ([Str], [List], [Dict], and [Set]) are equal if they are the same length, and also all their corresponding elements are equal.
## 4. [Num](Num#Num) values are equal if their numbers are equal, with one exception: if both arguments to `isEq` are *NaN*, then `isEq` returns `False`. See `Num.isNaN` for more about *NaN*.
##
## Note that `isEq` takes `'val` instead of `val`, which means `isEq` does not
## accept arguments whose types contain functions.
isEq : a, a -> Bool

## Calls [isEq] on the given values, then calls [not] on the result.
##
## `a != b` is shorthand for `Bool.isNotEq a b`
##
## Note that `isNotEq` takes `'val` instead of `val`, which means `isNotEq` does not
## accept arguments whose types contain functions.
isNotEq : a, a -> Bool
