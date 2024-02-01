interface Bool
    exposes [Bool, Eq, true, false, and, or, not, isEq, isNotEq]

## Defines a type that can be compared for total equality.
##
## Total equality means that all values of the type can be compared to each
## other, and two values `a`, `b` are identical if and only if `isEq a b` is
## `Bool.true`.
##
## Not all types support total equality. For example, [`F32`](../Num#F32) and [`F64`](../Num#F64) can
## be a `NaN` ([Not a Number](https://en.wikipedia.org/wiki/NaN)), and the
## [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754) floating point standard
## specifies that two `NaN`s are not equal.
Eq implements
    ## Returns `Bool.true` if the input values are equal. This is
    ## equivalent to the logic
    ## [XNOR](https://en.wikipedia.org/wiki/Logical_equality) gate. The infix
    ## operator `==` can be used as shorthand for `Bool.isEq`.
    ##
    ## **Note** that when `isEq` is determined by the Roc compiler, values are
    ## compared using structural equality. The rules for this are as follows:
    ##
    ## 1. Tags are equal if their name and also contents are equal.
    ## 2. Records are equal if their fields are equal.
    ## 3. The collections [Str], [List], [Dict], and [Set] are equal iff they
    ## are the same length and their elements are equal.
    ## 4. [Num] values are equal if their numbers are equal. However, if both
    ## inputs are *NaN* then `isEq` returns `Bool.false`. Refer to `Num.isNaN`
    ## for more detail.
    ## 5. Functions cannot be compared for structural equality, therefore Roc
    ## cannot derive `isEq` for types that contain functions.
    isEq : a, a -> Bool where a implements Eq

## Represents the boolean true and false using an opaque type.
## `Bool` implements the `Eq` ability.
Bool := [True, False] implements [Eq { isEq: boolIsEq }]

boolIsEq = \@Bool b1, @Bool b2 -> structuralEq b1 b2

## The boolean true value.
true : Bool
true = @Bool True

## The boolean false value.
false : Bool
false = @Bool False

## Returns `Bool.true` when both inputs are `Bool.true`. This is equivalent to
## the logic [AND](https://en.wikipedia.org/wiki/Logical_conjunction)
## gate. The infix operator `&&` can also be used as shorthand for
## `Bool.and`.
##
## ```
## expect (Bool.and Bool.true Bool.true) == Bool.true
## expect (Bool.true && Bool.true) == Bool.true
## expect (Bool.false && Bool.true) == Bool.false
## expect (Bool.true && Bool.false) == Bool.false
## expect (Bool.false && Bool.false) == Bool.false
## ```
##
## ## Performance Details
##
## In Roc the `&&` and `||` work the same way as any
## other function. However, in some languages `&&` and `||` are special-cased.
## In these languages the compiler will skip evaluating the expression after the
## first operator under certain circumstances. For example an expression like
## `enablePets && likesDogs user` would compile to.
## ```
## if enablePets then
##     likesDogs user
## else
##     Bool.false
## ```
## Roc does not do this because conditionals like `if` and `when` have a
## performance cost. Calling a function can sometimes be faster across the board
## than doing an `if` to decide whether to skip calling it.
and : Bool, Bool -> Bool

## Returns `Bool.true` when either input is a `Bool.true`. This is equivalent to
## the logic [OR](https://en.wikipedia.org/wiki/Logical_disjunction) gate.
## The infix operator `||` can also be used as shorthand for `Bool.or`.
## ```
## expect (Bool.or Bool.false Bool.true) == Bool.true
## expect (Bool.true || Bool.true) == Bool.true
## expect (Bool.false || Bool.true) == Bool.true
## expect (Bool.true || Bool.false) == Bool.true
## expect (Bool.false || Bool.false) == Bool.false
## ```
##
## ## Performance Details
##
## In Roc the `&&` and `||` work the same way as any
## other functions. However, in some languages `&&` and `||` are special-cased.
## Refer to the note in `Bool.and` for more detail.
or : Bool, Bool -> Bool

## Returns `Bool.false` when given `Bool.true`, and vice versa. This is
## equivalent to the logic [NOT](https://en.wikipedia.org/wiki/Negation)
## gate. The operator `!` can also be used as shorthand for `Bool.not`.
## ```
## expect (Bool.not Bool.false) == Bool.true
## expect (!Bool.false) == Bool.true
## ```
not : Bool -> Bool

## This will call the function `Bool.isEq` on the inputs, and then `Bool.not`
## on the result. The is equivalent to the logic
## [XOR](https://en.wikipedia.org/wiki/Exclusive_or) gate. The infix operator
## `!=` can also be used as shorthand for `Bool.isNotEq`.
##
## **Note** that `isNotEq` does not accept arguments whose types contain
## functions.
## ```
## expect (Bool.isNotEq Bool.false Bool.true) == Bool.true
## expect (Bool.false != Bool.false) == Bool.false
## expect "Apples" != "Oranges"
## ```
isNotEq : a, a -> Bool where a implements Eq
isNotEq = \a, b -> structuralNotEq a b

# INTERNAL COMPILER USE ONLY: used to lower calls to `isEq` to structural
# equality via the `Eq` low-level for derived types.
structuralEq : a, a -> Bool

# INTERNAL COMPILER USE ONLY: used to lower calls to `isNotEq` to structural
# inequality via the `NotEq` low-level for derived types.
structuralNotEq : a, a -> Bool
