module [Bool, Eq, true, false, and, or, not, is_eq, is_not_eq]

## Defines a type that can be compared for total equality.
##
## Total equality means that all values of the type can be compared to each
## other, and two values `a`, `b` are identical if and only if `isEq(a, b)` is
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
    ## operator `==` can be used as shorthand for `Bool.is_eq`.
    ##
    ## **Note** that when `is_eq` is determined by the Roc compiler, values are
    ## compared using structural equality. The rules for this are as follows:
    ##
    ## 1. Tags are equal if their name and also contents are equal.
    ## 2. Records are equal if their fields are equal.
    ## 3. The collections [Str], [List], [Dict], and [Set] are equal iff they
    ## are the same length and their elements are equal.
    ## 4. [Num] values are equal if their numbers are equal. However, if both
    ## inputs are *NaN* then `is_eq` returns `Bool.false`. Refer to `Num.is_nan`
    ## for more detail.
    ## 5. Functions cannot be compared for structural equality, therefore Roc
    ## cannot derive `is_eq` for types that contain functions.
    is_eq : a, a -> Bool where a implements Eq

## Represents the boolean true and false using an opaque type.
## `Bool` implements the `Eq` ability.
Bool := [True, False] implements [Eq { is_eq: bool_is_eq }]

bool_is_eq = \@Bool(b1), @Bool(b2) -> structural_eq(b1, b2)

## The boolean true value.
true : Bool
true = @Bool(True)

## The boolean false value.
false : Bool
false = @Bool(False)

## Returns `Bool.true` when both inputs are `Bool.true`. This is equivalent to
## the logic [AND](https://en.wikipedia.org/wiki/Logical_conjunction)
## gate. The infix operator `&&` can also be used as shorthand for
## `Bool.and`.
##
## ```roc
## expect Bool.and(Bool.true, Bool.true) == Bool.true
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
## `enable_pets && likes_dogs(user)` would compile to.
## ```roc
## if enable_pets then
##     likes_dogs(user)
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
## ```roc
## expect Bool.or(Bool.false, Bool.true) == Bool.true
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
## ```roc
## expect Bool.not(Bool.false) == Bool.true
## expect !Bool.false == Bool.true
## ```
not : Bool -> Bool

## This will call the function `Bool.is_eq` on the inputs, and then `Bool.not`
## on the result. The is equivalent to the logic
## [XOR](https://en.wikipedia.org/wiki/Exclusive_or) gate. The infix operator
## `!=` can also be used as shorthand for `Bool.is_not_eq`.
##
## **Note** that `is_not_eq` does not accept arguments whose types contain
## functions.
## ```roc
## expect Bool.is_not_eq(Bool.false, Bool.true) == Bool.true
## expect (Bool.false != Bool.false) == Bool.false
## expect "Apples" != "Oranges"
## ```
is_not_eq : a, a -> Bool where a implements Eq
is_not_eq = \a, b -> structural_not_eq(a, b)

# INTERNAL COMPILER USE ONLY: used to lower calls to `is_eq` to structural
# equality via the `Eq` low-level for derived types.
structural_eq : a, a -> Bool

# INTERNAL COMPILER USE ONLY: used to lower calls to `is_not_eq` to structural
# inequality via the `NotEq` low-level for derived types.
structural_not_eq : a, a -> Bool
