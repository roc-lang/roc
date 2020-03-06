interface Bool
    exposes [ Bool, not, equal, notEqual ]
    imports []

## Either #True or #False.
Bool : [ False, True ]

## Returns #False when given #True, and vice versa.
not : Bool -> Bool

## Returns #True when given #True and #True, and #False when either argument is #False.
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
## In dev builds, this works exactly as described. In release builds, as a
## performance optimization, the compiler translates calls to #Bool.and
## (and #Bool.or) from function calls into conditionals. If its first
## argument evalutes to #False, then any function calls in the second argument
## get skipped, and the entire expression immediately evaluates to #False.
##
## For example:
##
##     List.isEmpty list && Str.isEmpty str
##
## In a release build, `Str.isEmpty` will only be called if `List.isEmpty list`
## returns #True. If `List.isEmpty list` returns #False, the entire expression
## will immediately evaluate to #False.
##
## Since all Roc expressions are pure, this will always give the same answer
## as if both #Bool arguments had been fully evaluated (as they are in
## dev builds), but it can potentially avoid costly function calls in release builds.
##
## Because this optimization only skips function calls, you can opt out of it
## by calling the function up front, and giving its result a name. For example:
##
##     emptyStr = Str.isEmpty str
##
##     List.isEmpty list && emptyStr
##
## Here, `Str.isEmpty` will always be called no matter what, and the `&&` will
## not get compiled to a conditional because there are no function calls
## involved in its second argument.
##
## If you know the functions involved in the second argument are trivial
## (for example, they are other #&&, #||, and #Bool.not operations), then
## this can potentially be a (likely extremely minor) performance optimization
## because a logical `AND` instruction typically executes faster than a
## [branch misprediction](https://danluu.com/branch-prediction).
##
## That said, in practice the `&& Str.isEmpty str` approach will typically run
## faster than the `&& emptyStr` approach - both for `Str.isEmpty` in particular
## as well as for most functions in general.
and : Bool, Bool -> Bool


## Returns #True when given #True for either argument, and #False only when given #False and #False.
##
## `a || b` is shorthand for `Bool.or a b`.
##
## >>> True || True
#
## >>> True || False
#
## >>> False || True
##
## >>> False || False
##
## ## Performance Notes
##
## #Bool.or does the same "compile to a conditional in release mode" optimization
## that #Bool.and does, except it short-circuits when the first argument is
## #True (causing it to immediately returns #True).
##
## See the performance notes for #Bool.and for details.
or : Bool, Bool -> Bool

## Returns #True if the two values are *structurally equal*, and #False otherwise.
##
## Structural equality works as follows:
##
## 1. #Int and #Float values are equal if their numbers are equal.
## 2. Records are equal if all their fields are equal.
## 3. Global tags are equal if they are the same tag, and also their contents (if any) are equal.
## 4. Private tags are equal if they are the same tag, in the same module, and also their contents (if any) are equal.
## 5. Collections (#String, #List, #Map, #Set, and #Bytes) are equal if they are the same length, and also all their corresponding elements are equal.
## 6. All functions are considered equal. (So `Bool.not == Bool.not` will return #True, as you might expect, but also `Num.abs == Num.negate` will return #True, as you might not. This design is because function equality has been formally proven to be undecidable in the general case, and returning #True in all cases turns out to be mostly harmless - especially compared to alternative designs like crashing, making #equal inconvenient to use, and so on.)
##
## This is the same as the #== operator.
eq : val, val -> Bool

## Calls #eq on the given values, then calls #not on the result.
##
## This is the same as the #=/= operator.
notEq : val, val -> Bool
notEq = \left, right ->
    not (equal left right)
