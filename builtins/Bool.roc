interface Bool
    exposes [ Bool, not, equal, notEqual ]
    imports []

## Either #True or #False.
Bool : [ False, True ]

## Returns #False when given #True, and vice versa.
not : Bool -> Bool
not = \bool ->
    when bool is
        False -> True
        True -> False

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
