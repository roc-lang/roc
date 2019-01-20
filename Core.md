Uses for unions:

1. Reduce

## reduce : (a, b -> b | Done), b, Vec a -> b

Being able to return Done when you're done means you can implement things like
`find` without needing recursion.

Having to return (Continue val) or Done every time would be tedious. This is a
simpler API.

2. Additional guarantees

I can define my reducing function as either of these:

reduceFn : String, Int -> Int

reduceFn : String, Int -> Int | Done

These types unify, meaning I can say something more specific about my 
types than I can with something like Maybe. "This reduce call never bails
out early."

# Vec


## empty : Vec *

## isEmpty : Vec * -> Bool

## length : Vec * -> Int

## get : Int, Vec elem -> elem | Nil

## put : Int, elem, Vec elem -> Vec elem

## push : elem, Vec elem -> Vec elem

## concat : Vec elem, Vec elem -> Vec elem

Not thrilled with this. Order too easy to mess up in pipeline vs not.

## find : (elem -> Bool) -> Vec elem -> elem | Nil

## map : (a -> b), Vec a -> Vec b

## indexedMap : (Int, a -> b), Vec a -> Vec b

## reduce : (a, b -> b | Done), b, Vec a -> b

## reduceFromEnd : (a, b -> b | Done), b, Vec a -> b

## keepIf : (elem -> Bool), Vec elem -> Vec elem

## dropIf : (elem -> Bool), Vec elem -> Vec elem


# String

## isEmpty : String -> Bool

## length : String -> Int

## replace : { every : String, with : String }, String -> String

## concat : String, String -> String

Not thrilled with this. Order too easy to mess up in pipeline vs not.

## join : String, Vec String -> String

## split : String, String -> Vec String

## takeFirst : Int, String -> String

## takeLast : Int, String -> String

## dropFirst : Int, String -> String

## dropLast : Int, String -> String

## startsWith : String, String -> Bool

## endWith : String, String -> Bool

## toInt : String -> Int | Nil

## toFloat : String -> Float | Nil

## fromInt : Int -> String

## fromFloat : Float -> String

## toUpper : String -> String

## toLower : String -> String

## trim : String -> String

## padStart : Int, String -> String

## padEnd : Int, String -> String
