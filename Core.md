Uses for unions:

1. Reduce

## reduce : (a, b -> b | Done), b, Array a -> b

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



Example:


fibonacci : Int -> Int
fibonacci = index ->
    if index <= 1 then
        index
    else
        fibonacci (index - 1) + fibonacci (index - 2)




# Array


## empty : Array *

## isEmpty : Array * -> Bool

## length : Array * -> Int

## get : Int, Array elem -> elem | Nil

## put : Int, elem, Array elem -> Array elem

## push : elem, Array elem -> Array elem

## concat : Array elem, Array elem -> Array elem

## find : (elem -> Bool) -> Array elem -> elem | Nil

## map : (a -> b), Array a -> Array b

## indexedMap : (Int, a -> b), Array a -> Array b

## reduce : (a, b -> b | Done), b, Array a -> b

## reduceFromEnd : (a, b -> b | Done), b, Array a -> b

## keepIf : (elem -> Bool), Array elem -> Array elem

## dropIf : (elem -> Bool), Array elem -> Array elem


# String

## isEmpty : String -> Bool

## length : String -> Int

## replace : { every : String, with : String }, String -> String

## concat : String, String -> String

## join : String, Array String -> String

## split : String, String -> Array String

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
