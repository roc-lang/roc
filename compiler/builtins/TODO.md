# Builtins to do

## `List`

* `last : List elem -> Result elem [ ListWasEmpty ]*`
* `max : List (Num a) -> Result (Num a) [ ListWasEmpty ]*`
* `min : List (Num a) -> Result (Num a) [ ListWasEmpty ]*`
* `sum : List (Num a) -> Num a`
* `product : List (Num a) -> Num a`
* `repeat : elem, Int -> List elem`
* `intersperse : List elem, elem -> List elem`
* `range : Int a, Int a -> List (Int a)`
* `sort : List elem, (elem, elem -> [ Lt, Eq, Gt ]) -> List elem`
* `set : List elem, Int, elem -> List elem`
* `dropLast : List elem -> List elem`
* `dropFirst : List elem -> List elem`
* `drop : List elem, Int -> List elem`
* `takeFirst : List elem, Int -> List elem`
* `takeLast : List elem, Int -> List elem`
* `split : List elem, Int -> { before: List elem, others: List elem }`
* `sublist : List elem, { start : Int, len : Int } -> List elem`
* `update : List elem, Int, (elem -> elem) -> List elem`
* `updater : List elem, Int -> { elem, new : elem -> List elem }`
* `mapWithIndex : List before, (before, Int -> after) -> List after`
* `mapOks : List before, (before -> Result after *) -> List after`
* `oks : List (Result elem *) -> List elem`
* `joinMap : List before, (before -> List after) -> List after`
* `map2 : List a, List b, (a, b -> c) -> List c`
* `map3 : List a, List b, List c, (a, b, c -> d) -> List d`
* `map4 : List a, List b, List c, List d, (a, b, c, d -> e) -> List e`
* `walkUntil : List elem, { start : state, step : (state, elem -> [ Continue state, Stop state ]) } -> state`
* `walkBackwardsUntil : List elem, { start : state, step : (state, elem -> [ Continue state, Stop state ]) } -> state`
* `all : List elem, (elem -> Bool) -> Bool`
* `any : List elem, (elem -> Bool) -> Bool`

## `Num`

* `toStr : Num * -> Str`
* `cast : Num * -> Num *`
* `divRound : Int, Int -> Int`
* `xor : Int -> Int -> Int`
* `and : Int -> Int -> Int`
* `not : Int -> Int`
* `compare : Num a, Num a -> [ Lt, Eq, Gt ]`
