interface List
    exposes
        [
            isEmpty,
            get,
            set,
            replace,
            append,
            map,
            len,
            walkBackwards,
            concat,
            first,
            single,
            repeat,
            reverse,
            prepend,
            join,
            keepIf,
            contains,
            sum,
            walk,
            last,
            keepOks,
            keepErrs,
            mapWithIndex,
            map2,
            map3,
            product,
            walkUntil,
            range,
            sortWith,
            drop,
            swap,
            dropAt,
            dropLast,
            min,
            max,
            map4,
            dropFirst,
            joinMap,
            any,
            takeFirst,
            takeLast,
            find,
            sublist,
            intersperse,
            split,
            all,
            dropIf,
            sortAsc,
            sortDesc,
        ]
    imports
        [
            Bool.{ Bool }
        ]

isEmpty : List a -> Bool
isEmpty = \list ->
    List.len list == 0

get : List a, Nat -> Result a [ OutOfBounds ]*
replace : List a, Nat, a -> { list : List a, value : a }

set : List a, Nat, a -> List a
set = \list, index, value ->
    (List.replace list index value).list

append : List a, a -> List a
prepend : List a, a -> List a
len : List a -> Nat
concat : List a, List a -> List a
last : List a -> Result a [ ListWasEmpty ]*
single : a -> List a
repeat : a, Nat -> List a
reverse : List a -> List a
join : List (List a) -> List a
contains : List a, a -> Bool
walk : List elem, state, (state, elem -> state) -> state
walkBackwards  : List elem, state, (state, elem -> state) -> state
walkUntil : List elem, state, (state, elem -> [ Continue state, Stop state ]) -> state

sum : List (Num a) -> Num a
sum = \list ->
    List.walk list 0 Num.add

product : List (Num a) -> Num a
product = \list ->
    List.walk list 1 Num.mul

any : List a, (a -> Bool) -> Bool
all : List a, (a -> Bool) -> Bool

keepIf : List a, (a -> Bool) -> List a
dropIf : List a, (a -> Bool) -> List a
dropIf = \list, predicate ->
    List.keepIf list (\e -> Bool.not (predicate e))

keepOks : List before, (before -> Result after *) -> List after
keepErrs: List before, (before -> Result * after) -> List after
map : List a, (a -> b) -> List b
map2 : List a, List b, (a, b -> c) -> List c
map3 : List a, List b, List c, (a, b, c -> d) -> List d
map4 : List a, List b, List c, List d, (a, b, c, d -> e) -> List e
mapWithIndex : List a, (a, Nat -> b) -> List b
range : Int a, Int a -> List (Int a)
sortWith : List a, (a, a -> [ LT, EQ, GT ] ) -> List a
sortAsc : List (Num a) -> List (Num a)
sortAsc = \list -> List.sortWith list Num.compare

sortDesc : List (Num a) -> List (Num a)
sortDesc = \list -> List.sortWith list (\a, b -> Num.compare b a)

swap : List a, Nat, Nat -> List a

first : List a -> Result a [ ListWasEmpty ]*

dropFirst : List elem -> List elem
dropLast : List elem -> List elem

takeFirst : List elem, Nat -> List elem
takeLast : List elem, Nat -> List elem

drop : List elem, Nat -> List elem
dropAt : List elem, Nat -> List elem

min :  List (Num a) -> Result (Num a) [ ListWasEmpty ]*
min = \list ->
    when List.first list is
        Ok initial ->
            Ok (minHelp list initial)

        Err ListWasEmpty ->
            Err ListWasEmpty


minHelp :  List (Num a), Num a -> Num a
minHelp = \list, initial ->
    List.walk list initial \bestSoFar, current ->
        if current < bestSoFar then
            current

        else
            bestSoFar

max :  List (Num a) -> Result (Num a) [ ListWasEmpty ]*
max = \list ->
    when List.first list is
        Ok initial ->
            Ok (maxHelp list initial)

        Err ListWasEmpty ->
            Err ListWasEmpty


maxHelp :  List (Num a), Num a -> Num a
maxHelp = \list, initial ->
    List.walk list initial \bestSoFar, current ->
        if current > bestSoFar then
            current

        else
            bestSoFar

joinMap : List a, (a -> List b) -> List b
joinMap = \list, mapper ->
    List.walk list [] (\state, elem -> List.concat state (mapper elem))

find : List elem, (elem -> Bool) -> Result elem [ NotFound ]*
sublist : List elem, { start : Nat, len : Nat } -> List elem
intersperse : List elem, elem -> List elem
split : List elem, Nat -> { before: List elem, others: List elem }
