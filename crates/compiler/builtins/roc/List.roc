interface List
    exposes [
        isEmpty,
        get,
        set,
        replace,
        append,
        map,
        len,
        withCapacity,
        iterate,
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
        mapTry,
        dropFirst,
        joinMap,
        any,
        takeFirst,
        takeLast,
        findFirst,
        findLast,
        findFirstIndex,
        findLastIndex,
        sublist,
        intersperse,
        split,
        splitFirst,
        splitLast,
        startsWith,
        endsWith,
        all,
        dropIf,
        sortAsc,
        sortDesc,
        reserve,
    ]
    imports [
        Bool.{ Bool },
    ]

## Types
## A sequential list of values.
##
## >>> [1, 2, 3] # a list of numbers
## >>> ["a", "b", "c"] # a list of strings
## >>> [[1.1], [], [2.2, 3.3]] # a list of lists of numbers
##
## The maximum size of a [List] is limited by the amount of heap memory available
## to the current process. If there is not enough memory available, attempting to
## create the list could crash. (On Linux, where [overcommit](https://www.etalabs.net/overcommit.html)
## is normally enabled, not having enough memory could result in the list appearing
## to be created just fine, but then crashing later.)
##
## > The theoretical maximum length for a list created in Roc is half of
## > `Num.maxNat`. Attempting to create a list bigger than that
## > in Roc code will always fail, although in practice it is likely to fail
## > at much smaller lengths due to insufficient memory being available.
##
## ## Performance Details
##
## Under the hood, a list is a record containing a `len : Nat` field as well
## as a pointer to a reference count and a flat array of bytes. Unique lists
## store a capacity #Nat instead of a reference count.
##
## ## Shared Lists
##
## Shared lists are [reference counted](https://en.wikipedia.org/wiki/Reference_counting).
##
## Each time a given list gets referenced, its reference count ("refcount" for short)
## gets incremented. Each time a list goes out of scope, its refcount count gets
## decremented. Once a refcount, has been decremented more times than it has been
## incremented, we know nothing is referencing it anymore, and the list's memory
## will be immediately freed.
##
## Let's look at an example.
##
##     ratings = [5, 4, 3]
##
##     { foo: ratings, bar: ratings }
##
## The first line binds the name `ratings` to the list `[5, 4, 3]`. The list
## begins with a refcount of 1, because so far only `ratings` is referencing it.
##
## The second line alters this refcount. `{ foo: ratings` references
## the `ratings` list, which will result in its refcount getting incremented
## from 0 to 1. Similarly, `bar: ratings }` also references the `ratings` list,
## which will result in its refcount getting incremented from 1 to 2.
##
## Let's turn this example into a function.
##
##     getRatings = \first ->
##         ratings = [first, 4, 3]
##
##         { foo: ratings, bar: ratings }
##
##     getRatings 5
##
## At the end of the `getRatings` function, when the record gets returned,
## the original `ratings =` binding has gone out of scope and is no longer
## accessible. (Trying to reference `ratings` outside the scope of the
## `getRatings` function would be an error!)
##
## Since `ratings` represented a way to reference the list, and that way is no
## longer accessible, the list's refcount gets decremented when `ratings` goes
## out of scope. It will decrease from 2 back down to 1.
##
## Putting these together, when we call `getRatings 5`, what we get back is
## a record with two fields, `foo`, and `bar`, each of which refers to the same
## list, and that list has a refcount of 1.
##
## Let's change the last line to be `(getRatings 5).bar` instead of `getRatings 5`:
##
##     getRatings = \first ->
##         ratings = [first, 4, 3]
##
##         { foo: ratings, bar: ratings }
##
##     (getRatings 5).bar
##
## Now, when this expression returns, only the `bar` field of the record will
## be returned. This will mean that the `foo` field becomes inaccessible, causing
## the list's refcount to get decremented from 2 to 1. At this point, the list is back
## where it started: there is only 1 reference to it.
##
## Finally let's suppose the final line were changed to this:
##
##     List.first (getRatings 5).bar
##
## This call to [List.first] means that even the list in the `bar` field has become
## inaccessible. As such, this line will cause the list's refcount to get
## decremented all the way to 0. At that point, nothing is referencing the list
## anymore, and its memory will get freed.
##
## Things are different if this is a list of lists instead of a list of numbers.
## Let's look at a simpler example using [List.first] - first with a list of numbers,
## and then with a list of lists, to see how they differ.
##
## Here's the example using a list of numbers.
##
##     nums = [1, 2, 3, 4, 5, 6, 7]
##
##     first = List.first nums
##     last = List.last nums
##
##     first
##
## It makes a list, calls [List.first] and [List.last] on it, and then returns `first`.
##
## Here's the equivalent code with a list of lists:
##
##     lists = [[1], [2, 3], [], [4, 5, 6, 7]]
##
##     first = List.first lists
##     last = List.last lists
##
##     first
##
## TODO explain how in the former example, when we go to free `nums` at the end,
## we can free it immediately because there are no other refcounts. However,
## in the case of `lists`, we have to iterate through the list and decrement
## the refcounts of each of its contained lists - because they, too, have
## refcounts! Importantly, because the first element had its refcount incremented
## because the function returned `first`, that element will actually end up
## *not* getting freed at the end - but all the others will be.
##
## In the `lists` example, `lists = [...]` also creates a list with an initial
## refcount of 1. Separately, it also creates several other lists - each with
## their own refcounts - to go inside that list. (The empty list at the end
## does not use heap memory, and thus has no refcount.)
##
## At the end, we once again call [List.first] on the list, but this time
##
## * Copying small lists (64 elements or fewer) is typically slightly faster than copying small persistent data structures. This is because, at small sizes, persistent data structures tend to be thin wrappers around flat arrays anyway. They don't have any copying advantage until crossing a certain minimum size threshold.
## * Even when copying is faster, other list operations may still be slightly slower with persistent data structures. For example, even if it were a persistent data structure, [List.map], [List.walk], and [List.keepIf] would all need to traverse every element in the list and build up the result from scratch. These operations are all
## * Roc's compiler optimizes many list operations into in-place mutations behind the scenes, depending on how the list is being used. For example, [List.map], [List.keepIf], and [List.set] can all be optimized to perform in-place mutations.
## * If possible, it is usually best for performance to use large lists in a way where the optimizer can turn them into in-place mutations. If this is not possible, a persistent data structure might be faster - but this is a rare enough scenario that it would not be good for the average Roc program's performance if this were the way [List] worked by default. Instead, you can look outside Roc's standard modules for an implementation of a persistent data structure - likely built using [List] under the hood!
##  Check if the list is empty.
##
## >>> List.isEmpty [1, 2, 3]
##
## >>> List.isEmpty []
isEmpty : List a -> Bool
isEmpty = \list ->
    List.len list == 0

# unsafe primitive that does not perform a bounds check
# but will cause a reference count increment on the value it got out of the list
getUnsafe : List a, Nat -> a

get : List a, Nat -> Result a [OutOfBounds]*
get = \list, index ->
    if index < List.len list then
        Ok (List.getUnsafe list index)
    else
        Err OutOfBounds

# unsafe primitive that does not perform a bounds check
# but will cause a reference count increment on the value it got out of the list
replaceUnsafe : List a, Nat, a -> { list : List a, value : a }

replace : List a, Nat, a -> { list : List a, value : a }
replace = \list, index, newValue ->
    if index < List.len list then
        List.replaceUnsafe list index newValue
    else
        { list, value: newValue }

## Replaces the element at the given index with a replacement.
##
## >>> List.set ["a", "b", "c"] 1 "B"
##
## If the given index is outside the bounds of the list, returns the original
## list unmodified.
##
## To drop the element at a given index, instead of replacing it, see [List.dropAt].
set : List a, Nat, a -> List a
set = \list, index, value ->
    (List.replace list index value).list

## Add a single element to the end of a list.
##
## >>> List.append [1, 2, 3] 4
##
## >>> [0, 1, 2]
## >>>     |> List.append 3
append : List a, a -> List a
append = \list, element ->
    list
    |> List.reserve 1
    |> List.appendUnsafe element

## Writes the element after the current last element unconditionally.
## In other words, it is assumed that
##
## - the list is owned (i.e. can be updated in-place
## - the list has at least one element of spare capacity
appendUnsafe : List a, a -> List a

## Add a single element to the beginning of a list.
##
## >>> List.prepend [1, 2, 3] 0
##
## >>> [2, 3, 4]
## >>>     |> List.prepend 1
prepend : List a, a -> List a

## Returns the length of the list - the number of elements it contains.
##
## One [List] can store up to 2,147,483,648 elements (just over 2 billion), which
## is exactly equal to the highest valid #I32 value. This means the #U32 this function
## returns can always be safely converted to an #I32 without losing any data.
len : List a -> Nat

## Create a list with space for at least capacity elements
withCapacity : Nat -> List a

## Enlarge the list for at least capacity additional elements
reserve : List a, Nat -> List a

## Put two lists together.
##
## >>> List.concat [1, 2, 3] [4, 5]
##
## >>> [0, 1, 2]
## >>>     |> List.concat [3, 4]
concat : List a, List a -> List a

## Returns the last element in the list, or `ListWasEmpty` if it was empty.
last : List a -> Result a [ListWasEmpty]*
last = \list ->
    when List.get list (Num.subSaturated (List.len list) 1) is
        Ok v -> Ok v
        Err _ -> Err ListWasEmpty

## A list with a single element in it.
##
## This is useful in pipelines, like so:
##
##     websites =
##         Str.concat domain ".com"
##             |> List.single
##
single : a -> List a
single = \x -> [x]

## Returns a list with the given length, where every element is the given value.
##
##
repeat : a, Nat -> List a
repeat = \value, count ->
    repeatHelp value count (List.withCapacity count)

repeatHelp : a, Nat, List a -> List a
repeatHelp = \value, count, accum ->
    if count > 0 then
        repeatHelp value (count - 1) (List.appendUnsafe accum value)
    else
        accum

## Returns the list with its elements reversed.
##
## >>> List.reverse [1, 2, 3]
reverse : List a -> List a
reverse = \list ->
    reverseHelp list 0 (Num.subSaturated (List.len list) 1)

reverseHelp = \list, left, right ->
    if left < right then
        reverseHelp (List.swap list left right) (left + 1) (right - 1)
    else
        list

## Join the given lists together into one list.
##
## >>> List.join [[1, 2, 3], [4, 5], [], [6, 7]]
##
## >>> List.join [[], []]
##
## >>> List.join []
join : List (List a) -> List a
join = \lists ->
    totalLength =
        List.walk lists 0 (\state, list -> state + List.len list)

    List.walk lists (List.withCapacity totalLength) (\state, list -> List.concat state list)

contains : List a, a -> Bool
contains = \list, needle ->
    List.any list (\x -> x == needle)

## Build a value using each element in the list.
##
## Starting with a given `state` value, this walks through each element in the
## list from first to last, running a given `step` function on that element
## which updates the `state`. It returns the final `state` at the end.
##
## You can use it in a pipeline:
##
##     [2, 4, 8]
##         |> List.walk { start: 0, step: Num.add }
##
## This returns 14 because:
## * `state` starts at 0 (because of `start: 0`)
## * Each `step` runs `Num.add state elem`, and the return value becomes the new `state`.
##
## Here is a table of how `state` changes as [List.walk] walks over the elements
## `[2, 4, 8]` using #Num.add as its `step` function to determine the next `state`.
##
## `state` | `elem` | `step state elem` (`Num.add state elem`)
## --------+--------+-----------------------------------------
## 0       |        |
## 0       | 2      | 2
## 2       | 4      | 6
## 6       | 8      | 14
##
## So `state` goes through these changes:
## 1. `0` (because of `start: 0`)
## 2. `1` (because of `Num.add state elem` with `state` = 0 and `elem` = 1
##
##     [1, 2, 3]
##         |> List.walk { start: 0, step: Num.sub }
##
## This returns -6 because
##
## Note that in other languages, `walk` is sometimes called `reduce`,
## `fold`, `foldLeft`, or `foldl`.
walk : List elem, state, (state, elem -> state) -> state
walk = \list, state, func ->
    walkHelp : _, _ -> [Continue _, Break []]
    walkHelp = \currentState, element -> Continue (func currentState element)

    when List.iterate list state walkHelp is
        Continue newState -> newState

## Note that in other languages, `walkBackwards` is sometimes called `reduceRight`,
## `fold`, `foldRight`, or `foldr`.
walkBackwards : List elem, state, (state, elem -> state) -> state
walkBackwards = \list, state, func ->
    walkBackwardsHelp list state func (len list)

## internal helper
walkBackwardsHelp : List elem, state, (state, elem -> state), Nat -> state
walkBackwardsHelp = \list, state, f, indexPlusOne ->
    if indexPlusOne == 0 then
        state
    else
        index = indexPlusOne - 1
        nextState = f state (getUnsafe list index)

        walkBackwardsHelp list nextState f index

## Same as [List.walk], except you can stop walking early.
##
## ## Performance Details
##
## Compared to [List.walk], this can potentially visit fewer elements (which can
## improve performance) at the cost of making each step take longer.
## However, the added cost to each step is extremely small, and can easily
## be outweighed if it results in skipping even a small number of elements.
##
## As such, it is typically better for performance to use this over [List.walk]
## if returning `Break` earlier than the last element is expected to be common.
walkUntil : List elem, state, (state, elem -> [Continue state, Break state]) -> state
walkUntil = \list, initial, step ->
    when List.iterate list initial step is
        Continue new -> new
        Break new -> new

sum : List (Num a) -> Num a
sum = \list ->
    List.walk list 0 Num.add

product : List (Num a) -> Num a
product = \list ->
    List.walk list 1 Num.mul

## Run the given predicate on each element of the list, returning `True` if
## any of the elements satisfy it.
any : List a, (a -> Bool) -> Bool
any = \list, predicate ->
    looper = \{}, element ->
        if predicate element then
            Break {}
        else
            Continue {}

    when List.iterate list {} looper is
        Continue {} -> False
        Break {} -> True

## Run the given predicate on each element of the list, returning `True` if
## all of the elements satisfy it.
all : List a, (a -> Bool) -> Bool
all = \list, predicate ->
    looper = \{}, element ->
        if predicate element then
            Continue {}
        else
            Break {}

    when List.iterate list {} looper is
        Continue {} -> True
        Break {} -> False

## Run the given function on each element of a list, and return all the
## elements for which the function returned `True`.
##
## >>> List.keepIf [1, 2, 3, 4] (\num -> num > 2)
##
## ## Performance Details
##
## [List.keepIf] always returns a list that takes up exactly the same amount
## of memory as the original, even if its length decreases. This is because it
## can't know in advance exactly how much space it will need, and if it guesses a
## length that's too low, it would have to re-allocate.
##
## (If you want to do an operation like this which reduces the memory footprint
## of the resulting list, you can do two passes over the lis with [List.walk] - one
## to calculate the precise new size, and another to populate the new list.)
##
## If given a unique list, [List.keepIf] will mutate it in place to assemble the appropriate list.
## If that happens, this function will not allocate any new memory on the heap.
## If all elements in the list end up being kept, Roc will return the original
## list unaltered.
##
keepIf : List a, (a -> Bool) -> List a
keepIf = \list, predicate ->
    length = List.len list

    keepIfHelp list predicate 0 0 length

keepIfHelp : List a, (a -> Bool), Nat, Nat, Nat -> List a
keepIfHelp = \list, predicate, kept, index, length ->
    if index < length then
        if predicate (List.getUnsafe list index) then
            keepIfHelp (List.swap list kept index) predicate (kept + 1) (index + 1) length
        else
            keepIfHelp list predicate kept (index + 1) length
    else
        List.takeFirst list kept

## Run the given function on each element of a list, and return all the
## elements for which the function returned `False`.
##
## >>> List.dropIf [1, 2, 3, 4] (\num -> num > 2)
##
## ## Performance Details
##
## `List.dropIf` has the same performance characteristics as [List.keepIf].
## See its documentation for details on those characteristics!
dropIf : List a, (a -> Bool) -> List a
dropIf = \list, predicate ->
    List.keepIf list (\e -> Bool.not (predicate e))

## This works like [List.map], except only the transformed values that are
## wrapped in `Ok` are kept. Any that are wrapped in `Err` are dropped.
##
## >>> List.keepOks [["a", "b"], [], [], ["c", "d", "e"]] List.last
##
## >>> fn = \str -> if Str.isEmpty str then Err StrWasEmpty else Ok (Str.len str)
## >>>
## >>> List.keepOks ["", "a", "bc", "", "d", "ef", ""]
keepOks : List before, (before -> Result after *) -> List after
keepOks = \list, toResult ->
    walker = \accum, element ->
        when toResult element is
            Ok keep -> List.append accum keep
            Err _drop -> accum

    List.walk list (List.withCapacity (List.len list)) walker

## This works like [List.map], except only the transformed values that are
## wrapped in `Err` are kept. Any that are wrapped in `Ok` are dropped.
##
## >>> List.keepErrs [["a", "b"], [], [], ["c", "d", "e"]] List.last
##
## >>> fn = \str -> if Str.isEmpty str then Err StrWasEmpty else Ok (Str.len str)
## >>>
## >>> List.keepErrs ["", "a", "bc", "", "d", "ef", ""]
keepErrs : List before, (before -> Result * after) -> List after
keepErrs = \list, toResult ->
    walker = \accum, element ->
        when toResult element is
            Ok _drop -> accum
            Err keep -> List.append accum keep

    List.walk list (List.withCapacity (List.len list)) walker

## Convert each element in the list to something new, by calling a conversion
## function on each of them. Then return a new list of the converted values.
##
## > List.map [1, 2, 3] (\num -> num + 1)
##
## > List.map ["", "a", "bc"] Str.isEmpty
map : List a, (a -> b) -> List b

## Run a transformation function on the first element of each list,
## and use that as the first element in the returned list.
## Repeat until a list runs out of elements.
##
## Some languages have a function named `zip`, which does something similar to
## calling [List.map2] passing two lists and `Pair`:
##
## >>> zipped = List.map2 ["a", "b", "c"] [1, 2, 3] Pair
map2 : List a, List b, (a, b -> c) -> List c

## Run a transformation function on the first element of each list,
## and use that as the first element in the returned list.
## Repeat until a list runs out of elements.
map3 : List a, List b, List c, (a, b, c -> d) -> List d

## Run a transformation function on the first element of each list,
## and use that as the first element in the returned list.
## Repeat until a list runs out of elements.
map4 : List a, List b, List c, List d, (a, b, c, d -> e) -> List e

## This works like [List.map], except it also passes the index
## of the element to the conversion function.
mapWithIndex : List a, (a, Nat -> b) -> List b
mapWithIndex = \src, func ->
    length = len src
    dest = withCapacity length

    mapWithIndexHelp src dest func 0 length

# Internal helper
mapWithIndexHelp : List a, List b, (a, Nat -> b), Nat, Nat -> List b
mapWithIndexHelp = \src, dest, func, index, length ->
    if index < length then
        elem = getUnsafe src index
        mappedElem = func elem index
        newDest = List.appendUnsafe dest mappedElem

        mapWithIndexHelp src newDest func (index + 1) length
    else
        dest

## Returns a list of all the integers between one and another,
## including both of the given numbers.
##
## >>> List.range 2 8
range : Int a, Int a -> List (Int a)
range = \start, end ->
    when Num.compare start end is
        GT -> []
        EQ -> [start]
        LT ->
            length = Num.intCast (end - start)

            rangeHelp (List.withCapacity length) start end

rangeHelp : List (Int a), Int a, Int a -> List (Int a)
rangeHelp = \accum, start, end ->
    if end <= start then
        accum
    else
        rangeHelp (List.appendUnsafe accum start) (start + 1) end

## Sort with a custom comparison function
sortWith : List a, (a, a -> [LT, EQ, GT]) -> List a

## Sorts a list in ascending order (lowest to highest), using a function which
## specifies a way to represent each element as a number.
##
## To sort in descending order (highest to lowest), use [List.sortDesc] instead.
sortAsc : List (Num a) -> List (Num a)
sortAsc = \list -> List.sortWith list Num.compare

## Sorts a list in descending order (highest to lowest), using a function which
## specifies a way to represent each element as a number.
##
## To sort in ascending order (lowest to highest), use [List.sortAsc] instead.
sortDesc : List (Num a) -> List (Num a)
sortDesc = \list -> List.sortWith list (\a, b -> Num.compare b a)

swap : List a, Nat, Nat -> List a

## Returns the first element in the list, or `ListWasEmpty` if it was empty.
first : List a -> Result a [ListWasEmpty]*
first = \list ->
    when List.get list 0 is
        Ok v -> Ok v
        Err _ -> Err ListWasEmpty

## Remove the first element from the list.
##
## Returns the new list (with the removed element missing).
dropFirst : List elem -> List elem
dropFirst = \list ->
    List.dropAt list 0

## Remove the last element from the list.
##
## Returns the new list (with the removed element missing).
dropLast : List elem -> List elem
dropLast = \list ->
    List.dropAt list (Num.subSaturated (List.len list) 1)

## Returns the given number of elements from the beginning of the list.
##
## >>> List.takeFirst 4 [1, 2, 3, 4, 5, 6, 7, 8]
##
## If there are fewer elements in the list than the requested number,
## returns the entire list.
##
## >>> List.takeFirst 5 [1, 2]
##
## To *remove* elements from the beginning of the list, use `List.takeLast`.
##
## To remove elements from both the beginning and end of the list,
## use `List.sublist`.
##
## To split the list into two lists, use `List.split`.
##
## ## Performance Details
##
## When given a Unique list, this runs extremely fast. It sets the list's length
## to the given length value, and frees the leftover elements. This runs very
## slightly faster than `List.takeLast`.
##
## In fact, `List.takeFirst 1 list` runs faster than `List.first list` when given
## a Unique list, because [List.first] returns the first element as well -
## which introduces a conditional bounds check as well as a memory load.
takeFirst : List elem, Nat -> List elem
takeFirst = \list, outputLength ->
    List.sublist list { start: 0, len: outputLength }

## Returns the given number of elements from the end of the list.
##
## >>> List.takeLast 4 [1, 2, 3, 4, 5, 6, 7, 8]
##
## If there are fewer elements in the list than the requested number,
## returns the entire list.
##
## >>> List.takeLast 5 [1, 2]
##
## To *remove* elements from the end of the list, use `List.takeFirst`.
##
## To remove elements from both the beginning and end of the list,
## use `List.sublist`.
##
## To split the list into two lists, use `List.split`.
##
## ## Performance Details
##
## When given a Unique list, this runs extremely fast. It moves the list's
## pointer to the index at the given length value, updates its length,
## and frees the leftover elements. This runs very nearly as fast as
## `List.takeFirst` on a Unique list.
##
## In fact, `List.takeLast 1 list` runs faster than `List.first list` when given
## a Unique list, because [List.first] returns the first element as well -
## which introduces a conditional bounds check as well as a memory load.
takeLast : List elem, Nat -> List elem
takeLast = \list, outputLength ->
    List.sublist list { start: Num.subSaturated (List.len list) outputLength, len: outputLength }

## Drops n elements from the beginning of the list.
drop : List elem, Nat -> List elem
drop = \list, n ->
    remaining = Num.subSaturated (List.len list) n

    List.takeLast list remaining

## Drops the element at the given index from the list.
##
## This has no effect if the given index is outside the bounds of the list.
##
## To replace the element at a given index, instead of dropping it, see [List.set].
dropAt : List elem, Nat -> List elem

min : List (Num a) -> Result (Num a) [ListWasEmpty]*
min = \list ->
    when List.first list is
        Ok initial ->
            Ok (minHelp list initial)

        Err ListWasEmpty ->
            Err ListWasEmpty

minHelp : List (Num a), Num a -> Num a
minHelp = \list, initial ->
    List.walk list initial \bestSoFar, current ->
        if current < bestSoFar then
            current
        else
            bestSoFar

max : List (Num a) -> Result (Num a) [ListWasEmpty]*
max = \list ->
    when List.first list is
        Ok initial ->
            Ok (maxHelp list initial)

        Err ListWasEmpty ->
            Err ListWasEmpty

maxHelp : List (Num a), Num a -> Num a
maxHelp = \list, initial ->
    List.walk list initial \bestSoFar, current ->
        if current > bestSoFar then
            current
        else
            bestSoFar

## Like [List.map], except the transformation function wraps the return value
## in a list. At the end, all the lists get joined together into one list.
##
## You may know a similar function named `concatMap` in other languages.
joinMap : List a, (a -> List b) -> List b
joinMap = \list, mapper ->
    List.walk list [] \state, elem -> List.concat state (mapper elem)

## Returns the first element of the list satisfying a predicate function.
## If no satisfying element is found, an `Err NotFound` is returned.
findFirst : List elem, (elem -> Bool) -> Result elem [NotFound]*
findFirst = \list, pred ->
    callback = \_, elem ->
        if pred elem then
            Break elem
        else
            Continue {}

    when List.iterate list {} callback is
        Continue {} -> Err NotFound
        Break found -> Ok found

## Returns the last element of the list satisfying a predicate function.
## If no satisfying element is found, an `Err NotFound` is returned.
findLast : List elem, (elem -> Bool) -> Result elem [NotFound]*
findLast = \list, pred ->
    callback = \_, elem ->
        if pred elem then
            Break elem
        else
            Continue {}

    when List.iterateBackwards list {} callback is
        Continue {} -> Err NotFound
        Break found -> Ok found

## Returns the index at which the first element in the list
## satisfying a predicate function can be found.
## If no satisfying element is found, an `Err NotFound` is returned.
findFirstIndex : List elem, (elem -> Bool) -> Result Nat [NotFound]*
findFirstIndex = \list, matcher ->
    foundIndex = List.iterate list 0 \index, elem ->
        if matcher elem then
            Break index
        else
            Continue (index + 1)

    when foundIndex is
        Break index -> Ok index
        Continue _ -> Err NotFound

## Returns the last index at which the first element in the list
## satisfying a predicate function can be found.
## If no satisfying element is found, an `Err NotFound` is returned.
findLastIndex : List elem, (elem -> Bool) -> Result Nat [NotFound]*
findLastIndex = \list, matches ->
    foundIndex = List.iterateBackwards list (List.len list) \prevIndex, elem ->
        if matches elem then
            Break (prevIndex - 1)
        else
            Continue (prevIndex - 1)

    when foundIndex is
        Break index -> Ok index
        Continue _ -> Err NotFound

## Returns a subsection of the given list, beginning at the `start` index and
## including a total of `len` elements.
##
## If `start` is outside the bounds of the given list, returns the empty list.
##
## >>> List.sublist [1, 2, 3] { start: 4, len: 0 }
##
## If more elements are requested than exist in the list, returns as many as it can.
##
## >>> List.sublist [1, 2, 3, 4, 5] { start: 2, len: 10 }
##
## > If you want a sublist which goes all the way to the end of the list, no
## > matter how long the list is, `List.takeLast` can do that more efficiently.
##
## Some languages have a function called **`slice`** which works similarly to this.
sublist : List elem, { start : Nat, len : Nat } -> List elem
sublist = \list, config ->
    if config.len == 0 then
        []
    else
        sublistLowlevel list config.start config.len

## low-level slicing operation that does no bounds checking
sublistLowlevel : List elem, Nat, Nat -> List elem

## Intersperses `sep` between the elements of `list`
## >>> List.intersperse 9 [1, 2, 3]     # [1, 9, 2, 9, 3]
intersperse : List elem, elem -> List elem
intersperse = \list, sep ->
    capacity = 2 * List.len list
    init = List.withCapacity capacity
    newList =
        List.walk list init \acc, elem ->
            acc
            |> List.appendUnsafe elem
            |> List.appendUnsafe sep

    List.dropLast newList

## Returns `True` if the first list starts with the second list.
##
## If the second list is empty, this always returns `True`; every list
## is considered to "start with" an empty list.
##
## If the first list is empty, this only returns `True` if the second list is empty.
startsWith : List elem, List elem -> Bool
startsWith = \list, prefix ->
    # TODO once we have seamless slices, verify that this wouldn't
    # have better performance with a function like List.compareSublists
    prefix == List.sublist list { start: 0, len: List.len prefix }

## Returns `True` if the first list ends with the second list.
##
## If the second list is empty, this always returns `True`; every list
## is considered to "end with" an empty list.
##
## If the first list is empty, this only returns `True` if the second list is empty.
endsWith : List elem, List elem -> Bool
endsWith = \list, suffix ->
    # TODO once we have seamless slices, verify that this wouldn't
    # have better performance with a function like List.compareSublists
    length = List.len suffix
    start = Num.subSaturated (List.len list) length

    suffix == List.sublist list { start, len: length }

## Splits the list into two lists, around the given index.
##
## The returned lists are labeled `before` and `others`. The `before` list will
## contain all the elements whose index in the original list was **less than**
## than the given index, # and the `others` list will be all the others. (This
## means if you give an index of 0, the `before` list will be empty and the
## `others` list will have the same elements as the original list.)
split : List elem, Nat -> { before : List elem, others : List elem }
split = \elements, userSplitIndex ->
    length = List.len elements
    splitIndex = if length > userSplitIndex then userSplitIndex else length
    before = List.sublist elements { start: 0, len: splitIndex }
    others = List.sublist elements { start: splitIndex, len: length - splitIndex }

    { before, others }

## Returns the elements before the first occurrence of a delimiter, as well as the
## remaining elements after that occurrence. If the delimiter is not found, returns `Err`.
##
##     List.splitFirst [Foo, Z, Bar, Z, Baz] Z == Ok { before: [Foo], after: [Bar, Baz] }
splitFirst : List elem, elem -> Result { before : List elem, after : List elem } [NotFound]*
splitFirst = \list, delimiter ->
    when List.findFirstIndex list (\elem -> elem == delimiter) is
        Ok index ->
            before = List.sublist list { start: 0, len: index }
            after = List.sublist list { start: index + 1, len: List.len list - index - 1 }

            Ok { before, after }

        Err NotFound -> Err NotFound

## Returns the elements before the last occurrence of a delimiter, as well as the
## remaining elements after that occurrence. If the delimiter is not found, returns `Err`.
##
##     List.splitLast [Foo, Z, Bar, Z, Baz] Z == Ok { before: [Foo, Bar], after: [Baz] }
splitLast : List elem, elem -> Result { before : List elem, after : List elem } [NotFound]*
splitLast = \list, delimiter ->
    when List.findLastIndex list (\elem -> elem == delimiter) is
        Ok index ->
            before = List.sublist list { start: 0, len: index }
            after = List.sublist list { start: index + 1, len: List.len list - index - 1 }

            Ok { before, after }

        Err NotFound -> Err NotFound

## Like [List.map], except the transformation function returns a [Result].
## If that function ever returns `Err`, [mapTry] immediately returns that `Err`.
## If it returns `Ok` for every element, [mapTry] returns `Ok` with the transformed list.
mapTry : List elem, (elem -> Result ok err) -> Result (List ok) err
mapTry = \list, toResult ->
    walkTry list [] \state, elem ->
        Result.map (toResult elem) \ok ->
            List.append state ok

## This is the same as `iterate` but with Result instead of [Continue, Break].
## Using `Result` saves a conditional in `mapTry`.
## It might be useful to expose this in userspace?
walkTry : List elem, state, (state, elem -> Result state err) -> Result state err
walkTry = \list, init, func ->
    walkTryHelp list init func 0 (List.len list)

## internal helper
walkTryHelp : List elem, state, (state, elem -> Result state err), Nat, Nat -> Result state err
walkTryHelp = \list, state, f, index, length ->
    if index < length then
        when f state (List.getUnsafe list index) is
            Ok nextState -> walkTryHelp list nextState f (index + 1) length
            Err b -> Err b
    else
        Ok state

## Primitive for iterating over a List, being able to decide at every element whether to continue
iterate : List elem, s, (s, elem -> [Continue s, Break b]) -> [Continue s, Break b]
iterate = \list, init, func ->
    iterHelp list init func 0 (List.len list)

## internal helper
iterHelp : List elem, s, (s, elem -> [Continue s, Break b]), Nat, Nat -> [Continue s, Break b]
iterHelp = \list, state, f, index, length ->
    if index < length then
        when f state (List.getUnsafe list index) is
            Continue nextState -> iterHelp list nextState f (index + 1) length
            Break b -> Break b
    else
        Continue state

## Primitive for iterating over a List from back to front, being able to decide at every
## element whether to continue
iterateBackwards : List elem, s, (s, elem -> [Continue s, Break b]) -> [Continue s, Break b]
iterateBackwards = \list, init, func ->
    iterBackwardsHelp list init func (List.len list)

## internal helper
iterBackwardsHelp : List elem, s, (s, elem -> [Continue s, Break b]), Nat -> [Continue s, Break b]
iterBackwardsHelp = \list, state, f, prevIndex ->
    if prevIndex > 0 then
        index = prevIndex - 1

        when f state (List.getUnsafe list index) is
            Continue nextState -> iterBackwardsHelp list nextState f index
            Break b -> Break b
    else
        Continue state
