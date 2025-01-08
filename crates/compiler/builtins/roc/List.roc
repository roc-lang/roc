module [
    isEmpty,
    get,
    set,
    replace,
    update,
    append,
    appendIfOk,
    prepend,
    prependIfOk,
    map,
    len,
    withCapacity,
    walkBackwards,
    concat,
    first,
    single,
    repeat,
    reverse,
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
    walkWithIndex,
    walkUntil,
    walkWithIndexUntil,
    walkFrom,
    walkFromUntil,
    range,
    sortWith,
    swap,
    dropAt,
    min,
    max,
    map4,
    mapTry,
    walkTry,
    joinMap,
    any,
    takeFirst,
    takeLast,
    dropFirst,
    dropLast,
    findFirst,
    findLast,
    findFirstIndex,
    findLastIndex,
    sublist,
    intersperse,
    splitAt,
    splitOn,
    splitOnList,
    splitFirst,
    splitLast,
    startsWith,
    endsWith,
    all,
    dropIf,
    sortAsc,
    sortDesc,
    reserve,
    releaseExcessCapacity,
    walkBackwardsUntil,
    countIf,
    chunksOf,
    concatUtf8,
    forEach!,
    forEachTry!,
    walk!,
]

import Bool exposing [Bool, Eq]
import Result exposing [Result]
import Num exposing [U64, Num, U8]

## ## Types
##
## A sequential list of values.
## ```roc
## [1, 2, 3] # a list of numbers
## ["a", "b", "c"] # a list of strings
## [[1.1], [], [2.2, 3.3]] # a list of lists of numbers
## ```
## The maximum size of a [List] is limited by the amount of heap memory available
## to the current process. If there is not enough memory available, attempting to
## create the list could crash. (On Linux, where [overcommit](https://www.etalabs.net/overcommit.html)
## is normally enabled, not having enough memory could result in the list appearing
## to be created just fine, but then crashing later.)
##
## > The theoretical maximum length for a list created in Roc is `Num.maxI32` on 32-bit systems
## > and `Num.maxI64` on 64-bit systems. Attempting to create a list bigger than that
## > in Roc code will always fail, although in practice it is likely to fail
## > at much smaller lengths due to insufficient memory being available.
##
## ## Performance Details
##
## Under the hood, a list is a record containing a `len : U64` field, a `capacity : U64`
## field, and a pointer to a reference count and a flat array of bytes.
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
## ```roc
## ratings = [5, 4, 3]
##
## { foo: ratings, bar: ratings }
## ```
## The first line binds the name `ratings` to the list `[5, 4, 3]`. The list
## begins with a refcount of 1, because so far only `ratings` is referencing it.
##
## The second line alters this refcount. `{ foo: ratings` references
## the `ratings` list, and so does `bar: ratings }`. This will result in its
## refcount getting incremented from 1 to 3.
##
## Let's turn this example into a function.
## ```roc
## getRatings = \first ->
##     ratings = [first, 4, 3]
##
##     { foo: ratings, bar: ratings }
##
## getRatings 5
## ```
## At the end of the `getRatings` function, when the record gets returned,
## the original `ratings =` binding has gone out of scope and is no longer
## accessible. (Trying to reference `ratings` outside the scope of the
## `getRatings` function would be an error!)
##
## Since `ratings` represented a way to reference the list, and that way is no
## longer accessible, the list's refcount gets decremented when `ratings` goes
## out of scope. It will decrease from 3 back down to 2.
##
## Putting these together, when we call `getRatings 5`, what we get back is
## a record with two fields, `foo`, and `bar`, each of which refers to the same
## list, and that list has a refcount of 2.
##
## Let's change the last line to be `(getRatings 5).bar` instead of `getRatings 5`:
## ```roc
## getRatings = \first ->
##     ratings = [first, 4, 3]
##
##     { foo: ratings, bar: ratings }
##
## (getRatings 5).bar
## ```
## Now, when this expression returns, only the `bar` field of the record will
## be returned. This will mean that the `foo` field becomes inaccessible, causing
## the list's refcount to get decremented from 2 to 1. At this point, the list is back
## where it started: there is only 1 reference to it.
##
## Finally let's suppose the final line were changed to this:
## ```roc
## List.first (getRatings 5).bar
## ```
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
## ```roc
## nums = [1, 2, 3, 4, 5, 6, 7]
##
## first = List.first nums
## last = List.last nums
##
## first
## ```
## It makes a list, calls [List.first] and [List.last] on it, and then returns `first`.
##
## Here's the equivalent code with a list of lists:
## ```roc
## lists = [[1], [2, 3], [], [4, 5, 6, 7]]
##
## first = List.first lists
## last = List.last lists
##
## first
## ```
## **TODO** explain how in the former example, when we go to free `nums` at the end,
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

# separator so List.isEmpty doesn't absorb the above into its doc comment

##  Check if the list is empty.
## ```roc
## List.isEmpty [1, 2, 3]
##
## List.isEmpty []
## ```
isEmpty : List * -> Bool
isEmpty = \list ->
    List.len list == 0

# unsafe primitive that does not perform a bounds check
# but will cause a reference count increment on the value it got out of the list
getUnsafe : List a, U64 -> a

## Returns an element from a list at the given index.
##
## Returns `Err OutOfBounds` if the given index exceeds the List's length
## ```roc
## expect List.get [100, 200, 300] 1 == Ok 200
## expect List.get [100, 200, 300] 5 == Err OutOfBounds
## ```
get : List a, U64 -> Result a [OutOfBounds]
get = \list, index ->
    if index < List.len list then
        Ok (List.getUnsafe list index)
    else
        Err OutOfBounds

# unsafe primitive that does not perform a bounds check
# but will cause a reference count increment on the value it got out of the list
replaceUnsafe : List a, U64, a -> { list : List a, value : a }

replace : List a, U64, a -> { list : List a, value : a }
replace = \list, index, newValue ->
    if index < List.len list then
        List.replaceUnsafe list index newValue
    else
        { list, value: newValue }

## Replaces the element at the given index with a replacement.
## ```roc
## List.set ["a", "b", "c"] 1 "B"
## ```
## If the given index is outside the bounds of the list, returns the original
## list unmodified.
##
## To drop the element at a given index, instead of replacing it, see [List.dropAt].
set : List a, U64, a -> List a
set = \list, index, value ->
    (List.replace list index value).list

## Updates the element at the given index with the given function.
## ```roc
## List.update [1, 2, 3] 1 (\x -> x + 1)
## ```
## If the given index is outside the bounds of the list, returns the original
## list unmodified.
##
## To replace the element at a given index, instead of updating based on the current value,
## see [List.set] and [List.replace]
update : List a, U64, (a -> a) -> List a
update = \list, index, func ->
    when List.get list index is
        Err OutOfBounds -> list
        Ok value ->
            newValue = func value
            (replaceUnsafe list index newValue).list

# Update one element in bounds
expect
    list : List U64
    list = [1, 2, 3]
    got = update list 1 (\x -> x + 42)
    want = [1, 44, 3]
    got == want

# Update out of bounds
expect
    list : List U64
    list = [1, 2, 3]
    got = update list 5 (\x -> x + 42)
    got == list

# Update chain
expect
    list : List U64
    list = [1, 2, 3]
    got =
        list
        |> update 0 (\x -> x + 10)
        |> update 1 (\x -> x + 20)
        |> update 2 (\x -> x + 30)
    want = [11, 22, 33]
    got == want

## Add a single element to the end of a list.
## ```roc
## List.append [1, 2, 3] 4
##
## [0, 1, 2]
##     |> List.append 3
## ```
append : List a, a -> List a
append = \list, element ->
    list
    |> List.reserve 1
    |> List.appendUnsafe element

## If the given [Result] is `Ok`, add it to the end of a list.
## Otherwise, return the list unmodified.
##
## ```roc
## List.appendIfOk [1, 2, 3] (Ok 4)
##
## [0, 1, 2]
##     |> List.appendIfOk (Err 3)
## ```
appendIfOk : List a, Result a * -> List a
appendIfOk = \list, result ->
    when result is
        Ok elem -> append list elem
        Err _ -> list

## Writes the element after the current last element unconditionally.
## In other words, it is assumed that
##
## - the list is owned (i.e. can be updated in-place
## - the list has at least one element of spare capacity
appendUnsafe : List a, a -> List a

## Add a single element to the beginning of a list.
## ```roc
## List.prepend [1, 2, 3] 0
##
## [2, 3, 4]
##     |> List.prepend 1
## ```
prepend : List a, a -> List a

## If the given [Result] is `Ok`, add it to the beginning of a list.
## Otherwise, return the list unmodified.
##
## ```roc
## List.prepend [1, 2, 3] (Ok 0)
##
## [2, 3, 4]
##     |> List.prepend (Err 1)
## ```
prependIfOk : List a, Result a * -> List a
prependIfOk = \list, result ->
    when result is
        Ok elem -> prepend list elem
        Err _ -> list

## Returns the length of the list - the number of elements it contains.
##
## One [List] can store up to `Num.maxI64` elements on 64-bit targets and `Num.maxI32` on 32-bit targets like wasm.
## This means the #U64 this function returns can always be safely converted to #I64 or #I32, depending on the target.
len : List * -> U64

## Create a list with space for at least capacity elements
withCapacity : U64 -> List *

## Enlarge the list for at least capacity additional elements
reserve : List a, U64 -> List a

## Shrink the memory footprint of a list such that it's capacity and length are equal.
## Note: This will also convert seamless slices to regular lists.
releaseExcessCapacity : List a -> List a

## Put two lists together.
## ```roc
## List.concat [1, 2, 3] [4, 5]
##
## [0, 1, 2]
##     |> List.concat [3, 4]
## ```
concat : List a, List a -> List a

## Returns the last element in the list, or `ListWasEmpty` if it was empty.
## ```roc
## expect List.last [1, 2, 3] == Ok 3
## expect List.last [] == Err ListWasEmpty
## ```
last : List a -> Result a [ListWasEmpty]
last = \list ->
    when List.get list (Num.subSaturated (List.len list) 1) is
        Ok v -> Ok v
        Err _ -> Err ListWasEmpty

## A list with a single element in it.
##
## This is useful in pipelines, like so:
## ```roc
## websites =
##     Str.concat domain ".com"
##         |> List.single
## ```
single : a -> List a
single = \x -> [x]

## Returns a list with the given length, where every element is the given value.
repeat : a, U64 -> List a
repeat = \value, count ->
    repeatHelp value count (List.withCapacity count)

repeatHelp : a, U64, List a -> List a
repeatHelp = \value, count, accum ->
    if count > 0 then
        repeatHelp value (Num.subWrap count 1) (List.appendUnsafe accum value)
    else
        accum

## Returns the list with its elements reversed.
## ```roc
## expect List.reverse [1, 2, 3] == [3, 2, 1]
## ```
reverse : List a -> List a
reverse = \list ->
    end = List.len list |> Num.subSaturated 1
    reverseHelp (List.clone list) 0 end

reverseHelp = \list, left, right ->
    if left < right then
        reverseHelp (List.swap list left right) (Num.addWrap left 1) (Num.subWrap right 1)
    else
        list

# Ensures that the list in unique (will re-use if already unique)
clone : List a -> List a

## Join the given lists together into one list.
## ```roc
## expect List.join [[1], [2, 3], [], [4, 5]] == [1, 2, 3, 4, 5]
## expect List.join [[], []] == []
## expect List.join [] == []
## ```
join : List (List a) -> List a
join = \lists ->
    totalLength =
        List.walk lists 0 (\state, list -> Num.addWrap state (List.len list))

    List.walk lists (List.withCapacity totalLength) \state, list -> List.concat state list

contains : List a, a -> Bool where a implements Eq
contains = \list, needle ->
    List.any list (\x -> x == needle)

## Build a value using each element in the list.
##
## Starting with a given `state` value, this walks through each element in the
## list from first to last, running a given `step` function on that element
## which updates the `state`. It returns the final `state` at the end.
##
## You can use it in a pipeline:
## ```roc
## [2, 4, 8]
##     |> List.walk 0 Num.add
## ```
## This returns 14 because:
## * `state` starts at 0
## * Each `step` runs `Num.add state elem`, and the return value becomes the new `state`.
##
## Here is a table of how `state` changes as [List.walk] walks over the elements
## `[2, 4, 8]` using [Num.add] as its `step` function to determine the next `state`.
##
## state | elem  | Num.add state elem
## :---: | :---: | :----------------:
## 0     |       |
## 0     | 2     | 2
## 2     | 4     | 6
## 6     | 8     | 14
##
## The following returns -6:
## ```roc
## [1, 2, 3]
##     |> List.walk 0 Num.sub
## ```
## Note that in other languages, `walk` is sometimes called `reduce`,
## `fold`, `foldLeft`, or `foldl`.
walk : List elem, state, (state, elem -> state) -> state
walk = \list, init, func ->
    walkHelp list init func 0 (List.len list)

## internal helper
walkHelp : List elem, s, (s, elem -> s), U64, U64 -> s
walkHelp = \list, state, f, index, length ->
    if index < length then
        nextState = f state (List.getUnsafe list index)

        walkHelp list nextState f (Num.addWrap index 1) length
    else
        state

## Like [walk], but at each step the function also receives the index of the current element.
walkWithIndex : List elem, state, (state, elem, U64 -> state) -> state
walkWithIndex = \list, init, func ->
    walkWithIndexHelp list init func 0 (List.len list)

## internal helper
walkWithIndexHelp : List elem, s, (s, elem, U64 -> s), U64, U64 -> s
walkWithIndexHelp = \list, state, f, index, length ->
    if index < length then
        nextState = f state (List.getUnsafe list index) index

        walkWithIndexHelp list nextState f (Num.addWrap index 1) length
    else
        state

## Like [walkUntil], but at each step the function also receives the index of the current element.
walkWithIndexUntil : List elem, state, (state, elem, U64 -> [Continue state, Break state]) -> state
walkWithIndexUntil = \list, state, f ->
    when walkWithIndexUntilHelp list state f 0 (List.len list) is
        Continue new -> new
        Break new -> new

## internal helper
walkWithIndexUntilHelp : List elem, s, (s, elem, U64 -> [Continue s, Break b]), U64, U64 -> [Continue s, Break b]
walkWithIndexUntilHelp = \list, state, f, index, length ->
    if index < length then
        when f state (List.getUnsafe list index) index is
            Continue nextState ->
                walkWithIndexUntilHelp list nextState f (Num.addWrap index 1) length

            Break b -> Break b
    else
        Continue state

## Note that in other languages, `walkBackwards` is sometimes called `reduceRight`,
## `fold`, `foldRight`, or `foldr`.
walkBackwards : List elem, state, (state, elem -> state) -> state
walkBackwards = \list, state, func ->
    walkBackwardsHelp list state func (len list)

## internal helper
walkBackwardsHelp : List elem, state, (state, elem -> state), U64 -> state
walkBackwardsHelp = \list, state, f, indexPlusOne ->
    if indexPlusOne == 0 then
        state
    else
        index = Num.subWrap indexPlusOne 1
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

## Same as [List.walkUntil], but does it from the end of the list instead.
walkBackwardsUntil : List elem, state, (state, elem -> [Continue state, Break state]) -> state
walkBackwardsUntil = \list, initial, func ->
    when List.iterateBackwards list initial func is
        Continue new -> new
        Break new -> new

## Walks to the end of the list from a specified starting index
walkFrom : List elem, U64, state, (state, elem -> state) -> state
walkFrom = \list, index, state, func ->
    step : _, _ -> [Continue _, Break []]
    step = \currentState, element -> Continue (func currentState element)

    when List.iterHelp list state step index (List.len list) is
        Continue new -> new

## A combination of [List.walkFrom] and [List.walkUntil]
walkFromUntil : List elem, U64, state, (state, elem -> [Continue state, Break state]) -> state
walkFromUntil = \list, index, state, func ->
    when List.iterHelp list state func index (List.len list) is
        Continue new -> new
        Break new -> new

sum : List (Num a) -> Num a
sum = \list ->
    List.walk list 0 Num.add

product : List (Num a) -> Num a
product = \list ->
    List.walk list 1 Num.mul

## Run the given predicate on each element of the list, returning `Bool.true` if
## any of the elements satisfy it.
any : List a, (a -> Bool) -> Bool
any = \list, predicate ->
    looper = \{}, element ->
        if predicate element then
            Break {}
        else
            Continue {}

    when List.iterate list {} looper is
        Continue {} -> Bool.false
        Break {} -> Bool.true

## Run the given predicate on each element of the list, returning `Bool.true` if
## all of the elements satisfy it.
all : List a, (a -> Bool) -> Bool
all = \list, predicate ->
    looper = \{}, element ->
        if predicate element then
            Continue {}
        else
            Break {}

    when List.iterate list {} looper is
        Continue {} -> Bool.true
        Break {} -> Bool.false

## Run the given function on each element of a list, and return all the
## elements for which the function returned `Bool.true`.
## ```roc
## List.keepIf [1, 2, 3, 4] (\num -> num > 2)
## ```
## ## Performance Details
##
## [List.keepIf] always returns a list that takes up exactly the same amount
## of memory as the original, even if its length decreases. This is because it
## can't know in advance exactly how much space it will need, and if it guesses a
## length that's too low, it would have to re-allocate.
##
## (If you want to do an operation like this which reduces the memory footprint
## of the resulting list, you can do two passes over the list with [List.walk] - one
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

keepIfHelp : List a, (a -> Bool), U64, U64, U64 -> List a
keepIfHelp = \list, predicate, kept, index, length ->
    if index < length then
        if predicate (List.getUnsafe list index) then
            keepIfHelp (List.swap list kept index) predicate (Num.addWrap kept 1) (Num.addWrap index 1) length
        else
            keepIfHelp list predicate kept (Num.addWrap index 1) length
    else
        List.takeFirst list kept

## Run the given function on each element of a list, and return all the
## elements for which the function returned `Bool.false`.
## ```roc
## List.dropIf [1, 2, 3, 4] (\num -> num > 2)
## ```
## ## Performance Details
##
## `List.dropIf` has the same performance characteristics as [List.keepIf].
## See its documentation for details on those characteristics!
dropIf : List a, (a -> Bool) -> List a
dropIf = \list, predicate ->
    List.keepIf list (\e -> Bool.not (predicate e))

## Run the given function on each element of a list, and return the
## number of elements for which the function returned `Bool.true`.
## ```roc
## expect List.countIf [1, -2, -3] Num.isNegative == 2
## expect List.countIf [1, 2, 3] (\num -> num > 1 ) == 2
## ```
countIf : List a, (a -> Bool) -> U64
countIf = \list, predicate ->
    walkState = \state, elem ->
        if predicate elem then
            Num.addWrap state 1
        else
            state

    List.walk list 0 walkState

## This works like [List.map], except only the transformed values that are
## wrapped in `Ok` are kept. Any that are wrapped in `Err` are dropped.
## ```roc
## expect List.keepOks ["1", "Two", "23", "Bird"] Str.toI32 == [1, 23]
##
## expect List.keepOks [["a", "b"], [], ["c", "d", "e"], [] ] List.first == ["a", "c"]
##
## fn = \str -> if Str.isEmpty str then Err StrWasEmpty else Ok str
## expect List.keepOks ["", "a", "bc", "", "d", "ef", ""] fn == ["a", "bc", "d", "ef"]
## ```
keepOks : List before, (before -> Result after *) -> List after
keepOks = \list, toResult ->
    walker = \accum, element ->
        when toResult element is
            Ok keep -> List.append accum keep
            Err _drop -> accum

    List.walk list (List.withCapacity (List.len list)) walker

## This works like [List.map], except only the transformed values that are
## wrapped in `Err` are kept. Any that are wrapped in `Ok` are dropped.
## ```roc
## List.keepErrs [["a", "b"], [], [], ["c", "d", "e"]] List.last
##
## fn = \str -> if Str.isEmpty str then Err StrWasEmpty else Ok (Str.len str)
##
## List.keepErrs ["", "a", "bc", "", "d", "ef", ""]
## ```
keepErrs : List before, (before -> Result * after) -> List after
keepErrs = \list, toResult ->
    walker = \accum, element ->
        when toResult element is
            Ok _drop -> accum
            Err keep -> List.append accum keep

    List.walk list (List.withCapacity (List.len list)) walker

## Convert each element in the list to something new, by calling a conversion
## function on each of them. Then return a new list of the converted values.
## ```roc
## expect List.map [1, 2, 3] (\num -> num + 1) == [2, 3, 4]
##
## expect List.map ["", "a", "bc"] Str.isEmpty == [Bool.true, Bool.false, Bool.false]
## ```
map : List a, (a -> b) -> List b
map = \list, mapper ->
    # TODO: allow checking the refcounting and running the map inplace.
    # Preferably allow it even if the types are different (must be same size with padding though).
    length = List.len list
    List.walk
        list
        (List.withCapacity length)
        \state, elem ->
            List.appendUnsafe state (mapper elem)

## Run a transformation function on the first element of each list,
## and use that as the first element in the returned list.
## Repeat until a list runs out of elements.
##
## Some languages have a function named `zip`, which does something similar to
## calling [List.map2] passing two lists and `Pair`:
## ```roc
## zipped = List.map2 ["a", "b", "c"] [1, 2, 3] Pair
## ```
map2 : List a, List b, (a, b -> c) -> List c
map2 = \listA, listB, mapper ->
    length = Num.min (List.len listA) (List.len listB)
    map2Help listA listB (List.withCapacity length) mapper 0 length

map2Help : List a, List b, List c, (a, b -> c), U64, U64 -> List c
map2Help = \listA, listB, out, mapper, index, length ->
    if index < length then
        mapped = mapper (List.getUnsafe listA index) (List.getUnsafe listB index)

        map2Help listA listB (List.appendUnsafe out mapped) mapper (Num.addWrap index 1) length
    else
        out

## Run a transformation function on the first element of each list,
## and use that as the first element in the returned list.
## Repeat until a list runs out of elements.
map3 : List a, List b, List c, (a, b, c -> d) -> List d
map3 = \listA, listB, listC, mapper ->
    length = Num.min
        (Num.min (List.len listA) (List.len listB))
        (List.len listC)
    map3Help listA listB listC (List.withCapacity length) mapper 0 length

map3Help : List a, List b, List c, List d, (a, b, c -> d), U64, U64 -> List d
map3Help = \listA, listB, listC, out, mapper, index, length ->
    if index < length then
        mapped = mapper (List.getUnsafe listA index) (List.getUnsafe listB index) (List.getUnsafe listC index)

        map3Help listA listB listC (List.appendUnsafe out mapped) mapper (Num.addWrap index 1) length
    else
        out

## Run a transformation function on the first element of each list,
## and use that as the first element in the returned list.
## Repeat until a list runs out of elements.
map4 : List a, List b, List c, List d, (a, b, c, d -> e) -> List e
map4 = \listA, listB, listC, listD, mapper ->
    length = Num.min
        (Num.min (List.len listA) (List.len listB))
        (Num.min (List.len listC) (List.len listD))
    map4Help listA listB listC listD (List.withCapacity length) mapper 0 length

map4Help : List a, List b, List c, List d, List e, (a, b, c, d -> e), U64, U64 -> List e
map4Help = \listA, listB, listC, listD, out, mapper, index, length ->
    if index < length then
        mapped = mapper (List.getUnsafe listA index) (List.getUnsafe listB index) (List.getUnsafe listC index) (List.getUnsafe listD index)

        map4Help listA listB listC listD (List.append out mapped) mapper (Num.addWrap index 1) length
    else
        out

## This works like [List.map], except it also passes the index
## of the element to the conversion function.
## ```roc
## expect List.mapWithIndex [10, 20, 30] (\num, index -> num + index) == [10, 21, 32]
## ```
mapWithIndex : List a, (a, U64 -> b) -> List b
mapWithIndex = \src, func ->
    length = len src
    dest = withCapacity length

    mapWithIndexHelp src dest func 0 length

# Internal helper
mapWithIndexHelp : List a, List b, (a, U64 -> b), U64, U64 -> List b
mapWithIndexHelp = \src, dest, func, index, length ->
    if index < length then
        elem = getUnsafe src index
        mappedElem = func elem index
        newDest = List.appendUnsafe dest mappedElem

        mapWithIndexHelp src newDest func (Num.addWrap index 1) length
    else
        dest

## Returns a list of all the integers between `start` and `end`.
##
## To include the `start` and `end` integers themselves, use `At` like so:
## ```roc
## List.range { start: At 2, end: At 5 } # returns [2, 3, 4, 5]
## ```
## To exclude them, use `After` and `Before`, like so:
## ```roc
## List.range { start: After 2, end: Before 5 } # returns [3, 4]
## ```
## You can have the list end at a certain length rather than a certain integer:
## ```roc
## List.range { start: At 6, end: Length 4 } # returns [6, 7, 8, 9]
## ```
## If `step` is specified, each integer increases by that much. (`step: 1` is the default.)
## ```roc
## List.range { start: After 0, end: Before 9, step: 3 } # returns [3, 6]
## ```
## List.range will also generate a reversed list if step is negative or end comes before start:
## ```roc
## List.range { start: At 5, end: At 2 } # returns [5, 4, 3, 2]
## ```
## All of these options are compatible with the others. For example, you can use `At` or `After`
## with `start` regardless of what `end` and `step` are set to.
range : _
range = \{ start, end, step ?? 0 } ->
    { calcNext, stepIsPositive } =
        if step == 0 then
            when T start end is
                T (At x) (At y) | T (At x) (Before y) | T (After x) (At y) | T (After x) (Before y) ->
                    if x < y then
                        {
                            calcNext: \i -> Num.addChecked i 1,
                            stepIsPositive: Bool.true,
                        }
                    else
                        {
                            calcNext: \i -> Num.subChecked i 1,
                            stepIsPositive: Bool.false,
                        }

                T (At _) (Length _) | T (After _) (Length _) ->
                    {
                        calcNext: \i -> Num.addChecked i 1,
                        stepIsPositive: Bool.true,
                    }
        else
            {
                calcNext: \i -> Num.addChecked i step,
                stepIsPositive: step > 0,
            }

    inclusiveStart =
        when start is
            At x -> Ok x
            After x -> calcNext x

    when end is
        At at ->
            isValid =
                if stepIsPositive then
                    \i -> i <= at
                else
                    \i -> i >= at

            # TODO: switch to List.withCapacity
            rangeHelp [] inclusiveStart calcNext isValid

        Before before ->
            isValid =
                if stepIsPositive then
                    \i -> i < before
                else
                    \i -> i > before

            # TODO: switch to List.withCapacity
            rangeHelp [] inclusiveStart calcNext isValid

        Length l ->
            rangeLengthHelp (List.withCapacity l) inclusiveStart l calcNext

rangeHelp = \accum, i, calcNext, isValid ->
    when i is
        Ok val ->
            if isValid val then
                # TODO: change this to List.appendUnsafe once capacity is set correctly
                rangeHelp (List.append accum val) (calcNext val) calcNext isValid
            else
                accum

        Err _ ->
            # We went past the end of the numeric range and there is no next.
            # return the generated list.
            accum

rangeLengthHelp = \accum, i, remaining, calcNext ->
    if remaining == 0 then
        accum
    else
        when i is
            Ok val ->
                rangeLengthHelp (List.appendUnsafe accum val) (calcNext val) (Num.subWrap remaining 1) calcNext

            Err _ ->
                # We went past the end of the numeric range and there is no next.
                # The list is not the correct length yet, so we must crash.
                crash "List.range: failed to generate enough elements to fill the range before overflowing the numeric type"

expect
    List.range { start: At 0, end: At 4 } == [0, 1, 2, 3, 4]

expect
    List.range { start: After 0, end: At 4 } == [1, 2, 3, 4]

expect
    List.range { start: At 0, end: At 4, step: 2 } == [0, 2, 4]

expect
    List.range { start: At 0, end: Before 4 } == [0, 1, 2, 3]

expect
    List.range { start: After 0, end: Before 4 } == [1, 2, 3]

expect
    List.range { start: At 0, end: Before 4, step: 2 } == [0, 2]

expect
    List.range { start: At 4, end: Length 5 } == [4, 5, 6, 7, 8]

expect
    List.range { start: At 4, end: Length 5, step: 10 } == [4, 14, 24, 34, 44]

expect
    List.range { start: At 4, end: Length 5, step: -3 } == [4, 1, -2, -5, -8]

expect
    List.range { start: After 250u8, end: At 255 } == [251, 252, 253, 254, 255]

expect
    List.range { start: After 250u8, end: At 255, step: 10 } == []

expect
    List.range { start: After 250u8, end: At 245, step: 10 } == []

expect
    List.range { start: At 4, end: At 0 } == [4, 3, 2, 1, 0]

## Sort with a custom comparison function
sortWith : List a, (a, a -> [LT, EQ, GT]) -> List a

## Sorts a list of numbers in ascending order (lowest to highest).
##
## To sort in descending order (highest to lowest), use [List.sortDesc] instead.
sortAsc : List (Num a) -> List (Num a)
sortAsc = \list -> List.sortWith list Num.compare

## Sorts a list of numbers in descending order (highest to lowest).
##
## To sort in ascending order (lowest to highest), use [List.sortAsc] instead.
sortDesc : List (Num a) -> List (Num a)
sortDesc = \list -> List.sortWith list (\a, b -> Num.compare b a)

swap : List a, U64, U64 -> List a

## Returns the first element in the list, or `ListWasEmpty` if it was empty.
first : List a -> Result a [ListWasEmpty]
first = \list ->
    when List.get list 0 is
        Ok v -> Ok v
        Err _ -> Err ListWasEmpty

## Returns the given number of elements from the beginning of the list.
## ```roc
## List.takeFirst [1, 2, 3, 4, 5, 6, 7, 8] 4
## ```
## If there are fewer elements in the list than the requested number,
## returns the entire list.
## ```roc
## List.takeFirst [1, 2] 5
## ```
## To *remove* elements from the beginning of the list, use `List.takeLast`.
##
## To remove elements from both the beginning and end of the list,
## use `List.sublist`.
##
## To split the list into two lists, use `List.splitAt`.
##
takeFirst : List elem, U64 -> List elem
takeFirst = \list, outputLength ->
    List.sublist list { start: 0, len: outputLength }

## Returns the given number of elements from the end of the list.
## ```roc
## List.takeLast [1, 2, 3, 4, 5, 6, 7, 8] 4
## ```
## If there are fewer elements in the list than the requested number,
## returns the entire list.
## ```roc
## List.takeLast [1, 2] 5
## ```
## To *remove* elements from the end of the list, use `List.takeFirst`.
##
## To remove elements from both the beginning and end of the list,
## use `List.sublist`.
##
## To split the list into two lists, use `List.splitAt`.
##
takeLast : List elem, U64 -> List elem
takeLast = \list, outputLength ->
    List.sublist list { start: Num.subSaturated (List.len list) outputLength, len: outputLength }

## Drops n elements from the beginning of the list.
dropFirst : List elem, U64 -> List elem
dropFirst = \list, n ->
    remaining = Num.subSaturated (List.len list) n

    List.takeLast list remaining

## Drops n elements from the end of the list.
dropLast : List elem, U64 -> List elem
dropLast = \list, n ->
    remaining = Num.subSaturated (List.len list) n

    List.takeFirst list remaining

## Drops the element at the given index from the list.
##
## This has no effect if the given index is outside the bounds of the list.
##
## To replace the element at a given index, instead of dropping it, see [List.set].
dropAt : List elem, U64 -> List elem

min : List (Num a) -> Result (Num a) [ListWasEmpty]
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

max : List (Num a) -> Result (Num a) [ListWasEmpty]
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
findFirst : List elem, (elem -> Bool) -> Result elem [NotFound]
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
findLast : List elem, (elem -> Bool) -> Result elem [NotFound]
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
findFirstIndex : List elem, (elem -> Bool) -> Result U64 [NotFound]
findFirstIndex = \list, matcher ->
    foundIndex = List.iterate list 0 \index, elem ->
        if matcher elem then
            Break index
        else
            Continue (Num.addWrap index 1)

    when foundIndex is
        Break index -> Ok index
        Continue _ -> Err NotFound

## Returns the last index at which the first element in the list
## satisfying a predicate function can be found.
## If no satisfying element is found, an `Err NotFound` is returned.
findLastIndex : List elem, (elem -> Bool) -> Result U64 [NotFound]
findLastIndex = \list, matches ->
    foundIndex = List.iterateBackwards list (List.len list) \prevIndex, elem ->
        answer = Num.subWrap prevIndex 1

        if matches elem then
            Break answer
        else
            Continue answer

    when foundIndex is
        Break index -> Ok index
        Continue _ -> Err NotFound

## Returns a subsection of the given list, beginning at the `start` index and
## including a total of `len` elements.
##
## If `start` is outside the bounds of the given list, returns the empty list.
## ```roc
## List.sublist [1, 2, 3] { start: 4, len: 0 }
## ```
## If more elements are requested than exist in the list, returns as many as it can.
## ```roc
## List.sublist [1, 2, 3, 4, 5] { start: 2, len: 10 }
## ```
## > If you want a sublist which goes all the way to the end of the list, no
## > matter how long the list is, `List.takeLast` can do that more efficiently.
##
## Some languages have a function called **`slice`** which works similarly to this.
sublist : List elem, { start : U64, len : U64 } -> List elem
sublist = \list, config ->
    sublistLowlevel list config.start config.len

## low-level slicing operation that does no bounds checking
sublistLowlevel : List elem, U64, U64 -> List elem

## Intersperses `sep` between the elements of `list`
## ```roc
## List.intersperse [1, 2, 3] 9     # [1, 9, 2, 9, 3]
## ```
intersperse : List elem, elem -> List elem
intersperse = \list, sep ->
    capacity = 2 * List.len list
    init = List.withCapacity capacity
    newList =
        List.walk list init \acc, elem ->
            acc
            |> List.appendUnsafe elem
            |> List.appendUnsafe sep

    List.dropLast newList 1

## Returns `Bool.true` if the first list starts with the second list.
##
## If the second list is empty, this always returns `Bool.true`; every list
## is considered to "start with" an empty list.
##
## If the first list is empty, this only returns `Bool.true` if the second list is empty.
startsWith : List elem, List elem -> Bool where elem implements Eq
startsWith = \list, prefix ->
    # TODO once we have seamless slices, verify that this wouldn't
    # have better performance with a function like List.compareSublists
    prefix == List.sublist list { start: 0, len: List.len prefix }

## Returns `Bool.true` if the first list ends with the second list.
##
## If the second list is empty, this always returns `Bool.true`; every list
## is considered to "end with" an empty list.
##
## If the first list is empty, this only returns `Bool.true` if the second list is empty.
endsWith : List elem, List elem -> Bool where elem implements Eq
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
splitAt : List elem, U64 -> { before : List elem, others : List elem }
splitAt = \elements, userSplitIndex ->
    length = List.len elements
    splitIndex = if length > userSplitIndex then userSplitIndex else length
    before = List.sublist elements { start: 0, len: splitIndex }
    others = List.sublist elements { start: splitIndex, len: Num.subWrap length splitIndex }

    { before, others }

## Splits the input list on the delimiter element.
##
## ```roc
## List.splitOn [1, 2, 3] 2 == [[1], [3]]
## ```
splitOn : List a, a -> List (List a) where a implements Eq
splitOn = \elements, delimiter ->
    help = \remaining, chunks, currentChunk ->
        when remaining is
            [] -> List.append chunks currentChunk
            [x, .. as rest] if x == delimiter ->
                help rest (List.append chunks currentChunk) []

            [x, .. as rest] ->
                help rest chunks (List.append currentChunk x)
    help elements [] []

## Splits the input list on the delimiter list.
##
## ```roc
## List.splitOnList [1, 2, 3] [1, 2] == [[], [3]]
## ```
splitOnList : List a, List a -> List (List a) where a implements Eq
splitOnList = \elements, delimiter ->
    help = \remaining, chunks, currentChunk ->
        when remaining is
            [] -> List.append chunks currentChunk
            [x, .. as rest] ->
                if List.startsWith remaining delimiter then
                    help (List.dropFirst remaining (List.len delimiter)) (List.append chunks currentChunk) []
                else
                    help rest chunks (List.append currentChunk x)

    if delimiter == [] then
        [elements]
    else
        help elements [] []

## Returns the elements before the first occurrence of a delimiter, as well as the
## remaining elements after that occurrence. If the delimiter is not found, returns `Err`.
## ```roc
## List.splitFirst [Foo, Z, Bar, Z, Baz] Z == Ok { before: [Foo], after: [Bar, Z, Baz] }
## ```
splitFirst : List elem, elem -> Result { before : List elem, after : List elem } [NotFound] where elem implements Eq
splitFirst = \list, delimiter ->
    when List.findFirstIndex list (\elem -> elem == delimiter) is
        Ok index ->
            before = List.sublist list { start: 0, len: index }
            after = List.sublist list { start: Num.addWrap index 1, len: Num.subWrap (List.len list) index |> Num.subWrap 1 }

            Ok { before, after }

        Err NotFound -> Err NotFound

## Returns the elements before the last occurrence of a delimiter, as well as the
## remaining elements after that occurrence. If the delimiter is not found, returns `Err`.
## ```roc
## List.splitLast [Foo, Z, Bar, Z, Baz] Z == Ok { before: [Foo, Z, Bar], after: [Baz] }
## ```
splitLast : List elem, elem -> Result { before : List elem, after : List elem } [NotFound] where elem implements Eq
splitLast = \list, delimiter ->
    when List.findLastIndex list (\elem -> elem == delimiter) is
        Ok index ->
            before = List.sublist list { start: 0, len: index }
            after = List.sublist list { start: Num.addWrap index 1, len: Num.subWrap (List.len list) index |> Num.subWrap 1 }

            Ok { before, after }

        Err NotFound -> Err NotFound

## Splits the list into many chunks, each of which is length of the given chunk
## size. The last chunk will be shorter if the list does not evenly divide by the
## chunk size. If the provided list is empty or if the chunk size is 0 then the
## result is an empty list.
chunksOf : List a, U64 -> List (List a)
chunksOf = \list, chunkSize ->
    if chunkSize == 0 || List.isEmpty list then
        []
    else
        chunkCapacity = Num.divCeil (List.len list) chunkSize
        chunksOfHelp list chunkSize (List.withCapacity chunkCapacity)

chunksOfHelp : List a, U64, List (List a) -> List (List a)
chunksOfHelp = \listRest, chunkSize, chunks ->
    if List.isEmpty listRest then
        chunks
    else
        { before, others } = List.splitAt listRest chunkSize
        chunksOfHelp others chunkSize (List.append chunks before)

## Like [List.map], except the transformation function returns a [Result].
## If that function ever returns `Err`, [mapTry] immediately returns that `Err`.
## If it returns `Ok` for every element, [mapTry] returns `Ok` with the transformed list.
mapTry : List elem, (elem -> Result ok err) -> Result (List ok) err
mapTry = \list, toResult ->
    walkTry list [] \state, elem ->
        Result.map (toResult elem) \ok ->
            List.append state ok

## Same as [List.walk], except you can stop walking early by returning `Err`.
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
walkTry : List elem, state, (state, elem -> Result state err) -> Result state err
walkTry = \list, init, func ->
    walkTryHelp list init func 0 (List.len list)

## internal helper
walkTryHelp : List elem, state, (state, elem -> Result state err), U64, U64 -> Result state err
walkTryHelp = \list, state, f, index, length ->
    if index < length then
        when f state (List.getUnsafe list index) is
            Ok nextState -> walkTryHelp list nextState f (Num.addWrap index 1) length
            Err b -> Err b
    else
        Ok state

## Primitive for iterating over a List, being able to decide at every element whether to continue
iterate : List elem, s, (s, elem -> [Continue s, Break b]) -> [Continue s, Break b]
iterate = \list, init, func ->
    iterHelp list init func 0 (List.len list)

## internal helper
iterHelp : List elem, s, (s, elem -> [Continue s, Break b]), U64, U64 -> [Continue s, Break b]
iterHelp = \list, state, f, index, length ->
    if index < length then
        when f state (List.getUnsafe list index) is
            Continue nextState -> iterHelp list nextState f (Num.addWrap index 1) length
            Break b -> Break b
    else
        Continue state

## Primitive for iterating over a List from back to front, being able to decide at every
## element whether to continue
iterateBackwards : List elem, s, (s, elem -> [Continue s, Break b]) -> [Continue s, Break b]
iterateBackwards = \list, init, func ->
    iterBackwardsHelp list init func (List.len list)

## internal helper
iterBackwardsHelp : List elem, s, (s, elem -> [Continue s, Break b]), U64 -> [Continue s, Break b]
iterBackwardsHelp = \list, state, f, prevIndex ->
    if prevIndex > 0 then
        index = Num.subWrap prevIndex 1

        when f state (List.getUnsafe list index) is
            Continue nextState -> iterBackwardsHelp list nextState f index
            Break b -> Break b
    else
        Continue state

## Concatenates the bytes of a string encoded as utf8 to a list of bytes.
## ```roc
## expect (List.concatUtf8 [1, 2, 3, 4] "🐦") == [1, 2, 3, 4, 240, 159, 144, 166]
## ```
concatUtf8 : List U8, Str -> List U8

expect (List.concatUtf8 [1, 2, 3, 4] "🐦") == [1, 2, 3, 4, 240, 159, 144, 166]

## Run an effectful function for each element on the list.
##
## ```roc
## List.forEach! ["Alice", "Bob", "Charlie"] \name ->
##     createAccount! name
##     log! "Account created"
## ```
##
## If the function might fail or you need to return early, use [forEachTry!].
forEach! : List a, (a => {}) => {}
forEach! = \list, func! ->
    when list is
        [] ->
            {}

        [elem, .. as rest] ->
            func! elem
            forEach! rest func!

## Run an effectful function that might fail for each element on the list.
##
## If the function returns `Err`, the iteration stops and the error is returned.
##
## ```roc
## List.forEachTry! filesToDelete \path ->
##     try File.delete! path
##     Stdout.line! "$(path) deleted"
## ```
forEachTry! : List a, (a => Result {} err) => Result {} err
forEachTry! = \list, func! ->
    when list is
        [] ->
            Ok {}

        [elem, .. as rest] ->
            when func! elem is
                Ok {} ->
                    forEachTry! rest func!

                Err err ->
                    Err err

## Build a value from the contents of a list, using an effectful function.
##
## ```roc
## now_multiples = List.walk! [1, 2, 3] [] \nums, i ->
##         now = Utc.now! {} |> Utc.to_millis_since_epoch
##         List.append nums (now * i)
## ```
##
## This is the same as [walk], except that the step function can have effects.
walk! : List elem, state, (state, elem => state) => state
walk! = \list, state, func! ->
    when list is
        [] -> state
        [elem, .. as rest] ->
            nextState = func! state elem
            walk! rest nextState func!
