module [
    is_empty,
    get,
    set,
    replace,
    update,
    append,
    append_if_ok,
    prepend,
    prepend_if_ok,
    map,
    len,
    with_capacity,
    walk_backwards,
    concat,
    first,
    single,
    repeat,
    reverse,
    join,
    keep_if,
    contains,
    sum,
    walk,
    last,
    keep_oks,
    keep_errs,
    map_with_index,
    map2,
    map3,
    product,
    walk_with_index,
    walk_until,
    walk_with_index_until,
    walk_from,
    walk_from_until,
    range,
    sort_with,
    swap,
    drop_at,
    min,
    max,
    map4,
    map_try,
    walk_try,
    join_map,
    any,
    take_first,
    take_last,
    drop_first,
    drop_last,
    find_first,
    find_last,
    find_first_index,
    find_last_index,
    sublist,
    intersperse,
    split_at,
    split_on,
    split_on_list,
    split_first,
    split_last,
    starts_with,
    ends_with,
    all,
    drop_if,
    sort_asc,
    sort_desc,
    reserve,
    release_excess_capacity,
    walk_backwards_until,
    count_if,
    chunks_of,
    concat_utf8,
    for_each!,
    for_each_try!,
    walk!,
    walk_try!,
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
## > The theoretical maximum length for a list created in Roc is `Num.max_i32` on 32-bit systems
## > and `Num.max_i64` on 64-bit systems. Attempting to create a list bigger than that
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
## get_ratings = \first ->
##     ratings = [first, 4, 3]
##
##     { foo: ratings, bar: ratings }
##
## get_ratings(5)
## ```
## At the end of the `get_ratings` function, when the record gets returned,
## the original `ratings =` binding has gone out of scope and is no longer
## accessible. (Trying to reference `ratings` outside the scope of the
## `get_ratings` function would be an error!)
##
## Since `ratings` represented a way to reference the list, and that way is no
## longer accessible, the list's refcount gets decremented when `ratings` goes
## out of scope. It will decrease from 3 back down to 2.
##
## Putting these together, when we call `get_ratings(5)`, what we get back is
## a record with two fields, `foo`, and `bar`, each of which refers to the same
## list, and that list has a refcount of 2.
##
## Let's change the last line to be `get_ratings(5).bar` instead of `get_ratings(5)`:
## ```roc
## get_ratings = \first ->
##     ratings = [first, 4, 3]
##
##     { foo: ratings, bar: ratings }
##
## get_ratings(5).bar
## ```
## Now, when this expression returns, only the `bar` field of the record will
## be returned. This will mean that the `foo` field becomes inaccessible, causing
## the list's refcount to get decremented from 2 to 1. At this point, the list is back
## where it started: there is only 1 reference to it.
##
## Finally let's suppose the final line were changed to this:
## ```roc
## List.first(get_ratings(5).bar)
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
## first = List.first(nums)
## last = List.last(nums)
##
## first
## ```
## It makes a list, calls [List.first] and [List.last] on it, and then returns `first`.
##
## Here's the equivalent code with a list of lists:
## ```roc
## lists = [[1], [2, 3], [], [4, 5, 6, 7]]
##
## first = List.first(lists)
## last = List.last(lists)
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
## * Even when copying is faster, other list operations may still be slightly slower with persistent data structures. For example, even if it were a persistent data structure, [List.map], [List.walk], and [List.keep_if] would all need to traverse every element in the list and build up the result from scratch. These operations are all
## * Roc's compiler optimizes many list operations into in-place mutations behind the scenes, depending on how the list is being used. For example, [List.map], [List.keep_if], and [List.set] can all be optimized to perform in-place mutations.
## * If possible, it is usually best for performance to use large lists in a way where the optimizer can turn them into in-place mutations. If this is not possible, a persistent data structure might be faster - but this is a rare enough scenario that it would not be good for the average Roc program's performance if this were the way [List] worked by default. Instead, you can look outside Roc's standard modules for an implementation of a persistent data structure - likely built using [List] under the hood!

# separator so List.is_empty doesn't absorb the above into its doc comment

##  Check if the list is empty.
## ```roc
## List.is_empty([1, 2, 3])
##
## List.is_empty([])
## ```
is_empty : List * -> Bool
is_empty = |list|
    List.len(list) == 0

# unsafe primitive that does not perform a bounds check
# but will cause a reference count increment on the value it got out of the list
get_unsafe : List a, U64 -> a

## Returns an element from a list at the given index.
##
## Returns `Err OutOfBounds` if the given index exceeds the List's length
## ```roc
## expect List.get([100, 200, 300], 1) == Ok(200)
## expect List.get([100, 200, 300], 5) == Err(OutOfBounds)
## ```
get : List a, U64 -> Result a [OutOfBounds]
get = |list, index|
    if index < List.len(list) then
        Ok(List.get_unsafe(list, index))
    else
        Err(OutOfBounds)

# unsafe primitive that does not perform a bounds check
# but will cause a reference count increment on the value it got out of the list
replace_unsafe : List a, U64, a -> { list : List a, value : a }

replace : List a, U64, a -> { list : List a, value : a }
replace = |list, index, new_value|
    if index < List.len(list) then
        List.replace_unsafe(list, index, new_value)
    else
        { list, value: new_value }

## Replaces the element at the given index with a replacement.
## ```roc
## List.set(["a", "b", "c"], 1, "B")
## ```
## If the given index is outside the bounds of the list, returns the original
## list unmodified.
##
## To drop the element at a given index, instead of replacing it, see [List.drop_at].
set : List a, U64, a -> List a
set = |list, index, value|
    (List.replace(list, index, value)).list

## Updates the element at the given index with the given function.
## ```roc
## List.update([1, 2, 3], 1, (\x -> x + 1))
## ```
## If the given index is outside the bounds of the list, returns the original
## list unmodified.
##
## To replace the element at a given index, instead of updating based on the current value,
## see [List.set] and [List.replace]
update : List a, U64, (a -> a) -> List a
update = |list, index, func|
    when List.get(list, index) is
        Err(OutOfBounds) -> list
        Ok(value) ->
            new_value = func(value)
            (replace_unsafe(list, index, new_value)).list

# Update one element in bounds
expect
    list : List U64
    list = [1, 2, 3]
    got = update(list, 1, |x| x + 42)
    want = [1, 44, 3]
    got == want

# Update out of bounds
expect
    list : List U64
    list = [1, 2, 3]
    got = update(list, 5, |x| x + 42)
    got == list

# Update chain
expect
    list : List U64
    list = [1, 2, 3]
    got =
        list
        |> update(0, |x| x + 10)
        |> update(1, |x| x + 20)
        |> update(2, |x| x + 30)
    want = [11, 22, 33]
    got == want

## Add a single element to the end of a list.
## ```roc
## List.append([1, 2, 3], 4)
##
## [0, 1, 2]
##     |> List.append(3)
## ```
append : List a, a -> List a
append = |list, element|
    list
    |> List.reserve(1)
    |> List.append_unsafe(element)

## If the given [Result] is `Ok`, add it to the end of a list.
## Otherwise, return the list unmodified.
##
## ```roc
## List.append_if_ok([1, 2, 3], Ok(4))
##
## [0, 1, 2]
##     |> List.append_if_ok(Err(3))
## ```
append_if_ok : List a, Result a * -> List a
append_if_ok = |list, result|
    when result is
        Ok(elem) -> append(list, elem)
        Err(_) -> list

## Writes the element after the current last element unconditionally.
## In other words, it is assumed that
##
## - the list is owned (i.e. can be updated in-place)
## - the list has at least one element of spare capacity
append_unsafe : List a, a -> List a

## Add a single element to the beginning of a list.
## ```roc
## List.prepend([1, 2, 3], 0)
##
## [2, 3, 4]
##     |> List.prepend(1)
## ```
prepend : List a, a -> List a

## If the given [Result] is `Ok`, add it to the beginning of a list.
## Otherwise, return the list unmodified.
##
## ```roc
## List.prepend([1, 2, 3], Ok(0))
##
## [2, 3, 4]
##     |> List.prepend(Err(1))
## ```
prepend_if_ok : List a, Result a * -> List a
prepend_if_ok = |list, result|
    when result is
        Ok(elem) -> prepend(list, elem)
        Err(_) -> list

## Returns the length of the list - the number of elements it contains.
##
## One [List] can store up to `Num.max_i64` elements on 64-bit targets and `Num.max_i32` on 32-bit targets like wasm.
## This means the #U64 this function returns can always be safely converted to #I64 or #I32, depending on the target.
len : List * -> U64

## Create a list with space for at least capacity elements
with_capacity : U64 -> List *

## Enlarge the list for at least capacity additional elements
reserve : List a, U64 -> List a

## Shrink the memory footprint of a list such that it's capacity and length are equal.
## Note: This will also convert seamless slices to regular lists.
release_excess_capacity : List a -> List a

## Put two lists together.
## ```roc
## List.concat([1, 2, 3], [4, 5])
##
## [0, 1, 2]
##     |> List.concat([3, 4])
## ```
concat : List a, List a -> List a

## Returns the last element in the list, or `ListWasEmpty` if it was empty.
## ```roc
## expect List.last([1, 2, 3]) == Ok(3)
## expect List.last([]) == Err(ListWasEmpty)
## ```
last : List a -> Result a [ListWasEmpty]
last = |list|
    when List.get(list, Num.sub_saturated(List.len(list), 1)) is
        Ok(v) -> Ok(v)
        Err(_) -> Err(ListWasEmpty)

## A list with a single element in it.
##
## This is useful in pipelines, like so:
## ```roc
## websites =
##     Str.concat(domain, ".com")
##         |> List.single
## ```
single : a -> List a
single = |x| [x]

## Returns a list with the given length, where every element is the given value.
repeat : a, U64 -> List a
repeat = |value, count|
    repeat_help(value, count, List.with_capacity(count))

repeat_help : a, U64, List a -> List a
repeat_help = |value, count, accum|
    if count > 0 then
        repeat_help(value, Num.sub_wrap(count, 1), List.append_unsafe(accum, value))
    else
        accum

## Returns the list with its elements reversed.
## ```roc
## expect List.reverse([1, 2, 3]) == [3, 2, 1]
## ```
reverse : List a -> List a
reverse = |list|
    end = List.len(list) |> Num.sub_saturated(1)
    reverse_help(List.clone(list), 0, end)

reverse_help = |list, left, right|
    if left < right then
        reverse_help(List.swap(list, left, right), Num.add_wrap(left, 1), Num.sub_wrap(right, 1))
    else
        list

# Ensures that the list in unique (will re-use if already unique)
clone : List a -> List a

## Join the given lists together into one list.
## ```roc
## expect List.join([[1], [2, 3], [], [4, 5]]) == [1, 2, 3, 4, 5]
## expect List.join([[], []]) == []
## expect List.join([]) == []
## ```
join : List (List a) -> List a
join = |lists|
    total_length =
        List.walk(lists, 0, |state, list| Num.add_wrap(state, List.len(list)))

    List.walk(lists, List.with_capacity(total_length), |state, list| List.concat(state, list))

contains : List a, a -> Bool where a implements Eq
contains = |list, needle|
    List.any(list, |x| x == needle)

## Build a value using each element in the list.
##
## Starting with a given `state` value, this walks through each element in the
## list from first to last, running a given `step` function on that element
## which updates the `state`. It returns the final `state` at the end.
##
## You can use it in a pipeline:
## ```roc
## [2, 4, 8]
##     |> List.walk(0, Num.add)
## ```
## This returns 14 because:
## * `state` starts at 0
## * Each `step` runs `Num.add(state, elem)`, and the return value becomes the new `state`.
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
##     |> List.walk(0, Num.sub)
## ```
## Note that in other languages, `walk` is sometimes called `reduce`,
## `fold`, `fold_left`, or `foldl`.
walk : List elem, state, (state, elem -> state) -> state
walk = |list, init, func|
    walk_help(list, init, func, 0, List.len(list))

## internal helper
walk_help : List elem, s, (s, elem -> s), U64, U64 -> s
walk_help = |list, state, f, index, length|
    if index < length then
        next_state = f(state, List.get_unsafe(list, index))

        walk_help(list, next_state, f, Num.add_wrap(index, 1), length)
    else
        state

## Like [walk], but at each step the function also receives the index of the current element.
walk_with_index : List elem, state, (state, elem, U64 -> state) -> state
walk_with_index = |list, init, func|
    walk_with_index_help(list, init, func, 0, List.len(list))

## internal helper
walk_with_index_help : List elem, s, (s, elem, U64 -> s), U64, U64 -> s
walk_with_index_help = |list, state, f, index, length|
    if index < length then
        next_state = f(state, List.get_unsafe(list, index), index)

        walk_with_index_help(list, next_state, f, Num.add_wrap(index, 1), length)
    else
        state

## Like [walk_until], but at each step the function also receives the index of the current element.
walk_with_index_until : List elem, state, (state, elem, U64 -> [Continue state, Break state]) -> state
walk_with_index_until = |list, state, f|
    when walk_with_index_until_help(list, state, f, 0, List.len(list)) is
        Continue(new) -> new
        Break(new) -> new

## internal helper
walk_with_index_until_help : List elem, s, (s, elem, U64 -> [Continue s, Break b]), U64, U64 -> [Continue s, Break b]
walk_with_index_until_help = |list, state, f, index, length|
    if index < length then
        when f(state, List.get_unsafe(list, index), index) is
            Continue(next_state) ->
                walk_with_index_until_help(list, next_state, f, Num.add_wrap(index, 1), length)

            Break(b) -> Break(b)
    else
        Continue(state)

## Note that in other languages, `walk_backwards` is sometimes called `reduce_right`,
## `fold`, `fold_right`, or `foldr`.
walk_backwards : List elem, state, (state, elem -> state) -> state
walk_backwards = |list, state, func|
    walk_backwards_help(list, state, func, len(list))

## internal helper
walk_backwards_help : List elem, state, (state, elem -> state), U64 -> state
walk_backwards_help = |list, state, f, index_plus_one|
    if index_plus_one == 0 then
        state
    else
        index = Num.sub_wrap(index_plus_one, 1)
        next_state = f(state, get_unsafe(list, index))

        walk_backwards_help(list, next_state, f, index)

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
walk_until : List elem, state, (state, elem -> [Continue state, Break state]) -> state
walk_until = |list, initial, step|
    when List.iterate(list, initial, step) is
        Continue(new) -> new
        Break(new) -> new

## Same as [List.walk_until], but does it from the end of the list instead.
walk_backwards_until : List elem, state, (state, elem -> [Continue state, Break state]) -> state
walk_backwards_until = |list, initial, func|
    when List.iterate_backwards(list, initial, func) is
        Continue(new) -> new
        Break(new) -> new

## Walks to the end of the list from a specified starting index
walk_from : List elem, U64, state, (state, elem -> state) -> state
walk_from = |list, index, state, func|
    step : _, _ -> [Continue _, Break []]
    step = |current_state, element| Continue(func(current_state, element))

    when List.iter_help(list, state, step, index, List.len(list)) is
        Continue(new) -> new

## A combination of [List.walk_from] and [List.walk_until]
walk_from_until : List elem, U64, state, (state, elem -> [Continue state, Break state]) -> state
walk_from_until = |list, index, state, func|
    when List.iter_help(list, state, func, index, List.len(list)) is
        Continue(new) -> new
        Break(new) -> new

sum : List (Num a) -> Num a
sum = |list|
    List.walk(list, 0, Num.add)

product : List (Num a) -> Num a
product = |list|
    List.walk(list, 1, Num.mul)

## Run the given predicate on each element of the list, returning `Bool.true` if
## any of the elements satisfy it.
any : List a, (a -> Bool) -> Bool
any = |list, predicate|
    looper = |{}, element|
        if predicate(element) then
            Break({})
        else
            Continue({})

    when List.iterate(list, {}, looper) is
        Continue({}) -> Bool.false
        Break({}) -> Bool.true

## Run the given predicate on each element of the list, returning `Bool.true` if
## all of the elements satisfy it.
all : List a, (a -> Bool) -> Bool
all = |list, predicate|
    looper = |{}, element|
        if predicate(element) then
            Continue({})
        else
            Break({})

    when List.iterate(list, {}, looper) is
        Continue({}) -> Bool.true
        Break({}) -> Bool.false

## Run the given function on each element of a list, and return all the
## elements for which the function returned `Bool.true`.
## ```roc
## List.keep_if([1, 2, 3, 4], (\num -> num > 2))
## ```
## ## Performance Details
##
## [List.keep_if] always returns a list that takes up exactly the same amount
## of memory as the original, even if its length decreases. This is because it
## can't know in advance exactly how much space it will need, and if it guesses a
## length that's too low, it would have to re-allocate.
##
## (If you want to do an operation like this which reduces the memory footprint
## of the resulting list, you can do two passes over the list with [List.walk] - one
## to calculate the precise new size, and another to populate the new list.)
##
## If given a unique list, [List.keep_if] will mutate it in place to assemble the appropriate list.
## If that happens, this function will not allocate any new memory on the heap.
## If all elements in the list end up being kept, Roc will return the original
## list unaltered.
##
keep_if : List a, (a -> Bool) -> List a
keep_if = |list, predicate|
    length = List.len(list)

    keep_if_help(list, predicate, 0, 0, length)

keep_if_help : List a, (a -> Bool), U64, U64, U64 -> List a
keep_if_help = |list, predicate, kept, index, length|
    if index < length then
        if predicate(List.get_unsafe(list, index)) then
            keep_if_help(List.swap(list, kept, index), predicate, Num.add_wrap(kept, 1), Num.add_wrap(index, 1), length)
        else
            keep_if_help(list, predicate, kept, Num.add_wrap(index, 1), length)
    else
        List.take_first(list, kept)

## Run the given function on each element of a list, and return all the
## elements for which the function returned `Bool.false`.
## ```roc
## List.drop_if([1, 2, 3, 4], (\num -> num > 2))
## ```
## ## Performance Details
##
## `List.drop_if` has the same performance characteristics as [List.keep_if].
## See its documentation for details on those characteristics!
drop_if : List a, (a -> Bool) -> List a
drop_if = |list, predicate|
    List.keep_if(list, |e| Bool.not(predicate(e)))

## Run the given function on each element of a list, and return the
## number of elements for which the function returned `Bool.true`.
## ```roc
## expect List.count_if([1, -2, -3], Num.is_negative) == 2
## expect List.count_if([1, 2, 3], (\num -> num > 1)) == 2
## ```
count_if : List a, (a -> Bool) -> U64
count_if = |list, predicate|
    walk_state = |state, elem|
        if predicate(elem) then
            Num.add_wrap(state, 1)
        else
            state

    List.walk(list, 0, walk_state)

## This works like [List.map], except only the transformed values that are
## wrapped in `Ok` are kept. Any that are wrapped in `Err` are dropped.
## ```roc
## expect List.keep_oks(["1", "Two", "23", "Bird"], Str.to_i32) == [1, 23]
##
## expect List.keep_oks([["a", "b"], [], ["c", "d", "e"], [] ], List.first) == ["a", "c"]
##
## fn = \str -> if Str.is_empty(str) then Err(StrWasEmpty) else Ok(str)
## expect List.keep_oks(["", "a", "bc", "", "d", "ef", ""], fn) == ["a", "bc", "d", "ef"]
## ```
keep_oks : List before, (before -> Result after *) -> List after
keep_oks = |list, to_result|
    walker = |accum, element|
        when to_result(element) is
            Ok(keep) -> List.append(accum, keep)
            Err(_drop) -> accum

    List.walk(list, List.with_capacity(List.len(list)), walker)

## This works like [List.map], except only the transformed values that are
## wrapped in `Err` are kept. Any that are wrapped in `Ok` are dropped.
## ```roc
## List.keep_errs([["a", "b"], [], [], ["c", "d", "e"]], List.last)
##
## fn = \str -> if Str.is_empty(str) then Err(StrWasEmpty) else Okd(Str.len(str))
##
## List.keep_errs(["", "a", "bc", "", "d", "ef", ""], fn)
## ```
keep_errs : List before, (before -> Result * after) -> List after
keep_errs = |list, to_result|
    walker = |accum, element|
        when to_result(element) is
            Ok(_drop) -> accum
            Err(keep) -> List.append(accum, keep)

    List.walk(list, List.with_capacity(List.len(list)), walker)

## Convert each element in the list to something new, by calling a conversion
## function on each of them. Then return a new list of the converted values.
## ```roc
## expect List.map([1, 2, 3], (\num -> num + 1)) == [2, 3, 4]
##
## expect List.map(["", "a", "bc"], Str.is_empty) == [Bool.true, Bool.false, Bool.false]
## ```
map : List a, (a -> b) -> List b
map = |list, mapper|
    # TODO: allow checking the refcounting and running the map inplace.
    # Preferably allow it even if the types are different (must be same size with padding though).
    length = List.len(list)
    List.walk(
        list,
        List.with_capacity(length),
        |state, elem|
            List.append_unsafe(state, mapper(elem)),
    )

## Run a transformation function on the first element of each list,
## and use that as the first element in the returned list.
## Repeat until a list runs out of elements.
##
## Some languages have a function named `zip`, which does something similar to
## calling [List.map2] passing two lists and `Pair`:
## ```roc
## zipped = List.map2(["a", "b", "c"], [1, 2, 3], Pair)
## ```
map2 : List a, List b, (a, b -> c) -> List c
map2 = |list_a, list_b, mapper|
    length = Num.min(List.len(list_a), List.len(list_b))
    map2_help(list_a, list_b, List.with_capacity(length), mapper, 0, length)

map2_help : List a, List b, List c, (a, b -> c), U64, U64 -> List c
map2_help = |list_a, list_b, out, mapper, index, length|
    if index < length then
        mapped = mapper(List.get_unsafe(list_a, index), List.get_unsafe(list_b, index))

        map2_help(list_a, list_b, List.append_unsafe(out, mapped), mapper, Num.add_wrap(index, 1), length)
    else
        out

## Run a transformation function on the first element of each list,
## and use that as the first element in the returned list.
## Repeat until a list runs out of elements.
map3 : List a, List b, List c, (a, b, c -> d) -> List d
map3 = |list_a, list_b, list_c, mapper|
    length = Num.min(
        Num.min(List.len(list_a), List.len(list_b)),
        List.len(list_c),
    )
    map3_help(list_a, list_b, list_c, List.with_capacity(length), mapper, 0, length)

map3_help : List a, List b, List c, List d, (a, b, c -> d), U64, U64 -> List d
map3_help = |list_a, list_b, list_c, out, mapper, index, length|
    if index < length then
        mapped = mapper(List.get_unsafe(list_a, index), List.get_unsafe(list_b, index), List.get_unsafe(list_c, index))

        map3_help(list_a, list_b, list_c, List.append_unsafe(out, mapped), mapper, Num.add_wrap(index, 1), length)
    else
        out

## Run a transformation function on the first element of each list,
## and use that as the first element in the returned list.
## Repeat until a list runs out of elements.
map4 : List a, List b, List c, List d, (a, b, c, d -> e) -> List e
map4 = |list_a, list_b, list_c, list_d, mapper|
    length = Num.min(
        Num.min(List.len(list_a), List.len(list_b)),
        Num.min(List.len(list_c), List.len(list_d)),
    )
    map4_help(list_a, list_b, list_c, list_d, List.with_capacity(length), mapper, 0, length)

map4_help : List a, List b, List c, List d, List e, (a, b, c, d -> e), U64, U64 -> List e
map4_help = |list_a, list_b, list_c, list_d, out, mapper, index, length|
    if index < length then
        mapped = mapper(List.get_unsafe(list_a, index), List.get_unsafe(list_b, index), List.get_unsafe(list_c, index), List.get_unsafe(list_d, index))

        map4_help(list_a, list_b, list_c, list_d, List.append(out, mapped), mapper, Num.add_wrap(index, 1), length)
    else
        out

## This works like [List.map], except it also passes the index
## of the element to the conversion function.
## ```roc
## expect List.map_with_index([10, 20, 30], (\num, index -> num + index)) == [10, 21, 32]
## ```
map_with_index : List a, (a, U64 -> b) -> List b
map_with_index = |src, func|
    length = len(src)
    dest = with_capacity(length)

    map_with_index_help(src, dest, func, 0, length)

# Internal helper
map_with_index_help : List a, List b, (a, U64 -> b), U64, U64 -> List b
map_with_index_help = |src, dest, func, index, length|
    if index < length then
        elem = get_unsafe(src, index)
        mapped_elem = func(elem, index)
        new_dest = List.append_unsafe(dest, mapped_elem)

        map_with_index_help(src, new_dest, func, Num.add_wrap(index, 1), length)
    else
        dest

## Returns a list of all the integers between `start` and `end`.
##
## To include the `start` and `end` integers themselves, use `At` like so:
## ```roc
## List.range({ start: At 2, end: At 5 }) == [2, 3, 4, 5]
## ```
## To exclude them, use `After` and `Before`, like so:
## ```roc
## List.range({ start: After 2, end: Before 5 }) == [3, 4]
## ```
## You can have the list end at a certain length rather than a certain integer:
## ```roc
## List.range({ start: At 6, end: Length 4 }) == [6, 7, 8, 9]
## ```
## If `step` is specified, each integer increases by that much. (`step: 1` is the default.)
## ```roc
## List.range({ start: After 0, end: Before 9, step: 3 }) == [3, 6]
## ```
## List.range will also generate a reversed list if step is negative or end comes before start:
## ```roc
## List.range({ start: At 5, end: At 2 }) == [5, 4, 3, 2]
## ```
## All of these options are compatible with the others. For example, you can use `At` or `After`
## with `start` regardless of what `end` and `step` are set to.
range : _
range = |{ start, end, step ?? 0 }|
    { calc_next, step_is_positive } =
        if step == 0 then
            when T(start, end) is
                T(At(x), At(y)) | T(At(x), Before(y)) | T(After(x), At(y)) | T(After(x), Before(y)) ->
                    if x < y then
                        {
                            calc_next: |i| Num.add_checked(i, 1),
                            step_is_positive: Bool.true,
                        }
                    else
                        {
                            calc_next: |i| Num.sub_checked(i, 1),
                            step_is_positive: Bool.false,
                        }

                T(At(_), Length(_)) | T(After(_), Length(_)) ->
                    {
                        calc_next: |i| Num.add_checked(i, 1),
                        step_is_positive: Bool.true,
                    }
        else
            {
                calc_next: |i| Num.add_checked(i, step),
                step_is_positive: step > 0,
            }

    inclusive_start =
        when start is
            At(x) -> Ok(x)
            After(x) -> calc_next(x)

    when end is
        At(at) ->
            is_valid =
                if step_is_positive then
                    |i| i <= at
                else
                    |i| i >= at

            # TODO: switch to List.with_capacity
            range_help([], inclusive_start, calc_next, is_valid)

        Before(before) ->
            is_valid =
                if step_is_positive then
                    |i| i < before
                else
                    |i| i > before

            # TODO: switch to List.with_capacity
            range_help([], inclusive_start, calc_next, is_valid)

        Length(l) ->
            range_length_help(List.with_capacity(l), inclusive_start, l, calc_next)

range_help = |accum, i, calc_next, is_valid|
    when i is
        Ok(val) ->
            if is_valid(val) then
                # TODO: change this to List.append_unsafe once capacity is set correctly
                range_help(List.append(accum, val), calc_next(val), calc_next, is_valid)
            else
                accum

        Err(_) ->
            # We went past the end of the numeric range and there is no next.
            # return the generated list.
            accum

range_length_help = |accum, i, remaining, calc_next|
    if remaining == 0 then
        accum
    else
        when i is
            Ok(val) ->
                range_length_help(List.append_unsafe(accum, val), calc_next(val), Num.sub_wrap(remaining, 1), calc_next)

            Err(_) ->
                # We went past the end of the numeric range and there is no next.
                # The list is not the correct length yet, so we must crash.
                crash("List.range: failed to generate enough elements to fill the range before overflowing the numeric type")

expect
    List.range({ start: At(0), end: At(4) }) == [0, 1, 2, 3, 4]

expect
    List.range({ start: After(0), end: At(4) }) == [1, 2, 3, 4]

expect
    List.range({ start: At(0), end: At(4), step: 2 }) == [0, 2, 4]

expect
    List.range({ start: At(0), end: Before(4) }) == [0, 1, 2, 3]

expect
    List.range({ start: After(0), end: Before(4) }) == [1, 2, 3]

expect
    List.range({ start: At(0), end: Before(4), step: 2 }) == [0, 2]

expect
    List.range({ start: At(4), end: Length(5) }) == [4, 5, 6, 7, 8]

expect
    List.range({ start: At(4), end: Length(5), step: 10 }) == [4, 14, 24, 34, 44]

expect
    List.range({ start: At(4), end: Length(5), step: -3 }) == [4, 1, -2, -5, -8]

expect
    List.range({ start: After(250u8), end: At(255) }) == [251, 252, 253, 254, 255]

expect
    List.range({ start: After(250u8), end: At(255), step: 10 }) == []

expect
    List.range({ start: After(250u8), end: At(245), step: 10 }) == []

expect
    List.range({ start: At(4), end: At(0) }) == [4, 3, 2, 1, 0]

## Sort with a custom comparison function
sort_with : List a, (a, a -> [LT, EQ, GT]) -> List a

## Sorts a list of numbers in ascending order (lowest to highest).
##
## To sort in descending order (highest to lowest), use [List.sort_desc] instead.
sort_asc : List (Num a) -> List (Num a)
sort_asc = |list| List.sort_with(list, Num.compare)

## Sorts a list of numbers in descending order (highest to lowest).
##
## To sort in ascending order (lowest to highest), use [List.sort_asc] instead.
sort_desc : List (Num a) -> List (Num a)
sort_desc = |list| List.sort_with(list, |a, b| Num.compare(b, a))

swap : List a, U64, U64 -> List a

## Returns the first element in the list, or `ListWasEmpty` if it was empty.
first : List a -> Result a [ListWasEmpty]
first = |list|
    when List.get(list, 0) is
        Ok(v) -> Ok(v)
        Err(_) -> Err(ListWasEmpty)

## Returns the given number of elements from the beginning of the list.
## ```roc
## List.take_first([1, 2, 3, 4, 5, 6, 7, 8], 4) == [1, 2, 3, 4]
## ```
## If there are fewer elements in the list than the requested number,
## returns the entire list.
## ```roc
## List.take_first([1, 2], 5) == [1, 2]
## ```
## To *remove* elements from the beginning of the list, use `List.take_last`.
##
## To remove elements from both the beginning and end of the list,
## use `List.sublist`.
##
## To split the list into two lists, use `List.split_at`.
##
take_first : List elem, U64 -> List elem
take_first = |list, output_length|
    List.sublist(list, { start: 0, len: output_length })

## Returns the given number of elements from the end of the list.
## ```roc
## List.take_last([1, 2, 3, 4, 5, 6, 7, 8], 4) == [5, 6, 7, 8]
## ```
## If there are fewer elements in the list than the requested number,
## returns the entire list.
## ```roc
## List.take_last([1, 2], 5) == [1, 2]
## ```
## To *remove* elements from the end of the list, use `List.take_first`.
##
## To remove elements from both the beginning and end of the list,
## use `List.sublist`.
##
## To split the list into two lists, use `List.split_at`.
##
take_last : List elem, U64 -> List elem
take_last = |list, output_length|
    List.sublist(list, { start: Num.sub_saturated(List.len(list), output_length), len: output_length })

## Drops n elements from the beginning of the list.
drop_first : List elem, U64 -> List elem
drop_first = |list, n|
    remaining = Num.sub_saturated(List.len(list), n)

    List.take_last(list, remaining)

## Drops n elements from the end of the list.
drop_last : List elem, U64 -> List elem
drop_last = |list, n|
    remaining = Num.sub_saturated(List.len(list), n)

    List.take_first(list, remaining)

## Drops the element at the given index from the list.
##
## This has no effect if the given index is outside the bounds of the list.
##
## To replace the element at a given index, instead of dropping it, see [List.set].
drop_at : List elem, U64 -> List elem

min : List (Num a) -> Result (Num a) [ListWasEmpty]
min = |list|
    when List.first(list) is
        Ok(initial) ->
            Ok(min_help(list, initial))

        Err(ListWasEmpty) ->
            Err(ListWasEmpty)

min_help : List (Num a), Num a -> Num a
min_help = |list, initial|
    List.walk(
        list,
        initial,
        |best_so_far, current|
            if current < best_so_far then
                current
            else
                best_so_far,
    )

max : List (Num a) -> Result (Num a) [ListWasEmpty]
max = |list|
    when List.first(list) is
        Ok(initial) ->
            Ok(max_help(list, initial))

        Err(ListWasEmpty) ->
            Err(ListWasEmpty)

max_help : List (Num a), Num a -> Num a
max_help = |list, initial|
    List.walk(
        list,
        initial,
        |best_so_far, current|
            if current > best_so_far then
                current
            else
                best_so_far,
    )

## Like [List.map], except the transformation function wraps the return value
## in a list. At the end, all the lists get joined together into one list.
##
## You may know a similar function named `concat_map` in other languages.
join_map : List a, (a -> List b) -> List b
join_map = |list, mapper|
    List.walk(list, [], |state, elem| List.concat(state, mapper(elem)))

## Returns the first element of the list satisfying a predicate function.
## If no satisfying element is found, an `Err NotFound` is returned.
find_first : List elem, (elem -> Bool) -> Result elem [NotFound]
find_first = |list, pred|
    callback = |_, elem|
        if pred(elem) then
            Break(elem)
        else
            Continue({})

    when List.iterate(list, {}, callback) is
        Continue({}) -> Err(NotFound)
        Break(found) -> Ok(found)

## Returns the last element of the list satisfying a predicate function.
## If no satisfying element is found, an `Err NotFound` is returned.
find_last : List elem, (elem -> Bool) -> Result elem [NotFound]
find_last = |list, pred|
    callback = |_, elem|
        if pred(elem) then
            Break(elem)
        else
            Continue({})

    when List.iterate_backwards(list, {}, callback) is
        Continue({}) -> Err(NotFound)
        Break(found) -> Ok(found)

## Returns the index at which the first element in the list
## satisfying a predicate function can be found.
## If no satisfying element is found, an `Err NotFound` is returned.
find_first_index : List elem, (elem -> Bool) -> Result U64 [NotFound]
find_first_index = |list, matcher|
    found_index = List.iterate(
        list,
        0,
        |index, elem|
            if matcher(elem) then
                Break(index)
            else
                Continue(Num.add_wrap(index, 1)),
    )

    when found_index is
        Break(index) -> Ok(index)
        Continue(_) -> Err(NotFound)

## Returns the last index at which the first element in the list
## satisfying a predicate function can be found.
## If no satisfying element is found, an `Err NotFound` is returned.
find_last_index : List elem, (elem -> Bool) -> Result U64 [NotFound]
find_last_index = |list, matches|
    found_index = List.iterate_backwards(
        list,
        List.len(list),
        |prev_index, elem|
            answer = Num.sub_wrap(prev_index, 1)

            if matches(elem) then
                Break(answer)
            else
                Continue(answer),
    )

    when found_index is
        Break(index) -> Ok(index)
        Continue(_) -> Err(NotFound)

## Returns a subsection of the given list, beginning at the `start` index and
## including a total of `len` elements.
##
## If `start` is outside the bounds of the given list, returns the empty list.
## ```roc
## List.sublist([1, 2, 3], { start: 4, len: 0 })
## ```
## If more elements are requested than exist in the list, returns as many as it can.
## ```roc
## List.sublist([1, 2, 3, 4, 5], { start: 2, len: 10 })
## ```
## > If you want a sublist which goes all the way to the end of the list, no
## > matter how long the list is, `List.take_last` can do that more efficiently.
##
## Some languages have a function called **`slice`** which works similarly to this.
sublist : List elem, { start : U64, len : U64 } -> List elem
sublist = |list, config|
    sublist_lowlevel(list, config.start, config.len)

## low-level slicing operation that does no bounds checking
sublist_lowlevel : List elem, U64, U64 -> List elem

## Intersperses `sep` between the elements of `list`
## ```roc
## List.intersperse([1, 2, 3], 9) == [1, 9, 2, 9, 3]
## ```
intersperse : List elem, elem -> List elem
intersperse = |list, sep|
    capacity = 2 * List.len(list)
    init = List.with_capacity(capacity)
    new_list =
        List.walk(
            list,
            init,
            |acc, elem|
                acc
                |> List.append_unsafe(elem)
                |> List.append_unsafe(sep),
        )

    List.drop_last(new_list, 1)

## Returns `Bool.true` if the first list starts with the second list.
##
## If the second list is empty, this always returns `Bool.true`; every list
## is considered to "start with" an empty list.
##
## If the first list is empty, this only returns `Bool.true` if the second list is empty.
starts_with : List elem, List elem -> Bool where elem implements Eq
starts_with = |list, prefix|
    # TODO once we have seamless slices, verify that this wouldn't
    # have better performance with a function like List.compare_sublists
    prefix == List.sublist(list, { start: 0, len: List.len(prefix) })

## Returns `Bool.true` if the first list ends with the second list.
##
## If the second list is empty, this always returns `Bool.true`; every list
## is considered to "end with" an empty list.
##
## If the first list is empty, this only returns `Bool.true` if the second list is empty.
ends_with : List elem, List elem -> Bool where elem implements Eq
ends_with = |list, suffix|
    # TODO once we have seamless slices, verify that this wouldn't
    # have better performance with a function like List.compare_sublists
    length = List.len(suffix)
    start = Num.sub_saturated(List.len(list), length)

    suffix == List.sublist(list, { start, len: length })

## Splits the list into two lists, around the given index.
##
## The returned lists are labeled `before` and `others`. The `before` list will
## contain all the elements whose index in the original list was **less than**
## than the given index, # and the `others` list will be all the others. (This
## means if you give an index of 0, the `before` list will be empty and the
## `others` list will have the same elements as the original list.)
split_at : List elem, U64 -> { before : List elem, others : List elem }
split_at = |elements, user_split_index|
    length = List.len(elements)
    split_index = if length > user_split_index then user_split_index else length
    before = List.sublist(elements, { start: 0, len: split_index })
    others = List.sublist(elements, { start: split_index, len: Num.sub_wrap(length, split_index) })

    { before, others }

## Splits the input list on the delimiter element.
##
## ```roc
## List.split_on([1, 2, 3], 2) == [[1], [3]]
## ```
split_on : List a, a -> List (List a) where a implements Eq
split_on = |elements, delimiter|
    help = |remaining, chunks, current_chunk|
        when remaining is
            [] -> List.append(chunks, current_chunk)
            [x, .. as rest] if x == delimiter ->
                help(rest, List.append(chunks, current_chunk), [])

            [x, .. as rest] ->
                help(rest, chunks, List.append(current_chunk, x))
    help(elements, [], [])

## Splits the input list on the delimiter list.
##
## ```roc
## List.split_on_list([1, 2, 3], [1, 2]) == [[], [3]]
## ```
split_on_list : List a, List a -> List (List a) where a implements Eq
split_on_list = |elements, delimiter|
    help = |remaining, chunks, current_chunk|
        when remaining is
            [] -> List.append(chunks, current_chunk)
            [x, .. as rest] ->
                if List.starts_with(remaining, delimiter) then
                    help(List.drop_first(remaining, List.len(delimiter)), List.append(chunks, current_chunk), [])
                else
                    help(rest, chunks, List.append(current_chunk, x))

    if delimiter == [] then
        [elements]
    else
        help(elements, [], [])

## Returns the elements before the first occurrence of a delimiter, as well as the
## remaining elements after that occurrence. If the delimiter is not found, returns `Err`.
## ```roc
## List.split_first([Foo, Z, Bar, Z, Baz], Z) == Ok({ before: [Foo], after: [Bar, Z, Baz] })
## ```
split_first : List elem, elem -> Result { before : List elem, after : List elem } [NotFound] where elem implements Eq
split_first = |list, delimiter|
    when List.find_first_index(list, |elem| elem == delimiter) is
        Ok(index) ->
            before = List.sublist(list, { start: 0, len: index })
            after = List.sublist(list, { start: Num.add_wrap(index, 1), len: Num.sub_wrap(List.len(list), index) |> Num.sub_wrap(1) })

            Ok({ before, after })

        Err(NotFound) -> Err(NotFound)

## Returns the elements before the last occurrence of a delimiter, as well as the
## remaining elements after that occurrence. If the delimiter is not found, returns `Err`.
## ```roc
## List.split_last([Foo, Z, Bar, Z, Baz], Z) == Ok({ before: [Foo, Z, Bar], after: [Baz] })
## ```
split_last : List elem, elem -> Result { before : List elem, after : List elem } [NotFound] where elem implements Eq
split_last = |list, delimiter|
    when List.find_last_index(list, |elem| elem == delimiter) is
        Ok(index) ->
            before = List.sublist(list, { start: 0, len: index })
            after = List.sublist(list, { start: Num.add_wrap(index, 1), len: Num.sub_wrap(List.len(list), index) |> Num.sub_wrap(1) })

            Ok({ before, after })

        Err(NotFound) -> Err(NotFound)

## Splits the list into many chunks, each of which is length of the given chunk
## size. The last chunk will be shorter if the list does not evenly divide by the
## chunk size. If the provided list is empty or if the chunk size is 0 then the
## result is an empty list.
chunks_of : List a, U64 -> List (List a)
chunks_of = |list, chunk_size|
    if chunk_size == 0 || List.is_empty(list) then
        []
    else
        chunk_capacity = Num.div_ceil(List.len(list), chunk_size)
        chunks_of_help(list, chunk_size, List.with_capacity(chunk_capacity))

chunks_of_help : List a, U64, List (List a) -> List (List a)
chunks_of_help = |list_rest, chunk_size, chunks|
    if List.is_empty(list_rest) then
        chunks
    else
        { before, others } = List.split_at(list_rest, chunk_size)
        chunks_of_help(others, chunk_size, List.append(chunks, before))

## Like [List.map], except the transformation function returns a [Result].
## If that function ever returns `Err`, [map_try] immediately returns that `Err`.
## If it returns `Ok` for every element, [map_try] returns `Ok` with the transformed list.
map_try : List elem, (elem -> Result ok err) -> Result (List ok) err
map_try = |list, to_result|
    walk_try(
        list,
        [],
        |state, elem|
            Result.map_ok(
                to_result(elem),
                |ok|
                    List.append(state, ok),
            ),
    )

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
walk_try : List elem, state, (state, elem -> Result state err) -> Result state err
walk_try = |list, init, func|
    walk_try_help(list, init, func, 0, List.len(list))

## internal helper
walk_try_help : List elem, state, (state, elem -> Result state err), U64, U64 -> Result state err
walk_try_help = |list, state, f, index, length|
    if index < length then
        when f(state, List.get_unsafe(list, index)) is
            Ok(next_state) -> walk_try_help(list, next_state, f, Num.add_wrap(index, 1), length)
            Err(b) -> Err(b)
    else
        Ok(state)

## Primitive for iterating over a List, being able to decide at every element whether to continue
iterate : List elem, s, (s, elem -> [Continue s, Break b]) -> [Continue s, Break b]
iterate = |list, init, func|
    iter_help(list, init, func, 0, List.len(list))

## internal helper
iter_help : List elem, s, (s, elem -> [Continue s, Break b]), U64, U64 -> [Continue s, Break b]
iter_help = |list, state, f, index, length|
    if index < length then
        when f(state, List.get_unsafe(list, index)) is
            Continue(next_state) -> iter_help(list, next_state, f, Num.add_wrap(index, 1), length)
            Break(b) -> Break(b)
    else
        Continue(state)

## Primitive for iterating over a List from back to front, being able to decide at every
## element whether to continue
iterate_backwards : List elem, s, (s, elem -> [Continue s, Break b]) -> [Continue s, Break b]
iterate_backwards = |list, init, func|
    iter_backwards_help(list, init, func, List.len(list))

## internal helper
iter_backwards_help : List elem, s, (s, elem -> [Continue s, Break b]), U64 -> [Continue s, Break b]
iter_backwards_help = |list, state, f, prev_index|
    if prev_index > 0 then
        index = Num.sub_wrap(prev_index, 1)

        when f(state, List.get_unsafe(list, index)) is
            Continue(next_state) -> iter_backwards_help(list, next_state, f, index)
            Break(b) -> Break(b)
    else
        Continue(state)

## Concatenates the bytes of a string encoded as utf8 to a list of bytes.
## ```roc
## expect List.concat_utf8([1, 2, 3, 4], "🐦") == [1, 2, 3, 4, 240, 159, 144, 166]
## ```
concat_utf8 : List U8, Str -> List U8

expect (List.concat_utf8([1, 2, 3, 4], "🐦")) == [1, 2, 3, 4, 240, 159, 144, 166]

## Run an effectful function for each element on the list.
##
## ```roc
## List.for_each!(["Alice", "Bob", "Charlie"], \name ->
##     create_account!(name)
##     log!("Account created")
## )
## ```
##
## If the function might fail or you need to return early, use [for_each_try!].
for_each! : List a, (a => {}) => {}
for_each! = |list, func!|
    when list is
        [] ->
            {}

        [elem, .. as rest] ->
            func!(elem)
            for_each!(rest, func!)

## Run an effectful function that might fail for each element on the list.
##
## If the function returns `Err`, the iteration stops and the error is returned.
##
## ```roc
## List.for_each_try!(files_to_delete, \path ->
##     File.delete!(path)?
##
##     Stdout.line!("${path} deleted")
## )
## ```
for_each_try! : List a, (a => Result {} err) => Result {} err
for_each_try! = |list, func!|
    when list is
        [] ->
            Ok({})

        [elem, .. as rest] ->
            when func!(elem) is
                Ok({}) ->
                    for_each_try!(rest, func!)

                Err(err) ->
                    Err(err)

## Build a value from the contents of a list, using an effectful function.
##
## ```roc
## now_multiples = List.walk!([1, 2, 3], [], \nums, i ->
##      now = Utc.now!({}) |> Utc.to_millis_since_epoch
##      List.append(nums, now * i)
## )
## ```
##
## This is the same as [walk], except that the step function can have effects.
walk! : List elem, state, (state, elem => state) => state
walk! = |list, state, func!|
    when list is
        [] ->
            state

        [elem, .. as rest] ->
            next_state = func!(state, elem)
            walk!(rest, next_state, func!)

## Build a value from the contents of a list, using an effectful function that might fail.
##
## If the function returns `Err`, the iteration stops and the error is returned.
##
## ```
## names =
##     List.walk_try!(
##         ["First", "Middle", "Last"],
##         [],
##         \accumulator, which ->
##             Stdout.write!("${which} name: ")?
##             name = Stdin.line!({})?
##             Ok(List.append(accumulator, name)),
##     )?
## ```
##
## This is the same as [walk_try], except that the step function can have effects.
walk_try! : List elem, state, (state, elem => Result state err) => Result state err
walk_try! = |list, state, func!|
    when list is
        [] ->
            Ok(state)

        [elem, .. as rest] ->
            when func!(state, elem) is
                Ok(next_state) ->
                    walk_try!(rest, next_state, func!)

                Err(err) ->
                    Err(err)
