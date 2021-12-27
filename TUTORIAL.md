# Tutorial

This is a tutorial for how to build Roc applications. It covers the REPL, basic
types (strings, lists, tags, and functions), syntax (`when`, `if then else`)
and more!

Enjoy!

## Strings and Numbers

Let’s start by getting acquainted with Roc’s Read Eval Print Loop, or REPL for
short. Run this in a terminal:

```
$ roc repl
```

You should see this:

```
The rockin’ roc repl
```

Try typing this in and pressing enter:

```coffee
>> "Hello, World!"
"Hello, World!" : Str
```

Congratulations! You've just written your first Roc code!

Specifically, you entered the *expression* `"Hello, World!"` into the REPL,
and the REPL printed it back out. It also printed `: Str`, which is the
expression's type. We'll talk about types later; for now, we'll ignore the `:`
and whatever comes after it whenever the REPL prints them.

Let's try putting in a more complicated expression:

```coffee
>> 1 + 1
2 : Num *
```

According to the Roc REPL, one plus one equals two. Checks out!

Roc will respect [order of operations](https://en.wikipedia.org/wiki/Order_of_operations) when using multiple arithmetic operators
like `+` and `-`, but you can use parentheses to specify exactly how they should
be grouped.

```coffee
>> 1 + 2 * (3 - 4)
-1 : Num *
```

Let's try calling a function:

```coffee
>> Str.concat "Hi " "there!"
"Hi there!" : Str
```

In this expression, we're calling the `Str.concat` function
passing two arguments: the string `"Hi "` and the string `"there!"`. The
`Str.concat` function *concatenates* two strings together (that is, it puts
one after the other) and returns the resulting combined string of
`"Hi there!"`.

Note that in Roc, we don't need parentheses or commas to call functions.
We don't write `Str.concat("Hi ", "there!")` but rather `Str.concat "Hi " "there!"`.

Just like in the arithmetic example above, we can use parentheses to specify
how nested function calls should work. For example, we could write this:

```coffee
>> Str.concat "Birds: " (Num.toStr 42)
"Birds: 42" : Str
```

This calls `Num.toStr` on the number `42`, which converts it into the string
`"42"`, and then passes that string as the second argument to `Str.concat`.
The parentheses are important here to specify how the function calls nest!
Try removing them, and see what happens:

```coffee
>> Str.concat "Birds: " Num.toStr 42
<error>
```

This error message says that we've given `Str.concat` too many arguments.
Indeed we have! We've passed it three arguments: the string `"Birds"`, the
function `Num.toStr`, and the number `42`. That's not what we wanted to do.
Putting parentheses around the `Num.toStr 42` call clarifies that we want it
to be evaluated as its own expression, rather than being two arguments to
`Str.concat`.

Both the `Str.concat` function and the `Num.toStr` function have a `.` in
their names. In `Str.concat`, `Str` is the name of a *module*, and `concat`
is the name of a function inside that module. Similarly, `Num` is a different
module, and `toStr` is a function inside that module.

We'll get into more depth about modules later, but for now you can think of
a module as a named collection of functions. It'll be awhile before we want
to use them for more than that anyway!

## Building an Application

Let's move out of the REPL and create our first Roc application.

Create a new file called `Hello.roc` and put this inside it:

```coffee
app "hello"
    packages { pf: "examples/cli/platform" }
    imports [ pf.Stdout ]
    provides [ main ] to pf

main = Stdout.line "I'm a Roc application!"
```

> **NOTE:** This assumes you've put Hello.roc in the root directory of the
> Roc source code. If you'd like to put it somewhere else, you'll need to replace
> `"examples/cli/"` with the path to the `examples/cli/` folder in
> that source code. In the future, Roc will have the tutorial built in, and this
> aside will no longer be necessary!

Try running this with:

```
$ roc Hello.roc
```

You should see this:

```
I'm a Roc application!
```

Congratulations - you've now written your first Roc application! We'll go over what the parts of
this file above `main` do later, but first let's play around a bit.
Try replacing the `main` line with this:

```coffee
main = Stdout.line "There are \(total) animals."

birds = 3

iguanas = 2

total = Num.toStr (birds + iguanas)
```

Now if you run `roc Hello.roc`, you should see this:

```
There are 5 animals.
```

`Hello.roc` now has four definitions - or *defs* for
short - namely, `main`, `birds`, `iguanas`, and `total`.

A definition names an expression.
- The first def assigns the name `main` to the expression `Stdout.line "I have \(numDefs) definitions."`.  The `Stdout.line` function takes a string and prints it as a line to [`stdout`] (the terminal's standard output device).
- The next two defs assign the names `birds` and `iguanas` to the expressions `3` and `2`.
- The last def assigns the name `total` to the expression `Num.toStr (birds + iguanas)`.

Once we have a def, we can use its name in other expressions.
For example, the `total` expression refers to `birds` and `iguanas`.

We can also refer to defs inside strings using *string interpolation*. The
string `"There are \(total) animals."` evaluates to the same thing as calling
`Str.concat "There are " (Str.concat total " animals.")` directly.

You can name a def using any combination of letters and numbers, but they have
to start with a letter. Note that definitions are constant; once we've assigned
a name to an expression, we can't reassign it! We'd get an error if we wrote this:

```coffee
birds = 3

birds = 2
```

Order of defs doesn't matter. We defined `birds` and `iguanas` before
`total` (which uses both of them), but we defined `main` before `total` even though
it uses `total`. If you like, you can change the order of these defs to anything
you like, and everything will still work the same way!

This works because Roc expressions don't have *side effects*. We'll talk more
about side effects later.

## Functions and `if`

So far we've called functions like `Num.toStr`, `Str.concat`, and `Stdout.line`.
Next let's try defining a function of our own.

```coffee
main = Stdout.line "There are \(total) animals."

birds = 3

iguanas = 2

total = addAndStringify birds iguanas

addAndStringify = \num1, num2 ->
    Num.toStr (num1 + num2)
```

This new `addAndStringify` function we've defined takes two numbers, adds them,
calls `Num.toStr` on the result, and returns that. The `\num1, num2 ->` syntax
defines a function's arguments, and the expression after the `->` is the body
of the function. The expression at the end of the body (`Num.toStr (num1 + num2)`
in this case) is returned automatically.

Let's modify the function to return an empty string if the numbers add to zero.

```coffee
addAndStringify = \num1, num2 ->
    sum = num1 + num2

    if sum == 0 then
        ""
    else
        Num.toStr sum
```

We did two things here:
* We introduced a local def named `sum`, and set it equal to `num1 + num2`. Because we defined `sum` inside `addAndStringify`, it will not be accessible outside that function.
* We added an `if` / `then` / `else` conditional to return either `""` or `Num.toStr sum` depending on whether `sum == 0`.

Of note, we couldn't have done `total = num1 + num2` because that would be
redefining `total` in the global scope, and defs can't be redefined. (However, we could use the name
`sum` for a def in a different function, because then they'd be in completely
different scopes and wouldn't affect each other.)

Also note that every `if` must be accompanied by both `then` and also `else`.
Having an `if` without an `else` is an error, because in Roc, everything is
an expression - which means it must evaluate to a value. If there were ever an
`if` without an `else`, that would be an expression that might not evaluate to
a value!

We can combine `if` and `else` to get `else if`, like so:

```coffee
addAndStringify = \num1, num2 ->
    sum = num1 + num2

    if sum == 0 then
        ""
    else if sum < 0 then
        "negative"
    else
        Num.toStr sum
```

Note that `else if` is not a separate language keyword! It's just an `if`/`else` where
the `else` branch contains another `if`/`else`. This is easier to see with different indentation:

```coffee
addAndStringify = \num1, num2 ->
    sum = num1 + num2

    if sum == 0 then
        ""
    else
        if sum < 0 then
            "negative"
        else
            Num.toStr sum
```

This code is equivalent to writing `else if sum < 0 then` on one line, although the stylistic
convention is to write `else if` on the same line.

## Records

Currently our `addAndStringify` function takes two arguments. We can instead make
it take one argument like so:

```coffee
total = addAndStringify { birds: 5, iguanas: 7 }

addAndStringify = \counts ->
    Num.toStr (counts.birds + counts.iguanas)
```

The function now takes a *record*, which is a group of values that travel together.
Records are not objects; they don't have methods or inheritance, they just store values.

We create the record when we write `{ birds: 5, iguanas: 7 }`. This defines
a record with two *fields* - namely, the `birds` field and the `iguanas` field -
and then assigns the number `5` to the `birds` field and the number `7` to the
`iguanas` field.

When we write `counts.birds`, it accesses the `birds` field of the `counts` record,
and when we write `counts.iguanas` it accesses the `iguanas` field. When we use `==`
on records, it compares all the fields in both records with `==`, and only returns true
if all fields on both records return true for their `==` comparisons. If one record has
more fields than the other, or if the types associated with a given field are different
between one field and the other, the Roc compiler will give an error at build time.

> **Note:** Some other languages have a concept of "identity equality" that's separate from
> the "structural equality" we just described. Roc does not have a concept of identity equality;
> this is the only way equality works!

The `addAndStringify` function will accept any record with at least the fields `birds` and
`iguanas`, but it will also accept records with more fields. For example:

```coffee
total = addAndStringify { birds: 5, iguanas: 7 }

totalWithNote = addAndStringify { birds: 4, iguanas: 3, note: "Whee!" }

addAndStringify = \counts ->
    Num.toStr (counts.birds + counts.iguanas)
```

This works because `addWithStringify` only uses `counts.birds` and `counts.iguanas`.
If we were to use `counts.note` inside `addWithStringify`, then we would get an error
because `total` is calling `addAndStringify` passing a record that doesn't have a `note` field.

Record fields can have any combination of types we want. `totalWithNote` uses a record that
has a mixture of numbers and strings, but we can also have record fields that other types of
values - including other records, or even functions!

```coffee
{ birds: 4, nestedRecord: { someFunction: (\arg -> arg + 1), name: "Sam" } }
```

Whenever we're setting a field to be a def that has the same name as the field -
for example, `{ x: x }` - we can shorten it to just writing the name of the def alone -
for example, `{ x }`. We can do this with as many fields as we like, e.g.
`{ x: x, y: y }` can alternately be written `{ x, y }`, `{ x: x, y }`, or `{ x, y: y }`.

### Record destructuring

We can use *destructuring* to avoid naming a record in a function argument, instead
giving names to its individual fields:

```coffee
addAndStringify = \{ birds, iguanas } ->
    Num.toStr (birds + iguanas)
```

Here, we've *destructured* the record to create a `birds` def that's assigned to its `birds`
field, and an `iguanas` def that's assigned to its `iguanas` field. We can customize this if we
like:

```coffee
addAndStringify = \{ birds, iguanas: lizards } ->
    Num.toStr (birds + lizards)
```

In this version, we created a `lizards` def that's assigned to the record's `iguanas` field.
(We could also do something similar with the `birds` field if we like.)

It's possible to destructure a record while still naming it. Here's an example where we
use the `as` keyword to name the record `counts` while also destructuring its fields:

```coffee
addAndStringify = \{ iguanas: lizards } as counts ->
    Num.toStr (counts.birds + lizards)
```

Notice that here we didn't bother destructuring the `birds` field. You can always omit fields
from a destructure if you aren't going to use them!

Finally, destructuring can be used in defs too:

```coffee
{ x, y } = { x: 5, y: 10 }
```

### Building records from other records

So far we've only constructed records from scratch, by specifying all of their fields. We can
also construct new records by using another record to use as a starting point, and then
specifying only the fields we want to be different. For example, here are two ways to
get the same record:

```coffee
original = { birds: 5, iguanas: 7, zebras: 2, goats: 1 }

fromScratch = { birds: 4, iguanas: 3, zebras: 2, goats: 1 }
fromOriginal = { original & birds: 4, iguanas: 3 }
```

The `fromScratch` and `fromOriginal` records are equal, although they're assembled in
different ways.

* `fromScratch` was built using the same record syntax we've been using up to this point.
* `fromOriginal` created a new record using the contents of `original` as defaults for fields that it didn't specify after the `&`.

Note that when we do this, the fields you're overriding must all be present on the original record,
and their values must have the same type as the corresponding values in the original record.

## Tags

Sometimes we want to represent that something can have one of several values. For example:

```coffee
stoplightColor =
    if something > 0 then
        Red
    else if something == 0 then
        Yellow
    else
        Green
```

Here, `stoplightColor` can have one of three values: `Red`, `Yellow`, or `Green`.
The capitalization is very important! If these were lowercase (`red`, `yellow`, `green`),
then they would refer to defs. However, because they are capitalized, they instead
refer to *tags*.

A tag is a literal value just like a number or a string. Similarly to how I can write
the number `42` or the string `"forty-two"` without defining them first, I can also write
the tag `FortyTwo` without defining it first. Also, similarly to how `42 == 42` and
`"forty-two" == "forty-two"`, it's also the case that `FortyTwo == FortyTwo`.

Speaking of equals, if we put `42 == 42` into `roc repl`, the output we'll see is `True`.
This is because booleans in Roc are tags; a boolean is either the tag `True` or the tag
`False`. So I can write `if True then` or `if False then` and it will work as expected,
even though I'd get an error if I wrote `if "true" then` or `if 1 then`. (Roc doesn't
have a concept of "truthiness" - you always have to use booleans for conditionals!)

Let's say we wanted to turn `stoplightColor` from a `Red`, `Green`, or `Yellow` into
a string. Here's one way we could do that:

```elm
stoplightStr =
    if stoplightColor == Red then
        "red"
    else if stoplightColor == Green then
        "green"
    else
        "yellow"
```

We can express this logic more concisely using `when`/`is` instead of `if`/`then`:

```elm
stoplightStr =
    when stoplightColor is
        Red -> "red"
        Green -> "green"
        Yellow -> "yellow"
```

This results in the same value for `stoplightStr`. In both the `when` version and the `if` version, we
have three conditional branches, and each of them evaluates to a string. The difference is how the
conditions are specified; here, we specify between `when` and `is` that we're making comparisons against
`stoplightColor`, and then we specify the different things we're comparing it to: `Red`, `Green`, and `Yellow`.

Besides being more concise, there are other advantages to using `when` here.

1. We don't have to specify an `else` branch, so the code can be more self-documenting about exactly what all the options are.
2. We get more compiler help. If we try deleting any of these branches, we'll get a compile-time error saying that we forgot to cover a case that could come up. For example, if we delete the `Green ->` branch, the compiler will say that we didn't handle the possibility that `stoplightColor` could be `Green`. It knows this because `Green` is one of the possibilities in our `stoplightColor = if …` definition.

We can still have the equivalent of an `else` branch in our `when` if we like. Instead of writing "else", we write
"_ ->" like so:

```coffee
stoplightStr =
    when stoplightColor is
        Red -> "red"
        _ -> "not red"
```

This lets us more concisely handle multiple cases. However, it has the downside that if we add a new case -
for example, if we introduce the possibility of `stoplightColor` being `Orange`, the compiler can no longer
tell us we forgot to handle that possibility in our `when`. After all, we are handling it - just maybe not
in the way we'd decide to if the compiler had drawn our attention to it!

We can make this `when` *exhaustive* (that is, covering all possibilities) without using `_ ->` by using
`|` to specify multiple matching conditions for the same branch:

```coffee
stoplightStr =
    when stoplightColor is
        Red -> "red"
        Green | Yellow -> "not red"
```

You can read `Green | Yellow` as "either `Green` or `Yellow`". By writing it this way, if we introduce the
possibility that `stoplightColor` can be `Orange`, we'll get a compiler error telling us we forgot to cover
that case in this `when`, and then we can handle it however we think is best.

We can also combine `if` and `when` to make branches more specific:

```coffee
stoplightStr =
    when stoplightColor is
        Red -> "red"
        Green | Yellow if contrast > 75 -> "not red, but very high contrast"
        Green | Yellow if contrast > 50 -> "not red, but high contrast"
        Green | Yellow -> "not red"
```

This will give the same answer for `spotlightStr` as if we had written the following:

```coffee
stoplightStr =
    when stoplightColor is
        Red -> "red"
        Green | Yellow ->
            if contrast > 75 then
                "not red, but very high contrast"
            else if saturation > 50 then
                "not red, but high contrast"
            else
                "not red"
```

Either style can be a reasonable choice depending on the cirumstances.

### Tags with payloads

Tags can have *payloads* - that is, values contained within them. For example:

```coffee
stoplightColor =
    if something > 100 then
        Red
    else if something > 0 then
        Yellow
    else if something == 0 then
        Green
    else
        Custom "some other color"

stoplightStr =
    when stoplightColor is
        Red -> "red"
        Green | Yellow -> "not red"
        Custom description -> description
```

This makes two changes to our earlier `stoplightColor` / `stoplightStr` example.

1. We sometimes set `stoplightColor` to be `Custom "some other color"`. When we did this, we gave the `Custom` tag a *payload* of the string `"some other color"`.
2. We added a `Custom` tag in our `when`, with a payload which we named `description`. Because we did this, we were able to refer to `description` in the body of the branch (that is, the part after the `->`) just like any other def.

Any tag can be given a payload like this. A payload doesn't have to be a string; we could also have said (for example) `Custom { r: 40, g: 60, b: 80 }` to specify an RGB color instead of a string. Then in our `when` we could have written `Custom record ->` and then after the `->` used `record.r`, `record.g`, and `record.b` to access the `40`, `60`, `80` values. We could also have written `Custom { r, g, b } ->` to *destructure* the record, and then
accessed these `r`, `g`, and `b` defs after the `->` instead.

## Lists

Another thing we can do in Roc is to make a *list* of values. Here's an example:

```coffee
names = [ "Sam", "Lee", "Ari" ]
```

This is a list with three elements in it, all strings. We can add a fourth
element using `List.append` like so:

```coffee
List.append names "Jess"
```

This returns a **new** list with `"Jess"` after `"Ari"`, and doesn't modify the original list at all.
All values in Roc (including lists, but also records, strings, numbers, and so on) are immutable,
meaning whenever we want to "change" them, we want to instead pass them a function which returns some
variation of what was passed in.

### List.map

A common way to transform one list into another is to use `List.map`. Here's an example of how to
use it:

```coffee
List.map [ 1, 2, 3 ] \num -> num * 2
```

This returns `[ 2, 4, 6 ]`. `List.map` takes two arguments:

1. An input list
2. A function that will be called on each element of that list

It then returns a list which it creates by calling the given function on each element in the input list.
In this example, `List.map` calls the function `\num -> num * 2` on each element in
`[ 1, 2, 3 ]` to get a new list of `[ 2, 4, 6 ]`.

We can also give `List.map` a named function, instead of an anonymous one:

For example, the `Num.isOdd` function returns `True` if it's given an odd number, and `False` otherwise.
So `Num.isOdd 5` returns `True` and `Num.isOdd 2` returns `False`.

So calling `List.map [ 1, 2, 3 ] Num.isOdd` returns a new list of `[ True, False, True ]`.

### List element type compatibility

If we tried to give `List.map` a function that didn't work on the elements in the list, then we'd get
an error at compile time. Here's a valid, and then an invalid example:

```coffee
# working example
List.map [ -1, 2, 3, -4 ] Num.isNegative
# returns [ True, False, False, True ]
```

```coffee
# invalid example
List.map [ "A", "B", "C" ] Num.isNegative
# error: isNegative doesn't work on strings!
```

Because `Num.isNegative` works on numbers and not strings, calling `List.map` with `Num.isNegative` and a
list of numbers works, but doing the same with a list of strings doesn't work.

This wouldn't work either:

```coffee
List.map [ "A", "B", "C", 1, 2, 3 ] Num.isNegative
```

In fact, this wouldn't work for a more fundamental reason: every element in a Roc list has to share the same type.
For example, we can have a list of strings like `[ "Sam", "Lee", "Ari" ]`, or a list of numbers like
`[ 1, 2, 3, 4, 5 ]` but we can't have a list which mixes strings and numbers like `[ "Sam", 1, "Lee", 2, 3 ]` -
that would be a compile-time error.

Ensuring all elements in a list share a type eliminates entire categories of problems.
For example, it means that whenever you use `List.append` to
add elements to a list, as long as you don't have any compile-time errors, you won't get any runtime errors
from calling `List.map` afterwards - no matter what you appended to the list! More generally, it's safe to assume
that unless you run out of memory, `List.map` will run successfully unless you got a compile-time error about an
incompatibility (like `Num.negate` on a list of strings).

### Lists that hold elements of different types

We can use tags with payloads to make a list that contains a mixture of different types. For example:

```coffee
List.map [ StrElem "A", StrElem "b", NumElem 1, StrElem "c", NumElem -3 ] \elem ->
    when elem is
        NumElem num -> Num.isNegative num
        StrElem str -> Str.isCapitalized str

# returns [ True, False, False, False, True ]
```

Compare this with the example from earlier, which caused a compile-time error:

```coffee
List.map [ "A", "B", "C", 1, 2, 3 ] Num.isNegative
```

The version that uses tags works because we aren't trying to call `Num.isNegative` on each element.
Instead, we're using a `when` to tell when we've got a string or a number, and then calling either
`Num.isNegative` or `Str.isCapitalized` depending on which type we have.

We could take this as far as we like, adding more different tags (e.g. `BoolElem True`) and then adding
more branches to the `when` to handle them appropriately.

### `List.any` and `List.all`

There are several functions that work like `List.map` - they walk through each element of a list and do
something with it. Another is `List.any`, which returns `True` if calling the given function on any element
in the list returns `True`:

```coffee
List.any [ 1, 2, 3 ] Num.isOdd
# returns True because 1 and 3 are odd
```
```coffee
List.any [ 1, 2, 3 ] Num.isNegative
# returns False because none of these is negative
```

There's also `List.all` which only returns `True` if all the elements in the list pass the test:

```coffee
List.all [ 1, 2, 3 ] Num.isOdd
# returns False because 2 is not odd
```
```coffee
List.all [ 1, 2, 3 ] Num.isPositive
# returns True because all of these are positive
```

### Removing elements from a list

You can also drop elements from a list. One way is `List.dropAt` - for example:

```coffee
List.dropAt [ "Sam", "Lee", "Ari" ] 1
# drops the element at offset 1 ("Lee") and returns [ "Sam", "Ari" ]
```

Another way is to use `List.keepIf`, which passes each of the list's elements to the given
function, and then keeps them only if that function returns `True`.

```coffee
List.keepIf [ 1, 2, 3, 4, 5 ] Num.isEven
# returns [ 2, 4 ]
```

There's also `List.dropIf`, which does the reverse:

```coffee
List.dropIf [ 1, 2, 3, 4, 5 ] Num.isEven
# returns [ 1, 3, 5 ]
```

### Custom operations that walk over a list

You can make your own custom operations that walk over all the elements in a list, using `List.walk`.
Let's look at an example and then walk (ha!) through it.

```coffee
List.walk [ 1, 2, 3, 4, 5 ] { evens: [], odds: [] } \state, elem ->
    if Num.isEven elem then
        { state & evens: List.append state.evens elem }
    else
        { state & odds: List.append state.odds elem }

# returns { evens: [ 2, 4 ], odds: [ 1, 3, 5 ] }
```

`List.walk` walks through each element of the list, building up a state as it goes. At the end,
it returns the final state - whatever it ended up being after processing the last element. The `\state, elem ->`
function it takes as its last argument accepts both the current state as well as the current list element
it's looking at, and then returns the new state based on whatever it decides to do with that element.

In this example, we walk over the list `[ 1, 2, 3, 4, 5 ]` and add each element to either the `evens` or `odds`
field of a `state` record `{ evens, odds }`. By the end, that record has a list of all the even numbers in the
list as well as a list of all the odd numbers.

The state doesn't have to be a record; it can be anything you want. For example, if you made it a boolean, you
could implement `List.any` using `List.walk`. You could also make the state be a list, and implement `List.map`,
`List.keepIf`, or `List.dropIf`. There are a lot of things you can do with `List.walk` - it's very flexible!

It can be tricky to remember the argument order for `List.walk` at first. A helpful trick is that the arguments
follow the same pattern as what we've seen with `List.map`, `List.any`, `List.keepIf`, and `List.dropIf`: the
first argument is a list, and the last argument is a function. The difference here is that `List.walk` has one
more argument than those other functions; the only place it could go while preserving that pattern is the middle!

That third argument specifies the initial `state` - what it's set to before the `\state, elem ->` function has
been called on it even once. (If the list is empty, the `\state, elem ->` function will never get called and
the initial state gets returned immediately.)

> **Note:** Other languages give this operation different names, such as "fold," "reduce," "accumulate,"
> "aggregate," "compress," and "inject."

### Getting an individual element from a list

Another thing we can do with a list is to get an individual element out of it. `List.get` is a common way to do this;
it takes a list and an index, and then returns the element at that index...if there is one. But what if there isn't?

For example, what do each of these return?

```coffee
List.get [ "a", "b", "c" ] 1
```
```coffee
List.get [ "a", "b", "c" ] 100
```

The answer is that the first one returns `Ok "b"` and the second one returns `Err OutOfBounds`.
They both return tags! This is done so that the caller becomes responsible for handling the possibility that
the index is outside the bounds of that particular list.

Here's how calling `List.get` can look in practice:

```coffee
when List.get [ "a", "b", "c" ] index is
    Ok str -> "I got this string: \(str)"
    Err OutOfBounds -> "That index was out of bounds, sorry!"
```

There's also `List.first`, which always gets the first element, and `List.last` which always gets the last.
They return `Err ListWasEmpty` instead of `Err OutOfBounds`, because the only way they can fail is if you
pass them an empty list!

These functions demonstrate a common pattern in Roc: operations that can fail returning either an `Ok` tag
with the answer (if successful), or an `Err` tag with another tag describing what went wrong (if unsuccessful).
In fact, it's such a common pattern that there's a whole module called `Result` which deals with these two tags.
Here are some examples of `Result` functions:

```coffee
Result.withDefault (List.get [ "a", "b", "c" ] 100) ""
# returns "" because that's the default we said to use if List.get returned an Err
```

```coffee
Result.isOk (List.get [ "a", "b", "c" ] 1)
# returns True because `List.get` returned an `Ok` tag. (The payload gets ignored.)

# Note: There's a Result.isErr function that works similarly.
```

### The pipe operator

When you have nested function calls, sometimes it can be clearer to write them in a "pipelined"
style using the `|>` operator. Here are three examples of writing the same expression; they all
compile to exactly the same thing, but two of them use the `|>` operator to change how the calls look.

```coffee
Result.withDefault (List.get [ "a", "b", "c" ] 1) ""
```

```coffee
List.get [ "a", "b", "c" ] 1
    |> Result.withDefault ""
```

The `|>` operator takes the value that comes before the `|>` and passes it as the first argument to whatever
comes after the `|>` - so in the example above, the `|>` takes `List.get [ "a", "b", "c" ] 1` and passes that
value as the first argument to `Result.withDefault` - making `""` the second argument to `Result.withDefault`.

We can take this a step further like so:

```coffee
[ "a", "b", "c" ]
    |> List.get 1
    |> Result.withDefault ""
```

This is still equivalent to the first expression. Since `|>` is known as the "pipe operator," we can read
this as "start with `[ "a", "b", "c" ]`, then pipe it to `List.get`, then pipe it to `Result.withDefault`."

One reason the `|>` operator injects the value as the first argument is to make it work better with
functions where argument order matters. For example, these two uses of `List.append` are equivalent:

```coffee
List.append [ "a", "b", "c" ] "d"
```
```coffee
[ "a", "b", "c" ]
    |> List.append "d"
```

Another example is `Num.div`. All three of the following do the same thing, because `a / b` in Roc is syntax
sugar for `Num.div a b`:

```coffee
first / second
```
```coffee
Num.div first second
```
```coffee
first
    |> Num.div second
```

All operators in Roc are syntax sugar for normal function calls. See the "Operator Desugaring Table"
at the end of this tutorial for a complete list of them.

## Types

Sometimes you may want to document the type of a definition. For example, you might write:

```ruby
# Takes a firstName string and a lastName string, and returns a string
fullName = \firstName, lastName ->
    "\(firstName) \(lastName)"
```

Comments can be valuable documentation, but they can also get out of date and become misleading.
If someone changes this function and forgets to update the comment, it will no longer be accurate.

### Type annotations

Here's another way to document this function's type, which doesn't have that problem:

```coffee
fullName : Str, Str -> Str
fullName = \firstName, lastName ->
    "\(firstName) \(lastName)"
```

The `fullName :` line is a *type annotation*. It's a strictly optional piece of metadata we can add
above a def to describe its type. Unlike a comment, the Roc compiler will check type annotations for
accuracy. If the annotation ever doesn't fit with the implementation, we'll get a compile-time error.

The annotation `fullName : Str, Str -> Str` says "`fullName` is a function that takes two strings as
arguments and returns a string."

We can give type annotations to any value, not just functions. For example:

```coffee
firstName : Str
firstName = "Amy"

lastName : Str
lastName = "Lee"
```

These annotations say that both `firstName` and `lastName` have the type `Str`.

We can annotate records similarly. For example, we could move `firstName` and `lastName` into a record like so:

```coffee
amy : { firstName : Str, lastName : Str }
amy = { firstName: "Amy", lastName: "Lee" }

jen : { firstName : Str, lastName : Str }
jen = { firstName: "Jen", lastName: "Majura" }
```

When we have a recurring type annotation like this, it can be nice to give it its own name. We do this like
so:

```coffee
Musician : { firstName : Str, lastName : Str }

amy : Musician
amy = { firstName: "Amy", lastName: "Lee" }

simone : Musician
simone = { firstName: "Simone", lastName: "Simons" }
```

Here, `Musician` is a *type alias*. A type alias is like a def, except it gives a name to a type
instead of to a value. Just like how you can read `name : Str` as "`name` has the type `Str`,"
you can also read `Musician : { firstName : Str, lastName : Str }` as "`Musician` has the type
`{ firstName : Str, lastName : Str }`."

We can also give type annotations to tag unions:

```coffee
colorFromStr : Str -> [ Red, Green, Yellow ]
colorFromStr = \string ->
    when string is
        "red" -> Red
        "green" -> Green
        _ -> Yellow
```

You can read the type `[ Red, Green, Yellow ]` as "a tag union of the tags `Red`, `Green`, and `Yellow`."

When we annotate a list type, we have to specify the type of its elements:

```coffee
names : List Str
names = [ "Amy", "Simone", "Tarja" ]
```

You can read `List Str` as "a list of strings." Here, `Str` is a *type parameter* that tells us what type of
`List` we're dealing with. `List` is a *parameterized type*, which means it's a type that requires a type
parameter; there's no way to give something a type of `List` without a type parameter - you have to specify
what type of list it is, such as `List Str` or `List Bool` or `List { firstName : Str, lastName : Str }`.

There are some functions that work on any list, regardless of its type parameter. For example, `List.isEmpty`
has this type:

```coffee
isEmpty : List * -> Bool
```

The `*` is a *wildcard type* - that is, a type that's compatible with any other type. `List *` is compatible
with any type of `List` - so, `List Str`, `List Bool`, and so on. So you can call
`List.isEmpty [ "I am a List Str" ]` as well as `List.isEmpty [ True ]`, and they will both work fine.

The wildcard type also comes up with empty lists. Suppose we have one function that takes a `List Str` and another
function that takes a `List Bool`. We might reasonably expect to be able to pass an empty list (that is, `[]`) to
either of these functions. And so we can! This is because a `[]` value has the type `List *` - that is,
"a list with a wildcard type parameter," or "a list whose element type could be anything."

`List.reverse` works similarly to `List.isEmpty`, but with an important distinction. As with `isEmpty`, we can
call `List.reverse` on any list, regardless of its type parameter. However, consider these calls:

```coffee
strings : List Str
strings = List.reverse [ "a", "b" ]

bools : List Bool
bools = List.reverse [ True, False ]
```

In the `strings` example, we have `List.reverse` returning a `List Str`. In the `bools` example, it's returning a
`List Bool`. So what's the type of `List.reverse`?

We saw that `List.isEmpty` has the type `List * -> Bool`, so we might think the type of `List.reverse` would be
`reverse : List * -> List *`. However, remember that we also saw that the type of the empty list is `List *`?
`List * -> List *` is actually the type of a function that always returns empty lists! That's not what we want.

What we want is something like one of these:

```coffee
reverse : List elem -> List elem
```
```coffee
reverse : List value -> List value
```
```coffee
reverse : List a -> List a
```

Any of these will work, because `elem`, `value`, and `a` are all *type variables*. A type variable connects
two or more types in the same annotation. So you can read `List elem -> List elem` as "takes a list and returns
a list that has the same element type." Just like `List.reverse` does!

You can choose any name you like for a type variable, but it has to be lowercase. (You may have noticed all the
types we've used until now are uppercase; that is no accident! Lowercase types are always type variables, so
all other named types have to be uppercase.) All three of the above type annotations are equivalent;
the only difference is that we chose different names (`elem`, `value`, and `a`) for their type variables.

You can tell some interesting things about functions based on the type parameters involved. For example,
any function that returns `List *` definitely always returns an empty list. You don't need to look at the rest
of the type annotation, or even the function's implementation! The only way to have a function that returns
`List *` is if it returns an empty list.

Similarly, the only way to have a function whose type is `a -> a` is if the function's implementation returns
its argument without modifying it in any way. This is known as [the identity function](https://en.wikipedia.org/wiki/Identity_function).

### Numeric types

[ This part of the tutorial has not been written yet. Coming soon! ]

### Open and closed records

[ This part of the tutorial has not been written yet. Coming soon! ]

### Open and closed tag unions

[ This part of the tutorial has not been written yet. Coming soon! ]

## Interface modules

[ This part of the tutorial has not been written yet. Coming soon! ]

## Builtin modules

There are several modules that are built into the Roc compiler, which are imported automatically into every
Roc module. They are:

1. `Bool`
2. `Str`
3. `Num`
4. `List`
5. `Result`
6. `Dict`
7. `Set`

You may have noticed that we already used the first five - for example, when we wrote `Str.concat` and `Num.isEven`,
we were referencing functions stored in the `Str` and `Num` modules.

These modules are not ordinary `.roc` files that live on your filesystem. Rather, they are built directly into the
Roc compiler. That's why they're called "builtins!"

Besides being built into the compiler, the builtin modules are different from other modules in that:

* They are always imported. You never need to add them to `imports`.
* All their types are imported unqualified automatically. So you never need to write `Num.Nat`, because it's as if the `Num` module was imported using `imports [ Num.{ Nat } ]` (and the same for all the other types in the `Num` module).

## The app module header

Let's take a closer look at the part of `Hello.roc` above `main`:

```coffee
app "hello"
    packages { pf: "examples/cli/platform" }
    imports [ pf.Stdout ]
    provides main to pf
```

This is known as a *module header*. Every `.roc` file is a *module*, and there
are different types of modules. We know this particular one is an *application module*
(or *app module* for short) because it begins with the `app` keyword.

The line `app "hello"` states that this module defines a Roc application, and
that building this application should produce an executable named `hello`. This
means when you run `roc Hello.roc`, the Roc compiler will build an executable
named `hello` (or `hello.exe` on Windows) and run it. You can also build the executable
without running it by running `roc build Hello.roc`.

The remaining lines all involve the *platform* this application is built on:

```coffee
packages { pf: "examples/cli/platform" }
imports [ pf.Stdout ]
provides main to pf
```

The `packages { pf: "examples/cli/platform" }` part says two things:

- We're going to be using a *package* (that is, a collection of modules) called `"examples/cli/platform"`
- We're going to name that package `pf` so we can refer to it more concisely in the future.

The `imports [ pf.Stdout ]` line says that we want to import the `Stdout` module
from the `pf` package, and make it available in the current module.

This import has a direct interaction with our definition of `main`. Let's look
at that again:

```coffee
main = Stdout.line "I'm a Roc application!"
```

Here, `main` is calling a function called `Stdout.line`. More specifically, it's
calling a function named `line` which is exposed by a module named
`Stdout`.

When we write `imports [ pf.Stdout ]`, it specifies that the `Stdout`
module comes from the `pf` package.

Since `pf` was the name we chose for the `examples/cli/platform` package
(when we wrote `packages { pf: "examples/cli/platform" }`), this `imports` line
tells the Roc compiler that when we call `Stdout.line`, it should look for that
`line` function in the `Stdout` module of the `examples/cli/platform` package.

# Building a Command-Line Interface (CLI)

## Tasks

Tasks are technically not part of the Roc language, but they're very common in
platforms. Let's use the CLI platform in `examples/cli` as an example!

In the CLI platform, we have four operations we can do:

* Write a string to the console
* Read a string from user input
* Write a string to a file
* Read a string from a file

We'll use these four operations to learn about tasks.

First, let's do a basic "Hello World" using the tutorial app.

```coffee
app "cli-tutorial"
    packages { pf: "examples/cli/platform" }
    imports [ pf.Stdout ]
    provides [ main ] to pf

main =
    Stdout.line "Hello, World!"
```

The `Stdout.line` function takes a `Str` and writes it to [standard output](https://en.wikipedia.org/wiki/Standard_streams#Standard_output_(stdout)).
It has this type:

```coffee
Stdout.line : Str -> Task {} *
```

A `Task` represents an *effect* - that is, an interaction with state outside your Roc program,
such as the console's standard output, or a file.

When we set `main` to be a `Task`, the task will get run when we run our program. Here, we've set
`main` to be a task that writes `"Hello, World!"` to `stdout` when it gets run, so that's what
our program does!

`Task` has two type parameters: the type of value it produces when it finishes running, and any
errors that might happen when running it. `Stdout.line` has the type `Task {} *` because it doesn't
produce any values when it finishes (hence the `{}`) and there aren't any errors that can happen
when it runs (hence the `*`).

In contrast, `Stdin.line` produces a `Str` when it finishes reading from [standard input](https://en.wikipedia.org/wiki/Standard_streams#Standard_input_(stdin)). That `Str` is reflected in its type:

```coffee
Stdin.line : Task Str *
```

Let's change `main` to read a line from `stdin`, and then print it back out again:

```swift
app "cli-tutorial"
    packages { pf: "examples/cli/platform" }
    imports [ pf.Stdout, pf.Stdin, pf.Task ]
    provides [ main ] to pf

main =
    Task.await Stdin.line \text ->
        Stdout.line "You just entered: \(text)"
```

If you run this program, at first it won't do anything. It's waiting for you to type something
in and press Enter! Once you do, it should print back out what you entered.

The `Task.await` function combines two tasks into one bigger `Task` which first runs one of the
given tasks and then the other. In this case, it's combining a `Stdin.line` task with a `Stdout.line`
task into one bigger `Task`, and then setting `main` to be that bigger task.

The type of `Task.await` is:

```haskell
Task.await : Task a err, (a -> Task b err) -> Task b err
```

The second argument to `Task.await` is a "callback function" which runs after the first task
completes. This callback function receives the output of that first task, and then returns
the second task.  This means the second task can make use of output from the first task, like
we did in our `\text -> …` callback function here:

```swift
\text ->
    Stdout.line "You just entered: \(text)"
```

Notice that, just like before, we're still setting `main` to be a single `Task`. This is how we'll
always do it! We'll keep building up bigger and bigger `Task`s out of smaller tasks, and then setting
`main` to be that one big `Task`.

For example, we can print a prompt before we pause to read from `stdin`, so it no longer looks like
the program isn't doing anything when we start it up:

```swift
main =
    Task.await (Stdout.line "Type something press Enter:") \_ ->
        Task.await Stdin.line \text ->
            Stdout.line "You just entered: \(text)"
```

This works, but we can make it a little nicer to read. Let's change it to the following:

```haskell
app "cli-tutorial"
    packages { pf: "examples/cli/platform" }
    imports [ pf.Stdout, pf.Stdin, pf.Task.{ await } ]
    provides [ main ] to pf

main =
    await (Stdout.line "Type something press Enter:") \_ ->
        await Stdin.line \text ->
            Stdout.line "You just entered: \(text)"
```

Here we've changed how we're importing the `Task` module. Before it was
`pf.Task` and now it's `pf.Task.{ await }`. The difference is that we're
importing `await` in an *unqualified* way, meaning now whenever we write `await`
in this module, it will refer to `Task.await` - so we no longer need to write
`Task.` every time we want to `await`.

It's most common in Roc to call functions from other modules in a *qualified* way
(`Task.await`) rather than unqualified (`await`) like this, but it can be nice
for a function with an uncommon name (like "await") which often gets called repeatedly
across a small number of lines of code.

Speaking of calling `await` repeatedly, if we keep calling it more and more on this
code, we'll end up doing a lot of indenting. If we'd rather not indent so much, we
can rewrite `main` into this style which looks different but does the same thing:

```swift
main =
    _ <- await (Stdout.line "Type something press Enter:")
    text <- await Stdin.line

    Stdout.line "You just entered: \(text)"
```

This `<-` syntax is called *backpassing*. The `<-` is a way to define an
anonymous function, just like `\ … ->` is.

Here, we're using backpassing to define two anonymous functions. Here's one of them:

```swift
text <-

Stdout.line "You just entered: \(text)"
```

It may not look like it, but this code is defining an anonymous function! You might
remember it as the anonymous function we previously defined like this:

```swift
\text ->
    Stdout.line "You just entered: \(text)"
```

These two anonymous functions are the same, just defined using different syntax.

The reason the `<-` syntax is called *backpassing* is because it both defines a
function and passes that function *back* as an argument to whatever comes after
the `<-` (which in this case is `await Stdin.line`).

Let's look at these two complete expressions side by side. They are both
saying exactly the same thing, with different syntax!

Here's the original:

```swift
await Stdin.line \text ->
    Stdout.line "You just entered: \(text)"
```

And here's the equivalent expression with backpassing syntax:

```swift
text <- await Stdin.line

Stdout.line "You just entered: \(text)"
```

Here's the other function we're defining with backpassing:

```swift
_ <-
text <- await Stdin.line

Stdout.line "You just entered: \(text)"
```

We could also have written that function this way if we preferred:

```swift
_ <-

await Stdin.line \text ->
    Stdout.line "You just entered: \(text)"
```

This is using a mix of a backpassing function `_ <-` and a normal function `\text ->`,
which is totally allowed! Since backpassing is nothing more than syntax sugar for
defining a function and passing back as an argument to another function, there's no
reason we can't mix and match if we like.

That said, the typical style in which this `main` would be written in Roc is using
backpassing for all the `await` calls, like we had above:

```swift
main =
    _ <- await (Stdout.line "Type something press Enter:")
    text <- await Stdin.line

    Stdout.line "You just entered: \(text)"
```

This way, it reads like a series of instructions:
1. First, run the `Stdout.line` task and await its completion. Ignore its output (hence the underscore in `_ <-`)
2. Next, run the `Stdin.line` task and await its completion. Name its output `text`.
3. Finally, run the `Stdout.line` task again, using the `text` value we got from the `Stdin.line` effect.

Some important things to note about backpassing and `await`:
* `await` is not a language keyword in Roc! It's referring to the `Task.await` function, which we imported unqualified by writing `Task.{ await }` in our module imports. (That said, it is playing a similar role here to the `await` keyword in languages that have `async`/`await` keywords, even though in this case it's a function instead of a special keyword.)
* Backpassing syntax does not need to be used with `await` in particular. It can be used with any function.
* Roc's compiler treats functions defined with backpassing exactly the same way as functions defined the other way. The only difference between `\text ->` and `text <-` is how they look, so feel free to use whichever looks nicer to you!

## Operator Desugaring Table

Here are various Roc expressions involving operators, and what they desugar to.

| Expression      | Desugars to      |
| --------------- | ---------------- |
| `a + b`           | `Num.add a b`      |
| `a - b`           | `Num.sub a b`      |
| `a * b`           | `Num.mul a b`      |
| `a / b`           | `Num.div a b`    |
| `a // b`          | `Num.divFloor a b`      |
| `a ^ b`           | `Num.pow a b`      |
| `a % b`           | `Num.rem a b`    |
| `a %% b`          | `Num.mod a b`    |
| `a >> b`          | `Num.shr a b`    |
| `a << b`          | `Num.shl a b`    |
| `-a`              | `Num.neg a`        |
| `-f x y`          | `Num.neg (f x y)`  |
| `a == b`          | `Bool.isEq a b`    |
| `a != b`          | `Bool.isNotEq a b` |
| `a && b`          | `Bool.and a b`     |
| `a \|\| b`          | `Bool.or a b`      |
| `!a`              | `Bool.not a`       |
| `!f x y`          | `Bool.not (f x y)` |
| `a \|> b`          | `b a`              |
| `a b c \|> f x y`  | `f (a b c) x y`    |
