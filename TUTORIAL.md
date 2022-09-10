# Tutorial

This is a tutorial for how to build Roc applications. It covers the REPL, basic
types (strings, lists, tags, and functions), syntax (`when`, `if then else`)
and more!

Enjoy!

## Getting started

Learn how to install roc on your machine [here](https://github.com/roc-lang/roc/tree/main/getting_started#installation).

## Strings and Numbers

Let’s start by getting acquainted with Roc’s Read Eval Print Loop, or REPL for
short. Run this in a terminal:

```sh
$ roc repl
```

You should see this:

```sh
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
    packages { pf: "examples/interactive/cli-platform/main.roc" }
    imports [pf.Stdout]
    provides [main] to pf

main = Stdout.line "I'm a Roc application!"
```

> **NOTE:** This assumes you've put Hello.roc in the root directory of the Roc
> source code. If you'd like to put it somewhere else, you'll need to replace
> `"examples/interactive/cli-platform/main.roc"` with the path to the
> `examples/interactive/cli-platform/main.roc` file in that source code. In the future,
> Roc will have the tutorial built in, and this aside will no longer be
> necessary!

Try running this with:

```sh
$ roc Hello.roc
```

You should see this:

```sh
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

```sh
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

- We introduced a local def named `sum`, and set it equal to `num1 + num2`. Because we defined `sum` inside `addAndStringify`, it will not be accessible outside that function.
- We added an `if` / `then` / `else` conditional to return either `""` or `Num.toStr sum` depending on whether `sum == 0`.

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
`iguanas` field. Order doesn't matter with record fields; we could have also specified
`iguanas` first and `birds` second, and Roc would consider it the exact same record.

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

This works because `addAndStringify` only uses `counts.birds` and `counts.iguanas`.
If we were to use `counts.note` inside `addAndStringify`, then we would get an error
because `total` is calling `addAndStringify` passing a record that doesn't have a `note` field.

Record fields can have any combination of types we want. `totalWithNote` uses a record that
has a mixture of numbers and strings, but we can also have record fields with other types of
values - including other records, or even functions!

```coffee
{ birds: 4, nestedRecord: { someFunction: (\arg -> arg + 1), name: "Sam" } }
```

### Record shorthands

Roc has a couple of shorthands you can use to express some record-related operations more concisely.

Instead of writing `\record -> record.x` we can write `.x` and it will evaluate to the same thing:
a function that takes a record and returns its `x` field. You can do this with any field you want.
For example:

```elm
returnFoo = .foo

returnFoo { foo: "hi!", bar: "blah" }
# returns "hi!"
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

- `fromScratch` was built using the same record syntax we've been using up to this point.
- `fromOriginal` created a new record using the contents of `original` as defaults for fields that it didn't specify after the `&`.

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

This will give the same answer for `stoplightStr` as if we had written the following:

```coffee
stoplightStr =
    when stoplightColor is
        Red -> "red"
        Green | Yellow ->
            if contrast > 75 then
                "not red, but very high contrast"
            else if contrast > 50 then
                "not red, but high contrast"
            else
                "not red"
```

Either style can be a reasonable choice depending on the circumstances.

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

A tag can also have a payload with more than one value. Instead of `Custom { r: 40, g: 60, b: 80 }` we could
write `Custom 40 60 80`. If we did that, then instead of destructuring a record with `Custom { r, g, b } ->`
inside a `when`, we would write `Custom r g b ->` to destructure the values directly out of the payload.

We refer to whatever comes before a `->` in a `when` expression as a *pattern* - so for example, in the
`Custom description -> description` branch, `Custom description` would be a pattern. In programming, using
patterns in branching conditionals like `when` is known as [pattern matching](https://en.wikipedia.org/wiki/Pattern_matching). You may hear people say things like "let's pattern match on `Custom` here" as a way to
suggest making a `when` branch that begins with something like `Custom description ->`.

## Lists

Another thing we can do in Roc is to make a *list* of values. Here's an example:

```coffee
names = ["Sam", "Lee", "Ari"]
```

This is a list with three elements in it, all strings. We can add a fourth
element using `List.append` like so:

```coffee
List.append names "Jess"
```

This returns a **new** list with `"Jess"` after `"Ari"`, and doesn't modify the original list at all.
All values in Roc (including lists, but also records, strings, numbers, and so on) are immutable,
meaning whenever we want to "change" them, we want to instead pass them to a function which returns some
variation of what was passed in.

### List.map

A common way to transform one list into another is to use `List.map`. Here's an example of how to
use it:

```coffee
List.map [1, 2, 3] \num -> num * 2
```

This returns `[2, 4, 6]`. `List.map` takes two arguments:

1. An input list
2. A function that will be called on each element of that list

It then returns a list which it creates by calling the given function on each element in the input list.
In this example, `List.map` calls the function `\num -> num * 2` on each element in
`[1, 2, 3]` to get a new list of `[2, 4, 6]`.

We can also give `List.map` a named function, instead of an anonymous one:

For example, the `Num.isOdd` function returns `True` if it's given an odd number, and `False` otherwise.
So `Num.isOdd 5` returns `True` and `Num.isOdd 2` returns `False`.

So calling `List.map [1, 2, 3] Num.isOdd` returns a new list of `[True, False, True]`.

### List element type compatibility

If we tried to give `List.map` a function that didn't work on the elements in the list, then we'd get
an error at compile time. Here's a valid, and then an invalid example:

```coffee
# working example
List.map [-1, 2, 3, -4] Num.isNegative
# returns [True, False, False, True]
```

```coffee
# invalid example
List.map ["A", "B", "C"] Num.isNegative
# error: isNegative doesn't work on strings!
```

Because `Num.isNegative` works on numbers and not strings, calling `List.map` with `Num.isNegative` and a
list of numbers works, but doing the same with a list of strings doesn't work.

This wouldn't work either:

```coffee
List.map ["A", "B", "C", 1, 2, 3] Num.isNegative
```

In fact, this wouldn't work for a more fundamental reason: every element in a Roc list has to share the same type.
For example, we can have a list of strings like `["Sam", "Lee", "Ari"]`, or a list of numbers like
`[1, 2, 3, 4, 5]` but we can't have a list which mixes strings and numbers like `["Sam", 1, "Lee", 2, 3]` -
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
List.map [StrElem "A", StrElem "b", NumElem 1, StrElem "c", NumElem -3] \elem ->
    when elem is
        NumElem num -> Num.isNegative num
        StrElem str -> Str.isCapitalized str

# returns [True, False, False, False, True]
```

Compare this with the example from earlier, which caused a compile-time error:

```coffee
List.map ["A", "B", "C", 1, 2, 3] Num.isNegative
```

The version that uses tags works because we aren't trying to call `Num.isNegative` on each element.
Instead, we're using a `when` to tell when we've got a string or a number, and then calling either
`Num.isNegative` or `Str.isCapitalized` depending on which type we have.

We could take this as far as we like, adding more different tags (e.g. `BoolElem True`) and then adding
more branches to the `when` to handle them appropriately.

### Using tags as functions

Let's say I want to apply a tag to a bunch of elements in a list. For example:

```elm
List.map ["a", "b", "c"] \str -> Foo str
```

This is a perfectly reasonable way to write it, but I can also write it like this:

```elm
List.map ["a", "b", "c"] Foo
```

These two versions compile to the same thing. As a convenience, Roc lets you specify
a tag name where a function is expected; when you do this, the compiler infers that you
want a function which uses all of its arguments as the payload to the given tag.

### `List.any` and `List.all`

There are several functions that work like `List.map` - they walk through each element of a list and do
something with it. Another is `List.any`, which returns `True` if calling the given function on any element
in the list returns `True`:

```coffee
List.any [1, 2, 3] Num.isOdd
# returns True because 1 and 3 are odd
```

```coffee
List.any [1, 2, 3] Num.isNegative
# returns False because none of these is negative
```

There's also `List.all` which only returns `True` if all the elements in the list pass the test:

```coffee
List.all [1, 2, 3] Num.isOdd
# returns False because 2 is not odd
```

```coffee
List.all [1, 2, 3] Num.isPositive
# returns True because all of these are positive
```

### Removing elements from a list

You can also drop elements from a list. One way is `List.dropAt` - for example:

```coffee
List.dropAt ["Sam", "Lee", "Ari"] 1
# drops the element at offset 1 ("Lee") and returns ["Sam", "Ari"]
```

Another way is to use `List.keepIf`, which passes each of the list's elements to the given
function, and then keeps them only if that function returns `True`.

```coffee
List.keepIf [1, 2, 3, 4, 5] Num.isEven
# returns [2, 4]
```

There's also `List.dropIf`, which does the reverse:

```coffee
List.dropIf [1, 2, 3, 4, 5] Num.isEven
# returns [1, 3, 5]
```

### Custom operations that walk over a list

You can make your own custom operations that walk over all the elements in a list, using `List.walk`.
Let's look at an example and then walk (ha!) through it.

```coffee
List.walk [1, 2, 3, 4, 5] { evens: [], odds: [] } \state, elem ->
    if Num.isEven elem then
        { state & evens: List.append state.evens elem }
    else
        { state & odds: List.append state.odds elem }

# returns { evens: [2, 4], odds: [1, 3, 5] }
```

`List.walk` walks through each element of the list, building up a state as it goes. At the end,
it returns the final state - whatever it ended up being after processing the last element. The `\state, elem ->`
function it takes as its last argument accepts both the current state as well as the current list element
it's looking at, and then returns the new state based on whatever it decides to do with that element.

In this example, we walk over the list `[1, 2, 3, 4, 5]` and add each element to either the `evens` or `odds`
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
List.get ["a", "b", "c"] 1
```

```coffee
List.get ["a", "b", "c"] 100
```

The answer is that the first one returns `Ok "b"` and the second one returns `Err OutOfBounds`.
They both return tags! This is done so that the caller becomes responsible for handling the possibility that
the index is outside the bounds of that particular list.

Here's how calling `List.get` can look in practice:

```coffee
when List.get ["a", "b", "c"] index is
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
Result.withDefault (List.get ["a", "b", "c"] 100) ""
# returns "" because that's the default we said to use if List.get returned an Err
```

```coffee
Result.isOk (List.get ["a", "b", "c"] 1)
# returns True because `List.get` returned an `Ok` tag. (The payload gets ignored.)

# Note: There's a Result.isErr function that works similarly.
```

### The pipe operator

When you have nested function calls, sometimes it can be clearer to write them in a "pipelined"
style using the `|>` operator. Here are three examples of writing the same expression; they all
compile to exactly the same thing, but two of them use the `|>` operator to change how the calls look.

```coffee
Result.withDefault (List.get ["a", "b", "c"] 1) ""
```

```coffee
List.get ["a", "b", "c"] 1
    |> Result.withDefault ""
```

The `|>` operator takes the value that comes before the `|>` and passes it as the first argument to whatever
comes after the `|>` - so in the example above, the `|>` takes `List.get ["a", "b", "c"] 1` and passes that
value as the first argument to `Result.withDefault` - making `""` the second argument to `Result.withDefault`.

We can take this a step further like so:

```coffee
["a", "b", "c"]
    |> List.get 1
    |> Result.withDefault ""
```

This is still equivalent to the first expression. Since `|>` is known as the "pipe operator," we can read
this as "start with `["a", "b", "c"]`, then pipe it to `List.get`, then pipe it to `Result.withDefault`."

One reason the `|>` operator injects the value as the first argument is to make it work better with
functions where argument order matters. For example, these two uses of `List.append` are equivalent:

```coffee
List.append ["a", "b", "c"] "d"
```

```coffee
["a", "b", "c"]
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
colorFromStr : Str -> [Red, Green, Yellow]
colorFromStr = \string ->
    when string is
        "red" -> Red
        "green" -> Green
        _ -> Yellow
```

You can read the type `[Red, Green, Yellow]` as "a tag union of the tags `Red`, `Green`, and `Yellow`."

When we annotate a list type, we have to specify the type of its elements:

```coffee
names : List Str
names = ["Amy", "Simone", "Tarja"]
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
`List.isEmpty ["I am a List Str"]` as well as `List.isEmpty [True]`, and they will both work fine.

The wildcard type also comes up with empty lists. Suppose we have one function that takes a `List Str` and another
function that takes a `List Bool`. We might reasonably expect to be able to pass an empty list (that is, `[]`) to
either of these functions. And so we can! This is because a `[]` value has the type `List *` - that is,
"a list with a wildcard type parameter," or "a list whose element type could be anything."

`List.reverse` works similarly to `List.isEmpty`, but with an important distinction. As with `isEmpty`, we can
call `List.reverse` on any list, regardless of its type parameter. However, consider these calls:

```coffee
strings : List Str
strings = List.reverse ["a", "b"]

bools : List Bool
bools = List.reverse [True, False]
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

## Numeric types

Roc has different numeric types that each have different tradeoffs.
They can all be broken down into two categories: [fractions](https://en.wikipedia.org/wiki/Fraction),
and [integers](https://en.wikipedia.org/wiki/Integer). In Roc we call these `Frac` and `Int` for short.

### Integers

Roc's integer types have two important characteristics: their *size* and their [*signedness*](https://en.wikipedia.org/wiki/Signedness).
Together, these two characteristics determine the range of numbers the integer type can represent.

For example, the Roc type `U8` can represent the numbers 0 through 255, whereas the `I16` type can represent
the numbers -32768 through 32767. You can actually infer these ranges from their names (`U8` and `I16`) alone!

The `U` in `U8` indicates that it's *unsigned*, meaning that it can't have a minus [sign](https://en.wikipedia.org/wiki/Sign_(mathematics)), and therefore can't be negative. The fact that it's unsigned tells us immediately that
its lowest value is zero. The 8 in `U8` means it is 8 [bits](https://en.wikipedia.org/wiki/Bit) in size, which
means it has room to represent 2⁸ (which is equal to 256) different numbers. Since one of those 256 different numbers
is 0, we can look at `U8` and know that it goes from `0` (since it's unsigned) to `255` (2⁸ - 1, since it's 8 bits).

If we change `U8` to `I8`, making it a *signed* 8-bit integer, the range changes. Because it's still 8 bits, it still
has room to represent 2⁸ (that is, 256) different numbers. However, now in addition to one of those 256 numbers
being zero, about half of the rest will be negative, and the others positive. So instead of ranging from, say -255
to 255 (which, counting zero, would represent 511 different numbers; too many to fit in 8 bits!) an `I8` value
ranges from -128 to 127.

Notice that the negative extreme is `-128` versus `127` (not `128`) on the positive side. That's because of
needing room for zero; the slot for zero is taken from the positive range because zero doesn't have a minus sign.
So in general, you can find the lowest signed number by taking its total range (256 different numbers in the case
of an 8-bit integer) and dividing it in half (half of 256 is 128, so -128 is `I8`'s lowest number). To find the
highest number, take the positive version of the lowest number (so, convert `-128` to `128`) and then subtract 1
to make room for zero (so, `128` becomes `127`; `I8` ranges from -128 to 127).

Following this pattern, the 16 in `I16` means that it's a signed 16-bit integer.
That tells us it has room to represent 2¹⁶ (which is equal to 65536) different numbers. Half of 65536 is 32768,
so the lowest `I16` would be -32768, and the highest would be 32767. Knowing that, we can also quickly tell that
the lowest `U16` would be zero (since it always is for unsigned integers), and the highest `U16` would be 65535.

Choosing a size depends on your performance needs and the range of numbers you want to represent. Consider:

- Larger integer sizes can represent a wider range of numbers. If you absolutely need to represent numbers in a certain range, make sure to pick an integer size that can hold them!
- Smaller integer sizes take up less memory. These savings rarely matters in variables and function arguments, but the sizes of integers that you use in data structures can add up. This can also affect whether those data structures fit in [cache lines](https://en.wikipedia.org/wiki/CPU_cache#Cache_performance), which can easily be a performance bottleneck.
- Certain processors work faster on some numeric sizes than others. There isn't even a general rule like "larger numeric sizes run slower" (or the reverse, for that matter) that applies to all processors. In fact, if the CPU is taking too long to run numeric calculations, you may find a performance improvement by experimenting with numeric sizes that are larger than otherwise necessary. However, in practice, doing this typically degrades overall performance, so be careful to measure properly!

Here are the different fixed-size integer types that Roc supports:

| Range                                                        | Type  | Size     |
| -----------------------------------------------------------: | :---- | :------- |
|                                             `-128`<br/>`127` | `I8`  | 1 Byte   |
|                                                `0`<br/>`255` | `U8`  | 1 Byte   |
|                                       `-32_768`<br/>`32_767` | `I16` | 2 Bytes  |
|                                             `0`<br/>`65_535` | `U16` | 2 Bytes  |
|                         `-2_147_483_648`<br/>`2_147_483_647` | `I32` | 4 Bytes  |
|                     `0`<br/>(over 4 billion) `4_294_967_295` | `U32` | 4 Bytes  |
| `-9_223_372_036_854_775_808`<br/>`9_223_372_036_854_775_807` | `I64` | 8 Bytes  |
|   `0`<br/>(over 18 quintillion) `18_446_744_073_709_551_615` | `U64` | 8 Bytes  |
|  `-170_141_183_460_469_231_731_687_303_715_884_105_728`<br/>`170_141_183_460_469_231_731_687_303_715_884_105_727`  | `I128` | 16 Bytes |
| `0`<br/>(over 340 undecillion) `340_282_366_920_938_463_463_374_607_431_768_211_455` | `U128` | 16 Bytes |

Roc also has one variable-size integer type: `Nat` (short for "natural number").
The size of `Nat` is equal to the size of a memory address, which varies by system.
For example, when compiling for a 64-bit system, `Nat` works the same way as `U64`.
When compiling for a 32-bit system, it works the same way as `U32`. Most popular
computing devices today are 64-bit, so `Nat` is usually the same as `U64`, but
Web Assembly is typically 32-bit - so when running a Roc program built for Web Assembly,
`Nat` will work like a `U32` in that program.

A common use for `Nat` is to store the length of a collection like a `List`;
there's a function `List.len : List * -> Nat` which returns the length of the given list.
64-bit systems can represent longer lists in memory than 32-bit systems can,
which is why the length of a list is represented as a `Nat`.

If any operation would result in an integer that is either too big
or too small to fit in that range (e.g. calling `Int.maxI32 + 1`, which adds 1 to
the highest possible 32-bit integer), then the operation will *overflow*.
When an overflow occurs, the program will crash.

As such, it's very important to design your integer operations not to exceed these bounds!

### Fractions

Roc has three fractional types:

- `F32`, a 32-bit [floating-point number](https://en.wikipedia.org/wiki/IEEE_754)
- `F64`, a 64-bit [floating-point number](https://en.wikipedia.org/wiki/IEEE_754)
- `Dec`, a 128-bit decimal [fixed-point number](https://en.wikipedia.org/wiki/Fixed-point_arithmetic)

These are different from integers in that they can represent numbers with fractional components,
such as 1.5 and -0.123.

`Dec` is the best default choice for representing base-10 decimal numbers
like currency, because it is base-10 under the hood. In contrast,
`F64` and `F32` are base-2 under the hood, which can lead to decimal
precision loss even when doing addition and subtraction. For example, when
using `F64`, running 0.1 + 0.2 returns 0.3000000000000000444089209850062616169452667236328125,
whereas when using `Dec`, 0.1 + 0.2 returns 0.3.

`F32` and `F64` have direct hardware support on common processors today. There is no hardware support
for fixed-point decimals, so under the hood, a `Dec` is an `I128`; operations on it perform
[base-10 fixed-point arithmetic](https://en.wikipedia.org/wiki/Fixed-point_arithmetic)
with 18 decimal places of precision.

This means a `Dec` can represent whole numbers up to slightly over 170
quintillion, along with 18 decimal places. (To be precise, it can store
numbers between `-170_141_183_460_469_231_731.687303715884105728`
and `170_141_183_460_469_231_731.687303715884105727`.) Why 18
decimal places? It's the highest number of decimal places where you can still
convert any `U64` to a `Dec` without losing information.

While the fixed-point `Dec` has a fixed range, the floating-point `F32` and `F64` do not.
Instead, outside of a certain range they start to lose precision instead of immediately overflowing
the way integers and `Dec` do. `F64` can represent [between 15 and 17 significant digits](https://en.wikipedia.org/wiki/Double-precision_floating-point_format) before losing precision, whereas `F32` can only represent [between 6 and 9](https://en.wikipedia.org/wiki/Single-precision_floating-point_format#IEEE_754_single-precision_binary_floating-point_format:_binary32).

There are some use cases where `F64` and `F32` can be better choices than `Dec`
despite their precision drawbacks. For example, in graphical applications they
can be a better choice for representing coordinates because they take up less memory,
various relevant calculations run faster, and decimal precision loss isn't as big a concern
when dealing with screen coordinates as it is when dealing with something like currency.

### Num, Int, and Frac

Some operations work on specific numeric types - such as `I64` or `Dec` - but operations support
multiple numeric types. For example, the `Num.abs` function works on any number, since you can
take the [absolute value](https://en.wikipedia.org/wiki/Absolute_value) of integers and fractions alike.
Its type is:

```elm
abs : Num a -> Num a
```

This type says `abs` takes a number and then returns a number of the same type. That's because the
`Num` type is compatible with both integers and fractions.

There's also an `Int` type which is only compatible with integers, and a `Frac` type which is only
compatible with fractions. For example:

```elm
Num.xor : Int a, Int a -> Int a
```

```elm
Num.cos : Frac a -> Frac a
```

When you write a number literal in Roc, it has the type `Num *`. So you could call `Num.xor 1 1`
and also `Num.cos 1` and have them all work as expected; the number literal `1` has the type
`Num *`, which is compatible with the more constrained types `Int` and `Frac`. For the same reason,
you can pass number literals to functions expecting even more constrained types, like `I32` or `F64`.

### Typed Number Literals

When writing a number literal in Roc you can specify the numeric type as a suffix of the literal.
`1u8` specifies `1` as an unsigned 8-bit integer, `5i32` specifies `5` as a signed 32-bit integer, etc.
The full list of possible suffixes includes:
`i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`, `i128`, `u128`, `nat`, `f32`, `f64`, `dec`

### Hexadecimal Integer Literals

Integer literals can be written in hexadecimal form by prefixing with `0x` followed by hexadecimal characters.
`0xFE` evaluates to decimal `254`
The integer type can be specified as a suffix to the hexadecimal literal,
so `0xC8u8` evaluates to decimal `200` as an unsigned 8-bit integer.

### Binary Integer Literals

Integer literals can be written in binary form by prefixing with `0b` followed by the 1's and 0's representing
each bit. `0b0000_1000` evaluates to decimal `8`
The integer type can be specified as a suffix to the binary literal,
so `0b0100u8` evaluates to decimal `4` as an unsigned 8-bit integer.

## Interface modules

[This part of the tutorial has not been written yet. Coming soon!]

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

- They are always imported. You never need to add them to `imports`.
- All their types are imported unqualified automatically. So you never need to write `Num.Nat`, because it's as if the `Num` module was imported using `imports [Num.{ Nat }]` (and the same for all the other types in the `Num` module).

## The app module header

Let's take a closer look at the part of `Hello.roc` above `main`:

```coffee
app "hello"
    packages { pf: "examples/interactive/cli-platform/main.roc" }
    imports [pf.Stdout]
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
packages { pf: "examples/interactive/cli-platform/main.roc" }
imports [pf.Stdout]
provides main to pf
```

The `packages { pf: "examples/interactive/cli-platform/main.roc" }` part says two things:

- We're going to be using a *package* (that is, a collection of modules) called `"examples/interactive/cli-platform/main.roc"`
- We're going to name that package `pf` so we can refer to it more concisely in the future.

The `imports [pf.Stdout]` line says that we want to import the `Stdout` module
from the `pf` package, and make it available in the current module.

This import has a direct interaction with our definition of `main`. Let's look
at that again:

```coffee
main = Stdout.line "I'm a Roc application!"
```

Here, `main` is calling a function called `Stdout.line`. More specifically, it's
calling a function named `line` which is exposed by a module named
`Stdout`.

When we write `imports [pf.Stdout]`, it specifies that the `Stdout`
module comes from the `pf` package.

Since `pf` was the name we chose for the `examples/interactive/cli-platform/main.roc`
package (when we wrote `packages { pf: "examples/interactive/cli-platform/main.roc" }`),
this `imports` line tells the Roc compiler that when we call `Stdout.line`, it
should look for that `line` function in the `Stdout` module of the
`examples/interactive/cli-platform/main.roc` package.

## Tasks

Tasks are technically not part of the Roc language, but they're very common in
platforms. Let's use the CLI platform in `examples/interactive/cli-platform/main.roc` as an example!

In the CLI platform, we have four operations we can do:

- Write a string to the console
- Read a string from user input
- Write a string to a file
- Read a string from a file

We'll use these four operations to learn about tasks.

First, let's do a basic "Hello World" using the tutorial app.

```coffee
app "cli-tutorial"
    packages { pf: "examples/interactive/cli-platform/main.roc" }
    imports [pf.Stdout]
    provides [main] to pf

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
    packages { pf: "examples/interactive/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Stdin, pf.Task]
    provides [main] to pf

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
    packages { pf: "examples/interactive/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Stdin, pf.Task.{ await }]
    provides [main] to pf

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

- `await` is not a language keyword in Roc! It's referring to the `Task.await` function, which we imported unqualified by writing `Task.{ await }` in our module imports. (That said, it is playing a similar role here to the `await` keyword in languages that have `async`/`await` keywords, even though in this case it's a function instead of a special keyword.)
- Backpassing syntax does not need to be used with `await` in particular. It can be used with any function.
- Roc's compiler treats functions defined with backpassing exactly the same way as functions defined the other way. The only difference between `\text ->` and `text <-` is how they look, so feel free to use whichever looks nicer to you!

## Appendix: Advanced Concepts

Here are some concepts you likely won't need as a beginner, but may want to know about eventually.
This is listed as an appendix rather than the main tutorial, to emphasize that it's totally fine
to stop reading here and go build things!

### Open Records and Closed Records

Let's say I write a function which takes a record with a `firstName`
and `lastName` field, and puts them together with a space in between:

```swift
fullName = \user ->
    "\(user.firstName) \(user.lastName)"
```

I can pass this function a record that has more fields than just
`firstName` and `lastName`, as long as it has *at least* both of those fields
(and both of them are strings). So any of these calls would work:

- `fullName { firstName: "Sam", lastName: "Sample" }`
- `fullName { firstName: "Sam", lastName: "Sample", email: "blah@example.com" }`
- `fullName { age: 5, firstName: "Sam", things: 3, lastName: "Sample", role: Admin }`

This `user` argument is an *open record* - that is, a description of a minimum set of fields
on a record, and their types. When a function takes an open record as an argument,
it's okay if you pass it a record with more fields than just the ones specified.

In contrast, a *closed record* is one that requires an exact set of fields (and their types),
with no additional fields accepted.

If we add a type annotation to this `fullName` function, we can choose to have it accept either
an open record or a closed record:

```coffee
# Closed record
fullName : { firstName : Str, lastName : Str } -> Str
fullName = \user ->
    "\(user.firstName) \(user.lastName)"
```

```coffee
# Open record (because of the `*`)
fullName : { firstName : Str, lastName : Str }* -> Str
fullName = \user ->
    "\(user.firstName) \(user.lastName)"
```

The `*` in the type `{ firstName : Str, lastName : Str }*` is what makes it an open record type.
This `*` is the *wildcard type* we saw earlier with empty lists. (An empty list has the type `List *`,
in contrast to something like `List Str` which is a list of strings.)

This is because record types can optionally end in a type variable. Just like how we can have `List *`
or `List a -> List a`, we can also have `{ first : Str, last : Str }*` or
`{ first : Str, last : Str }a -> { first: Str, last : Str }a`. The differences are that in `List a`,
the type variable is required and appears with a space after `List`; in a record, the type variable
is optional, and appears (with no space) immediately after `}`.

If the type variable in a record type is a `*` (such as in `{ first : Str, last : Str }*`), then
it's an open record. If the type variable is missing, then it's a closed record. You can also specify
a closed record by putting a `{}` as the type variable (so for example, `{ email : Str }{}` is another way to write
`{ email : Str }`). In practice, closed records are basically always written without the `{}` on the end,
but later on we'll see a situation where putting types other than `*` in that spot can be useful.

### Constrained Records

The type variable can also be a named type variable, like so:

```coffee
addHttps : { url : Str }a -> { url : Str }a
addHttps = \record ->
    { record & url: "https://\(record.url)" }
```

This function uses *constrained records* in its type. The annotation is saying:

- This function takes a record which has at least a `url` field, and possibly others
- That `url` field has the type `Str`
- It returns a record of exactly the same type as the one it was given

So if we give this function a record with five fields, it will return a record with those
same five fields. The only requirement is that one of those fields must be `url : Str`.

In practice, constrained records appear in type annotations much less often than open or closed records do.

Here's when you can typically expect to encounter these three flavors of type variables in records:

- *Open records* are what the compiler infers when you use a record as an argument, or when destructuring it (for example, `{ x, y } =`).
- *Closed records* are what the compiler infers when you create a new record (for example, `{ x: 5, y: 6 }`)
- *Constrained records* are what the compiler infers when you do a record update (for example, `{ user & email: newEmail }`)

Of note, you can pass a closed record to a function that accepts a smaller open record, but not the reverse.
So a function `{ a : Str, b : Bool }* -> Str` can accept an `{ a : Str, b : Bool, c : Bool }` record,
but a function `{ a : Str, b : Bool, c : Bool } -> Str` would not accept an `{ a : Str, b : Bool }*` record.

This is because if a function accepts `{ a : Str, b : Bool, c : Bool }`, that means it might access the `c`
field of that record. So if you passed it a record that was not guaranteed to have all three of those fields
present (such as an `{ a : Str, b : Bool }*` record, which only guarantees that the fields `a` and `b` are present),
the function might try to access a `c` field at runtime that did not exist!

### Type Variables in Record Annotations

You can add type annotations to make record types less flexible than what the compiler infers, but not more
flexible. For example, you can use an annotation to tell the compiler to treat a record as closed when it would
be inferred as open (or constrained), but you can't use an annotation to make a record open when it would be
inferred as closed.

If you like, you can always annotate your functions as accepting open records. However, in practice this may not
always be the nicest choice. For example, let's say you have a `User` type alias, like so:

```coffee
User : {
    email : Str,
    firstName : Str,
    lastName : Str,
}
```

This defines `User` to be a closed record, which in practice is the most common way records named `User`
tend to be defined.

If you want to have a function take a `User`, you might write its type like so:

```elm
isValid : User -> Bool
```

If you want to have a function return a `User`, you might write its type like so:

```elm
userFromEmail : Str -> User
```

A function which takes a user and returns a user might look like this:

```elm
capitalizeNames : User -> User
```

This is a perfectly reasonable way to write all of these functions. However, I
might decide that I really want the `isValid` function to take an open record -
that is, a record with *at least* the fields of this `User` record, but possibly others as well.

Since open records have a type variable (like `*` in `{ email : Str }*` or `a` in
`{ email : Str }a -> { email : Str }a`), in order to do this I'd need to add a
type variable to the `User` type alias:

```coffee
User a : {
    email : Str,
    firstName : Str,
    lastName : Str,
}a
```

Notice that the `a` type variable appears not only in `User a` but also in `}a` at the end of the
record type!

Using `User a` type alias, I can still write the same three functions, but now their types need to look different.
This is what the first one would look like:

```elm
isValid : User * -> Bool
```

Here, the `User *` type alias substitutes `*` for the type variable `a` in the type alias,
which takes it from `{ email : Str, … }a` to `{ email : Str, … }*`. Now I can pass it any
record that has at least the fields in `User`, and possibly others as well, which was my goal.

```elm
userFromEmail : Str -> User {}
```

Here, the `User {}` type alias substitutes `{}` for the type variable `a` in the type alias,
which takes it from `{ email : Str, … }a` to `{ email : Str, … }{}`. As noted earlier,
this is another way to specify a closed record: putting a `{}` after it, in the same place that
you'd find a `*` in an open record.

> **Aside:** This works because you can form new record types by replacing the type variable with
> other record types. For example, `{ a : Str, b : Str }` can also be written `{ a : Str }{ b : Str }`.
> You can chain these more than once, e.g. `{ a : Str }{ b : Str }{ c : Str, d : Str }`.
> This is more useful when used with type annotations; for example, `{ a : Str, b : Str }User` describes
> a closed record consisting of all the fields in the closed record `User`, plus `a : Str` and `b : Str`.

This function still returns the same record as it always did, it just needs to be annotated as
`User {}` now instead of just `User`, because the `User` type alias has a variable in it that must be
specified.

The third function might need to use a named type variable:

```elm
capitalizeNames : User a -> User a
```

If this function does a record update on the given user, and returns that - for example, if its
definition were `capitalizeNames = \user -> { user & email: "blah" }` - then it needs to use the
same named type variable for both the argument and return value.

However, if returns a new `User` that it created from scratch, then its type could instead be:

```elm
capitalizeNames : User * -> User {}
```

This says that it takes a record with at least the fields specified in the `User` type alias,
and possibly others...and then returns a record with exactly the fields specified in the `User`
type alias, and no others.

These three examples illustrate why it's relatively uncommon to use open records for type aliases:
it makes a lot of types need to incorporate a type variable that otherwise they could omit,
all so that `isValid` can be given something that has not only the fields `User` has, but
some others as well. (In the case of a `User` record in particular, it may be that the extra
fields were included due to a mistake rather than on purpose, and accepting an open record could
prevent the compiler from raising an error that would have revealed the mistake.)

That said, this is a useful technique to know about if you want to (for example) make a record
type that accumulates more and more fields as it progresses through a series of operations.

### Open and Closed Tag Unions

Just like how Roc has open records and closed records, it also has open and closed tag unions.

The *open tag union* (or *open union* for short) `[Foo Str, Bar Bool]*` represents a tag that might
be `Foo Str` and might be `Bar Bool`, but might also be some other tag whose type isn't known at compile time.

Because an open union represents possibilities that are impossible to know ahead of time, any `when` I use on a
`[Foo Str, Bar Bool]*` value must include a catch-all `_ ->` branch. Otherwise, if one of those
unknown tags were to come up, the `when` would not know what to do with it! For example:

```coffee
example : [Foo Str, Bar Bool]* -> Bool
example = \tag ->
    when tag is
        Foo str -> Str.isEmpty str
        Bar bool -> bool
        _ -> False
```

In contrast, a *closed tag union* (or *closed union*) like `[Foo Str, Bar Bool]` (without the `*`)
represents an exhaustive set of possible tags. If I use a `when` on one of these, I can match on `Foo`
only and then on `Bar` only, with no need for a catch-all branch. For example:

```coffee
example : [Foo Str, Bar Bool] -> Bool
example = \tag ->
    when tag is
        Foo str -> Str.isEmpty str
        Bar bool -> bool
```

If we were to remove the type annotations from the previous two code examples, Roc would infer the same
types for them anyway.

It would infer `tag : [Foo Str, Bar Bool]` for the latter example because the `when tag is` expression
only includes a `Foo Str` branch and a `Bar Bool` branch, and nothing else. Since the `when` doesn't handle
any other possibilities, these two tags must be the only possible ones the `tag` argument could be.

It would infer `tag : [Foo Str, Bar Bool]*` for the former example because the `when tag is` expression
includes a `Foo Str` branch and a `Bar Bool` branch - meaning we know about at least those two specific
possibilities - but also a `_ ->` branch, indicating that there may be other tags we don't know about. Since
the `when` is flexible enough to handle all possible tags, `tag` gets inferred as an open union.

Putting these together, whether a tag union is inferred to be open or closed depends on which possibilities
the implementation actually handles.

> **Aside:** As with open and closed records, we can use type annotations to make tag union types less flexible
> than what would be inferred. If we added a `_ ->` branch to the second example above, the compiler would still
> accept `example : [Foo Str, Bar Bool] -> Bool` as the type annotation, even though the catch-all branch
> would permit the more flexible `example : [Foo Str, Bar Bool]* -> Bool` annotation instead.

### Combining Open Unions

When we make a new record, it's inferred to be a closed record. For example, in `foo { a: "hi" }`,
the type of `{ a: "hi" }` is inferred to be `{ a : Str }`. In contrast, when we make a new tag, it's inferred
to be an open union. So in `foo (Bar "hi")`, the type of `Bar "hi"` is inferred to be `[Bar Str]*`.

This is because open unions can accumulate additional tags based on how they're used in the program,
whereas closed unions cannot. For example, let's look at this conditional:

```elm
if x > 5 then
    "foo"
else
    7
```

This will be a type mismatch because the two branches have incompatible types. Strings and numbers are not
type-compatible! Now let's look at another example:

```elm
if x > 5 then
    Ok "foo"
else
    Err "bar"
```

This shouldn't be a type mismatch, because we can see that the two branches are compatible; they are both
tags that could easily coexist in the same tag union. But if the compiler inferred the type of `Ok "foo"` to be
the closed union `[Ok Str]`, and likewise for `Err "bar"` and `[Err Str]`, then this would have to be
a type mismatch - because those two closed unions are incompatible.

Instead, the compiler infers `Ok "foo"` to be the open union `[Ok Str]*`, and `Err "bar"` to be the open
union `[Err Str]*`. Then, when using them together in this conditional, the inferred type of the conditional
becomes `[Ok Str, Err Str]*` - that is, the combination of the unions in each of its branches. (Branches in
a `when` work the same way with open unions.)

Earlier we saw how a function which accepts an open union must account for more possibilities, by including
catch-all `_ ->` patterns in its `when` expressions. So *accepting* an open union means you have more requirements.
In contrast, when you already *have* a value which is an open union, you have fewer requirements. A value
which is an open union (like `Ok "foo"`, which has the type `[Ok Str]*`) can be provided to anything that's
expecting a tag union (no matter whether it's open or closed), as long as the expected tag union includes at least
the tags in the open union you're providing.

So if I have an `[Ok Str]*` value, I can pass it to functions with any of these types (among others):

- `[Ok Str]* -> Bool`
- `[Ok Str] -> Bool`
- `[Ok Str, Err Bool]* -> Bool`
- `[Ok Str, Err Bool] -> Bool`
- `[Ok Str, Err Bool, Whatever]* -> Bool`
- `[Ok Str, Err Bool, Whatever] -> Bool`
- `Result Str Bool -> Bool`
- `[Err Bool, Whatever]* -> Bool`

That last one works because a function accepting an open union can accept any unrecognized tag, including
`Ok Str` - even though it is not mentioned as one of the tags in `[Err Bool, Whatever]*`! Remember, when
a function accepts an open tag union, any `when` branches on that union must include a catch-all `_ ->` branch,
which is the branch that will end up handling the `Ok Str` value we pass in.

However, I could not pass an `[Ok Str]*` to a function with a *closed* tag union argument that did not
mention `Ok Str` as one of its tags. So if I tried to pass `[Ok Str]*` to a function with the type
`[Err Bool, Whatever] -> Str`, I would get a type mismatch - because a `when` in that function could
be handling the `Err Bool` possibility and the `Whatever` possibility, and since it would not necessarily have
a catch-all `_ ->` branch, it might not know what to do with an `Ok Str` if it received one.

> **Note:** It wouldn't be accurate to say that a function which accepts an open union handles
> "all possible tags." For example, if I have a function `[Ok Str]* -> Bool` and I pass it
> `Ok 5`, that will still be a type mismatch. If you think about it, a `when` in that function might
> have the branch `Ok str ->` which assumes there's a string inside that `Ok`, and if `Ok 5` type-checked,
> then that assumption would be false and things would break!
>
> So `[Ok Str]*` is more restrictive than `[]*`. It's basically saying "this may or may not be an `Ok` tag,
> but if it is an `Ok` tag, then it's guaranteed to have a payload of exactly `Str`."

In summary, here's a way to think about the difference between open unions in a value you have, compared to a value you're accepting:

- If you *have* a closed union, that means it has all the tags it ever will, and can't accumulate more.
- If you *have* an open union, that means it can accumulate more tags through conditional branches.
- If you *accept* a closed union, that means you only have to handle the possibilities listed in the union.
- If you *accept* an open union, that means you have to handle the possibility that it has a tag you can't know about.

### Type Variables in Tag Unions

Earlier we saw these two examples, one with an open tag union and the other with a closed one:

```coffee
example : [Foo Str, Bar Bool]* -> Bool
example = \tag ->
    when tag is
        Foo str -> Str.isEmpty str
        Bar bool -> bool
        _ -> False
```

```coffee
example : [Foo Str, Bar Bool] -> Bool
example = \tag ->
    when tag is
        Foo str -> Str.isEmpty str
        Bar bool -> bool
```

Similarly to how there are open records with a `*`, closed records with nothing,
and constrained records with a named type variable, we can also have *constrained tag unions*
with a named type variable. Here's an example:

```coffee
example : [Foo Str, Bar Bool]a -> [Foo Str, Bar Bool]a
example = \tag ->
    when tag is
        Foo str -> Bar (Str.isEmpty str)
        Bar _ -> Bar False
        other -> other
```

This type says that the `example` function will take either a `Foo Str` tag, or a `Bar Bool` tag,
or possibly another tag we don't know about at compile time - and it also says that the function's
return type is the same as the type of its argument.

So if we give this function a `[Foo Str, Bar Bool, Baz (List Str)]` argument, then it will be guaranteed
to return a `[Foo Str, Bar Bool, Baz (List Str)]` value. This is more constrained than a function that
returned `[Foo Str, Bar Bool]*` because that would say it could return *any* other tag (in addition to
the `Foo Str` and `Bar Bool` we already know about).

If we removed the type annotation from `example` above, Roc's compiler would infer the same type anyway.
This may be surprising if you look closely at the body of the function, because:

- The return type includes `Foo Str`, but no branch explicitly returns `Foo`. Couldn't the return type be `[Bar Bool]a` instead?
- The argument type includes `Bar Bool` even though we never look at `Bar`'s payload. Couldn't the argument type be inferred to be `Bar *` instead of `Bar Bool`, since we never look at it?

The reason it has this type is the `other -> other` branch. Take a look at that branch, and ask this question:
"What is the type of `other`?" There has to be exactly one answer! It can't be the case that `other` has one
type before the `->` and another type after it; whenever you see a named value in Roc, it is guaranteed to have
the same type everywhere it appears in that scope.

For this reason, any time you see a function that only runs a `when` on its only argument, and that `when`
includes a branch like `x -> x` or `other -> other`, the function's argument type and return type must necessarily
be equivalent.

> **Note:** Just like with records, you can also replace the type variable in tag union types with a concrete type.
> For example, `[Foo Str][Bar Bool][Baz (List Str)]` is equivalent to `[Foo Str, Bar Bool, Baz (List Str)]`.
>
> Also just like with records, you can use this to compose tag union type aliases. For example, you can write
> `NetworkError : [Timeout, Disconnected]` and then `Problem : [InvalidInput, UnknownFormat]NetworkError`

### Phantom Types

[This part of the tutorial has not been written yet. Coming soon!]

### Operator Desugaring Table

Here are various Roc expressions involving operators, and what they desugar to.

| Expression      | Desugars to      |
| --------------- | ---------------- |
| `a + b`           | `Num.add a b`      |
| `a - b`           | `Num.sub a b`      |
| `a * b`           | `Num.mul a b`      |
| `a / b`           | `Num.div a b`    |
| `a // b`          | `Num.divTrunc a b`      |
| `a ^ b`           | `Num.pow a b`      |
| `a % b`           | `Num.rem a b`    |
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
