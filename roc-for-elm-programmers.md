# Roc for Elm programmers

Roc is a direct descendant of the [Elm programming language](https://elm-lang.org/). The two languages are similar, but not quite the same!

This is a guide to help Elm programmers learn what's different between Elm and Roc.

> NOTE: As of 2020, only a subset of what's in this document has been implemented.

## Comments

Like [Python](https://www.python.org), [Ruby](https://www.ruby-lang.org), [Perl](https://www.perl.org), and [Elixir](https://elixir-lang.org/), inline comments in Roc begin with `#` instead of `--`.

Doc comments begin with `##` instead.

Like Python, Roc does not have multiline comment syntax.

## String Interpolation

Roc strings work like Elm strings except that they support string interpolation.
Here's a Roc string which uses interpolation:

```elm
"Hi, my name is \(name)!"
```

The Elm equivalent would be:

```elm
"Hi, my name is " ++ name ++ "!"
```

This interpolation syntax comes from [Swift](https://swift.org/). Only a single
identifier can go inside the parentheses (like `(name)` here), and the identifier
needs to be a string already. Arbitrary expressions are not allowed, which means
weird situations like string literals inside string literals do not come up.

Roc strings also have the type `Str` rather than Elm's `String`. This is to make
common qualified operations like `Str.toInt` more concise; the idea is that you'll use the
abbreviation often enough that you'll quickly get used to it. (I got used to [`str` in
Rust](https://doc.rust-lang.org/std/primitive.str.html) very quickly.)

## Type Aliases

Rather than a `type alias` keyword, in Roc you define type aliases with `:` like so:

```elm
Username : Str
```

You can also define type aliases anywhere, not just at the top level.
Their scoping rules work as normal.

Separately, Roc also allows standalone type annotations with no corresponding implementation.
So I can write this as an annotation with no implementation:

```elm
getUsername : User -> Username
```

Roc will automatically fill in the implementation of this as the equivalent of
a `Debug.todo`. If it ever gets run, it will crash, but for debugging purposes
or sketching out APIs, you don't need to bother writing `getUsername = Debug.todo "implement"`.

## `let` syntax

Imagine if Elm's `let`...`in` worked exactly the same way, except you removed the `let` and `in` keywords. That's how it works in Roc.

For example, this Elm code computes `someNumber` to be `1234`:

```elm
someNumber =
    let
        foo =
            1000

        blah =
            234
    in
    foo + blah
```

Here's the equivalent Roc code:

```elm
someNumber =
    foo =
        1000

    blah =
        234

    foo + blah
```

Like `let`...`in` in Elm, this is indentation-sensitive. Each of the definitions
("defs" for short) must have the same indentation as the ending expression.

## Function definitions

Roc only has one syntax for defining functions, and it looks almost exactly
like Elm's anonymous functions. The one difference is that multiple arguments
are separated by commas.

So where in Elm you might write `foo a b =` in Roc you'd write `foo = \a, b ->` instead.

One minor benefit of the comma is that you don't need to use parens to
destructure arguments inline. For example, in Elm, you always need to use parentheses
to destructure variants inline in function declarations, like in these two examples:

```
\(UserId id1) (UserId id2) ->
\(UserId id) ->
```

Without the parentheses, it wouldn't be clear where one argument ended and the next one began.

In Roc, the commas make argument boundaries unambiguous, so no parens are needed.
You can rewrite the above like so:

```
\UserId id1, UserId id2 ->
\UserId id ->
```

## Unbound type variables

In Elm, every type variable is named. For example:

```
List.reverse : List a -> List a

[] : List a
```

The `a` in `List.reverse` is a *bound* type variable, because it appears more than once in the type.
Whatever the first list's `a` is, that's what the second list's `a` must be as well.

The `a` in `[]` is an *unbound* type variable. It has no restrictions,
which is why `[]` can be passed to any function that expects a `List`.

In Roc, this distinction between bound and unbound type variables is reflected at
the syntax level. Here are those types in Roc:

```
List.reverse : List a -> List a

[] : List *
```

The `*` is the "wildcard" type variable. It is only for unbound type variables like this.
Like the wildcard `*` in path globs like `*.txt`, it matches anything.

> You can still choose names for unbound type variables if you like, but the
> compiler will infer them as `*` by default.

In Elm, the type of `always` is `a -> (b -> a)`. In Roc, it is:

```elm
always : a -> (* -> a)
```

This makes unbound type variables easier to talk about out loud. Rather than saying
(for example) "List a" or "Html msg with a lowercase m" you can say "List star" or "Html star".

## Pattern matching

Roc's pattern matching conditionals work about the same as how they do in Elm.
Here are two differences:

* Roc uses the syntax `when`...`is` instead of `case`...`of`
* In Roc, you can use `|` to handle multiple patterns in the same way

For example:

```elm
when color is
    Blue -> 1
    Green | Red | Yellow -> 2
    Purple -> 3
```

## Custom Types

This is the biggest semantic difference between Roc and Elm.

Let's start with the motivation. Suppose I'm using a platform for making a
web server, and I want to:

* Read some data from a file
* Send a HTTP request containing some of the data from the file
* Write some data to a file containing some of the data from the HTTP response

Assuming I'm writing this on a Roc platform which has a `Task`-based API,
here's how that code might look:

```elm
doStuff = \filename ->
    Task.after (File.read filename) \fileData ->
    Task.after (Http.get (urlFromData fileData)) \response ->
    File.write filename (responseToData response)
```

A few things to note before getting into how this relates to custom types:

1. This is written in a style designed for chained effects. It's kinda like [`do` notation](https://en.wikibooks.org/wiki/Haskell/do_notation), but implemented as a formatting convention instead of special syntax.
2. In Elm you'd need to add a `<|` before the anonymous functions (e.g. `<| \response ->`) but in Roc you don't. (That parsing design decision was partly motivated by supporting this style of chained effects.)
3. `Task.after` is `Task.andThen` with its arguments flipped.

What would the type of the above expression be? Let's say these function calls
have the following types:

```elm
File.read : Filename -> Task File.Data File.ReadErr
File.write : Filename, File.Data -> Task File.Data File.WriteErr
Http.get : Url -> Task Http.Response Http.Err

after : Task a err, (a -> Task b err) -> Task b err
```

If these are the types, the result would be a type mismatch. Those `Task` values
have incompatible error types, so `after` won't be able to chain them together.

This situation is one of the motivations behind Roc's *tags* feature. Using tags,
not only will this type-check, but at the end we get a combined error type which
has the union of all the possible errors that could have occurred in this sequence.
We can then handle those errors using a single `when`, like so:

```elm
when error is
    # Http.Err possibilities
    PageNotFound -> ...
    Timeout -> ...
    BadPayload -> ...

    # File.ReadErr possibilities
    FileNotFound -> ...
    ReadAcessDenied -> ...
    FileCorrupted -> ...

    # File.WriteErr possibilities
    DirectoryNotFound -> ...
    WriteAcessDenied -> ...
    DiskFull -> ...
```



Here is a set
of slightly different types that would make the above expression compile.
(`after` is unchanged.)

```elm
File.read : Filename -> Task File.Data (File.ReadErr *)
File.write : Filename, File.Data -> Task File.Data (File.WriteErr *)
Http.get : Url -> Task Http.Response (Http.Err *)

after : Task a err, (a -> Task b err) -> Task b err
```

The key is that each of the error types expands to a Roc *tag union*. Here's how
they look:

```elm
Http.Err a : [PageNotFound, Timeout, BadPayload]a
File.ReadErr a : [FileNotFound, Corrupted, BadFormat]a
File.WriteErr a : [FileNotFound, DiskFull]a
```


```elm
first : List elem -> [Ok elem, ListWasEmpty]*
```

> It's motivated primarily by error handling in chained effects
> (e.g. multiple consecutive `Task.andThen`s between tasks with incompatible error types),
> which doesn't really come up in Elm but often will in Roc. Explaining how
> this improves that situation is out of scope for this document; even without
> that explanation, this section is already the longest!

Instead of traditional algebraic data types (like Elm has), Roc uses something more like
OCaml's *polymorphic variants*. In Roc they're called *tags*. Here are some examples of using tags in a REPL:

```
> True
True : [ True ]*

> False
False : [ False ]*

> Ok 5
Ok 5 : [ Ok Int ]*

> SomethingIJustMadeUp 1 2 "stuff"
SomethingIJustMadeUp 1 2 "stuff" : [ SomethingIJustMadeUp Int Int Str ]*

> x = Foo
Foo : [ Foo ]*

> y = Foo "hi" 5
Foo "hi" 5 : [ Foo Str Int ]*

> z = Foo 1 2
Foo 1 2 : [ Foo Int Int ]*
```

Tags are different from variants in Elm in several ways.

One difference is that you can make up any tag you want, on the fly, and use it in any module,
without declaring it first. (These cannot be used to create opaque types; we'll discuss those
in the next section.)

Another difference is that the same tag can be used with different arities and types.
In the REPL above, `x`, `y`, and `z`, can all coexist in the same module even though
they use `Foo` with different arities - and also with different types within the same arity.

Now let's say I do a pattern match with no type annotations.

```elm
when foo is
    MyInt num -> num + 1
    MyFloat float -> Num.round float
```

The inferred type of this expression would be `[ MyInt Int, MyFloat Float ]`,
based on its usage.

> As with OCaml's polymorphic variants, exhaustiveness checking is still in full effect here.
> It's based on usage; if any code pathways led to `foo` being set to the tag `Blah`,
> I'd get an exhaustiveness error because this `when` does not have a `Blah` branch.

There's an important interaction here between the inferred type of a *when-expression* and
the inferred type of a tag value. Note which types have a `*` and which do not.

```elm
x : [ Foo ]*
x = Foo

y : [ Bar Float ]*
y = Bar 3.14

toInt : [ Foo, Bar Float ] -> Int
toInt = \tag ->
    when tag is
        Foo -> 1
        Bar float -> Num.round float
```

Each of these type annotations involves a *tag union* - a collection of tags bracketed by `[` and `]`.

* The type `[ Foo, Bar Float ]` is a **closed** tag union.
* The type `[ Foo ]*` is an **open** tag union.

You can pass `x` to `toInt` because an open tag union is type-compatible with
any closed tag union which contains its tags (in this case, the `Foo` tag). You can also
pass `y` to `toInt` for the same reason.

In general, when you make a tag value, you'll get an open tag union (with a `*`).
Using `when` *can* get you a closed union (a union without a `*`) but that's not
always what happens. Here's a `when` in which the inferred type is an open tag union:

```elm
alwaysFoo : [ Foo Int ]* -> [ Foo Int ]*
alwaysFoo = \tag ->
    when tag is
        Foo num -> Foo (num + 1)
        _ -> Foo 0
```

The return value is an open tag union because all branches return something
tagged with `Foo`.

The argument is also an open tag union, because this *when-expression* has
a default branch; that argument is compatible with any tag union. This means you
can pass the function some totally nonsensical tag, and it will still compile.

> Note that the argument does *not* have the type `*`. That's because you
> cannot pass it values of any type; you can only pass it tags!
>
> You could, if you wanted, change the argument's annotation to be `[]*` and
> it would compile. After all, its default branch means it will accept any tag!
>
> Still, the compiler will infer `[ Foo Int ]*` based on usage.

Just because `[ Foo Int ]*` is the inferred type of this argument,
doesn't mean you have to accept that much flexibility. You can restrict it
by removing the `*`. For example, if you changed the annotation to this...

```elm
alwaysFoo : [ Foo Int, Bar Str ] -> [ Foo Int ]*
```

...then the function would only accept tags like `Foo 5` and `Bar "hi"`. By writing
out your own annotations, you can get the same level of restriction you get with
traditional algebraic data types (which, after all, come with the requirement that
you write out their annotations). Using annotations, you can restrict even
*when-expressions* with default branches to accept only the values you define to be valid.

In fact, if you want a traditional algebraic data type in Roc, you can get about the same
functionality by making (and then using) a type alias for a closed tag union.
Here's exactly how `Result` is defined using tags in Roc's standard library:

```elm
Result ok err : [ Ok ok, Err err ]
```

You can also use tags to define recursive data structures, because recursive
type aliases are allowed as long as the recursion happens within a tag. For example:

```elm
LinkedList a : [ Nil, Cons a (LinkedList a) ]
```

> Inferred recursive tags use the `as` keyword, which is what OCaml does to
> display inferred types of recursive polymorphic variants. For example, the
> inferred version of the above type alias would be:
>
> `[ Nil, Cons a b ] as b`

The `*` in open tag unions is actually an unbound ("wildcard") type variable.
It can be bound too, with a lowercase letter like any other bound type variable.
Here's an example:

```elm
incrementFoo : [ Foo Int ]a -> [ Foo Int ]a
incrementFoo = \tag ->
    when tag is
        Foo num -> Foo (num + 1)
        other -> other
```

The `*` says "this union can also include any other tags", and here the `a` says
"the return value union includes `Foo Int`, plus whichever other tags the argument
includes in its union."

> The Roc type `[]` is equivalent to Elm's `Never`. You can never satisfy it!

One final note about tags: *tag application* is not the same as *function application*,
the way it is with Elm's variants. For example:

* `foo bar` is function application, because `foo` is lowercase.
* `Foo bar` is tag application, because `Foo` is uppercase.

So this wouldn't compile:

```
foo : [ Foo ]*
foo = Foo

foo bar
```

You can't "call" the type `[ Foo ]*` because it's not a function.

In practical terms, this also means you can't do `|> Decode.map UserId`
because `UserId` is not a function, and `map` expects a function.
This code would work in Elm, but in Roc you'd need to use an anonymous function -
e.g. `|> Decode.map (\val -> UserId val)` - or a helper function, e.g.
`|> Decode.map UserId.fromInt`

## Opaque Types

The tags discussed in the previous section are globally available, which means
they cannot be used to create opaque types.

*Private tags* let you create opaque types. They work just like the *global tags*
from the previous section, except:

* Private tags begin with an `@` (e.g. `@Foo` instead of `Foo`)
* Private tags are scoped to the current module, rather than globally scoped
* Private tags can only be instantiated in the current module

For example, suppose I define these inside the `UserId` module:

```elm
UserId : [ @UserId Int ]

fromInt : Int -> UserId
fromInt = \int ->
    @UserId int

toInt : UserId -> Int
toInt = \@UserId int ->
    int
```
I can now expose the `UserId` type alias, which other modules can use as an opaque type.

It's not even syntactically possible for me to expose the `@UserId` tag, because `@` tags are not allowed in the exposing list. Only code written in this `UserId` module can instantiate a `@UserId` instance.

> If I were to write `@UserId` inside another module (e.g. `Main`), it would compile,
> but that `@UserId` would be type-incompatible with one created inside the `UserId` module.
> Even trying to use `==` on them would be a type mismatch, because I would be comparing
> a `[ UserId.@UserId Int ]*` with a `[ Main.@UserId Int ]*`, which are incompatible.

## Phantom Types

[Phantom types](https://medium.com/@ckoster22/advanced-types-in-elm-phantom-types-808044c5946d)
exist in Elm but not in Roc. This is because phantom types can't be defined
using type aliases (in fact, there is a custom error message in Elm if you
try to do this), and Roc only has type aliases. However, in Roc, you can achieve
the same API and runtime performance characteristics as if you had phantom types,
only using *phantom values* instead.

A phantom value is one which affects types, but which holds no information at runtime.
As an example, let's say I wanted to define a [units library](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/) -
a classic example of phantom types. I could do that like this:

```
Quantity units data : [ Quantity units data ]

km : Num n -> Quantity [ Km ] (Num n)
km = \num ->
    Quantity Km num

cm : Num n -> Quantity [ Cm ] (Num n)
cm = \num ->
    Quantity Cm num

mm : Num n -> Quantity [ Mm ] (Num n)
mm = \num ->
    Quantity Mm num

add : Quantity u (Num n), Quantity u (Num n) -> Quantity u (Num n)
add = \Quantity units a, Quantity _ b ->
    Quantity units (a + b)
```

From a performance perspective, it's relevant here that `[ Km ]`, `[ Cm ]`, and `[ Mm ]`
are all unions containing a single tag. That means they hold no information at runtime
(they would always destructure to the same tag), which means they can be "unboxed" away -
that is, discarded prior to code generation.

During code generation, Roc treats `Quantity [ Km ] Int` as equivalent to `Quantity Int`.
Then, becaue `Quantity Int` is an alias for `[ Quantity Int ]`, it will unbox again
and reduce that all the way down to to `Int`.

This means that, just like phantom *types*, phantom *values* affect type checking
only, and have no runtime overhead. Rust has a related concept called [phantom data](https://doc.rust-lang.org/nomicon/phantom-data.html).

## Modules and Shadowing

In Elm, my main module (where `main` lives) might begin like this:

```elm
module MyApp exposing (main)

import Parser
import Http exposing (Request)
import Task exposing (Task, after)
```

Roc application modules (where the equivalent of `main` lives) begin with the
`app` keyword rather than the `module` keyword, and the import syntax is a bit different.
Here's how the above module header would look in Roc:

```elm
app imports [ Parser, Http.{ Request }, Task.{ Task, after } ]
```

`app` modules are application entrypoints, and they don't formally expose anything.
They also don't have names, so other modules can't even import them!

Modules that *can* be imported are `interface` modules. Their headers look like this:

```elm
interface Parser
    exposes [ Parser, map, oneOf, parse ]
    imports [ Utf8 ]
```

The name `interface` is intended to draw attention to the fact that the interface
these expose is very important.

All imports and exports in Roc are enumerated explicitly; there is no `..` syntax.

> Since neither global tags nor private tags have a notion of "importing variants"
> (global tags are always available in all modules, and private tags are
> never available in other modules), there's also no `exposing (Foo(..))` equivalent.

Like Elm, Roc does not allow shadowing.

Elm does permit overriding open imports - e.g. if you have
`import Foo exposing (bar)`, or `import Foo exposing (..)`, you can still define
`bar = ...` in the module. Roc considers this shadowing and does not allow it.

## Record Syntax

Roc uses Rust/JavaScript syntax for record literals, e.g. `{ x: 1, y: 2 }`.

It also allows omitting the value; `{ x, y }` is sugar for `{ x: x, y: y }`.

You can pattern match on exact record values, e.g. `{ x: 5 } ->`.

Roc does not have the "a type alias for a record creates a convenience constructor function"
feature that Elm has. This is partly because `Point x y` is already defined to be tag application
in Roc, but also because `{ x, y }` would be the recommended way to write it regardless.

Closed record annotations look the same as they do in Elm, e.g.
`{ x : Int, y : Int }`. Open record annotations look a bit different.

In Elm:

```
{ a | x : Int, y : Int } -> Int
```

In Roc:

```
{ x : Int, y : Int }* -> Int
```

Here, the open record's type variable appears attached to the `}`.

> In the Elm example, the `a` is unbound, which in Roc means it appears as `*`.

Here's how that looks with a bound type varaible. In Elm:

```elm
{ a | x : Int, y : Int } -> { a | x : Int, y : Int }
```

In Roc:

```elm
{ x : Int, y : Int }a -> { x : Int, y : Int }a
```

By design, this syntax makes the unbound case look natural and the bound case look unnatural.

That's because writing a function that accepts an open record with an unbound
type variable (e.g. "this record, plus other fields if you like") is a totally reasonable
thing to do - as often as you like! It has multiple upsides: it makes "named arguments"
work with data model records more often, and makes it easier to change functions in
backwards-compatible ways. It has no major downsides.

The syntax encourages doing this. "Just add a star" like so:

```elm
{ x : Int, y : Int }* -> Int
```

In contrast, using records with bound variables should be extremely rare.

They need to exist for the type system to work, and they aren't *useless*,
but if you find yourself reaching for them, there is an extremely high chance that there's a
better way to write that code.

The unnatural-looking syntax is the language's way of nudging you to reconsider,
to search a little further for a better way to express things.

## Record Update

Elm has "record update" syntax like this:

```elm
{ user | firstName = "Sam", lastName = "Sample" }
```

Roc has the same feature, but its syntax looks like this:

```elm
{ user & firstName: "Sam", lastName: "Sample" }
```

The record before the `&` can be qualified, like so:

```elm
{ Foo.defaultConfig & timeZone: utc }
```

However, it cannot involve record field access. So this would *not* compile:

```elm
{ Foo.defaults.config & timeZone: utc }
```

## Optional Record Fields

There's a pattern in Elm where you pass a function a record of configuration
values, some of which you don't really care about and want to leave as defaults.
To incorporate the default config options, you call the function like so:

```elm
table { defaultConfig | height = 800, width = 600 }
```

This way, as the caller I'm specifying only the `height` and `width` fields,
and leaving the others to whatever is inside `defaultConfig`. Perhaps it also
has the fields `x` and `y`.

In Roc, you can do this like so:

```elm
table { height = 800, width = 600 }
```

...and the `table` function will fill in its default values for `x` and `y`.
There is no need to use a `defaultConfig` record.

Here's how `table` would be defined in Roc:

```
table = \{ height, width, x ? 0.0, y ? 0.0 } ->
```

This is using *optional field destructuring* to destructure a record while
also providing default values for any fields that might be missing.
Here's the type of `table`:

```
table : { height : Float, width : Float, x ? Float, y ? Float } -> Table
table = \{ height, width, x ? 0.0, y ? 0.0 } ->
```

This says that `table` takes a record with two *required* fields (`height` and
`width` and two *optional* fields (`x` and `y`). It also says that all of those
fields have the type `Float` This means you can choose to omit `x`, `y`, or both,
when calling the function...but if you provide them, they must be numbers.

This is also the type that would have been inferred for `table` if no annotation
had been written. Roc's compiler can tell from the destructuring syntax
`x ? 0.0` that `x` is an optional field, and that it has the type `Float`. These
default values can reference other expressions in the record destructure; if you
wanted, you could write `{ height, width, x ? 0.0, y ? x + 1 }`.

Destructuring is the only way to implement a record with optional fields.
(For example, if you write the expression `config.x` and `x` is an optional field,
you'll get a compile error.)

This means it's never possible to end up with an "optional value" that exists
outside a record field. Optionality is a concept that exists only in record fields,
and it's intended for the use case of config records like this. The ergonomics
of destructuring mean this wouldn't be a good fit for data modeling.

## Function equality

In Elm, if you write `(\val -> val) == (\val -> val)`, you currently get a runtime exception
which links to [the `==` docs](https://package.elm-lang.org/packages/elm/core/latest/Basics#==),
which explain why this is the current behavior and what the better version will look like.

> OCaml also has the "runtime exception if you compare functions for structural equality"
> behavior, but unlike Elm, in OCaml this appears to be the long-term design.

In Roc, function equality is a compile error, tracked explicitly in the type system.
Here's the type of Roc's equality function:

```elm
'val, 'val -> Bool
```

Whenever a named type variable in Roc has a `'` at the beginning, that means
it is a *functionless* type - a type which cannot involve functions.
If there are any functions in that type, it's a type mismatch. This is true
whether `val` itself is a function, or if it's a type that wraps a function,
like `{ predicate: Int -> Bool }` or `List (Bool -> Bool)`.

So if you write `(\a -> a) == (\a -> a)` in Roc, you'll get a type mismatch.
If you wrap both sides of that `==` in a record or list, you'll still get a
type mismatch.

If a named type variable has a `'` anywhere in a given type, then it must have a `'`
everywhere in that type. So it would be an error to have a type like `x, 'x -> Bool`
because `x` has a `'` in one place but not everywhere.

## Standard Data Structures

Elm has `List`, `Array`, `Set`, and `Dict` in the standard library.

Roc has `List`, `Set`, and `Map` in the standard library.

Here are the differences:

* `List` in Roc uses the term "list" the way Python does: to mean an unordered sequence of elements. Roc's `List` is more like an array, in that all the elements are sequential in memory and can be accessed in constant time. It still uses the `[` `]` syntax for list literals. Also there is no `::` operator because "cons" is not an efficient operation on an array like it is in a linked list.
* `Map` in Roc is like `Dict` in Elm, except it's backed by hashing rather than ordering. Roc silently computes hash values for any value that can be used with `==`, so instead of a `comparable` constraint on `Set` elements and `Map` keys, in Roc they instead have the *functionless* constraint indicated with a `'`. So to add to a `Set` you use `Set.add : Set 'elem, 'elem -> Set 'elem`, and putting a value into a Map is `Map.put : Map 'key val, 'key, val -> Map 'key val`.
* `Set` in Roc is like `Set` in Elm: it's shorthand for a `Map` with keys but no value, and it has a slightly different API.

> The main reason it's called `Map` instead of `Dict` is that it's annoying to have a conversation about `Dict` out loud, let alone to teach it in a workshop, because you have to be so careful to enunciate. `Map` is one letter shorter, doesn't have this problem, is widely used, and never seems to be confused with the `map` function in practice (in e.g. JavaScript and Rust, both of which have both `Map` and `map`) even though it seems like it would in theory.

Roc also has a literal syntax for maps and sets. Here's how to write a `Map` literal:

```elm
{: "Sam" => 1, "Ali" => 2, firstName => 3 :}
```

This expression has the type `Map Str Int`, and the `firstName` variable would
necessarily be a `Str` as well.

The `Map` literal syntax is for two reasons. First, Roc doesn't have tuples; without tuples, initializing the above `Map` would involve an API that looked something like one of these:

```elm
Map.fromList [ { k: "Sam", v: 1 }, { k: "Ali", v: 2 }, { k: firstName, v: 3 } ]

Map.fromList [ KV "Sam" 1, KV "Ali" 2, KV firstName 3 ]
```

This works, but is not nearly as nice to read.

Additionally, map literals can compile direcly to efficient initialization code without needing to (hopefully be able to) optimize away the intermediate `List` involved in  `fromList`.

`{::}` is an empty `Map`.

You can write a `Set` literal like this:

```elm
[: "Sam", "Ali", firstName :]
```

The `Set` literal syntax is partly for the initialization benefit, and also for symmetry
with the `Map` literal syntax.

`[::]` is an empty `Set`.

Roc does not have syntax for pattern matching on data structures - not even `[` `]` like Elm does.

## Numbers

Like Elm, Roc has two numeric types: `Int` and `Float`. `Float` is the same as it is in
Elm: it's a 64-bit floating point number. In Roc, `Int` is a 64-bit integer. Also
unlike Elm, if you encounter an overflow with either of them, you get a runtime
exception rather than wrapping overflow behavior (or a float becoming `Infinity` or `-Infinity`).
You can opt into wrapping overflow instead with functions like `Int.wrapAdd`,
or into a function that returns a `Result` that gives `Err` if it overflows, like `Int.tryAdd`.

Roc does not let floating point calculations result in `Infinity`, `-Infinity`, or `NaN`.
Any operation which would result in one of these (such as `sqrt` or `/`) will
result in a runtime exception. Similarly to overflow, you can opt into handling these
a different way, such as `Float.trySqrt` which returns a `Result`.

The way `+` works here is also a bit different than in Elm. Imagine if Elm's
`(+)` operator had this type:

```elm
Num a -> Num a -> Num a
```

Now imagine if `Int` were actually a type alias for `Num Integer`, and `Float` were actually
a type alias for `Num FloatingPoint`. That's exactly how things work in Roc.

(`Integer` and `FloatingPoint` are both defined like `Never`; you can never instantiate one.
They are used only as phantom types.)

So Roc does not use `number`, but rather uses `Num` - which works more like `List`.
Either way, you get `+` being able to work on both `Int` and `Float`!

## `comparable`, `appendable`, and `number`

These don't exist in Roc.

* `appendable` is only used in Elm for the `(++)` operator, and Roc doesn't have that operator.
* `comparable` is used for comparison operators (like `<` and such), plus `List.sort`, `Dict`, and `Set`. Roc's `List.sort` accepts a `Sorter` argument which specifies how to sort the elements. Roc's comparison operators (like `<`) only accept numbers; `"foo" < "bar"` is valid Elm, but will not compile in Roc. Roc's dictionaries and sets are hashmaps behind the scenes (rather than ordered trees), and their keys have no visible type restrictions.
* `number` is replaced by `Num`, as described earlier.

Like in Elm, number literals with decimal points are `Float`. However, number
literals *without* a decimal point are `Num *` instead of `number`. 
Also [like Python](https://www.python.org/dev/peps/pep-0515/)
Roc permits underscores in number literals for readability purposes. Roc also supports 
hexadecimal (`0x01`), octal (`0o01`), and binary (`0b01`) integer literals; these 
literals all have type `Int` instead of `Num *`.

If you put these into a hypothetical Roc REPL, here's what you'd see:

```elm
> 1_024 + 1_024
2048 : Num *

> 1 + 2.14
3.14 : Float

> 1.0 + 1
2.0 : Float

> 1.1 + 0x11
<type mismatch between 1.1 : Float and 0x11 : Int>

> 11 + 0x11
28 : Int
```

## Operators

In Elm, operators are functions. In Roc, all operators are syntax sugar.

This means, for example, that you cannot write `(/)` in Roc; that would be a syntax
error. However, the `/` operator in Roc is infix syntax sugar for `Num.div`,
which is a normal function you can pass to anything you like.

Elm has one unary operator, namely `-`. (In Elm, `-x` means
"apply unary `negate` to `x`.") Roc has that one, and also unary `!`.
The expression `!foo` desugars to `not foo`, and `!foo bar` desugars to `not (foo bar)`.

This was introduced because Roc does not expose any functions globally by default
(the way Elm does with `Basics` functions like `not`, `round`, etc.).
In Roc, only operators and standard types (like `Int` and `Bool`) are exposed globally.
Having to fully qualify `not` was annoying, and making an exception just for `not` seemed
less appealing than making an operator for it, especially when unary `!` is so widely used
in other languages.

Because Roc has unary `!`, its "not equal to" operator is `!=` instead of Elm's `/=`,
for symmetry with unary `!`.

There's an Operator Desugaring Table at the end of this guide, so you can see exactly
what each Roc operator desugars to.

## The `<|` operator

Roc has no `<|` operator. (It does have `|>` though.)

In Elm, `<|` is used as a minor convenience for when you want to avoid some parens
in a single-line expression (e.g. `foo <| bar baz` over `foo (bar baz)`) and as
a major convenience when you want to pass an anonymous function, `if`, or `case` as an argument.

For example, `elm-test` relies on it:

```elm
test "it works" <|
    \_ -> ...
```

In Roc, this does not require a `<|`. This Roc code does the same thing as the preceding Elm code:

```elm
test "it works"
    \_ -> ...
```

You don't need parens or an operator to pass an anonymous function, `when`, or `if` as arguments. Here's another example:

```elixir
foo 1 2 if something then 3 else 4

# Same as `foo 1 2 (if something then 3 else 4)`
```

[CoffeeScript](http://coffeescript.org/) also does this the way Roc does.

## Currying and `|>`

Roc functions aren't curried. Calling `(List.append foo)` is a type mismatch
because `List.append` takes 2 arguments, not 1.

For this reason, function type annotations separate arguments with `,` instead of `->`. In Roc, the type of `List.take` is:

```elm
List.take : List a, Int -> List a
```

You might also notice that Roc's `List.take` takes its arguments in the reverse order
from how they are in Elm; the `List` is the first argument in Roc, whereas it would
be the last argument in Elm. This is because Roc's `|>` operator works like Elixir's
rather than like Elm's; here is an example of what it does in Roc:

```elixir
a b c
    |> f x y

# f (a b c) x y
```

In Roc, the `|>` operator inserts the previous expression as the *first* argument
to the subsequent expression, rather than as the *last* argument as it does in Elm.

This makes a number of operations more useful in pipelines. For example, in Roc, `|> Num.div 2.0` divides by 2:

```elixir
2000
  |> Num.div 2.0

# 1000.0 : Float
```

In Elm, where `|>` inserts 2 as the last argument, 2 ends up being the *numerator*
rather than the denominator:

```elm
2000
  |> (/) 2.0

# 0.001 : Float
```

Another example is `List.append`. In Roc:

```elixir
[ 1, 2 ]
  |> List.append [ 3, 4 ]

# [ 1, 2, 3, 4 ]
```

In Elm:

```elm
[ 1, 2 ]
  |> List.append [ 3, 4 ]

# [ 3, 4, 1, 2 ]
```

> There are various trade-offs here, of course. Elm's `|>` has a [very elegant implementation](https://github.com/elm/core/blob/665624859a7a432107059411737e16d1d5cb6373/src/Basics.elm#L873-L874), and `(|>)` in Elm can be usefully passed to other
> functions (e.g. `fold`) whereas in Roc it's not even possible to express the type of `|>`.

As a consequence of `|>` working differently, "pipe-friendly" argument ordering is also
different. That's why `List.take` has a "flipped" signature in Roc; otherwise, `|> List.take 5` wouldn't work. Here's the type of Roc's `List.take` again, and also a pipeline using it:

```coffeescript
List.take : List a, Int -> List a

[ 1, 2, 3, 4, 5 ]
    |> List.take 3

# The above expression gives the same answer it would in Elm.
```

Roc has no `<<` or `>>` operators, and there are no functions in the standard library
for general-purpose pointfree function composition.

## Standard library

`elm/core` has these modules:

* `Array`
* `Basics`
* `Bitwise`
* `Char`
* `Debug`
* `Dict`
* `List`
* `Maybe`
* `Platform`
* `Platform.Cmd`
* `Platform.Sub`
* `Process`
* `Result`
* `Set`
* `String`
* `Task`
* `Tuple`

In Roc, the standard library is not a standalone package. It is baked into the compiler,
and you can't upgrade it independently of a compiler release; whatever version of
Roc you're using, that's the version of the standard library you're using too.
(This is because Roc doesn't have a concept like Elm's `Kernel`; it would not be
possible to ship Roc's standard library as a separate package!)

Roc's standard library has these modules:

* `Bool`
* `Num`
* `Int`
* `Float`
* `List`
* `Map`
* `Set`
* `Bytes`
* `Str`
* `Result`

Some differences to note:

* All these standard modules are imported by default into every module. They also expose all their types (e.g. `Bool`, `Int`, `Result`) but they do not expose any values - not even `negate` or `not`. (`True`, `False`, `Ok`, and `Err` are all global tags, so they do not need to be exposed; they are globally available regardless!)
* In Roc it's called `Str` instead of `String`.
* No `Char`. This is by design. What most people think of as a "character" is a rendered glyph. However, rendered glyphs are comprised of [grapheme clusters](https://stackoverflow.com/a/27331885), which are a variable number of Unicode code points - and there's no upper bound on how many code points there can be in a single cluster. In a world of emoji, I think this makes `Char` error-prone and it's better to have `String` be the smallest indivisible unit. If you want to iterate over grapheme clusters, use a `Str -> List Str` function which breaks the string down on grapheme boundaries. For this reason there also isn't a `Str.length` function; in the context of strings, "length" is ambiguous. (Does it refer to number of bytes? Number of Unicode code points? Number of graphemes?)
* No `Basics`. You use everything from the standard library fully-qualified; e.g. `Bool.isEq` or `Num.add` or `Float.ceiling`. There is no `Never` because `[]` already serves that purpose. (Roc's standard library doesn't include an equivalent of `Basics.never`, but it's one line of code and anyone can implmement it: `never = \a -> never a`.)
* No `Tuple`. Roc doesn't have tuple syntax. As a convention, `Tup` can be used to represent tuples (e.g. `List.zip : List a, List b -> List [ Tup a b ]*`), but this comes up infrequently compared to languages that have dedicated syntax for it.
* No `Task`. By design, platform authors implement `Task` (or don't; it's up to them) - it's not something that really *could* be usefully present in Roc's standard library.
* No `Process`, `Platform`, `Cmd`, or `Sub` - similarly to `Task`, these are things platform authors would include, or not.
* No `Maybe`. This is by design. If a function returns a potential error, use `Result` with an error type that uses a zero-arg tag to describe what went wrong. (For example, `List.first : List a -> Result a [ ListWasEmpty ]*` instead of `List.first : List a -> Maybe a`.) If you want to have a record field be optional, use an Optional Record Field directly (see earlier). If you want to describe something that's neither an operation that can fail nor an optional field, use a more descriptive tag - e.g. for a nullable JSON decoder, instead of `nullable : Decoder a -> Decoder (Maybe a)`, make a self-documenting API like `nullable : Decoder a -> Decoder [ Null, NonNull a ]*`.
* `List` refers to something more like Elm's `Array`, as noted earlier.

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
