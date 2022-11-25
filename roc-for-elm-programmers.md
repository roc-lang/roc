# Roc for Elm programmers

Roc is a direct descendant of the [Elm programming language](https://elm-lang.org/). The two languages are similar, but not the same!

This is a guide to help Elm programmers learn what's different between Elm and Roc.

> NOTE: Almost all that's in this document has been implemented - but not quite all of it!

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
weird situations like string literals inside string literals don't come up.

Roc strings also have the type `Str` rather than Elm's `String`. This is to make
common qualified operations like `Str.join` more concise; the idea is that you'll use the
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
a [`Debug.todo`](https://package.elm-lang.org/packages/elm/core/latest/Debug#todo).
If it ever gets run, it will crash, but for debugging purposes or sketching out
APIs, you don't need to bother writing `getUsername = Debug.todo "implement"`.

## `let` syntax

Imagine if Elm's `let`...`in` worked exactly the same way, except you removed
the `let` and `in` keywords. That's how it works in Roc.

For example, consider this Elm code:

```elm
numbers =
    let
        num1 =
            123

        num2 =
            456
    in
    [num1, num2]
```

Here's the equivalent Roc code:

```elm
numbers =
    num1 =
        123

    num2 =
        456

    [num1, num2]
```

Like `let`...`in` in Elm, this is indentation-sensitive. Each of the definitions
("defs" for short) must have the same indentation as the ending expression.

Roc has a built-in formatter that has a lot in common with `elm-format` (e.g. no configuration,
no enforced line length) but also some stylistic differences. One notable difference is that
it doesn't use as much spacing. For example, if you ran `roc format` on the following Roc
code, the formatter would not change it:

```elm
numbers =
    num1 = 123
    num2 = 456

    [num1, num2]
```

## Function definitions

Roc only has one syntax for defining functions, and it looks almost exactly
like Elm's anonymous functions. The one difference is that multiple arguments
are separated by commas.

So where in Elm you might write `foo a b =` in Roc you'd write `foo = \a, b ->` instead.

One minor benefit of the comma is that you don't need to use parentheses to
destructure arguments inline. For example, in Elm, you always need to use parens
to destructure variants inline in function declarations, like in these two examples:

```elm
\(UserId id1) (UserId id2) ->
```

```elm
\(UserId id) ->
```

Without the parentheses, it wouldn't be clear where one argument ended and the next one began.

In Roc, the commas make argument boundaries unambiguous, so no parens are needed.
You can write the above like so in Roc:

```elm
\UserId id1, UserId id2 ->
```

```elm
\UserId id ->
```

## Unbound type variables

In Elm, every type variable is named. For example:

```elm
List.reverse : List a -> List a

[] : List a
```

The `a` in `List.reverse` is a *bound* type variable, because it appears more than once in the type.
Whatever the first list's `a` is, that's what the second list's `a` must be as well.

The `a` in `[] : List a` is an *unbound* type variable. It has no restrictions,
which is why `[]` can be passed to any function that expects a `List`.

In Roc, this distinction between bound and unbound type variables is reflected at
the syntax level. Here are those types in Roc:

```elm
List.reverse : List a -> List a

[] : List *
```

The `*` is the "wildcard" type variable. It is only for unbound type variables like this.
Like the wildcard `*` in path globs like `*.txt`, it matches anything.

> You can still choose names for unbound type variables if you like, but the
> compiler will infer them as `*` by default.

In Elm, the type of `always` is `a -> (b -> a)`. The equivalent Roc type would be:

```elm
always : a -> (* -> a)
```

This makes unbound type variables easier to talk about out loud. Rather than saying
(for example) "List a" or "Html msg with a lowercase m" you can say "List star" or "Html star".

## Record Syntax

Roc uses Rust/JavaScript syntax for record literals, e.g. `{ x: 1, y: 2 }`.

It also allows omitting the value; `{ x, y }` is sugar for `{ x: x, y: y }`.

You can pattern match on exact record values, e.g. `{ x: 5 } ->`.

Roc does not have the "a type alias for a record creates a convenience constructor function"
feature that Elm has. However, it does allow trailing commas, in both values and
type annotations. Roc's formatter (which is built into the compiler, and which
is zero-configuration like `elm-format`) formats multi-line record literals (and
record types) with a comma at the end of each line, like so:

```elm
user = {
    firstName: "Sam",
    lastName: "Sample",
    email: "sam@example.com",
}
```

This is easy to read and leads to tidy version control diffs; no matter how
you reorder these fields, or add or remove fields, the diffs will only be of the
relevant fields in question, not any adjacent fields or tokens.

> Lists are formatted similarly to records, except of course they don't have labeled fields.

Closed record annotations look the same as they do in Elm, e.g.
`{ name : Str, email : Str }`. Open record annotations look a bit different.

In Elm:

```elm
{ a | name : Str, email : Str } -> Str
```

In Roc:

```coffee
{ name : Str, email : Str }* -> Str
```

Here, the open record's type variable appears immediately after the `}`.

> In the Elm example, the `a` is unbound, which in Roc means it appears as `*`.

This syntax makes it easier to write a function that accepts an open record with an unbound
type variable (e.g. "this record, plus other fields if you like"). This is a totally reasonable
thing to do - as often as you like! It has multiple upsides: it makes "named arguments"
work with data model records more often, and makes it easier to change functions in
backwards-compatible ways. It has no major downsides.

The syntax encourages doing this. "Just add a star" like so:

```elm
{ name : Str, email : Str }* -> Str
```

You can also use bound type variables too. In Elm:

```elm
{ a | name : Str, email : Str } -> { a | name : Str, email : Str }
```

In Roc:

```elm
{ name : Str, email : Str }a -> { name : Str, email : Str }a
```

> Like in Elm, using records with bound variables should be extremely rare.
> They need to exist for the type system to work, and they aren't *useless*,
> but any time you find yourself reaching for them, there is a very high chance
> that there's a better way to write that code!

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

```javascript
table { height: 800, width: 600 }
```

...and the `table` function will fill in its default values for `x` and `y`.
There is no need to use a `defaultConfig` record.

Here's how that `table` function would be implemented in Roc:

```elixir
table = \{ height, width, title ? "", description ? "" } ->
```

This is using *optional field destructuring* to destructure a record while
also providing default values for any fields that might be missing.
Here's the type of `table`:

```elixir
table :
    {
        height : Pixels,
        width : Pixels,
        title ? Str,
        description ? Str,
    }
    -> Table
table = \{ height, width, title ? "", description ? "" } ->
```

This says that `table` takes a record with two *required* fields (`height` and
`width` and two *optional* fields (`title` and `description`). It also says that
the `height` and `width` fields have the type `Pixels` (a type alias for some
numeric type), whereas the `title` and `description` fields have the type `Str`.
This means you can choose to omit `title`, `description`, or both, when calling
the function...but if you provide them, they must have the type `Str`.

This is also the type that would have been inferred for `table` if no annotation
had been written. Roc's compiler can tell from the destructuring syntax
`title ? ""` that `title` is an optional field, and that it has the type `Str`.
These default values can reference other expressions in the record destructure; if you
wanted, you could write
`{ height, width, title ? "", description ? Str.concat "A table called " title }`.

Destructuring is the only way to implement a record with optional fields.
(For example, if you write the expression `config.title` and `title` is an
optional field, you'll get a compile error.)

This means it's never possible to end up with an "optional value" that exists
outside a record field. Optionality is a concept that exists only in record
fields, and it's intended for the use case of config records like this. The
ergonomics of destructuring mean this wouldn't be a good fit for data modeling.

## Pattern matching

Roc's pattern matching conditionals work about the same as how they do in Elm.
Here are two differences:

- Roc uses the syntax `when`...`is` instead of `case`...`of`
- In Roc, you can use `|` to handle multiple patterns in the same way

For example:

```rust
when color is
    Blue -> 1
    Green | Red | Yellow -> 2
    Purple -> 3
```

Like Rust, you can add an `if` guard after a pattern:

```rust
when color is
    Blue -> 1
    Green | Red | Yellow if totalColors >= 3 -> 2
    Green | Red | Yellow -> 4 # only gets run if totalColors < 3
```

This gives you a way to use constants in patterns:

```rust
pi = 3.14
e = 2.72

when number is
    0 -> "zero"
    1 -> "one"
    v if v == pi -> "pi"
    v if v == e -> "e"
    _ -> ""
```

## Booleans

Roc has a `Bool` module (with operations like `Bool.and` and `Bool.or`; Roc does not have
a `Basics` module), and `Bool` is an opaque type. The values `Bool.true` and `Bool.false` work
like `True` and `False` do in Elm.

## Custom Types

This is the biggest semantic difference between Roc and Elm.

Let's start with the motivation. Suppose I'm using a platform for making a
web server, and I want to:

- Read some data from a file
- Send an HTTP request containing some of the data from the file
- Write some data to a file containing some of the data from the HTTP response

Assuming I'm writing this on a Roc platform which has a `Task`-based API,
and that `Task.await` is like Elm's `Task.andThen` but with the arguments
flipped, here's one way I might write this:

```elm
doStuff = \filename ->
    Task.await (File.read filename) \fileData ->
        Task.await (Http.get (urlFromData fileData)) \response ->
            File.write filename (responseToData response)
```

> Note that in Elm you'd need to add a `<|` before the anonymous functions
> (e.g. `<| \response ->`) but in Roc you don't.

What would the type of the above expression be? Let's say these function calls
have the following types:

```elm
File.read : Filename -> Task File.Data File.ReadErr
File.write : Filename, File.Data -> Task File.Data File.WriteErr
Http.get : Url -> Task Http.Response Http.Err

Task.await : Task a err, (a -> Task b err) -> Task b err
```

If these are the types, the result would be a type mismatch. Those `Task` values
have incompatible error types, so `await` won't be able to chain them together.

This situation is one of the motivations behind Roc's *tags* feature. Using tags,
not only will this type-check, but at the end we get a combined error type which
has the union of all the possible errors that could have occurred in this sequence.
We can then handle those errors using a single `when`, like so:

```coffeescript
when error is
    # Http.Err possibilities
    PageNotFound -> ...
    Timeout -> ...
    BadPayload -> ...

    # File.ReadErr possibilities
    FileNotFound -> ...
    ReadAccessDenied -> ...
    FileCorrupted -> ...

    # File.WriteErr possibilities
    DirectoryNotFound -> ...
    WriteAccessDenied -> ...
    DiskFull -> ...
```

Here is the set of slightly different types that will make the original chained
expression compile. (`await` is unchanged.)

```elm
File.read : Filename -> Task File.Data (File.ReadErr *)
File.write : Filename, File.Data -> Task File.Data (File.WriteErr *)
Http.get : Url -> Task Http.Response (Http.Err *)

await : Task a err, (a -> Task b err) -> Task b err
```

The key is that each of the error types is a type alias for a Roc *tag union*.
Here's how those look:

```elm
Http.Err a : [
    PageNotFound,
    Timeout,
    BadPayload Str,
]a

File.ReadErr a : [
    FileNotFound,
    Corrupted,
    BadFormat,
]a

File.WriteErr a : [
    FileNotFound,
    DiskFull,
]a
```

For a side-by-side comparison, here's how we would implement something similar in Elm:

```elm
type Http.Err
    = PageNotFound
    | Timeout
    | BadPayload String

type File.ReadErr
    = FileNotFound
    | Corrupted
    | BadFormat

type File.WriteErr
    = FileNotFound
    | DiskFull
```

There are a few differences between them, but the most significant one here is
that the Roc version has a type variable.

That type variable has a similar purpose to the type variable in Elm's *open records*
(e.g. the `a` in `{ a | name : String, email : String }` which in Roc would be
`{ name : Str, email : Str }a`) - except applied to sum types (such as
Elm's custom types) instead of product types (such as records).

> If you were to remove the type variables from the Roc declarations for
> `Http.Err`, `File.ReadErr`, and `File.WriteErr`, they would work practically
> the same way as the Elm one. Roc *tag unions* can be used as traditional
> algebraic data types, and they have the usual support for pattern matching,
> exhaustiveness checking, and so on.

You don't need to declare tag unions before using them. Instead, you can
just write a *tag* (essentially a variant) anywhere you like, and Roc will infer
the type of the union it goes in.

Here are some examples of using tags in a REPL:

```coffee
> Red
Red : [Red]*

> Blue
Blue : [Blue]*

> Ok "hi"
Ok "hi" : [Ok Str]*

> SomethingIJustMadeUp "hi" "there"
SomethingIJustMadeUp "hi" "there" : [SomethingIJustMadeUp Str Str]*

> x = Foo
Foo : [Foo]*

> y = Foo "hi" Bar
Foo "hi" 5 : [Foo Str [Bar]*]*

> z = Foo ["str1", "str2"]
Foo ["str1", "str2"] : [Foo (List Str)]*
```

The `[` `]`s in the types are tag *unions*, and they list all the possible
different *tags* that the value could be at runtime. In all of these tag unions,
there is only one tag. Notice the `*` at the end; that's the type variable
we saw earlier.

Similarly to how if you put `{ name = "" }` into `elm repl`, it will
infer a type of `{ a | name : String }` - that is, an *open record* with an
unbound type variable and `name : Str` field - if you put a tag `Foo ""` into
`roc repl`, it will infer a type of `[Foo Str]*` - that is, an *open tag union*
with one alternative: a `Foo` tag with a `Str` payload.

The same tag can be used with different arities and types. In the REPL above,
`x`, `y`, and `z`, can all coexist even though they use `Foo` with different
arities - and also with different types within the same arity.

Similarly, you can pattern match on tags without declaring any types, and Roc
will infer the type of the tag union being matched.

For example, suppose we don't write any type annotations anywhere, and have
this pattern match:

```coffeescript
when blah is
    MyStr str -> Str.concat str "!"
    MyBool bool -> Bool.not bool
```

The inferred type of this expression would be `[MyStr Str, MyBool Bool]`.

> Exhaustiveness checking is still in full effect here.  It's based on usage;
> if any code pathways led to `blah` being set to the tag `Foo`, I'd get
> an exhaustiveness error because this `when` does not have a `Foo` branch.

There's an important interaction here between the inferred type of a *when-expression* and
the inferred type of a tag value. Note which types have a `*` and which do not.

```elm
x : [Foo]*
x = Foo

y : [Bar Str]*
y = Bar "stuff"

tagToStr : [Foo, Bar Str] -> Str
tagToStr = \tag ->
    when tag is
        Foo -> "hi"
        Bar str -> Str.concat str "!"
```

Each of these type annotations involves a *tag union* - a collection of tags bracketed by `[` and `]`.

- The type `[Foo, Bar Str]` is a **closed** tag union.
- The type `[Foo]*` is an **open** tag union.

You can pass `x` to `tagToStr` because an open tag union is type-compatible with
any closed tag union which contains its tags (in this case, the `Foo` tag). You can also
pass `y` to `tagToStr` for the same reason.

In general, when you make a tag value, you'll get an open tag union (with a `*`).
Using `when` *can* get you a closed union (a union without a `*`) but that's not
always what happens. Here's a `when` in which the inferred type is an open tag union:

```elm
alwaysFoo : [Foo Str]* -> [Foo Str]*
alwaysFoo = \tag ->
    when tag is
        Foo str -> Foo (Str.concat str "!")
        _ -> Foo ""
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
> Still, the compiler will infer `[Foo Str]*` based on usage.

Just because `[Foo Str]*` is the inferred type of this argument,
doesn't mean you have to accept that much flexibility. You can restrict it
by removing the `*`. For example, if you changed the annotation to this...

```elm
alwaysFoo : [Foo Str, Bar Bool] -> [Foo Str]*
```

...then the function would only accept tags like `Foo "hi"` and `Bar (x == y)`. By writing
out your own annotations, you can get the same level of restriction you get with
traditional algebraic data types (which, after all, come with the requirement that
you write out their annotations). Using annotations, you can restrict even
*when-expressions* with default branches to accept only the values you define to be valid.

In fact, if you want a traditional algebraic data type in Roc, you can get about the same
functionality by making (and then using) a type alias for a closed tag union.
Here's exactly how `Result` is defined using tags in Roc's standard library:

```elm
Result ok err : [Ok ok, Err err]
```

You can also use tags to define recursive data structures, because recursive
type aliases are allowed as long as the recursion happens within a tag. For example:

```elm
LinkedList a : [Nil, Cons a (LinkedList a)]
```

> Inferred recursive tags use the `as` keyword. For example, the
> inferred version of the above type alias would be:
>
> `[Nil, Cons a b] as b`

The `*` in open tag unions is actually an unbound ("wildcard") type variable.
It can be bound too, with a lowercase letter like any other bound type variable.
Here's an example:

```elm
exclaimFoo : [Foo Str]a -> [Foo Str]a
exclaimFoo = \tag ->
    when tag is
        Foo str -> Foo (Str.concat str "!")
        other -> other
```

The `*` says "this union can also include any other tags", and here the `a` says
"the return value union includes `Foo Str`, plus whichever other tags the argument
includes in its union."

> The Roc type `[]` is equivalent to Elm's `Never`. You can never satisfy it!

## Opaque Types

In Elm, you can choose to expose (or not) custom types' constructors in order to create [opaque types](http://sporto.github.io/elm-patterns/advanced/opaque-types.html).
Since Roc's *tags* can be constructed in any module without importing anything, Roc has a separate
*opaque type* language feature to enable information hiding.

As an example, suppose I define these inside the `Username` module:

```elm
Username := Str

fromStr : Str -> Username
fromStr = \str ->
    @Username str

toStr : Username -> Str
toStr = \@Username str ->
    str
```

Here, `Username` is an opaque type. The `fromStr` function turns a string into a `Username`
by "calling" `@Username` on that string. The `toStr` function turns a `Username` back into
a string by pattern matching `@Username str ->` to unwrap the string from the `Username`.

Now that I've defined the `Username` opaque type, I can expose it so that other modules can use
it in type annotations. However, other modules can't use the `@Username` syntax to wrap or unwrap
`Username` values. That operation is only available in the same scope where `Username` itself was
defined; trying to use it outside that scope will give an error.

> If I were to define `Username := Str` inside another module (e.g. `Main`) and use `@Username`,
> it would compile, but that `Username` opaque type would not be considered equal to the one defined in
> the `Username` module. Although both opaque types have the name `Username`, they were defined in
> different modules. That means the two `Username` types would be type-incompatible with each other,
> and even attempting to use `==` to compare them would be a type mismatch.

## Modules and Shadowing

In Elm, my main module (where `main` lives) might begin like this:

```elm
module MyApp exposing (main)

import Parser
import Http exposing (Request)
import Task exposing (Task, await)
```

Roc application modules (where the equivalent of `main` lives) begin with the
`app` keyword rather than the `module` keyword, and the import syntax is a bit different.
Here's how the above module header imports section would look in Roc:

```elm
app imports [Parser, Http.{ Request }, Task.{ Task, await }]
```

`app` modules are application entry points, and they don't formally expose anything.
They also don't have names, so other modules can't even import them!

Modules that *can* be imported are `interface` modules. Their headers look like this:

```elm
interface Parser
    exposes [Parser, map, oneOf, parse]
    imports [Utf8]
```

The name `interface` is intended to draw attention to the fact that the interface
these expose is very important.

All imports and exports in Roc are enumerated explicitly; there is no `..` syntax.

> Since tags are available in all modules, Roc does not have a notion of
> "importing variants", and there's also no `exposing (Foo(..))` equivalent.
> (Later on, we'll talk about how opaque types work in Roc.)

Like Elm, Roc does not allow shadowing.

Elm does permit overriding open imports - e.g. if you have
`import Foo exposing (bar)`, or `import Foo exposing (..)`, you can still define
`bar = ...` in the module. Roc treats this as shadowing and does not allow it.

## Operators

In Elm, operators are functions. In Roc, all operators are syntax sugar.

This means, for example, that you cannot write `(/)` in Roc; that would be a syntax
error. However, the `/` operator in Roc is infix syntax sugar for `Num.div`,
which is a normal function you can pass to anything you like.

Elm has one unary operator, namely `-`. (In Elm, `-x` means
"apply unary `negate` to `x`.") Roc has that one too, and also unary `!`.
The expression `!foo` desugars to `Bool.not foo`, and `!foo bar` desugars
to `Bool.not (foo bar)`.

This was introduced because Roc does not expose any functions globally by default
(the way Elm does with `Basics` functions like `not`, `round`, etc.).
In Roc, only operators and standard types (like `Str` and `Bool`) are exposed globally.
Having to fully qualify `not` was annoying, and making an exception just for `not` seemed
less appealing than making an operator for it, especially when unary `!` is so widely used
in other languages.

Because Roc has unary `!`, its "not equal to" operator is `!=` instead of Elm's `/=`,
for symmetry with unary `!`.

There's an Operator Desugaring Table at the end of this guide, so you can see exactly
what each Roc operator desugars to.

## Currying and `|>`

Roc functions aren't curried. Calling `(List.append foo)` is a type mismatch
because `List.append` takes 2 arguments, not 1.

For this reason, function type annotations separate arguments with `,` instead of `->`.
In Roc, the type of `List.map` is:

```elm
List.map : List a, (a -> b) -> List b
```

You might notice that Roc's `List.map` takes its arguments in the reverse order
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

# 1000.0
```

In Elm, where `|>` inserts 2 as the last argument, 2 ends up being the *numerator*
rather than the denominator:

```elm
2000
  |> (/) 2.0

# 0.001
```

Another example is `List.append`, which is called `List.concat` in Roc:

```elixir
[1, 2]
  |> List.concat [3, 4]

# [1, 2, 3, 4]
```

In Elm:

```elm
[1, 2]
  |> List.append [3, 4]

# [3, 4, 1, 2]
```

> There are various trade-offs here, of course. Elm's `|>` has a [very elegant implementation](https://github.com/elm/core/blob/665624859a7a432107059411737e16d1d5cb6373/src/Basics.elm#L873-L874), and `(|>)` in Elm can be usefully passed to other
> functions (e.g. `fold`) whereas in Roc it's not even possible to express the type of `|>`.

As a consequence of `|>` working differently, "pipe-friendly" argument ordering is also
different. That's why `List.map` has a "flipped" signature in Roc; otherwise, `|> List.map Num.abs` wouldn't work on a list of numbers. Here's the type of Roc's `List.map` again, and also a pipeline using it:

```coffeescript
List.map : List a, (a -> b) -> List b

[-1, 2, 3, -4]
    |> List.map Num.abs
```

Roc has no `<<` or `>>` operators, and there are no functions in the standard library
for general-purpose pointfree function composition.

## The `<|` operator

Roc has no `<|` operator. (It does have `|>` though.)

In Elm, `<|` is used as a minor convenience for when you want to avoid some parens
in a single-line expression (e.g. `foo <| bar baz` over `foo (bar baz)`) and as
a major convenience when you want to pass an anonymous function, `if`, or `case` as an argument.

For example, `elm-test` relies on it:

```elm
test "it works" <|
    \_ -> verify stuff
```

In Roc, this does not require a `<|`. This Roc code does the same thing as the preceding Elm code:

```elm
test "it works"
    \_ -> verify stuff
```

This is convenient with higher-order functions which take a function as their
final argument. Since many Roc functions have the same type as Elm functions
except with their arguments flipped, this means it's possible to end a lot
of expressions with anonymous functions - e.g.

```elm
modifiedNums =
    List.map nums \num ->
        doubled = num * 2
        modified = modify doubled

        modified / 2
```

Separately, you don't need parens or an operator to pass `when` or `if` as
arguments. Here's another example:

```elixir
foo 1 2 if something then 3 else 4

# Same as `foo 1 2 (if something then 3 else 4)`
```

[CoffeeScript](http://coffeescript.org/) also does this the way Roc does.

## Backpassing

Suppose I'm using a platform for making a CLI, and I want to run several
`Task`s in a row which read some files from the disk. Here's one way I could do
that, assuming `Task.await` is like Elm's `Task.andThen` with arguments flipped:

```elm
readLicense : Filename -> Task License File.ReadErr
readLicense = \filename ->
    Task.await (File.readUtf8 settingsFilename) \settingsYaml ->
        when Yaml.decode settingsDecoder settingsYaml is
            Ok settings ->
                Task.await (File.readUtf8 settings.projectFilename) \csv ->
                    when Csv.decode projectDecoder csv is
                        Ok project ->
                            Task.await (File.readUf8 project.licenseFilename) \licenseStr ->
                                when License.fromStr licenseStr is
                                    Ok license -> Task.succeed license
                                    Err err -> Task.fail (InvalidFormat err)

                        Err err -> Task.fail (InvalidFormat err)

            Err err ->
                Task.fail (InvalidFormat err)
```

We can write this with `|>` instead of `when` like so:

```elm
readLicense : Filename -> Task License File.ReadErr
readLicense = \filename ->
    Task.await (File.readUtf8 settingsFilename) \settingsYaml ->
        settingsYaml
            |> Yaml.decode settingsDecoder
            |> Task.fromResult
            |> Task.mapFail InvalidFormat
            |> Task.await \settings ->
                Task.await (File.readUtf8 settings.projectFilename) \projectCsv ->
                    projectCsv
                        |> Csv.decode projectDecoder
                        |> Task.fromResult
                        |> Task.mapFail InvalidFormat
                        |> Task.await \project ->
                            Task.await (File.readUf8 project.licenseFilename) \licenseStr ->
                                License.fromStr licenseStr
                                    |> Task.fromResult
                                    |> Task.mapFail InvalidFormat
```

We can also write it this way, which is equivalent to the previous two ways:

```elm
readLicense : Filename -> Task License File.ReadErr
readLicense = \filename ->
    settingsYaml <- Task.await (File.readUtf8 settingsFilename)

    settings <-
        settingsYaml
            |> Yaml.decode settingsDecoder
            |> Task.fromResult
            |> Task.mapFail InvalidFormat

    projectCsv <- Task.await (File.readUtf8 settings.projectFilename)

    project <-
        projectCsv
            |> Csv.decode projectDecoder
            |> Task.fromResult
            |> Task.mapFail InvalidFormat

    licenseStr <-
        Task.await (File.readUf8 project.licenseFilename)

    License.fromStr licenseStr
        |> Task.fromResult
        |> Task.mapFail InvalidFormat
```

This uses *backpassing* syntax to nest anonymous functions without indenting them.
Here's a smaller demonstration of backpassing; the second snippet is sugar for the first.

```elm
list =
    List.map numbers \num -> num + 1
```

```elm
list =
    num <- List.map numbers

    num + 1
```

Both snippets are calling `List.map` passing `numbers` as the first argument,
and a `\num -> num + 1` function for the other argument.

The difference is that in the second snippet, the `\num -> num + 1` function is
written backwards, like this:

```elm
    num <-

    num + 1
```

This is called *backpassing* because you write the function *backwards* and then
immediately *pass* it as an argument to another function.

The other function - the one you're passing this one to - goes right after
the `<-` symbol. That function should be called with one argument missing at
the end, such as with `List.map numbers` (which is missing its final argument).

Here's another pair of snippets, this time using two backpassing calls:

```elm
incrementedNumbers =
    List.map lists \numbers ->
        List.map numbers \num -> num + 1
```

```elm
incrementedNumbers =
    numbers <- List.map lists
    num <- List.map numbers

    num + 1
```

In the second snippet, we have two functions defined in the backpassing style. The first function is:

```elm
numbers <-
    num <- List.map numbers

    num + 1
```

This function desugars to `\numbers -> …` and is being passed as the final argument
to `List.map lists`.

The second function defined in backpassing style is:

```elm
    num <-

    num + 1
```

This function desugars to `\numbers -> …` and is being passed as the final argument
to `List.map numbers`. That `List.map numbers` call is the body of
the `numbers <-` function we defined in backpassing style a moment ago - so
in a normal function definition, it would be `\numbers -> List.map numbers …`

Note that backpassing can be combined with the `|>` operator, which lets you
call a function with two arguments missing from the end - one provided by
the `|>` and the other provided by the `<-`, like so:

```elm
incrementedNumbers =
    num <-
        [1, 2, 3]
            |> List.reverse
            |> List.map

    num + 1
```

Here, the first argument to `List.map` is provided by the `|>`
(namely the reversed `[1, 2, 3]` list), and the second argument is provided by +the `<-` (namely the `\num -> …` function).

Backpassing can also be used with functions that take multiple arguments; for
example, you could write `key, value <- Dict.map dictionary` similarly to how
we used `List.map` here. That would desugar into a
`Dict.map dictionary \key, value -> …` function.

> To be clear, backpassing is designed to be used with chaining functions
> like `Task.await` which are prone to lots of nesting. It isn't designed to be
> used with functions like `List.map`; this is just a simplified example to show
> that `<-` can be used with any function...even those where it doesn't improve
> code clarity!

Finally, here's an example combining backpassing with ordinary `=` definitions:

```elm
task =
    user <- Task.await fetchUser

    url = user.baseUrl

    settings, bio, posts <- Task.map3 (getSettings url) (getBio url) (getPosts url)

    profile = makeProfile settings bio

    Task.succeed { profile, posts }
```

Here, every new name that's introduced to scope is aligned on the left-hand edge
of the expression - regardless of whether it's coming from `=` or from `<-`.

## Numbers

Like Elm, Roc organizes numbers into integers and floating-point numbers.
However, Roc breaks them down even further. For example, Roc has two different
sizes of float types to choose from:

- `F64` - a 64-bit [IEEE 754 binary floating point number](https://en.wikipedia.org/wiki/IEEE_754#Binary)
- `F32` - a 32-bit [IEEE 754 binary floating point number](https://en.wikipedia.org/wiki/IEEE_754#Binary)

Both types are desirable in different situations. For example, when doing
simulations, the precision of the `F64` type is desirable. On the other hand,
GPUs tend to heavily prefer 32-bit floats because a serious bottleneck is how
long it takes data to transfer from CPU to GPU, so having to send half as many
bytes per render (compared to 64-bit floats) can be huge for performance.

Roc also supports `Dec`, which is a 128-bit [fixed-point decimal](https://en.wikipedia.org/wiki/Fixed-point_arithmetic) number. `Dec` is decimal-based, so `0.1 + 0.2 == 0.3`
(whereas in binary floats [this is not true](https://0.30000000000000004.com/)),
which makes it much better for calculations involving currency, among other use cases.
The downside of `Dec` is that it does not have hardware support, so calculations involving
them take longer than they do with floats.

Similarly to how there are different sizes of floating point numbers,
there are also different sizes of integer to choose from:

- `I8`
- `I16`
- `I32`
- `I64`
- `I128`

Roc also has *unsigned* integers which are never negative. They are
`U8`, `U16`, `U32`, `U64`, `U128`, and `Nat`.

The size of `Nat` depends on what target you're building for; on a 64-bit target
(the most common), at runtime `Nat` will be the same as `U64`, whereas on a 32-bit
target (for example, WebAssembly) at runtime it will be the same as `U32` instead.
`Nat` comes up most often with collection lengths and indexing into collections.
For example:

- `List.len : List * -> Nat`
- `List.get : List elem, Nat -> Result elem [OutOfBounds]*`
- `List.set : List elem, Nat, elem -> List elem`

As with floats, which integer type to use depends on the values you want to support
as well as your performance needs. For example, raw sequences of bytes are typically
represented in Roc as `List U8`. You could also represent them as `List U128`,
but it's much more efficient to use `List U8`, since each byte will be at most 255 anyway.

Like Elm, it's possible in Roc to have functions that work on either integers
or floating-point numbers. However, the types are different. For example,
the type of `Num.add` (which the `+` operator desugars to) is:

```elm
Num.add : Num a, Num a -> Num a
```

This accepts any of the numeric types discussed above, from `I128` to `F32`
to `D64` and everything in between. This is because those are all type aliases
for `Num` types. For example:

- `I64` is a type alias for `Num (Integer Signed64)`
- `U8` is a type alias for `Num (Integer Unsigned8)`
- `F32` is a type alias for `Num (Fraction Binary32)`
- `Dec` is a type alias for `Num (Fraction Decimal)`

(Those types like `Integer`, `Fraction`, and `Signed64` are all defined like `Never`;
you can never instantiate one. They are used only as phantom types.)

So Roc does not use `number`, but rather uses `Num` - which works more like `List`.
Either way, you get `+` being able to work on both integers and floats!

Separately, there's also `Int a`, which is a type alias for `Num (Integer a)`,
and `Frac a`, which is a type alias for `Num (Fraction a)`. These allow functions
that can work on any integer or any fractional number. For example,
`Num.bitwiseAnd : Int a, Int a -> Int a`.

In Roc, number literals with decimal points are `Frac *` values.
Number literals *without* a decimal point are `Num *` values. Almost always these
will end up becoming something more specific, but in the unlikely event
(most often in a REPL) that you actually do end up with an operation that runs
on either an `Int *` or a `Num *` value, it will default to being treated as
an `I64`. Similarly, a `Frac *` value will default to being treated as a `D64`,
which means if someone is learning Roc as their first programming language and
they type `0.1 + 0.2` into a REPL, they won't be confused by the answer.

If you encounter integer or `Dec` overflow in Roc, by default you get a runtime
exception. You can opt into wrapping integer overflow instead with functions like
`Num.addWrap : Int a, Int a -> Int a`, or use a function that gives `Err` if it
overflows, like `Num.addChecked : Num a, Num a -> Result (Num a) [Overflow]*`.

Also [like Python](https://www.python.org/dev/peps/pep-0515/)
Roc permits underscores in number literals for readability purposes. Roc also supports
hexadecimal (`0x01`), octal (`0o01`), and binary (`0b01`) integer literals; these
literals all have type `Int *` instead of `Num *`.

If you put these into a hypothetical Roc REPL, here's what you'd see:

```elm
> 1_024 + 1_024
2048 : Num *

> 1 + 2.14
3.14 : Frac *

> 1.0 + 1
2.0 : Frac *

> 1.1 + 0x11
<type mismatch between `1.1 : Frac *` and `0x11 : Int *`>

> 11 + 0x11
28 : Int *
```

## Abilities

`comparable`, `appendable`, and `number` don't exist in Roc.

- `number` is replaced by `Num`, as described previously.
- `appendable` is only used in Elm for the `(++)` operator, and Roc doesn't have that operator.
- `comparable` is used in Elm for comparison operators (like `<` and such), plus `List.sort`, `Dict`, and `Set`. Roc's comparison operators (like `<`) only accept numbers; `"foo" < "bar"` is valid Elm, but will not compile in Roc. Roc's dictionaries and sets are hashmaps behind the scenes (rather than ordered trees), so their keys need to be hashable but not necessarily comparable.

That said, Roc's `Dict` and `Set` do have a restriction on their keys, just not `comparable`.
See the section on Abilities in [the tutorial](https://roc-lang.org/tutorial) for details.

## Standard library

`elm/core` has these modules:

- `Array`
- `Basics`
- `Bitwise`
- `Char`
- `Debug`
- `Dict`
- `List`
- `Maybe`
- `Platform`
- `Platform.Cmd`
- `Platform.Sub`
- `Process`
- `Result`
- `Set`
- `String`
- `Task`
- `Tuple`

In Roc, the standard library is not a standalone package. It is baked into the compiler,
and you can't upgrade it independently of a compiler release; whatever version of
Roc you're using, that's the version of the standard library you're using too.
(This is because Roc doesn't have a concept like Elm's `Kernel`; it would not be
possible to ship Roc's standard library as a separate package!)

Roc's standard library has these modules:

- `Str`
- `Bool`
- `Num`
- `List`
- `Dict`
- `Set`
- `Result`

Some differences to note:

- All these standard modules are imported by default into every module. They also expose all their types (e.g. `Bool`, `List`, `Result`) but they do not expose any values - not even `negate` or `not`. (`Ok` and `Err` are ordinary tags, so they do not need to be exposed; they are globally available regardless!)
- In Roc it's called `Str` instead of `String`.
- `List` refers to something more like Elm's `Array`, as noted earlier.
- No `Char`. This is by design. What most people think of as a "character" is a rendered glyph. However, rendered glyphs are comprised of [grapheme clusters](https://stackoverflow.com/a/27331885), which are a variable number of Unicode code points - and there's no upper bound on how many code points there can be in a single cluster. In a world of emoji, I think this makes `Char` error-prone and it's better to have `Str` be the only first-class unit. For convenience when working with unicode code points (e.g. for performance-critical tasks like parsing), the single-quote syntax is sugar for the corresponding `U32` code point - for example, writing `'鹏'` is exactly the same as writing `40527`. Like Rust, you get a compiler error if you put something in single quotes that's not a valid [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value).
- No `Basics`. You use everything from the standard library fully-qualified; e.g. `Bool.not` or `Num.negate` or `Num.ceiling`. There is no `Never` because `[]` already serves that purpose. (Roc's standard library doesn't include an equivalent of `Basics.never`, but it's one line of code and anyone can implement it: `never = \a -> never a`.)
- No `Tuple`. Roc doesn't have tuple syntax. As a convention, `Pair` can be used to represent tuples (e.g. `List.zip : List a, List b -> List [Pair a b]*`), but this comes up infrequently compared to languages that have dedicated syntax for it.
- No `Task`. By design, platform authors implement `Task` (or don't; it's up to them) - it's not something that really *could* be usefully present in Roc's standard library.
- No `Process`, `Platform`, `Cmd`, or `Sub` - similarly to `Task`, these are things platform authors would include, or not.
- No `Maybe`. This is by design. If a function returns a potential error, use `Result` with an error type that uses a zero-arg tag to describe what went wrong. (For example, `List.first : List a -> Result a [ListWasEmpty]*` instead of `List.first : List a -> Maybe a`.) If you want to have a record field be optional, use an Optional Record Field directly (see earlier). If you want to describe something that's neither an operation that can fail nor an optional field, use a more descriptive tag - e.g. for a nullable JSON decoder, instead of `nullable : Decoder a -> Decoder (Maybe a)`, make a self-documenting API like `nullable : Decoder a -> Decoder [Null, NonNull a]*`.

## Operator Desugaring Table

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
