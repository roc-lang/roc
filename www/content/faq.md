# FAQ

## [Where did the name Roc come from?](#name-origin) {#name-origin}

The Roc programming language is named after [a mythical bird](<https://en.wikipedia.org/wiki/Roc_(mythology)>).

<svg aria-labelledby="logo-svg-title logo-svg-desc" width="240" height="240" viewBox="0 0 51 53" fill="none" xmlns="http://www.w3.org/2000/svg"><title id="logo-svg-title">The Roc logo</title><desc id="logo-svg-desc">A purple origami bird made of six triangles</desc><path d="M23.6751 22.7086L17.655 53L27.4527 45.2132L26.4673 39.3424L23.6751 22.7086Z" class="logo-dark"></path><path d="M37.2438 19.0101L44.0315 26.3689L45 22L45.9665 16.6324L37.2438 19.0101Z" class="logo-light"></path><path d="M23.8834 3.21052L0 0L23.6751 22.7086L23.8834 3.21052Z" class="logo-light"></path><path d="M44.0315 26.3689L23.6751 22.7086L26.4673 39.3424L44.0315 26.3689Z" class="logo-light"></path><path d="M50.5 22L45.9665 16.6324L45 22H50.5Z" class="logo-dark"></path><path d="M23.6751 22.7086L44.0315 26.3689L37.2438 19.0101L23.8834 3.21052L23.6751 22.7086Z" class="logo-dark"></path>
</svg>

That's why the logo is a bird. It’s specifically an [_origami_ bird](https://youtu.be/9gni1t1k1uY) as an homage
to [Elm](https://elm-lang.org/)’s tangram logo.

Roc is a direct descendant of Elm. The languages are similar, but not the same.
[Origami](https://en.wikipedia.org/wiki/Origami) likewise has similarities to [tangrams](https://en.wikipedia.org/wiki/Tangram), although they are not the same.
Both involve making a surprising variety of things
from simple primitives. [_Folds_](<https://en.wikipedia.org/wiki/Fold_(higher-order_function)>)
are also common in functional programming.

The logo was made by tracing triangles onto a photo of a physical origami bird.
It’s made of triangles because triangles are a foundational primitive in
computer graphics.

The name was chosen because it makes for a three-letter file extension, it means something
fantastical, and it has incredible potential for puns. Here are some different ways to spell it:

- **Roc** - traditional
- **roc** - low-key
- **ROC** - [YELLING](https://package.elm-lang.org/packages/elm/core/latest/String#toUpper)
- **Röc** - [metal 🤘](https://en.wikipedia.org/wiki/Metal_umlaut)

Fun fact: "roc" translates to 鹏 in Chinese, [which means](https://www.mdbg.net/chinese/dictionary?page=worddict&wdrst=0&wdqb=%E9%B9%8F) "a large fabulous bird."

## [Why can't functions be compared for equality using the `==` operator?](#function-equality) {#function-equality}

Function equality has been proven to be undecidable in the general case because of the [halting problem](https://en.wikipedia.org/wiki/Halting_problem).
So while we as humans might be able to look at `\x -> x + 1` and `\x -> 1 + x` and know that they're equivalent,
in the general case it's not possible for a computer to do this reliably.

There are some other potential ways to define function equality, but they all have problems.

One way would be to have two functions be considered equal if their source code is equivalent. (Perhaps disregarding
comments and spaces.) This sounds reasonable, but it means that now revising a function to do
exactly the same thing as before (say, changing `\x -> x + 1` to `\x -> 1 + x`) can cause a bug in a
distant part of the code base. Defining function equality this way means that revising a function's internals
is no longer a safe, local operation - even if it gives all the same outputs for all the same inputs.

Another option would be to define it using "reference equality." This is what JavaScript does, for example.
However, Roc does not use reference equality anywhere else in the language, and it would mean that (for example)
passing `\x -> x + 1` to a function compared to defining `fn = \x -> x + 1` elsewhere and then passing `fn` into
the function might give different answers.

Both of these would make revising code riskier across the entire language, which is very undesirable.

Another option would be to define that function equality always returns `false`. So both of these would evaluate
to `false`:

```roc
(\x -> x + 1) == (\x -> 1 + x) #false
(\x -> x + 1) == (\x -> x + 1) #false
```

This makes function equality effectively useless, while still technically allowing it. It has some other downsides:

- Now if you put a function inside a record, using `==` on that record will still type-check, but it will then return `false`. This could lead to bugs if you didn't realize you had accidentally put a function in there - for example, because you were actually storing a different type (e.g. an opaque type) and didn't realize it had a function inside it.
- If you put a function (or a value containing a function) into a `Dict` or `Set`, you'll never be able to get it out again. This is a common problem with [NaN](https://en.wikipedia.org/wiki/NaN), which is also defined not to be equal to itself.

The first of these problems could be addressed by having function equality always return true instead of false (since that way it would not affect other fields' equality checks in a record), but that design has its own problems:

- Although function equality is still useless, `(\x -> x + 1) == (\x -> x)` returns `Bool.true`. Even if it didn't lead to bugs in practice, this would certainly be surprising and confusing to beginners.
- Now if you put several different functions into a `Dict` or `Set`, only one of them will be kept; the others will be discarded or overwritten. This could cause bugs if a value stored a function internally, and then other functions relied on that internal function for correctness.

Each of these designs makes Roc a language that's some combination of more error-prone, more confusing, and more
brittle to change. Disallowing function equality at compile time eliminates all of these drawbacks.

Note that you can provide a custom implementation of the `Eq` ability for an opaque type that contains a function,
in any way you like (including ignoring the function for equality).

## [Why is there no way to specify "import everything this module exposes" in `imports`?](#import-everything) {#import-everything}

In [Elm](https://elm-lang.org), it's possible to import a module in a way that brings everything that module
exposes into scope. It can be convenient, but like all programming language features, it has downsides.

A minor reason Roc doesn't have this feature is that exposing everything can make it more difficult
outside the editor (e.g. on a website) to tell where something comes from, especially if multiple imports are
using this. ("I don't see `blah` defined in this module, so it must be coming from an import...but which of
these several import-exposing-everything modules could it be? I'll have to check all of them, or
download this code base and open it up in the editor so I can jump to definition!")

The main reason for this design, though, is compiler performance.

Currently, the name resolution step in compilation can be parallelized across modules, because it's possible to
tell if there's a naming error within a module using only the contents of that module. If "expose everything" is
allowed, then it's no longer clear whether anything is a naming error or not, until all the "expose everything"
modules have been processed, so we know exactly which names they expose. Because that feature doesn't exist in Roc,
all modules can do name resolution in parallel.

Of note, allowing this feature would only slow down modules that used it; modules that didn't use it would still be
parallelizable. However, when people find out ways to speed up their builds (in any language), advice starts to
circulate about how to unlock those speed boosts. If Roc had this feature, it's predictable that a commonly-accepted
piece of advice would eventually circulate: "don't use this feature because it slows down your builds."

If a feature exists in a language, but the common recommendation is never to use it, that's cause for reconsidering
whether the feature should be in the language at all. In the case of this feature, it's simpler if the
language doesn't have it; that way nobody has to learn (or spend time spreading the word) about the
performance-boosting advice not to use it.

## [Why doesn't Roc have a `Maybe` or `Option` or `Optional` type, or `null` or `nil` or `undefined`?](#option-type) {#option-type}

It's common for programming languages to have a [null reference](https://en.wikipedia.org/wiki/Null_pointer)
(e.g. `null` in C, `nil` in Ruby, `None` in Python, or `undefined` in JavaScript).
The inventor of the null reference refers to it as his "[billion dollar mistake](https://en.wikipedia.org/wiki/Null_pointer#History)" because it "has led to innumerable errors, vulnerabilities, and system crashes, which have probably caused a billion dollars of pain and damage in the last forty years."

For this and other reasons, many languages do not include a null reference, but instead have a standard library
data type which can be used in situations where a null reference would otherwise be used. Common names for this
null reference alternative type include `Maybe` (like in Haskell or Elm), `Option` (like in OCaml or Rust),
and `Optional` (like in Java).

By design, Roc does not have one of these. There are several reasons for this.

First, if a function returns a potential error, Roc has the convention to use `Result` with an error type that
has a single tag describing what went wrong. (For example, `List.first : List a -> Result a [ListWasEmpty]`
instead of `List.first : List a -> Maybe a`.) This is not only more self-descriptive, it also composes better with
other operations that can fail; there's no need to have functions like `Result.toMaybe` or `Maybe.toResult`,
because in Roc, the convention is that operations that can fail always use `Result`.

Second, optional record fields can be handled using Roc's Default Value Record Field language feature, so using a type like `Maybe` there would be less ergonomic.

To describe something that's neither an optional field nor an operation that can fail, an explicit tag union can be
more descriptive than something like `Maybe`. For example, if a record type has an `artist` field, but the artist
information may not be available, compare these three alternative ways to represent that:

- `artist : Maybe Artist`
- `artist : [Loading, Loaded Artist]`
- `artist : [Unspecified, Specified Artist]`

All three versions tell us that we might not have access to an `Artist`. However, the `Maybe` version doesn't
tell us why that might be. The `Loading`/`Loaded` version tells us we don't have one _yet_, because we're
still loading it, whereas the `Unspecified`/`Specified` version tells us we don't have one and shouldn't expect
to have one later if we wait, because it wasn't specified.

Naming aside, using explicit tag unions also makes it easier to transition to richer data models. For example,
after using `[Loading, Loaded Artist]` for awhile, we might realize that there's another possible state: loading
failed due to an error. If we modify this to be `[Loading, Loaded Artist, Errored LoadingErr]`, all
of our code for the `Loading` and `Loaded` states will still work.

In contrast, if we'd had `Maybe Artist` and were using helper functions like `Maybe.isNone` (a common argument
for using `Maybe` even when it's less self-descriptive), we'd have to rewrite all the code which used those
helper functions. As such, a subtle downside of these helper functions is that they discourage any change to
the data model that would break their call sites, even if that change would improve the data model overall.

On a historical note, `Maybe` may have been thought of as a substitute for null references—as opposed to something that emerged organically based on specific motivating use cases after `Result` already existed. That said, in languages that do not have an equivalent of Roc's tag unions, it's much less ergonomic to write something like `Result a [ListWasEmpty]`, so that design would not fit those languages as well as it fits Roc.

## [Why doesn't Roc have a builtin "arbitrary-sized" number type like BigNum or BigDecimal?](#arbitrary-numbers) {#arbitrary-numbers}

Like all programming languages, Roc is subject to the limitations of the universe. Almost all numbers in mathematics cannot even be represented in the universe because you'd run out of matter trying to write them down, and therefore Roc must choose what subset of mathematics to support—including which numbers to support.

Roc supports 128-bit integers and 128-bit fixed-point decimal numbers. Here are some approximate ranges for those:

-   `I128`: ±170000000000000000000000000000000000000
-   `Dec`: ±170000000000000000000.000000000000000000

For heap-allocated numbers to be worth including in Roc's builtins, a sufficient number of real-world use cases would need to exist where the above ranges are too small, but a heap-allocated number would be big enough, _and_ the performance hit from the heap-allocated numbers would be acceptable to those use cases, _and_ a user-created implementation of heap-allocated numbers would not be acceptable, _and_ using 64-bit floating-point numbers (which can be even larger than these, at the cost of precision loss in some operations) would not be acceptable either.

So far, this has not happened.

## [Why doesn't Roc have higher-kinded polymorphism or arbitrary-rank types?](#arbitrary-rank-types) {#arbitrary-rank-types}

_Since this is a FAQ answer, it assumes familiarity with higher-kinded types and higher-rank types instead of including a primer on them._

A valuable aspect of Roc's type system is that it has decidable [principal](https://en.wikipedia.org/wiki/Principal_type)
type inference. This means that:

- At compile time, Roc can correctly infer the types for every expression in a program, even if you don't annotate any of the types.
- This inference always infers the most general type possible; you couldn't possibly add a valid type annotation that would make the type more flexible than the one that Roc would infer if you deleted the annotation.

It's been proven that any type system which supports either [higher-kinded polymorphism](https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf) or [arbitrary-rank types](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf) cannot have decidable
principal type inference. With either of those features in the language, there will be situations where the compiler
would be unable to infer a type—and you'd have to write a type annotation. This also means there would be
situations where the editor would not be able to reliably tell you the type of part of your program, unlike today
where it can accurately tell you the type of anything, even if you have no type annotations in your entire code base.

This is one factor that higher-rank and higher-kinded types have in common. There are other factors which are specific
to each.

### [Higher-rank types](#higher-rank-types) {#higher-rank-types}

Supporting higher-rank types in Roc has been discussed before, but it has several important downsides:

- It would increase the complexity of the language.
- It would make some compiler error messages more confusing (e.g. they might mention `forall` because that was the most general type that could be inferred, even if that wasn't helpful or related to the actual problem).
- It would substantially increase the complexity of the type checker, which would necessarily slow it down.
- It would make some Roc programs run significantly more slowly. Roc compiles programs by [monomorphizing](https://en.wikipedia.org/wiki/Monomorphization), and it's unclear how we could fully monomorphize programs containing Rank-2 types. This means compiling programs which include Rank-2 types (or higher) would require sacrificing monomorphization, which would substantially degrade runtime performance.

As such, the plan is for Roc to stick with Rank-1 types indefinitely.

### [Higher-kinded polymorphism](#higher-kinded-polymorphism) {#higher-kinded-polymorphism}

The explicit plan is that Roc will never support higher-kinded polymorphism.

On the technical side, the reasons for this are ordinary: like any language feature, HKP has both benefits and drawbacks,
and in the context of Roc, the drawbacks seem to outweigh the benefits. (Those who come to a different conclusion may
think HKP's drawbacks would be less of a big a deal in Roc. That's reasonable; we programmers often weigh the same
trade-offs differently.) To be clear, this analysis of HKP is in the specific context of Roc; there are plenty of
other languages where HKP seems like a great fit. For example, it's hard to imagine Haskell without it. Similarly,
lifetime annotations might be a natural fit for Rust, but they wouldn't be a good fit for Roc either.

It's also important to consider the cultural implications of deciding whether or not to support HKP.
To illustrate these implications, imagine this conversation:

**Programmer 1:** "How do you feel about higher-kinded polymorphism?"

**Programmer 2:** "I have no idea what that is."

**Programmer 1:** "Okay, how do you feel about monads?"

**Programmer 2:** "OH NO."

For some, this conversation does not require imagining, because it's so familiar: higher-kinded types come up in
conversation, another programmer asks what that means, monads are given as an example, and their reaction is
strongly negative. On the flip side, plenty of programmers love HKP and vigorously advocate for its addition
to languages they use which don't have it. Feelings about HKP seem strongly divided, maybe more so
than any other type system feature besides static and dynamic types.

It's impossible for a programming language to be neutral on this. If the language doesn't support HKP, nobody can
implement a Monad typeclass (or equivalent) in any way that can be expected to catch on. Advocacy to add HKP to the
language will inevitably follow. If the language does support HKP, one or more alternate standard libraries built
around monads will inevitably follow, along with corresponding cultural changes. (See Scala for example.)
Culturally, to support HKP is to take a side, and to decline to support it is also to take a side.

Given this, languages have three options:

- Have HKP and have Monad in the standard library. Embrace them and build a culture and ecosystem around them.
- Have HKP and don't have Monad in the standard library. An alternate standard library built around monads will inevitably emerge, and both the community and ecosystem will divide themselves along pro-monad and anti-monad lines.
- Don't have HKP; build a culture and ecosystem around other things.

Considering that these are the only three options, an early decision in Roc's design—not only on a technical
level, but on a cultural level as well—was to make it clear that the plan is for Roc never to support HKP.
The hope is that this clarity can save a lot of community members' time that would otherwise be spent on advocacy or
arguing between the two sides of the divide. Again, it's completely reasonable for anyone to have a different preference,
but given that languages can only choose one of these options, it seems clear that the right choice for Roc
is for it to never have higher-kinded polymorphism.

## [Why aren't Roc functions curried by default?](#curried-functions) {#curried-functions}

Although technically any language with first-class functions makes it possible to curry
any function (e.g. anyone can manually curry a Roc function `\x, y, z ->` by writing `\x -> \y -> \z ->` instead),
typically what people mean when they say Roc isn't a curried language is that Roc functions aren't curried
by default. The rest of this section will use "currying" as a shorthand for "functions that are curried
by default" for the sake of brevity.

Currying makes function calls more concise in some cases, but it has several significant downsides:

- It lowers error message quality, because there can no longer be an error for "function called with too few arguments." (Calling a function with fewer arguments is always valid in curried functions; the error you get instead will unavoidably be some other sort of type mismatch, and it will be up to you to figure out that the real problem was that you forgot an argument.)
- It makes the `|>` operator more error-prone in some cases.
- It makes higher-order function calls need more parentheses in some cases.
- It significantly increases the language's learning curve. (More on this later.)
- It facilitates pointfree function composition. (More on why this is listed as a downside later.)

There's also a downside that it would make runtime performance of compiled programs worse by default,
but it would most likely be possible to optimize that away at the cost of slightly longer compile times.

These downsides seem to outweigh the one upside (conciseness in some places). Here are some more details about each of
the downsides.

### [Currying and the `|>` operator](#curried-pipes) {#curried-pipes}

In Roc, both of these expressions evaluate to `"Hello, World!"`

```roc
Str.concat "Hello, " "World!"
```

```roc
"Hello, "
|> Str.concat "World!"
```

It's unsurprising to most beginners that these work the same way; it's common for a beginner who has recently learned
how `|>` works to assume that `|> Str.concat "!"` would concatenate `!` onto the end of a string.

This is not how it works in curried languages, however. In curried languages with a `|>` operator, the first expression
still returns `"Hello, World!"` but the second one returns `"World!Hello, "` instead. This can be an unpleasant surprise
for beginners, but even experienced users commonly find that this behavior is less useful than having both of
these expressions evaluate to the same thing.

In Roc, both expressions evaluate to the same thing because Roc's `|>` operator uses the expression before the `|>` as the _first_ argument, whereas in curried languages, `|>` uses it as the _last_ argument. For example, this is how `|>` works in both [F#](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/#function-symbols-and-operators) and in [Elm](https://package.elm-lang.org/packages/elm/core/1.0.5/Basics#|%3E), both of which are curried languages. In contrast, Roc's `|>` design uses the same argument ordering as [Elixir](https://hexdocs.pm/elixir/1.14.0/Kernel.html#%7C%3E/2) and [Gleam](https://gleam.run/book/tour/functions.html#pipe-operator), none of which are curried languages.

This comes up in other situations besides string concatenation. For example, consider subtraction and division:

```roc
someNumber
|> Num.div 2
```

```roc
someNumber
|> Num.sub 1
```

Again, it's reasonable to expect that `|> Num.div 2` will divide a number by 2, and that
`|> Num.sub 1` will subtract 1 from a number. In Roc, this is how they work, but in
curried languages they work the opposite way: `|> Num.div 2` takes the number 2 and
divides it by a number, and `|> Num.sub 1` takes the number 1 and subtracts a number
from it. This is once again both more surprising to beginners and less useful to
experienced users.

The way `|>` works in Roc has a second benefit when it comes to higher-order functions. Consider these two examples:

```roc
answer = List.map numbers \num ->
    someFunction
        "some argument"
        num
        anotherArg
```

```roc
numbers
|> List.map Num.abs
```

In Roc, `List.map` takes a list and then a function. Because of the way `|>` works in Roc, `numbers |> List.map Num.abs` passes `numbers` as the first argument to `List.map`, and `Num.abs` as the second argument. So both of these examples work fine.

In a curried language, these two examples couldn't both be valid. In order for `|> List.map Num.abs` to work in a curried language (where `|>` works the other way), `List.map` would have to take its arguments in the opposite order: the function first and the list second.

This means the first example would have to change from this...

```roc
answer = List.map numbers \num ->
    someFunction
        "some argument"
        num
        anotherArg
```

...to this:

```roc
answer =
    List.map
        (\num ->
            someFunction
                "some argument"
                num
                anotherArg
        )
        numbers
```

The Roc version of this is nicer in that it doesn't require parentheses around the function argument. A curried language
could theoretically adopt Roc's style of `|>` (where it pipes in the first argument instead of the last argument), but
to get this second benefit, the language would also need to have `List.map` take the function as its second argument
instead of the first. However, this would work against currying's one upside; it would no longer work to write
`(List.map negate)` if the `List.map` arguments were flipped, the way they are in Roc. So currying and `|>` are unavoidably
in tension.

As a historical note, these stylistic benefits (of `|> Num.sub 1` working as expected, and being able to write `List.map numbers \num ->`) were not among the original reasons Roc did not have currying. These benefits were discovered after the decision had already been made that Roc would not be a curried language, and they served to reinforce after the fact that the decision was the right one for Roc given the language's goals.

### [Currying and learning curve](#curried-learning-curve) {#curried-learning-curve}

Currying leads to function signatures that look surprising to beginners. For example, in Roc, the
[`Bool.and`](https://www.roc-lang.org/builtins/Bool#and) function has the type `Bool, Bool -> Bool`. If Roc were a
curried language, this function would instead have the type `Bool -> Bool -> Bool`. Since no mainstream programming
languages today are curried, anyone who knows a mainstream language and is learning their first curried language will
require additional explanation about why function types look this way.

This explanation is nontrivial. It requires explaining partial application, how curried functions facilitate partial
application, how function signatures accurately reflect that they're curried, and going through examples for all of these.
All of it builds up to the punchline that "technically, all functions in this language have a single argument," which
some percentage of learners find interesting, and some percentage still find confusing even after all that explanation.

It's common for beginners to report that currying only "clicked" for them after spending significant time writing code
in a curried language. This is not the end of the world, especially because it's easy enough to think "I still don't
totally get this even after that explanation, but I can remember that function arguments are separated by `->` in this
language and maybe I'll understand the rest later." Clearly currying doesn't preclude a language from being easy to learn,
because Elm has currying, and Elm's learning curve is famously gentle.

That said, beginners who feel confused while learning the language are less likely to continue with it.
And however easy Roc would be to learn if it had currying, the language is certainly easier to learn without it.

### [Pointfree function composition](#pointfree-composition) {#pointfree-composition}

[Pointfree function composition](https://en.wikipedia.org/wiki/Tacit_programming) is where you define
a new function by composing together two existing functions without naming intermediate arguments.
Here's an example:

```roc
reverseSort : List elem -> List elem
reverseSort = compose List.reverse List.sort

compose : (a -> b), (c -> a) -> (c -> b)
compose = \f, g, x -> f (g x)
```

Here's a way to write it without pointfree function composition:

```roc
reverseSort : List elem -> List elem
reverseSort = \list -> List.reverse (List.sort list)
```

It's common for programmers to build a mental model of what `compose List.reverse List.sort` does by mentally
translating it into `\list -> List.reverse (List.sort list)`. This extra mental translation step makes it take
longer to read and to understand despite being technically more concise. In more complex examples (this
is among the tamest of pointfree function composition examples), the chances increase of making a mistake in
the mental translation step, leading to a misundesrtanding of what the function is doing—which can cause bugs.

Some languages place such a high value on conciseness that they would consider the conciceness upside to outweigh
these downsides, but Roc is not one of those languages. It's considered stylistically better in Roc to write the
second version above. Given this, since currying facilitates pointfree function composition, making Roc a curried
language would have the downside of facilitating an antipattern in the language.

Stacking up all these downsides of currying against the one upside of making certain function calls more concise,
it seems clear that Roc should not be a curried language.

## [Will Roc ever have linear types, dependent types, refinement types, or uniqueness types?](#advanced-types) {#advanced-types}

The plan is for Roc to never have linear types, dependent types, refinement types, or uniqueness types.

Fast compile times are a primary goal for Roc, and a major downside of refinement types is an exponential increase in compile times. This rules out refinement types for Roc.

If Roc were to have linear types or uniqueness types, they would move things that are currently behind-the-scenes performance optimizations into the type system. For them to be effective across the ecosystem, they couldn't really be opt-in; everyone would have to use them, even those for whom the current system of behind-the-scenes optimizations already met their performance needs without any added type system complexity. Since the overwhelming majority of use cases are expected to fall into that latter group, adding linear types or uniqueness types to Roc would be a net negative for the ecosystem.

Dependent types are too risky of a bet for Roc to take. They have been implemented in programming languages for three decades, and for at least half that time period, it has been easy to find predictions that dependent types will be the future of type systems. Much harder to find are success stories of complex applications built with dependent types, which realized benefits that significantly outweighed the substantial complexity of introducing value semantics to a type system.

Perhaps more success stories will emerge over time, but in the meantime it remains an open question whether dependent types are net beneficial in practice to application development. Further experimentation would be required to answer this question, and Roc is not the right language to do those experiments.

## [Can one application have multiple platforms? Can platforms compose?](#multiple-platforms)

The short answer to each of these questions is "No." To understand why, it's helpful to look at this from the platform author's perspective.

Building a Roc platform means implementing two things:

1. The public-facing Roc API. This is the part the application author sees.
2. The "host," which provides the lower-level implementation behind that API. The host isn't written in Roc, and isn't visible to the application author.

Let's say I'm writing a platform and I decide to implement the host in C.

My C code will compile to an executable which does something like this:

1. Start running the program
2. At some point, call a function named something like "roc_app_main"
3. That roc_app_main function will result in the actual Roc application code running.

An important characteristic of this design is that the platform is in complete control of when all the Roc code runs. If the host is written in C, that C code will specify the `main()` function that runs when the compiled binary runs. (The Roc application might also have something named `main`, but that just compiles down to a pure function the C host can choose to call—or not—whenever it pleases.)

One of the main goals of this design is to give platform authors the ability to make a coherent experience for a specific domain. The public API can omit operations that aren't implementable in a particular host, or which wouldn't make sense in the target domain. The Task data structure provides enough information for the host to use any kind of asynchronous I/O system they like, or synchronous blocking I/O if that makes more sense.

The single-platform design is based around the idea of giving a platform author exclusive control over which primitives are available and how they're implemented at a low level. Exclusivity is the point! It's not really clear how "multiple platforms" or "composed platforms" would work, but certainly it would require sacrificing the current benefit of one platform being able to provide a cohesive experience for its domain.

With all that in mind, it's possible to write Roc code that can be used across multiple platforms. Applications can use platform-agnostic packages, as well as packages involving I/O, as long as their platform supports the I/O operations in question.

Similarly, platforms can share code for common Roc logic using normal Roc packages. Code can also be shared between hosts using whatever normal code sharing mechanisms their chosen host languages support.

Putting all this together, applications have exactly one platform, which enables platform authors to create a cohesive experience optimized for a particular domain, and both application authors and platform authors can share code as much as they like.

## [Will Roc ever compile to JavaScript, JVM/CLR/BEAM bytecode, or other higher-level targets?](#other-compilation-targets)

The plan is for Roc to compile only to very low-level targets like machine code and WebAssembly.

This is partly in order to keep the scope of the project smaller, but also because supporting higher-level targets could make it significantly more difficult to create an ecosystem of high-performance Roc packages. Techniques which are performance optimizations on low-level targets like machine code or WebAssembly might actually impede performance on higher-level targets, and vice versa.

Additionally, some of these higher-level targets may not be able to represent Roc's full range of numbers efficiently (e.g. Roc's very common [`U64`](https://www.roc-lang.org/builtins/Num#U64) would be represented in JavaScript as [a heap-allocated `BigInt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt), which would be massively slower), and their string representations may not work well with Roc's builtins (e.g. Roc's [`Str`](https://www.roc-lang.org/builtins/Str) uses UTF-8 and has operations which let you traverse those bytes directly, although the JVM, CLR, and JavaScript all use UTF-16).

Fortunately, since these higher-level targets tend to support some combination of C FFI, WebAssembly calls, or both, there's already a path to use Roc code in those environments even if Roc doesn't directly compile to their higher-level bytecode.

## [Will Roc's compiler ever be self-hosted? (That is, will it ever be written in Roc?)](#self-hosted-compiler) {#self-hosted-compiler}

The plan is to never implement Roc's compiler in Roc.

The goal is for Roc's compiler to deliver the best user experience possible. Compiler performance is strongly influenced by how memory is used, and there are many performance benefits to be gained from using a systems language like Rust which offers more direct control over memory than Roc ever should.

Roc isn't trying to be the best possible language for high-performance compiler development, but it is trying to have a high-performance compiler. The best tool for that job is a language other than Roc, so that's what we're using!

## [Why does Roc use the license it does?](#permissive-license) {#permissive-license}

The short explanation for why Roc is released under the [Universal Permissive License](https://opensource.org/licenses/UPL):

- Like [MIT](https://opensource.org/licenses/MIT), it's permissive and concise
- Like [Apache2](https://opensource.org/licenses/Apache-2.0), it protects against contributors claiming software patents over contributed code after the fact (MIT and BSD do not include protections against this)
- It's compatible with [GPLv2](https://opensource.org/licenses/GPL-2.0) (which [Apache2 is not](https://www.apache.org/licenses/GPL-compatibility.html))
- It's one license, unlike "MIT or Apache2, at your choice" (which is how [Rust addressed the problem](https://internals.rust-lang.org/t/rationale-of-apache-dual-licensing/8952/4) of MIT not having patent protections but Apache2 not being GPLv2 compatible)
- It's been approved by OSI, FSF, and Oracle's lawyers, so it has been not only vetted by three giants in the world of software licensing, but also three giants with competing interests - and they all approved it.

There's also [a longer explanation](https://github.com/roc-lang/roc/issues/1199) with more detail about the motivation and thought process, if you're interested.

## [Why does Roc use both Rust and Zig?](#rust-and-zig) {#rust-and-zig}

Roc's compiler has always been written in [Rust](https://www.rust-lang.org/). Roc's standard library was briefly written in Rust, but was soon rewritten in [Zig](https://ziglang.org/).

There were a few reasons for this rewrite.

1. We struggled to get Rust to emit LLVM bitcode in the format we needed, which is important so that LLVM can do whole-program optimizations across the standard library and compiled application.
2. Since the standard library has to interact with raw generated machine code (or LLVM bitcode), the Rust code unavoidably needed `unsafe` annotations all over the place. This made one of Rust's biggest selling points inapplicable in this particular use case.
3. Given that Rust's main selling points are inapplicable (its package ecosystem being another), Zig's much faster compile times are a welcome benefit.
4. Zig has more tools for working in a memory-unsafe environment, such as reporting memory leaks in tests. These have been helpful in finding bugs that are out of scope for safe Rust.

The split of Rust for the compiler and Zig for the standard library has worked well so far, and there are no plans to change it.
