# Frequently Asked Questions

## Where did the name Roc come from?

<img width="128" alt="The Roc logo, an origami bird" src="https://user-images.githubusercontent.com/1094080/92188927-e61ebd00-ee2b-11ea-97ef-2fc88e0094b0.png">

The Roc programming language is named after [a mythical bird](<https://en.wikipedia.org/wiki/Roc_(mythology)>).

That’s why the logo is a bird. It’s specifically an [_origami_ bird](https://youtu.be/9gni1t1k1uY) as an homage
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

## Why make a new editor instead of making an LSP plugin for VSCode, Vim or Emacs?

The Roc editor is one of the key areas where we want to innovate. Constraining ourselves to a plugin for existing editors would severely limit our possibilities for innovation.

A key part of our editor will be the use of plugins that are shipped with libraries. Think of a regex visualizer, parser debugger, or color picker. For library authors, it would be most convenient to write these plugins in Roc. Trying to dynamically load library plugins (written in Roc) in for example VSCode seems very difficult.

## Is there syntax highlighting for Vim/Emacs/VS Code or a LSP?

Not currently. Although they will presumably exist someday, while Roc is in the early days there's actually a conscious
effort to focus on the Roc Editor _instead of_ adding Roc support to other editors - specifically in order to give the Roc
Editor the best possible chance at kickstarting a virtuous cycle of plugin authorship.

This is an unusual approach, but there are more details in [this 2021 interview](https://youtu.be/ITrDd6-PbvY?t=212).

In the meantime, using CoffeeScript syntax highlighting for .roc files turns out to work surprisingly well!

## Why is there no way to specify "import everything this module exposes" in `imports`?

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
whether the feature should be in the language at all. In the case of this feature, I think it's simpler if the
language doesn't have it; that way nobody has to learn (or spend time spreading the word) about the
performance-boosting advice not to use it.

## Why can't functions be compared for equality using the `==` operator?

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

Another option would be to define that function equality always returns `False`. So both of these would evaluate
to `False`:

- `(\x -> x + 1) == (\x -> 1 + x)`
- `(\x -> x + 1) == (\x -> x + 1)`

This makes function equality effectively useless, while still technically allowing it. It has some other downsides:

- Now if you put a function inside a record, using `==` on that record will still type-check, but it will then return `False`. This could lead to bugs if you didn't realize you had accidentally put a function in there - for example, because you were actually storing a different type (e.g. an opaque type) and didn't realize it had a function inside it.
- If you put a function (or a value containing a function) into a `Dict` or `Set`, you'll never be able to get it out again. This is a common problem with [NaN](https://en.wikipedia.org/wiki/NaN), which is also defined not to be equal to itself.

The first of these problems could be addressed by having function equality always return `True` instead of `False` (since that way it would not affect other fields' equality checks in a record), but that design has its own problems:

- Although function equality is still useless, `(\x -> x + 1) == (\x -> x)` returns `True`. Even if it didn't lead to bugs in practice, this would certainly be surprising and confusing to beginners.
- Now if you put several different functions into a `Dict` or `Set`, only one of them will be kept; the others will be discarded or overwritten. This could cause bugs if a value stored a function internally, and then other functions relied on that internal function for correctness.

Each of these designs makes Roc a language that's some combination of more error-prone, more confusing, and more
brittle to change. Disallowing function equality at compile time eliminates all of these drawbacks.

## Why doesn't Roc have a `Maybe` or `Option` or `Optional` type, or `null` or `nil` or `undefined`?

It's common for programming languages to have a [null reference](https://en.wikipedia.org/wiki/Null_pointer)
(e.g. `null` in C, `nil` in Ruby, `None` in Python, or `undefined` in JavaScript).
The inventor of the null reference refers to it as his "[billion dollar mistake](https://en.wikipedia.org/wiki/Null_pointer#History)" because it "has led to innumerable errors, vulnerabilities, and system crashes, which have probably caused a billion dollars of pain and damage in the last forty years."

For this and other reasons, many languages do not include a null reference, but instead have a standard library
data type which can be used in situations where a null reference would otherwise be used. Common names for this
null reference alternative type include `Maybe` (like in Haskell or Elm), `Option` (like in OCaml or Rust),
and `Optional` (like in Java).

By design, Roc does not have one of these. There are several reasons for this.

First, if a function returns a potential error, Roc has the convention to use `Result` with an error type that
has a single tag describing what went wrong. (For example, `List.first : List a -> Result a [ListWasEmpty]*`
instead of `List.first : List a -> Maybe a`.) This is not only more self-descriptive, it also composes better with
other operations that can fail; there's no need to have functions like `Result.toMaybe` or `Maybe.toResult`,
because in Roc, the convention is that operations that can fail always use `Result`.

Second, optional record fields can be handled using Roc's Optional Record Field language feature, so using a type like `Maybe` there would be less ergonomic.

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

On a historical note, `Maybe` may have been thought of as a substitute for null references—as opposed to something that emerged organically based on specific motivating use cases after `Result` already existed. That said, in languages that do not have an equivalent of Roc's tag unions, it's much less ergonomic to write something like `Result a [ListWasEmpty]*`, so that design would not fit those languages as well as it fits Roc.

## Why doesn't Roc have higher-kinded polymorphism or arbitrary-rank types?

_Since this is a FAQ answer, I'm going to assume familiarity with higher-kinded types and higher-rank types instead of including a primer on them._

A valuable aspect of Roc's type system is that it has decidable [principal](https://en.wikipedia.org/wiki/Principal_type)
type inference. This means that:

- At compile time, Roc can correctly infer the types for every expression in a program, even if you don't annotate any of the types.
- This inference always infers the most general type possible; you couldn't possibly add a valid type annotation that would make the type more flexible than the one that Roc would infer if you deleted the annotation.

It's been proven that any type system which supports either [higher-kinded polymorphism](https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf) or [arbitrary-rank types](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf) cannot have decidable
principal type inference. With either of those features in the language, there will be situations where the compiler
would be unable to infer a type—and you'd have to write a type annotation. This also means there would be
situations where the editor would not be able to reliably tell you the type of part of your program, unlike today
where it can accurately tell you the type of anything, even if you have no type annotations in your entire code base.

### Arbitrary-rank types

Unlike arbitrary-rank (aka "Rank-N") types, both Rank-1 and Rank-2 type systems are compatible with principal
type inference. Roc currently uses Rank-1 types, and the benefits of Rank-N over Rank-2 don't seem worth
sacrificing principal type inference to attain, so let's focus on the trade-offs between Rank-1 and Rank-2.

Supporting Rank-2 types in Roc has been discussed before, but it has several important downsides:

- It would increase the complexity of the language.
- It would make some compiler error messages more confusing (e.g. they might mention `forall` because that was the most general type that could be inferred, even if that wasn't helpful or related to the actual problem).
- It would substantially increase the complexity of the type checker, which would necessarily slow it down.

No implementation of Rank-2 types can remove any of these downsides. Thus far, we've been able to come up
with sufficiently nice APIs that only require Rank-1 types, and we haven't seen a really compelling use case
where the gap between the Rank-2 and Rank-1 designs was big enough to justify switching to Rank-2.

Since I prefer Roc being simpler and having a faster compiler with nicer error messages, my hope is that Roc
will never get Rank-2 types. However, it may turn out that in the future we learn about currently-unknown
upsides that somehow outweigh these downsides, so I'm open to considering the possibility - while rooting against it.

### Higher-kinded polymorphism

I want to be really clear about this one: the explicit plan is that Roc will never support higher-kinded polymorphism.

On the technical side, the reasons for this are ordinary: I understand the practical benefits and
drawbacks of HKP, and I think the drawbacks outweigh the benefits when it comes to Roc. (Those who come to a
different conclusion may think HKP's drawbacks would be less of a big a deal in Roc than I do. That's reasonable;
we programmers often weigh the same trade-offs differently.) To be clear, I think this in the specific context of
Roc; there are plenty of other languages where HKP seems like a great fit. For example, it's hard to imagine Haskell
without it. Similarly, I think lifetime annotations are a great fit for Rust, but don't think they'd be right
for Roc either.

I also think it's important to consider the cultural implications of deciding whether or not to support HKP.
To illustrate what I mean, imagine this conversation:

**Programmer 1:** "How do you feel about higher-kinded polymorphism?"

**Programmer 2:** "I have no idea what that is."

**Programmer 1:** "Okay, how do you feel about monads?"

**Programmer 2:** "OH NO."

I've had several variations of this conversation: I'm talking about higher-kinded types,
another programmer asks what that means, I give monads as an example, and their reaction is strongly negative.
I've also had plenty of conversations with programmers who love HKP and vigorously advocate for its addition
to languages they use which don't have it. Feelings about HKP seem strongly divided, maybe more so
than any other type system feature besides static and dynamic types.

It's impossible for a programming language to be neutral on this. If the language doesn't support HKP, nobody can
implement a Monad typeclass (or equivalent) in any way that can be expected to catch on. Advocacy to add HKP to the
language will inevitably follow. If the language does support HKP, one or more alternate standard libraries built
around monads will inevitably follow, along with corresponding cultural changes. (See Scala for example.)
Culturally, to support HKP is to take a side, and to decline to support it is also to take a side.

Given this, language designers have three options:

- Have HKP and have Monad in the standard library. Embrace them and build a culture and ecosystem around them.
- Have HKP and don't have Monad in the standard library. An alternate standard library built around monads will inevitably emerge, and both the community and ecosystem will divide themselves along pro-monad and anti-monad lines.
- Don't have HKP; build a culture and ecosystem around other things.

Considering that these are the only three options, I think the best choice for Roc—not only on a technical
level, but on a cultural level as well—is to make it clear that the plan is for Roc never to support HKP.
I hope this clarity can save a lot of community members' time that would otherwise be spent on advocacy or
arguing between the two sides of the divide. Again, I think it's completely reasonable for anyone to have a
different preference, but given that language designers can only choose one of these options, I'm confident
I've made the right choice for Roc by designing it never to have higher-kinded polymorphism.

## Why do Roc's syntax and standard library differ from Elm's?

Roc is a direct descendant of [Elm](https://elm-lang.org/). However, there are some differences between the two languages.

Syntactic differences are among these. This is a feature, not a bug; if Roc had identical syntax to Elm, then it's
predictable that people would write code that was designed to work in both languages - and would then rely on
that being true, for example by making a package which advertised "Works in both Elm and Roc!" This in turn
would mean that later if either language were to change its syntax in a way that didn't make sense for the other,
the result would be broken code and sadness.

So why does Roc have the specific syntax changes it does? Here are some brief explanations:

- `#` instead of `--` for comments - this allows [hashbang](https://senthilnayagan.medium.com/shebang-hashbang-10966b8f28a8)s to work without needing special syntax. That isn't a use case Elm supports, but it is one Roc is designed to support.
- `{}` instead of `()` for the unit type - Elm has both, and they can both be used as a unit type. Since `{}` has other uses in the type system, but `()` doesn't, I consider it redundant and took it out.
- `when`...`is` instead of `case`...`of` - I predict it will be easier for beginners to pick up, because usually the way I explain `case`...`of` to beginners is by saying the words "when" and "is" out loud - e.g. "when `color` is `Red`, it runs this first branch; when `color` is `Blue`, it runs this other branch..."
- `:` instead of `=` for record field definitions (e.g. `{ foo: bar }` where Elm syntax would be `{ foo = bar }`): I like `=` being reserved for definitions, and `:` is the most popular alternative.
- Backpassing syntax - since Roc is designed to be used for use cases like command-line apps, shell scripts, and servers, I expect chained effects to come up a lot more often than they do in Elm. I think backpassing is nice for those use cases, similarly to how `do` notation is nice for them in Haskell.
- Tag unions instead of Elm's custom types (aka algebraic data types). This isn't just a syntactic change; tag unions are mainly in Roc because they can facilitate errors being accumulated across chained effects, which (as noted a moment ago) I expect to be a lot more common in Roc than in Elm. If you have tag unions, you don't really need a separate language feature for algebraic data types, since closed tag unions essentially work the same way - aside from not giving you a way to selectively expose variants or define phantom types. Roc's opaque types language feature covers those use cases instead.
- No `::` operator, or `::` pattern matching for lists. Both of these are for the same reason: an Elm `List` is a linked list, so both prepending to it and removing an element from the front are very cheap operations. In contrast, a Roc `List` is a flat array, so both prepending to it and removing an element from the front are among the most expensive operations you can possibly do with it! To get good performance, this usage pattern should be encouraged in Elm and discouraged in Roc. Since having special syntax would encourage it, it would not be good for Roc to have that syntax!
- No `<|` operator. In Elm, I almost exclusively found myself wanting to use this in conjunction with anonymous functions (e.g. `foo <| \bar -> ...`) or conditionals (e.g. `foo <| if bar then ...`). In Roc you can do both of these without the `<|`. That means the main remaining use for `<|` is to reduce parentheses, but I tend to think `|>` is better at that (or else the parens are fine), so after the other syntactic changes, I considered `<|` an unnecessary stylistic alternative to `|>` or parens.
- The `|>` operator passes the expression before the `|>` as the _first_ argument to the function after the `|>` instead of as the last argument. See the section on currying for details on why this works this way.
- `:` instead of `type alias` - I like to avoid reserved keywords for terms that are desirable in userspace, so that people don't have to name things `typ` because `type` is a reserved keyword, or `clazz` because `class` is reserved. (I couldn't think of satisfactory alternatives for `as`, `when`, `is`, or `if` other than different reserved keywords. I could see an argument for `then`—and maybe even `is`—being replaced with a `->` or `=>` or something, but I don't anticipate missing either of those words much in userspace. `then` is used in JavaScript promises, but I think there are several better names for that function.)
- No underscores in variable names - I've seen Elm beginners reflexively use `snake_case` over `camelCase` and then need to un-learn the habit after the compiler accepted it. I'd rather have the compiler give feedback that this isn't the way to do it in Roc, and suggest a camelCase alternative. I've also seen underscores used for lazy naming, e.g. `foo` and then `foo_`. If lazy naming is the goal, `foo2` is just as concise as `foo_`, but `foo3` is more concise than `foo__`. So in a way, removing `_` is a forcing function for improved laziness. (Of course, more descriptive naming would be even better.)
- Trailing commas - I've seen people walk away (in some cases physically!) from Elm as soon as they saw the leading commas in collection literals. While I think they've made a mistake by not pushing past this aesthetic preference to give the language a chance, I also would prefer not put them in a position to make such a mistake in the first place. Secondarily, while I'm personally fine with either style, between the two I prefer the look of trailing commas.
- The `!` unary prefix operator. I didn't want to have a `Basics` module (more on that in a moment), and without `Basics`, this would either need to be called fully-qualified (`Bool.not`) or else a module import of `Bool.{ not }` would be necessary. Both seemed less nice than supporting the `!` prefix that's common to so many widely-used languages, especially when we already have a unary prefix operator of `-` for negation (e.g. `-x`).
- `!=` for the inequality operator (instead of Elm's `/=`) - this one pairs more naturally with the `!` prefix operator and is also very common in other languages.

Roc also has a different standard library from Elm. Some of the differences come down to platforms and applications (e.g. having `Task` in Roc's standard library wouldn't make sense), but others do not. Here are some brief explanations:

- No `Basics` module. I wanted to have a simple rule of "all modules in the standard library are imported by default, and so are their exposed types," and that's it. Given that I wanted the comparison operators (e.g. `<`) to work only on numbers, it ended up that having `Num` and `Bool` modules meant that almost nothing would be left for a `Basics` equivalent in Roc except `identity` and `Never`. The Roc type `[]` (empty tag union) is equivalent to `Never`, so that wasn't necessary, and I generally think that `identity` is a good concept but a sign of an incomplete API whenever its use comes up in practice. For example, instead of calling `|> List.filterMap identity` I'd rather have access to a more self-descriptive function like `|> List.dropNothings`. With `Num` and `Bool`, and without `identity` and `Never`, there was nothing left in `Basics`.
- `Str` instead of `String` - after using the `str` type in Rust, I realized I had no issue whatsoever with the more concise name, especially since it was used in so many places (similar to `Msg` and `Cmd` in Elm) - so I decided to save a couple of letters.
- No function composition operators - I stopped using these in Elm so long ago, at one point I forgot they were in the language! See the FAQ entry on currying for details about why.
- No `Char`. What most people think of as a "character" is a rendered glyph. However, rendered glyphs are comprised of [grapheme clusters](https://stackoverflow.com/a/27331885), which are a variable number of Unicode code points - and there's no upper bound on how many code points there can be in a single cluster. In a world of emoji, I think this makes `Char` error-prone and it's better to have `Str` be the only first-class unit. For convenience when working with unicode code points (e.g. for performance-critical tasks like parsing), the single-quote syntax is sugar for the corresponding `U32` code point - for example, writing `'鹏'` is exactly the same as writing `40527`. Like Rust, you get a compiler error if you put something in single quotes that's not a valid [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value).
- No `Debug.log` - the editor can do a better job at this, or you can write `expect x != x` to see what `x` is when the expectation fails. Using the editor means your code doesn't change, and using `expect` gives a natural reminder to remove the debugging code before shipping: the build will fail.
- No `Debug.todo` - instead you can write a type annotation with no implementation below it; the type checker will treat it normally, but attempting to use the value will cause a runtime exception. This is a feature I've often wanted in Elm, because I like prototyping APIs by writing out the types only, but then when I want the compiler to type-check them for me, I end up having to add `Debug.todo` in various places.
- No `Maybe`. See the "Why doesn't Roc have a `Maybe`/`Option`/`Optional` type" FAQ question

## Why aren't Roc functions curried by default?

Although technically any language with first-class functions makes it possible to curry
any function (e.g. I can manually curry a Roc function `\x, y, z ->` by writing `\x -> \y -> \z ->` instead),
typically what people mean when they say Roc isn't a curried language is that Roc functions aren't curried
by default. For the rest of this section, I'll use "currying" as a shorthand for "functions that are curried
by default" for the sake of brevity.

As I see it, currying has one major upside and several major downsides. The upside:

- It makes function calls more concise in some cases.

The downsides:

- It lowers error message quality, because there can no longer be an error for "function called with too few arguments." (Calling a function with fewer arguments is always valid in curried functions; the error you get instead will unavoidably be some other sort of type mismatch, and it will be up to you to figure out that the real problem was that you forgot an argument.)
- It makes the `|>` operator more error-prone in some cases.
- It makes higher-order function calls need more parentheses in some cases.
- It significantly increases the language's learning curve. (More on this later.)
- It facilitates pointfree function composition. (More on why this is listed as a downside later.)

There's also a downside that it would make runtime performance of compiled programs worse by default,
but I assume it would be possible to optimize that away at the cost of slightly longer compile times.

I consider the one upside (conciseness in some places) extremely minor, and have almost never missed it in Roc.
Here are some more details about the downsides as I see them.

### Currying and the `|>` operator

In Roc, both of these expressions evaluate to `"Hello, World!"`

```elixir
Str.concat "Hello, " "World!"
```

```elixir
"Hello, "
|> Str.concat "World!"
```

In curried languages with a `|>` operator, the first expression still returns `"Hello, World!"` but the second one returns `"World!Hello, "`. This is because Roc's `|>` operator uses the expression before the `|>` as the _first_ argument, whereas in curried languages, `|>` uses it as the _last_ argument.

(For example, this is how `|>` works in both [F#](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/#function-symbols-and-operators) and in [Elm](https://package.elm-lang.org/packages/elm/core/1.0.5/Basics#|%3E), both of which are curried languages. In contrast, Roc's `|>` design uses the same argument ordering as [Elixir](https://hexdocs.pm/elixir/1.14.0/Kernel.html#%7C%3E/2) and [Gleam](https://gleam.run/book/tour/functions.html#pipe-operator), neither of which is a curried language.)

This comes up in other situations as well. For example, consider subtraction and division:

```elixir
someNumber
|> Num.div 2
```

```elixir
someNumber
|> Num.sub 1
```

What do you expect these expressions to do?

In Roc, the first  divides `someNumber` by 2 and the second one subtracts 1 from `someNumber`.  In languages where `|>` uses the other argument ordering, the first example instead takes 2 and divides it by `someNumber`, while the second takes 1 and subtracts `someNumber` from it. This was a pain point I ran into with curried languages, and I was pleasantly surprised that changing the argument ordering in `|>` addressed the pain point.

This style has a second benefit when it comes to higher-order functions. Consider these two examples:

```elixir
answer = List.map numbers \num ->
    someFunction
        "some argument"
        anotherArg
        someOtherArg
```

```elixir
numbers
|> List.map Num.abs
```

In Roc, `List.map` takes a list and then a function. Because of the way `|>` works in Roc, `numbers |> List.map Num.abs` passes `numbers` as the first argument to `List.map`, and `Num.abs` as the second argument. So both of these examples work fine.

In a curried language, these two examples couldn't both be valid. In order for `|> List.map Num.abs` to work in a curried language (where `|>` works the other way), `List.map` would have to take its arguments in the opposite order: the function first and the list second.

This means the first example would have to change from this...

```elixir
answer = List.map numbers \num ->
    someFunction
        "some argument"
        anotherArg
        someOtherArg
```

...to this:

```elixir
answer =
    List.map
        (\num ->
            someFunction
                "some argument"
                anotherArg
                someOtherArg
        )
        numbers
```

This was also a pain point I'd encountered in curried languages. I prefer the way the former example reads, but that style doesn't work with the argument order that currying encourages for higher-order functions like `List.map`. (Prior to using curried languages, I'd used [CoffeeScript](https://coffeescript.org/) in a functional style with [`_.map`](https://underscorejs.org/#map), and was disappointed to realize that I could no longer use the enjoyable style of `answer = _.map numbers (num) -> …` as I had before. In Roc, this style works.)

As a historical note, these stylistic benefits (of `|> Num.sub 1` working as expected, and being able to write `List.map numbers \num ->`) were not among the original reasons Roc did not have currying. These benefits were discovered after the decision had already been made that Roc would not be a curried language, and they served to reinforce after the fact that the decision was the right one for Roc given the language's goals.

### Currying and learning curve

Prior to designing Roc, I taught a lot of beginner [Elm](https://elm-lang.org/) workshops. Sometimes at
conferences, sometimes for [Frontend Masters](https://frontendmasters.com/courses/intro-elm/),
sometimes for free at local coding bootcamps or meetup groups.
In total I've spent well over 100 hours standing in front of a class, introducing the students to their
first pure functional programming language.

Here was my experience teaching currying:

- The only way to avoid teaching it is to refuse to explain why multi-argument functions have multiple `->`s in them. (If you don't explain it, at least one student will ask about it - and many if not all of the others will wonder.)
- Teaching currying properly takes a solid chunk of time, because it requires explaining partial application, explaining how curried functions facilitate partial application, how function signatures accurately reflect that they're curried, and going through examples for all of these.
- Even after doing all this, and iterating on my approach each time to try to explain it more effectively than I had the time before, I'd estimate that under 50% of the class ended up actually understanding currying. I consistently heard that in practice it only "clicked" for most people after spending significantly more time writing code with it.

This is not the end of the world, especially because it's easy enough to think "okay, I still don't totally get this
even after that explanation, but I can remember that function arguments are separated by `->` in this language
and maybe I'll understand the rest later." (Which they almost always do, if they stick with the language.)
Clearly currying doesn't preclude a language from being easy to learn, because Elm has currying, and Elm's learning
curve is famously gentle.

That said, beginners who feel confused while learning the language are less likely to continue with it.
And however easy Roc would be to learn if it had currying, the language is certainly easier to learn without it.

### Pointfree function composition

[Pointfree function composition](https://en.wikipedia.org/wiki/Tacit_programming) is where you define
a new function by composing together two existing functions without naming intermediate arguments.
Here's an example:

```elm
reverseSort : List elem -> List elem
reverseSort = compose List.reverse List.sort

compose : (a -> b), (c -> a) -> (c -> b)
compose = \f, g, x -> f (g x)
```

Here's how I would instead write this:

```elm
reverseSort : List elem -> List elem
reverseSort = \list -> List.reverse (List.sort list)
```

I've consistently found that I can more quickly and accurately understand function definitions that use
named arguments, even though the code is longer. I suspect this is because I'm faster at reading than I am at
desugaring, and whenever I read the top version I end up needing to mentally desugar it into the bottom version.
In more complex examples (this is among the tamest pointfree function composition examples I've seen), I make
a mistake in my mental desugaring, and misunderstand what the function is doing - which can cause bugs.

I assumed I would get faster and more accurate at this over time. However, by now it's been about a decade
since I first learned about the technique, and I'm still slower and less accurate at reading code that uses
pointfree function composition (including if I wrote it - but even moreso if I didn't) than code written with
with named arguments. I've asked a lot of other programmers about their experiences with pointfree function
composition over the years, and the overwhelming majority of responses have been consistent with my experience.

As such, my opinion about pointfree function composition has gotten less and less nuanced over time. I've now moved
past "it's the right tool for the job, sometimes" to concluding it's best thought of as an antipattern. This is
because I realized how much time I was spending evaluating on a case-by-case basis whether it might be the
right fit for a given situation. The time spent on this analysis alone vastly outweighed the sum of all the
benefits I got in the rare cases where I concluded it was a fit. So I've found the way to get the most out of
pointfree function composition is to never even think about using it; every other strategy leads to a worse outcome.

Currying facilitates the antipattern of pointfree function composition, which I view as a downside of currying.

Stacking up all these downsides of currying against the one upside of making certain function calls more concise,
I concluded that it would be a mistake to have it in Roc.

## Will Roc ever have linear types, dependent types, refinement types, or uniqueness types?

The plan is for Roc to never have linear types, dependent types, refinement types, or uniqueness types.

Fast compile times are a primary goal for Roc, and a major downside of refinement types is an exponential increase in compile times. This rules out refinement types for Roc.

If Roc were to have linear types or uniqueness types, they would move things that are currently behind-the-scenes performance optimizations into the type system. For them to be effective across the ecosystem, they couldn't really be opt-in; everyone would have to use them, even those for whom the current system of behind-the-scenes optimizations already met their performance needs without any added type system complexity. Since the overwhelming majority of use cases are expected to fall into that latter group, adding linear types or uniqueness types to Roc would be a net negative for the ecosystem.

Dependent types are too risky of a bet for Roc to take. They have been implemented in programming languages for three decades, and for at least half that time period, it has been easy to find predictions that dependent types will be the future of type systems. Much harder to find are success stories of complex applications built with dependent types, which realized benefits that significantly outweighed the substantial complexity of introducing value semantics to a type system.

Perhaps more success stories will emerge over time, but in the meantime it remains an open question whether dependent types are net beneficial in practice to application development. Further experimentation would be required to answer this question, and Roc is not the right language to do those experiments.

## Will Roc's compiler ever be self-hosted? (That is, will it ever be written in Roc?)

The plan is to never implement Roc's compiler in Roc.

The goal is for Roc's compiler to deliver the best user experience possible. Compiler performance is strongly influenced by how memory is used, and there are many performance benefits to be gained from using a systems language like Rust which offers more direct control over memory than Roc ever should.

Roc isn't trying to be the best possible language for high-performance compiler development, but it is trying to have a high-performance compiler. The best tool for that job is a language other than Roc, so that's what we're using!

## Why does Roc use the license it does?

The short explanation for why Roc is released under the [Universal Permissive License](https://opensource.org/licenses/UPL):

- Like [MIT](https://opensource.org/licenses/MIT), it's permissive and concise
- Like [Apache2](https://opensource.org/licenses/Apache-2.0), it protects against contributors claiming software patents over contributed code after the fact (MIT and BSD do not include protections against this)
- It's compatible with [GPLv2](https://opensource.org/licenses/GPL-2.0) (which [Apache2 is not](https://www.apache.org/licenses/GPL-compatibility.html))
- It's one license, unlike "MIT or Apache2, at your choice" (which is how [Rust addressed the problem](https://internals.rust-lang.org/t/rationale-of-apache-dual-licensing/8952/4) of MIT not having patent protections but Apache2 not being GPLv2 compatible)
- It's been approved by OSI, FSF, and Oracle's lawyers, so it has been not only vetted by three giants in the world of software licensing, but also three giants with competing interests - and they all approved it.

There's also [a longer explanation](https://github.com/roc-lang/roc/issues/1199) with more detail about the motivation and thought process, if you're interested.

## Why does Roc use both Rust and Zig?

Roc's compiler has always been written in [Rust](https://www.rust-lang.org/). Roc's standard library was briefly written in Rust, but was soon rewritten in [Zig](https://ziglang.org/).

There were a few reasons for this rewrite.

1. We struggled to get Rust to emit LLVM bitcode in the format we needed, which is important so that LLVM can do whole-program optimizations across the standard library and compiled application.
2. Since the standard library has to interact with raw generated machine code (or LLVM bitcode), the Rust code unavoidably needed `unsafe` annotations all over the place. This made one of Rust's biggest selling points inapplicable in this particular use case.
3. Given that Rust's main selling points are inapplicable (its package ecosystem being another), Zig's much faster compile times are a welcome benefit.
4. Zig has more tools for working in a memory-unsafe environment, such as reporting memory leaks in tests. These have been helpful in finding bugs that are out of scope for safe Rust.

The split of Rust for the compiler and Zig for the standard library has worked well so far, and there are no plans to change it.

## Why is the website so basic?

We have a very basic website on purpose, it helps set expectations that roc is a work in progress and not ready yet for a first release.
