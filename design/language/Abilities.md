# Proposal: Abilities in Roc

Status: we invite you to try out abilities for beta use, and are working on resolving known limitations (see issue [#2463](https://github.com/roc-lang/roc/issues/2463)).

This design idea addresses a variety of problems in Roc at once. It also unlocks some very exciting benefits that I didn't expect at the outset! It's a significant addition to the language, but it also means two other language features can be removed, and numbers can get a lot simpler.

Thankfully it's a non-breaking change for most Roc code, and in the few places where it actually is a breaking change, the fix should consist only of shifting a handful of characters around. Still, it feels like a big change because of all the implications it brings. Here we go!

## Background

Elm has a few specially constrained type variables: `number`, `comparable`, `appendable`, and the lesser-known `compappend`. Roc does not have these; it has no `appendable` or `compappend` equivalent, and instead of `number` and `comparable` it has:

-   `Num *` as the type of number literals, with type aliases like `I64 : Num (Integer Signed64)`
-   The functionless constraint for type variables; for example, the type of `Bool.isEq` is `'var, 'var -> Bool` - and the apostrophe at the beginning of `'var` means that it must represent a type that has no functions anywhere in it.

There are a few known problems with this design, as well as some missed opportunities.

### Problem 1: Nonsense numbers type-check

Right now in Roc, the following type-checks:

```coffee
x : Num [ Whatever Str, Blah (List {} -> Bool) ]
x = 5
```

This type-checks because the number literal 5 has the type `Num *`, which unifies with any `Num` - even if the type variable is complete nonsense.

It's not clear what should happen here after type-checking. What machine instructions should Roc generate for this nonsense number type? Suppose I later wrote (`if x + 1 > 0 then … `) - what hardware addition instruction should be generated there?

Arguably the compiler should throw an error - but when? We could do it at compile time, during code generation, but that goes against the design goal of "you can always run your Roc program, even if there are compile-time errors, and it will get as far as it can."

So then do we generate a runtime exception as soon as you encounter this code? Now Roc's type system is arguably unsound, because this is a runtime type error which the type checker approved.

Do we add an extra special type constraint just for Num to detect this during the type-checking phase? Now Num is special-cased in a way that no other type is…

None of these potential solutions have ever felt great to me.

### Problem 2: Custom number types can't use arithmetic operators

Roc's ordinary numbers should be enough for most use cases, but there are nice packages like [elm-units](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/) which can prevent [really expensive errors](https://spacemath.gsfc.nasa.gov/weekly/6Page53.pdf) by raising compile-time errors for mismatched units...at the cost of having to sacrifice normal arithmetic operators. You can't use `+` on your unit-ful numbers, because `+` in Roc desugars to `Num.add`, not (for example) `Quantity.add`.

Also, if 128-bit integers aren't big enough, because the numbers you're working with are outside the undecillion range (perhaps recording the distance between the Earth and the edge of the universe in individual atoms or something?) maybe you want to make an arbitrary-sized integer package. Again, you can do that, but you can't use `+` with it. Same with vector packages, matrix packages, etc.

This might not sound like a big problem (e.g. people deal with it in Java land), but in domains where you want to use custom numeric types, not having this is (so I've heard) a significant incentive to use plain numbers instead of more helpful data types.

### Problem 3: Decoders are still hard to learn

Roc is currently no different from Elm in this regard. I only recently realized that the design I'm about to describe can also address this problem, but I was very excited to discover that!

### Problem 4: Custom collection equality

Let's suppose I'm creating a custom data structure: a dictionary, possibly backed by a hash map or a tree. We'll ignore the internal structure of the storage field for now, but the basic technique we'd use would be a private tag wrapper to make an opaque type:

```coffee
Dict k v : [ @Dict { storage : … } ]
```

Today in Roc I can make a very nice API for this dictionary, but one thing I can't do is get `==` to do the right thing if my internal storage representation is sensitive to insertion order.

For example, suppose this `Dict` has an internal storage of a binary tree, which means it's possible to get two different internal storage representations depending on the order in which someone makes the same `Dict.insert` calls. Insertion order shouldn't affect equality - what matters is if the two dictionaries contain the same elements! - but by default it does, because `==` only knows how to check if the internal structures match exactly.

This feels like a significantly bigger problem in Roc than it is in Elm, because:

-   It's more likely that people will have applications where custom data structures are valuable, e.g. to efficiently store and retrieve millions of values in memory on a server. (This wouldn't likely happen in a browser-based UI.) Discord [ran into a use case like this](https://discord.com/blog/using-rust-to-scale-elixir-for-11-million-concurrent-users) in Elixir, and ended up turning to Rust FFI to get the performance they needed; I'm optimistic that we can get acceptable performance for use cases like this out of pure Roc data structure implementations, and pure Roc data structures would be much more ergonomic than interop - since having to use Task for every operation would be a significant downside for a data structure.
-   I want to make testing low-friction, especially within the editor, and some of the ideas I have for how to do that rely on `==` being automatically used behind the scenes to compare values against known good values. If someone wrote tests that relied on `==` and then wanted to swap out a data structure for a custom one (e.g. because they ran into the scaling issues Discord did), it would be extra bad if the new data structure stopped working with all the existing tests and they all had to be rewritten to no longer use these convenient testing features and instead use a custom `Dict.contentsEq` or something instead.

This is one of the most serious problems on this list. Not for the short term, but for the long term.

### Problem 5: How to specify functionlessness in documentation

In Roc's current design, certain types have a functionless constraint. For example, in `Bool.isEq : 'val, 'val -> Bool`, the type variable `'val` means "a type that contains no functions, which we are naming val here."

In this design, it's necessarily a breaking change when a type goes from functionless to function-ful, because that type can no longer be used with the `==` operator (among other things).

How do we report on that breaking change? What's the type diff? Just write the sentence "The type Foo was functionless before, but now it isn't" and call it a day? There are solutions to this, but I haven't encountered any I'm particularly fond of.

There's also a related problem with how to display it in documentation. If I have an opaque type that is functionless (as they usually will be), how should the docs display that? A little icon or something? It's more noteworthy when a type is function-ful, so should that be displayed as an icon instead even though there's only syntax in the language for function-less?

This is definitely solvable, but once again I can't name any solutions I love.

### Problem 6: No nice way to specify editor-specific code

One of the goals for Roc is to have packages ship with editor integrations.

For example, let's say I'm making a custom data structure like the `Dict` from earlier. I want to be able to render an interactive "expando" style `Dict` in the editor, so when someone is in the editor looking at a trace of the values running through their program, they can expand the dictionary to look at just its key-value pairs instead of having to wade through its potentially gnarly internal storage representation. It's a similar problem to equality: as the author of `Dict`, I want to customize that!

The question is how I should specify the rendering function for `Dict`. There isn't an obvious answer in current Roc. Would I write a view function in `Dict.roc`, and the editor just looks for a function by that name? If so, would I expose it directly from that module? If so, then does that mean the API docs for Dict will include a view function that's only there for the editor's benefit? Should there be some special language keyword to annotate it as "editor-only" so it doesn't clutter up the rest of the API docs?

As with the `Num *` problem, there are various ways to solve this using the current language primitives, but I haven't found any that seem really nice.

### Problem 7: Record-of-function passing

This is a minor problem, but worth noting briefly.

In Roc's development backend, we do mostly the same thing when generating X86-64 instructions and ARM instructions. However, there are also several points in the process where slightly different things need to happen depending on what architecture we're targeting.

In Rust, we can use traits to specialize these function calls in a way where Rust's compiler will monomorphize specialized versions of one generic function for each architecture, such that each specialized function does direct calls to the appropriate architecture-specific functions at the appropriate moments. It's exactly as efficient as if those specialized functions had each been written by hand, except they all get to share code.

In Roc, you can achieve this same level of reuse by passing around a record of functions, and calling them at the appropriate moments. While this works, it has strictly more overhead potential than the trait-based approach we're using in Rust. Maybe after a bunch of LLVM inlining and optimization passes, it will end up being equivalent, but presumably there will be cases where it does not.

Is the amount of overhead we're talking about here a big deal? Maybe, maybe not, depending on the use case. This is definitely a niche situation, but nevertheless a missed opportunity for some amount of speed compared to what other languages can do.

## Proposal: Abilities

This proposal is about a new language feature called "abilities," which addresses all of these problems in a nice way, while also making some other things possible in the language.

Abilities are similar to traits in Rust. Here's how the type of addition would change from today's world to the Abilities world:

**Today:**

```coffee
Num.add : Num a, Num a -> Num a
```

**Abilities:**

```coffee
Num.add : number, number -> number
    where number has Num
```

The new language keywords are emphasized in bold.

That where `number` has `Num` part is saying that whatever type gets used in place of the number type variable needs to have the Num ability. All the current number types (`I64`, `Dec`, etc.) would have the `Num` ability, the integer types would have the `Int` ability, and the fractional types would have the `Frac` ability.

All of those numeric abilities would be builtins, but you could also define your own custom abilities. Like Rust traits today (that is, Rust 1.56), abilities would not be higher-kinded. The explicit plan would be that they would never be higher-kinded, so it would never be possible to make a `Functor` or `Monad` ability.

### Number Literals

Abilities can require other abilities. For example, to have the `Int` ability, you also need to have the `Num` ability. This means that **has `Int`** is strictly more constraining than **has `Num`**, which in turn means that we can change the type of number literals to be "an unbound variable that has the `Num` ability," similarly to what Haskell does.

Here's how that would look in the REPL:

**Today:**

```coffee
» 5
5 : Num *
```

**Abilities:**

```coffee
» 5
5 : number
    where number has Num
```

I'm not sure which version is more beginner-friendly, to be honest.

The latter is more verbose, but it's much easier to guess roughly what it means. The `*` in `Num *` isn't really self-descriptive, so a beginner playing around in the repl who hasn't learned about type variables yet (let alone wildcard type variables) seems less likely to have any useful intuition about what `Num *` is saying compared to what `where number has Num` is saying.

This change to number literals would solve [Problem #1](#problem-1-nonsense-numbers-type-check) (nonsense numbers type-check) completely. The following would no longer type-check:

```coffee
x : Num [ Whatever Str, Blah (List {} -> Bool) ]
x = 5
```

You could write any of these instead:

```coffee
x : number
    where number has Num
x = 5
```

```coffee
x : integer
    where integer has Int
x = 5
```

```coffee
x : fraction where fraction has Frac # single-line ok!
x = 5
```

```coffee
x : U64
x = 5
```

...but there's no opportunity to inject any nonsense that the type checker would accept.

Since you can add abilities to your own custom types (as we'll see later), this means you can add `Num` to your own custom number types (as well as `Int` or `Frac`) and then use them with all the usual arithmetic operators. This solves [Problem #2](#problem-2-custom-number-types-cant-use-arithmetic-operators).

### Functionless Constraints

Here's how the type of `Bool.isEq` would change from the current world (using the functionless constraint with the ' syntax) to an Abilities world:

**Today:**

```coffee
Bool.isEq : 'val, 'val -> Bool
```

**Abilities:**

```coffee
Bool.isEq : val, val -> Bool
    where val has Eq
```

Similarly, a hash map collection could have:

```coffee
Dict.insert : k, v, Dict k v -> Dict k v
    where k has Hash
```

If Hash doesn't require `Eq` for some reason (although it probably should), then `Dict.insert` could require multiple abilities as part of the annotation, e.g. `where k has Hash, Eq`

In the Abilities world, Roc no longer needs the concept of the _functionless_ constraint, and it can be removed from the language. Abilities can cover all those use cases.

### Default Abilities

One of the many things I like about Elm is that I can make anonymous records and tuples have them Just Work with the `==` operator. In contrast, in Rust I have to name the struct and then add `#[deriving(Eq)]` to it if I want `==` to work on it.

However, in Rust, tuples work basically like how they do in Elm: equality Just Works as long as all the elements in the tuple have `Eq`. In fact, Rust tuples automatically derive a bunch of traits. We can do something similar in Roc.

Specifically, the idea would be to have all records and tags automatically have the following abilities by default, wherever possible. (For example, types that contain functions wouldn't get these abilities, because these operations are unsupported for functions!)

1. Eq
2. Hash
3. Sort
4. Encode
5. Decode

Eq and Hash work like they do in Rust, although as previously noted, I think Hash should probably require Eq. Sort is like Ord in Rust, although I prefer the name Sort because I think it should only be for sorting and not general ordering (e.g. I think the <, >, <=, and >= operators should continue to only accept numbers, not other sortable types like strings and booleans).

As for Encode and Decode...to put it mildly, they are exciting.

### Encode and Decode

[serde](https://docs.serde.rs/serde/) is among the most widely used Rust crates in the world - maybe the absolute most. It's for **ser**ializing and **de**serializing; hence, **serde**.

The way it works is that it provides `Serializable` and `Deserializable` traits that you can derive for your types (e.g. for your User type), as well as `Serializer` and `Deserializer` traits that anyone can define for their encoding formats (e.g. a JSON serializer).

[Putting these together](https://github.com/serde-rs/json#parsing-json-as-strongly-typed-data-structures), I can add `#[deriving(Serialiable, Deserializable)]` to my struct User definition, and then run something like `let user: User = serde_json::from_str(json)?` to turn my JSON into a User while handling failed decoding along the way via a `Result`.

Having spent a lot of time teaching JSON decoders to beginning Elm programmers, I can confidently say this seems massively easier for beginners to learn - even if it means they will abruptly have a lot to learn on the day where they want to do some more advanced decoding. It's also much more concise.

In the Abilities world, we can take this a step further than Rust does. We can have `Encode` and `Decode` as builtin abilities (and then also `Encoder` and `Decoder`, except they work like Serializers and Deserializers do in serde; you have an Encoder or Decoder for a particular encoding - e.g. JSON or XML - rather than for the value you want to encode or decode), and we can have the compiler automatically define them when possible, just like it does for `Eq` and the others.

This would mean that in Roc you could do, without any setup other than importing a package to get a generic **Json.decoder**, the following:

```coffee
result : Result User [ JsonDecodingErr ]*
result = Decode.decode Json.decoder jsonStr
```

So it would be like serde in Rust, except that - like with Elm records - you wouldn't even need to mark your User as deriving Encode and Decode; those abilities would already be there by default, just like `Eq`, `Hash`, and `Sort`.

This would solve [Problem #3](#problem-3-decoders-are-still-hard-to-learn), eliminating the need for a beginner curriculum to include the one technique I've seen beginning Elm programmers struggle the most to learn. That's a very big deal to me! I don't know whether decoding serialized data will be as common in Roc as it is in Elm, but I certainly expect it to come up often.

Other nice things about this design:

-   Since Encode and Decode are builtins, no packages need to depend on anything to make use of them. In Rust, it's currently a bit awkward that all packages that want to offer serializability have to depend on serde; it has become a nearly ubiquitous dependency in the Cargo ecosystem. By making it a builtin, Roc can avoid that problem.
-   Since Encode and Decode are agnostic to the actual encoding format, anyone can write a new Encoder and Decoder for whatever their new format is (e.g. XSON, the format that looks at XML and JSON and says "why not both?" - which I just made up) and have every serializable Roc type across the entire ecosystem instantly able to be serialized to/from that format.
-   This design still allows for evolving a default decoder into a bespoke decoder that can cover the same use cases that elm/json does (and a potentially very similar API).

I haven't looked into the details of what the exact design of this system would be, but at a glance it seems like based on the design of abilities and the design of serde, it should work out. (There may always be unexpected issues though!)

## Adding Abilities to a Type

So we've talked about default abilities, and how various builtins would use them. What about custom types? How would I make an actual `Dict` type with its own definition of equality?

To do that, we need to talk about a change to the language that was originally motivated by abilities, but which ultimately seems like a good change even if abilities weren't a thing.

### Newtypes

Let's suppose Roc no longer has private tags, but does have this syntax:

```coffee
UserId := U64
```

This declares a new concrete type in scope called `UserId`, which at runtime is a `U64` with no additional overhead.

To create one of these `UserId` values, we put a @ before the type and call it:

```coffee
userId : UserId
userId = @UserId 0
```

The expression `@UserId` has the type `U64 -> UserId`, which the compiler knows because this declaration is in scope:

```coffee
UserId := U64
```

Trying to use `@UserId` when a `UserId :=` declaration isn't in scope would give a compiler error.

`@UserId` can also be used in a pattern, to destructure the wrapped `U64`:

```coffee
getU64 : UserId -> U64
getU64 = \@UserId u64 -> u64
```

In this way, `@UserId` can be used almost identically to how private tags work today: call (`@UserId someU64`) to create a wrapped `U64`, and pattern match on `\@UserId someU64 ->` to destructure it. The only difference is that the resulting type is `UserId` instead of `[ @UserId ]`.

Because the `@` prefix syntax can only refer to a newtype declaration that's currently in scope, the newtype's implementation is hidden from other modules by default. (Of course you can still expose the type and functions to work on it.)

This design has a few advantages over private tags:

1. It's more focused. Wrapper types with hidden implementations are really the exact use case that private tags were designed for; the concept of a union of multiple private tags was never really necessary, and in this world it doesn't even exist.
2. It means there's just one "tags" concept, just like there's one "records" concept. No more "global tags and private tags" split.
3. The `UserId := U64` declaration is more concise than the private tag equivalent of `UserId : [ @UserId U64 ]`, and it speeds up type checking because there would be (many) fewer type aliases for the compiler to resolve.
4. It enables traditional phantom types, which Roc currently lacks - e.g.
   `Quantity count units := count`
   in Roc would make units a phantom type like in this Elm declaration:
   `type Quantity count units = Quantity count`

Even considered completely separately from Abilities, this "newtypes" design seems like a better design than private tags.

### Newtypes and Abilities

Another advantage the newtypes design has over private tags is that it offers a natural place to declare what abilities a type has.

With private tags, this isn't really possible because I can use @Foo in multiple different places in the same module, with multiple different payload arities and types - and even if I use a type alias to give it a shorter name, that type alias is still just an alias; it can't alter the characteristics of the type it's referring to. With the newtypes design, I can refer to a specific concrete type, and not just an alias of it - meaning I actually can alter its characteristics.

As an example, let's make a newtype declaration for Dict, and once again ignore the internal structure of the storage field for now:

```coffee
Dict k v := { storage : … }
```

This lets us construct a Dict by calling `@Dict { storage }` and destructure it similarly.

As discussed earlier, one problem with creating custom data structures like this in today's Roc is that `==` doesn't necessarily do the right thing. Here's a way to solve this issue:

```coffee
Dict k v := { storage : … } has
    [ Eq { isEq, isNotEq } ]
```

This says (among other things) that the `Dict` type has the `Eq` ability. For a type to have `Eq`, it must provide two functions: `isEq` and `isNotEq`. Here's how those look:

```coffee
isEq : Dict k v, Dict k v -> Bool
isNotEq : Dict k v, Dict k v -> Bool
```

In this `Eq { isEq, isNotEq }` declaration, I'm saying that `isEq` and `isNotEq` are functions already in scope. I could also choose different names using record literal syntax, e.g. `Eq { isEq: dictIsEq, isNotEq: dictIsNotEq }` - the relevant part is that I'm specifying the names of the functions (which must also be in scope) which specify how `Eq` for Dict should work.

Now that I've specified this, when I use `==` on two `Dict` values, this `isEq` function will get run instead of the default `==` implementation. This solves [Problem #3](#problem-3-decoders-are-still-hard-to-learn)!
I can also write something like has `Num` and provide the relevant functions to obtain a unit-ful number type, which solves [Problem #2](#problem-2-custom-number-types-cant-use-arithmetic-operators).

### Default Abilities for Newtypes

By default, if I don't use the has keyword when defining a newtype, Roc will give the type all the default builtin abilities it's eligible to have - so for example, it would get `Eq` and `Hash` by default unless it contains a function, in which case it's not eligible.

In this example, because I wrote has, the `Dict` type has `Eq` as well as the other default ones. I could instead use has `only`, which means `Dict` should not have any of the default abilities, and should instead have only the ones I list.

```coffee
Dict k v := { storage : … } has
    [
        Eq { isEq, isNotEq },
        Hash { hash },
        Sort { compare },
        Foo { bar: baz }
    ]
```

Using `has` means if new default abilities are later added to the language, `Dict` will get them automatically. This may or may not be desirable, depending on what the ability is; maybe, like equality, it will be wrong by default for `Dict`, and maybe I'll wish I had chosen has `only`.

On the other hand, if everyone uses has `only` everywhere as a precaution, and a new default ability gets added to the language, a staggering amount of collective hours would be spent going around adding it to all the has `only` declarations for `UserId` and such. So a good guideline might be for custom collections like `Dict` to recommend using has `only`, and for thin wrappers like `UserId` to use `has custom`.

Of note, this syntax neatly solves [Problem #5](#problem-5-how-to-specify-functionlessness-in-documentation) - where functionlessness is awkward to talk about in API type diffs and documentation. This is a straightforward way to render the `Dict` type in documentation:

```coffee
Dict k v has only
    [ Eq, Hash, Sort ]
```

I can immediately see exactly what abilities this type has. The same is true if I used has `custom` or omitted the has clause entirely. API diffs can use this same representation, with a diff like +Eq -Sort to show which abilities were added or removed.

### Encode and Hash

I'm not sure if we actually need separate Hash and Encode abilities. At a high level, hashing is essentially encoding a value as an integer. Since all default types will get Encode anyway, maybe all we need is to have "hashers" be implemented as Encoders. This would mean there's one less default ability in the mix, which would be a nice simplification.

However, I'm not sure what the differences are between Rust's Hash trait and Hasher type, and serde's Serializable trait and Serializer types. Maybe there's a relevant difference that would justify having a separate Hash ability. I'm not sure! I figure it's at least worth exploring.

It might look surprising at first for a `Dict` implemented as a hash map to require that its keys have `Encode`, but I don't think that's a significant downside.

### Encode and toStr

Similarly, anyone could write a `toStr` function that works on any type that has `Encode`, by using an Encoder which encodes strings.

In Elm, having a general toString function proved error-prone (because it was so flexible it masked type mismatches - at work I saw this cause a production bug!) which was why it was replaced by String.fromInt and String.fromFloat. I had originally planned to do the same in Roc, but Encode would mean that anyone can write a flexible toStr and publish it as a package without acknowledging the potential for masking bugs.

Knowing that there's a 100% chance that would happen eventually, it seems like it would be better to just publish an Encode.str which encodes values as strings, and which can be used like toStr except you have to actually call (`Encode.encode Encode.str value`) instead of `toStr`. This would mean that although it's an option, it's (by design!) less ergonomic than a flexible function like Num.fromStr, which means the path of least resistance (and least error-proneness) is to use `Num.fromStr` instead of this.

One benefit to having something like `Encode.str` available in the language is that it can be nice for logging - e.g. when sending tracing information to a server that only programmers will ever see, not users. That's the only situation where I've ever personally missed the old Elm `toString`.

## Defining New Abilities

Here's how I might have defined Eq if it weren't already a builtin ability:

```coffee
Eq has { isEq, isNotEq }


isEq : val, val -> Bool where val has Eq


isNotEq : val, val -> Bool where val has Eq
```

There are two steps here:

1. Define what functions Eq has
2. Declare those functions as top-level type annotations with no bodies

Having done both, now if anyone wants to say that another type **has Eq**, that type needs to implement these two functions. I can also expose these functions from this module directly - so for example, if I'm in the Bool module, I can have it do `exposes [ isEq, isNotEq ]`, and now anyone can call `Bool.isEq` and it will run this function (or rather, the implementation of this function on whatever type that **has Eq** which was passed to `Bool.isEq`!)

Within these `isEq` and `isNotEq` functions' types, **has Eq** is allowed even though those functions are part of the definition of what `Eq` means. The compiler detects these and treats them essentially like "Self" in Rust - that is, when I say that my `Dict k v` newtype **has Eq**, its `isEq` implementation will have the type` Dict k v, Dict k v -> Bool` because the compiler will have replaced the val in the `Eq` definition with `Dict k v`.

The compiler knew to do that substitution with **val** because of **val has Eq** in the declaration of `isEq` itself. If `isEq` also had other abilities in its has clause, e.g. **val has Eq, foo has Sort**, it wouldn't do the substitutions with foo because **Sort** is not the name of the ability currently being defined.

For this reason, if you are defining a function on **Eq** (such as **isEq**), and you have more than one type variable which **has Eq**, the result is a compiler error. This would be like trying to have more than one `Self` in Rust!

### Abilities that depend on other abilities

I mentioned earlier that in order to have either Int or Frac, a type must also have the Num ability. You can add those constraints after the **has** keyword, like so:

```coffee
Int has Num, { …functions go here as normal… }
```

Now whenever someone wants to make a newtype which **has Int**, that newtype must also explicitly specify that it **has Num** - otherwise, they'll get a compiler error. Similarly, any function which requires that an argument **has Num** will also accept any type that **has Int**.

### Defining abilities for existing types after the fact

It's conceivable that defining a new ability could support adding that ability to existing types. For example, maybe I make a new ability called Foo, and I want all numbers to have Foo.

It's too late for me to go back and get Num's newtype declaration to specify has Foo, because Num existed before Foo did!

It's possible that Roc could support a way to do this when defining a new ability. It could say for example `Eq has {...} with [ Num { isEq: numIsEq, … } ]`

However, upon reflection, I think this idea is fatally flawed and we shouldn't do it.

On the positive side, this wouldn't introduce any ambiguity. Because Roc doesn't allow cyclic imports, it's already impossible to define two conflicting definitions for a given ability function (e.g. if I define isEq for numbers when defining Num, then Num must import the module where Eq is defined, meaning I can't possibly have Eq's definition mention Num - or else the module where Eq is defined would have had to import Num as well, creating an import cycle!) so that can't happen.

This also wouldn't necessarily introduce any "you need to import a trait for this to work" compiler errors like we see in Rust.

If I'm passing a newtype named Blah to a function which expects that it **has Baz**, then by virtue of the fact that I have a Blah at all, I must have the module where it's defined already loaded in the current build (maybe not as a direct dependency of my module, but definitely as an indirect dependency). Similarly, because I'm calling a function that **has Baz**, I must also (at least indirectly) have the module where Baz is defined loaded. If both modules are loaded, I will definitely be able to find the function implementation(s) I need in either the one or the other, and because Roc wouldn't support orphan instances, I don't need to check any other modules.

However, this can cause some serious problems. Once I've done this, now the module where the type is defined can never import the module where the ability is defined. What if the author of that module wants to define that a different type defined in that module has this ability? Tough luck; can't import the ability module, because that would create an import cycle. Gotta move that type out of that module, even if that would create other problems.

This is even worse if the type and the ability are in different packages; now your entire package can't even depend on the package where the ability is defined! What if the reason the author of the ability added it to that other type was just to avoid having to coordinate with the author of the other package (or just to save them some time)? Now they've locked that author out from controlling their own type!

From this point, even if both authors coordinate, the only way to permit the author of the type to take back control over the implementation of that ability on that type is if the ability author releases a breaking change of that package which drops the ability from the type - so that the author of the type can finally import it without causing a cyclic dependency. I want to incentivize strong backwards compatibility commitments for package authors once their APIs have settled, and this feature would make such commitments unworkable.

All of this makes me think that "if you want a type to have the ability you're defining, you should coordinate with that author" is the best policy to encourage, and in that world, the feature makes no sense except perhaps in the very specific case of builtin types (which necessarily can't depend on packages). Since there are a (small) finite number of those, it seems plausible that the ability author can do one-off special-case workarounds for those instead of needing a separate language feature.

### Abilities for Editor-Specific Code

I don't know exactly what the API for editor plugins should be yet, but they do have some characteristics that are important:

-   Making or modifying editor plugins should be so easy, basically everyone does it. This means that code for editor plugins should be written in normal Roc, and the API should have a shallow learning curve.
-   Editor plugins should ship with packages (or even just modules within a local project), but should have no impact on runtime performance of those modules/packages. So it's located there, but can't affect the surrounding code.
-   There's more than one way to integrate with the editor. For example:
-   You can add entries to context menus for certain types
-   You can override the default way a type would be rendered in the editor (e.g. an expando for a custom collection)
-   You can make big, interactive integrations like a [regex explorer](https://www.youtube.com/watch?v=ZnYa99QoznE&t=6105s)

Abilities offer a nice way to address all of these.

-   They can ship with modules and packages without affecting runtime performance. They describe a new ability for a type (namely, an editor integration for that type), but as long as no production code uses it, runtime performance is unaffected - they're just functions that never get called, and won't even be present in the final optimized binary.
-   Since abilities are used elsewhere in the language, there's nothing editor-specific to learn other than the APIs themselves (which is unavoidable), so the learning curve for how to add editor plugins is minimal: just declare that your newtype has a particular ability, and the editor will pick up on it.
-   Since any given type can have multiple abilities, the different ways to integrate with the editor can too. There can be one ability for adding context menu items, another for specifying how the type renders, etc.

In this way, abilities solve [problem #6](#problem-6-no-nice-way-to-specify-editor-specific-code).

### Avoiding the Classification Trap

Although I think custom user-defined abilities are worth having in the language because they address [Problem #7](#problem-7-record-of-function-passing), I hope they are used rarely in practice.

I chose the name "ability" rather than like Trait or Typeclass because I don't want to encourage _classification_ - that is, using the language feature to spend a bunch of time thinking about how to classify types by what they "are."

This seems to be a common exercise in statically typed languages with classes; see for example the well-known introductory example "`a Bicycle is a Vehicle`" which to me is primarily teaching students how to waste time adding complexity to their code bases for the satisfaction of classifying things, and no practical benefit.

(This happens in FP too; I doubt [Semiring](https://pursuit.purescript.org/packages/purescript-prelude/5.0.1/docs/Data.Semiring) ends up in a standard library because people kept opening issues saying they were unable to write some really valuable production code without it. A more likely history of that design decision is that a semiring is the mathematically proper way to `classify` those particular `types`, and `typeclasses` encourage classifying types right there in the name.)

In my view, type classification is a tempting but ultimately counterproductive exercise that puts a tax on a community which grows linearly with the size of that community: once enough people start doing it, everyone becomes under pressure to do the same, lest their code look suspiciously under-classified. I don't want this to happen in Roc.

Hopefully the name "abilities" will frame the feature as giving a type a new ability and nothing more. It's not about saying what the type _is_, but rather what you can do with it.
