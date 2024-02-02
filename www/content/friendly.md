# Friendly

Besides having a [friendly community](/community), Roc also prioritizes being a user-friendly language. This impacts the syntax, semantics, and tools Roc ships with.

## [Syntax and Formatter](#syntax) {#syntax}

Roc's syntax isn't trivial, but there also isn't much of it to learn. It's designed to be uncluttered and unambiguous. A goal is that you can normally look at a piece of code and quickly get an accurate mental model of what it means, without having to think through several layers of indirection. Here are some examples:

- `user.email` always accesses the `email` field of a record named `user`. <span class="nowrap">(Roc has</span> no inheritance, subclassing, or proxying.)
- `Email.isValid` always refers to something named `isValid` exported by a module named `Email`. (Module names are always capitalized, and variables/constants never are.) Modules are always defined statically and can't be modified at runtime; there's no [monkey patching](https://en.wikipedia.org/wiki/Monkey_patch) to consider either.
- `x = doSomething y z` always declares a new constant `x` (Roc has [no mutable variables, reassignment, or shadowing](/functional)) to be whatever the `doSomething` function returns when passed the arguments `y` and `z`. (Function calls in Roc don't need parentheses or commas.)
- `"Name: $(Str.trim name)"` uses *string interpolation* syntax: a dollar sign inside a string literal, followed by an expression in parentheses.

Roc also ships with a source code formatter that helps you maintain a consistent style with little effort. The `roc format` command neatly formats your source code according to a common style, and it's designed with the time-saving feature of having no configuration options. This feature saves teams all the time they would otherwise spend debating which stylistic tweaks to settle on!

## [Helpful compiler](#helpful-compiler) {#helpful-compiler}

Roc's compiler is designed to help you out. It does complete type inference across all your code, and the type system is [sound](https://en.wikipedia.org/wiki/Type_safety). This means you'll never get a runtime type mismatch if everything type-checked (including null exceptions; Roc doesn't have the [billion-dollar mistake](https://en.wikipedia.org/wiki/Null_pointer#History)), and you also don't have to write any type annotations—the compiler can infer all the types in your program.

If there's a problem at compile time, the compiler is designed to report it in a helpful way. Here's an example:

<pre><samp class="code-snippet"><span class="literal">── TYPE MISMATCH ─────── /home/my-roc-project/main.roc ─</span>

Something is off with the <span class="literal">then</span> branch of this <span class="literal">if</span>:

<span class="literal">4│</span>      someInt : I64
<span class="literal">5│</span>      someInt =
<span class="literal">6│</span>          if someDecimal > 0 then
<span class="literal">7│</span>              someDecimal + 1
                <span class="error">^^^^^^^^^^^^^^^</span>

This branch is a fraction of type:

    <span class="literal">Dec</span>

But the type annotation on `someInt` says it should be:

    <span class="literal">I64</span>

<span class="literal">Tip:</span> You can convert between integers and fractions
using functions like `Num.toFrac` and `Num.round`.</samp></pre>

If you like, you can run a program that has compile-time errors like this. (If the program reaches the error at runtime, it will crash.)

This lets you do things like trying out code that's only partially finished, or running tests for one part of your code base while other parts have compile errors. (Note that this feature is only partially completed, and often errors out; it has a ways to go before it works for all compile errors!)

## [Serialization inference](#serialization-inference) {#serialization-inference}

When dealing with [serialized data](https://en.wikipedia.org/wiki/Serialization), an important question is how and when that data will be decoded from a binary format (such as network packets or bytes on disk) into your program's data structures in memory.

A technique used in some popular languages today is to decode without validation. For example, some languages parse [JSON](https://www.json.org) using a function whose return type is unchecked at compile time (commonly called an `any` type). This technique has a low up-front cost, because it does not require specifying the expected shape of the JSON data.

Unfortunately, if there's any mismatch between the way that returned value ends up being used and the runtime shape of the JSON, it can result in errors that are time-consuming to debug because they are distant from (and may appear unrelated to) the JSON decoding where the problem originated. Since Roc has a [sound type system](https://en.wikipedia.org/wiki/Type_safety), it does not have an `any` type, and cannot support this technique.

Another technique is to validate the serialized data against a schema specified at compile time, and give an error during decoding if the data doesn't match this schema. Serialization formats like [protocol buffers](https://protobuf.dev/) require this approach, but some languages encourage (or require) doing it for _all_ serialized data formats, which prevents decoding errors from propagating throughout the program and causing distant errors. Roc supports and encourages using this technique.

In addition to this, Roc also supports serialization _inference_. It has some characteristics of both other approaches:
- Like the first technique, it does not require specifying a schema up front.
- Like the second technique, it reports any errors immediately during decoding rather than letting the problems propagate through the program.

This technique works by using Roc's type inference to infer the expected shape of serialized data based on how it's used in your program. Here's an example, using [`Decode.fromBytes`](https://www.roc-lang.org/builtins/Decode#fromBytes) to decode some JSON:

<pre><samp class="code-snippet"><span class="kw">when</span> Decode<span class="punctuation section">.</span>fromBytes data Json<span class="punctuation section">.</span>codec <span class="kw">is</span>
    <span class="literal">Ok</span> decoded <span class="kw">-></span> <span class="comment"># (use the decoded data here)</span>
    <span class="literal">Err</span> err <span class="kw">-></span> <span class="comment"># handle the decoding failure</span></samp></pre>

In this example, whether the `Ok` or `Err` branch gets taken at runtime is determined by the way the `decoded` value is used in the source code.

For example, if `decoded` is used like a record with a `username` field and an `email` field, both of which are strings, then this will fail at runtime if the JSON doesn't have fields with those names and those types. No type annotations are needed for this; it relies entirely on Roc's type inference, which by design can correctly infer types for your entire program even without annotations.

Serialization inference has a low up-front cost in the same way that the decode-without-validating technique does, but it doesn't have the downside of decoding failures propagating throughout your program to cause distant errors at runtime. (It also works for encoding; there is an [Encode.toBytes](https://www.roc-lang.org/builtins/Encode#toBytes) function which encodes similarly to how [`Decode.fromBytes`](https://www.roc-lang.org/builtins/Decode#fromBytes) decodes.)

Explicitly writing out a schema has its own benefits that can balance out the extra up-front time investment, but having both techniques available means you can choose whatever will work best for you in a given scenario.

## [Testing](#testing) {#testing}

The `roc test` command runs a Roc program's tests. Each test is declared with the `expect` keyword, and can be as short as one line. For example, this is a complete test:

```roc
## One plus one should equal two.
expect 1 + 1 == 2
```

If the test fails, `roc test` will show you the source code of the `expect`, along with the values of any named variables inside it, so you don't have to separately check what they were.

If you write a documentation comment right before it (like `## One plus one should equal two` here), it will appear in the test output, so you can use that to add some descriptive context to the test if you want to.

## [Inline expectations](#inline-expect) {#inline-expect}

You can also use `expect` in the middle of functions. This lets you verify assumptions that can't reasonably be encoded in types, but which can be checked at runtime. Similarly to [assertions](https://en.wikipedia.org/wiki/Assertion_(software_development)) in other languages, these will run not only during normal program execution, but also during your tests—and they will fail the test if any of them fails.

Unlike assertions (and unlike the `crash` keyword), failed `expect`s do not halt the program; instead, the failure will be reported and the program will continue. This means all `expect`s can be safely removed during `--optimize` builds without affecting program behavior—and so `--optimize` does remove them. This means you can add inline `expect`s without having to weigh each one's helpfulness against the performance cost of its runtime check, because they won't have any runtime cost after `--optimize` removes them.

In the future, there are plans to add built-in support for [benchmarking](https://en.wikipedia.org/wiki/Benchmark_(computing)), [generative tests](https://en.wikipedia.org/wiki/Software_testing#Property_testing), [snapshot tests](https://en.wikipedia.org/wiki/Software_testing#Output_comparison_testing), simulated I/O (so you don't have to actually run the real I/O operations, but also don't have to change your code to accommodate the tests), and "reproduction replays"—tests generated from a recording of what actually happened during a particular run of your program, which deterministically simulate all the I/O that happened.

## Functional

Besides being designed to be [fast](/fast) and friendly, Roc is also a functional programming language.

[What does _functional_ mean here?](/functional)
