
# Debugging using `dbg`

Today, the only way to do "printline debugging" in a Roc function that doesn't return a Task are is to use the expect x != x trick. In contrast, Elm has [Debug.log](https://package.elm-lang.org/packages/elm/core/latest/Debug#log), which works like Rust's [dbg!](https://doc.rust-lang.org/std/macro.dbg.html) macro. Both of them do this:

1. Accept a string and a value.
2. Print the string and the value to the console.
3. Return the value.

Rust's dbg! additionally prints the source code line number, which I've found helpful when debugging.

## Proposed Design

We introduce a new language keyword, `dbg`. (Not `debug` because that's more likely to be an identifier people want to use in userspace. As a bonus, it's shorter.) It would look like this:

<pre><samp>myFunction = \arg ->
    # this prints the value of arg to stderr:
    dbg arg

    arg + 1
</pre></samp>

This works like inline expect in several ways:

1. You write dbg on its own line, wherever you'd normally be able to write a def.
2. Syntactically it looks like you're "calling" it passing two parameters, even though it's not actually a function.
3. It only does anything in roc dev and roc test; roc build discards it.
4. Whenever it prints something, it includes source code information. However, unlike expect - which actually shows source code - this would be a much more concise filename + line/col number (like Rust's dbg! does) because there could be quite a lot of these being printed!
5. It works by having the application communicate to the parent roc process that's running in roc dev or roc test, which in turn does the writing to stderr. Since it's the roc process that's writing to stderr, the platform (running in its child process) can't access this stderr stream, and therefore can't even accidentally rely on it or alter it.

Additionally, if there are any dbg calls anywhere in your code base, you'll get a warning which lists the source code locations of all the dbg calls. Since warnings mean roc exits with a nonzero status code, this means you can't forget to clean up your dbg calls before merging your changes; CI will fail if you left any of them in there. This also makes it extra clear that the feature is only intended to be used for debugging purposes during development, and not for production use.

## Design Notes

It's possible that in the future we should allow calling dbg inline (like Elm's Debug.log or Rust's dbg! macro), where it prints the value you give it and then returns it. However, I'd rather start with this design and then consider that (if there's interest in it) as a separate addition.

I know there's demand for this style because in Elm it was a common technique to write:

<pre><samp>_ = Debug.log …
</pre></samp>

…as a way to get around the fact that it didn't have a way to call it on its own line. So since I know for sure there's demand for that style, it makes sense to me to start with that and then see if there's demand for more.

Another consideration is whether dbg should take multiple arguments. In other words, whether this should be allowed:

<pre><samp>dbg "arg was:" arg
</pre></samp>

This has some downsides. For example, what happens if you just want to print a string like the classic "HERE" or something like that? Do you now need to do this?

<pre><samp>dbg "HERE!" {}
</pre></samp>

That's annoying, and this is a very common use case. One possible design is that the second "argument" could be optional (since this isn't actually a function call even though it looks like one; the rules can be different), but if so, then what should this do?

<pre><samp>dbg foo bar
</pre></samp>

If dbg always takes 2 expressions, it's unambiguous what this does. Also if dbg always takes 1 expression, it's unambiguous what this does: it's the same as `dbg (foo bar)`. However, if dbg takes one optional argument, then it has to mean the first thing; we can't allow dbg foo bar to be equivalent to dbg (foo bar).

That's a potential future downside in case we want to allow inline dbg. Now wrapping a call to Str.isEmpty blah has to be (dbg (Str.isEmpty blah)) when it could have been (dbg Str.isEmpty blah). With syntax highlighting, it's clear that this is a language keyword and not a function call, so being able to skip the parens would be nice here. An optional second argument would make that design impossible.

Another consideration is what an optional second dbg argument would mean for expect. In that world, what should this do?

<pre><samp>expect foo bar
</pre></samp>

Today, there's no reason this couldn't be equivalent to expect (foo bar). However, if dbg doesn't work that way, and expect does work that way, it's predictable that this would feel like a surprising inconsistency.

So another nice thing about dbg always taking one expression is that expect also takes one expression, so their syntactic rules can be completely consistent.

However, one-expression dbg is still annoying in this case:

<pre><samp>dbg "arg was:" arg
</pre></samp>

However, tuples offer a quick fix:

<pre><samp>dbg ("arg was:", arg)
</pre></samp>

Also, tuples mean you can have as many of these as you want:

<pre><samp>dbg ("arg1 was:", arg1, " and arg2 was:", arg2)
</pre></samp>

This feels to me like an acceptable price to pay for the simplicity, consistency, and syntactic flexibility of having dbg take one expression.
