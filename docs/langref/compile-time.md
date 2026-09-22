# Compile-Time Evaluation

When possible, Roc evaluates expressions at compile time instead of at runtime. Once they have been evaluated, their values are stored in the compiled binary as static constants.

## Which Expressions Get Evaluated?

### Top-level declarations

Top-level declarations in [modules](modules) all get evaluated at compile time. For example:

```roc
x = 1
y = 2
z = x + y

main! = |_args| {
    echo!("z is ${z.to_str()}")
}
```

Here, the top-level declarations are `x`, `y`, `z`, and the `main!` function. `x`, `y`, and `main!` are all literals ([number literals](numbers) for `x` and `y` and a [lambda](functions) literal for `main!`), so they evaluate to themselves.

`z` gets evaluated at compile time to `x + y`. So if you rewrote this code to have `z = 3` and ran the program, it would produce the same output at runtime, with the same performance.

### Top-level equivalent expressions

Any expression which could have been a top-level declaration, even if it isn't *actually* a top-level declaration, also gets evaluated at compile time.

For example, `z.to_str()` could have been a top-level declaration because it only references a top-level declaration (namely, `z`). As such, `z.to_str()` will also get evaluated at compile time to produce the string `"3"`.

The [string interpolation](strings#string-literal-conversion-and-interpolation) expression `"z is ${z.to_str()}"` also gets evaluated at compile time. It could have been a top-level declaration because it depends only on a [pure function](functions#pure-functions) (`to_str`) being called on a top-level value (`z`).

Putting all this together, the string `"z is 3"` will end up embedded in the static data section of this program's final binary. The values `1`, `2`, `3`, and `"z is "` (from the interpolation) will be dead-code eliminated and will not be stored in the binary.

### Effectful functions never run at compile time

You may recall that [effectful functions](functions#effectful-functions) may only be called from within other effectful functions. This implies that they can't be called from top-level declarations; the following top-level declaration would give an error:

```roc
foo = echo!("hi")
```

This means effectful functions never run at compile time. So in the following expression:

```roc
echo!("z is ${z.to_str()}")
```

The entire string gets evaluated at compile time, but the `echo!` itself only gets evaluated at runtime.

To summarize, here's what happens to each part of that expression:

- `z` gets evaluated at compile time.
- `z.to_str()` also gets evaluated at compile time.
- The string interpolation also gets evaluated at compile time.
- The call to `echo!` does not get evaluated at compile time; it runs at runtime, using the string that was already computed at compile time.

### Terminology

Compile-time evaluation is sometimes called _comptime_, as in "this runs at comptime." In the literature,
it's often called [compile-time function execution](https://en.wikipedia.org/wiki/Compile-time_function_execution),
or CTFE for short.

### Empty builtin collections never get stored

[`List.with_capacity`](../List#with_capacity) is a [pure function](functions#pure-functions) that returns a list with zero length and nonzero capacity. However, storing that empty list in the static binary would defeat the entire purpose of `List.with_capacity`! In general, storing empty [lists](../List) (or empty [strings](strings), [dictionaries, or sets](dictionaries-and-sets)) in the static binary is pointless at best and counterproductive at worst, so Roc doesn't do it. Empty builtin collections never get stored.

An eligible call like `List.with_capacity(123)` will still be evaluated at compile time, it's just that nothing will be stored in the static data section of the binary. Instead, the resulting list (no items, length zero, capacity 123) will evaluate at runtime to a call to `List.with_capacity(123)`. A call to `List.with_capacity(0)` will be equivalent to the empty list literal (`[]`), and will not result in a function call at runtime.

Note that it's not `List.with_capacity` itself that's special-cased; rather, it's the empty builtin collections. You can still build nonempty lists at compile time by calling functions which use `List.with_capacity`; storing it in the binary is only skipped if its length is actually zero at the end of compile-time evaluation.

## Performance

Doing work at compile time instead of runtime ordinarily makes a program run faster because it does less work.

That said, compile-time evaluation can result in larger binaries, which can negatively impact runtime performance. For example, consider this function:

```roc
make_fives = |count| List.repeat(5, count)
```

If you call this at runtime with a large number, it can make a big list at runtime which takes up a lot of memory. If you only do this under rare circumstances, it rarely needs to take up that memory.

In contrast, if you do it at compile time, and the big list ends up in the program's static binary data, then when the binary gets loaded into memory to run the program, the memory is always being taken up even if that code path never gets run.

- trick: both branches of an [`if`](if-else) that depends on args, gets optimized away prob hopefully haha

## Uses

Compile-time evaluation makes it practical to do expensive setup work once, while the program is being compiled,
instead of every time the program runs.

[Parsers](parsers) are a good example. A parser built with [`parser_for`](static-dispatch#parsing-and-encoding)
(for example, by calling `Json.parser_camel()`) is assembled specifically for the type being parsed. When that
parser is a top-level constant, all the work of assembling it happens at compile time, and the compiled program
only contains the finished parser. See [Parsers at Compile Time](parsers#parsers-at-compile-time).

Compile-time evaluation also combines well with [importing non-Roc files](statements#importing-non-roc-files).
For example, this parses a JSON configuration file at compile time:

```roc
import "config.json" as config_text : Str

config : Try({ port : U16, host : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
config = Json.parse(config_text)
```

Since `config` is a top-level constant, the program contains the already-parsed configuration, and neither
reads nor parses `config.json` at runtime.

Other common uses include precomputing lookup tables, and building data structures (such as
[dictionaries](dictionaries-and-sets)) whose contents are known in advance.
