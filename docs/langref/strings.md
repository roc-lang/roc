# Strings

Roc strings are designed to represent text. For example, `"Hi!"` is a string.

If you're interested in the low-level details of how they're stored in memory, you can skip ahead
to [UTF-8](#utf-8) and [Performance](#performance), but most readers will benefit more from
starting at a high level.

## String Literals

A string literal is text between double quotes, like `"Hello!"`. Inside the quotes, a backslash
starts an _escape sequence_, which is a way to write a character that would otherwise be
difficult or impossible to put there:

| Escape | Meaning |
| --- | --- |
| `\n` | newline |
| `\r` | carriage return |
| `\t` | tab |
| `\"` | `"` |
| `\'` | `'` |
| `\\` | `\` |
| `\$` | `$` (so you can write `\${` without starting an interpolation) |
| `\u(e9)` | the Unicode code point with the given hexadecimal number (here, `é`) |

Any other character after a backslash gives a compile-time error.

### Interpolation

Writing `${…}` inside a string literal inserts the value of the expression between the braces:

```roc
greeting = "Hello, ${name}!"
```

The value has to be a `Str`. To insert something else, like a number, convert it to a string
first: `"You have ${count.to_str()} messages."`

### Multiline Strings

For text that spans several lines, start each line with `\\`:

```roc
poem =
    \\Roses are red,
    \\Violets are blue,
    \\${name} wrote this
    \\Just for you.
```

Each `\\` line becomes one line of the string, with newlines in between (but no newline at the
end). Everything after the `\\` is part of the string, including any leading spaces, so the
indentation before the `\\` doesn't affect the string's contents. Interpolation works in
multiline strings too.

## Unicode

Roc strings represent text using [Unicode](https://unicode.org), which
is a deep topic. (The [Unicode glossary](http://www.unicode.org/glossary/) has over 500 entries in it!)

This guide will provide a basic overview of Unicode, including the relevant differences between these concepts:

* Code points
* Graphemes
* UTF-8

It will also explain why some operations are included in Roc's builtin [Str](../Str)
module, and why others are in separate packages like [roc-lang/unicode](https://github.com/roc-lang/unicode).

## Graphemes

Let's start with the following string:

"👩‍👩‍👦‍👦"

Some might call this a "character" because that's what they perceive it to be. And in a monospace font, it looks to be about the same width as the letter "A" or the punctuation mark "!"—both of which could also reasonably be called a "character." Unfortunately, the term "character" in programming has changed meanings many times across the years and across programming languages, and today it's become more confusing than helpful.

Unicode uses the less ambiguous term [*grapheme*](https://www.unicode.org/glossary/#grapheme), which it defines as a "user-perceived character" (as opposed to one of the several historical ways the term "character" has been used in programming) or, alternatively, "A minimally distinctive unit of writing in the context of a particular writing system." By Unicode's definition, each of the following is an individual grapheme:

* `a`
* `鹏`
* `👩‍👩‍👦‍👦`

Note that although *grapheme* is less ambiguous than *character*, its definition is still somewhat open to interpretation. To address this, Unicode has formally specified [text segmentation rules](https://www.unicode.org/reports/tr29/) which define grapheme boundaries in precise technical terms. We won't get into those rules here.

## Code Points

Every Unicode text value can be broken down into [Unicode code points](http://www.unicode.org/glossary/#code_point), which are integers that describe different components of the text. In memory, every Roc string is a sequence of these integers stored in a format called UTF-8, which will be discussed [later](#utf-8).

The string `"👩‍👩‍👦‍👦"` happens to be made up of these code points:

```
[128105, 8205, 128105, 8205, 128102, 8205, 128102]
```

From this we can see that:

-   One grapheme can be made up of multiple code points. In fact, there is no upper limit on how many code points can go into a single grapheme!
-   Sometimes code points repeat within an individual grapheme. Here, 128105 repeats twice, as does 128102, and there's an 8205 in between each of the other code points.

## Combining Code Points

The reason every other code point in this string is 8205 is that 8205 is a code point which joins together other code points. This emoji, ["Family: Woman, Woman, Boy, Boy"](https://emojipedia.org/family-woman-woman-boy-boy) is made by combining several emoji using [zero-width joiners](https://emojipedia.org/zero-width-joiner) like code point 8205.

Here are those code points again, this time with the strings they correspond to:

```
[128105] # "👩"
[8205]   # (joiner)
[128105] # "👩"
[8205]   # (joiner)
[128102] # "👦"
[8205]   # (joiner)
[128102] # "👦"
```

One way to read this is "woman emoji joined to woman emoji joined to boy emoji joined to boy emoji." Without the joins, it would be:

"👩👩👦👦"

With the joins, however, it is instead:

"👩‍👩‍👦‍👦"

Even though it looks smaller when rendered, the second string takes up almost twice as much memory as the first one! That's because it has all the same code points as the first one…except with the addition of the zero-width joiners in between them.

## Single quote syntax

Try putting `'👩'` into `roc repl`. You should see this:

```
» '👩'

128105.0
```

The single-quote `'` syntax lets you put a grapheme directly into your source code (so you can see what it looks like) as long as that grapheme contains only one code point, like 👩 does (namely, the code point 128105). At runtime, the single-quoted value will be treated the same as an ordinary number literal—in other words, `'👩'` is syntax sugar for writing `128105`. (The repl displays it as `128105.0` because number literals [default to `Dec`](numbers#defaulting-to-dec) when nothing pins them to a particular number type.)

You can verify this in `roc repl`:

```
» '👩' == 128105

True
```

Double quotes (`"`), on the other hand, are not type-compatible with integers—not only because strings can be empty (`""` is valid, but `''` is not) but also because there may be more than one code point involved in any given string!

## String Literals for Other Types

String literals are usually `Str` values, but other types can opt into being written as string
literals, the same way [custom number types](numbers#custom-number-types) can opt into number
literals. A type that has a [`from_quote`](static-dispatch#literal-conversion) method can be
written as a plain string literal (like `"/users"`) anywhere that type is expected, and a type
with a [`from_interpolation`](static-dispatch#literal-conversion) method can be written as a
string literal with interpolations in it (like `"/users/${id}"`).

## String equality and normalization

Besides emoji like 👩‍👩‍👦‍👦, another classic example of code points combining to render as one grapheme has to do with accent marks. Try putting these two different strings into `roc repl`:

```
"caf\u(e9)"
"cafe\u(301)"
```

The `\u(e9)` syntax is a way of inserting code points into string literals. In this case, it's the same as inserting the hexadecimal number `0xe9` as a code point onto the end of the string `"caf"`. Since Unicode code point `0xe9` happens to be `é`, the string `"caf\u(e9)"` ends up being identical in memory to the string `"café"`.

We can verify this by putting the following into `roc repl`:

```
» "caf\u(e9)" == "café"

True
```

As it turns out, `"cafe\u(301)"` is another way to represent the same word. The Unicode code point 0x301 represents a ["combining acute accent"](https://unicodeplus.com/U+0301)—which essentially means that it will add an accent mark to whatever came before it. In this case, since `"cafe\u(301)"` has an `e` before the `"\u(301)"`, that `e` ends up with an accent mark on it and becomes `é`.

Although these two strings get rendered identically to one another, they are different in memory because their code points are different! We can also confirm this in `roc repl`:

```
» "caf\u(e9)" == "cafe\u(301)"

False
```

This can be a source of bugs! One way to prevent this problem is to perform string normalization.

## String normalization

_Normalization_ converts a string into a standard form, so that strings which render identically also
have identical code points. The Unicode standard defines several normalization forms, the most common
of which are:

- **NFC** (Normalization Form C, for "Composed"), which uses a single code point wherever possible. In NFC, `"cafe\u(301)"` becomes `"caf\u(e9)"`.
- **NFD** (Normalization Form D, for "Decomposed"), which splits characters into multiple code points wherever possible. In NFD, `"caf\u(e9)"` becomes `"cafe\u(301)"`.

After two strings have both been normalized to the same form, comparing them with `==` gives the answer
you'd expect based on how they're rendered.

A good time to normalize strings is when they first enter the program, such as when reading user input or
parsing a file. That way, the rest of the program can compare them using `==` without having to worry about
the same text being represented using different code points.

Roc's builtin `Str` module does not perform normalization.

## Why not normalize automatically

It would be possible for Roc to perform string normalization automatically on every equality check, in order to prevent bugs like this. Unfortunately, normalization takes significantly more CPU time than equality comparisons do, which means it's much more efficient to perform normalization once and then fast equality checks from then on. This is the design that Roc encourages.

## UTF-8

Roc strings are stored in memory as [UTF-8](https://en.wikipedia.org/wiki/UTF-8), which encodes each
code point as a sequence of between one and four bytes. ASCII characters like `a` take up one byte each,
whereas other code points take up more. For example, `é` (`\u(e9)`) takes two bytes, and many emoji take four.

Every `Str` is guaranteed to be valid UTF-8. For this reason, converting a `Str` to bytes always succeeds,
whereas converting bytes to a `Str` can fail:

```roc
"café".to_utf8() # [99, 97, 102, 195, 169]

Str.from_utf8([99, 97, 102, 195, 169]) # Ok("café")

Str.from_utf8([99, 255]) # Err(BadUtf8({ index: 1, problem: InvalidStartByte }))
```

If you'd rather replace invalid bytes than get an error, [`Str.from_utf8_lossy`](../Str#from_utf8_lossy)
replaces each invalid byte sequence with the Unicode replacement character (`�`).

Since code points can take up different numbers of bytes, the number of bytes in a string isn't necessarily
the number of code points, let alone the number of graphemes. This is why `Str.len` doesn't return a number;
instead, it returns a `LearnAboutStringsInRoc` tag containing an explanation of this. To get the number of bytes,
use [`Str.count_utf8_bytes`](../Str#count_utf8_bytes), which returns exactly what its name says:

```roc
"café".count_utf8_bytes() # 5, because é takes 2 bytes
```

(To check whether a string is empty, use [`Str.is_empty`](../Str#is_empty).)

## When to use each of these

Deciding when to use each of these can be nonobvious to say the least! When is it a good idea to reach for code points? Graphemes? UTF-8?

The way Roc organizes the `Str` module and supporting packages is designed to help answer this question. Every situation is different, but the following rules of thumb are typical:

* Most often, using `Str` values along with helper functions like [`split_on`](../Str#split_on), [`join_with`](../Str#join_with), and so on, is the best option.
* If you are specifically implementing a parser, working in UTF-8 bytes is usually the best option. So functions like [`to_utf8`](../Str#to_utf8), [`from_utf8`](../Str#from_utf8), and so on. (Note that single-quote literals produce number literals, so ASCII-range literals like `'a'` gives an integer literal that works with a UTF-8 `U8`.)
* If you are implementing a Unicode library like [roc-lang/unicode](https://github.com/roc-lang/unicode), working in terms of code points will be unavoidable. Aside from basic readability considerations like `\u(...)` in string literals, if you have the option to avoid working in terms of code points, it is almost always correct to avoid them.
* If it seems like a good idea to split a string into "characters" (graphemes), you should definitely stop and reconsider whether this is really the best design. Almost always, doing this is some combination of more error-prone or slower (usually both) than doing something else that does not require taking graphemes into consideration.

For this reason (among others), grapheme functions live in [roc-lang/unicode](https://github.com/roc-lang/unicode) rather than in [`Str`](../Str). They are more niche than they seem, so they should not be reached for all the time!


## Surrogates

Since Roc only allows valid UTF-8, surrogate pairs (including individual high and low surrogates)
are not valid syntax, not even in single quotes or `\u(…)` escapes.

## Bidirectional controls

Literal Unicode bidirectional controls are not allowed in Roc source. They can
make code appear different from what the compiler executes (CVE-2021-42574).
This restriction applies inside strings too, even when the controls are balanced.
Use an explicit Unicode escape when a string needs a directional control:

```roc
right_to_left_override = "\u(202E)"
```

The escape produces the actual character in the string at runtime. Ordinary
Arabic, Hebrew, and other Unicode text remains valid. Roc diagnostics display
controls visibly, for example `<U+202E RLO>`, without changing string values.

The restricted code points are U+061C, U+200E–U+200F, U+202A–U+202E, and
U+2066–U+2069. Formatting refuses affected source and leaves the file unchanged.

## Performance

### Memory Layout

A `Str` is 24 bytes on a 64-bit target (12 bytes on a 32-bit target), made up of three
pointer-sized pieces: a pointer to the string's bytes, the string's length, and its _capacity_
(how many bytes it has room for before it needs more memory).

Short strings don't need a pointer, though. If a string's UTF-8 bytes fit in 23 bytes or fewer
on a 64-bit target (11 or fewer on a 32-bit target), the bytes are stored right there in the
`Str` itself, and no heap allocation happens at all. This is called the _small string
optimization_. Lots of strings in practice are short (names, keys, identifiers, short labels,
and so on), so this saves lots of allocations.

Longer strings store their bytes in a heap allocation, which is
[reference counted](expressions#reference-counting).

### Substrings

Operations that return part of a string, like [`split_on`](../Str#split_on),
[`drop_prefix`](../Str#drop_prefix), and [`trim`](../Str#trim), don't copy the bytes. Instead,
they return a _slice_: a `Str` whose pointer points into the middle of the original string's
heap allocation. That makes them fast, regardless of how long the string is.

The tradeoff is that a slice keeps the whole original allocation alive. So if you read a
100-megabyte file into a string, split it into lines, and then keep just one of those lines
around, the whole 100 megabytes stays in memory as long as that one line does. If that's a
problem, you can make an independent copy of the line with `"".concat(line)`, which copies the
line's bytes into a new allocation.

### Building Strings

Like [lists](expressions#opportunistic-mutation), strings get updated in place when they're
unique. So appending to a string in a loop doesn't copy the whole string each time:

```roc
var $text = Str.with_capacity(1024)

for word in words {
    $text = $text.concat(word).concat(" ")
}
```

Since `$text` is the only reference to the string, each `concat` adds bytes to the end of the
existing allocation (getting more memory when it runs out of room).
[`Str.with_capacity`](../Str#with_capacity) and [`Str.reserve`](../Str#reserve) let you allocate
enough room up front, if you know roughly how big the string will get.

### Equality

Comparing two strings with `==` compares their bytes, so it takes time proportional to the
length of the strings. It can stop early, though. Strings with different lengths are never
equal, and two strings that share the same memory (for example, because one was passed around
and compared with itself) are always equal, so neither case needs to look at the bytes at all.
