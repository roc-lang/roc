# Strings

Roc strings are designed to represent text. For example, `"Hi!"` is a string.

If you're interested in the low-level details of their [UTF-8 representation](#low-level), you can skip ahead to that section, but most readers will benefit more from starting at a high level.

## Unicode

Roc strings reprsent text using [Unicode](https://unicode.org), which
is a deep topic. (The [Unicode glossary](http://www.unicode.org/glossary/) has over 500 entries in it!)

This guide will provide a basic overview of Unicode, including the relevant differences between these concepts:

* Code points
* Graphemes
* UTF-8

It will also explain why some operations are included in Roc's builtin [Str](https://www.roc-lang.org/builtins/Str)
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

Every Unicode text value can be broken down into [Unicode code points](http://www.unicode.org/glossary/#code_point), which are integers that describe different components of the text. In memory, every Roc string is a sequence of these integers stored in a format called UTF-8, which will be discussed [later](#low-level).

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

128105 : Int *
```

The single-quote `'` syntax lets you put a grapheme directly into your source code (so you can see what it looks like) as long as that grapheme contains only one code point, like 👩 does (namely, the code point 128105). At runtime, the single-quoted value will be treated the same as an ordinary number literal—in other words, `'👩'` is syntax sugar for writing `128105`.

You can verify this in `roc repl`:

```
» '👩' == 128105

Bool.true : Bool
```

Double quotes (`"`), on the other hand, are not type-compatible with integers—not only because strings can be empty (`""` is valid, but `''` is not) but also because there may be more than one code point involved in any given string!

## String equality and normalization

Besides emoji like 👩‍👩‍👦‍👦, another classic example of code points combining to render as one grapheme has to do with accent marks. Try putting these two different strings into `roc repl`:

```
"caf\u(e9)"
"caf\u(301)e"
```

The `\u(e9)` syntax is a way of inserting code points into string literals. In this case, it's the same as inserting the hexadecimal number `0xe9` as a code point onto the end of the string `"caf"`. Since Unicode code point `0xe9` happens to be `é`, the string `"caf\u(e9)"` ends up being identical in memory to the string `"café"`.

We can verify this by putting the following into `roc repl`:

```
» "caf\u(e9)" == "café"

Bool.true : Bool
```

As it turns out, `"cafe\u(301)"` is another way to represent the same word. The Unicode code point 0x301 represents a ["combining acute accent"](https://unicodeplus.com/U+0301)—which essentially means that it will add an accent mark to whatever came before it. In this case, since `"cafe\u(301)"` has an `e` before the `"\u(301)"`, that `e` ends up with an accent mark on it and becomes `é`.

Although these two strings get rendered identically to one another, they are different in memory because their code points are different! We can also confirm this in `roc repl`:

```
» "caf\u(e9)" == "cafe\u(301)"

Bool.false : Bool
```

This can be a source of bugs! One way to prevent this problem is to perform string normalization.

## String normalization

## Why not normalize automatically

It would be possible for Roc to perform string normalization automatically on every equality check, in order to prevent bugs like this. Unfortunately, normalization takes significantly more CPU time than equality comparisons do, which means it's much more efficient to perform normalization once and then fast equality checks from then on. This is the design that Roc encourages.

## UTF-8

## When to use each of these

Deciding when to use each of these can be nonobvious to say the least! When is it a good idea to reach for code points? Graphemes? UTF-8?

The way Roc organizes the `Str` module and supporting packages is designed to help answer this question. Every situation is different, but the following rules of thumb are typical:

* Most often, using `Str` values along with helper functions like [`split`](https://www.roc-lang.org/builtins/Str#split), [`joinWith`](https://www.roc-lang.org/builtins/Str#joinWith), and so on, is the best option.
* If you are specifically implementing a parser, working in UTF-8 bytes is usually the best option. So functions like [`walkUtf8`](https://www.roc-lang.org/builtins/Str#walkUtf8), [toUtf8](https://www.roc-lang.org/builtins/Str#toUtf8), and so on. (Note that single-quote literals produce number literals, so ASCII-range literals like `'a'` gives an integer literal that works with a UTF-8 `U8`.)
* If you are implementing a Unicode library like [roc-lang/unicode](https://github.com/roc-lang/unicode), working in terms of code points will be unavoidable. Aside from basic readability considerations like `\u(...)` in string literals, if you have the option to avoid working in terms of code points, it is almost always correct to avoid them.
* If it seems like a good idea to split a string into "characters" (graphemes), you should definitely stop and reconsider whether this is really the best design. Almost always, doing this is some combination of more error-prone or slower (usually both) than doing something else that does not require taking graphemes into consideration.

For this reason (among others), grapheme functions live in [roc-lang/unicode](https://github.com/roc-lang/unicode) rather than in [`Str`](https://www.roc-lang.org/builtins/Str). They are more niche than they seem, so they should not be reached for all the time! Another reason is that 


# Low-Level

Since Roc only allows valid UTF-8, surrogate pairs (including individual high and low surrogates) are not valid syntax, not even in single quotes.
