## Strings represent text. For example, `"Hi!"` is a string.
##
## This guide starts at a high level and works down to the in-memory representation of strings and their [performance characteristics](#performance). For reasons that will be explained later in this guide, some string operations are in the `Str` module while others (notably [capitalization](#capitalization), [code points](#code-points), [graphemes](#graphemes), and sorting) are in separate packages. There's also a list of recommendations for [when to use code points, graphemes, and UTF-8](#when-to-use).
##
## ## Syntax
##
## The most common way to represent strings is using quotation marks:
##
## ```
## "Hello, World!"
## ```
##
## Using this syntax, the whole string must go on one line. You can write multiline strings using triple quotes:
##
## ```
## text =
##     """
##     In memory, this string will not have any spaces
##     at its start. That's because the first line
##     starts at the same indentation level as the
##     opening quotation mark. Actually, none of these
##     lines will be indented.
##
##         However, this line will be indented!
##     """
## ```
##
## In triple-quoted strings, both the opening and closing `"""` must be at the same indentation level. Lines in the string begin at that indentation level; the spaces that indent the multiline string itself are not considered content.
##
## ### Interpolation
##
## *String interpolation* is syntax for inserting a string into another string.
##
## ```
## name = "Sam"
##
## "Hi, my name is $(name)!"
## ```
##
## This will evaluate to the string `"Hi, my name is Sam!"`
##
## You can put any expression you like inside the parentheses, as long as it's all on one line:
##
## ```
## colors = ["red", "green", "blue"]
##
## "The colors are $(colors |> Str.joinWith ", ")!"
## ```
##
## Interpolation can be used in multiline strings, but the part inside the parentheses must still be on one line.
##
## ### Escapes
##
## There are a few special escape sequences in strings:
##
## * `\n` becomes a [newline](https://en.wikipedia.org/wiki/Newline)
## * `\r` becomes a [carriage return](https://en.wikipedia.org/wiki/Carriage_return#Computers)
## * `\t` becomes a [tab](https://en.wikipedia.org/wiki/Tab_key#Tab_characters)
## * `\"` becomes a normal `"` (this lets you write `"` inside a single-line string)
## * `\\` becomes a normal `\` (this lets you write `\` without it being treated as an escape)
## * `\$` becomes a normal `$` (this lets you write `$` followed by `(` without it being treated as [interpolation](#interpolation))
##
## These work in both single-line and multiline strings. We'll also discuss another escape later, for inserting [Unicode code points](#code-points) into a string.
##
## ### Single quote syntax
##
## Try putting `'👩'` into `roc repl`. You should see this:
##
## ```
## » '👩'
##
## 128105 : Int *
## ```
##
## The single-quote `'` syntax lets you represent a Unicode code point (discussed in the next section) in source code, in a way that renders as the actual text it represents rather than as a number literal. This lets you see what it looks like in the source code rather than looking at a number.
##
## At runtime, the single-quoted value will be treated the same as an ordinary number literal—in other words, `'👩'` is syntax sugar for writing `128105`. You can verify this in `roc repl`:
##
## ```
## » '👩' == 128105
##
## Bool.true : Bool
## ```
##
## Double quotes (`"`), on the other hand, are not type-compatible with integers—not only because strings can be empty (`""` is valid, but `''` is not) but also because there may be more than one code point involved in any given string!
##
## There are also some special escape sequences in single-quote strings:
##
## * `\n` becomes a [newline](https://en.wikipedia.org/wiki/Newline)
## * `\r` becomes a [carriage return](https://en.wikipedia.org/wiki/Carriage_return#Computers)
## * `\t` becomes a [tab](https://en.wikipedia.org/wiki/Tab_key#Tab_characters)
## * `\'` becomes a normal `'` (this lets you write `'` inside a single-quote string)
## * `\\` becomes a normal `\` (this lets you write `\` without it being treated as an escape)
##
## Most often this single-quote syntax is used when writing parsers; most Roc programs never use it at all.
##
## ## Unicode
##
## Roc strings represent text using [Unicode](https://unicode.org) This guide will provide only a basic overview of Unicode (the [Unicode glossary](http://www.unicode.org/glossary/) has over 500 entries in it), but it will include the most relevant differences between these concepts:
##
## * Code points
## * Graphemes
## * UTF-8
##
## It will also explain why some operations are included in Roc's builtin [Str](https://www.roc-lang.org/builtins/Str)
## module, and why others are in separate packages like [roc-lang/unicode](https://github.com/roc-lang/unicode).
##
## ### Graphemes
##
## Let's start with the following string:
##
## `"👩‍👩‍👦‍👦"`
##
## Some might call this a "character." After all, in a monospace font, it looks to be about the same width as the letter "A" or the punctuation mark "!"—both of which are commonly called "characters." Unfortunately, the term "character" in programming has changed meanings many times across the years and across programming languages, and today it's become a major source of confusion.
##
## Unicode uses the less ambiguous term [*grapheme*](https://www.unicode.org/glossary/#grapheme), which it defines as a "user-perceived character" (as opposed to one of the several historical ways the term "character" has been used in programming) or, alternatively, "A minimally distinctive unit of writing in the context of a particular writing system."
##
## By Unicode's definition, each of the following is an individual grapheme:
##
## * `a`
## * `鹏`
## * `👩‍👩‍👦‍👦`
##
## Note that although *grapheme* is less ambiguous than *character*, its definition is still open to interpretation. To address this, Unicode has formally specified [text segmentation rules](https://www.unicode.org/reports/tr29/) which define grapheme boundaries in precise technical terms. We won't get into those rules here, but since they can change with new Unicode releases, functions for working with graphemes are in the [roc-lang/unicode](https://github.com/roc-lang/unicode) package rather than in the builtin [`Str`](https://www.roc-lang.org/builtins/Str) module. This allows them to be updated without being blocked on a new release of the Roc language.
##
## ### Code Points
##
## Every Unicode text value can be broken down into [Unicode code points](http://www.unicode.org/glossary/#code_point), which are integers between `0` and `285_212_438` that describe components of the text. In memory, every Roc string is a sequence of these integers stored in a format called UTF-8, which will be discussed [later](#utf8).
##
## The string `"👩‍👩‍👦‍👦"` happens to be made up of these code points:
##
## ```
## [128105, 8205, 128105, 8205, 128102, 8205, 128102]
## ```
##
## From this we can see that:
##
## -   One grapheme can be made up of multiple code points. In fact, there is no upper limit on how many code points can go into a single grapheme! (Some programming languages use the term "character" to refer to individual code points; this can be confusing for graphemes like 👩‍👩‍👦‍👦 because it visually looks like "one character" but no single code point can represent it.)
## -   Sometimes code points repeat within an individual grapheme. Here, 128105 repeats twice, as does 128102, and there's an 8205 in between each of the other code points.
##
## ### Combining Code Points
##
## The reason every other code point in 👩‍👩‍👦‍👦 is 8205 is that code point 8205 joins together other code points. This emoji, known as ["Family: Woman, Woman, Boy, Boy"](https://emojipedia.org/family-woman-woman-boy-boy), is made by combining several emoji using [zero-width joiners](https://emojipedia.org/zero-width-joiner)—which are represented by code point 8205 in memory, and which have no visual repesentation on their own.
##
## Here are those code points again, this time with comments about what they represent:
##
## ```
## [128105] # "👩"
## [8205]   # (joiner)
## [128105] # "👩"
## [8205]   # (joiner)
## [128102] # "👦"
## [8205]   # (joiner)
## [128102] # "👦"
## ```
##
## One way to read this is "woman emoji joined to woman emoji joined to boy emoji joined to boy emoji." Without the joins, it would be:
##
## ```
## "👩👩👦👦"
## ```
##
## With the joins, however, it is instead:
##
## ```
## "👩‍👩‍👦‍👦"
## ```
##
## Even though 👩‍👩‍👦‍👦 is visually smaller when rendered, it takes up almost twice as much memory as 👩👩👦👦 does! That's because it has all the same code points, plus the zero-width joiners in between them.
##
## ### String equality and normalization
##
## Besides emoji like 👩‍👩‍👦‍👦, another classic example of multiple code points being combined to render as one grapheme has to do with accent marks. Try putting these two strings into `roc repl`:
##
## ```
## "caf\u(e9)"
## "cafe\u(301)"
## ```
##
## The `\u(e9)` syntax is a way of inserting code points into string literals. In this case, it's the same as inserting the hexadecimal number `0xe9` as a code point onto the end of the string `"caf"`. Since Unicode code point `0xe9` happens to be `é`, the string `"caf\u(e9)"` ends up being identical in memory to the string `"café"`.
##
## We can verify this too:
##
## ```
## » "caf\u(e9)" == "café"
##
## Bool.true : Bool
## ```
##
## As it turns out, `"cafe\u(301)"` is another way to represent the same word. The Unicode code point 0x301 represents a ["combining acute accent"](https://unicodeplus.com/U+0301)—which essentially means that it will add an accent mark to whatever came before it. In this case, since `"cafe\u(301)"` has an `e` before the `"\u(301)"`, that `e` ends up with an accent mark on it and becomes `é`.
##
## Although these two strings get rendered identically to one another, they are different in memory because their code points are different! We can also confirm this in `roc repl`:
##
## ```
## » "caf\u(e9)" == "cafe\u(301)"
##
## Bool.false : Bool
## ```
##
## As you can imagine, this can be a source of bugs. Not only are they considered unequal, they also hash differently, meaning `"caf\u(e9)"` and `"cafe\u(301)"` can both be separate entries in the same [`Set`](https://www.roc-lang.org/builtins/Set).
##
##  One way to prevent problems like these is to perform [Unicode normalization](https://www.unicode.org/reports/tr15/), a process which converts conceptually equivalent strings (like `"caf\u(e9)"` and `"cafe\u(301)"`) into one canonical in-memory representation. This makes equality checks on them pass, among other benefits.
##
## It would be technically possible for Roc to perform string normalization automatically on every equality check. Unfortunately, although some programs might want to treat `"caf\u(e9)"` and `"cafe\u(301)"` as equivalent, for other programs it might actually be important to be able to tell them apart. If these equality checks always passed, then there would be no way to tell them apart!
##
## As such, normalization must be performed explicitly when desired. Like graphemes, Unicode normalization rules can change with new releases of Unicode. As such, these functions are in separate packages instead of builtins (normalization is planned to be in [roc-lang/unicode](https://github.com/roc-lang/unicode) in the future, but it has not yet been implemented) so that updates to these functions based on new Unicode releases can happen without waiting on new releases of the Roc language.
##
## ### Capitalization
##
## We've already seen two examples of Unicode definitions that can change with new Unicode releases: graphemes and normalization. Another is capitalization; these rules can change with new Unicode releases (most often in the form of additions of new languages, but breaking changes to capitalization rules for existing languages are also possible), and so they are not included in builtin [`Str`](https://www.roc-lang.org/builtins/Str).
##
## This might seem particularly surprising, since capitalization functions are commonly included in standard libraries. However, it turns out that "capitalizing an arbitrary string" is impossible to do correctly without additional information.
##
## For example, what is the capitalized version of this string?
##
## ```
## "i"
## ```
##
## * In English, the correct answer is `"I"`.
## * In Turkish, the correct answer is `"İ"`.
##
## Similarly, the correct lowercased version of the string `"I"` is `"i"` in English and `"ı"` in Turkish.
##
## Turkish is not the only language to use this [dotless i](https://en.wikipedia.org/wiki/Dotless_I), and it's an example of how a function which capitalizes strings cannot give correct answers without the additional information of which language's capitalization rules should be used.
##
## Many languages defer to the operating system's [localization](https://en.wikipedia.org/wiki/Internationalization_and_localization) settings for this information. In that design, calling a program's capitalization function with an input string of `"i"` might give an answer of `"I"` on one machine and `"İ"` on a different machine, even though it was the same program running on both systems. Naturally, this can cause bugs—but more than that, writing tests to prevent bugs like this usually requires extra complexity compared to writing ordinary tests.
##
## In general, Roc programs should give the same answers for the same inputs even when run on different machines. There are exceptions to this (e.g. a program running out of system resources on one machine, while being able to make more progress on a machine that has more resources), but operating system's language localization is not among them.
##
## For these reasons, capitalization functions are not in [`Str`](https://www.roc-lang.org/builtins/Str). There is a planned `roc-lang` package to handle use cases like capitalization and sorting—sorting can also vary by language as well as by things like country—but implementation work has not yet started on this package.
##
## ### UTF-8
##
## Earlier, we discussed how Unicode code points can be described as [`U32`](https://www.roc-lang.org/builtins/Num#U32) integers. However, many common code points are very low integers, and can fit into a `U8` instead of needing an entire `U32` to represent them in memory. UTF-8 takes advantage of this, using a variable-width encoding to represent code points in 1-4 bytes, which saves a lot of memory in the typical case—especially compared to [UTF-16](https://en.wikipedia.org/wiki/UTF-16), which always uses at least 2 bytes to represent each code point, or [UTF-32](https://en.wikipedia.org/wiki/UTF-32), which always uses the maximum 4 bytes.
##
## This guide won't cover all the details of UTF-8, but the basic idea is this:
##
## - If a code point is 127 or lower, UTF-8 stores it in 1 byte.
## - If it's between 128 and 2047, UTF-8 stores it in 2 bytes.
## - If it's between 2048 and 65535, UTF-8 stores it in 3 bytes.
## - If it's higher than that, UTF-8 stores it in 4 bytes.
##
## The specific [UTF-8 encoding](https://en.wikipedia.org/wiki/UTF-8#Encoding) of these bytes involves using 1 to 5 bits of each byte for metadata about multi-byte sequences.
##
## A valuable feature of UTF-8 is that it is backwards-compatible with the [ASCII](https://en.wikipedia.org/wiki/ASCII) encoding that was widely used for many years. ASCII existed before Unicode did, and only used the integers 0 to 127 to represent its equivalent of code points. The Unicode code points 0 to 127 represent the same semantic information as ASCII, (e.g. the number 64 represents the letter "A" in both ASCII and in Unicode), and since UTF-8 represents code points 0 to 127 using one byte, all valid ASCII strings can be successfully parsed as UTF-8 without any need for conversion.
##
## Since many textual computer encodings—including [CSV](https://en.wikipedia.org/wiki/CSV), [XML](https://en.wikipedia.org/wiki/XML), and [JSON](https://en.wikipedia.org/wiki/JSON)—do not use any code points above 127 for their delimiters, it is often possible to write parsers for these formats using only `Str` functions which present UTF-8 as raw `U8` sequences, such as [`Str.walkUtf8`](https://www.roc-lang.org/builtins/Str#walkUtf8) and [`Str.toUtf8`](https://www.roc-lang.org/builtins/Str#toUtf8). In the typical case where they do not to need to parse out individual Unicode code points, they can get everything they need from `Str` UTF-8 functions without needing to depend on other packages.
##
## ### When to use code points, graphemes, and UTF-8
##
## Deciding when to use code points, graphemes, and UTF-8 can be nonobvious to say the least!
##
## The way Roc organizes the `Str` module and supporting packages is designed to help answer this question. Every situation is different, but the following rules of thumb are typical:
##
## * Most often, using `Str` values along with helper functions like [`splitOn`](https://www.roc-lang.org/builtins/Str#splitOn), [`joinWith`](https://www.roc-lang.org/builtins/Str#joinWith), and so on, is the best option.
## * If you are specifically implementing a parser, working in UTF-8 bytes is usually the best option. So functions like [`walkUtf8`](https://www.roc-lang.org/builtins/Str#walkUtf8), [toUtf8](https://www.roc-lang.org/builtins/Str#toUtf8), and so on. (Note that single-quote literals produce number literals, so ASCII-range literals like `'a'` gives an integer literal that works with a UTF-8 `U8`.)
## * If you are implementing a Unicode library like [roc-lang/unicode](https://github.com/roc-lang/unicode), working in terms of code points will be unavoidable. Aside from basic readability considerations like `\u(...)` in string literals, if you have the option to avoid working in terms of code points, it is almost always correct to avoid them.
## * If it seems like a good idea to split a string into "characters" (graphemes), you should definitely stop and reconsider whether this is really the best design. Almost always, doing this is some combination of more error-prone or slower (usually both) than doing something else that does not require taking graphemes into consideration.
##
## For this reason (among others), grapheme functions live in [roc-lang/unicode](https://github.com/roc-lang/unicode) rather than in [`Str`](https://www.roc-lang.org/builtins/Str). They are more niche than they seem, so they should not be reached for all the time!
##
## ## Performance
##
## This section deals with how Roc strings are represented in memory, and their performance characteristics.
##
## A normal heap-allocated roc `Str` is represented on the stack as:
## - A "capacity" unsigned integer, which respresents how many bytes are allocated on the heap to hold the string's contents.
## - A "length" unsigned integer, which rerepresents how many of the "capacity" bytes are actually in use. (A `Str` can have more bytes allocated on the heap than are actually in use.)
## - The memory address of the first byte in the string's actual contents.
##
## Each of these three fields is the same size: 64 bits on a 64-bit system, and 32 bits on a 32-bit system. The actual contents of the string are stored in one contiguous sequence of bytes, encoded as UTF-8, often on the heap but sometimes elsewhere—more on this later. Empty strings do not have heap allocations, so an empty `Str` on a 64-bit system still takes up 24 bytes on the stack (due to its three 64-bit fields).
##
## ### Reference counting and opportunistic mutation
##
## Like lists, dictionaries, and sets, Roc strings are automatically reference-counted and can benefit from opportunistic in-place mutation. The reference count is stored on the heap immediately before the first byte of the string's contents, and it has the same size as a memory address. This means it can count so high that it's impossible to write a Roc program which overflows a reference count, because having that many simultaneous references (each of which is a memory address) would have exhausted the operating system's address space first.
##
## When the string's reference count is 1, functions like [`Str.concat`](https://www.roc-lang.org/builtins/Str#concat) and [`Str.replaceEach`](https://www.roc-lang.org/builtins/Str#replaceEach) mutate the string in-place rather than allocating a new string. This preserves semantic immutability because it is unobservable in terms of the operation's output; if the reference count is 1, it means that memory would have otherwise been deallocated immediately anyway, and it's more efficient to reuse it instead of deallocating it and then immediately making a new allocation.
##
##  The contents of statically-known strings (today that means string literals) are stored in the readonly section of the binary, so they do not need heap allocations or reference counts. They are not eligible for in-place mutation, since mutating the readonly section of the binary would cause an operating system [access violation](https://en.wikipedia.org/wiki/Segmentation_fault).
##
## ### Small String Optimization
##
## Roc uses a "small string optimization" when representing certain strings in memory.
##
## If you have a sufficiently long string, then on a 64-bit system it will be represented on the stack using 24 bytes, and on a 32-bit system it will take 12 bytes—plus however many bytes are in the string itself—on the heap. However, if there is a string shorter than either of these stack sizes (so, a string of up to 23 bytes on a 64-bit system, and up to 11 bytes on a 32-bit system), then that string will be stored entirely on the stack rather than having a separate heap allocation at all.
##
## This can be much more memory-efficient! However, `List` does not have this optimization (it has some runtime cost, and in the case of `List` it's not anticipated to come up nearly as often), which means when converting a small string to `List U8` it can result in a heap allocation.
##
## Note that this optimization is based entirely on how many UTF-8 bytes the string takes up in memory. It doesn't matter how many [graphemes](#graphemes), [code points](#code-points) or anything else it has; the only factor that determines whether a particular string is eligible for the small string optimization is the number of UTF-8 bytes it takes up in memory!
##
## ### Seamless Slices
##
## Try putting this into `roc repl`:
##
## ```
## » "foo/bar/baz" |> Str.splitOn "/"
##
## ["foo", "bar", "baz"] : List Str
## ```
##
## All of these strings are small enough that the [small string optimization](#small) will apply, so none of them will be allocated on the heap.
##
## Now let's suppose they were long enough that this optimization no longer applied:
##
## ```
## » "a much, much, much, much/longer/string compared to the last one!" |> Str.splitOn "/"
##
## ["a much, much, much, much", "longer", "string compared to the last one!"] : List Str
## ```
##
## Here, the only strings small enough for the small string optimization are `"/"` and `"longer"`. They will be allocated on the stack.
##
## The first and last strings in the returned list `"a much, much, much, much"` and `"string compared to the last one!"` will not be allocated on the heap either. Instead, they will be *seamless slices*, which means they will share memory with the original input string.
##
## * `"a much, much, much, much"` will share the first 24 bytes of the original string.
## * `"string compared to the last one!"` will share the last 32 bytes of the original string.
##
## All of these strings are semantically immutable, so sharing these bytes is an implementation detail that should only affect performance. By design, there is no way at either compile time or runtime to tell whether a string is a seamless slice. This allows the optimization's behavior to change in the future without affecting Roc programs' semantic behavior.
##
## Seamless slices create additional references to the original string, which make it ineligible for opportunistic mutation (along with the slices themselves; slices are never eligible for mutation), and which also make it take longer before the original string can be deallocated. A case where this might be noticeable in terms of performance would be:
## 1. A function takes a very large string as an argument and returns a much smaller slice into that string.
## 2. The smaller slice is used for a long time in the program, whereas the much larger original string stops being used.
## 3. In this situation, it might have been better for total program memory usage (although not necessarily overall performance) if the original large string could have been deallocated sooner, even at the expense of having to copy the smaller string into a new allocation instead of reusing the bytes with a seamless slice.
##
## If a situation like this comes up, a slice can be turned into a separate string by using [`Str.concat`](https://www.roc-lang.org/builtins/Str#concat) to concatenate the slice onto an empty string (or one created with [`Str.withCapacity`](https://www.roc-lang.org/builtins/Str#withCapacity)).
##
## Currently, the only way to get seamless slices of strings is by calling certain `Str` functions which return them. In general, `Str` functions which accept a string and return a subset of that string tend to do this. [`Str.trim`](https://www.roc-lang.org/builtins/Str#trim) is another example of a function which returns a seamless slice.
module [
    Utf8Problem,
    Utf8ByteProblem,
    concat,
    isEmpty,
    joinWith,
    splitOn,
    repeat,
    countUtf8Bytes,
    toUtf8,
    fromUtf8,
    startsWith,
    endsWith,
    trim,
    trimStart,
    trimEnd,
    toDec,
    toF64,
    toF32,
    toU128,
    toI128,
    toU64,
    toI64,
    toU32,
    toI32,
    toU16,
    toI16,
    toU8,
    toI8,
    replaceEach,
    replaceFirst,
    replaceLast,
    splitFirst,
    splitLast,
    walkUtf8,
    walkUtf8WithIndex,
    reserve,
    releaseExcessCapacity,
    withCapacity,
    withPrefix,
    contains,
    dropPrefix,
    dropSuffix,
]

import Bool exposing [Bool]
import Result exposing [Result]
import List
import Num exposing [Num, U8, U16, U32, U64, U128, I8, I16, I32, I64, I128, F32, F64, Dec]

Utf8ByteProblem : [
    InvalidStartByte,
    UnexpectedEndOfSequence,
    ExpectedContinuation,
    OverlongEncoding,
    CodepointTooLarge,
    EncodesSurrogateHalf,
]

Utf8Problem : { byteIndex : U64, problem : Utf8ByteProblem }

## Returns [Bool.true] if the string is empty, and [Bool.false] otherwise.
## ```roc
## expect Str.isEmpty "hi!" == Bool.false
## expect Str.isEmpty "" == Bool.true
## ```
isEmpty : Str -> Bool

## Concatenates two strings together.
## ```roc
## expect Str.concat "ab" "cd" == "abcd"
## expect Str.concat "hello" "" == "hello"
## expect Str.concat "" "" == ""
## ```
concat : Str, Str -> Str

## Returns a string of the specified capacity without any content.
##
## This is a performance optimization tool that's like calling [Str.reserve] on an empty string.
## It's useful when you plan to build up a string incrementally, for example by calling [Str.concat] on it:
##
## ```roc
## greeting = "Hello and welcome to Roc"
## subject = "Awesome Programmer"
##
## # Evaluates to "Hello and welcome to Roc, Awesome Programmer!"
## helloWorld =
##     Str.withCapacity 45
##     |> Str.concat greeting
##     |> Str.concat ", "
##     |> Str.concat subject
##     |> Str.concat "!"
## ```
##
## In general, if you plan to use [Str.concat] on an empty string, it will be faster to start with
## [Str.withCapacity] than with `""`. Even if you don't know the exact capacity of the string, giving [withCapacity]
## a higher value than ends up being necessary can help prevent reallocation and copying—at
## the cost of using more memory than is necessary.
##
## For more details on how the performance optimization works, see [Str.reserve].
withCapacity : U64 -> Str

## Increase a string's capacity by at least the given number of additional bytes.
##
## This can improve the performance of string concatenation operations like [Str.concat] by
## allocating extra capacity up front, which can prevent the need for reallocations and copies.
## Consider the following example which does not use [Str.reserve]:
##
## ```roc
## greeting = "Hello and welcome to Roc"
## subject = "Awesome Programmer"
##
## # Evaluates to "Hello and welcome to Roc, Awesome Programmer!"
## helloWorld =
##     greeting
##     |> Str.concat ", "
##     |> Str.concat subject
##     |> Str.concat "!"
## ```
##
## In this example:
## 1. We start with `greeting`, which has both a length and capacity of 24 (bytes).
## 2. `|> Str.concat ", "` will see that there isn't enough capacity to add 2 more bytes for the `", "`, so it will create a new heap allocation with enough bytes to hold both. (This probably will be more than 7 bytes, because when [Str] functions reallocate, they apply a multiplier to the exact capacity required. This makes it less likely that future realloctions will be needed. The multiplier amount is not specified, because it may change in future releases of Roc, but it will likely be around 1.5 to 2 times the exact capacity required.) Then it will copy the current bytes (`"Hello"`) into the new allocation, and finally concatenate the `", "` into the new allocation. The old allocation will then be deallocated because it's no longer referenced anywhere in the program.
## 3. `|> Str.concat subject` will again check if there is enough capacity in the string. If it doesn't find enough capacity once again, it will make a third allocation, copy the existing bytes (`"Hello, "`) into that third allocation, and then deallocate the second allocation because it's already no longer being referenced anywhere else in the program. (It may find enough capacity in this particular case, because the previous [Str.concat] allocated something like 1.5 to 2 times the necessary capacity in order to anticipate future concatenations like this...but if something longer than `"World"` were being concatenated here, it might still require further reallocation and copying.)
## 4. `|> Str.concat "!\n"` will repeat this process once more.
##
## This process can have significant performance costs due to multiple reallocation of new strings, copying between old strings and new strings, and deallocation of immediately obsolete strings.
##
## Here's a modified example which uses [Str.reserve] to eliminate the need for all that reallocation, copying, and deallocation.
##
## ```roc
## helloWorld =
##     greeting
##     |> Str.reserve 21
##     |> Str.concat ", "
##     |> Str.concat subject
##     |> Str.concat "!"
## ```
##
## In this example:
## 1. We again start with `greeting`, which has both a length and capacity of 24 bytes.
## 2. `|> Str.reserve 21` will ensure that there is enough capacity in the string for an additional 21 bytes (to make room for `", "`, `"Awesome Programmer"`, and `"!"`). Since the current capacity is only 24, it will create a new 45-byte (24 + 21) heap allocation and copy the contents of the existing allocation (`greeting`) into it.
## 3. `|> Str.concat ", "` will concatenate `, ` to the string. No reallocation, copying, or deallocation will be necessary, because the string already has a capacity of 45 btytes, and `greeting` will only use 24 of them.
## 4. `|> Str.concat subject` will concatenate `subject` (`"Awesome Programmer"`) to the string. Again, no reallocation, copying, or deallocation will be necessary.
## 5. `|> Str.concat "!\n"` will concatenate `"!\n"` to the string, still without any reallocation, copying, or deallocation.
##
## Here, [Str.reserve] prevented multiple reallocations, copies, and deallocations during the
## [Str.concat] calls. Notice that it did perform a heap allocation before any [Str.concat] calls
## were made, which means that using [Str.reserve] is not free! You should only use it if you actually
## expect to make use of the extra capacity.
##
## Ideally, you'd be able to predict exactly how many extra bytes of capacity will be needed, but this
## may not always be knowable. When you don't know exactly how many bytes to reserve, you can often get better
## performance by choosing a number of bytes that's too high, because a number that's too low could lead to reallocations. There's a limit to
## this, of course; if you always give it ten times what it turns out to need, that could prevent
## reallocations but will also waste a lot of memory!
##
## If you plan to use [Str.reserve] on an empty string, it's generally better to use [Str.withCapacity] instead.
reserve : Str, U64 -> Str

## Combines a [List] of strings into a single string, with a separator
## string in between each.
## ```roc
## expect Str.joinWith ["one", "two", "three"] ", " == "one, two, three"
## expect Str.joinWith ["1", "2", "3", "4"] "." == "1.2.3.4"
## ```
joinWith : List Str, Str -> Str

## Split a string around a separator.
##
## Passing `""` for the separator is not useful;
## it returns the original string wrapped in a [List].
## ```roc
## expect Str.splitOn "1,2,3" "," == ["1","2","3"]
## expect Str.splitOn "1,2,3" "" == ["1,2,3"]
## ```
splitOn : Str, Str -> List Str

## Repeats a string the given number of times.
## ```roc
## expect Str.repeat "z" 3 == "zzz"
## expect Str.repeat "na" 8 == "nananananananana"
## ```
## Returns `""` when given `""` for the string or `0` for the count.
## ```roc
## expect Str.repeat "" 10 == ""
## expect Str.repeat "anything" 0 == ""
## ```
repeat : Str, U64 -> Str

## Returns a [List] of the string's [U8] UTF-8 [code units](https://unicode.org/glossary/#code_unit).
## (To split the string into a [List] of smaller [Str] values instead of [U8] values,
## see [Str.splitOn].)
## ```roc
## expect Str.toUtf8 "Roc" == [82, 111, 99]
## expect Str.toUtf8 "鹏" == [233, 185, 143]
## expect Str.toUtf8 "சி" == [224, 174, 154, 224, 174, 191]
## expect Str.toUtf8 "🐦" == [240, 159, 144, 166]
## ```
toUtf8 : Str -> List U8

## Converts a [List] of [U8] UTF-8 [code units](https://unicode.org/glossary/#code_unit) to a string.
##
## Returns `Err` if the given bytes are invalid UTF-8, and returns `Ok ""` when given `[]`.
## ```roc
## expect Str.fromUtf8 [82, 111, 99] == Ok "Roc"
## expect Str.fromUtf8 [233, 185, 143] == Ok "鹏"
## expect Str.fromUtf8 [224, 174, 154, 224, 174, 191] == Ok "சி"
## expect Str.fromUtf8 [240, 159, 144, 166] == Ok "🐦"
## expect Str.fromUtf8 [] == Ok ""
## expect Str.fromUtf8 [255] |> Result.isErr
## ```
fromUtf8 : List U8 -> Result Str [BadUtf8 { problem : Utf8ByteProblem, index : U64 }]
fromUtf8 = \bytes ->
    result = fromUtf8Lowlevel bytes

    if result.cIsOk then
        Ok result.bString
    else
        Err (BadUtf8 { problem: result.dProblemCode, index: result.aByteIndex })

expect (Str.fromUtf8 [82, 111, 99]) == Ok "Roc"
expect (Str.fromUtf8 [224, 174, 154, 224, 174, 191]) == Ok "சி"
expect (Str.fromUtf8 [240, 159, 144, 166]) == Ok "🐦"
expect (Str.fromUtf8 []) == Ok ""
expect (Str.fromUtf8 [255]) |> Result.isErr

FromUtf8Result : {
    aByteIndex : U64,
    bString : Str,
    cIsOk : Bool,
    dProblemCode : Utf8ByteProblem,
}

fromUtf8Lowlevel : List U8 -> FromUtf8Result

## Check if the given [Str] starts with a value.
## ```roc
## expect Str.startsWith "ABC" "A" == Bool.true
## expect Str.startsWith "ABC" "X" == Bool.false
## ```
startsWith : Str, Str -> Bool

## Check if the given [Str] ends with a value.
## ```roc
## expect Str.endsWith "ABC" "C" == Bool.true
## expect Str.endsWith "ABC" "X" == Bool.false
## ```
endsWith : Str, Str -> Bool

## Return the [Str] with all whitespace removed from both the beginning
## as well as the end.
## ```roc
## expect Str.trim "   Hello      \n\n" == "Hello"
## ```
trim : Str -> Str

## Return the [Str] with all whitespace removed from the beginning.
## ```roc
## expect Str.trimStart "   Hello      \n\n" == "Hello      \n\n"
## ```
trimStart : Str -> Str

## Return the [Str] with all whitespace removed from the end.
## ```roc
## expect Str.trimEnd "   Hello      \n\n" == "   Hello"
## ```
trimEnd : Str -> Str

## Encode a [Str] to a [Dec]. A [Dec] value is a 128-bit decimal
## [fixed-point number](https://en.wikipedia.org/wiki/Fixed-point_arithmetic).
## ```roc
## expect Str.toDec "10" == Ok 10dec
## expect Str.toDec "-0.25" == Ok -0.25dec
## expect Str.toDec "not a number" == Err InvalidNumStr
## ```
toDec : Str -> Result Dec [InvalidNumStr]
toDec = \string -> strToNumHelp string

## Encode a [Str] to a [F64]. A [F64] value is a 64-bit
## [floating-point number](https://en.wikipedia.org/wiki/IEEE_754) and can be
## specified with a `f64` suffix.
## ```roc
## expect Str.toF64 "0.10" == Ok 0.10f64
## expect Str.toF64 "not a number" == Err InvalidNumStr
## ```
toF64 : Str -> Result F64 [InvalidNumStr]
toF64 = \string -> strToNumHelp string

## Encode a [Str] to a [F32].A [F32] value is a 32-bit
## [floating-point number](https://en.wikipedia.org/wiki/IEEE_754) and can be
## specified with a `f32` suffix.
## ```roc
## expect Str.toF32 "0.10" == Ok 0.10f32
## expect Str.toF32 "not a number" == Err InvalidNumStr
## ```
toF32 : Str -> Result F32 [InvalidNumStr]
toF32 = \string -> strToNumHelp string

## Encode a [Str] to an unsigned [U128] integer. A [U128] value can hold numbers
## from `0` to `340_282_366_920_938_463_463_374_607_431_768_211_455` (over
## 340 undecillion). It can be specified with a u128 suffix.
## ```roc
## expect Str.toU128 "1500" == Ok 1500u128
## expect Str.toU128 "0.1" == Err InvalidNumStr
## expect Str.toU128 "-1" == Err InvalidNumStr
## expect Str.toU128 "not a number" == Err InvalidNumStr
## ```
toU128 : Str -> Result U128 [InvalidNumStr]
toU128 = \string -> strToNumHelp string

## Encode a [Str] to a signed [I128] integer. A [I128] value can hold numbers
## from `-170_141_183_460_469_231_731_687_303_715_884_105_728` to
## `170_141_183_460_469_231_731_687_303_715_884_105_727`. It can be specified
## with a i128 suffix.
## ```roc
## expect Str.toI128 "1500" == Ok 1500i128
## expect Str.toI128 "-1" == Ok -1i128
## expect Str.toI128 "0.1" == Err InvalidNumStr
## expect Str.toI128 "not a number" == Err InvalidNumStr
## ```
toI128 : Str -> Result I128 [InvalidNumStr]
toI128 = \string -> strToNumHelp string

## Encode a [Str] to an unsigned [U64] integer. A [U64] value can hold numbers
## from `0` to `18_446_744_073_709_551_615` (over 18 quintillion). It
## can be specified with a u64 suffix.
## ```roc
## expect Str.toU64 "1500" == Ok 1500u64
## expect Str.toU64 "0.1" == Err InvalidNumStr
## expect Str.toU64 "-1" == Err InvalidNumStr
## expect Str.toU64 "not a number" == Err InvalidNumStr
## ```
toU64 : Str -> Result U64 [InvalidNumStr]
toU64 = \string -> strToNumHelp string

## Encode a [Str] to a signed [I64] integer. A [I64] value can hold numbers
## from `-9_223_372_036_854_775_808` to `9_223_372_036_854_775_807`. It can be
## specified with a i64 suffix.
## ```roc
## expect Str.toI64 "1500" == Ok 1500i64
## expect Str.toI64 "-1" == Ok -1i64
## expect Str.toI64 "0.1" == Err InvalidNumStr
## expect Str.toI64 "not a number" == Err InvalidNumStr
## ```
toI64 : Str -> Result I64 [InvalidNumStr]
toI64 = \string -> strToNumHelp string

## Encode a [Str] to an unsigned [U32] integer. A [U32] value can hold numbers
## from `0` to `4_294_967_295` (over 4 billion). It can be specified with
## a u32 suffix.
## ```roc
## expect Str.toU32 "1500" == Ok 1500u32
## expect Str.toU32 "0.1" == Err InvalidNumStr
## expect Str.toU32 "-1" == Err InvalidNumStr
## expect Str.toU32 "not a number" == Err InvalidNumStr
## ```
toU32 : Str -> Result U32 [InvalidNumStr]
toU32 = \string -> strToNumHelp string

## Encode a [Str] to a signed [I32] integer. A [I32] value can hold numbers
## from `-2_147_483_648` to `2_147_483_647`. It can be
## specified with a i32 suffix.
## ```roc
## expect Str.toI32 "1500" == Ok 1500i32
## expect Str.toI32 "-1" == Ok -1i32
## expect Str.toI32 "0.1" == Err InvalidNumStr
## expect Str.toI32 "not a number" == Err InvalidNumStr
## ```
toI32 : Str -> Result I32 [InvalidNumStr]
toI32 = \string -> strToNumHelp string

## Encode a [Str] to an unsigned [U16] integer. A [U16] value can hold numbers
## from `0` to `65_535`. It can be specified with a u16 suffix.
## ```roc
## expect Str.toU16 "1500" == Ok 1500u16
## expect Str.toU16 "0.1" == Err InvalidNumStr
## expect Str.toU16 "-1" == Err InvalidNumStr
## expect Str.toU16 "not a number" == Err InvalidNumStr
## ```
toU16 : Str -> Result U16 [InvalidNumStr]
toU16 = \string -> strToNumHelp string

## Encode a [Str] to a signed [I16] integer. A [I16] value can hold numbers
## from `-32_768` to `32_767`. It can be
## specified with a i16 suffix.
## ```roc
## expect Str.toI16 "1500" == Ok 1500i16
## expect Str.toI16 "-1" == Ok -1i16
## expect Str.toI16 "0.1" == Err InvalidNumStr
## expect Str.toI16 "not a number" == Err InvalidNumStr
## ```
toI16 : Str -> Result I16 [InvalidNumStr]
toI16 = \string -> strToNumHelp string

## Encode a [Str] to an unsigned [U8] integer. A [U8] value can hold numbers
## from `0` to `255`. It can be specified with a u8 suffix.
## ```roc
## expect Str.toU8 "250" == Ok 250u8
## expect Str.toU8 "-0.1" == Err InvalidNumStr
## expect Str.toU8 "not a number" == Err InvalidNumStr
## expect Str.toU8 "1500" == Err InvalidNumStr
## ```
toU8 : Str -> Result U8 [InvalidNumStr]
toU8 = \string -> strToNumHelp string

## Encode a [Str] to a signed [I8] integer. A [I8] value can hold numbers
## from `-128` to `127`. It can be
## specified with a i8 suffix.
## ```roc
## expect Str.toI8 "-15" == Ok -15i8
## expect Str.toI8 "150.00" == Err InvalidNumStr
## expect Str.toI8 "not a number" == Err InvalidNumStr
## ```
toI8 : Str -> Result I8 [InvalidNumStr]
toI8 = \string -> strToNumHelp string

## Get the byte at the given index, without performing a bounds check.
getUnsafe : Str, U64 -> U8

## Gives the number of bytes in a [Str] value.
## ```roc
## expect Str.countUtf8Bytes "Hello World" == 11
## ```
countUtf8Bytes : Str -> U64

## string slice that does not do bounds checking or utf-8 verification
substringUnsafe : Str, U64, U64 -> Str

## Returns the given [Str] with each occurrence of a substring replaced.
## If the substring is not found, returns the original string.
##
## ```roc
## expect Str.replaceEach "foo/bar/baz" "/" "_" == "foo_bar_baz"
## expect Str.replaceEach "not here" "/" "_" == "not here"
## ```
replaceEach : Str, Str, Str -> Str
replaceEach = \haystack, needle, flower ->
    when splitFirst haystack needle is
        Ok { before, after } ->
            # We found at least one needle, so start the buffer off with
            # `before` followed by the first replacement flower.
            Str.withCapacity (Str.countUtf8Bytes haystack)
            |> Str.concat before
            |> Str.concat flower
            |> replaceEachHelp after needle flower

        Err NotFound -> haystack

replaceEachHelp : Str, Str, Str, Str -> Str
replaceEachHelp = \buf, haystack, needle, flower ->
    when splitFirst haystack needle is
        Ok { before, after } ->
            buf
            |> Str.concat before
            |> Str.concat flower
            |> replaceEachHelp after needle flower

        Err NotFound -> Str.concat buf haystack

expect Str.replaceEach "abXdeXghi" "X" "_" == "ab_de_ghi"
expect Str.replaceEach "abcdefg" "nothing" "_" == "abcdefg"

## Returns the given [Str] with the first occurrence of a substring replaced.
## If the substring is not found, returns the original string.
##
## ```roc
## expect Str.replaceFirst "foo/bar/baz" "/" "_" == "foo_bar/baz"
## expect Str.replaceFirst "no slashes here" "/" "_" == "no slashes here"
## ```
replaceFirst : Str, Str, Str -> Str
replaceFirst = \haystack, needle, flower ->
    when splitFirst haystack needle is
        Ok { before, after } ->
            "$(before)$(flower)$(after)"

        Err NotFound -> haystack

expect Str.replaceFirst "abXdeXghi" "X" "_" == "ab_deXghi"
expect Str.replaceFirst "abcdefg" "nothing" "_" == "abcdefg"

## Returns the given [Str] with the last occurrence of a substring replaced.
## If the substring is not found, returns the original string.
##
## ```roc
## expect Str.replaceLast "foo/bar/baz" "/" "_" == "foo/bar_baz"
## expect Str.replaceLast "no slashes here" "/" "_" == "no slashes here"
## ```
replaceLast : Str, Str, Str -> Str
replaceLast = \haystack, needle, flower ->
    when splitLast haystack needle is
        Ok { before, after } ->
            "$(before)$(flower)$(after)"

        Err NotFound -> haystack

expect Str.replaceLast "abXdeXghi" "X" "_" == "abXde_ghi"
expect Str.replaceLast "abcdefg" "nothing" "_" == "abcdefg"

## Returns the given [Str] before the first occurrence of a [delimiter](https://www.computerhope.com/jargon/d/delimite.htm), as well
## as the rest of the string after that occurrence.
## Returns [Err NotFound] if the delimiter is not found.
## ```roc
## expect Str.splitFirst "foo/bar/baz" "/" == Ok { before: "foo", after: "bar/baz" }
## expect Str.splitFirst "no slashes here" "/" == Err NotFound
## ```
splitFirst : Str, Str -> Result { before : Str, after : Str } [NotFound]
splitFirst = \haystack, needle ->
    when firstMatch haystack needle is
        Some index ->
            remaining = Str.countUtf8Bytes haystack - Str.countUtf8Bytes needle - index

            before = Str.substringUnsafe haystack 0 index
            after = Str.substringUnsafe haystack (Num.addWrap index (Str.countUtf8Bytes needle)) remaining

            Ok { before, after }

        None ->
            Err NotFound

# splitFirst when needle isn't in haystack
expect splitFirst "foo" "z" == Err NotFound

# splitFirst when needle isn't in haystack, and haystack is empty
expect splitFirst "" "z" == Err NotFound

# splitFirst when haystack ends with needle repeated
expect splitFirst "foo" "o" == Ok { before: "f", after: "o" }

# splitFirst with multi-byte needle
expect splitFirst "hullabaloo" "ab" == Ok { before: "hull", after: "aloo" }

# splitFirst when needle is haystack
expect splitFirst "foo" "foo" == Ok { before: "", after: "" }

firstMatch : Str, Str -> [Some U64, None]
firstMatch = \haystack, needle ->
    haystackLength = Str.countUtf8Bytes haystack
    needleLength = Str.countUtf8Bytes needle
    lastPossible = Num.subSaturated haystackLength needleLength

    firstMatchHelp haystack needle 0 lastPossible

firstMatchHelp : Str, Str, U64, U64 -> [Some U64, None]
firstMatchHelp = \haystack, needle, index, lastPossible ->
    if index <= lastPossible then
        if matchesAt haystack index needle then
            Some index
        else
            firstMatchHelp haystack needle (Num.addWrap index 1) lastPossible
    else
        None

## Returns the given [Str] before the last occurrence of a delimiter, as well as
## the rest of the string after that occurrence.
## Returns [Err NotFound] if the delimiter is not found.
## ```roc
## expect Str.splitLast "foo/bar/baz" "/" == Ok { before: "foo/bar", after: "baz" }
## expect Str.splitLast "no slashes here" "/" == Err NotFound
## ```
splitLast : Str, Str -> Result { before : Str, after : Str } [NotFound]
splitLast = \haystack, needle ->
    when lastMatch haystack needle is
        Some index ->
            remaining = Str.countUtf8Bytes haystack - Str.countUtf8Bytes needle - index

            before = Str.substringUnsafe haystack 0 index
            after = Str.substringUnsafe haystack (Num.addWrap index (Str.countUtf8Bytes needle)) remaining

            Ok { before, after }

        None ->
            Err NotFound

# splitLast when needle isn't in haystack
expect Str.splitLast "foo" "z" == Err NotFound

# splitLast when haystack ends with needle repeated
expect Str.splitLast "foo" "o" == Ok { before: "fo", after: "" }

# splitLast with multi-byte needle
expect Str.splitLast "hullabaloo" "ab" == Ok { before: "hull", after: "aloo" }

# splitLast when needle is haystack
expect Str.splitLast "foo" "foo" == Ok { before: "", after: "" }

lastMatch : Str, Str -> [Some U64, None]
lastMatch = \haystack, needle ->
    haystackLength = Str.countUtf8Bytes haystack
    needleLength = Str.countUtf8Bytes needle
    lastPossibleIndex = Num.subSaturated haystackLength needleLength

    lastMatchHelp haystack needle lastPossibleIndex

lastMatchHelp : Str, Str, U64 -> [Some U64, None]
lastMatchHelp = \haystack, needle, index ->
    if matchesAt haystack index needle then
        Some index
    else
        when Num.subChecked index 1 is
            Ok nextIndex ->
                lastMatchHelp haystack needle nextIndex

            Err _ ->
                None

min = \x, y -> if x < y then x else y

matchesAt : Str, U64, Str -> Bool
matchesAt = \haystack, haystackIndex, needle ->
    haystackLength = Str.countUtf8Bytes haystack
    needleLength = Str.countUtf8Bytes needle
    endIndex = min (Num.addSaturated haystackIndex needleLength) haystackLength

    matchesAtHelp {
        haystack,
        haystackIndex,
        needle,
        needleIndex: 0,
        needleLength,
        endIndex,
    }

matchesAtHelp = \state ->
    { haystack, haystackIndex, needle, needleIndex, needleLength, endIndex } = state
    isAtEndOfHaystack = haystackIndex >= endIndex

    if isAtEndOfHaystack then
        didWalkEntireNeedle = needleIndex == needleLength

        didWalkEntireNeedle
    else
        doesThisMatch =
            Str.getUnsafe haystack haystackIndex
            ==
            Str.getUnsafe needle needleIndex
        doesRestMatch =
            matchesAtHelp
                { state &
                    haystackIndex: Num.addWrap haystackIndex 1,
                    needleIndex: Num.addWrap needleIndex 1,
                }

        doesThisMatch && doesRestMatch

## Walks over the `UTF-8` bytes of the given [Str] and calls a function to update
## state for each byte. The index for that byte in the string is provided
## to the update function.
## ```roc
## f : List U8, U8, U64 -> List U8
## f = \state, byte, _ -> List.append state byte
## expect Str.walkUtf8WithIndex "ABC" [] f == [65, 66, 67]
## ```
walkUtf8WithIndex : Str, state, (state, U8, U64 -> state) -> state
walkUtf8WithIndex = \string, state, step ->
    walkUtf8WithIndexHelp string state step 0 (Str.countUtf8Bytes string)

walkUtf8WithIndexHelp : Str, state, (state, U8, U64 -> state), U64, U64 -> state
walkUtf8WithIndexHelp = \string, state, step, index, length ->
    if index < length then
        byte = Str.getUnsafe string index
        newState = step state byte index

        walkUtf8WithIndexHelp string newState step (Num.addWrap index 1) length
    else
        state

## Walks over the `UTF-8` bytes of the given [Str] and calls a function to update
## state for each byte.
##
## ```roc
## sumOfUtf8Bytes =
##     Str.walkUtf8 "Hello, World!" 0 \total, byte ->
##         total + byte
##
## expect sumOfUtf8Bytes == 105
## ```
walkUtf8 : Str, state, (state, U8 -> state) -> state
walkUtf8 = \str, initial, step ->
    walkUtf8Help str initial step 0 (Str.countUtf8Bytes str)

walkUtf8Help : Str, state, (state, U8 -> state), U64, U64 -> state
walkUtf8Help = \str, state, step, index, length ->
    if index < length then
        byte = Str.getUnsafe str index
        newState = step state byte

        walkUtf8Help str newState step (Num.addWrap index 1) length
    else
        state

expect (walkUtf8 "ABC" [] List.append) == [65, 66, 67]
expect (walkUtf8 "鹏" [] List.append) == [233, 185, 143]

## Shrink the memory footprint of a str such that its capacity and length are equal.
## Note: This will also convert seamless slices to regular lists.
releaseExcessCapacity : Str -> Str

strToNum : Str -> { berrorcode : U8, aresult : Num * }

strToNumHelp : Str -> Result (Num a) [InvalidNumStr]
strToNumHelp = \string ->
    result : { berrorcode : U8, aresult : Num a }
    result = strToNum string

    if result.berrorcode == 0 then
        Ok result.aresult
    else
        Err InvalidNumStr

## Adds a prefix to the given [Str].
## ```roc
## expect Str.withPrefix "Awesome" "Roc" == "RocAwesome"
## ```
withPrefix : Str, Str -> Str
withPrefix = \str, prefix -> Str.concat prefix str

## Determines whether or not the first Str contains the second.
## ```roc
## expect Str.contains "foobarbaz" "bar"
## expect !(Str.contains "apple" "orange")
## expect Str.contains "anything" ""
## ```
contains : Str, Str -> Bool
contains = \haystack, needle ->
    when firstMatch haystack needle is
        Some _index -> Bool.true
        None -> Bool.false

## Drops the given prefix [Str] from the start of a [Str]
## If the prefix is not found, returns the original string.
##
## ```roc
## expect Str.dropPrefix "bar" "foo" == "bar"
## expect Str.dropPrefix "foobar" "foo" == "bar"
## ```
dropPrefix : Str, Str -> Str
dropPrefix = \haystack, prefix ->
    if Str.startsWith haystack prefix then
        start = Str.countUtf8Bytes prefix
        len = Num.subWrap (Str.countUtf8Bytes haystack) start

        substringUnsafe haystack start len
    else
        haystack

## Drops the given suffix [Str] from the end of a [Str]
## If the suffix is not found, returns the original string.
##
## ```roc
## expect Str.dropSuffix "bar" "foo" == "bar"
## expect Str.dropSuffix "barfoo" "foo" == "bar"
## ```
dropSuffix : Str, Str -> Str
dropSuffix = \haystack, suffix ->
    if Str.endsWith haystack suffix then
        start = 0
        len = Num.subWrap (Str.countUtf8Bytes haystack) (Str.countUtf8Bytes suffix)

        substringUnsafe haystack start len
    else
        haystack
