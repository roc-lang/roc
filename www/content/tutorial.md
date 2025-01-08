<input id="tutorial-toc-toggle" name="tutorial-toc-toggle" type= "checkbox">
<nav id="tutorial-toc" aria-label="Table of Contents">
    <label id="close-tutorial-toc" for="tutorial-toc-toggle">close</label>
    <!-- TODO fix search: input [id "toc-search", type "text", placeholder "Search"] [] -->
    <ol>
    <li><a href="#repl">REPL</a></li>
    <li><a href="#building-an-application">Building an Application</a></li>
    <li><a href="#pattern-matching">Pattern Matching</a></li>
    <li><a href="#types">Types</a></li>
    <li><a href="#crashing">Crashing</a></li>
    <li><a href="#testing">Testing</a></li>
    <li><a href="#modules">Modules</a></li>
    <li><a href="#advanced-concepts">Advanced Concepts</a></li>
    </ol>
</nav>
<section id="tutorial-body">
    <section>
        <h1>Tutorial<label id="tutorial-toc-toggle-label" for="tutorial-toc-toggle">contents</label></h1>
        <p>Welcome to Roc!</p>
        <p>This tutorial will teach you how to build Roc applications. Along the way, you'll learn how to write tests, use the REPL, and more!</p>
    </section>
    <div class="banner">
        Roc is a <b>Work in Progress!</b> See the <a href="/plans">plans</a> page for more information.
    </div>
    <section>
        <h2 id="installation"><a href="#installation">Installation</a></h2>
        <p>Roc doesn’t have a numbered release or an installer yet, but you can follow the install instructions for your OS<a href="/install/getting_started.html#installation"> here </a>. If you get stuck, friendly people will be happy to help if you open a topic in<a href="https://roc.zulipchat.com/#narrow/stream/231634-beginners"> #beginners </a>on<a href="https://roc.zulipchat.com/"> Roc Zulip Chat </a>and ask for assistance!</p>
    </section>

## [LLM Docs](#llm-docs) {#llm-docs}

We have experimental LLM-friendly text files for our [tutorial](/llms.txt) and [standard library](/builtins/llms.txt) that you can use to prompt your favorite LLM to answer your questions about Roc!

## [REPL](#repl) {#repl}

Let's start by getting acquainted with Roc's [_Read-Eval-Print-Loop_](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop), or **REPL** for short.

You can use the online REPL at [roc-lang.org/repl](https://www.roc-lang.org/repl).

Or you can run this in a terminal: <code class="block">roc repl</code>, and if Roc is [installed](/install), you should see this:

<pre>
<samp>
  The rockin’ roc repl
────────────────────────

Enter an expression, or :help, or :q to quit.

</samp></pre>

So far, so good!

### [Hello, World!](#hello-world) {#hello-world}

Try typing this in the REPL and pressing Enter:

<pre><samp class="repl-prompt">"Hello, World!"</samp></pre>

The REPL should cheerfully display the following:

<pre><samp><span class="literal">"Hello, World!" </span><span class="colon">:</span> Str</samp></pre>

Congratulations! You've just written your first Roc code.

### [Naming Things](#naming-things) {#naming-things}

When you entered the _expression_ `"Hello, World!"`, the REPL printed it back out. It also printed `: Str`, because `Str` is that expression's type. We'll talk about types later; for now, let's ignore the `:` and whatever comes after it whenever we see them.

You can assign specific names to expressions. Try entering these lines:

```
greeting = "Hi"
audience = "World"
```

From now until you exit the REPL, you can refer to either `greeting` or `audience` by those names! We'll use these later on in the tutorial.

### [Arithmetic](#arithmetic) {#arithmetic}

Now let's try using an _operator_, specifically the `+` operator. Enter this:

<pre><samp class="repl-prompt">1 <span class="op">+</span> 1</samp></pre>

You should see this output:

<pre><samp>2 <span class="colon">:</span> Num * <span class="comment"></span></samp></pre>

According to the REPL, one plus one equals two. Sounds right!

Roc will respect [order of operations](https://en.wikipedia.org/wiki/Order_of_operations) when using multiple arithmetic operators like `+` and `-`, but you can use parentheses to specify exactly how they should be grouped.

<pre><samp><span class="repl-prompt">1 <span class="op">+</span> 2 <span class="op">*</span> (3 <span class="op">-</span> 4)

-1 <span class="colon">:</span> Num *
</span></samp></pre>

### [Calling Functions](#calling-functions) {#calling-functions}

Let's try calling a function:

<pre><samp><span class="repl-prompt">Str.concat "Hi " "there."</span>

<span class="literal">"Hi there."</span> <span class="colon">:</span> Str
</samp></pre>

Here we're calling the `Str.concat` function and passing two arguments: the string `"Hi "` and the string `"there."`. This _concatenates_ the two strings together (that is, it puts one after the other) and returns the resulting combined string of `"Hi there."`.

Note that in Roc, we don't need parentheses or commas to call functions. We don't write `Str.concat("Hi ", "there.")` but rather `Str.concat "Hi " "there."`.

That said, just like in the arithmetic example above, we can use parentheses to specify how nested function calls should work. For example, we could write this:

<pre><samp><span class="repl-prompt">Str.concat "Birds: " (Num.toStr 42)</span>

<span class="literal">"Birds: 42"</span> <span class="colon">:</span> Str
</samp></pre>

This calls `Num.toStr` on the number `42`, which converts it into the string `"42"`, and then passes that string as the second argument to `Str.concat`.

The parentheses are important here to specify how the function calls nest. Try removing them, and see what happens:

<pre><samp><span class="repl-prompt">Str.concat "Birds: " Num.toStr 42</span>

<span class="repl-err">&lt;error&gt;</span>
</samp></pre>

The error tells us that we've given `Str.concat` too many arguments. Indeed we have! We've passed it three arguments:

1.  The string `"Birds"`
2.  The function `Num.toStr`
3.  The number `42`

That's not what we intended to do. Putting parentheses around the `Num.toStr 42` call clarifies that we want it to be evaluated as its own expression, rather than being two arguments to `Str.concat`.

Both the `Str.concat` function and the `Num.toStr` function have a dot in their names. In `Str.concat`, `Str` is the name of a _module_, and `concat` is the name of a function inside that module. Similarly, `Num` is a module, and `toStr` is a function inside that module.

We'll get into more depth about modules later, but for now you can think of a module as a named collection of functions. Eventually we'll discuss how to use them for more than that.

### [String Interpolation](#string-interpolation) {#string-interpolation}

An alternative syntax for `Str.concat` is _string interpolation_, which looks like this:

<pre><samp class="repl-prompt"><span class="literal">"<span class="str-esc">$(</span><span class="str-interp">greeting</span><span class="str-esc">)</span> there, <span class="str-esc">$(</span><span class="str-interp">audience</span><span class="str-esc">)</span>."</span></samp></pre>

This is syntax sugar for calling `Str.concat` several times, like so:

```roc
Str.concat greeting (Str.concat " there, " (Str.concat audience "."))
```

You can put entire single-line expressions inside the parentheses in string interpolation. For example:

<pre><samp class="repl-prompt"><span class="literal">"Two plus three is: <span class="str-esc">$(</span><span class="str-interp">Num.toStr (2 + 3)</span><span class="str-esc">)</span>"</span></samp></pre>

By the way, there are many other ways to put strings together! Check out the [documentation](https://www.roc-lang.org/builtins/Str) for the `Str` module for more.

## [Building an Application](#building-an-application) {#building-an-application}

Let's move out of the REPL and create our first Roc application!

Make a file named `main.roc` and put this in it:

```roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.18.0/0APbwVN1_p1mJ96tXjaoiUCr8NBGamr8G8Ac_DrXR-o.tar.br" }

import pf.Stdout

main! = \_args ->
    Stdout.line! "Hi there, from inside a Roc app. 🎉"
```

Try running this with:

<samp>roc main.roc</samp>

You should see a message about a file being downloaded, followed by this:

<samp>Hi there, from inside a Roc app. 🎉</samp>

Congratulations, you've written your first Roc application! We'll go over what the parts above `main` do later, but let's play around a bit first.

### [Defs](#defs) {#defs}

Try replacing the `main` line with this:

```roc
birds = 3

iguanas = 2

total = Num.toStr (birds + iguanas)

main! = \_args ->
    Stdout.line! "There are $(total) animals."
```

Now run `roc main.roc` again. This time the "Downloading ..." message won't appear; the file has been cached from last time, and won't need to be downloaded again.

You should see this:

<samp>There are 5 animals.</samp>

`main.roc` now has four definitions (_defs_ for short) `birds`, <span class="nowrap">`iguanas`,</span> `total`, and `main!`.

A definition names an expression.

- The first two defs assign the names `birds` and `iguanas` to the expressions `3` and `2`.
- The next def assigns the name `total` to the expression `Num.toStr (birds + iguanas)`.

Once we have a def, we can use its name in other expressions. For example, the `total` expression refers to `birds` and `iguanas`, and `Stdout.line! "There are $(total) animals."` refers to `total`.

You can name a def using any combination of letters and numbers, but they have to start with a lowercase letter.

**Note:** Defs are constant; they can't be reassigned. We'd get an error if we wrote these two defs in the same scope:

```roc
birds = 3
birds = 2
```

### [Defining Functions](#defining-functions) {#defining-functions}

So far we've called functions like `Num.toStr`, `Str.concat`, and `Stdout.line`. Next let's try defining a function of our own.

```roc
birds = 3

iguanas = 2

total = add_and_stringify birds iguanas

main! = \_args ->
    Stdout.line! "There are $(total) animals."

add_and_stringify = \num1, num2 ->
    Num.toStr (num1 + num2)
```

This new `add_and_stringify` function we've defined accepts two numbers, adds them, calls `Num.toStr` on the result, and returns that.

The `\num1, num2 ->` syntax defines a function's arguments, and the expression after the `->` is the body of the function. Whenever a function gets called, its body expression gets evaluated and returned.

### [`if`-`then`-`else` expressions](#if-then-else) {#if-then-else}

Let's modify this function to return an empty string if the numbers add to zero.

```roc
add_and_stringify = \num1, num2 ->
    sum = num1 + num2

    if sum == 0 then
        ""
    else
        Num.toStr sum
```

We did two things here:

- We introduced a _local def_ named `sum`, and set it equal to `num1 + num2`. Because we defined `sum` inside `add_and_stringify`, it's _local_ to that scope and can't be accessed outside that function.
- We added an `if`\-`then`\-`else` conditional to return either `""` or `Num.toStr sum` depending on whether `sum == 0`.

Every `if` must be accompanied by both `then` and also `else`. Having an `if` without an `else` is an error, because `if` is an expression, and all expressions must evaluate to a value. If there were ever an `if` without an `else`, that would be an expression that might not evaluate to a value!

### [`else if` expressions](#else-if) {#else-if}

We can combine `if` and `else` to get `else if`, like so:

```roc
add_and_stringify = \num1, num2 ->
    sum = num1 + num2

    if sum == 0 then
        ""
    else if sum < 0 then
        "negative"
    else
        Num.toStr sum
```

Note that `else if` is not a separate language keyword! It's just an `if`/`else` where the `else` branch contains another `if`/`else`. This is easier to see with different indentation:

```roc
add_and_stringify = \num1, num2 ->
    sum = num1 + num2

    if sum == 0 then
        ""
    else
        if sum < 0 then
            "negative"
        else
            Num.toStr sum
```

This differently-indented version is equivalent to writing `else if sum < 0 then` on the same line, although the convention is to use the original version's style.

### [Comments](#comments) {#comments}

This is a comment in Roc:

```roc
# The 'name' field is unused by add_and_stringify
```

Whenever you write `#` it means that the rest of the line is a comment, and will not affect the
running program. Roc does not have multiline comment syntax.

### [Doc Comments](#doc-comments) {#doc-comments}

Comments that begin with `##` are "doc comments" which will be included in generated documentation (`roc docs`). They can include code blocks by adding five spaces after `##`.

```roc
## This is a comment for documentation, and includes a code block.
##
##     x = 2
##     expect x == 2
```

Like other comments, doc comments do not affect the running program.

### [Records](#records) {#records}

Currently our `add_and_stringify` function takes two arguments. We can instead make it take one argument like so:

```roc
total = add_and_stringify { birds: 5, iguanas: 7 }

add_and_stringify = \counts ->
    Num.toStr (counts.birds + counts.iguanas)
```

The function now takes a _record_, which is a group of named values. Records are not [objects](<https://en.wikipedia.org/wiki/Object_(computer_science)>); they don't have methods or inheritance, they just store information.

The expression `{ birds: 5, iguanas: 7 }` defines a record with two _fields_ (the `birds` field and the `iguanas` field) and then assigns the number `5` to the `birds` field and the number `7` to the `iguanas` field. Order doesn't matter with record fields; we could have also specified `iguanas` first and `birds` second, and Roc would consider it the exact same record.

When we write `counts.birds`, it accesses the `birds` field of the `counts` record, and when we write `counts.iguanas` it accesses the `iguanas` field.

When we use [`==`](/builtins/Bool#isEq) on records, it compares all the fields in both records with [`==`](/builtins/Bool#isEq), and only considers the two records equal if all of their fields are equal. If one record has more fields than the other, or if the types associated with a given field are different between one field and the other, the Roc compiler will give an error at build time.

> **Note:** Some other languages have a concept of "identity equality" that's separate from the "structural equality" we just described. Roc does not have a concept of identity equality; this is the only way equality works!

### [Accepting extra fields](#accepting-extra-fields) {#accepting-extra-fields}

The `add_and_stringify` function will accept any record with at least the fields `birds` and `iguanas`, but it will also accept records with more fields. For example:

```roc
total = add_and_stringify { birds: 5, iguanas: 7 }

# The `note` field is unused by add_and_stringify
total_with_note = add_and_stringify { birds: 4, iguanas: 3, note: "Whee!" }

add_and_stringify = \counts ->
    Num.toStr (counts.birds + counts.iguanas)
```

This works because `add_and_stringify` only uses `counts.birds` and `counts.iguanas`. If we were to use `counts.note` inside `add_and_stringify`, then we would get an error because `total` is calling `add_and_stringify` passing a record that doesn't have a `note` field.

### [Record shorthands](#record-shorthands) {#record-shorthands}

Roc has a couple of shorthands you can use to express some record-related operations more concisely.

Instead of writing `\record -> record.x` we can write `.x` and it will evaluate to the same thing: a function that takes a record and returns its `x` field. You can do this with any field you want. For example:

```roc
# returnFoo is a function that takes a record
# and returns the `foo` field of that record.
returnFoo = .foo

returnFoo { foo: "hi!", bar: "blah" }
# returns "hi!"
```

Sometimes we assign a def to a field that happens to have the same name—for example, `{ x: x }`.
In these cases, we shorten it to writing the name of the def alone—for example, `{ x }`. We can do this with as many fields as we like; here are several different ways to define the same record:

- `{ x: x, y: y }`
- `{ x, y }`
- `{ x: x, y }`
- `{ x, y: y }`

### [Record destructuring](#record-destructuring) {#record-destructuring}

We can use _destructuring_ to avoid naming a record in a function argument, instead giving names to its individual fields:

```roc
add_and_stringify = \{ birds, iguanas } ->
    Num.toStr (birds + iguanas)
```

Here, we've _destructured_ the record to create a `birds` def that's assigned to its `birds` field, and an `iguanas` def that's assigned to its `iguanas` field. We can customize this if we like:

```roc
add_and_stringify = \{ birds, iguanas: lizards } ->
    Num.toStr (birds + lizards)
```

In this version, we created a `lizards` def that's assigned to the record's `iguanas` field. (We could also do something similar with the `birds` field if we like.)

Finally, destructuring can be used in defs too:

```roc
{ x, y } = { x: 5, y: 10 }
```

### [Making records from other records](#making-records-from-other-records) {#making-records-from-other-records}

So far we've only constructed records from scratch, by specifying all of their fields. We can also construct new records by using another record to use as a starting point, and then specifying only the fields we want to be different. For example, here are two ways to get the same record:

```roc
original = { birds: 5, zebras: 2, iguanas: 7, goats: 1 }
from_scratch = { birds: 4, zebras: 2, iguanas: 3, goats: 1 }
from_original = { original & birds: 4, iguanas: 3 }
```

The `from_scratch` and `from_original` records are equal, although they're defined in different ways.

- `from_scratch` was built using the same record syntax we've been using up to this point.
- `from_original` created a new record using the contents of `original` as defaults for fields that it didn't specify after the `&`.

Note that `&` can't introduce new fields to a record, or change the types of existing fields.
(Trying to do either of these will result in an error at build time!)

### [Debugging with `dbg`](#dbg) {#dbg}

[Print debugging](https://en.wikipedia.org/wiki/Debugging#Techniques) is the most common debugging technique in the history of programming, and Roc has a `dbg` keyword to facilitate it. Here's an example of how to use `dbg`:

```roc
pluralize = \singular, plural, count ->
    dbg count

    if count == 1 then
        singular
    else
        plural
```

Whenever this `dbg` line of code is reached, the value of `count` will be printed to [stderr](<https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)>), along with the source code file and line number where the `dbg` itself was written:

<samp><span class="kw">[pluralize.roc 6:8]</span> 5</samp>

Here, `[pluralize.roc 6:8]` tells us that this `dbg` was written in the file `pluralize.roc` on line 6, column 8.

You can give `dbg` any expression you like, for example:

```roc
dbg Str.concat singular plural
```

You can also use `dbg` as a function inside an expression, which will print the function argument to stderr and then return the argument to the caller. For example:

```roc
inc = \n -> 1 + dbg n
```

### [Tuples](#tuples) {#tuples}

One way to have `dbg` print multiple values at a time is to wrap them in a record:

```roc
dbg { text: "the value of count is:", value: count }
```

A more concise way would be to wrap them in a _tuple_, like so:

```roc
dbg ("the value of count is:", count)
```

Visually, tuples in Roc look like lists (but with parentheses instead of square brackets). However, a tuple is much more like a record - in fact, tuples and records compile down to the exact same representation at runtime! So anywhere you would use a tuple, you can use a record instead, and the in-memory representation will be exactly the same.

Like records, tuples are fixed-length and can't be iterated over. Also, they can contain values of different types. The difference is that in a record, each field is labeled (and their position doesn't matter), whereas in a tuple, each field is specified by its position.

### [Accessing values in tuples](#tuple-access) {#tuple-access}

Just like how there are two ways to access a record's fields (namely, the `.` operator and [record destructuring](#record-destructuring)), there are also two similar ways to access tuple fields:

```roc
# tuple field access
tuple = ("hello", 42, ["list"])

first = tuple.0 # "hello"
second = tuple.1 # 42
third = tuple.2 # ["list"]
```

```roc
# tuple destructuring
(first, second, third) = ("hello", 42, ["list"])
```

<details>
    <summary>Pronouncing Tuple</summary>
    By the way, there are two common ways to pronounce "tuple"—one sounds like "two-pull" and the other rhymes with "supple"—and although no clear consensus has emerged in the programming world, people seem generally accepting when others pronounce it differently than they do.
</details>

## [Pattern Matching](#pattern-matching) {#pattern-matching}

Sometimes we want to represent that something can have one of several values. For example:

```roc
stoplight_color =
    if something > 0 then
        Red
    else if something == 0 then
        Yellow
    else
        Green
```

Here, `stoplight_color` can have one of three values: `Red`, `Yellow`, or `Green`. The capitalization is very important! If these were lowercase (`red`, `yellow`, `green`), then they would refer to defs. However, because they are capitalized, they instead refer to _tags_.

### [Tags](#tags) {#tags}

A tag is a literal value just like a number or a string. Similarly to how I can write the number `42` or the string `"forty-two"` without defining them first, I can also write the tag `FortyTwo` without defining it first. Also, similarly to how `42 == 42` and `"forty-two" == "forty-two"`, it's also the case that `FortyTwo == FortyTwo`.

Let's say we wanted to turn `stoplight_color` from a `Red`, `Green`, or `Yellow` into a string. Here's one way we could do that:

```roc
stoplight_str =
    if stoplight_color == Red then
        "red"
    else if stoplight_color == Green then
        "green"
    else
        "yellow"
```

We can express this logic more concisely using `when`/`is` instead of `if`/`then`:

```roc
stoplight_str =
    when stoplight_color is
        Red -> "red"
        Green -> "green"
        Yellow -> "yellow"
```

This results in the same value for `stoplight_str`. In both the `when` version and the `if` version, we have three conditional branches, and each of them evaluates to a string. The difference is how the conditions are specified; here, we specify between `when` and `is` that we're making comparisons against `stoplight_color`, and then we specify the different things we're comparing it to: `Red`, `Green`, and `Yellow`.

Besides being more concise, there are other advantages to using `when` here.

1.  We don't have to specify an `else` branch, so the code can be more self-documenting about exactly what all the options are.
2.  We get more compiler help. If we try deleting any of these branches, we'll get a compile-time error saying that we forgot to cover a case that could come up. For example, if we delete the `Green ->` branch, the compiler will say that we didn't handle the possibility that `stoplight_color` could be `Green`. It knows this because `Green` is one of the possibilities in our `stoplight_color = if ...` definition.

We can still have the equivalent of an `else` branch in our `when` if we like. Instead of writing `else`, we write `_ ->` like so:

```roc
stoplight_str =
    when stoplight_color is
        Red -> "red"
        _ -> "not red"
```

This lets us more concisely handle multiple cases. However, it has the downside that if we add a new case - for example, if we introduce the possibility of `stoplight_color` being `Orange`, the compiler can no longer tell us we forgot to handle that possibility in our `when`. After all, we are handling it - just maybe not in the way we'd decide to if the compiler had drawn our attention to it!

We can make this `when` _exhaustive_ (that is, covering all possibilities) without using `_ ->` by using `|` to specify multiple matching conditions for the same branch:

```roc
stoplight_str =
    when stoplight_color is
        Red -> "red"
        Green | Yellow -> "not red"
```

You can read `Green | Yellow` as "either `Green` or `Yellow`". By writing it this way, if we introduce the possibility that `stoplight_color` can be `Orange`, we'll get a compiler error telling us we forgot to cover that case in this `when`, and then we can handle it however we think is best.

We can also combine `if` and `when` to make branches more specific:

```roc
stoplight_str =
    when stoplight_color is
        Red -> "red"
        Green | Yellow if contrast > 75 -> "not red, but very high contrast"
        Green | Yellow if contrast > 50 -> "not red, but high contrast"
        Green | Yellow -> "not red"
```

This will give the same answer for `stoplight_str` as if we had written the following:

```roc
stoplight_str =
    when stoplight_color is
        Red -> "red"
        Green | Yellow ->
            if contrast > 75 then
                "not red, but very high contrast"
            else if contrast > 50 then
                "not red, but high contrast"
            else
                "not red"
```

Either style can be a reasonable choice depending on the circumstances.

### [Tags with payloads](#tags-with-payloads) {#tags-with-payloads}

Tags can have _payloads_—that is, values inside them. For example:

```roc
stoplight_color =
    if something > 100 then
        Red
    else if something > 0 then
        Yellow
    else if something == 0 then
        Green
    else
        Custom "some other color"

stoplight_str =
    when stoplight_color is
        Red -> "red"
        Green | Yellow -> "not red"
        Custom description -> description
```

This makes two changes to our earlier `stoplight_color` / `stoplight_str` example.

1.  We sometimes chose to set `stoplight_color` to be `Custom "some other color"`. When we did this, we gave the `Custom` tag a _payload_ of the string `"some other color"`.
2.  We added a `Custom` tag in our `when`, with a payload which we named `description`. Because we did this, we were able to refer to `description` in the body of the branch (that is, the part after the `->`) just like a def or a function argument.

Any tag can be given a payload like this. A payload doesn't have to be a string; we could also have said (for example) `Custom { r: 40, g: 60, b: 80 }` to specify an RGB color instead of a string. Then in our `when` we could have written `Custom record ->` and then after the `->` used `record.r`, `record.g`, and `record.b` to access the `40`, `60`, `80` values. We could also have written `Custom { r, g, b } ->` to _destructure_ the record, and then accessed these `r`, `g`, and `b` defs after the `->` instead.

A tag can also have a payload with more than one value. Instead of `Custom { r: 40, g: 60, b: 80 }` we could write `Custom 40 60 80`. If we did that, then instead of destructuring a record with `Custom { r, g, b } ->` inside a `when`, we would write `Custom r g b ->` to destructure the values directly out of the payload.

We refer to whatever comes before a `->` in a `when` expression as a _pattern_—so for example, in the `Custom description -> description` branch, `Custom description` would be a pattern. In programming, using patterns in branching conditionals like `when` is known as [pattern matching](https://en.wikipedia.org/wiki/Pattern_matching). You may hear people say things like "let's pattern match on `Custom` here" as a way to suggest making a `when` branch that begins with something like `Custom description ->`.

### [Booleans](#booleans) {#booleans}

In many programming languages, `true` and `false` are special language keywords that refer to the two [boolean](https://en.wikipedia.org/wiki/Boolean_data_type) values. In Roc, booleans do not get special keywords; instead, they are exposed as the ordinary values `Bool.true` and `Bool.false`.

This design is partly to keep the number of special keywords in the language smaller, but mainly to suggest how booleans are intended to be used in Roc: for [_boolean logic_](https://en.wikipedia.org/wiki/Boolean_algebra) (`&&`, `||`, and so on) as opposed to for data modeling. Tags are the preferred choice for data modeling, and having tag values be more concise than boolean values helps make this preference clear.

As an example of why tags are encouraged for data modeling, in many languages it would be common to write a record like `{ name: "Richard", isAdmin: Bool.true }`, but in Roc it would be preferable to write something like `{ name: "Richard", role: Admin }`. At first, the `role` field might only ever be set to `Admin` or `Normal`, but because the data has been modeled using tags instead of booleans, it's much easier to add other alternatives in the future, like `Guest` or `Moderator` - some of which might also want payloads.

### [Lists](#lists) {#lists}

Another thing we can do in Roc is to make a _list_ of values. Here's an example:

```roc
names = ["Sam", "Lee", "Ari"]
```

This is a list with three elements in it, all strings. We can add a fourth element using `List.append` like so:

```roc
List.append names "Jess"
```

This returns a **new** list with `"Jess"` after `"Ari"`, and doesn't modify the original list at all. All values in Roc (including lists, but also records, strings, numbers, and so on) are immutable, meaning whenever we want to "change" them, we want to instead pass them to a function which returns some variation of what was passed in.

### [List.map](#list-map) {#list-map}

A common way to transform one list into another is to use `List.map`. Here's an example of how to use it:

```roc
List.map [1, 2, 3] \num -> num * 2
```

This returns `[2, 4, 6]`.

`List.map` takes two arguments:

1.  An input list
2.  A function that will be called on each element of that list

It then returns a list which it creates by calling the given function on each element in the input list. In this example, `List.map` calls the function `\num -> num * 2` on each element in `[1, 2, 3]` to get a new list of `[2, 4, 6]`.

We can also give `List.map` a named function, instead of an anonymous one:

```roc
List.map [1, 2, 3] Num.isOdd
```

This `Num.isOdd` function returns `Bool.true` if it's given an odd number, and `Bool.false` otherwise. So `Num.isOdd 5` returns `Bool.true` and `Num.isOdd 2` returns `Bool.false`.

As such, calling `List.map [1, 2, 3] Num.isOdd` returns a new list of `[Bool.true, Bool.false, Bool.true]`.

### [List element type compatibility](#list-element-type-compatibility) {#list-element-type-compatibility}

If we tried to give `List.map` a function that didn't work on the elements in the list, then we'd get an error at compile time. Here's a valid, and then an invalid example:

```roc
# working example
List.map [-1, 2, 3, -4] Num.isNegative
# returns [Bool.true, Bool.false, Bool.false, Bool.true]
```

```roc
# invalid example
List.map ["A", "B", "C"] Num.isNegative
# error: isNegative doesn't work on strings!
```

Because `Num.isNegative` works on numbers and not strings, calling `List.map` with `Num.isNegative` and a list of numbers works, but doing the same with a list of strings doesn't work.

This wouldn't work either:

```roc
List.map ["A", "B", "C", 1, 2, 3] Num.isNegative
```

Every element in a Roc list has to share the same type. For example, we can have a list of strings like `["Sam", "Lee", "Ari"]`, or a list of numbers like `[1, 2, 3, 4, 5]` but we can't have a list which mixes strings and numbers like `["Sam", 1, "Lee", 2, 3]`, that would be a compile-time error.

Ensuring that all elements in a list share a type eliminates entire categories of problems. For example, it means that whenever you use `List.append` to add elements to a list, as long as you don't have any compile-time errors, you won't get any runtime errors from calling `List.map` afterwards, no matter what you appended to the list! More generally, it's safe to assume that unless you run out of memory, `List.map` will run successfully unless you got a compile-time error about an incompatibility (like `Num.neg` on a list of strings).

### [Lists that hold elements of different types](#lists-that-hold-elements-of-different-types) {#lists-that-hold-elements-of-different-types}

We can use tags with payloads to make a list that contains a mixture of different types. For example:

```roc
List.map [StrElem "A", StrElem "b", NumElem 1, StrElem "c", NumElem -3] \elem ->
    when elem is
        NumElem num -> Num.isNegative num
        StrElem str -> Str.startsWith str "A"
# returns [Bool.true, Bool.false, Bool.false, Bool.false, Bool.true]
```

Compare this with the example from earlier, which caused a compile-time error:

```roc
List.map ["A", "B", "C", 1, 2, 3] Num.isNegative
```

The version that uses tags works because we aren't trying to call `Num.isNegative` on each element. Instead, we're using a `when` to tell when we've got a string or a number, and then calling either `Num.isNegative` or `Str.startsWith` depending on which type we have.

We could take this as far as we like, adding more different tags (e.g. `BoolElem Bool.true`) and then adding more branches to the `when` to handle them appropriately.

### [Using tags as functions](#using-tags-as-functions) {#using-tags-as-functions}

Let's say I want to apply a tag to a bunch of elements in a list. For example:

```roc
List.map ["a", "b", "c"] \str -> Foo str
```

This is a perfectly reasonable way to write it, but I can also write it like this:

```roc
List.map ["a", "b", "c"] Foo
```

These two versions compile to the same thing. As a convenience, Roc lets you specify a tag name where a function is expected; when you do this, the compiler infers that you want a function which uses all of its arguments as the payload to the given tag.

### [List.any and List.all](#list-any-and-list-all) {#list-any-and-list-all}

There are several functions that work like `List.map`, they walk through each element of a list and do something with it. Another is `List.any`, which returns `Bool.true` if calling the given function on any element in the list returns `Bool.true`:

```roc
List.any [1, 2, 3] Num.isOdd
# returns `Bool.true` because 1 and 3 are odd
```

```roc
List.any [1, 2, 3] Num.isNegative
# returns `Bool.false` because none of these is negative
```

There's also `List.all` which only returns `Bool.true` if all the elements in the list pass the test:

```roc
List.all [1, 2, 3] Num.isOdd
# returns `Bool.false` because 2 is not odd
```

```roc
List.all [1, 2, 3] Num.isPositive
# returns `Bool.true` because all of these are positive
```

### [Removing elements from a list](#removing-elements-from-a-list) {#removing-elements-from-a-list}

You can also drop elements from a list. One way is `List.dropAt` - for example:

```roc
List.dropAt ["Sam", "Lee", "Ari"] 1
# drops the element at offset 1 ("Lee") and returns ["Sam", "Ari"]
```

Another way is to use `List.keepIf`, which passes each of the list's elements to the given function, and then keeps them only if that function returns `Bool.true`.

```roc
List.keepIf [1, 2, 3, 4, 5] Num.isEven
# returns [2, 4]
```

There's also `List.dropIf`, which does the opposite:

```roc
List.dropIf [1, 2, 3, 4, 5] Num.isEven
# returns [1, 3, 5]
```

### [Getting an individual element from a list](#getting-an-individual-element-from-a-list) {#getting-an-individual-element-from-a-list}

Another thing we can do with a list is to get an individual element out of it. `List.get` is a common way to do this; it takes a list and an index, and then returns the element at that index... if there is one. But what if there isn't?

For example, what do each of these return?

```roc
List.get ["a", "b", "c"] 1
```

```roc
List.get ["a", "b", "c"] 100
```

The answer is that the first one returns `Ok "b"` and the second one returns `Err OutOfBounds`. They both return tags! This is done so that the caller becomes responsible for handling the possibility that the index is outside the bounds of that particular list.

Here's how calling `List.get` can look in practice:

```roc
when List.get ["a", "b", "c"] index is
    Ok str -> "I got this string: $(str)"
    Err OutOfBounds -> "That index was out of bounds, sorry!"
```

There's also `List.first`, which always gets the first element, and `List.last` which always gets the last. They return `Err ListWasEmpty` instead of `Err OutOfBounds`, because the only way they can fail is if you pass them an empty list!

### [Error Handling](#error-handling) {#error-handling}

The `List` functions such as `List.get`, `List.first`, and `List.last` demonstrate a common pattern in Roc: operations that can fail returning either an `Ok` tag with the answer (if successful), or an `Err` tag with another tag describing what went wrong (if unsuccessful). In fact, it's such a common pattern that there's a whole module called `Result` which deals with these two tags. Here are some examples of `Result` functions:

```roc
Result.withDefault (List.get ["a", "b", "c"] 100) ""
# returns "" because that's the default we said to use if List.get returned an Err
```

```roc
Result.isOk (List.get ["a", "b", "c"] 1)
# returns `Bool.true` because `List.get` returned an `Ok` tag. (The payload gets ignored.)

# Note: There's a Result.isErr function that works similarly.
```

```roc
# Running this will produce `Ok "c"`
Result.try (Str.toU64 "2") listGet

listGet : U64 -> Result Str [OutOfBounds]
listGet = \index ->
    List.get ["a", "b", "c", "d"] index

# Notes:
#  - `Str.toU64 "2"` parses the string "2" to the integer 2, and returns `Ok 2` (more on
#    integer types later)
#  - since parsing is successful, `Result.try` passes 2 to the `listGet` function
#  - passing "abc" or "1000" instead of "2" would have resulted in `Err InvalidNumStr`
#    or `Err OutOfBounds` respectively
```

`Result.try` is often used to chain two functions that return `Result` (as in the example above). This prevents you from needing to add error handling code at every intermediate step.

### [The `try` keyword](#the-try-keyword) {#the-try-keyword}

Roc has a `try` keyword, which is convenient syntax sugar for `Result.try`. For example, consider the following `get_letter` function:

```roc
get_letter : Str -> Result Str [OutOfBounds, InvalidNumStr]
get_letter = \index_str ->
    index = try Str.toU64 index_str
    List.get ["a", "b", "c", "d"] index
```

Here's what this does:

- If the `Str.toU64` function returns an `Ok` value, then `try` will return what's inside the `Ok`. For example:
  - If we call `get_letter "2"`, then `Str.toU64` returns `Ok 2`, and the `try` unwraps to the integer 2, so `index` is set to 2 (not `Ok 2`). Then the `List.get` function is called and returns `Ok "c"`.
  - If the `Str.toU64` function returns an `Err` value, then the `try` keyword immediately interrupts the `get_letter` function and makes it return this error.
  - For example, if we call `get_letter "abc"`, then the call to `Str.toU64` returns `Err InvalidNumStr`, and the `try` keyword ensures that the `get_letter` function returns this error immediately, without executing the rest of the function.

Thanks to the `try` keyword, your code can focus on the "happy path" (where nothing fails) and simply bubble up to the caller any error that might occur. Your error handling code can be neatly separated, and you can rest assured that you won't forget to handle any errors, since the compiler will let you know. See this [code example](https://github.com/roc-lang/examples/blob/main/examples/Results/main.roc) for more details on error handling.

Now let's get back to lists!

### [Walking the elements in a list](#walking-the-elements-in-a-list) {#walking-the-elements-in-a-list}

We've now seen a few different ways you can transform lists. Sometimes, though, there's nothing
that quite does what you want, and you might find yourself calling `List.get` repeatedly to
retrieve every element in the list and use it to build up the new value you want. That approach
can work, but it has a few downsides:

- Each `List.get` call returns a `Result` that must be dealt with, even though you plan to use every element in the list anyway
- There's a runtime performance overhead associated with each of these `Result`s, which you won't find in other "look at every element in the list" operations like `List.keepIf`.
- It's more verbose than the alternative we're about to discuss

The `List.walk` function gives you a way to walk over the elements in a list and build up whatever
return value you like. It's a great alternative to calling `List.get` on every element in the list
because it's more concise, runs faster, and doesn't give you any `Result`s to deal with.

Here's an example:

```roc
List.walk [1, 2, 3, 4, 5] { evens: [], odds: [] } \state, elem ->
    if Num.isEven elem then
        { state & evens: List.append state.evens elem }
    else
        { state & odds: List.append state.odds elem }

# returns { evens: [2, 4], odds: [1, 3, 5] }
```

In this example, we walk over the list `[1, 2, 3, 4, 5]` and add each element to either the `evens` or `odds` field of a `state` record: `{ evens, odds }`. By the end, that record has a list of all the even numbers in the list and a list of all the odd numbers.

`List.walk` takes a few ingredients:

1. A list. (`[1, 2, 3, 4, 5]`)
2. An initial `state` value. (`{ evens: [], odds: [] }`)
3. A function which takes the current `state` and element, and returns a new `state`. (`\state, elem -> ...`)

It then proceeds to walk over each element in the list and call that function. Each time, the state that function returns becomes the argument to the next function call. Here are the arguments the function will receive, and what it will return, as `List.walk` walks over the list `[1, 2, 3, 4, 5]`:

| State                             | Element | Return Value                         |
| --------------------------------- | ------- | ------------------------------------ |
| `{ evens: [], odds: [] }`         | `1`     | `{ evens: [], odds: [1] }`           |
| `{ evens: [], odds: [1] }`        | `2`     | `{ evens: [2], odds: [1] }`          |
| `{ evens: [2], odds: [1] }`       | `3`     | `{ evens: [2], odds: [1, 3] }`       |
| `{ evens: [2], odds: [1, 3] }`    | `4`     | `{ evens: [2, 4], odds: [1, 3] }`    |
| `{ evens: [2, 4], odds: [1, 3] }` | `5`     | `{ evens: [2, 4], odds: [1, 3, 5] }` |

Note that the initial `state` argument is `{ evens: [], odds: [] }` because that's the argument
we passed `List.walk` for its initial state. From then on, each `state` argument is whatever the
previous function call returned.

Once the list has run out of elements, `List.walk` returns whatever the final function call returned—in this case, `{ evens: [2, 4], odds: [1, 3, 5] }`. (If the list was empty, the function never gets called and `List.walk` returns the initial state.)

Note that the state doesn't have to be a record; it can be anything you want. For example, if you made it a `Bool`, you could implement `List.any` using `List.walk`. You could also make the state be a list, and implement `List.map`, `List.keepIf`, or `List.dropIf`. There are a lot of things you can do with `List.walk`!

A helpful way to remember the argument order for `List.walk` is that that its arguments follow the same pattern as what we've seen with `List.map`, `List.any`, `List.keepIf`, and `List.dropIf`: the first argument is a list, and the last argument is a function. The difference here is that `List.walk` has one more argument than those other functions; the only place it could go while preserving that pattern is in the middle!

> **Note:** Other languages give this operation different names, such as `fold`, `reduce`, `accumulate`, `aggregate`, `compress`, and `inject`. Consider using one of the following if you would like to call an effectful function on a list of

```roc
List.forEach! : List a, (a => {}) => {}
List.forEachTry! : List a, (a => Result {} err) => Result {} err
```

### [Pattern Matching on Lists](#pattern-matching-on-lists) {#pattern-matching-on-lists}

You can also pattern match on lists, like so:

```roc
when my_list is
    [] -> 0 # the list is empty
    [Foo, ..] -> 1 # it starts with a Foo tag
    [_, ..] -> 2 # it contains at least one element, which we ignore
    [Foo, Bar, ..] -> 3 # it starts with a Foo tag followed by a Bar tag
    [Foo, Bar, Baz] -> 4 # it has exactly 3 elements: Foo, Bar, and Baz
    [Foo, a, ..] -> 5 # its first element is Foo, and its second we name `a`
    [Ok a, ..] -> 6 # it starts with an Ok containing a payload named `a`
    [.., Foo] -> 7 # it ends with a Foo tag
    [A, B, .., C, D] -> 8 # it has certain elements at the beginning and end
    [head, .. as tail] -> 9 # destructure a list into a first element (head) and the rest (tail)
```

This can be both more concise and more efficient (at runtime) than calling [`List.get`](https://www.roc-lang.org/builtins/List#get) multiple times, since each call to `get` requires a separate conditional to handle the different `Result`s they return.

> **Note:** Each list pattern can only have one `..`, which is known as the "rest pattern" because it's where the _rest_ of the list goes.

See the [Pattern Matching example](https://www.roc-lang.org/examples/PatternMatching/README.html) which shows different ways to do pattern matching in Roc using tags, strings, and numbers.

### [The pipe operator](#the-pipe-operator) {#the-pipe-operator}

When you have nested function calls, sometimes it can be clearer to write them in a "pipelined" style using the `|>` operator. Here are three examples of writing the same expression; they all compile to exactly the same thing, but two of them use the `|>` operator to change how the calls look.

```roc
Result.withDefault (List.get ["a", "b", "c"] 1) ""
```

```roc
List.get ["a", "b", "c"] 1
|> Result.withDefault ""
```

The `|>` operator takes the value that comes before the `|>` and passes it as the first argument to whatever comes after the `|>`. So in the example above, the `|>` takes `List.get ["a", "b", "c"] 1` and passes that value as the first argument to `Result.withDefault`, making `""` the second argument to `Result.withDefault`.

We can take this a step further like so:

```roc
["a", "b", "c"]
|> List.get 1
|> Result.withDefault ""
```

This is still equivalent to the first expression. Since `|>` is known as the "pipe operator," we can read this as "start with `["a", "b", "c"]`, then pipe it to `List.get`, then pipe it to `Result.withDefault`."

One reason the `|>` operator injects the value as the first argument is to make it work better with functions where argument order matters. For example, these two uses of `List.append` are equivalent:

```roc
List.append ["a", "b", "c"] "d"
```

```roc
["a", "b", "c"]
|> List.append "d"
```

Another example is `Num.div`. All three of the following do the same thing, because `a / b` in Roc is syntax sugar for `Num.div a b`:

```roc
first / second
```

```roc
Num.div first second
```

```roc
first |> Num.div second
```

All operators in Roc are syntax sugar for normal function calls. See the [Operator Desugaring Table](https://www.roc-lang.org/tutorial#operator-desugaring-table) at the end of this tutorial for a complete list of them.

## [Types](#types) {#types}

Sometimes you may want to document the type of a definition. For example, you might write:

```roc
# Takes a first_name string and a last_name string, and returns a string
full_name = \first_name, last_name ->
    "$(first_name) $(last_name)"
```

Comments can be valuable documentation, but they can also get out of date and become misleading. If someone changes this function and forgets to update the comment, it will no longer be accurate.

### [Type Annotations](#type-annotations) {#type-annotations}

Here's another way to document this function's type, which doesn't have that problem:

```roc
full_name : Str, Str -> Str
full_name = \first_name, last_name ->
    "$(first_name) $(last_name)"
```

The `full_name :` line is a _type annotation_. It's a strictly optional piece of metadata we can add above a def to describe its type. Unlike a comment, the Roc compiler will check type annotations for accuracy. If the annotation ever doesn't fit with the implementation, we'll get a compile-time error.

The annotation `full_name : Str, Str -> Str` says "`full_name` is a function that takes two strings as arguments and returns a string."

We can give type annotations to any value, not just functions. For example:

```roc
first_name : Str
first_name = "Amy"

last_name : Str
last_name = "Lee"
```

These annotations say that both `first_name` and `last_name` have the type `Str`.

We can annotate records similarly. For example, we could move `first_name` and `last_name` into a record like so:

```roc
amy : { first_name : Str, last_name : Str }
amy = { first_name: "Amy", last_name: "Lee" }

jen : { first_name : Str, last_name : Str }
jen = { first_name: "Jen", last_name: "Majura" }
```

### [Type Aliases](#type-aliases) {#type-aliases}

When we have a recurring type annotation like this, it can be nice to give it its own name. We do this like so:

```roc
Musician : { first_name : Str, last_name : Str }

amy : Musician
amy = { first_name: "Amy", last_name: "Lee" }

simone : Musician
simone = { first_name: "Simone", last_name: "Simons" }
```

Here, `Musician` is a _type alias_. A type alias is like a def, except it gives a name to a type instead of to a value. Just like how you can read `name : Str` as "`name` has the type `Str`," you can also read `Musician : { first_name : Str, last_name : Str }` as "`Musician` has the type `{ first_name : Str, last_name : Str }`."

### [Type Parameters](#type-parameters) {#type-parameters}

Annotations for lists must specify what type the list's elements have:

```roc
names : List Str
names = ["Amy", "Simone", "Tarja"]
```

You can read `List Str` as "a list of strings." Here, `Str` is a _type parameter_ that tells us what type of `List` we're dealing with. `List` is a _parameterized type_, which means it's a type that requires a type parameter. There's no way to give something a type of `List` without a type parameter. You have to specify what type of list it is, such as `List Str` or `List Bool` or `List { first_name : Str, last_name : Str }`.

### [Wildcard Types (\*)](#wildcard-type) {#wildcard-type}

There are some functions that work on any list, regardless of its type parameter. For example, `List.isEmpty` has this type:

```roc
isEmpty : List * -> Bool
```

The `*` is a _wildcard type_; a type that's compatible with any other type. `List *` is compatible with any type of `List` like `List Str`, `List Bool`, and so on. So you can call `List.isEmpty ["I am a List Str"]` as well as `List.isEmpty [Bool.true]`, and they will both work fine.

The wildcard type also comes up with empty lists. Suppose we have one function that takes a `List Str` and another function that takes a `List Bool`. We might reasonably expect to be able to pass an empty list (that is, `[]`) to either of these functions, and we can! This is because a `[]` value has the type `List *`. It is a "list with a wildcard type parameter", or a "list whose element type could be anything."

### [Type Variables](#type-variables) {#type-variables}

`List.reverse` works similarly to `List.isEmpty`, but with an important distinction. As with `isEmpty`, we can call `List.reverse` on any list, regardless of its type parameter. However, consider these calls:

```roc
strings : List Str
strings = List.reverse ["a", "b"]

bools : List Bool
bools = List.reverse [Bool.true, Bool.false]
```

In the `strings` example, we have `List.reverse` returning a `List Str`. In the `bools` example, it's returning a `List Bool`. So what's the type of `List.reverse`?

We saw that `List.isEmpty` has the type `List * -> Bool`, so we might think the type of `List.reverse` would be `reverse : List * -> List *`. However, remember that we also saw that the type of the empty list is `List *`? `List * -> List *` is actually the type of a function that always returns empty lists! That's not what we want.

What we want is something like one of these:

```roc
reverse : List elem -> List elem
```

```roc
reverse : List value -> List value
```

```roc
reverse : List a -> List a
```

Any of these will work, because `elem`, `value`, and `a` are all _type variables_. A type variable connects two or more types in the same annotation. So you can read `List elem -> List elem` as "takes a list and returns a list that has **the same element type**." Just like `List.reverse` does!

You can choose any name you like for a type variable, but it has to be lowercase. (You may have noticed all the types we've used until now are uppercase; that is no accident! Lowercase types are always type variables, so all other named types have to be uppercase.) All three of the above type annotations are equivalent; the only difference is that we chose different names (`elem`, `value`, and `a`) for their type variables.

You can tell some interesting things about functions based on the type parameters involved. For example, any function that returns `List *` definitely always returns an empty list. You don't need to look at the rest of the type annotation, or even the function's implementation! The only way to have a function that returns `List *` is if it returns an empty list.

Similarly, the only way to have a function whose type is `a -> a` is if the function's implementation returns its argument without modifying it in any way. This is known as [the identity function](https://en.wikipedia.org/wiki/Identity_function).

### [Tag Union Types](#tag-union-types) {#tag-union-types}

We can also annotate types that include tags:

```roc
color_from_str : Str -> [Red, Green, Yellow]
color_from_str = \string ->
    when string is
        "red" -> Red
        "green" -> Green
        _ -> Yellow
```

You can read the type `[Red, Green, Yellow]` as "a tag union of the tags `Red`, `Green`, and `Yellow`."

Some tag unions have only one tag in them. For example:

```roc
red_tag : [Red]
red_tag = Red
```

### [Accumulating Tag Types](#accumulating-tag-types) {#accumulating-tag-types}

Tag union types can accumulate more tags based on how they're used. Consider this `if` expression:

```roc
\str ->
    if Str.isEmpty str then
        Ok "it was empty"
    else
        Err ["it was not empty"]
```

Here, Roc sees that the first branch has the type `[Ok Str]` and that the `else` branch has the type `[Err (List Str)]`, so it concludes that the whole `if` expression evaluates to the combination of those two tag unions: `[Ok Str, Err (List Str)]`.

This means this entire `\str -> ...` function has the type `Str -> [Ok Str, Err (List Str)]`. However, it would be most common to annotate it as `Result Str (List Str)` instead, because the `Result` type (for operations like `Result.withDefault`, which we saw earlier) is a type alias for a tag union with `Ok` and `Err` tags that each have one payload:

```roc
Result ok err : [Ok ok, Err err]
```

We just saw how tag unions get combined when different branches of a conditional return different tags. Another way tag unions can get combined is through pattern matching. For example:

```roc
when color is
    Red -> "red"
    Yellow -> "yellow"
    Green -> "green"
```

Here, Roc's compiler will infer that `color`'s type is `[Red, Yellow, Green]`, because those are the three possibilities this `when` handles.

### [Opaque Types](#opaque-types) {#opaque-types}

A type can be defined to be opaque to hide its internal structure. This is a lot more amazing than it may seem. It can make your code more modular, robust, and easier to read:

- If a type is opaque you can modify its internal structure and be certain that no dependencies need to be updated.
- You can prevent that data needs to be checked multiple times. For example, you can create an opaque `NonEmptyList` from a `List` after you've checked it. Now all functions that you pass this `NonEmptyList` to do not need to handle the empty list case.
- Having the type `Username` in a type signature gives you more context compared to `Str`. Even if the `Username` is an opaque type for `Str`.

You can create an opaque type with the `:=` operator. Let's make one called `Username`:

```roc
Username := Str

from_str : Str -> Username
from_str = \str ->
    @Username str

to_str : Username -> Str
to_str = \@Username str ->
    str
```

The `from_str` function turns a string into a `Username` by calling `@Username` on that string. The `to_str` function turns a `Username` back into a string by pattern matching `@Username str` to unwrap the string from the `Username` opaque type.

Now we can expose the `Username` opaque type so that other modules can use it in type annotations. However, other modules can't use the `@Username` syntax to wrap or unwrap `Username` values. That operation is only available in the same scope where `Username` itself was defined; trying to use it outside that scope will give an error.

Note that if we define `Username := Str` inside another module (e.g. `Main`) and also use `@Username`, this will compile, however the new `Username` type in main would not be equal to the one defined in the `Username` module. Although both opaque types have the name `Username`, they were defined in different modules and so they are type-incompatible with each other, and even attempting to use `==` to compare them would be a type mismatch.

### [Integers](#integers) {#integers}

Roc has different numeric types that each have different tradeoffs. They can all be broken down into two categories: [fractions](https://en.wikipedia.org/wiki/Fraction), and [integers](https://en.wikipedia.org/wiki/Integer). In Roc we call these `Frac` and `Int` for short.

Integer types have two important characteristics: their _size_ and their [_signedness_](https://en.wikipedia.org/wiki/Signedness). Together, these two characteristics determine the range of numbers the integer type can represent.

For example, the Roc type `U8` can represent the numbers 0 through 255, whereas the `I16` type can represent the numbers -32768 through 32767. You can actually infer these ranges from their names (`U8` and `I16`) alone!

The `U` in `U8` indicates that it's _unsigned_, meaning that it can't have a minus [sign](<https://en.wikipedia.org/wiki/Sign_(mathematics)>), and therefore can't be negative. The fact that it's unsigned tells us immediately that its lowest value is zero. The 8 in `U8` means it is 8 [bits](https://en.wikipedia.org/wiki/Bit) in size, which means it has room to represent 2⁸ (=256) different numbers. Since one of those 256 different numbers is 0, we can look at `U8` and know that it goes from `0` (since it's unsigned) to `255` (2⁸ - 1, since it's 8 bits).

If we change `U8` to `I8`, making it a _signed_ 8-bit integer, the range changes. Because it's still 8 bits, it still has room to represent 2⁸ different numbers. However, now in addition to one of those 256 numbers being zero, about half of the rest will be negative, and the others positive. So instead of ranging from, say -255 to 255 (which, counting zero, would represent 511 different numbers; too many to fit in 8 bits!) an `I8` value ranges from -128 to 127.

Notice that the negative extreme is `-128` versus `127` (not `128`) on the positive side. That's because of needing room for zero; the slot for zero is taken from the positive range because zero doesn't have a minus sign.

Following this pattern, the 16 in `I16` means that it's a signed 16-bit integer. That tells us it has room to represent 2¹⁶ (=65536) different numbers. Half of 65536 is 32768, so the lowest `I16` would be -32768, and the highest would be 32767.

Choosing a size depends on your performance needs and the range of numbers you want to represent. Consider:

- Larger integer sizes can represent a wider range of numbers. If you absolutely need to represent numbers in a certain range, make sure to pick an integer size that can hold them!
- Smaller integer sizes take up less memory. These savings rarely matters in variables and function arguments, but the sizes of integers that you use in data structures can add up. This can also affect whether those data structures fit in [cache lines](https://en.wikipedia.org/wiki/CPU_cache#Cache_performance), which can easily be a performance bottleneck.
- Certain processors work faster on some numeric sizes than others. There isn't even a general rule like "larger numeric sizes run slower" (or the reverse, for that matter) that applies to all processors. In fact, if the CPU is taking too long to run numeric calculations, you may find a performance improvement by experimenting with numeric sizes that are larger than otherwise necessary. However, in practice, doing this typically degrades overall performance, so be careful to measure properly!

Here are the different fixed-size integer types that Roc supports:

| Range                                                                                                             | Type   |
| ----------------------------------------------------------------------------------------------------------------- | ------ |
| `-128` <br> `127`                                                                                                 | `I8`   |
| `0` <br> `255`                                                                                                    | `U8`   |
| `-32_768` <br> `32_767`                                                                                           | `I16`  |
| `0` <br> `65_535`                                                                                                 | `U16`  |
| `-2_147_483_648` <br> `2_147_483_647`                                                                             | `I32`  |
| `0` <br>`4_294_967_295` (over 4 billion)                                                                          | `U32`  |
| `-9_223_372_036_854_775_808` <br> `9_223_372_036_854_775_807`                                                     | `I64`  |
| `0` <br>`18_446_744_073_709_551_615` (over 18 quintillion)                                                        | `U64`  |
| `-170_141_183_460_469_231_731_687_303_715_884_105_728` <br> `170_141_183_460_469_231_731_687_303_715_884_105_727` | `I128` |
| `0` <br>`340_282_366_920_938_463_463_374_607_431_768_211_455` (over 340 undecillion)                              | `U128` |

If any operation would result in an integer that is either too big or too small to fit in that range (e.g. calling `Int.maxI32 + 1`, which adds 1 to the highest possible 32-bit integer), then the operation will [overflow](https://en.wikipedia.org/wiki/Integer_overflow). When an overflow occurs, the program will crash.

As such, it's very important to design your integer operations not to exceed these bounds!

### [Fractions](#fractions) {#fractions}

Roc has three fractional types:

- `F32`, a 32-bit [floating-point number](https://en.wikipedia.org/wiki/IEEE_754)
- `F64`, a 64-bit [floating-point number](https://en.wikipedia.org/wiki/IEEE_754)
- `Dec`, a 128-bit decimal [fixed-point number](https://en.wikipedia.org/wiki/Fixed-point_arithmetic)

These are different from integers, they can represent numbers with fractional components, such as 1.5 and -0.123.

`Dec` is the best default choice for representing [base-10 decimal numbers](https://en.wikipedia.org/wiki/Decimal) like [currency](https://en.wikipedia.org/wiki/Currency), because it is base-10 under the hood. In contrast, `F64` and `F32` are [base-2](https://en.wikipedia.org/wiki/Binary_number) under the hood, which can lead to decimal precision loss even when doing addition and subtraction. For example, when using `F64`, running 0.1 + 0.2 returns 0.3000000000000000444089209850062616169452667236328125, whereas when using `Dec`, 0.1 + 0.2 returns 0.3.

`F32` and `F64` have direct hardware support on common processors today. There is no hardware support for fixed-point decimals, so under the hood, a `Dec` is an `I128`; operations on it perform [base-10 fixed-point arithmetic](https://en.wikipedia.org/wiki/Fixed-point_arithmetic) with 18 decimal places of precision.

This means a `Dec` can represent whole numbers up to slightly over 170 quintillion, along with 18 decimal places. (To be precise, it can store numbers between `-170_141_183_460_469_231_731.687303715884105728` and `170_141_183_460_469_231_731.687303715884105727`.) Why 18 decimal places? It's the highest number of decimal places where you can still convert any `U64` to a `Dec` without losing information.

While the fixed-point `Dec` has a fixed range, the floating-point `F32` and `F64` do not. Instead, outside of a certain range they start to lose precision instead of immediately overflowing the way integers and `Dec` do. `F64` can represent [between 15 and 17 significant digits](https://en.wikipedia.org/wiki/Double-precision_floating-point_format) before losing precision, whereas `F32` can only represent [between 6 and 9](https://en.wikipedia.org/wiki/Single-precision_floating-point_format#IEEE_754_single-precision_binary_floating-point_format:_binary32).

There are some use cases where `F64` and `F32` can be better choices than `Dec` despite their precision drawbacks. For example, in graphical applications they can be a better choice for representing coordinates because they take up less memory, various relevant calculations run faster, and decimal precision loss isn't as big a concern when dealing with screen coordinates as it is when dealing with something like currency.

### [Num, Int, and Frac](#num-int-and-frac) {#num-int-and-frac}

Some operations work on specific numeric types - such as `I64` or `Dec` - but some operations support multiple numeric types. For example, the `Num.abs` function works on any number, since you can take the [absolute value](https://en.wikipedia.org/wiki/Absolute_value) of integers and fractions alike. Its type is:

```roc
abs : Num a -> Num a
```

This type says `abs` takes a number and then returns a number of the same type. Remember that we can see the type of number is the same because the [type variable](#type-variables) `a` is used on both sides. That's because the `Num` type is compatible with both integers and fractions.

There's also an `Int` type which is only compatible with integers, and a `Frac` type which is only compatible with fractions. For example:

```roc
Num.bitwiseXor : Int a, Int a -> Int a
```

```roc
Num.cos : Frac a -> Frac a
```

When you write a number literal in Roc, it has the type `Num *`. So you could call `Num.bitwiseXor 1 1` and also `Num.cos 1` and have them all work as expected; the number literal `1` has the type `Num *`, which is compatible with the more constrained types `Int` and `Frac`. For the same reason, you can pass number literals to functions expecting even more constrained types, like `I32` or `F64`.

### [Number Literals](#number-literals) {#number-literals}

By default, a number literal with no decimal point has the type `Num *`—that is, we know it's "a number" but nothing more specific. (Number literals with decimal points have the type `Frac *` instead.)

You can give a number literal a more specific type by adding the type you want as a lowercase suffix. For example, `1u8` specifies `1` with the type `U8`, and `5dec` specifies `5` with the type `Dec`.

The full list of possible suffixes includes:

`u8`, `i8`, `u16`, `i16`, `u32`, `i32`, `u64`, `i64`, `u128`, `i128`, `f32`, `f64`, `dec`

Integer literals can be written in [hexadecimal](https://en.wikipedia.org/wiki/Hexadecimal) form by prefixing with `0x` followed by hexadecimal characters (`a` - `f` in addition to `0` - `9`). For example, writing `0xfe` is the same as writing `254`. Similarly, the prefix `0b` specifies binary integers. Writing `0b0000_1000` is the same as writing `8`.

### [Default-Value Record Fields](#default-value-record-fields) {#default-value-record-fields}

Sometimes you may want to write a function that accepts configuration options. The way to do this in Roc is to accept a record. When you do this, it can sometimes be convenient to provide default values for certain fields in that record, so the caller can omit those fields and let them be populated by the defaults you specified.

For example:

```roc
table = \{ height, width, title ? "oak", description ? "a wooden table" } ->
```

This is using _default value field destructuring_ to destructure a record while
providing default values for any fields that the caller didn't provide. Here, the `?` operator
essentially means "if the caller did not provide this field, default it to the following value."

The type of `table` uses `?` instead of `:` to indicate which fields the caller can omit:

```roc
table :
    {
        height : U64,
        width : U64,
        title ? Str,
        description ? Str,
    }
    -> Table
```

This says that `table` takes a record with two fields that are _required_—`height` and
`width`—and two fields that may be _omitted_, namely `title` and `description`. It also says that
the `height` and `width` fields have the type `U64` and the `title` and `description` fields have
the type `Str`. This means you can choose to omit the `title`, `description`, or both fields,
when calling the function…but if you provide them, they must have the type `Str`.

This is also the type that would have been inferred for `table` if it had no annotation.
Roc's compiler can tell from the destructuring syntax `title ? "oak"` that `title` is a field with a default,
and that it has the type `Str`.

Destructuring is the only way to implement a record with default value fields! For example,
if you write the expression `config.title` and `title` is a default value field, you'll get a compile error.

Note that this language feature is really designed for passing configuration records to functions.
It's not intended to be used for data modeling; if you want to represent a value that might not be available,
use a tag union with tag names that describe why the value might not be there. For example, the tag union
`[Specified Str, Unspecified]` conveys different information from `[Found Str, NotFound]` even though both
of them store either a `Str` or nothing.

## [Crashing](#crashing) {#crashing}

Ideally, Roc programs would never crash. However, there are some situations where they may. For example:

1.  When doing normal integer arithmetic (e.g. `x + y`) that [overflows](https://en.wikipedia.org/wiki/Integer_overflow).
2.  When the system runs out of memory.
3.  When a variable-length collection (like a `List` or `Str`) gets too long to be representable in the operating system's address space. (A 64-bit operating system's address space can represent several [exabytes](https://en.wikipedia.org/wiki/Byte#Multiple-byte_units) of data, so this case should not come up often.)

Crashes in Roc are not like [try/catch exceptions](https://en.wikipedia.org/wiki/Exception_handling) found in some other programming languages. There is no way to "catch" a crash. It immediately ends the program, and what happens next is defined by the [platform](https://github.com/roc-lang/roc/wiki/Roc-concepts-explained#platform). For example, a command-line interface platform might exit with a nonzero [exit code](https://en.wikipedia.org/wiki/Exit_status), whereas a web server platform might have the current request respond with a [HTTP 500 error](https://en.wikipedia.org/wiki/List_of_HTTP_status_codes#500).

### [Crashing in unreachable branches](#crashing-in-unreachable-branches) {#crashing-in-unreachable-branches}

You can intentionally crash a Roc program, for example inside a conditional branch that you believe is unreachable. Suppose you're certain that a particular `List U8` contains valid UTF-8 bytes, which means when you call `Str.fromUtf8` on it, the `Result` it returns will always be `Ok`. In that scenario, you can use the `crash` keyword to handle the `Err` case like so:

```roc
answer : Str
answer =
    when Str.fromUtf8 definitely_valid_utf8 is
        Ok str -> str
        Err _ -> crash "This should never happen!"
```

If the unthinkable happens, and somehow the program reaches this `Err` branch even though that was thought to be impossible, then it will crash - just like if the system had run out of memory. The string passed to `crash` will be provided to the platform as context; each platform may do something different with it.

> **Note:** `crash` is a language keyword and not a function; you can't assign `crash` to a variable or pass it to a function.

### [Crashing for TODOs](#crashing-for-todos) {#crashing-for-todos}

Another use for `crash` is as a TODO marker when you're in the middle of building something:

```roc
if x > y then
    transmogrify (x * 2)
else
    crash "TODO handle the x <= y case"
```

This lets you do things like write tests for the non-`crash` branch, and then come back and finish the other branch later.

### [Crashing for error handling](#crashing-for-error-handling) {#crashing-for-error-handling}

`crash` is not for error handling.

The reason Roc has a `crash` keyword is for scenarios where it's expected that no error will ever happen (like in [unreachable branches](#crashing-in-unreachable-branches)), or where graceful error handling is infeasible (like running out of memory).

Errors that are recoverable should be represented using normal Roc types (like [Result](https://www.roc-lang.org/builtins/Result)) and then handled without crashing. For example, by having the application report that something went wrong, and then continue running from there.

## [Testing](#testing) {#testing}

You can write automated tests for your Roc code like so:

```roc
pluralize = \singular, plural, count ->
    count_str = Num.toStr count

    if count == 1 then
        "$(count_str) $(singular)"
    else
        "$(count_str) $(plural)"

expect pluralize "cactus" "cacti" 1 == "1 cactus"

expect pluralize "cactus" "cacti" 2 == "2 cacti"
```

If you put this in a file named `main.roc` and run `roc test`, Roc will execute the two `expect` expressions (that is, the two `pluralize` calls) and report any that returned `Bool.false`.

If a test fails, it will not show the actual value that differs from the expected value. This [will be resolved in the future](https://github.com/roc-lang/roc/issues/4633). For now, to show the actual value you can write the expect like this:

```roc
expect
    funcOut = pluralize "cactus" "cacti" 1

    funcOut == "2 cactus"
```

### [Inline Expectations](#inline-expects) {#inline-expects}

Expects do not have to be at the top level:

```roc
pluralize = \singular, plural, count ->
    count_str = Num.toStr count

    if count == 1 then
        "$(count_str) $(singular)"
    else
        expect count > 0

        "$(count_str) $(plural)"
```

This `expect` will fail if you call `pluralize` passing a count of 0.

Note that inline `expect`s do not halt the program! They are designed to inform, not to affect control flow. Different `roc` commands will also handle `expect`s differently:

- `roc build` discards all `expect`s for optimal runtime performance.
- `roc dev` only runs inline `expect`s that are encountered during normal execution of the program.
- `roc test` runs top level `expect`s and inline `expect`s that are encountered because of the running of top level `expect`s.

Let's clear up any confusion with an example:

```roc
main =
    expect 1 == 2

    Stdout.line! "Hi there."

double = \num ->
    expect num > -1

    num * 2

expect double 0 == 0
```

- `roc build` wil run `main`, ignore `expect 1 == 2` and just print `Hi there.`.
- `roc dev` will run `main`, tell you `expect 1 == 2` failed but will still print `Hi there`.
- `roc test` will run `expect double 0 == 0` followed by `expect num > -1` and will print how many top level expects passed: `0 failed and 1 passed in 100 ms.`.

## [Modules](#modules) {#modules}

Each `.roc` file is a separate module and contains Roc code for different purposes. Here are all of the different types of modules that Roc supports;

- **Builtins** provide functions that are automatically imported into every module.
- **Applications** are combined with a platform and compiled into an executable.
- **Modules** provide functions which can be imported into other modules.
- **Packages** organise modules to share functionality across applications and platforms.
- **Platforms** provide effects such as IO to interface with the outside world.
- **Hosted** _note this module type is likely to be deprecated soon_.

### [Builtin Modules](#builtin-modules) {#builtin-modules}

There are several modules that are built into the Roc compiler, which are imported automatically into every Roc module. They are:

1.  [Str](https://www.roc-lang.org/builtins/Str)
2.  [Num](https://www.roc-lang.org/builtins/Num)
3.  [Bool](https://www.roc-lang.org/builtins/Bool)
4.  [Result](https://www.roc-lang.org/builtins/Result)
5.  [List](https://www.roc-lang.org/builtins/List)
6.  [Dict](https://www.roc-lang.org/builtins/Dict)
7.  [Set](https://www.roc-lang.org/builtins/Set)
8.  [Decode](https://www.roc-lang.org/builtins/Decode)
9.  [Encode](https://www.roc-lang.org/builtins/Encode)
10. [Hash](https://www.roc-lang.org/builtins/Hash)
11. [Box](https://www.roc-lang.org/builtins/Box)
12. [Inspect](https://www.roc-lang.org/builtins/Inspect)

You may have noticed that we already used the first five. For example, when we wrote `Str.concat` and `Num.isEven`, we were referencing functions stored in the `Str` and `Num` modules.

These modules are not ordinary `.roc` files that live on your filesystem. Rather, they are built directly into the Roc compiler. That's why they're called "builtins!"

Besides being built into the compiler, the builtin modules are different from other modules in that:

- They are always imported. You never need to add them to `imports`.
- All their types are imported unqualified automatically. So you never need to write `Num.Dec`, because it's as if the `Num` module was imported using `imports [Num.{ Dec }]` (the same is true for all the other types in the `Num` module.)

### [App Module Header](#app-module-header) {#app-module-header}

Let's take a closer look at the part of `main.roc` above the `main` def:

```roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.18.0/0APbwVN1_p1mJ96tXjaoiUCr8NBGamr8G8Ac_DrXR-o.tar.br" }

import pf.Stdout
```

This is known as a _module header_. Every `.roc` file is a _module_, and there are different types of modules. We know this particular one is an _application module_ because it begins with the `app` keyword.

The line `app [main!]` shows that this module is a Roc application and which [platform](https://github.com/roc-lang/roc/wiki/Roc-concepts-explained#platform) it is built on.

The `{ pf: platform "https://...tar.br" }` part says four things:

- We're going to be using a _package_ (a collection of modules) that can be downloaded from the URL `"https://...tar.br"`
- That package's [base64](https://en.wikipedia.org/wiki/Base64#URL_applications)\-encoded [BLAKE3](<https://en.wikipedia.org/wiki/BLAKE_(hash_function)#BLAKE3>) cryptographic hash is the long string at the end (before the `.tar.br` file extension). Once the file has been downloaded, its contents will be verified against this hash, and it will only be installed if they match. This way, you can be confident the download was neither corrupted nor changed since it was originally published.
- We're going to name that package `pf` so we can refer to it more concisely in the future.
- This package is the [platform](#platform-modules-platform-modules) we have chosen for our app.

The `import pf.Stdout` line says that we want to import the `Stdout` module from the `pf` package, and make it available in the current module.

This import has a direct interaction with our definition of `main`. Let's look at that again:

```roc
main! = \_args ->
    Stdout.line! "Hi there, from inside a Roc app. 🎉"
```

Here, `main!` is calling a function called `Stdout.line!`. More specifically, it's calling a function named `line!` which is exposed by a module named `Stdout`.

When we write `import pf.Stdout`, it specifies that the `Stdout` module comes from the package we named `pf` in the `packages { pf: ... }` section.

You can find documentation for the `Stdout.line!` function in the [Stdout](https://www.roc-lang.org/packages/basic-cli/Stdout#line) module documentation.

If we would like to include other modules in our application, say `AdditionalModule.roc` and `AnotherModule.roc`, then they can be imported directly like this:

```roc
import pf.Stdout
import AdditionalModule
import AnotherModule
```

You can also use the `as` keyword if you would like to use a different name:

```roc
import uuid.Generate as Uuid
```

...and the `exposing` keyword to bring values or functions into the current scope:

```roc
import pf.Stdout exposing [line!]

main! = \_args ->
    line! "Hi there, from inside a Roc app. 🎉"
```

### [Package Modules](#package-modules) {#package-modules}

Package modules enable Roc code to be easily re-used and shared. This is achieved by organizing code into different modules and then including these in the `package` field of the package file structure, `package [ MyModule ] {}`. The modules that are listed in the `package` field are then available for use in applications, platforms, or other packages. Internal modules that are not listed will be unavailable for use outside of the package.

See [Parser Package](https://github.com/lukewilliamboswell/roc-parser/tree/main/package) for an example.

Package documentation can be generated using the Roc cli with `roc docs /package/*.roc`.

Build a package for distribution with `roc build --bundle .tar.br /package/main.roc`. This will create a single tarball that can then be easily shared online using a URL.

You can import a package that is available either locally, or from a URL into a Roc application or platform. This is achieved by specifying the package in the `packages` section of the application or platform file structure. For example, `{ .., parser: "<package URL>" }` is an example that imports a parser module from a URL.

How does the Roc cli import and download a package from a URL?

1. First it checks to see whether the relevant folder already exists in the local filesystem and if not, creates it. If there is a package already downloaded then there is no need to download or extract anything. Packages are cached in a directory, typically `~/.cache/roc` on UNIX, and `%APPDATA%\\Roc` on Windows.
2. It then downloads the file at that URL and verifies that the hash of the file matches the hash at the end of the URL.
3. If the hash of the file matches the hash in the URL, then decompress and extract its contents into the cache folder so that it can be used.

Why is a Roc package URL so long?

Including the hash solves a number of problems:

1. The package at the URL can not suddenly change and cause different behavior.
2. Because of 1. there is no need to check the URL on every compilation to see if we have the latest version.
3. If the domain of the URL expires, a malicious actor can change the package but the hash will not match so the roc cli will reject it.

### [Regular Modules](#regular-modules) {#regular-modules}

\[This part of the tutorial has not been written yet. Coming soon!\]

### [Platform Modules](#platform-modules) {#platform-modules}

\[This part of the tutorial has not been written yet. Coming soon!\]

### [Importing Files](#importing-files) {#importing-files}

You can import files directly into your module as a `Str` or a `List U8` at compile time. This is can be useful when working with data you would like to keep in a separate file, e.g. JSON or YAML configuration.

```roc
import "some-file" as some_str : Str
import "some-file" as some_bytes : List U8
```

See the [Ingest Files Example](https://www.roc-lang.org/examples/IngestFiles/README.html) for a demonstration on using this feature.

## [Effectful functions](#efffectful-functions) {#efffectful-functions}

There are two types of functions in roc, "pure" and "effectful". Consider these two functions:

```roc
with_extension : Str -> Str
with_extension = \filename ->
    "$(filename).roc"

read_file! : Str => Str
read_file! = \path ->
    File.read_utf8! with_extension(path) |> Result.withDefault ""
```

Notice the subtle difference in these functions' types:

- Str `->` Str is the type of the pure function `with_extension`
- Str `=>` Str is the type of the effectful function `read_file!`

The arrow in the type tells you whether a function is pure or might perform effects. Pure functions use `->` and effectful functions use `=>`. Effectful functions can call either pure functions or other effectful functions, but pure functions can only call other pure functions. (The fact that pure functions can only call pure functions is part of the definition of pure functions; Roc is just reflecting that fact in the type system.)

The suffix character `!` at the end of a function name marks it as effectful. This is a naming convention, and is enforced by the compiler with a warning.

Let's look at some examples. In the `basic-cli` platform, here are four operations we can do:

- Write a string to the terminal
- Read a string from user input
- Write a string to a file
- Read a string from a file

We'll use these four operations to learn about effects.

Let's revisit that first application.

```roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.18.0/0APbwVN1_p1mJ96tXjaoiUCr8NBGamr8G8Ac_DrXR-o.tar.br" }

import pf.Stdout

main! = \_args ->
    Stdout.line! "Hi there, from inside a Roc app. 🎉"
```

This code prints "Hi there, from inside a Roc app. 🎉" to the [standard output](<https://en.wikipedia.org/wiki/Standard_streams#Standard_output_(stdout)>). `Stdout.line` has this type:

```roc
Stdout.line! : Str => Result {} [StdoutErr IOErr]
```

An effectulful function is capable of interacting with state outside your Roc program, such as the terminal's standard output, or a file.

When we call `main!`, the host will provide the arguments passed from the cli `_args` (here we're just ignoring these), and then run. Here, we've set `main!` to be an effectful function that writes `"Hi there, from inside a Roc app. 🎉"` to `stdout` when it gets run, so that's what our program does!

The `Stdout.line!` function here returns a `Result` when called. If it succeeds it returns the unit value `{}`, or if it fails it returns the tag `StdoutErr` with an `IOErr` payload.

In contrast, when `Stdin.line! : {} => Result Str [EndOfFile, StdinErr IOErr]` finishes reading a line from [standard input](<https://en.wikipedia.org/wiki/Standard_streams#Standard_input_(stdin)>), it produces either a `Str` or else `EndOfFile` or a `StdinErr` error tag if standard input reached its end (which can happen if the user types Ctrl+D on UNIX systems or Ctrl+Z on Windows). These possibilities can all be seen just in the type definition of the function.

Note that to call this `Stdin.line!`, you need to provide the unit value `{}` as an argument.

### [Reading values](#reading-values) {#reading-values}

Let's change `main!` to read a line from `stdin`, and then print what we got:

```roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.18.0/0APbwVN1_p1mJ96tXjaoiUCr8NBGamr8G8Ac_DrXR-o.tar.br" }

import pf.Stdout
import pf.Stdin

main! = \_args ->
    try Stdout.line! "Type in something and press Enter:"
    input = try Stdin.line! {}
    try Stdout.line! "Your input was: $(input)"

    Ok {}
```

If you run this program, it will print "Type in something and press Enter:" and then pause.
That's because it's waiting for you to type something in and press Enter! Once you do,
it should print back out what you entered.

### [Effectul failure](#failure) {#failure}

Sometimes, effects can fail. For example, reading from a file might fail if the file is not found.
Even reading from stdin and writing to stdout can fail!

For example, the `Stdin.line!` function can fail if stdin is closed before it receives a line. You can try this out
by running the program and pressing Ctrl+Z on Windows, or Ctrl+D on macOS or Linux. (Press that key rather than
typing in text and pressing Enter.) You'll see a default error message, which the `basic-cli` platform provides
in case an error occurs that we didn't handle.

Although this default error handling behavior might be exactly what we want if we're writing a quick script,
high-quality programs handle errors gracefully. Fortunately, we can do this nicely in Roc!

If we wanted to add the type annotation to `main!` that Roc is inferring for it, we would add this annotation:

```roc
import pf.Stdout
import pf.Stdin
import pf.Arg exposing [Arg]

main! : List Arg => Result {} [EndOfFile, StdinErr Stdin.IOErr, StdoutErr Stdout.IOErr]
main = #...
```

Let's break down what this type is saying:

- Both the `!` suffix in the name, and the `=>` in the type, tell us this is an effectful function. Its two type parameters are just like the ones we saw in `Result` earlier: the first type tells us what this function will produce if it succeeds, and the other one tells us what it will produce if it fails.
- `{}` tells us that this function always produces an empty record when it succeeds. (That is, it doesn't produce anything useful. Empty records don't have any information in them!) This is because the last expression in `main!` comes from `Stdout.line!`, which doesn't produce anything. (In contrast, the `Stdin` function's first type parameter is a `Str`, because it produces a `Str` if it succeeds.)
- `[EndOfFile, StdinErr Stdin.IOErr, StdoutErr Stdout.IOErr]` tells us the different ways this function can fail. The `StdoutErr` and `StdinErr` tags are there because we used `Stdout.line!` and `Stdin.line!`.

To understand the error a little more, let's try temporarily commenting out our current `main!` and replacing
it with this one:

```roc
main! : _ => Result {} [Exit I32 Str]
main! = \_args -> Err (Exit 42 "An error happened!")
```

Now if we run the application, it will print the line "An error happened!" to stderr and exit with a status code of 42. (You can check the status code of the most recent terminal command that finished in Windows by running `echo %ERRORLEVEL%` (or `$LASTEXITCODE` in PowerShell), or by running `echo $?` in macOS or Linux.)

Now let's try running it with this version of `main`:

```roc
main! : _ => Result {} [Exit I32 Str]
main! = \_args -> Ok {}
```

This program won't print anything at all, but it will exit with a status code of `0`, indicating success.

In summary:

- If the `main!` function ends in a `Ok {}`, then it means the final expression succeeded and the program will exit with status code 0.
- If the `main!` function ends in a `Err (Exit 42 "…")`, then it means it failed, and the only information we got about the failure was that the program should exit with code 42 instead of 0, and that it should print a particular string to stderr to inform the user about what happened.

### [Handling failure](#handling-failures) {#handling-failures}

If `main!` ends up failing with any other errors besides `Exit` (such as `StdoutErr` or `StdinErr`), then the `basic-cli` platform's automatic error handling will handle them by printing out words taken from the source code (such as "StdoutErr" and "StdinErr"), which could lead to a bad experience for people using this program!

We can prevent that by gracefully handling the other error types, and then translating them into `Exit` errors so that they affect the program's exit code and don't result in the platform printing anything.

A convenient way to make sure we've handled all the other errors is to keep our current type annotation for `main!` but restore our old implementation:

```roc
main! : List Arg => Result {} [Exit I32 Str]
main! = \_args ->
    try Stdout.line! "Type in something and press Enter:"
    input = try Stdin.line! {}
    try Stdout.line! "Your input was: $(input)"

    Ok {}
```

Adding this type annotation will give us a type mismatch - which is exactly what we want in this case!

The type mismatch is telling us that we're claiming the `main!` function will only ever fail with an `Exit` tag, but this implementation can _also_ fail with `EndOfFile`, `StdoutErr` and `StdinErr` tags we saw earlier.

In other words, adding this annotation effectively opted us out of `basic-cli`'s default error handling. Now any potential failures (now and in the future) will have to be handled somehow; if we forget to handle any, we'll get a type mismatch like this! For that reason, `basic-cli` applications that are intended to be high-quality (so, not things like quick scripts) will generally benefit from applying this type annotation to `main!`.

Here's one way we can handle those errors:

```roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.18.0/0APbwVN1_p1mJ96tXjaoiUCr8NBGamr8G8Ac_DrXR-o.tar.br" }

import pf.Stdout
import pf.Stdin
import pf.Arg exposing [Arg]

main! : List Arg => Result {} [Exit I32 Str]
main! = \_args ->
    Result.mapErr (my_function! {}) \err ->
        when err is
            StdoutErr _ -> Exit 1i32 "Error writing to stdout."
            StdinErr _ -> Exit 2i32 "Error writing to stdin."
            EndOfFile -> Exit 3i32 "End of file reached."

my_function! : {} => Result {} [EndOfFile, StdinErr _, StdoutErr _]
my_function! = \{} ->
    try Stdout.line! "Type in something and press Enter:"
    input = try Stdin.line! {}
    try Stdout.line! "Your input was: $(input)"

    Ok {}
```

The `Result.mapErr` function translates one error into another. Here, we're translating the `EndOfFile`, `StdoutErr` and `StdinErr` errors into `Exit` errors which include a different exit code plus a message that will print to stderr to explain what happened.

### [The \_ type](#underscore) {#underscore}

Note from our last example how we used `_` to create _partial_ type annotations. For example

```roc
when err is
    StdoutErr _ -> Exit 1i32 "Error writing to stdout."
    StdinErr _ -> Exit 2i32 "Error writing to stdin."
    EndOfFile -> Exit 3i32 "End of file reached."
```

Wherever a `_` appears in a type annotation, it essentially means "I'm choosing not to annotate this part right here" and it lets Roc use type inference to fill in the blank behind the scenes. Roc will still compile them and check their types as normal (just like it did before we had any annotations at all); the `_` is about which parts of the type we're choosing to annotate and which parts we're leaving to inference.

This is a useful technique to use when we don't want to write out a bunch of error types that we're going to handle anyway, and would otherwise have to keep updating every time a new error appeared. If we want to know the full list of errors, we can see it in a number of ways:

- Look at the branches of our `when err is`, since they cover all the errors (and we'd get a compile error if we left any out)
- If we're using an editor that supports it, hovering over the `_` might display the inferred type that goes there.
- We can put an obviously wrong type in there (e.g. replace the `{}` with `Str`, which is totally wrong) and look at the compiler error to see what it inferred as the correct type.

### [Ignoring informationless return values](#ignoring-informationless-return-values) {#ignoring-informationless-return-values}

The compiler warns us about any unused variable, but as we saw before, if we don't intend to use a variable then we can name it `_` to clarify intent and silence the warning.

Furthermore, if a variable can't possibly contain any useful information, then we can ignore it entirely.

Let's revisit an earlier example:

```roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.18.0/0APbwVN1_p1mJ96tXjaoiUCr8NBGamr8G8Ac_DrXR-o.tar.br" }

import pf.Stdout
import pf.Stdin

main! = \_args ->
    try Stdout.line! "Type in something and press Enter:"
    input = try Stdin.line! {}
    try Stdout.line! "Your input was: $(input)"
    Ok {}
```

It looks like this block goes "expression, assignment, expression", but expressions are only allowed on the last line of a block. What's happening here?

Since `Stdout.line! : Str => Result {} [StdoutErr IOErr]`, in the above we are unwrapping the `Result` returned by calling these effects.

An alternative option, is to just ignore the return value entirely:

```roc
main! = \_args ->
    _ = Stdout.line! "Type in something and press Enter:"
    when Stdin.line! {} is
        Ok input ->
            _ = Stdout.line! "Your input was: $(input)"
            Ok {}
        Err _ ->
            Ok {}
```

### [Tagging errors](#tagging-errors) {#tagging-errors}

Although it's rare, it is possible that either of the `Stdout.line!` operations in our example could fail:

```roc
main! = \_args ->
    try Stdout.line! "Type something and press Enter."
    input = try Stdin.line! {}
    try Stdout.line! "You entered: $(input)"
    Ok {}
```

(In this particular example, it's very unlikely that this would come up at all, and even if it did, we might not care which one caused the problem. But you can imagine having multiple HTTP requests, or file writes, and wanting to know which of them was the one that failed.)

If an error happended here, we wouldn't know which effectul function was the cause of the failure.

One option is to "tag the error" using `Result.mapErr` to wrap the error in a [tag](#tags) like so:

```roc
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.18.0/0APbwVN1_p1mJ96tXjaoiUCr8NBGamr8G8Ac_DrXR-o.tar.br" }

import pf.Stdout
import pf.Stdin

main! = \_args ->

    Stdout.line! "Type something and press Enter."
    |> Result.mapErr UnableToPrintPrompt
    |> try

    input =
        Stdin.line! {}
        |> Result.mapErr UnableToReadInput
        |> try

    Stdout.line! "You entered: $(input)"
    |> Result.mapErr UnableToPrintInput
    |> try

    Ok {}
```

The `mapErr` function has this type:

```
Result.mapErr : Result ok a, (a -> b) -> Result ok b
```

Here we're passing in "tagging functions"—namely, `UnableToPrintPrompt` and `UnableToReadInput`. (See [Using tags as functions](#using-tags-as-functions) for how this works.)

This code is doing three things:

1. Call `Stdout.line! "..."`, which returns a `Result` value
2. Transform that `Result` value into another `Result` value using `|> Result.mapErr`
3. Unwrap that final `Result` value (returned by `mapErr`) using `try` and return early if it's an error

See the [Error Handling example](https://www.roc-lang.org/examples/ErrorHandling/README.html) for a more detailed explanation of error handling in a larger program.

### [Displaying Roc values with `Inspect.toStr`](#inspect) {#inspect}

The [`Inspect.toStr`](https://www.roc-lang.org/builtins/Inspect#toStr) function returns a `Str` representation of any Roc value using its [`Inspect` ability](/abilities#inspect-ability). It's useful for things like debugging and logging (although [`dbg`](https://www.roc-lang.org/tutorial#debugging) is often nicer for debugging in particular), but its output is almost never something that should be shown to end users! In this case we're just using it for our own learning, but it would be better to run a `when` on `e` and display a more helpful message.

```roc
when err is
    StdoutErr e -> Exit 1 "Error writing to stdout: $(Inspect.toStr e)"
    StdinErr e -> Exit 2 "Error writing to stdin: $(Inspect.toStr e)"
```

### [The early `return` keyword](#the-early-return-keyword) {#the-early-return-keyword}

The `return` keyword can interrupt a function and return any value we want.

This is rarely necessary, due to Roc's powerful syntax for expressions and pattern matching, but it's included as a comfort to developers familiar with its frequent use in primarily-imperative programing languages.

For example:

```roc
stoplight_str : Str
stoplight_str =
    stoplight_color =
        if this_ss_a_bad_time then
            return "Hey, listen, I just don't want to do this."
        else
            previous_stop_light_color
    when stoplight_color is
        Red -> "red"
        Green | Yellow if contrast > 75 -> "not red, but very high contrast"
        Green | Yellow if contrast > 50 -> "not red, but high contrast"
        Green | Yellow -> "not red"
```

## [Examples](#examples) {#examples}

Well done on making it this far!

We've covered all of the basic syntax and features of Roc in this Tutorial. You should now have a good foundation and be ready to start writing your own applications.

You can continue reading through more-advanced topics below, or perhaps checkout some of the [Examples](/examples) for more a detailed exploration of ways to do various things.

## [Under Construction](#under-construction) {#under-construction}

Here are some features that are either coming soon or leaving soon. You may see these used in others' code, but for now you can choose to ignore them all!

### [The `?` postfix "try" operator](#the-postfix-try-operator) {#the-postfix-try-operator}

STATUS: Work in progress - being modified to support parens and commas calling convention. At this time it may lead to confusing and unhelpful error messages.

Roc also has a `?` postfix operator, which behaves similar to the `try` keyword. An example:

```roc
get_letter : Str -> Result Str [OutOfBounds, InvalidNumStr]
get_letter = \index_str ->
    index = Str.toU64? index_str
    List.get ["a", "b", "c", "d"] index
```

## [Advanced Concepts](#advanced-concepts) {#advanced-concepts}

Here are some concepts you likely won't need as a beginner, but may want to know about eventually. This is listed as an appendix rather than the main tutorial, to emphasize that it's totally fine to stop reading here and go build things!

### [Open Records and Closed Records](#open-records-and-closed-records) {#open-records-and-closed-records}

Let's say I write a function which takes a record with a `first_name` and `last_name` field, and puts them together with a space in between:

```roc
full_name = \user ->
    "$(user.first_name) $(user.last_name)"
```

I can pass this function a record that has more fields than just `first_name` and `last_name`, as long as it has _at least_ both of those fields (and both of them are strings). So any of these calls would work:

- `full_name { first_name: "Sam", last_name: "Sample" }`
- `full_name { first_name: "Sam", last_name: "Sample", email: "blah@example.com" }`
- `full_name { age: 5, first_name: "Sam", things: 3, last_name: "Sample", role: Admin }`

This `user` argument is an _open record_ - that is, a description of a minimum set of fields on a record, and their types. When a function takes an open record as an argument, it's okay if you pass it a record with more fields than just the ones specified.

In contrast, a _closed record_ is one that requires an exact set of fields (and their types), with no additional fields accepted.

If we add a type annotation to this `full_name` function, we can choose to have it accept either an open record or a closed record:

```roc
# Closed record
full_name : { first_name : Str, last_name : Str } -> Str
full_name = \user ->
    "$(user.first_name) $(user.last_name)"
```

```roc
# Open record (because of the `*`)
full_name : { first_name : Str, last_name : Str }* -> Str
full_name = \user ->
    "$(user.first_name) $(user.last_name)"
```

The `*` in the type `{ first_name : Str, last_name : Str }*` is what makes it an open record type. This `*` is the _wildcard type_ we saw earlier with empty lists. (An empty list has the type `List *`, in contrast to something like `List Str` which is a list of strings.)

This is because record types can optionally end in a type variable. Just like how we can have `List *` or `List a -> List a`, we can also have `{ first : Str, last : Str }*` or `{ first : Str, last : Str }a -> { first : Str, last : Str }a`. The differences are that in `List a`, the type variable is required and appears with a space after `List`; in a record, the type variable is optional, and appears (with no space) immediately after `}`.

If the type variable in a record type is a `*` (such as in `{ first : Str, last : Str }*`), then it's an open record. If the type variable is missing, then it's a closed record. You can also specify a closed record by putting a `{}` as the type variable (so for example, `{ email : Str }{}` is another way to write `{ email : Str }`). In practice, closed records are basically always written without the `{}` on the end, but later on we'll see a situation where putting types other than `*` in that spot can be useful.

### [Constrained Records](#constrained-records) {#constrained-records}

The type variable can also be a named type variable, like so:

```roc
add_https : { url : Str }a -> { url : Str }a
add_https = \record ->
    { record & url: "https://$(record.url)" }
```

This function uses _constrained records_ in its type. The annotation is saying:

- This function takes a record which has at least a `url` field, and possibly others
- That `url` field has the type `Str`
- It returns a record of exactly the same type as the one it was given

So if we give this function a record with five fields, it will return a record with those same five fields. The only requirement is that one of those fields must be `url: Str`.

In practice, constrained records appear in type annotations much less often than open or closed records do.

Here's when you can typically expect to encounter these three flavors of type variables in records:

- _Open records_ are what the compiler infers when you use a record as an argument, or when destructuring it (for example, `{ x, y } =`).
- _Closed records_ are what the compiler infers when you create a new record (for example, `{ x: 5, y: 6 }`)
- _Constrained records_ are what the compiler infers when you do a record update (for example, `{ user & email: newEmail }`)

Of note, you can pass a closed record to a function that accepts a smaller open record, but not the reverse. So a function `{ a : Str, b : Bool }* -> Str` can accept an `{ a : Str, b : Bool, c : Bool }` record, but a function `{ a : Str, b : Bool, c : Bool } -> Str` would not accept an `{ a : Str, b : Bool }*` record.

This is because if a function accepts `{ a : Str, b : Bool, c : Bool }`, that means it might access the `c` field of that record. So if you passed it a record that was not guaranteed to have all three of those fields present (such as an `{ a : Str, b : Bool }*` record, which only guarantees that the fields `a` and `b` are present), the function might try to access a `c` field at runtime that did not exist!

### [Type Variables in Record Annotations](#type-variables-in-record-annotations) {#type-variables-in-record-annotations}

You can add type annotations to make record types less flexible than what the compiler infers, but not more flexible. For example, you can use an annotation to tell the compiler to treat a record as closed when it would be inferred as open (or constrained), but you can't use an annotation to make a record open when it would be inferred as closed.

If you like, you can always annotate your functions as accepting open records. However, in practice this may not always be the nicest choice. For example, let's say you have a `User` type alias, like so:

```roc
User : {
    email : Str,
    first_name : Str,
    last_name : Str,
}
```

This defines `User` to be a closed record, which in practice is the most common way records named `User` tend to be defined.

If you want to have a function take a `User`, you might write its type like so:

```roc
is_valid : User -> Bool
```

If you want to have a function return a `User`, you might write its type like so:

```roc
user_from_email : Str -> User
```

A function which takes a user and returns a user might look like this:

```roc
capitalize_names : User -> User
```

This is a perfectly reasonable way to write all of these functions. However, I might decide that I really want the `is_valid` function to take an open record; a record with _at least_ the fields of this `User` record, but possibly others as well.

Since open records have a type variable (like `*` in `{ email : Str }*` or `a` in `{ email : Str }a -> { email : Str }a`), in order to do this I'd need to add a type variable to the `User` type alias:

```roc
User a : {
    email : Str
    first_name : Str
    last_name : Str
}a
```

Notice that the `a` type variable appears not only in `User a` but also in `}a` at the end of the record type!

Using `User a` type alias, I can still write the same three functions, but now their types need to look different. This is what the first one would look like:

```roc
is_valid : User * -> Bool
```

Here, the `User *` type alias substitutes `*` for the type variable `a` in the type alias, which takes it from `{ email : Str, ... }a` to `{ email : Str, ... }*`. Now I can pass it any record that has at least the fields in `User`, and possibly others as well, which was my goal.

```roc
user_from_email : Str -> User {}
```

Here, the `User {}` type alias substitutes `{}` for the type variable `a` in the type alias, which takes it from `{ email : Str, ... }a` to `{ email : Str, ... }{}`. As noted earlier, this is another way to specify a closed record: putting a `{}` after it, in the same place that you'd find a `*` in an open record.

> **Aside:** This works because you can form new record types by replacing the type variable with other record types. For example, `{ a : Str, b : Str }` can also be written `{ a : Str }{ b : Str }`. You can chain these more than once, e.g. `{ a : Str }{ b : Str }{ c : Str, d : Str }`. This is more useful when used with type annotations; for example, `{ a : Str, b : Str }User` describes a closed record consisting of all the fields in the closed record `User`, plus `a : Str` and `b : Str`.

This function still returns the same record as it always did, it just needs to be annotated as `User {}` now instead of just `User`, because the `User` type alias has a variable in it that must be specified.

The third function might need to use a named type variable:

```roc
capitalize_names : User a -> User a
```

If this function does a record update on the given user, and returns that - for example, if its definition were `capitalize_names = \user -> { user & email: "blah" }` - then it needs to use the same named type variable for both the argument and return value.

However, if returns a new `User` that it created from scratch, then its type could instead be:

```roc
capitalize_names : User * -> User {}
```

This says that it takes a record with at least the fields specified in the `User` type alias, and possibly others...and then returns a record with exactly the fields specified in the `User` type alias, and no others.

These three examples illustrate why it's relatively uncommon to use open records for type aliases: it makes a lot of types need to incorporate a type variable that otherwise they could omit, all so that `is_valid` can be given something that has not only the fields `User` has, but some others as well. (In the case of a `User` record in particular, it may be that the extra fields were included due to a mistake rather than on purpose, and accepting an open record could prevent the compiler from raising an error that would have revealed the mistake.)

That said, this is a useful technique to know about if you want to (for example) make a record type that accumulates more and more fields as it progresses through a series of operations.

### [Open and Closed Tag Unions](#open-and-closed-tag-unions) {#open-and-closed-tag-unions}

Just like how Roc has open records and closed records, it also has open and closed tag unions.

The _open tag union_ (or _open union_ for short) `[Foo Str, Bar Bool]*` represents a tag that might be `Foo Str` and might be `Bar Bool`, but might also be some other tag whose type isn't known at compile time.

Because an open union represents possibilities that are impossible to know ahead of time, any `when` I use on a `[Foo Str, Bar Bool]*` value must include a catch-all `_ ->` branch. Otherwise, if one of those unknown tags were to come up, the `when` would not know what to do with it! For example:

```roc
example : [Foo Str, Bar Bool]* -> Bool
example = \tag ->
    when tag is
        Foo str -> Str.isEmpty str
        Bar bool -> bool
        _ -> Bool.false
```

In contrast, a _closed tag union_ (or _closed union_) like `[Foo Str, Bar Bool]` (without the `*`) represents the set of all possible tags. If I use a `when` on one of these, I can match on `Foo` only and then on `Bar` only, with no need for a catch-all branch. For example:

```roc
example : [Foo Str, Bar Bool] -> Bool
example = \tag ->
    when tag is
        Foo str -> Str.isEmpty str
        Bar bool -> bool
```

If we were to remove the type annotations from the previous two code examples, Roc would infer the same types for them anyway.

It would infer `tag : [Foo Str, Bar Bool]` for the latter example because the `when tag is` expression only includes a `Foo Str` branch and a `Bar Bool` branch, and nothing else. Since the `when` doesn't handle any other possibilities, these two tags must be the only possible ones.

It would infer `tag : [Foo Str, Bar Bool]*` for the former example because the `when tag is` expression includes a `Foo Str` branch and a `Bar Bool` branch but also a `_ ->` branch, indicating that there may be other tags we don't know about. Since the `when` is flexible enough to handle all possible tags, `tag` gets inferred as an open union.

Putting these together, whether a tag union is inferred to be open or closed depends on which possibilities the implementation actually handles.

> **Aside:** As with open and closed records, we can use type annotations to make tag union types less flexible than what would be inferred. If we added a `_ ->` branch to the second example above, the compiler would still accept `example : [Foo Str, Bar Bool] -> Bool` as the type annotation, even though the catch-all branch would permit the more flexible `example : [Foo Str, Bar Bool]* -> Bool` annotation instead.

### [Combining Open Unions](#combining-open-unions) {#combining-open-unions}

When we make a new record, it's inferred to be a closed record. For example, in `foo { a: "hi" }`, the type of `{ a: "hi" }` is inferred to be `{ a : Str }`. In contrast, when we make a new tag, it's inferred to be an open union. So in `foo (Bar "hi")`, the type of `Bar "hi"` is inferred to be `[Bar Str]*`.

This is because open unions can accumulate additional tags based on how they're used in the program, whereas closed unions cannot. For example, let's look at this conditional:

```roc
if x > 5 then
    "foo"
else
    7
```

This will be a type mismatch because the two branches have incompatible types. Strings and numbers are not type-compatible! Now let's look at another example:

```roc
if x > 5 then
    Ok "foo"
else
    Err "bar"
```

This shouldn't be a type mismatch, because we can see that the two branches are compatible; they are both tags that could easily coexist in the same tag union. But if the compiler inferred the type of `Ok "foo"` to be the closed union `[Ok Str]`, and likewise for `Err "bar"` and `[Err Str]`, then this would have to be a type mismatch - because those two closed unions are incompatible.

Instead, the compiler infers `Ok "foo"` to be the open union `[Ok Str]*`, and `Err "bar"` to be the open union `[Err Str]*`. Then, when using them together in this conditional, the inferred type of the conditional becomes `[Ok Str, Err Str]*` - that is, the combination of the unions in each of its branches. (Branches in a `when` work the same way with open unions.)

Earlier we saw how a function which accepts an open union must account for more possibilities, by including catch-all `_ ->` patterns in its `when` expressions. So _accepting_ an open union means you have more requirements. In contrast, when you already _have_ a value which is an open union, you have fewer requirements. A value which is an open union (like `Ok "foo"`, which has the type `[Ok Str]*`) can be provided to anything that's expecting a tag union (no matter whether it's open or closed), as long as the expected tag union includes at least the tags in the open union you're providing.

So if I have an `[Ok Str]*` value, I can pass it to functions with any of these types (among others):

| Function Type                           | Can it receive `[Ok Str]*`? |
| --------------------------------------- | --------------------------- |
| `[Ok Str]* -> Bool`                     | Yes                         |
| `[Ok Str] -> Bool`                      | Yes                         |
| `[Ok Str, Err Bool]* -> Bool`           | Yes                         |
| `[Ok Str, Err Bool] -> Bool`            | Yes                         |
| `[Ok Str, Err Bool, Whatever]* -> Bool` | Yes                         |
| `[Ok Str, Err Bool, Whatever] -> Bool`  | Yes                         |
| `Result Str Bool -> Bool`               | Yes                         |
| `[Err Bool, Whatever]* -> Bool`         | Yes                         |

That last one works because a function accepting an open union can accept any unrecognized tag (including `Ok Str`) even though it is not mentioned as one of the tags in `[Err Bool, Whatever]*`! Remember, when a function accepts an open tag union, any `when` branches on that union must include a catch-all `_ ->` branch, which is the branch that will end up handling the `Ok Str` value we pass in.

However, I could not pass an `[Ok Str]*` to a function with a _closed_ tag union argument that did not mention `Ok Str` as one of its tags. So if I tried to pass `[Ok Str]*` to a function with the type `[Err Bool, Whatever] -> Str`, I would get a type mismatch - because a `when` in that function could be handling the `Err Bool` possibility and the `Whatever` possibility, and since it would not necessarily have a catch-all `_ ->` branch, it might not know what to do with an `Ok Str` if it received one.

> **Note:** It wouldn't be accurate to say that a function which accepts an open union handles "all possible tags." For example, if I have a function `[Ok Str]* -> Bool` and I pass it `Ok 5`, that will still be a type mismatch. If you think about it, a `when` in that function might have the branch `Ok str ->` which assumes there's a string inside that `Ok`, and if `Ok 5` type-checked, then that assumption would be false and things would break!
>
> So `[Ok Str]*` is more restrictive than `[]*`. It's basically saying "this may or may not be an `Ok` tag, but if it is an `Ok` tag, then it's guaranteed to have a payload of exactly `Str`."

In summary, here's a way to think about the difference between open unions in a value you have, compared to a value you're accepting:

- If you _have_ a closed union, that means it has all the tags it ever will, and can't accumulate more.
- If you _have_ an open union, that means it can accumulate more tags through conditional branches.
- If you _accept_ a closed union, that means you only have to handle the possibilities listed in the union.
- If you _accept_ an open union, that means you have to handle the possibility that it has a tag you can't know about.

### [Type Variables in Tag Unions](#type-variables-in-tag-unions) {#type-variables-in-tag-unions}

Earlier we saw these two examples, one with an open tag union and the other with a closed one:

```roc
example : [Foo Str, Bar Bool]* -> Bool
example = \tag ->
    when tag is
        Foo str -> Str.isEmpty str
        Bar bool -> bool
        _ -> Bool.false
```

```roc
example : [Foo Str, Bar Bool] -> Bool
example = \tag ->
    when tag is
        Foo str -> Str.isEmpty str
        Bar bool -> bool
```

Similarly to how there are open records with a `*`, closed records with nothing, and constrained records with a named type variable, we can also have _constrained tag unions_ with a named type variable. Here's an example:

```roc
example : [Foo Str, Bar Bool]a -> [Foo Str, Bar Bool]a
example = \tag ->
    when tag is
        Foo str -> Bar (Str.isEmpty str)
        Bar bool -> Bar Bool.false
        other -> other
```

This type says that the `example` function will take either a `Foo Str` tag, or a `Bar Bool` tag, or possibly another tag we don't know about at compile time and it also says that the function's return type is the same as the type of its argument.

So if we give this function a `[Foo Str, Bar Bool, Baz (List Str)]` argument, then it will be guaranteed to return a `[Foo Str, Bar Bool, Baz (List Str)]` value. This is more constrained than a function that returned `[Foo Str, Bar Bool]*` because that would say it could return _any_ other tag (in addition to the `Foo Str` and `Bar Bool` we already know about).

If we removed the type annotation from `example` above, Roc's compiler would infer the same type anyway. This may be surprising if you look closely at the body of the function, because:

- The return type includes `Foo Str`, but no branch explicitly returns `Foo`. Couldn't the return type be `[Bar Bool]a` instead?
- The argument type includes `Bar Bool` even though we never look at `Bar`'s payload. Couldn't the argument type be inferred to be `Bar *` instead of `Bar Bool`, since we never look at it?

The reason it has this type is the `other -> other` branch. Take a look at that branch, and ask this question: "What is the type of `other`?" There has to be exactly one answer! It can't be the case that `other` has one type before the `->` and another type after it; whenever you see a named value in Roc, it is guaranteed to have the same type everywhere it appears in that scope.

For this reason, any time you see a function that only runs a `when` on its only argument, and that `when` includes a branch like `x -> x` or `other -> other`, the function's argument type and return type must necessarily be equivalent.

> **Note:** Just like with records, you can also replace the type variable in tag union types with a concrete type. For example, `[Foo Str][Bar Bool][Baz (List Str)]` is equivalent to `[Foo Str, Bar Bool, Baz (List Str)]`.
>
> Also just like with records, you can use this to compose tag union type aliases. For example, you can write `NetworkError : [Timeout, Disconnected]` and then `Problem : [InvalidInput, UnknownFormat]NetworkError`

### [Record Builder](#record-builder) {#record-builder}

Record builders are a syntax sugar for sequencing actions and collecting the intermediate results as fields in a record. All you need to build a record is a `map2`-style function that takes two values of the same type and combines them using a provided combiner function. There are many convenient APIs we can build with this simple syntax.

For example, let's say we want a record builder to match URLs as follows:

```roc
combine_matchers : UrlMatcher a, UrlMatcher b, (a, b -> c) -> UrlMatcher c
combine_matchers = \matcherA, matcherB, combiner -> ...

user_tab_matcher : UrlMatcher { users: {}, userId: U64, tab: Str }
user_tab_matcher =
    { combine_matchers <-
        users: exact_segment "users",
        userId: u64_segment,
        tab: any_segment,
    }

expect
    user_tab_matcher
    |> matchOnUrl "/users/123/account"
    == Ok { users: {}, userId: 123, tab: "account" }
```

The `user_tab_matcher` record builder desugars to the following:

```roc
user_tab_matcher =
    combine_matchers
        (exact_segment "users")
        (
            combine_matchers
                u64_segment
                any_segment
                \userId, tab -> (userId, tab)
        )
        \users, (userId, tab) -> { users, userId, tab }
```

You can see that the `combine_matchers` builder function is simply applied in sequence, pairing up all fields until a record is created.

You'll notice that the `users` field above holds an empty record, and isn't a useful part of the result. If you want to ignore such a field in the record builder, prefix its name with an underscore as you would do to ignore a variable:

```roc
user_tab_matcher : UrlMatcher { userId: U64 }
user_tab_matcher =
    { combine_matchers <-
        _: exact_segment "users",
        userId: u64_segment,
        _tab: any_segment,
    }

expect
    user_tab_matcher
    |> matchOnUrl "/users/123/account"
    == Ok { userId: 123 }
```

If you want to see other examples of using record builders, look at the [Record Builder Example](https://www.roc-lang.org/examples/RecordBuilder/README.html).

### [Reserved Keywords](#reserved-keywords) {#reserved-keywords}

These are reserved keywords in Roc. You can't choose any of them as names, except as record field names.

`as`, `crash`, `dbg`, `else`, `expect`, `expect-fx`, `if`, `import`, `is`, `return`, `then`, `try`, `when`

Other keywords are used only in specific places, so they are not reserved. This includes:

`app`, `exposes`, `exposing`, `generates`, `implements`, `module`, `package`, `packages`, `platform`, `requires`, `where`, `with`

## [Operator Desugaring Table](#operator-desugaring-table) {#operator-desugaring-table}

Here are various Roc expressions involving operators, and what they desugar to.

| Expression                   | Desugars To                                                                  |
| ---------------------------- | ---------------------------------------------------------------------------- |
| `a + b`                      | `Num.add a b`                                                                |
| `a - b`                      | `Num.sub a b`                                                                |
| `a * b`                      | `Num.mul a b`                                                                |
| `a / b`                      | `Num.div a b`                                                                |
| `a // b`                     | `Num.divTrunc a b`                                                           |
| `a ^ b`                      | `Num.pow a b`                                                                |
| `a % b`                      | `Num.rem a b`                                                                |
| `-a`                         | `Num.neg a`                                                                  |
| `a == b`                     | `Bool.isEq a b`                                                              |
| `a != b`                     | `Bool.isNotEq a b`                                                           |
| `a && b`                     | `Bool.and a b`                                                               |
| <code>a \|\| b</code>        | `Bool.or a b`                                                                |
| `!a`                         | `Bool.not a`                                                                 |
| <code>a \|> f</code>         | `f a`                                                                        |
| <code>f a b \|> g x y</code> | `g (f a b) x y`                                                              |
| `f!`                         | [see example](https://www.roc-lang.org/examples/DesugaringAwait/README.html) |
| `f?`                         | [see example](https://www.roc-lang.org/examples/DesugaringTry/README.html)   |

### [Additional Resources](#additional-resources) {#additional-resources}

You've completed the tutorial, well done!

If you are looking for more resources to learn Roc, check out [these links](/install#additional-learning-resources).

</section>
<script type="text/javascript" src="/builtins/search.js" defer></script>
