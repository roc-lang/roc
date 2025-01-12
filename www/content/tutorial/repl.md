## [The REPL](/tutorial/repl) {/tutorial/repl}

Let's start by getting acquainted with Roc's [_Read-Eval-Print-Loop_](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop), or **REPL** for short.

You can either use the online REPL at [roc-lang.org/repl](https://www.roc-lang.org/repl), or you can run this in a terminal:

<pre><samp>roc repl</samp></pre>

If Roc is [installed](/install), you should see this:

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

When you entered the _expression_ `"Hello, World!"`, the REPL printed it back out.

It also printed `: Str`, because `Str` is that expression's _type. We'll talk about types later; for now, let's ignore the `:` and whatever comes after it whenever we see them.

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

<pre><samp>2 <span class="colon">:</span> Num a</samp></pre>

According to the REPL, one plus one equals two. Sounds right!

Roc will respect [order of operations](https://en.wikipedia.org/wiki/Order_of_operations) when using multiple arithmetic operators like `+` and `-`, but you can use parentheses to specify exactly how they should be grouped.

<pre><samp><span class="repl-prompt">1 <span class="op">+</span> 2 <span class="op">*</span> (3 <span class="op">-</span> 4)

-1 <span class="colon">:</span> Num a
</span></samp></pre>

Roc supports different [types of numbers](https://www.roc-lang.org/builtins/Num), such as [integers](https://www.roc-lang.org/builtins/Num#Int)
and [fractional numbers](https://www.roc-lang.org/builtins/Num#Frac). We'll use more of them later.

### [Calling Functions](#calling-functions) {#calling-functions}

Let's try calling a function:

<pre><samp><span class="repl-prompt">Str.concat("Hi ", "there")</span>

<span class="literal">"Hi there"</span> <span class="colon">:</span> Str
</samp></pre>

Here we're calling the `Str.concat` function and passing two arguments: the string `"Hi "` and the string `"there"`. This _concatenates_ the two strings together (that is, it puts one after the other) and returns the resulting combined string of `"Hi there"`.

We can nest function calls, to pass the value returned by one function as an argument to another:

<pre><samp><span class="repl-prompt">Str.concat("Birds: ", Num.to_str(42))</span>

<span class="literal">"Birds: 42"</span> <span class="colon">:</span> Str
</samp></pre>

Both the `Str.concat` function and the `Num.to_str` function have a dot in their names. In `Str.concat`, `Str` is the
name of a _module_, and `concat` is the name of a function inside that module. Similarly, `Num` is a module,
and `to_str` is a function inside that module. (It's common for a type to be defined in a module with the same name,
such as the `Str` type being defined in the `Str` module and the `Num` type being defined in the `Num` module.)

We'll get into more depth about modules later, but for now you can think of a module as a named collection of functions.
Eventually we'll discuss how to use them for more than that.

### [Static Dispatch](#static-dispatch) {#static-dispatch}

We can write this code more concisely using Roc's [static dispatch](https://en.wikipedia.org/wiki/Static_dispatch):

<pre><samp><span class="repl-prompt">"Birds: ".concat(42.to_str())</span>

<span class="literal">"Birds: 42"</span> <span class="colon">:</span> Str
</samp></pre>

This code does the same thing as the previous code:
- `"Birds: ".concat(…)` is equivalent to `Str.concat("Birds: ", …)`
- `42.to_str()` is equivalent to `Num.to_str(42)`

Here's what this code is doing:
1. When you write `"Birds: ".concat(…)`, the compiler first infers the type of the value before the `.` (In this case, its type would be `Str`.)
2. Next, it looks up where that type was originally defined. (The `Str` type is defined in the `Str` module.)
3. Inside that module, it looks for an exposed function named `concat`. (It finds one: the `Str.concat` function we called previously!)
4. Finally, it calls that function passing the value before the `.` as the first argument. (So, passing `"Birds: "` as the first argument in this case.)

Similarly, `42.to_str()` resolves to `Num.to_str(42)` because:
- `42` is a `Num`, and the `Num` type is defined in the `Num` module
- The `Num` module exposes a function named `Num.to_str`

This process is called _static_ dispatch because it all happens at compile time. Some other languages have
[dynamic dispatch](https://en.wikipedia.org/wiki/Dynamic_dispatch), in which code like this can change
meaning at runtime, but dynamic dispatch has runtime overhead and Roc does not support it by design.

> We informally refer to this style of function calling as "method calling," because it's how [methods](https://en.wikipedia.org/wiki/Method_(computer_programming))
> are typically called in languages that have those as a first-class language feature. Roc has no first-class way to declare
> a "method"—as we saw, `my_str.concat(…)` resolves to a call to the ordinary `Str.concat` function—but we do casually
> use the term "method" to refer to functions that can be called in this style.
>
> So if someone refers to "the `concat` method on `Str`," they're talking about the `Str.concat` function.

At runtime there's no difference between these two calling styles, because they both result in the functions `Str.concat`
and `Num.to_str` being called with the same arguments. It's entirely a stylistic choice which way you decide to write this,
although the *method-style calling* is generally the preferred style to use in Roc.

### [String Interpolation](#string-interpolation) {#string-interpolation}

An alternative syntax for `Str.concat` is _string interpolation_, which looks like this:

<pre><samp class="repl-prompt"><span class="literal">"<span class="str-esc">${</span><span class="str-interp">greeting</span><span class="str-esc">}</span>, <span class="str-esc">${</span><span class="str-interp">audience</span><span class="str-esc">}</span>."</span></samp></pre>

This is syntax sugar for calling `Str.concat` several times, like so:

```roc
greeting.concat(", ".concat(audience.concat(".")))
```

You can put entire single-line expressions between the `${` and `}` in string interpolation. For example:

<pre><samp class="repl-prompt"><span class="literal">"Two plus three is: <span class="str-esc">${</span><span class="str-interp">(2 + 3).to_str()</span><span class="str-esc">}</span>"</span></samp></pre>

By the way, there are many other ways to put strings together! Check out the [documentation](https://www.roc-lang.org/builtins/Str) for the `Str` module for more.

Next, we'll move out of the REPL and into [Building an Application](/tutorial/applications).
