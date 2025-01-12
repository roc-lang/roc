## [Building an Application](/tutorial/applications) {/tutorial/applications}

Now that we've [gotten familiar with the REPL](/tutorial/repl), let's create our first Roc application!

Make a file named `main.roc` and put this in it:

```roc
echo!("Hi there, from inside a Roc app. 🎉")
```

This `echo!` function we've called is a built-in function that prints a string to the console.
(The `!` at the end of the name is a naming convention for functions that have [side effects](https://en.wikipedia.org/wiki/Side_effect_(computer_science)).
We'll discuss side effects more later.)

Try running this program with:

<samp>roc main.roc</samp>

You should see this:

<samp>Hi there, from inside a Roc app. 🎉</samp>

Congratulations, you've written your first Roc application!

### [Defs](#defs) {#defs}

Try replacing the line in `main.roc` with this:

```roc
birds = 3
iguanas = 2
total = (birds + iguanas).to_str()

echo!("There are ${total} animals.")
```

You should see this:

<samp>There are 5 animals.</samp>

`main.roc` now has three definitions (_defs_ for short) `birds`, <span class="nowrap">`iguanas`,</span> and `total`.

A definition names an expression.

- The first two defs assign the names `birds` and `iguanas` to the expressions `3` and `2`.
- The next def assigns the name `total` to the expression `(birds + iguanas).to_str()`.

Once we have a def, we can use its name in other expressions. For example, the `total` expression refers to `birds` and `iguanas`, and the expression `echo!("There are ${total} animals.")` refers to `total`.

You can name a def using any combination of letters and numbers, but they have to start with a lowercase letter.

**Note:** Defs are constant; they can't be reassigned. We'd get an error if we wrote these two defs in the same scope:

```roc
birds = 3
birds = 2
```

Later, we'll learn about _vars_, which support reassignment but are less flexible
than defs in other ways.

### [Defining Functions](#defining-functions) {#defining-functions}

So far we've called functions like `Num.to_str`, `Str.concat`, and `echo!` which were
already predefined for us. Next let's try defining a function of our own.

```roc
add_and_stringify = |num1, num2| (num1 + num2).to_str()
birds = 3
iguanas = 2
total = add_and_stringify(birds, iguanas)

echo!("There are ${total} animals.")
```

This new `add_and_stringify` function we've defined accepts two numbers, adds them,
calls `.to_str()` on the result, and returns that.

The `|num1, num2|` syntax defines a function's arguments, and the expression afterwards
(in this case, the expression `(num1 + num2).to_str()`) is the body of the function.
Whenever a function gets called, its body expression gets evaluated and returned.

### [`if`-`then`-`else` expressions](#if-then-else) {#if-then-else}

Let's modify this function to return an empty string if the numbers add to zero.

```roc
add_and_stringify = |num1, num2|
    sum = num1 + num2

    if sum == 0 then
        ""
    else
        sum.to_str()
```

We did two things here:

- We introduced a _local def_ named `sum`, and set it equal to `num1 + num2`. Because we defined `sum` inside `add_and_stringify`, it's _local_ to that scope and can't be accessed outside that function.
- We added an `if`\-`then`\-`else` conditional to return either `""` or `sum.to_str()` depending on whether `sum == 0`.

This `if` must be accompanied by both `then` and also `else`. That's because this `if` is
being used as an expression (namely, the expression this `add_and_stringify` function is returning),
and all expressions must evaluate to a value. Without the `else` branch, the function would
not know what to return if `sum` was nonzero!

### [`else if` expressions](#else-if) {#else-if}

We can combine `if` and `else` to get `else if`, like so:

```roc
add_and_stringify = |num1, num2|
    sum = num1 + num2

    if sum == 0 then
        ""
    else if sum < 0 then
        "negative"
    else
        sum.to_str()
```

Note that `else if` is not a separate language keyword! It's just an `if`/`else` where the `else`
branch contains another `if`/`else`. This is easier to see with different indentation:

```roc
add_and_stringify = |num1, num2|
    sum = num1 + num2

    if sum == 0 then
        ""
    else
        if sum < 0 then
            "negative"
        else
            sum.to_str()
```

This differently-indented version is equivalent to writing `else if sum < 0 then` on the same line,
although the convention is to use the original version's indentation style.

### [Getting user input](#input) {#input}

Let's make this program interactive! We can prompt the user to input our two numbers
before adding them together. Let's replace our `birds` and `iguanas` defs with the following:

```roc
echo!("Enter the number of birds:")
birds = input!()

echo!("Enter the number of iguanas:")
iguanas = input!()
```

If you try to run this program, you'll see a compile error that has this at the top:

<pre>
TYPE MISMATCH
</pre>

The problem is that the input!() function returns a string, but we're trying to use `birds`
and `iguanas` as numbers. The error calls this a _type mismatch_ because there's a mismatch
between the type we're using in one place and what it's expected to be in another.

The reason you're seeing this error before the program even runs is that Roc's compiler
does [type inference](https://en.wikipedia.org/wiki/Type_inference) to infer the types
of everything in your program at compile time. That's what it was using to print out the
types of things [in the REPL earlier](/tutorial/repl)—it knows them all before running anything!

### [Converting strings to numbers](#str-to-num) {#str-to-num}

When we convert a string to a number in Roc, we have to specify what number type we want.

Roc has several different number types; in this case, we're going to choose [`I64`](https://www.roc-lang.org/builtins/Num#U64),
which is a 64-bit [integer](https://en.wikipedia.org/wiki/Integer_(computer_science)#Value_and_representation).
Without getting into the details, an `I64` can hold any integer up to about 9 [quintillion](https://en.wikipedia.org/wiki/Names_of_large_numbers#Quintillion)
(and they can also be negative), which should be plenty to represent the number of iguanas and
birds in our program.

We can use the [`Str.to_i64`](https://www.roc-lang.org/builtins/Str#to_i64) function to convert
our strings to numbers:

```roc
birds = input!().to_i64()
```
```roc
iguanas = input!().to_i64()
```

This still won't quite work, however. That's becasue converting from strings to numbers
is an operation that can fail, and we haven't yet specified what to do if it fails.

For example, if I input the string `"42"`, it can be converted to the number `42`.
However, if I input the string `"jumping up and down"`, it can't be converted to a number
and the operation will fail. The fact that `Str.to_i64` can fail is a part of the function,
and Roc will give a compile error if that failure scenario has not been handled.

We can specify how to handle the failure by providing a default value of `0` like so:

```roc
birds = input!().to_i64() ?? 0
```
```roc
iguanas = input!().to_i64() ?? 0
```

Breaking down what each of these expressions is doing:
- `input!()` gets input from the user and returns it as a string.
- `.to_i64()` attempts to convert that string into an `I64` integer, which can fail.
- `?? 0` says that if it fails, fall back on using the integer `0` by default.

Now that we've specified how to handle failure, we can run the program and try inputting
different values to see what it prints out!

If you try entering an invalid string, you'll see that it gets silently interpreted as zero,
and then the program prints the wrong answer. This is not a great user experience, and
we'll improve on it in the next section by using [pattern matching](/tutorial/pattern-matching)
to handle errors like this more gracefully.
