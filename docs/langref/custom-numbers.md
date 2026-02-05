# Custom Number Types

You can define your own number types in Roc, by creating a nominal type with a `from_numeral` method:

```roc
Ratio :: {
    numerator : I128,
    denominator : U128,
}.{ 
	from_numeral : Numeral -> Try(Ratio, [InvalidNumeral(Str)])
	from_numeral = |numeral| {
	    numerator = Numeral.init(numeral.is_negative(), numeral.digits_before_pt(), [])
	    denominator = Numeral.init(False, [], numeral.digits_after_pt())

        Ok({
            numerator: I128.from_numeral(numerator)?,
            denominator: U128.from_numeral(denominator)?,
        })
	}
	
	is_negative : Ratio -> Bool
	is_negative = |self| self.numerator.is_negative()
}

expect {
    my_ratio : Ratio
    my_ratio = -123.456
    
    my_ratio.is_negative()
}
```

Roc uses _numeral_ as a shorthand for _number literal_. In the example at the end, `-123.456` is a numeral.

Whenever Roc's type system infers a particular type for a numeral, that type's `from_numeral` method will be called at
compile time. In this `expect` example, the numeral in `my_ratio = -123.456` gets the type `Ratio` from the type annotation,
but it could also have gotten that type in other ways, such as by being passed to a function that expected a `Ratio`.

Once the type system has inferred that `-123.456` is a `Ratio`, it means that `Ratio.from_numeral` will be called at
compile time, passing the `Numeral` representation of `-123.456`. That `Numeral` representation is designed to efficiently
represent arbitrarily-large number literals, which it does by storing three pieces of information:

- A `Bool` for whether it's negative (`True` if negative, `False` otherwise)
- A `List(U8)` representing the digits before the decimal point
- A `List(U8)` representing the digits after the decimal point

In memory, these `List(U8)`s don't store base-10 digits, but rather base-256 digits. That's the most efficient way to store
them in memory, because it doesn't waste any bits; a `U8` can store 256 distinct numbers. 

Note that `from_numeral` returns a `Try`. This is because it can fail in various ways, including:

- The number is an integer type, but there were digits after the decimal point specified
- The digits were outside the number type's supported range
- The numeral was specified to be negative, but the number type does not support negative numbers

Since `from_numeral` is run at compile time, you get a compile-time error if the numeral doesn't work with the custom number.
(The `Str` in the `InvalidNumeral(Str)` tag will be shown in the compile-time error message, and should explain what the problem was.) You also get an error if `from_numeral` crashes.

Once your custom number type has `from_numeral`, you can use regular number literals to represent it. You can also make it
work with arithmetic operators by giving it the methods those operators desugar to. For example, `a + b` desugars to `a.plus(b)`,
so if you want your number type to support `+`, give it a `plus` method.

Let's say we implemented a `div` method on our `Ratio` type:

```roc
div : Ratio, Ratio -> Ratio
```

Now we can do this:

```roc
one_third : Ratio
one_third = 1 / 3
```

The `1 / 3` desugars to `1.div(3)`, and the `one_third : Ratio` type annotation means this `div` method is inferred to return a `Ratio`.

There's an important detail here about how the desugaring from `1 / 3` to `1.div(3)` works: it adds the constraint that the return type of `div` is the same as its first argument.

To see why this distinction matters, let's suppose I literally wrote the desugared version:

```roc
one_third : Ratio
one_third = 1.div(3)
```

In this version, the inferred type of that `1` number would be:

```roc
num where [
    # `1` has a `from_numeral` method
    num.from_numeral : Numeral -> Try(num, [InvalidNumeral(Str)]), 
    
    # It also has a `div` method that returns a `Ratio`
    num.div : num, other -> Ratio, 
    
    # The other argument to `div` has `from_numeral` too, but that's all we know
    other.from_numeral : Numeral -> Try(other, [InvalidNumeral(Str)]), 
]
```

However, because `1 / 3` desguars to `1.div(3)` _with the additional constraint_ that the return type of `div`
is the same as the type of its first argument, the fact that we already know `div` returns a `Ratio` means that we know
`1` must be a `Ratio` as well, at which point since `Ratio.div` has the type `Ratio, Ratio -> Ratio`, we also know
that `3` must be a `Ratio` too.

So it's a slight oversimplification to say that `1 / 3` "desugars" to `1.div(3)`, 
because of the additional constraint that `div`'s return type must be the same as 
its first argument.

Note that there is no constraint on the other argument. This is important, because some
number types have arithmetic operations where the arguments have different types.

A straightforward example of this is `Duration`. If I make a duration of 5 seconds,
I want to be able to divide that by an integer, such as `U32`, not another `Duration`.
It makes sense to have "4 seconds divided by 2," and it would not make sense to
require saying "4 seconds divided by 2 seconds" - which would be the requirement if
`div` assumed both arguments had the same type (like they happen to in number types
like `Ratio` and `Dec`, but not in number types like `Duration`).

## Number suffix

You can optionally specify a number type inline using a suffix like so:

```roc
custom = -123.456.Foo
```

This will result in `Foo.from_numeral` being called at compile time on the numeral `-123.456`, giving a compile-time error if the `Try` it returns is an `Err`, and so on. This works the same  way for custom number types and builtin ones, such as `42.I64` or `12.34.F32`.

Note that when using this suffix syntax, `Foo.from_numeral` does not have to return a `Foo`!

An example of where this is useful: you can create a type called `Sec` (for "seconds") and another called `Min` (for "minutes") which define `from_numeral` methods that both return `Duration` rather than `Sec` or `Min`, like so:

```roc
Sec :: {}.{
    from_numeral : Numeral -> Try(Duration, [InvalidNumeral(Str)]), 
    from_numeral = |numeral| Ok(Duration.from_seconds(I64.from_numeral(numeral)?))
}

Min :: {}.{
    from_numeral : Numeral -> Try(Duration, [InvalidNumeral(Str)]), 
    from_numeral = |numeral| Ok(Duration.from_minutes(I64.from_numeral(numeral)?))
}
```

These let you write expressions like:

```roc
duration : Duration
duration = 5.Min + 30.Sec
```

This is nicely human-readable, and will be evaluated at compile time into a `Duration` constant.

## Defaulting to `Dec`

Earlier, we saw this example:

```roc
my_ratio : Ratio
my_ratio = -123.456
```

Here, `-123.456` gets the inferred type of `Ratio`, so `Ratio.from_numeral` will be called on it.

However, when the inferred type of the numeral is instead `num where […]` like we saw in the `1 / 3` example,
Roc's compiler uses other factors to determine which `from_numeral` to call.

When no other information is present, the default numeric type used is `Dec`. Here's a simple example of where
that comes up:

```roc
if 1 + 1 == 2 {
    # …
}
```

How does Roc know whether to run the code inside that `if`? None of the numerals in `1 + 1 == 2` will be inferred
to have specific types, so how can the compiler know what concrete type to use for the calculation?

The answer is that it defaults to `Dec`. So this ends up being equivalent to `if 1.Dec + 1.Dec == 2.Dec`.
