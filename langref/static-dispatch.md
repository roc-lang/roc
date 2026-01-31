# Static Dispatch

_Dispatch_ is where the same call expression can result in a different function being run,
depending on the types of its arguments and/or return value. It's a form of [ad hoc polymorphism](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism).

[_Static_ dispatch](https://en.wikipedia.org/wiki/Static_dispatch) is where only types known
at compile time affect which function gets run. This is in contrast to [_dynamic_ dispatch](https://en.wikipedia.org/wiki/Dynamic_dispatch),
which uses runtime information to decide which function gets run.

Roc's only ad hoc polymorphism system is static dispatch, and dynamic dispatch is unsupported
by design. A major reason for this is that Roc's static dispatch has no runtime overhead; 
after compilation, it's exactly as if you had called the function directly. (In contrast, 
it's impossible to avoid runtime overhead in dynamic dispatch, because it has to process 
information at runtime to do the dispatch.) 

## Methods

A _method_ is a function that's associated with a type and are defined in the `.{ }` block after a nominal type:

```roc
Counter := { value: I64 }.{
    new : () -> Counter
    new = || { value: 0 }

    increment : Counter -> Counter
    increment = |{ value }| { value: value + 1 }
}
```

```roc
counter : Counter
counter = Counter.new().increment()
```

## Special Methods

Roc recognizes certain method names as having special meaning. When defined on a nominal type, these methods are called automatically in specific contexts.

### `to_inspect`

The `to_inspect` method customizes how a value is rendered by `Str.inspect`.

```roc
Color := [Red, Green, Blue].{
    to_inspect : Color -> Str
    to_inspect = |color| match color {
        Red => "Color::Red"
        Green => "Color::Green"
        Blue => "Color::Blue"
    }
}
```

When `Str.inspect` is called on a `Color` value, it uses the custom `to_inspect` method instead of the default rendering:

```roc
red : Color
red = Red

Str.inspect(red)  # "Color::Red"
```

Without a `to_inspect` method, the default rendering includes the type name:

```roc
ColorDefault := [Red, Green, Blue]

c : ColorDefault
c = Red

Str.inspect(c)  # "ColorDefault.Red"
```