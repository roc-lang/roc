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
