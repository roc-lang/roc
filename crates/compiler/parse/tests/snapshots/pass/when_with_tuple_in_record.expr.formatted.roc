when { foo: (1, 2) } is
    { foo: (1, x) } -> x
    { foo: (_, b) } -> 3 + b