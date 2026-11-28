TailCarry :: [].{
    forward : a, U64 -> a
    forward = |x, n| if n == 0 x else TailCarry.forward_back(x, n - 1)

    forward_back : a, U64 -> a
    forward_back = |x, n| if n == 0 x else TailCarry.forward(x, n - 1)

    # Only `double` produces bytes in the frame it replaces.
    mixed : a, U64 -> a where [a.plus : a, a -> a]
    mixed = |x, n| if n == 0 x else TailCarry.double(x, n - 1)

    double : a, U64 -> a where [a.plus : a, a -> a]
    double = |x, n| if n == 0 x else TailCarry.mixed(x + x, n - 1)

    closed : U64, U64 -> U64
    closed = |x, n| if n == 0 x else TailCarry.closed_back(x, n - 1)

    closed_back : U64, U64 -> U64
    closed_back = |x, n| if n == 0 x else TailCarry.closed(x, n - 1)
}
