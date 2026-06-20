Complex := { re: F64, im: F64 }.{
    is_eq: Complex, Complex -> Bool
    is_eq = |a, b| a.re == b.re and a.im == b.im

    plus: Complex, Complex => Complex
    plus = |a, b| {
        re: a.re + b.re,
        im: a.im + b.im,
    }

    times = |a, b| {
        re: a.re * b.re - a.im * b.im,
        im: a.re * b.im + a.im * b.re,
    }
}
