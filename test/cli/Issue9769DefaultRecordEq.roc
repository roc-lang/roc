Issue9769DefaultRecordEq :: [].{}

Rational := { num : I64, den : I64 }.{
    new : { num : I64, den : I64 } -> Rational
    new = |{ num, den }| {
        { num, den }->reduce()
    }

    plus : Rational, Rational -> Rational
    plus = |{ num: num1, den: den1 }, { num: num2, den: den2 }| {
        { num: num1 * den2 + num2 * den1, den: den1 * den2 }->reduce()
    }

    reduce : Rational -> Rational
    reduce = |{ num, den }| {
        gcd = |m, n| if n == 0 {
            m
        } else {
            gcd(n, (m % n))
        }

        sign = |n| if n < 0 {
            -1
        } else {
            1
        }

        abs_num = num.abs()
        abs_den = den.abs()
        d = gcd(abs_num, abs_den)

        { num: sign(num) * sign(den) * abs_num // d, den: abs_den // d }
    }
}

# repro for https://github.com/roc-lang/roc/issues/9769
expect {
    r1 = Rational.new({ num: 1, den: 2 })
    r2 = Rational.new({ num: 2, den: 3 })
    result = r1 + r2

    result == Rational.new({ num: 7, den: 6 })
}

expect {
    r1 = Rational.new({ num: 1, den: 2 })
    r2 = Rational.new({ num: -2, den: 3 })
    result = r1 + r2

    result == Rational.new({ num: -1, den: 6 })
}
