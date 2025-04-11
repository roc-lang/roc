app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import cli.Stdout

main! = |_args|
    List.range({ start: At(1), end: At(10) })
    |> List.map(fizz_buzz)
    |> Str.join_with(",")
    |> Stdout.line!

## Determine the FizzBuzz value for a given integer.
## Returns "Fizz" for multiples of 3, "Buzz" for
## multiples of 5, "FizzBuzz" for multiples of both
## 3 and 5, and the original number for anything else.
fizz_buzz : I32 -> Str
fizz_buzz = |n|
    fizz = n % 3 == 0
    buzz = n % 5 == 0

    if fizz and buzz then
        "FizzBuzz"
    else if fizz then
        "Fizz"
    else if buzz then
        "Buzz"
    else
        Num.to_str(n)

## Test Case 1: not a multiple of 3 or 5
expect fizz_buzz(1) == "1"
expect fizz_buzz(7) == "7"

## Test Case 2: multiple of 3
expect fizz_buzz(3) == "Fizz"
expect fizz_buzz(9) == "Fizz"

## Test Case 3: multiple of 5
expect fizz_buzz(5) == "Buzz"
expect fizz_buzz(20) == "Buzz"

## Test Case 4: multiple of both 3 and 5
expect fizz_buzz(15) == "FizzBuzz"
expect fizz_buzz(45) == "FizzBuzz"
