MyNum := { value : U64 }.{
    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
    from_numeral = |_| Ok({ value: bump })
}

number : MyNum
number = 1

bump : U64
bump = number.value

main! = |_| Ok({})
