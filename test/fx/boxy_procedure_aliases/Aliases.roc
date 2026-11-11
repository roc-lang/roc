Aliases :: [].{
    identity = |value| value
    direct = identity
    chained = direct

    make = |suffix| |value| "${value}${suffix}"
    stored : Str -> Str
    stored = make("!")
}
