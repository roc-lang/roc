app [main!] { pf: platform "../basic-cli/main.roc" }

get_name : { name: Str, age: U64 } -> Str
get_name = |person| person.name

main! = |_| {}
