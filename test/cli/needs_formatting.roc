app [main!] { pf: platform "../fx/platform/main.roc" }

# This file has intentional formatting issues
get_name : { name: Str, age: U64 } -> Str
get_name = |person|    person.name

add :    U64,U64->U64
add=|x,y|x+y

main! = || {}
