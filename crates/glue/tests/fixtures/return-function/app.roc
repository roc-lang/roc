app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main : { f: I64, I64 -> I64, g: I64, I64 -> I64 }
main = { f: add, g: sub }

add : I64, I64 -> I64
add = \x, y -> x + y

sub : I64, I64 -> I64
sub = \x, y -> x - y
