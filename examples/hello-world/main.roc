app "helloWorld"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

# main = "Hello, World!\n"
main = res "hello"


combine = \a, b -> (\x -> x |> a |> b )
const = \x -> (\_y -> x)
# list = [const "a", const "b", const "c"]
list = []
res : Str -> Str
res = List.walk list (const "z") (\c1, c2 -> combine c1 c2)
# res 200
