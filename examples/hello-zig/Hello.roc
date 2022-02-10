app "hello-world"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf


LinkedList a : [ Nil, Cons a (LinkedList a) ]

map : LinkedList a, (a -> b) -> LinkedList b
map = \list, f ->
    when list is
        Nil -> 
            Nil

        Cons x xs ->
            Cons (f x) (map xs f)

greeting =
    hi = "Hello"
    name = "World"

    "\(hi), \(name)!"

main = 
    when map (Cons 32 Nil) (\x -> x + 1) is 
        _ -> greeting
