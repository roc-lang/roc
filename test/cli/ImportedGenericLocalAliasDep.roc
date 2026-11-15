# Constants whose types keep their variables: a tuple and a list holding
# functions that require a method of the type they are called at.
ImportedGenericLocalAliasDep := [].{
    pair = (|s| s.concat("!"), 1)

    fns = [|s| s.concat(s)]

    # A tuple whose function runs at every type that has a `concat`.
    doubling = (|s| s.concat(s), 1)
}
