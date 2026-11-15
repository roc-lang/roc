# Constants whose types keep their variables: a tuple and a list holding
# functions that require a method of the type they are called at.
ImportedGenericLocalAliasDep := [].{
    pair = (|s| s.concat("!"), 1)

    fns = [|s| s.concat(s)]
}
