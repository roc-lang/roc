# Constants whose types keep their variables: a tuple, a list and two records
# holding functions that require a method of the type they are called at.
ImportedGenericLocalAliasDep := [].{
    pair = (|s| s.concat("!"), 1)

    fns = [|s| s.concat(s)]

    rec = { twice: |s| s.concat(s), tag: "t" }

    optional : { twice ?: _, tag : Str }
    optional = { twice: |s| s.concat(s), tag: "o" }
}
