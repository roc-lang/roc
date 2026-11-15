# A local name bound to an imported constant whose type keeps its variables.
# The name is generalized, so the declaration computes nothing: each use of
# the name reads the constant at the use's own types, and a use of a name a
# destructuring binds reads the constant and takes it apart at those types.
import ImportedGenericLocalAliasDep

ImportedGenericLocalAlias := [].{}

# A name a destructuring binds, called afterwards.
destructured = {
    (shout, _) = ImportedGenericLocalAliasDep.pair
    shout("hi")
}

# A name for the whole constant, read through at two types.
whole = {
    fns = ImportedGenericLocalAliasDep.fns
    text = match fns {
        [twice] => twice("ab")
        _ => ""
    }
    bytes = match fns {
        [twice] => twice([1.U8, 2])
        _ => []
    }
    (text, bytes)
}

# A destructured name a closure captures.
captured = {
    (shout, _) = ImportedGenericLocalAliasDep.pair
    apply = |x| shout(x)
    apply("cd")
}

# A use whose type is a variable of the enclosing function.
generic = |x| {
    (shout, _) = ImportedGenericLocalAliasDep.pair
    shout(x)
}

# A name a destructuring binds, called at two types.
destructured_twice = {
    (twice, _) = ImportedGenericLocalAliasDep.doubling
    (twice("ab"), twice([1.U8, 2]))
}

expect destructured == "hi!"
expect whole == ("abab", [1, 2, 1, 2])
expect captured == "cd!"
expect generic("gh") == "gh!"
expect destructured_twice == ("abab", [1, 2, 1, 2])
