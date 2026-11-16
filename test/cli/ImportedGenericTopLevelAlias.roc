# Top-level names for an imported constant whose type keeps its variables: a
# name for the whole constant, and names a destructuring binds. Each name is
# generalized, so every use reads the constant at the use's own types.
import ImportedGenericLocalAliasDep

ImportedGenericTopLevelAlias := [].{}

# A name for the whole constant, read through at two types.
whole = ImportedGenericLocalAliasDep.doubling

whole_text = (whole.0)("ab")
whole_bytes = (whole.0)([1.U8, 2])

# A name a tuple destructuring binds, called at two types.
(tuple_twice, _) = ImportedGenericLocalAliasDep.doubling

tuple_uses = (tuple_twice("ab"), tuple_twice([1.U8, 2]))

# Two names one tuple destructuring binds.
(shout, count) = ImportedGenericLocalAliasDep.pair

counted = (shout("hi"), count + 1)

# Names a record destructuring binds, one of them called at two types.
{ twice, tag } = ImportedGenericLocalAliasDep.rec

record_uses = (twice("ab"), twice([1.U8, 2]), tag)

# A destructured name used at a variable of the function that uses it.
generic = |x| tuple_twice(x)

expect whole_text == "abab"
expect whole_bytes == [1, 2, 1, 2]
expect tuple_uses == ("abab", [1, 2, 1, 2])
expect counted == ("hi!", 2)
expect record_uses == ("abab", [1, 2, 1, 2], "t")
expect generic("gh") == "ghgh"
