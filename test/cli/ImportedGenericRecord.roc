# Reads of an imported constant whose type keeps its variables. Each read
# computes the constant at the read's own types, so the requirement its
# functions place on those types is met by the read: `concat` of `Str` at
# one read and of `List(U8)` at another.
import ImportedGenericRecordDep

ImportedGenericRecord := [].{}

sent : Str
sent = (ImportedGenericRecordDep.effects.send)("hi")

doubled_str : Str
doubled_str = (ImportedGenericRecordDep.effects.twice)("ab")

doubled_list : List(U8)
doubled_list = (ImportedGenericRecordDep.effects.twice)([1, 2])

# A read inside a function body, computed when the function runs.
run = |s| (ImportedGenericRecordDep.effects.send)(s)

# The function read out of the record and bound to a name.
held = ImportedGenericRecordDep.effects.send

expect sent == "hi!"
expect doubled_str == "abab"
expect doubled_list == [1, 2, 1, 2]
expect run("yo") == "yo!"
expect held("x") == "x!"
