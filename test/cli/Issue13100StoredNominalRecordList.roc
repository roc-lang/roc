# https://github.com/roc-lang/roc/issues/13100
# A stored list retains its element's nominal record type.
Id := { bits : Str }

empty : List(Id)
empty = []

expect empty.len() == 0
