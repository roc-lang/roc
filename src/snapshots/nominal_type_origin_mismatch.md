---
META
  description = "Type mismatch showing nominal type origin from different module"
  type = file
SOURCE
  module []

  import Data exposing [Person]

  expectsPerson : Person -> Str
  expectsPerson = \p -> "Got a person"

  main =
      # This will cause a type mismatch
      expectsPerson("not a person")
EXPECTED
  TYPE MISMATCH
PROBLEMS
  This function expects a Person (from Data) value, but you're passing a Str