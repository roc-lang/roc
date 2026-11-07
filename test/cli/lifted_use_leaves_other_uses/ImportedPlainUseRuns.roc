import LiftingMaker

ImportedPlainUseRuns :: [].{}

plain = LiftingMaker.make(2)

plain_token = LiftingMaker.unit(2)

lifted : LiftingMaker.Counter
lifted = LiftingMaker.make(3)

expect Str.inspect(plain) == "{ count: 2 }"
expect Str.inspect(plain_token) == "{}"
expect lifted.describe() == "counter"
expect LiftingMaker.lifted.describe() == "counter"
