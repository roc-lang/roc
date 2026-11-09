import LiftingMaker

# The imported module lifts one use of each function into a nominal type.
# That use leaves the functions' own types anonymous, so a use here has no
# methods to call.

ImportedPlainUse :: [].{}

plain = LiftingMaker.make(2)

plain_token = LiftingMaker.unit(2)

expect plain.describe() == "counter"
expect plain_token.describe() == "token"
