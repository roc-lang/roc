import OpenMaker

# The rule is the same across modules: an imported function's record does not
# become a padded nominal type here, and gains no field here.

ImportedOpenRecordsRejected :: [].{}

Row := { key : U64, _ : U32, content : Str }

Wide := { key : U64, content : Str, note ?: Str }

padded : Row
padded = OpenMaker.make(1)

widened : Wide
widened = OpenMaker.make(2)

row : Row
row = { key: 3, content: "r" }

taken_apart = OpenMaker.read(row)
