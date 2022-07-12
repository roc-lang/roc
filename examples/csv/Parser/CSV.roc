interface Parser.CSV
  exposes [
  CSV,
  file,
  record
  ]
  imports [
  Parser.Core.{Parser, fail, const, alt, map, map2, apply, many, oneorMore, sepBy1, between, ignore},
  Parser.Str.{RawStr, runPartialStr, runStr, oneOf, codepoint, codepointSatisfies, string, scalar, digits}
  ]

## This is a CSV parser which follows RFC4180
##
## For simplicity's sake, the following things are not yet supported:
## - A file not ending in a final CRLF.
## - CSV files with headings

CSVField : RawStr
CSVRecord : List CSVField
CSV : List CSVRecord

file : Parser RawStr CSV
file = many recordNewline

recordNewline : Parser RawStr CSVRecord
recordNewline = map2 record crlf (\rec, _ -> rec)

record : Parser RawStr CSVRecord
record = sepBy1 field comma

field : Parser RawStr CSVField
field = alt escapedField nonescapedField

escapedField : Parser RawStr CSVField
escapedField = between (many escapedContents) dquote dquote
escapedContents = oneOf [
  twodquotes |> map (\_ -> 34), # An escaped double quote
  comma,
  cr,
  lf,
  textdata
]
twodquotes = string "\"\""

nonescapedField : Parser RawStr CSVField
nonescapedField = many textdata
comma = codepoint 44 # ','
cr = codepoint 13 # '\r'
dquote = codepoint 34 # '"'
lf = codepoint 10 # '\n'
crlf = string "\r\n"
textdata = codepointSatisfies (\x -> (x >= 32 && x <= 33) || (x >= 35 && x <= 43) || (x >= 45 && x <= 126))
