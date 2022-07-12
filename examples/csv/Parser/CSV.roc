interface Parser.CSV
  exposes [
  CSV,
  file,
  record,
  escapedField, # TODO
  escapedContents, # TODO
  ]
  imports [
  Parser.Core.{Parser, fail, const, alt, map, map2, apply, many, oneorMore, sepBy1, between, ignore},
  Parser.Str.{RawStr, runPartialStr, runStr, oneOf, codepoint, codepointSatisfies, string, scalar, digits}
  ]

## This is a CSV parser which follows RFC4180
##
## For simplicity's sake, the following things are not yet supported:
## - CSV files with headings
## - A file not ending in a final CRLF ("\r\n").
##
## The following however *is* supported
## - A simple LF ("\n") instead of CRLF ("\r\n") to separate records (and at the end).

CSVField : RawStr
CSVRecord : List CSVField
CSV : List CSVRecord

file : Parser RawStr CSV
file = many recordNewline

# The following compiles 6x slower, but follows the RFC to the letter (allowing the final CRLF to be omitted)
# file = map2 (many recordNewline) (alt record recordNewline) (\records, finalRecord -> List.concat records [finalRecord])

recordNewline : Parser RawStr CSVRecord
recordNewline = map2 record endOfLine (\rec, _ -> rec)

record : Parser RawStr CSVRecord
record = sepBy1 field comma

field : Parser RawStr CSVField
field = alt escapedField nonescapedField

escapedField : Parser RawStr CSVField
escapedField = between escapedContents dquote dquote
# escapedContents = many (oneOf [
#   twodquotes |> map (\_ -> 34), # An escaped double quote
#   comma,
#   cr,
#   lf,
#   textdata
# ])
escapedContents =
  (twodquotes |> map (\_ -> 34))
  |> alt comma
  |> alt cr
  |> alt lf
  |> alt textdata
  |> many

twodquotes = string "\"\""

nonescapedField : Parser RawStr CSVField
nonescapedField = many textdata
comma = codepoint 44 # ','
dquote = codepoint 34 # '"'
endOfLine = alt (ignore crlf) (ignore lf)
cr = codepoint 13 # '\r'
lf = codepoint 10 # '\n'
crlf = string "\r\n"
textdata = codepointSatisfies (\x -> (x >= 32 && x <= 33) || (x >= 35 && x <= 43) || (x >= 45 && x <= 126)) # Any printable char except " (34) and , (44)
