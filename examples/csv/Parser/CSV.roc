interface Parser.CSV
  exposes [
  CSV,
  CSVRecord,
  CSVField, # <- Might be unneeded?
  file,
  record,
  parseStr,
  parseCSV,
  field,
  ]
  imports [
  Parser.Core.{Parser, fail, const, alt, map, map2, apply, many, oneorMore, sepBy1, between, ignore},
  Parser.Str.{RawStr, parseStrPartial, parseStr, oneOf, codepoint, codepointSatisfies, string, scalar, digits}
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

parseStr : Parser CSVRecord a, Str -> List a
parseStr = \csvParser, input ->
  csvData = parseStrToCSV input
  parseCSV csvParser csvData

parseCSV : Parser CSVRecord a, CSV -> List a
parseCSV = \parser, csvData ->
  parse (many csvParser) csvData

record : Parser CSVRecord a
record = Parser.Core.const

field : Parser RawStr a -> Parser CSVRecord a
field = \fieldParser ->
  buildPrimitiveParser \csvRecord ->
    when List.get csvRecord 0 is
      Err OutOfBounds ->
        Err (ParsingFailure "expected another CSV field but there are no more fields in this record")
      Ok rawStr ->
        when Parser.Str.parseRawStr fieldParser rawStr is
          Ok val ->
            {val: val, input: (List.dropFirst csvRecord)}
          Err (ParsingFailure reason) ->
            Err (ParsingFailure reason)
          Err (ParsingIncomplete reason) ->
            Err (ParsingFailure "The field parser was unable to read the whole field: \(reason)")


parseStrToCSV : Parser Str CSV
parseStrToCSV = \input ->
  parse file (Str.toUtf8 input)

parseStrToCSVRecord : Parser Str CSVRecord
parseStrToCSVRecord = \input ->
  parse record (Str.toUtf8 input)


# The following are parsers to turn strings into CSV structures

file : Parser RawStr CSV
file = many recordNewline

# The following compiles 6x slower, but follows the RFC to the letter (allowing the final CRLF to be omitted)
# file : Parser RawStr CSV
# file = map2 (many recordNewline) (alt record recordNewline) (\records, finalRecord -> List.concat records [finalRecord])

recordNewline : Parser RawStr CSVRecord
recordNewline = map2 csvRecord endOfLine (\rec, _ -> rec)

csvRecord : Parser RawStr CSVRecord
csvRecord = sepBy1 csvField comma

csvField : Parser RawStr CSVField
csvFfield = alt escapedCsvField nonescapedCsvField

escapedCsvField : Parser RawStr CSVField
escapedCsvField = between escapedContents dquote dquote
escapedContents = many (oneOf [
  twodquotes |> map (\_ -> 34), # An escaped double quote
  comma,
  cr,
  lf,
  textdata
])

twodquotes = string "\"\""

nonescapedCsvField : Parser RawStr CSVField
nonescapedCsvField = many textdata
comma = codepoint 44 # ','
dquote = codepoint 34 # '"'
endOfLine = alt (ignore crlf) (ignore lf)
cr = codepoint 13 # '\r'
lf = codepoint 10 # '\n'
crlf = string "\r\n"
textdata = codepointSatisfies (\x -> (x >= 32 && x <= 33) || (x >= 35 && x <= 43) || (x >= 45 && x <= 126)) # Any printable char except " (34) and , (44)
