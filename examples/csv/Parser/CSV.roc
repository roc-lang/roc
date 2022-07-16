interface Parser.CSV
  exposes [
  CSV,
  CSVRecord,
  file,
  record,
  parseStr,
  parseCSV,
  parseStrToCSVRecord,
  field,
  string,
  nat,
  f64
  ]
  imports [
  Parser.Core.{Parser, parse, buildPrimitiveParser, fail, const, alt, map, map2, apply, many, oneorMore, sepBy1, between, ignore, flatten},
  Parser.Str.{RawStr, parseStrPartial, oneOf, codepoint, codepointSatisfies, scalar, digits, strFromRaw}
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

parseStr : Parser CSVRecord a, Str -> Result (List a) [ParsingFailure Str, SyntaxError Str, ParsingIncomplete CSVRecord]
parseStr = \csvParser, input ->
  when parseStrToCSV input is
    Err (ParsingIncomplete rest) ->
      restStr = Parser.Str.strFromRaw rest
      Err (SyntaxError restStr)
    Err (ParsingFailure str) ->
      Err (ParsingFailure str)
    Ok csvData ->
      when parseCSV csvParser csvData is
        Err (ParsingFailure str) ->
          Err (ParsingFailure str)
        Err (ParsingIncomplete problem) ->
          Err (ParsingIncomplete problem)
        Ok vals ->
          Ok vals

parseCSV : Parser CSVRecord a, CSV -> Result (List a) [ParsingFailure Str, ParsingIncomplete CSVRecord]
parseCSV = \csvParser, csvData ->
  List.walkUntil csvData (Ok []) \state, recordList ->
    when parse csvParser recordList (\leftover -> leftover == []) is
      Err (ParsingFailure problem) ->
        recordStr = recordList |> List.map strFromRaw |> Str.joinWith ", "
        problemStr = "\(problem)\nWhile parsing record `\(recordStr)`."
        Break (Err (ParsingFailure problemStr))
      Err (ParsingIncomplete problem) ->
        Break (Err (ParsingIncomplete problem))
      Ok val ->
        state
        |> Result.map (\vals -> List.append vals val)
        |> Continue

# Wrapper function to combine a set of fields into your desired `a`
#
# ## Usage example
#
# >>> record (\firstName -> \lastName -> \age -> User {firstName, lastName, age})
# >>> |> field string
# >>> |> field string
# >>> |> field nat
#
record : a -> Parser CSVRecord a
record = Parser.Core.const

field : Parser RawStr a -> Parser CSVRecord a
field = \fieldParser ->
  buildPrimitiveParser \fieldsList ->
    when List.get fieldsList 0 is
      Err OutOfBounds ->
        Err (ParsingFailure "expected another CSV field but there are no more fields in this record")
      Ok rawStr ->
        when Parser.Str.parseRawStr fieldParser rawStr is
          Ok val ->
            Ok {val: val, input: (List.dropFirst fieldsList)}
          Err (ParsingFailure reason) ->
            fieldStr = rawStr |> strFromRaw
            Err (ParsingFailure "Field `\(fieldStr)` from could not be parsed. \(reason)")
          Err (ParsingIncomplete reason) ->
            reasonStr = strFromRaw reason
            fieldsStr = fieldsList |> List.map strFromRaw |> Str.joinWith ", "
            Err (ParsingFailure "The field parser was unable to read the whole field: `\(reasonStr)` while parsing the first field of leftover \(fieldsStr))")

# Parser for a field containing a UTF8-encoded string
string : Parser CSVField Str
string = Parser.Str.anyString

nat : Parser CSVField Nat
nat =
  string
  |> map (\val ->
    when Str.toNat val is
      Ok num ->
        Ok num
      Err _ ->
        Err "The field is not a valid Nat: \(val)"
        )
  |> flatten

f64 : Parser CSVField F64
f64 =
  string
  |> map (\val ->
    when Str.toF64 val is
      Ok num ->
        Ok num
      Err _ ->
        Err "The field is not a valid F64: \(val)"
        )
  |> flatten

parseStrToCSV : Str -> Result CSV [ParsingFailure Str, ParsingIncomplete RawStr]
parseStrToCSV = \input ->
  parse file (Str.toUtf8 input) (\leftover -> leftover == [])

parseStrToCSVRecord : Str -> Result CSVRecord [ParsingFailure Str, ParsingIncomplete RawStr]
parseStrToCSVRecord = \input ->
  parse csvRecord (Str.toUtf8 input) (\leftover -> leftover == [])


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
csvField = alt escapedCsvField nonescapedCsvField

escapedCsvField : Parser RawStr CSVField
escapedCsvField = between escapedContents dquote dquote
escapedContents = many (oneOf [
  twodquotes |> map (\_ -> 34), # An escaped double quote
  comma,
  cr,
  lf,
  textdata
])

twodquotes = Parser.Str.string "\"\""

nonescapedCsvField : Parser RawStr CSVField
nonescapedCsvField = many textdata
comma = codepoint 44 # ','
dquote = codepoint 34 # '"'
endOfLine = alt (ignore crlf) (ignore lf)
cr = codepoint 13 # '\r'
lf = codepoint 10 # '\n'
crlf = Parser.Str.string "\r\n"
textdata = codepointSatisfies (\x -> (x >= 32 && x <= 33) || (x >= 35 && x <= 43) || (x >= 45 && x <= 126)) # Any printable char except " (34) and , (44)
