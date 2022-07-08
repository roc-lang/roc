interface ParserJson
  exposes [
  ]
    imports [
  ParserCore.{Parser, fail, const, map, map2, apply, many, oneOrMore, sepBy, sepBy1, between, ignore},
  ParserStr.{RawStr, runPartialStr, runStr, oneOf, string, scalar, digits}
]

JsonValue := [
  JsonNull,
  JsonBool Bool,
  JsonNum F64,
  JsonStr Str,
  JsonArray (List JsonValue),
  JsonDict (List [Pair Str JsonValue]),
]

jsonNull : Parser RawStr JsonValue
jsonNull =
    string "null"
    |> map \_val ->
      @JsonValue JsonNull

jsonBool : Parser RawStr JsonValue
jsonBool =
    oneOf [string "true", string "false"]
    |> map \val ->
        if val == "true" then
            @JsonValue (JsonBool True)
        else
            @JsonValue (JsonBool False)

# TODO: negative numbers
# TODO: floats and exponent notation
# TODO: Dealing with ints larger than 2^53 would be a nice extension.
jsonNum : Parser RawStr JsonValue
jsonNum = digits |> map \intVal ->
  @JsonValue (JsonNum (Num.toFrac intVal))


jsonWSChar =
  oneOf [
    scalar ' ',
    scalar '\t',
    scalar '\n',
  ]

jsonWS = ignore (many jsonWSChar)

jsonComma = map2 (scalar ',') jsonWS (\_, _ -> {})

jsonNumArray : Parser RawStr (List JsonValue)
jsonNumArray =
    jsonNum
    |> sepBy (scalar ',')
    |> between (scalar '[') (scalar ']')

jsonNumArray2 : Parser RawStr [JsonArray (List JsonValue)]
jsonNumArray2 =
    jsonNumArray
    |> map(\res -> (JsonArray res))

jsonNumArray3 : Parser RawStr JsonValue
jsonNumArray3 =
    jsonNumArray2
    |> map (\res -> @JsonValue res)
