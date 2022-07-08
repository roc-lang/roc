interface ParserJson
  exposes [
  ]
    imports [
  ParserCore.{Parser, fail, const, map, map2, apply, many, oneOrMore, sepBy, sepBy1, between},
  ParserStr.{RawStr, runPartialStr, runStr, oneOf, string, scalar}
]

JsonValue := [
  JsonNull,
  JsonBool Bool,
  JsonNum F64,
  JsonStr Str,
  JsonArray (List JsonValue),
  JsonDict (List [Pair Str JsonValue]),
]


jsonBool : Parser RawStr JsonValue
jsonBool =
    oneOf [string "true", string "false"]
    |> map \val ->
        if val == "true" then
            @JsonValue (JsonBool True)
        else
            @JsonValue (JsonBool False)

jsonNull : Parser RawStr JsonValue
jsonNull =
    string "null"
    |> map \_val ->
      @JsonValue JsonNull
