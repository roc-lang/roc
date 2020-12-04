interface Storage
    exposes [
        Storage,
        decoder,
        get,
        listener,
        set
    ]
    imports [
        Map.{ Map },
        Json.Decode.{ Decoder } as Decode
        Json.Encode as Encode
        Ports.FromJs as FromJs
        Ports.ToJs as ToJs
    ]


################################################################################
## TYPES ##
################################################################################


Storage : [
    @Storage (Map Str Decode.Value)
]


################################################################################
## API ##
################################################################################


get : Storage, Str, Decoder a -> [ Ok a, NotInStorage, DecodeError Decode.Error ]*
get = \key, decoder, @Storage map ->
    when Map.get map key is
        Ok json ->
            Decode.decodeValue decoder json

        Err NotFound ->
            NotInStorage


set : Encode.Value, Str -> Effect {}
set json str =
    ToJs.type "setStorage"
        |> ToJs.setFields [
            Field "key" (Encode.str str),
            Field "value" json
        ]
        |> ToJs.send


decoder : Decoder Storage
decoder =
    Decode.mapType Decode.value
        |> Decode.map \map -> @Storage map


################################################################################
## PORTS INCOMING ##
################################################################################


listener : (Storage -> msg) -> FromJs.Listener msg
listener toMsg =
    FromJs.listen "storageUpdated"
        (Decode.map decoder toMsg)

