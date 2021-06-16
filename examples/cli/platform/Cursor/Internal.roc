interface Cursor.Internal
    exposes [
        Cursor,
        Raw,
        fromNum,
        toNum,
    ]
    imports []

## NOTE: internally, a Cursor is a file descriptor on
## UNIX systems and a HFILE on Windows.
Raw : U32

Cursor : [ @Cursor Raw ]

Cursor a : [ @Cursor Raw a  ]

fromRaw : Raw, a -> Cursor a
fromRaw = \raw, a -> @Cursor raw a

toRaw : Cursor * -> Raw
toRaw = \@Cursor raw _ -> raw
