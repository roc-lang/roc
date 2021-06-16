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

fromRaw : Raw -> Cursor
fromRaw = \raw -> @Cursor raw

toRaw : Cursor -> Raw
toRaw = \@Cursor raw -> raw
