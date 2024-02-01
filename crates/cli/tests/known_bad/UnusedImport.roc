interface UnusedImport
    exposes [plainText, emText]

import Symbol exposing [Ident]

plainText = \str -> PlainText str

emText = \str -> EmText str
