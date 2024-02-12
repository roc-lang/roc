interface UnusedImportButWithALongFileNameForTesting
    exposes [plainText, emText]
    imports [Symbol.{ Ident }]

plainText = \str -> PlainText str

emText = \str -> EmText str
