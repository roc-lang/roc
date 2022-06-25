package roc/unicode 0.1.0
    roc 0.0.0
    exposes [ Unicode, Unicode.Scalar, Unicode.CodePt ]
    packages {}
    license UPL-1.0

# TODO should we hande Latin1 encoding? Other encodings? Should there be
# an Ascii module, or a separate roc/ascii package? Conside that ASCII is
# 7-bit, so not all U8s are valid ASCII! There's also Extended ASCII to consider.
