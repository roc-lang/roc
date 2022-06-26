interface Locale
    exposes [Locale, CharsetErr, toStr]
    imports [Path]

Locale := Str
# Note: Windows has "locale names" and "locale identifers" and says "the use of locale names
# is always preferable" on Windows Vista and later.
# https://docs.microsoft.com/en-us/windows/win32/intl/calling-the--locale-name--functions

## Interprets some bytes to be a string encoded using the given locale's character set
## ("charset").
##
## If the bytes are invalid for that charset, returns `Err`.
toStr : Locale, List U8 -> Result Str (CharsetErr *)

## Like [toStr], except it converts charset errors into the [Unicode replacement character](https://unicode.org/glossary/#replacement_character)
## (`"�"`).
display : Locale, List U8 -> Str

CharsetErr others : [
    CharsetErr {
        bytes : List U8,
        problem : [ ... ],
    }
]others
