api Str provides Str, isEmpty, join

## Types

Str := Str

## Convert

## Convert a #Float to a decimal string, rounding off to the given number of decimal places.
##
## Since #Float values are imprecise, it's usually best to limit this to the lowest
## number you can choose that will make sense for what you want to display.
##
## If you want to kep all the digits, passing #Int.highestSupported will accomplish this,
## but it's recommended to pass much smaller numbers instead.
##
## Passing a negative number for decimal places is equivalent to passing 0.
decimal : Int, Float -> Str

## Convert an #Int to a string.
int : Float -> Str

## Check

isEmpty : Str -> Bool

startsWith : Str, Str -> Bool

endsWith : Str, Str -> Bool

contains : Str, Str -> Bool

any : Str, Str -> Bool

all : Str, Str -> Bool

## Combine

## Combine a list of strings into a single string.
##
## > Str.join [ "a", "bc", "def" ]
join : List Str -> Str

## Combine a list of strings into a single string, with a separator
## string in between each.
##
## > Str.joinWith [ "one", "two", "three" ] ", "
joinWith : List Str, Str -> Str


padStart : Str, Int, Str -> Str

padEnd : Str, Int, Str -> Str


