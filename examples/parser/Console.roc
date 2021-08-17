interface Console exposes [black, green, red, yellow, blue, magenta, cyan, white] imports [ StrExtra ]

clearColor : Str
clearColor = makeColor 57

makeColor : U8 -> Str
makeColor = \u -> Str.fromUtf8 [27, 91, 51, u, 109] |> Result.withDefault ""

fg : U8, Str -> Str
fg = \color, str -> StrExtra.concat [ makeColor color, str, clearColor ]

black : Str -> Str
black = \str -> fg 48 str

red : Str -> Str
red = \str -> fg 49 str

green : Str -> Str
green = \str -> fg 50 str

yellow : Str -> Str
yellow = \str -> fg 51 str

blue : Str -> Str
blue = \str -> fg 52 str

magenta : Str -> Str
magenta = \str -> fg 53 str

cyan : Str -> Str
cyan = \str -> fg 54 str

white : Str -> Str
white = \str -> fg 55 str