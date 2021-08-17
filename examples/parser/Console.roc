interface Console exposes [black, green, red, yellow, blue, magenta, cyan, white,
  bgBlack, bgGreen, bgRed, bgYellow, bgBlue, bgMagenta, bgCyan, bgWhite] imports [ StrExtra ]

clearColor : Str
clearColor = makeColor 57

makeColor : U8 -> Str
makeColor = \u -> Str.fromUtf8 [27, 91, 51, u, 109] |> Result.withDefault ""

makeBGColor : U8 -> Str
makeBGColor = \u -> Str.fromUtf8 [27, 91, 52, u, 109] |> Result.withDefault ""

clearBGColor : Str
clearBGColor = makeBGColor 57

fg : U8, Str -> Str
fg = \color, str -> StrExtra.concat [ makeColor color, str, clearColor ]

bg : U8, Str -> Str
bg = \color, str -> StrExtra.concat [ makeBGColor color, str, clearBGColor ]


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

###

bgBlack : Str -> Str
bgBlack = \str -> bg 58 str

bgRed : Str -> Str
bgRed = \str -> bg 49 str

bgGreen : Str -> Str
bgGreen = \str -> bg 50 str

bgYellow : Str -> Str
bgYellow = \str -> bg 51 str

bgBlue : Str -> Str
bgBlue = \str -> bg 52 str

bgMagenta : Str -> Str
bgMagenta = \str -> bg 53 str

bgCyan : Str -> Str
bgCyan = \str -> bg 54 str

bgWhite : Str -> Str
bgWhite = \str -> bg 55 str