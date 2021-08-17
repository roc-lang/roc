interface Console exposes [green] imports [ Utility ]

greenColor : Str
greenColor = Str.fromUtf8 [27, 91, 51, 50, 109]

clearColor : Str
clearColor = Str.fromUtf8 [27, 91, 51, 57, 109]

green : Str -> Str
green = \str ->
    Utility.concatStrList [ greenColor, str, clearColor ]
