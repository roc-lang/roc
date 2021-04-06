interface Test
    exposes [ singleline, multiline, multiparagraph, codeblock ]
    imports []

## This is a block
Block elem : [ Block elem ]

## Single line documentation.
singleline : Bool -> Bool

## Multiline documentation.
## Without any complex syntax yet!
multiline : Bool -> Bool

## Multiparagraph documentation.
##
## Without any complex syntax yet!
multiparagraph : Bool -> Bool

## No documentation for not exposed function.
notExposed : Bool -> Bool

## Turns >>> into code block for now.
##
## >>> codeblock
codeblock : Bool -> Bool
