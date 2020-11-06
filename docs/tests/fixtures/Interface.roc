interface Test
    exposes [ singleline, multiline ]
    imports []

## Single line documentation.
singleline : Bool -> Bool

## Multiline documentation.
## Without any complex syntax yet!
multiline : Bool -> Bool

## No documentation for not exposed function.
notExposed : Bool -> Bool
