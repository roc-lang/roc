app "speak-aloud"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

main =
    greeting = "Hello"
    catchPhrase = "Roc 'n Roll"

    "\(greeting), let's \(catchPhrase)!"
