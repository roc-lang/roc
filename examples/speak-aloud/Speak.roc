app "speak-aloud"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

greeting =
    hi = "Hello"
    catchPhrase = "Roc 'n Roll"

    "\(hi), lets \(catchPhrase)!"

main = greeting
