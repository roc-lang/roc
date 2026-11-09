app [process_string] { pf: platform "./platform/main.roc" }

# Without an annotation the alias stays generic, so its scope holds a requirement
# that only the app's root evidence fills, at the type the platform relates.
via = |value| "Got the following from the host: ${value.trim()}"

again = via

process_string = again
