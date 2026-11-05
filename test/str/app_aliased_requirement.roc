app [process_string] { pf: platform "./platform/main.roc" }

# A platform requirement the app binds to an alias: the platform's use records
# no instantiation, so the app's root evidence fills the alias's scope.
via = |value| "Got the following from the host: ${value.trim()}"

again = via

process_string : Str -> Str
process_string = again
