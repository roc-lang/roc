app [process_string] { pf: platform "./platform/main.roc" }

# A generic procedure bound to a requirement the platform reaches through its
# own alias: the alias's lookup records no instantiation of this procedure.
process_string = |value| "Got the following from the host: ${value.trim()}"
