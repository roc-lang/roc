# The platform requires a wider error row than the app's annotation lists, which
# the platform may use as a caller does.
app [main!] { pf: platform "platform/main.roc" }

main! : List(Str) => Try({}, [Exit(I32)])
main! = |_args| Ok({})
