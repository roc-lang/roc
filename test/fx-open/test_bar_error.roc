app [main!] { pf: platform "./platform/main.roc" }

# Bar sorts before Exit alphabetically. The platform must not read Bar's
# discriminant as Exit's discriminant when matching [Exit(I32), ..].
main! = |_args| Err(Bar)
