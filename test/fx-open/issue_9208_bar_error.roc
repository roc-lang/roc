app [main!] { pf: platform "./platform/main.roc" }

# issue 9208: Bar sorts before Exit, so the platform must take the wildcard error branch.
main! = |_args| Err(Bar)
