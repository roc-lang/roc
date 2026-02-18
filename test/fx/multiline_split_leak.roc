app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

input =
    \\This is a longer line number one
    \\This is a longer line number two
	\\L68
	\\L30
	\\R48
	\\L5
	\\R60
	\\L55
	\\L1
	\\The last line is here

main! = || {
    for line in input.split_on("\n")
    Stdout.line!(line)
}
