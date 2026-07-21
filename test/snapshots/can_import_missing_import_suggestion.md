# META
~~~ini
description=Using a qualified ident that was never imported suggests an import with the header's package shorthand
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

main! = |_| {
    input = Stdin.line!()
    Stdout.line!(input)
}
~~~
# EXPECTED
DOES NOT EXIST - can_import_missing_import_suggestion.md:6:13:6:24
DOES NOT EXIST - can_import_missing_import_suggestion.md:7:12:7:17
# PROBLEMS

┌────────────────┐
│ DOES NOT EXIST ├─ `Stdin.line!` does not exist. ────────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  input = Stdin.line!()                                                     │
 │          ‾‾‾‾‾‾‾‾‾‾‾                                                       │
 └────────────────────────────── can_import_missing_import_suggestion.md:6:13 ┘

    The name `Stdin` is not an imported mod or a type in scope.


    If `Stdin` is a mod, you may need to import it at the top of the file.
    For example:

        import pf.Stdin


┌────────────────┐
│ DOES NOT EXIST ├─ `line!` was not found in `Stdout`. ───────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  Stdout.line!(input)                                                       │
 │         ‾‾‾‾‾                                                              │
 └────────────────────────────── can_import_missing_import_suggestion.md:7:12 ┘

    Check that `line!` is spelled correctly and that `Stdout` exposes it.

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-import (raw "pf.Stdout"))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "input"))
							(e-apply
								(e-ident (raw "Stdin.line!"))))
						(e-apply
							(e-ident (raw "Stdout.line!"))
							(e-ident (raw "input")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

main! = |_| {
	input = Stdin.line!()
	Stdout.line!(input)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main!"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-import (mod "pf.Stdout")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> Error")))
	(expressions
		(expr (type "_arg -> Error"))))
~~~
