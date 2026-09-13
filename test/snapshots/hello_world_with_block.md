# META
~~~ini
description=Hello world with a block
type=file
~~~
# SOURCE
~~~roc
# Hello world!

# Multiline comments?
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

main! = |_| {
	world = "World"
	# Hello
	Stdout.line!("Hello, world!")
}
~~~
# EXPECTED
NAME NOT IN SCOPE - hello_world_with_block.md:11:2:11:14
UNUSED VARIABLE - hello_world_with_block.md:9:2:9:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 11 2) (end 11 14))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "line!")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "hello_world_with_block.md") (start 11 2) (end 11 14) (annotation error) (line-text "\tStdout.line!(\"Hello, world!\")"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 9 2) (end 9 7))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "world")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_world")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "hello_world_with_block.md") (start 9 2) (end 9 7) (annotation error) (line-text "\tworld = \"World\"")))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
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
							(p-ident (raw "world"))
							(e-string
								(e-string-part (raw "World"))))
						(e-apply
							(e-ident (raw "Stdout.line!"))
							(e-string
								(e-string-part (raw "Hello, world!"))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
