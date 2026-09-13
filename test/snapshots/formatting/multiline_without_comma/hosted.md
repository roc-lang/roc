# META
~~~ini
description=Multiline without comma formatting hosted
type=file
~~~
# SOURCE
~~~roc
hosted [
	a!,
	b!
]

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - hosted.md:2:2:2:4
EXPOSED BUT NOT DEFINED - hosted.md:3:2:3:4
DECLARATION HAS NO VALUE - hosted.md:6:1:6:16
DECLARATION HAS NO VALUE - hosted.md:7:1:7:16
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 2 2) (end 2 4))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "a!")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "hosted.md") (start 2 2) (end 2 4) (annotation error) (line-text "\ta!,"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "a!")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 3 2) (end 3 4))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "b!")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "hosted.md") (start 3 2) (end 3 4) (annotation error) (line-text "\tb!"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "b!")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 6 1) (end 6 16))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "hosted.md") (start 6 1) (end 6 16) (annotation error) (line-text "a! : Str => Str"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 7 1) (end 7 16))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "hosted.md") (start 7 1) (end 7 16) (annotation error) (line-text "b! : Str => Str"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
KwHosted,OpenSquare,
LowerIdent,Comma,
LowerIdent,
CloseSquare,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(hosted
		(exposes
			(exposed-lower-ident
				(text "a!"))
			(exposed-lower-ident
				(text "b!"))))
	(statements
		(s-type-anno (name "a!")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-type-anno (name "b!")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))))
~~~
# FORMATTED
~~~roc
hosted [a!, b!]

a! : Str => Str

b! : Str => Str
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a!"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "b!"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => Str"))
		(patt (type "Str => Str")))
	(expressions
		(expr (type "Str => Str"))
		(expr (type "Str => Str"))))
~~~
