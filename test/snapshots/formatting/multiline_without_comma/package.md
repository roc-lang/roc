# META
~~~ini
description=Multiline without comma formatting package
type=file
~~~
# SOURCE
~~~roc
package
	[
		a!,
		b!
	]
	{
		a: "a",
		b: "b"
	}

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package.md:3:3:3:5
EXPOSED BUT NOT DEFINED - package.md:4:3:4:5
DECLARATION HAS NO VALUE - package.md:11:1:11:16
DECLARATION HAS NO VALUE - package.md:12:1:12:16
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 3 3) (end 3 5))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "a!")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "package.md") (start 3 3) (end 3 5) (annotation error) (line-text "\t\ta!,"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "a!")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 4 3) (end 4 5))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "b!")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "package.md") (start 4 3) (end 4 5) (annotation error) (line-text "\t\tb!"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "b!")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 11 1) (end 11 16))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "package.md") (start 11 1) (end 11 16) (annotation error) (line-text "a! : Str => Str"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 12 1) (end 12 16))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "package.md") (start 12 1) (end 12 16) (annotation error) (line-text "b! : Str => Str"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
KwPackage,
OpenSquare,
LowerIdent,Comma,
LowerIdent,
CloseSquare,
OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes
			(exposed-lower-ident
				(text "a!"))
			(exposed-lower-ident
				(text "b!")))
		(packages
			(record-field (name "a")
				(e-string
					(e-string-part (raw "a"))))
			(record-field (name "b")
				(e-string
					(e-string-part (raw "b"))))))
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
package [a!, b!] { a: "a", b: "b" }

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
