# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
foo : U64
bar : Thing(a, b, _)
biz : (a, b, c)
add_one : (
U8, U16 -> U32)
main! : List(String) -> Try({}, _)
tag_tuple : Value((a, b, c))
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
UNDECLARED TYPE - fuzz_crash_048.md:2:7:2:12
UNDECLARED TYPE - fuzz_crash_048.md:6:14:6:20
UNDECLARED TYPE - fuzz_crash_048.md:7:13:7:18
DECLARATION HAS NO VALUE - fuzz_crash_048.md:1:1:1:10
DECLARATION HAS NO VALUE - fuzz_crash_048.md:2:1:2:21
DECLARATION HAS NO VALUE - fuzz_crash_048.md:3:1:3:16
DECLARATION HAS NO VALUE - fuzz_crash_048.md:4:1:5:16
DECLARATION HAS NO VALUE - fuzz_crash_048.md:6:1:6:35
DECLARATION HAS NO VALUE - fuzz_crash_048.md:7:1:7:29
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "ASCII Control Character")
		(headline
			(reflow "ASCII control characters are not allowed in Roc source code."))
		(document))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 2 7) (end 2 12))
		(headline
			(reflow "The type ")
			(annotated code "Thing")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_048.md") (start 2 7) (end 2 12) (annotation error) (line-text "bar : Thing(a, b, _)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 6 14) (end 6 20))
		(headline
			(reflow "The type ")
			(annotated code "String")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_048.md") (start 6 14) (end 6 20) (annotation error) (line-text "main! : List(String) -> Try({}, _)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 7 13) (end 7 18))
		(headline
			(reflow "The type ")
			(annotated code "Value")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_048.md") (start 7 13) (end 7 18) (annotation error) (line-text "tag_tuple : Value((a, b, c))"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 1 1) (end 1 10))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_048.md") (start 1 1) (end 1 10) (annotation error) (line-text "foo : U64"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 2 1) (end 2 21))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_048.md") (start 2 1) (end 2 21) (annotation error) (line-text "bar : Thing(a, b, _)"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 3 1) (end 3 16))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_048.md") (start 3 1) (end 3 16) (annotation error) (line-text "biz : (a, b, c)"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 4 1) (end 5 16))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_048.md") (start 4 1) (end 5 16) (annotation error) (line-text "add_one : (\u{11}\nU8, U16 -> U32)"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 6 1) (end 6 35))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_048.md") (start 6 1) (end 6 35) (annotation error) (line-text "main! : List(String) -> Try({}, _)"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 7 1) (end 7 29))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_048.md") (start 7 1) (end 7 29) (annotation error) (line-text "tag_tuple : Value((a, b, c))"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,Underscore,CloseRound,
LowerIdent,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,OpenRound,
UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,Underscore,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "foo")
			(ty (name "U64")))
		(s-type-anno (name "bar")
			(ty-apply
				(ty (name "Thing"))
				(ty-var (raw "a"))
				(ty-var (raw "b"))
				(_)))
		(s-type-anno (name "biz")
			(ty-tuple
				(ty-var (raw "a"))
				(ty-var (raw "b"))
				(ty-var (raw "c"))))
		(s-type-anno (name "add_one")
			(ty-fn
				(ty (name "U8"))
				(ty (name "U16"))
				(ty (name "U32"))))
		(s-type-anno (name "main!")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty (name "String")))
				(ty-apply
					(ty (name "Try"))
					(ty-record)
					(_))))
		(s-type-anno (name "tag_tuple")
			(ty-apply
				(ty (name "Value"))
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "b"))
					(ty-var (raw "c")))))))
~~~
# FORMATTED
~~~roc
foo : U64

bar : Thing(a, b, _)

biz : (a, b, c)

add_one : (
	U8, U16 -> U32)

main! : List(String) -> Try({}, _)

tag_tuple : Value((a, b, c))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-anno-only)
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-assign (ident "bar"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "biz"))
		(e-anno-only)
		(annotation
			(ty-tuple
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))
				(ty-rigid-var (name "c")))))
	(d-let
		(p-assign (ident "add_one"))
		(e-anno-only)
		(annotation
			(ty-parens
				(ty-fn (effectful false)
					(ty-lookup (name "U8") (builtin))
					(ty-lookup (name "U16") (builtin))
					(ty-lookup (name "U32") (builtin))))))
	(d-let
		(p-assign (ident "main!"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-malformed))
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-underscore)))))
	(d-let
		(p-assign (ident "tag_tuple"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64"))
		(patt (type "Error"))
		(patt (type "(a, b, c)"))
		(patt (type "U8, U16 -> U32"))
		(patt (type "List(Error) -> Try({}, _d)"))
		(patt (type "Error")))
	(expressions
		(expr (type "U64"))
		(expr (type "Error"))
		(expr (type "(a, b, c)"))
		(expr (type "U8, U16 -> U32"))
		(expr (type "List(Error) -> Try({}, _d)"))
		(expr (type "Error"))))
~~~
