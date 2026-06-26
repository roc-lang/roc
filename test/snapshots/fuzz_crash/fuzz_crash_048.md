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

ASCII CONTROL CHARACTER

ASCII control characters are not allowed in Roc source code.



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Thing` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  bar : Thing(a, b, _)                                                      │
 │        ‾‾‾‾‾                                                               │
 └───────────────────────────────────────────────────── fuzz_crash_048.md:2:7 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `String` is not declared in this scope. ────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  main! : List(String) -> Try({}, _)                                        │
 │               ‾‾‾‾‾‾                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_048.md:6:14 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Value` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  tag_tuple : Value((a, b, c))                                              │
 │              ‾‾‾‾‾                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_048.md:7:13 ┘



┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  foo : U64                                                                 │
 │  ‾‾‾‾‾‾‾‾‾                                                                 │
 └───────────────────────────────────────────────────── fuzz_crash_048.md:1:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  bar : Thing(a, b, _)                                                      │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_048.md:2:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  biz : (a, b, c)                                                           │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                           │
 └───────────────────────────────────────────────────── fuzz_crash_048.md:3:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  add_one : (                                                              │
 │  U8, U16 -> U32)                                                           │
 │                                                                            │
 └───────────────────────────────────────────────────── fuzz_crash_048.md:4:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  main! : List(String) -> Try({}, _)                                        │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                        │
 └───────────────────────────────────────────────────── fuzz_crash_048.md:6:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  tag_tuple : Value((a, b, c))                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                              │
 └───────────────────────────────────────────────────── fuzz_crash_048.md:7:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

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
	(type-module)
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
		(e-anno-only)
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
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-malformed))
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-underscore)))))
	(d-let
		(p-assign (ident "tag_tuple"))
		(e-anno-only)
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
