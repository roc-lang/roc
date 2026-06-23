# META
~~~ini
description=Various type annotations
type=snippet
~~~
# SOURCE
~~~roc
foo : U64

bar : Thing(_a, _b, _)

baz : (_a, _b, _c)

add_one : (U8, U16 -> U32)

main! : List(String) -> Try({}, _)

tag_tuple : Value((_a, _b, _c))

closed_record_with_comma : {
	a : U8,
}

open_record_with_comma : {
	a : U8,
	..,
}
~~~
# EXPECTED
UNDECLARED TYPE - type_annotations.md:3:7:3:12
UNDECLARED TYPE - type_annotations.md:9:14:9:20
UNDECLARED TYPE - type_annotations.md:11:13:11:18
DECLARATION HAS NO VALUE - type_annotations.md:1:1:1:10
DECLARATION HAS NO VALUE - type_annotations.md:3:1:3:23
DECLARATION HAS NO VALUE - type_annotations.md:5:1:5:19
DECLARATION HAS NO VALUE - type_annotations.md:7:1:7:27
DECLARATION HAS NO VALUE - type_annotations.md:9:1:9:35
DECLARATION HAS NO VALUE - type_annotations.md:11:1:11:32
DECLARATION HAS NO VALUE - type_annotations.md:13:1:15:2
DECLARATION HAS NO VALUE - type_annotations.md:17:1:20:2
# PROBLEMS

┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Thing` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  bar : Thing(_a, _b, _)                                                    │
 │        ‾‾‾‾‾                                                               │
 └─────────────────────────────────────────────────── type_annotations.md:3:7 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `String` is not declared in this scope. ────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  main! : List(String) -> Try({}, _)                                        │
 │               ‾‾‾‾‾‾                                                       │
 └────────────────────────────────────────────────── type_annotations.md:9:14 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Value` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  tag_tuple : Value((_a, _b, _c))                                           │
 │              ‾‾‾‾‾                                                         │
 └───────────────────────────────────────────────── type_annotations.md:11:13 ┘



┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  foo : U64                                                                 │
 │  ‾‾‾‾‾‾‾‾‾                                                                 │
 └─────────────────────────────────────────────────── type_annotations.md:1:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  bar : Thing(_a, _b, _)                                                    │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                    │
 └─────────────────────────────────────────────────── type_annotations.md:3:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  baz : (_a, _b, _c)                                                        │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └─────────────────────────────────────────────────── type_annotations.md:5:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  add_one : (U8, U16 -> U32)                                                │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                │
 └─────────────────────────────────────────────────── type_annotations.md:7:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  main! : List(String) -> Try({}, _)                                        │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                        │
 └─────────────────────────────────────────────────── type_annotations.md:9:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  tag_tuple : Value((_a, _b, _c))                                           │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                           │
 └────────────────────────────────────────────────── type_annotations.md:11:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  closed_record_with_comma : {                                              │
 │      a : U8,                                                               │
 │  }                                                                         │
 │                                                                            │
 └────────────────────────────────────────────────── type_annotations.md:13:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  open_record_with_comma : {                                                │
 │      a : U8,                                                               │
 │      ..,                                                                   │
 │  }                                                                         │
 │                                                                            │
 └────────────────────────────────────────────────── type_annotations.md:17:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NamedUnderscore,Comma,NamedUnderscore,Comma,Underscore,CloseRound,
LowerIdent,OpColon,OpenRound,NamedUnderscore,Comma,NamedUnderscore,Comma,NamedUnderscore,CloseRound,
LowerIdent,OpColon,OpenRound,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,Underscore,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,NamedUnderscore,Comma,NamedUnderscore,Comma,NamedUnderscore,CloseRound,CloseRound,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
CloseCurly,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
DoubleDot,Comma,
CloseCurly,
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
				(underscore-ty-var (raw "_a"))
				(underscore-ty-var (raw "_b"))
				(_)))
		(s-type-anno (name "baz")
			(ty-tuple
				(underscore-ty-var (raw "_a"))
				(underscore-ty-var (raw "_b"))
				(underscore-ty-var (raw "_c"))))
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
					(underscore-ty-var (raw "_a"))
					(underscore-ty-var (raw "_b"))
					(underscore-ty-var (raw "_c")))))
		(s-type-anno (name "closed_record_with_comma")
			(ty-record
				(anno-record-field (name "a")
					(ty (name "U8")))))
		(s-type-anno (name "open_record_with_comma")
			(ty-record
				(anno-record-field (name "a")
					(ty (name "U8")))
				(ty-record-ext
					..)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
		(p-assign (ident "baz"))
		(e-anno-only)
		(annotation
			(ty-tuple
				(ty-rigid-var (name "_a"))
				(ty-rigid-var (name "_b"))
				(ty-rigid-var (name "_c")))))
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
			(ty-malformed)))
	(d-let
		(p-assign (ident "closed_record_with_comma"))
		(e-anno-only)
		(annotation
			(ty-record
				(field (field "a")
					(ty-lookup (name "U8") (builtin))))))
	(d-let
		(p-assign (ident "open_record_with_comma"))
		(e-anno-only)
		(annotation
			(ty-record
				(field (field "a")
					(ty-lookup (name "U8") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64"))
		(patt (type "Error"))
		(patt (type "(_a, _b, _c)"))
		(patt (type "U8, U16 -> U32"))
		(patt (type "List(Error) -> Try({}, _b)"))
		(patt (type "Error"))
		(patt (type "{ a: U8 }"))
		(patt (type "{ a: U8, .. }")))
	(expressions
		(expr (type "U64"))
		(expr (type "Error"))
		(expr (type "(_a, _b, _c)"))
		(expr (type "U8, U16 -> U32"))
		(expr (type "List(Error) -> Try({}, _b)"))
		(expr (type "Error"))
		(expr (type "{ a: U8 }"))
		(expr (type "{ a: U8, .. }"))))
~~~
