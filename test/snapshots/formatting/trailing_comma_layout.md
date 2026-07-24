# META
~~~ini
description=Trailing commas explicitly control collection layout
type=snippet
~~~
# SOURCE
~~~roc
import Foo exposing [
	one,
	two
]

compact_list = [
	1,
	2
]

expanded_list = [1, 2,]

compact_call = make(
	1,
	2
)

expanded_call = make(1, 2,)

compact_record = {
	one: 1,
	two: 2
}

expanded_record = { one: 1, two: 2, }

nested_inner = { values: [1, 2,] }
nested_outer = { values: [1, 2], }

compact_lambda = |
	one,
	two
| one

expanded_lambda_ident = |one, two,| one
expanded_lambda_block = |one, two,| {}
expanded_lambda_list = |one, two,| [one]
expanded_lambda_tuple = |one, two,| (one, two)
expanded_lambda_nested = |one, two,| |value| value

Pair(one, two) : (
	one,
	two
)

ExpandedPair(one, two,) : (one, two,)

RecordType : {
	one : Str,
	two : Str
}

ExpandedRecordType : { one : Str, two : Str, }

pattern = |value| {
	match value {
		Pair(
			one,
			two
		) => one
		ExpandedPair(one, two,) => two
	}
}

comment_forces = [
	1,
	# keep this comment attached
	2
]
~~~
# EXPECTED
NAME NOT IN SCOPE - trailing_comma_layout.md:13:16:13:20
NAME NOT IN SCOPE - trailing_comma_layout.md:18:17:18:21
UNUSED VARIABLE - trailing_comma_layout.md:32:2:32:5
UNUSED VARIABLE - trailing_comma_layout.md:35:31:35:34
UNUSED VARIABLE - trailing_comma_layout.md:36:26:36:29
UNUSED VARIABLE - trailing_comma_layout.md:36:31:36:34
UNUSED VARIABLE - trailing_comma_layout.md:37:30:37:33
UNUSED VARIABLE - trailing_comma_layout.md:39:27:39:30
UNUSED VARIABLE - trailing_comma_layout.md:39:32:39:35
UNUSED VARIABLE - trailing_comma_layout.md:59:4:59:7
UNUSED VARIABLE - trailing_comma_layout.md:61:16:61:19
# PROBLEMS

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `make` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  compact_call = make(                                                      │
 │                 ‾‾‾‾                                                       │
 └──────────────────────────────────────────── trailing_comma_layout.md:13:16 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `make` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  expanded_call = make(1, 2,)                                               │
 │                  ‾‾‾‾                                                      │
 └──────────────────────────────────────────── trailing_comma_layout.md:18:17 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `two` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  two                                                                       │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────── trailing_comma_layout.md:32:2 ┘

    If you don't need this variable, prefix it with an underscore like `_two`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `two` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  expanded_lambda_ident = |one, two,| one                                   │
 │                                ‾‾‾                                         │
 └──────────────────────────────────────────── trailing_comma_layout.md:35:31 ┘

    If you don't need this variable, prefix it with an underscore like `_two`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `one` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  expanded_lambda_block = |one, two,| {}                                    │
 │                           ‾‾‾                                              │
 └──────────────────────────────────────────── trailing_comma_layout.md:36:26 ┘

    If you don't need this variable, prefix it with an underscore like `_one`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `two` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  expanded_lambda_block = |one, two,| {}                                    │
 │                                ‾‾‾                                         │
 └──────────────────────────────────────────── trailing_comma_layout.md:36:31 ┘

    If you don't need this variable, prefix it with an underscore like `_two`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `two` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  expanded_lambda_list = |one, two,| [one]                                  │
 │                               ‾‾‾                                          │
 └──────────────────────────────────────────── trailing_comma_layout.md:37:30 ┘

    If you don't need this variable, prefix it with an underscore like `_two`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `one` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  expanded_lambda_nested = |one, two,| |value| value                        │
 │                            ‾‾‾                                             │
 └──────────────────────────────────────────── trailing_comma_layout.md:39:27 ┘

    If you don't need this variable, prefix it with an underscore like `_one`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `two` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  expanded_lambda_nested = |one, two,| |value| value                        │
 │                                 ‾‾‾                                        │
 └──────────────────────────────────────────── trailing_comma_layout.md:39:32 ┘

    If you don't need this variable, prefix it with an underscore like `_two`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `two` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  two                                                                       │
 │  ‾‾‾                                                                       │
 └───────────────────────────────────────────── trailing_comma_layout.md:59:4 ┘

    If you don't need this variable, prefix it with an underscore like `_two`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `one` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  ExpandedPair(one, two,) => two                                            │
 │               ‾‾‾                                                          │
 └──────────────────────────────────────────── trailing_comma_layout.md:61:16 ┘

    If you don't need this variable, prefix it with an underscore like `_one`
    to suppress this warning.

# TOKENS
~~~zig
KwImport,UpperIdent,KwExposing,OpenSquare,
LowerIdent,Comma,
LowerIdent,
CloseSquare,
LowerIdent,OpAssign,OpenSquare,
Int,Comma,
Int,
CloseSquare,
LowerIdent,OpAssign,OpenSquare,Int,Comma,Int,Comma,CloseSquare,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,
Int,Comma,
Int,
CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,Comma,CloseRound,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,Comma,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,OpenSquare,Int,Comma,Int,Comma,CloseSquare,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,OpenSquare,Int,Comma,Int,CloseSquare,Comma,CloseCurly,
LowerIdent,OpAssign,OpBar,
LowerIdent,Comma,
LowerIdent,
OpBar,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,OpBar,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,OpBar,OpenCurly,CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,OpBar,OpenSquare,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,OpBar,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,CloseRound,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,Comma,CloseRound,
UpperIdent,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
LowerIdent,OpColon,UpperIdent,
CloseCurly,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpenSquare,
Int,Comma,
Int,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "Foo")
			(exposing
				(exposed-lower-ident
					(text "one"))
				(exposed-lower-ident
					(text "two"))))
		(s-decl
			(p-ident (raw "compact_list"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))))
		(s-decl
			(p-ident (raw "expanded_list"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))))
		(s-decl
			(p-ident (raw "compact_call"))
			(e-apply
				(e-ident (raw "make"))
				(e-int (raw "1"))
				(e-int (raw "2"))))
		(s-decl
			(p-ident (raw "expanded_call"))
			(e-apply
				(e-ident (raw "make"))
				(e-int (raw "1"))
				(e-int (raw "2"))))
		(s-decl
			(p-ident (raw "compact_record"))
			(e-record
				(field (field "one")
					(e-int (raw "1")))
				(field (field "two")
					(e-int (raw "2")))))
		(s-decl
			(p-ident (raw "expanded_record"))
			(e-record
				(field (field "one")
					(e-int (raw "1")))
				(field (field "two")
					(e-int (raw "2")))))
		(s-decl
			(p-ident (raw "nested_inner"))
			(e-record
				(field (field "values")
					(e-list
						(e-int (raw "1"))
						(e-int (raw "2"))))))
		(s-decl
			(p-ident (raw "nested_outer"))
			(e-record
				(field (field "values")
					(e-list
						(e-int (raw "1"))
						(e-int (raw "2"))))))
		(s-decl
			(p-ident (raw "compact_lambda"))
			(e-lambda
				(args
					(p-ident (raw "one"))
					(p-ident (raw "two")))
				(e-ident (raw "one"))))
		(s-decl
			(p-ident (raw "expanded_lambda_ident"))
			(e-lambda
				(args
					(p-ident (raw "one"))
					(p-ident (raw "two")))
				(e-ident (raw "one"))))
		(s-decl
			(p-ident (raw "expanded_lambda_block"))
			(e-lambda
				(args
					(p-ident (raw "one"))
					(p-ident (raw "two")))
				(e-record)))
		(s-decl
			(p-ident (raw "expanded_lambda_list"))
			(e-lambda
				(args
					(p-ident (raw "one"))
					(p-ident (raw "two")))
				(e-list
					(e-ident (raw "one")))))
		(s-decl
			(p-ident (raw "expanded_lambda_tuple"))
			(e-lambda
				(args
					(p-ident (raw "one"))
					(p-ident (raw "two")))
				(e-tuple
					(e-ident (raw "one"))
					(e-ident (raw "two")))))
		(s-decl
			(p-ident (raw "expanded_lambda_nested"))
			(e-lambda
				(args
					(p-ident (raw "one"))
					(p-ident (raw "two")))
				(e-lambda
					(args
						(p-ident (raw "value")))
					(e-ident (raw "value")))))
		(s-type-decl
			(header (name "Pair")
				(args
					(ty-var (raw "one"))
					(ty-var (raw "two"))))
			(ty-tuple
				(ty-var (raw "one"))
				(ty-var (raw "two"))))
		(s-type-decl
			(header (name "ExpandedPair")
				(args
					(ty-var (raw "one"))
					(ty-var (raw "two"))))
			(ty-tuple
				(ty-var (raw "one"))
				(ty-var (raw "two"))))
		(s-type-decl
			(header (name "RecordType")
				(args))
			(ty-record
				(anno-record-field (name "one")
					(ty (name "Str")))
				(anno-record-field (name "two")
					(ty (name "Str")))))
		(s-type-decl
			(header (name "ExpandedRecordType")
				(args))
			(ty-record
				(anno-record-field (name "one")
					(ty (name "Str")))
				(anno-record-field (name "two")
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "pattern"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-block
					(statements
						(e-match
							(e-ident (raw "value"))
							(branches
								(branch
									(p-tag (raw "Pair")
										(p-ident (raw "one"))
										(p-ident (raw "two")))
									(e-ident (raw "one")))
								(branch
									(p-tag (raw "ExpandedPair")
										(p-ident (raw "one"))
										(p-ident (raw "two")))
									(e-ident (raw "two")))))))))
		(s-decl
			(p-ident (raw "comment_forces"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
import Foo exposing [one, two]

compact_list = [1, 2]

expanded_list = [
	1,
	2,
]

compact_call = make(1, 2)

expanded_call = make(
	1,
	2,
)

compact_record = { one: 1, two: 2 }

expanded_record = {
	one: 1,
	two: 2,
}

nested_inner = {
	values: [
		1,
		2,
	],
}

nested_outer = {
	values: [1, 2],
}

compact_lambda = |one, two| one

expanded_lambda_ident = |
	one,
	two,
| one

expanded_lambda_block = |
	one,
	two,
| {}

expanded_lambda_list = |
	one,
	two,
| [one]

expanded_lambda_tuple = |
	one,
	two,
| (one, two)

expanded_lambda_nested = |
	one,
	two,
| |value| value

Pair(one, two) : (one, two)

ExpandedPair(
	one,
	two,
) : (
	one,
	two,
)

RecordType : { one : Str, two : Str }

ExpandedRecordType : {
	one : Str,
	two : Str,
}

pattern = |value| {
	match value {
		Pair(one, two) => one
		ExpandedPair(
			one,
			two,
		) => two
	}
}

comment_forces = [
	1,
	# keep this comment attached
	2,
]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "compact_list"))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2")))))
	(d-let
		(p-assign (ident "expanded_list"))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2")))))
	(d-let
		(p-assign (ident "compact_call"))
		(e-call
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-num (value "1"))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "expanded_call"))
		(e-call
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-num (value "1"))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "compact_record"))
		(e-record
			(fields
				(field (name "one")
					(e-num (value "1")))
				(field (name "two")
					(e-num (value "2"))))))
	(d-let
		(p-assign (ident "expanded_record"))
		(e-record
			(fields
				(field (name "one")
					(e-num (value "1")))
				(field (name "two")
					(e-num (value "2"))))))
	(d-let
		(p-assign (ident "nested_inner"))
		(e-record
			(fields
				(field (name "values")
					(e-list
						(elems
							(e-num (value "1"))
							(e-num (value "2"))))))))
	(d-let
		(p-assign (ident "nested_outer"))
		(e-record
			(fields
				(field (name "values")
					(e-list
						(elems
							(e-num (value "1"))
							(e-num (value "2"))))))))
	(d-let
		(p-assign (ident "compact_lambda"))
		(e-lambda
			(args
				(p-assign (ident "one"))
				(p-assign (ident "two")))
			(e-lookup-local
				(p-assign (ident "one")))))
	(d-let
		(p-assign (ident "expanded_lambda_ident"))
		(e-lambda
			(args
				(p-assign (ident "one"))
				(p-assign (ident "two")))
			(e-lookup-local
				(p-assign (ident "one")))))
	(d-let
		(p-assign (ident "expanded_lambda_block"))
		(e-lambda
			(args
				(p-assign (ident "one"))
				(p-assign (ident "two")))
			(e-empty_record)))
	(d-let
		(p-assign (ident "expanded_lambda_list"))
		(e-lambda
			(args
				(p-assign (ident "one"))
				(p-assign (ident "two")))
			(e-list
				(elems
					(e-lookup-local
						(p-assign (ident "one")))))))
	(d-let
		(p-assign (ident "expanded_lambda_tuple"))
		(e-lambda
			(args
				(p-assign (ident "one"))
				(p-assign (ident "two")))
			(e-tuple
				(elems
					(e-lookup-local
						(p-assign (ident "one")))
					(e-lookup-local
						(p-assign (ident "two")))))))
	(d-let
		(p-assign (ident "expanded_lambda_nested"))
		(e-lambda
			(args
				(p-assign (ident "one"))
				(p-assign (ident "two")))
			(e-lambda
				(args
					(p-assign (ident "value")))
				(e-lookup-local
					(p-assign (ident "value"))))))
	(d-let
		(p-assign (ident "pattern"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-block
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "value"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-lookup-local
										(p-assign (ident "one")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-lookup-local
										(p-assign (ident "two")))))))))))
	(d-let
		(p-assign (ident "comment_forces"))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2")))))
	(s-import (mod "Foo")
		(exposes
			(exposed (name "one") (wildcard false))
			(exposed (name "two") (wildcard false))))
	(s-alias-decl
		(ty-header (name "Pair")
			(ty-args
				(ty-rigid-var (name "one"))
				(ty-rigid-var (name "two"))))
		(ty-tuple
			(ty-rigid-var-lookup (ty-rigid-var (name "one")))
			(ty-rigid-var-lookup (ty-rigid-var (name "two")))))
	(s-alias-decl
		(ty-header (name "ExpandedPair")
			(ty-args
				(ty-rigid-var (name "one"))
				(ty-rigid-var (name "two"))))
		(ty-tuple
			(ty-rigid-var-lookup (ty-rigid-var (name "one")))
			(ty-rigid-var-lookup (ty-rigid-var (name "two")))))
	(s-alias-decl
		(ty-header (name "RecordType"))
		(ty-record
			(field (field "one")
				(ty-lookup (name "Str") (builtin)))
			(field (field "two")
				(ty-lookup (name "Str") (builtin)))))
	(s-alias-decl
		(ty-header (name "ExpandedRecordType"))
		(ty-record
			(field (field "one")
				(ty-lookup (name "Str") (builtin)))
			(field (field "two")
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Dec)"))
		(patt (type "List(Dec)"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "{ one: Dec, two: Dec }"))
		(patt (type "{ one: Dec, two: Dec }"))
		(patt (type "{ values: List(Dec) }"))
		(patt (type "{ values: List(Dec) }"))
		(patt (type "a, _arg -> a"))
		(patt (type "a, _arg -> a"))
		(patt (type "_arg, _arg2 -> {}"))
		(patt (type "a, _arg -> List(a)"))
		(patt (type "a, b -> (a, b)"))
		(patt (type "_arg, _arg2 -> (a -> a)"))
		(patt (type "[ExpandedPair(_a, b), Pair(b, _c)] -> b"))
		(patt (type "List(Dec)")))
	(type_decls
		(alias (type "Pair(one, two)")
			(ty-header (name "Pair")
				(ty-args
					(ty-rigid-var (name "one"))
					(ty-rigid-var (name "two")))))
		(alias (type "ExpandedPair(one, two)")
			(ty-header (name "ExpandedPair")
				(ty-args
					(ty-rigid-var (name "one"))
					(ty-rigid-var (name "two")))))
		(alias (type "RecordType")
			(ty-header (name "RecordType")))
		(alias (type "ExpandedRecordType")
			(ty-header (name "ExpandedRecordType"))))
	(expressions
		(expr (type "List(Dec)"))
		(expr (type "List(Dec)"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "{ one: Dec, two: Dec }"))
		(expr (type "{ one: Dec, two: Dec }"))
		(expr (type "{ values: List(Dec) }"))
		(expr (type "{ values: List(Dec) }"))
		(expr (type "a, _arg -> a"))
		(expr (type "a, _arg -> a"))
		(expr (type "_arg, _arg2 -> {}"))
		(expr (type "a, _arg -> List(a)"))
		(expr (type "a, b -> (a, b)"))
		(expr (type "_arg, _arg2 -> (a -> a)"))
		(expr (type "[ExpandedPair(_a, b), Pair(b, _c)] -> b"))
		(expr (type "List(Dec)"))))
~~~
