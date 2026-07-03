# META
~~~ini
description=Match expression with invalid (old style) list rest patterns should error
type=expr
canonicalize_diagnostics=true
~~~
# SOURCE
~~~roc
match items {
    [first, ..rest] => 0 # invalid rest pattern should error
    [..rest, last] => 1 # invalid rest pattern should error
    [x, ..rest, y] => 2 # invalid rest pattern should error
}
~~~
# EXPECTED
BAD LIST REST PATTERN SYNTAX - list_rest_invalid.md:2:13:2:19
BAD LIST REST PATTERN SYNTAX - list_rest_invalid.md:3:6:3:12
BAD LIST REST PATTERN SYNTAX - list_rest_invalid.md:4:9:4:15
NAME NOT IN SCOPE - list_rest_invalid.md:1:7:1:12
UNUSED VARIABLE - list_rest_invalid.md:2:6:2:11
UNUSED VARIABLE - list_rest_invalid.md:2:15:2:15
UNUSED VARIABLE - list_rest_invalid.md:3:8:3:8
UNUSED VARIABLE - list_rest_invalid.md:3:14:3:18
UNUSED VARIABLE - list_rest_invalid.md:4:6:4:7
UNUSED VARIABLE - list_rest_invalid.md:4:11:4:11
UNUSED VARIABLE - list_rest_invalid.md:4:17:4:18
# PROBLEMS

┌──────────────────────────────┐
│ BAD LIST REST PATTERN SYNTAX ├─ List rest patterns should use the `.. as ───┐
└┬─────────────────────────────┘  name` syntax, not `..name`.                 │
 │                                                                            │
 │  [first, ..rest] => 0 # invalid rest pattern should error                  │
 │          ‾‾‾‾‾‾                                                            │
 └───────────────────────────────────────────────── list_rest_invalid.md:2:13 ┘

    For example, use `[first, .. as rest]` instead of `[first, ..rest]`.


┌──────────────────────────────┐
│ BAD LIST REST PATTERN SYNTAX ├─ List rest patterns should use the `.. as ───┐
└┬─────────────────────────────┘  name` syntax, not `..name`.                 │
 │                                                                            │
 │  [..rest, last] => 1 # invalid rest pattern should error                   │
 │   ‾‾‾‾‾‾                                                                   │
 └────────────────────────────────────────────────── list_rest_invalid.md:3:6 ┘

    For example, use `[first, .. as rest]` instead of `[first, ..rest]`.


┌──────────────────────────────┐
│ BAD LIST REST PATTERN SYNTAX ├─ List rest patterns should use the `.. as ───┐
└┬─────────────────────────────┘  name` syntax, not `..name`.                 │
 │                                                                            │
 │  [x, ..rest, y] => 2 # invalid rest pattern should error                   │
 │      ‾‾‾‾‾‾                                                                │
 └────────────────────────────────────────────────── list_rest_invalid.md:4:9 ┘

    For example, use `[first, .. as rest]` instead of `[first, ..rest]`.


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `items` in this scope. ───────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  match items {                                                             │
 │        ‾‾‾‾‾                                                               │
 └────────────────────────────────────────────────── list_rest_invalid.md:1:7 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `first` is defined here and then never used. ───┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  [first, ..rest] => 0 # invalid rest pattern should error                  │
 │   ‾‾‾‾‾                                                                    │
 └────────────────────────────────────────────────── list_rest_invalid.md:2:6 ┘

    If you don't need this variable, prefix it with an underscore like `_first`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `rest` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  [first, ..rest] => 0 # invalid rest pattern should error                  │
 │            ‾                                                               │
 └───────────────────────────────────────────────── list_rest_invalid.md:2:15 ┘

    If you don't need this variable, prefix it with an underscore like `_rest`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `rest` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  [..rest, last] => 1 # invalid rest pattern should error                   │
 │     ‾                                                                      │
 └────────────────────────────────────────────────── list_rest_invalid.md:3:8 ┘

    If you don't need this variable, prefix it with an underscore like `_rest`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `last` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  [..rest, last] => 1 # invalid rest pattern should error                   │
 │           ‾‾‾‾                                                             │
 └───────────────────────────────────────────────── list_rest_invalid.md:3:14 ┘

    If you don't need this variable, prefix it with an underscore like `_last`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `x` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  [x, ..rest, y] => 2 # invalid rest pattern should error                   │
 │   ‾                                                                        │
 └────────────────────────────────────────────────── list_rest_invalid.md:4:6 ┘

    If you don't need this variable, prefix it with an underscore like `_x` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `rest` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  [x, ..rest, y] => 2 # invalid rest pattern should error                   │
 │        ‾                                                                   │
 └───────────────────────────────────────────────── list_rest_invalid.md:4:11 ┘

    If you don't need this variable, prefix it with an underscore like `_rest`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `y` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  [x, ..rest, y] => 2 # invalid rest pattern should error                   │
 │              ‾                                                             │
 └───────────────────────────────────────────────── list_rest_invalid.md:4:17 ┘

    If you don't need this variable, prefix it with an underscore like `_y` to
    suppress this warning.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,CloseSquare,OpFatArrow,Int,
OpenSquare,DoubleDot,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,Int,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "items"))
	(branches
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-list-rest (name "rest")))
			(e-int (raw "0")))
		(branch
			(p-list
				(p-list-rest (name "rest"))
				(p-ident (raw "last")))
			(e-int (raw "1")))
		(branch
			(p-list
				(p-ident (raw "x"))
				(p-list-rest (name "rest"))
				(p-ident (raw "y")))
			(e-int (raw "2")))))
~~~
# FORMATTED
~~~roc
match items {
	[first, .. as rest] => 0 # invalid rest pattern should error
	[.. as rest, last] => 1 # invalid rest pattern should error
	[x, .. as rest, y] => 2
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first")))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-num (value "0"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "last")))
							(rest-at (index 0)
								(p-assign (ident "rest"))))))
				(value
					(e-num (value "1"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))
								(p-assign (ident "y")))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-num (value "2")))))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
