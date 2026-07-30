# META
~~~ini
description=Error cases for where clauses
type=snippet
~~~
# SOURCE
~~~roc
# Missing colon in constraint
broken_fn1 : a -> b
  where [a.method -> b]

# Empty where clause
broken_fn2 : a -> b
  where []

# Referencing undefined type variable
broken_fn3 : a -> b
  where [c.method : c -> d]
~~~
# EXPECTED
EXPECTED CONSTRAINT TYPE - where_clauses_error_cases.md:3:10:3:11
EXPECTED WHERE CLAUSE END - where_clauses_error_cases.md:3:3:3:21
UNEXPECTED STATEMENT - where_clauses_error_cases.md:3:22:3:23
UNEXPECTED STATEMENT - where_clauses_error_cases.md:3:23:3:24
EXPECTED WHERE CONSTRAINT - where_clauses_error_cases.md:7:3:7:10
UNEXPECTED STATEMENT - where_clauses_error_cases.md:7:10:7:11
MALFORMED WHERE CLAUSE - where_clauses_error_cases.md:3:10:3:21
MALFORMED WHERE CLAUSE - where_clauses_error_cases.md:7:3:7:10
DECLARATION HAS NO VALUE - where_clauses_error_cases.md:2:1:3:21
DECLARATION HAS NO VALUE - where_clauses_error_cases.md:6:1:7:10
DECLARATION HAS NO VALUE - where_clauses_error_cases.md:10:1:11:28
# PROBLEMS

┌──────────────────────────┐
│ EXPECTED CONSTRAINT TYPE ├─ I was parsing a `where` method constraint, ─────┐
└┬─────────────────────────┘  and I expected `:` before the method type.      │
 │                                                                            │
 │  where [a.method -> b]                                                     │
 │         ‾                                                                  │
 └───────────────────────────────────────── where_clauses_error_cases.md:3:10 ┘

    Method constraints use a colon between the method name and its type.

    For example:
        where [a.hash : a -> U64]

    I found `a` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌───────────────────────────┐
│ EXPECTED WHERE CLAUSE END ├─ I was parsing a `where` clause, and I ─────────┐
└┬──────────────────────────┘  expected `]`.                                  │
 │                                                                            │
 │  where [a.method -> b]                                                     │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └────────────────────────────────────────── where_clauses_error_cases.md:3:3 ┘

    Close the where constraint list after the final constraint.

    For example:
        where [a.hash : a -> U64]

    I found `where [a.method ->` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  where [a.method -> b]                                                     │
 │                     ‾                                                      │
 └───────────────────────────────────────── where_clauses_error_cases.md:3:22 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `b` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  where [a.method -> b]                                                     │
 │                      ‾                                                     │
 └───────────────────────────────────────── where_clauses_error_cases.md:3:23 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `]` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌───────────────────────────┐
│ EXPECTED WHERE CONSTRAINT ├─ I was parsing a `where` clause, and I ─────────┐
└┬──────────────────────────┘  expected at least one constraint.              │
 │                                                                            │
 │  where []                                                                  │
 │  ‾‾‾‾‾‾‾                                                                   │
 └────────────────────────────────────────── where_clauses_error_cases.md:7:3 ┘

    Remove the empty `where` clause or add a constraint inside the brackets.

    For example:
        where [a.hash : a -> U64]

    I found `where [` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  where []                                                                  │
 │         ‾                                                                  │
 └───────────────────────────────────────── where_clauses_error_cases.md:7:10 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `]` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌────────────────────────┐
│ MALFORMED WHERE CLAUSE ├─ This where clause could not be parsed correctly. ─┐
└┬───────────────────────┘                                                    │
 │                                                                            │
 │  where [a.method -> b]                                                     │
 │         ‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └───────────────────────────────────────── where_clauses_error_cases.md:3:10 ┘

    Check the syntax of your where clause.


┌────────────────────────┐
│ MALFORMED WHERE CLAUSE ├─ This where clause could not be parsed correctly. ─┐
└┬───────────────────────┘                                                    │
 │                                                                            │
 │  where []                                                                  │
 │  ‾‾‾‾‾‾‾                                                                   │
 └────────────────────────────────────────── where_clauses_error_cases.md:7:3 ┘

    Check the syntax of your where clause.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  broken_fn1 : a -> b                                                       │
 │    where [a.method -> b]                                                   │
 │                                                                            │
 └────────────────────────────────────────── where_clauses_error_cases.md:2:1 ┘

    Add a value body here, or put hosted functions in a platform type mod so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  broken_fn2 : a -> b                                                       │
 │    where []                                                                │
 │                                                                            │
 └────────────────────────────────────────── where_clauses_error_cases.md:6:1 ┘

    Add a value body here, or put hosted functions in a platform type mod so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  broken_fn3 : a -> b                                                       │
 │    where [c.method : c -> d]                                               │
 │                                                                            │
 └───────────────────────────────────────── where_clauses_error_cases.md:10:1 ┘

    Add a value body here, or put hosted functions in a platform type mod so
    they are published through the host boundary.

# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
KwWhere,OpenSquare,CloseSquare,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "broken_fn1")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b")))
			(where
				(malformed (reason "where_expected_colon"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "broken_fn2")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b")))
			(where
				(malformed (reason "where_expected_constraints"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "broken_fn3")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b")))
			(where
				(method (mod-of "c") (name "method")
					(args
						(ty-var (raw "c")))
					(ty-var (raw "d")))))))
~~~
# FORMATTED
~~~roc
# Missing colon in constraint
broken_fn1 : a -> b
	where []


# Empty where clause
broken_fn2 : a -> b
	where []


# Referencing undefined type variable
broken_fn3 : a -> b
	where [c.method : c -> d]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "broken_fn1"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b")))
			(where
				(malformed))))
	(d-let
		(p-assign (ident "broken_fn2"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b")))
			(where
				(malformed))))
	(d-let
		(p-assign (ident "broken_fn3"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b")))
			(where
				(method (ty-rigid-var (name "c")) (name "method")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "c"))))
					(ty-rigid-var (name "d")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> b"))
		(patt (type "a -> b"))
		(patt (type "a -> b")))
	(expressions
		(expr (type "a -> b"))
		(expr (type "a -> b"))
		(expr (type "a -> b"))))
~~~
