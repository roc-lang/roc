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
WHERE CLAUSE ERROR - where_clauses_error_cases.md:3:10:3:11
WHERE CLAUSE ERROR - where_clauses_error_cases.md:3:3:3:21
PARSE ERROR - where_clauses_error_cases.md:3:22:3:23
PARSE ERROR - where_clauses_error_cases.md:3:23:3:24
WHERE CLAUSE ERROR - where_clauses_error_cases.md:7:3:7:10
PARSE ERROR - where_clauses_error_cases.md:7:10:7:11
MALFORMED WHERE CLAUSE - where_clauses_error_cases.md:3:10:3:21
MALFORMED WHERE CLAUSE - where_clauses_error_cases.md:7:3:7:10
# PROBLEMS
**WHERE CLAUSE ERROR**
Expected a colon **:** after the method name in this where clause constraint.
Method constraints require a colon to separate the method name from its type.
For example:     a.method : a -> b

**where_clauses_error_cases.md:3:10:3:11:**
```roc
  where [a.method -> b]
```
         ^


**WHERE CLAUSE ERROR**
Expected a closing bracket **]** after the where clause constraints.
Where clauses should look like:     where [a.method : Type]

**where_clauses_error_cases.md:3:3:3:21:**
```roc
  where [a.method -> b]
```
  ^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**where_clauses_error_cases.md:3:22:3:23:**
```roc
  where [a.method -> b]
```
                     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**where_clauses_error_cases.md:3:23:3:24:**
```roc
  where [a.method -> b]
```
                      ^


**WHERE CLAUSE ERROR**
A `where` clause cannot be empty.
Where clauses must contain at least one constraint.
For example:
        where [a.method : a -> b]

**where_clauses_error_cases.md:7:3:7:10:**
```roc
  where []
```
  ^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**where_clauses_error_cases.md:7:10:7:11:**
```roc
  where []
```
         ^


**MALFORMED WHERE CLAUSE**
This where clause could not be parsed correctly.

**where_clauses_error_cases.md:3:10:3:21:**
```roc
  where [a.method -> b]
```
         ^^^^^^^^^^^

Check the syntax of your where clause.

**MALFORMED WHERE CLAUSE**
This where clause could not be parsed correctly.

**where_clauses_error_cases.md:7:3:7:10:**
```roc
  where []
```
  ^^^^^^^

Check the syntax of your where clause.

# TOKENS
~~~zig
LowerIdent(2:1-2:11),OpColon(2:12-2:13),LowerIdent(2:14-2:15),OpArrow(2:16-2:18),LowerIdent(2:19-2:20),
KwWhere(3:3-3:8),OpenSquare(3:9-3:10),LowerIdent(3:10-3:11),NoSpaceDotLowerIdent(3:11-3:18),OpArrow(3:19-3:21),LowerIdent(3:22-3:23),CloseSquare(3:23-3:24),
LowerIdent(6:1-6:11),OpColon(6:12-6:13),LowerIdent(6:14-6:15),OpArrow(6:16-6:18),LowerIdent(6:19-6:20),
KwWhere(7:3-7:8),OpenSquare(7:9-7:10),CloseSquare(7:10-7:11),
LowerIdent(10:1-10:11),OpColon(10:12-10:13),LowerIdent(10:14-10:15),OpArrow(10:16-10:18),LowerIdent(10:19-10:20),
KwWhere(11:3-11:8),OpenSquare(11:9-11:10),LowerIdent(11:10-11:11),NoSpaceDotLowerIdent(11:11-11:18),OpColon(11:19-11:20),LowerIdent(11:21-11:22),OpArrow(11:23-11:25),LowerIdent(11:26-11:27),CloseSquare(11:27-11:28),
EndOfFile(12:1-12:1),
~~~
# PARSE
~~~clojure
(file @2.1-11.28
	(type-module @2.1-2.11)
	(statements
		(s-type-anno @2.1-3.21 (name "broken_fn1")
			(ty-fn @2.14-2.20
				(ty-var @2.14-2.15 (raw "a"))
				(ty-var @2.19-2.20 (raw "b")))
			(where
				(malformed @3.10-3.21 (reason "where_expected_colon"))))
		(s-malformed @3.22-3.23 (tag "statement_unexpected_token"))
		(s-malformed @3.23-3.24 (tag "statement_unexpected_token"))
		(s-type-anno @6.1-7.10 (name "broken_fn2")
			(ty-fn @6.14-6.20
				(ty-var @6.14-6.15 (raw "a"))
				(ty-var @6.19-6.20 (raw "b")))
			(where
				(malformed @7.3-7.10 (reason "where_expected_constraints"))))
		(s-malformed @7.10-7.11 (tag "statement_unexpected_token"))
		(s-type-anno @10.1-11.28 (name "broken_fn3")
			(ty-fn @10.14-10.20
				(ty-var @10.14-10.15 (raw "a"))
				(ty-var @10.19-10.20 (raw "b")))
			(where
				(method @11.10-11.27 (module-of "c") (name "method")
					(args
						(ty-var @11.21-11.22 (raw "c")))
					(ty-var @11.26-11.27 (raw "d")))))))
~~~
# FORMATTED
~~~roc
# Missing colon in constraint
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
	(s-type-anno @2.1-3.21 (name "broken_fn1")
		(ty-fn @2.14-2.20 (effectful false)
			(ty-rigid-var @2.14-2.15 (name "a"))
			(ty-rigid-var @2.19-2.20 (name "b")))
		(where
			(malformed @3.10-3.21)))
	(s-type-anno @6.1-7.10 (name "broken_fn2")
		(ty-fn @6.14-6.20 (effectful false)
			(ty-rigid-var @6.14-6.15 (name "a"))
			(ty-rigid-var @6.19-6.20 (name "b")))
		(where
			(malformed @7.3-7.10)))
	(s-type-anno @10.1-11.28 (name "broken_fn3")
		(ty-fn @10.14-10.20 (effectful false)
			(ty-rigid-var @10.14-10.15 (name "a"))
			(ty-rigid-var @10.19-10.20 (name "b")))
		(where
			(method @11.10-11.27 (module-of "c") (ident "method")
				(args
					(ty-rigid-var @11.21-11.22 (name "c")))
				(ty-rigid-var @11.26-11.27 (name "d")))))
	(ext-decl @11.10-11.27 (ident "c.method") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
