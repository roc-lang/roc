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
	(type-module)
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
				(method (module-of "c") (name "method")
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
	(s-type-anno (name "broken_fn1")
		(ty-fn (effectful false)
			(ty-rigid-var (name "a"))
			(ty-rigid-var (name "b")))
		(where
			(malformed)))
	(s-type-anno (name "broken_fn2")
		(ty-fn (effectful false)
			(ty-rigid-var (name "a"))
			(ty-rigid-var (name "b")))
		(where
			(malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
