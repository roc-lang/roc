# META
~~~ini
description=Error cases for where clauses
type=snippet
~~~
# SOURCE
~~~roc
# Missing colon in constraint
broken_fn1 : a -> b
  where
    module(a).method -> b

# Empty where clause
broken_fn2 : a -> b
  where

# Referencing undefined type variable
broken_fn3 : a -> b
  where
    module(c).method : c -> d
~~~
# EXPECTED
WHERE CLAUSE ERROR - where_clauses_error_cases.md:4:5:4:11
PARSE ERROR - where_clauses_error_cases.md:4:25:4:26
WHERE CLAUSE ERROR - where_clauses_error_cases.md:8:3:8:8
MALFORMED WHERE CLAUSE - where_clauses_error_cases.md:4:5:4:24
MALFORMED WHERE CLAUSE - where_clauses_error_cases.md:8:3:8:8
# PROBLEMS
**WHERE CLAUSE ERROR**
Expected a colon **:** after the method name in this where clause constraint.
Method constraints require a colon to separate the method name from its type.
For example:     module(a).method : a -> b

**where_clauses_error_cases.md:4:5:4:11:**
```roc
    module(a).method -> b
```
    ^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**where_clauses_error_cases.md:4:25:4:26:**
```roc
    module(a).method -> b
```
                        ^


**WHERE CLAUSE ERROR**
A `where` clause cannot be empty.
Where clauses must contain at least one constraint.
For example:
        module(a).method : a -> b

**where_clauses_error_cases.md:8:3:8:8:**
```roc
  where
```
  ^^^^^


**MALFORMED WHERE CLAUSE**
This where clause could not be parsed correctly.

**where_clauses_error_cases.md:4:5:4:24:**
```roc
    module(a).method -> b
```
    ^^^^^^^^^^^^^^^^^^^

Check the syntax of your where clause.

**MALFORMED WHERE CLAUSE**
This where clause could not be parsed correctly.

**where_clauses_error_cases.md:8:3:8:8:**
```roc
  where
```
  ^^^^^

Check the syntax of your where clause.

# TOKENS
~~~zig
LowerIdent(2:1-2:11),OpColon(2:12-2:13),LowerIdent(2:14-2:15),OpArrow(2:16-2:18),LowerIdent(2:19-2:20),
KwWhere(3:3-3:8),
KwModule(4:5-4:11),NoSpaceOpenRound(4:11-4:12),LowerIdent(4:12-4:13),CloseRound(4:13-4:14),NoSpaceDotLowerIdent(4:14-4:21),OpArrow(4:22-4:24),LowerIdent(4:25-4:26),
LowerIdent(7:1-7:11),OpColon(7:12-7:13),LowerIdent(7:14-7:15),OpArrow(7:16-7:18),LowerIdent(7:19-7:20),
KwWhere(8:3-8:8),
LowerIdent(11:1-11:11),OpColon(11:12-11:13),LowerIdent(11:14-11:15),OpArrow(11:16-11:18),LowerIdent(11:19-11:20),
KwWhere(12:3-12:8),
KwModule(13:5-13:11),NoSpaceOpenRound(13:11-13:12),LowerIdent(13:12-13:13),CloseRound(13:13-13:14),NoSpaceDotLowerIdent(13:14-13:21),OpColon(13:22-13:23),LowerIdent(13:24-13:25),OpArrow(13:26-13:28),LowerIdent(13:29-13:30),
EndOfFile(14:1-14:1),
~~~
# PARSE
~~~clojure
(file @2.1-13.30
	(type-module @2.1-2.11)
	(statements
		(s-type-anno @2.1-4.24 (name "broken_fn1")
			(ty-fn @2.14-2.20
				(ty-var @2.14-2.15 (raw "a"))
				(ty-var @2.19-2.20 (raw "b")))
			(where
				(malformed @4.5-4.24 (reason "where_expected_colon"))))
		(s-malformed @4.25-4.26 (tag "statement_unexpected_token"))
		(s-type-anno @7.1-8.8 (name "broken_fn2")
			(ty-fn @7.14-7.20
				(ty-var @7.14-7.15 (raw "a"))
				(ty-var @7.19-7.20 (raw "b")))
			(where
				(malformed @8.3-8.8 (reason "where_expected_constraints"))))
		(s-type-anno @11.1-13.30 (name "broken_fn3")
			(ty-fn @11.14-11.20
				(ty-var @11.14-11.15 (raw "a"))
				(ty-var @11.19-11.20 (raw "b")))
			(where
				(method @13.5-13.30 (module-of "c") (name "method")
					(args
						(ty-var @13.24-13.25 (raw "c")))
					(ty-var @13.29-13.30 (raw "d")))))))
~~~
# FORMATTED
~~~roc
# Missing colon in constraint
broken_fn1 : a -> b
	where
		

# Empty where clause
broken_fn2 : a -> b
	where 

# Referencing undefined type variable
broken_fn3 : a -> b
	where
		module(c).method : c -> d
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno @2.1-4.24 (name "broken_fn1")
		(ty-fn @2.14-2.20 (effectful false)
			(ty-rigid-var @2.14-2.15 (name "a"))
			(ty-rigid-var @2.19-2.20 (name "b")))
		(where
			(malformed @4.5-4.24)))
	(s-type-anno @7.1-8.8 (name "broken_fn2")
		(ty-fn @7.14-7.20 (effectful false)
			(ty-rigid-var @7.14-7.15 (name "a"))
			(ty-rigid-var @7.19-7.20 (name "b")))
		(where
			(malformed @8.3-8.8)))
	(s-type-anno @11.1-13.30 (name "broken_fn3")
		(ty-fn @11.14-11.20 (effectful false)
			(ty-rigid-var @11.14-11.15 (name "a"))
			(ty-rigid-var @11.19-11.20 (name "b")))
		(where
			(method @13.5-13.30 (module-of "c") (ident "method")
				(args
					(ty-rigid-var @13.24-13.25 (name "c")))
				(ty-rigid-var @13.29-13.30 (name "d")))))
	(ext-decl @13.5-13.30 (ident "module(c).method") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
