# META
~~~ini
description=Error cases for where clauses
type=file
~~~
# SOURCE
~~~roc
module [broken_fn1, broken_fn2, broken_fn3]

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
    module(c).method : c -> d,
~~~
# EXPECTED
WHERE CLAUSE ERROR - where_clauses_error_cases.md:6:5:6:11
WHERE CLAUSE ERROR - where_clauses_error_cases.md:10:3:10:8
MALFORMED WHERE CLAUSE - where_clauses_error_cases.md:6:5:6:24
INVALID STATEMENT - where_clauses_error_cases.md:6:25:6:26
MALFORMED WHERE CLAUSE - where_clauses_error_cases.md:10:3:10:8
UNDECLARED TYPE VARIABLE - where_clauses_error_cases.md:15:24:15:25
UNDECLARED TYPE VARIABLE - where_clauses_error_cases.md:15:29:15:30
EXPOSED BUT NOT DEFINED - where_clauses_error_cases.md:1:9:1:19
EXPOSED BUT NOT DEFINED - where_clauses_error_cases.md:1:21:1:31
EXPOSED BUT NOT DEFINED - where_clauses_error_cases.md:1:33:1:43
# PROBLEMS
**WHERE CLAUSE ERROR**
Expected a colon **:** after the method name in this where clause constraint.
Method constraints require a colon to separate the method name from its type.
For example:     module(a).method : a -> b

Here is the problematic code:
**where_clauses_error_cases.md:6:5:6:11:**
```roc
    module(a).method -> b
```
    ^^^^^^


**WHERE CLAUSE ERROR**
A `where` clause cannot be empty.
Where clauses must contain at least one constraint.
For example:
        module(a).method : a -> b

Here is the problematic code:
**where_clauses_error_cases.md:10:3:10:8:**
```roc
  where
```
  ^^^^^


**MALFORMED WHERE CLAUSE**
This where clause could not be parsed correctly.

**where_clauses_error_cases.md:6:5:6:24:**
```roc
    module(a).method -> b
```
    ^^^^^^^^^^^^^^^^^^^

Check the syntax of your where clause.

**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**where_clauses_error_cases.md:6:25:6:26:**
```roc
    module(a).method -> b
```
                        ^


**MALFORMED WHERE CLAUSE**
This where clause could not be parsed correctly.

**where_clauses_error_cases.md:10:3:10:8:**
```roc
  where
```
  ^^^^^

Check the syntax of your where clause.

**UNDECLARED TYPE VARIABLE**
The type variable _c_ is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**where_clauses_error_cases.md:15:24:15:25:**
```roc
    module(c).method : c -> d,
```
                       ^


**UNDECLARED TYPE VARIABLE**
The type variable _d_ is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**where_clauses_error_cases.md:15:29:15:30:**
```roc
    module(c).method : c -> d,
```
                            ^


**EXPOSED BUT NOT DEFINED**
The module header says that `broken_fn1` is exposed, but it is not defined anywhere in this module.

**where_clauses_error_cases.md:1:9:1:19:**
```roc
module [broken_fn1, broken_fn2, broken_fn3]
```
        ^^^^^^^^^^
You can fix this by either defining `broken_fn1` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `broken_fn2` is exposed, but it is not defined anywhere in this module.

**where_clauses_error_cases.md:1:21:1:31:**
```roc
module [broken_fn1, broken_fn2, broken_fn3]
```
                    ^^^^^^^^^^
You can fix this by either defining `broken_fn2` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `broken_fn3` is exposed, but it is not defined anywhere in this module.

**where_clauses_error_cases.md:1:33:1:43:**
```roc
module [broken_fn1, broken_fn2, broken_fn3]
```
                                ^^^^^^^^^^
You can fix this by either defining `broken_fn3` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:19),Comma(1:19-1:20),LowerIdent(1:21-1:31),Comma(1:31-1:32),LowerIdent(1:33-1:43),CloseSquare(1:43-1:44),
LowerIdent(4:1-4:11),OpColon(4:12-4:13),LowerIdent(4:14-4:15),OpArrow(4:16-4:18),LowerIdent(4:19-4:20),
KwWhere(5:3-5:8),
KwModule(6:5-6:11),NoSpaceOpenRound(6:11-6:12),LowerIdent(6:12-6:13),CloseRound(6:13-6:14),NoSpaceDotLowerIdent(6:14-6:21),OpArrow(6:22-6:24),LowerIdent(6:25-6:26),
LowerIdent(9:1-9:11),OpColon(9:12-9:13),LowerIdent(9:14-9:15),OpArrow(9:16-9:18),LowerIdent(9:19-9:20),
KwWhere(10:3-10:8),
LowerIdent(13:1-13:11),OpColon(13:12-13:13),LowerIdent(13:14-13:15),OpArrow(13:16-13:18),LowerIdent(13:19-13:20),
KwWhere(14:3-14:8),
KwModule(15:5-15:11),NoSpaceOpenRound(15:11-15:12),LowerIdent(15:12-15:13),CloseRound(15:13-15:14),NoSpaceDotLowerIdent(15:14-15:21),OpColon(15:22-15:23),LowerIdent(15:24-15:25),OpArrow(15:26-15:28),LowerIdent(15:29-15:30),Comma(15:30-15:31),EndOfFile(15:31-15:31),
~~~
# PARSE
~~~clojure
(file @1.1-15.31
	(module @1.1-1.44
		(exposes @1.8-1.44
			(exposed-lower-ident @1.9-1.19
				(text "broken_fn1"))
			(exposed-lower-ident @1.21-1.31
				(text "broken_fn2"))
			(exposed-lower-ident @1.33-1.43
				(text "broken_fn3"))))
	(statements
		(s-type-anno @4.1-6.24 (name "broken_fn1")
			(ty-fn @4.14-4.20
				(ty-var @4.14-4.15 (raw "a"))
				(ty-var @4.19-4.20 (raw "b")))
			(where
				(malformed @6.5-6.24 (reason "where_expected_colon"))))
		(e-ident @6.25-6.26 (raw "b"))
		(s-type-anno @9.1-10.8 (name "broken_fn2")
			(ty-fn @9.14-9.20
				(ty-var @9.14-9.15 (raw "a"))
				(ty-var @9.19-9.20 (raw "b")))
			(where
				(malformed @10.3-10.8 (reason "where_expected_constraints"))))
		(s-type-anno @13.1-15.31 (name "broken_fn3")
			(ty-fn @13.14-13.20
				(ty-var @13.14-13.15 (raw "a"))
				(ty-var @13.19-13.20 (raw "b")))
			(where
				(method @15.5-15.30 (module-of "c") (name "method")
					(args
						(ty-var @15.24-15.25 (raw "c")))
					(ty-var @15.29-15.30 (raw "d")))))))
~~~
# FORMATTED
~~~roc
module [broken_fn1, broken_fn2, broken_fn3]

# Missing colon in constraint
broken_fn1 : a -> b
 where
	,
b

# Empty where clause
broken_fn2 : a -> b
 where
	,

# Referencing undefined type variable
broken_fn3 : a -> b
 where
	module(c).method : c -> d,
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno @4.1-6.24 (name "broken_fn1")
		(ty-fn @4.14-4.20 (effectful false)
			(ty-var @4.14-4.15 (name "a"))
			(ty-var @4.19-4.20 (name "b")))
		(where
			(malformed @6.5-6.24)))
	(s-type-anno @9.1-10.8 (name "broken_fn2")
		(ty-fn @9.14-9.20 (effectful false)
			(ty-var @9.14-9.15 (name "a"))
			(ty-var @9.19-9.20 (name "b")))
		(where
			(malformed @10.3-10.8)))
	(s-type-anno @13.1-15.31 (name "broken_fn3")
		(ty-fn @13.14-13.20 (effectful false)
			(ty-var @13.14-13.15 (name "a"))
			(ty-var @13.19-13.20 (name "b")))
		(where
			(method @15.5-15.30 (module-of "c") (ident "method")
				(args
					(ty-var @15.24-15.25 (name "c")))
				(ty-var @15.29-15.30 (name "d")))))
	(ext-decl @15.5-15.30 (ident "module(c).method") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
