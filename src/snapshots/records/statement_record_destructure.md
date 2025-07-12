# META
~~~ini
description=Record destructuring in assignment statement
type=file
~~~
# SOURCE
~~~roc
{ name, age, email } = person
~~~
# EXPECTED
MISSING HEADER - statement_record_destructure.md:1:1:1:2
UNEXPECTED TOKEN IN EXPRESSION - statement_record_destructure.md:1:7:1:8
UNEXPECTED TOKEN IN EXPRESSION - statement_record_destructure.md:1:12:1:13
UNEXPECTED TOKEN IN EXPRESSION - statement_record_destructure.md:1:20:1:21
UNEXPECTED TOKEN IN EXPRESSION - statement_record_destructure.md:1:22:1:23
INVALID STATEMENT - statement_record_destructure.md:1:3:1:7
INVALID STATEMENT - statement_record_destructure.md:1:7:1:8
INVALID STATEMENT - statement_record_destructure.md:1:9:1:12
INVALID STATEMENT - statement_record_destructure.md:1:12:1:13
INVALID STATEMENT - statement_record_destructure.md:1:14:1:19
INVALID STATEMENT - statement_record_destructure.md:1:20:1:21
INVALID STATEMENT - statement_record_destructure.md:1:22:1:23
INVALID STATEMENT - statement_record_destructure.md:1:24:1:30
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**statement_record_destructure.md:1:1:1:2:**
```roc
{ name, age, email } = person
```
^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**statement_record_destructure.md:1:7:1:8:**
```roc
{ name, age, email } = person
```
      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**statement_record_destructure.md:1:12:1:13:**
```roc
{ name, age, email } = person
```
           ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**statement_record_destructure.md:1:20:1:21:**
```roc
{ name, age, email } = person
```
                   ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**statement_record_destructure.md:1:22:1:23:**
```roc
{ name, age, email } = person
```
                     ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**statement_record_destructure.md:1:3:1:7:**
```roc
{ name, age, email } = person
```
  ^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**statement_record_destructure.md:1:7:1:8:**
```roc
{ name, age, email } = person
```
      ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**statement_record_destructure.md:1:9:1:12:**
```roc
{ name, age, email } = person
```
        ^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**statement_record_destructure.md:1:12:1:13:**
```roc
{ name, age, email } = person
```
           ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**statement_record_destructure.md:1:14:1:19:**
```roc
{ name, age, email } = person
```
             ^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**statement_record_destructure.md:1:20:1:21:**
```roc
{ name, age, email } = person
```
                   ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**statement_record_destructure.md:1:22:1:23:**
```roc
{ name, age, email } = person
```
                     ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**statement_record_destructure.md:1:24:1:30:**
```roc
{ name, age, email } = person
```
                       ^^^^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:7),Comma(1:7-1:8),LowerIdent(1:9-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:19),CloseCurly(1:20-1:21),OpAssign(1:22-1:23),LowerIdent(1:24-1:30),EndOfFile(1:30-1:30),
~~~
# PARSE
~~~clojure
(file @1.1-1.30
	(malformed-header @1.1-1.2 (tag "missing_header"))
	(statements
		(e-ident @1.3-1.7 (raw "name"))
		(e-malformed @1.7-1.8 (reason "expr_unexpected_token"))
		(e-ident @1.9-1.12 (raw "age"))
		(e-malformed @1.12-1.13 (reason "expr_unexpected_token"))
		(e-ident @1.14-1.19 (raw "email"))
		(e-malformed @1.20-1.21 (reason "expr_unexpected_token"))
		(e-malformed @1.22-1.23 (reason "expr_unexpected_token"))
		(e-ident @1.24-1.30 (raw "person"))))
~~~
# FORMATTED
~~~roc
nameageemailperson
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
