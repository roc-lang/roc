# META
~~~ini
description=A primitive
type=snippet
~~~
# SOURCE
~~~roc
import json.Json [foo, BAR]
~~~
# EXPECTED
UNEXPECTED STATEMENT - stmt_import.md:1:18:1:19
UNEXPECTED STATEMENT - stmt_import.md:1:19:1:22
UNEXPECTED STATEMENT - stmt_import.md:1:22:1:23
TYPE APPLICATION NEEDS PARENTHESES - stmt_import.md:1:27:1:28
DUPLICATE DEFINITION - stmt_import.md:1:1:1:17
# PROBLEMS
── ✗ unexpected statement ────────────────────────────────── stmt_import.md:1:18

I was parsing a statement, and this token cannot start a statement here.

import json.Json [foo, BAR]
                 ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ unexpected statement ────────────────────────────────── stmt_import.md:1:19

I was parsing a statement, and this token cannot start a statement here.

import json.Json [foo, BAR]
                  ^^^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found foo here.
Names that start with lowercase letters are value names or record field names,
depending on the surrounding syntax.

── ✗ unexpected statement ────────────────────────────────── stmt_import.md:1:22

I was parsing a statement, and this token cannot start a statement here.

import json.Json [foo, BAR]
                     ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found , here.
A comma separates items, but there must be a valid item on both sides of it.

── ✗ type application needs parentheses ──────────────────── stmt_import.md:1:27

I was parsing a type annotation, and I found a type argument without
parentheses.

import json.Json [foo, BAR]
                          ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ● duplicate definition ─────────────────────────────────── stmt_import.md:1:1

The name Json is being redeclared here:

import json.Json [foo, BAR]
^^^^^^^^^^^^^^^^

In this scope, Json was already defined in stmt_import.md:1:1:

import json.Json [foo, BAR]
^

# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,OpenSquare,LowerIdent,Comma,UpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "json.Json"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
import json.Json
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (mod "json.Json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
