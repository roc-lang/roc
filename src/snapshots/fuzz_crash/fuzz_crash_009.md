# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
f{o,
     ]

foo =

    "onmo %
~~~
# PROBLEMS
TOKENIZE: (2:6-2:6) MismatchedBrace:
     ]
     ^TOKENIZE: (6:6-6:12) UnclosedString:
    "onmo %
     ^^^^^^PARSER: missing_header
**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:3-1:4),Comma(1:4-1:5),Newline(1:1-1:1),
CloseCurly(2:6-2:7),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),Newline(1:1-1:1),
Newline(1:1-1:1),
StringStart(6:5-6:6),StringPart(6:6-6:12),EndOfFile(6:12-6:12),
~~~
# PARSE
~~~clojure
(file (1:1-6:12)
	(malformed_header (1:1-1:2) "missing_header")
	(statements
		(record (1:2-2:7) (field "o"))
		(decl (4:1-6:12)
			(ident (4:1-4:4) "foo")
			(string (6:5-6:12) (string_part (6:6-6:12) "onmo %")))))
~~~
# FORMATTED
~~~roc
{
	o,
}

foo = 

	"onmo %"
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (4:1-4:4)
				(pid 13)
				(ident "foo")))
		(def_expr
			(e_string (6:5-6:12) (e_literal (6:6-6:12) "onmo %")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "foo" 16 (type "Str")))
	(expressions
		(expr (6:5-6:12) 15 (type "Str"))))
~~~