# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
H{o,
    ]
foo =

    "on        (string 'onmo %')))
~~~
# PROBLEMS
TOKENIZE: (2:3-2:3) AsciiControl:
    ]
  ^TOKENIZE: (2:6-2:6) MismatchedBrace:
    ]
     ^TOKENIZE: (5:6-5:35) UnclosedString:
    "on        (string 'onmo %')))
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^PARSER: missing_header
**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:3-1:4),Comma(1:4-1:5),Newline(1:1-1:1),
CloseCurly(2:6-2:7),Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),Newline(1:1-1:1),
Newline(1:1-1:1),
StringStart(5:5-5:6),StringPart(5:6-5:35),EndOfFile(5:35-5:35),
~~~
# PARSE
~~~clojure
(file (1:1-5:35)
	(malformed_header (1:1-1:2) "missing_header")
	(statements
		(record (1:2-2:7) (field "o"))
		(decl (3:1-5:35)
			(ident (3:1-3:4) "foo")
			(string (5:5-5:35) (string_part (5:6-5:35) "on        (string 'onmo %')))")))))
~~~
# FORMATTED
~~~roc
{
	o,
}
foo = 

	"on        (string 'onmo %')))"
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (3:1-3:4)
				(pid 13)
				(ident "foo")))
		(def_expr
			(e_string (5:5-5:35) (e_literal (5:6-5:35) "on        (string 'onmo %')))")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "foo" 16 (type "Str")))
	(expressions
		(expr (5:5-5:35) 15 (type "Str"))))
~~~