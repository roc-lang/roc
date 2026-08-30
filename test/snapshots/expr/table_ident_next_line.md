# META
~~~ini
description=bare table ident then a lowercase name on the next line is not a table literal
type=snippet
~~~
# SOURCE
~~~roc
table_for_host = table

names_for_host : List(Str)
names_for_host = names
~~~
# EXPECTED
NAME NOT IN SCOPE - table_ident_next_line.md:1:18:1:23
NAME NOT IN SCOPE - table_ident_next_line.md:4:18:4:23
# PROBLEMS
── ✗ name not in scope ─────────────────────────── table_ident_next_line.md:1:18

Nothing is named table in this scope.

table_for_host = table
                 ^^^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ─────────────────────────── table_ident_next_line.md:4:18

Nothing is named names in this scope.

names_for_host = names
                 ^^^^^

Is it misspelled, or is there an import missing?

# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "table_for_host"))
			(e-ident (raw "table")))
		(s-type-anno (name "names_for_host")
			(ty-apply
				(ty (name "List"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "names_for_host"))
			(e-ident (raw "names")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "table_for_host"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "names_for_host"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "List(Str)")))
	(expressions
		(expr (type "Error"))
		(expr (type "List(Str)"))))
~~~
