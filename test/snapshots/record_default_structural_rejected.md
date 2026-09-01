# META
~~~ini
description=A `??` default is rejected in every structural record position: a type alias, an inline value annotation, and a record nested inside a nominal backing (direct fields only)
type=snippet
~~~
# SOURCE
~~~roc
AliasCfg : { count : U8 ?? 1 }

alias_cfg : AliasCfg
alias_cfg = { count: 1 }

inline_cfg : { count : U8 ?? 2 }
inline_cfg = { count: 2 }

Nested := { inner : { count : U8 ?? 3 } }

nested : Nested
nested = Nested.{ inner: { count: 3 } }
~~~
# EXPECTED
DEFAULT NOT ALLOWED IN STRUCTURAL RECORD - record_default_structural_rejected.md:1:14:1:29
DEFAULT NOT ALLOWED IN STRUCTURAL RECORD - record_default_structural_rejected.md:6:16:6:31
DEFAULT NOT ALLOWED IN STRUCTURAL RECORD - record_default_structural_rejected.md:9:23:9:38
# PROBLEMS
── ✗ default not allowed in structural record ─ record_default_structural_rejected.md:1:14

Field defaults (??) are only allowed on the fields of a nominal record type
declaration's backing record, not in structural record types (type aliases,
inline annotations, or nested records).

AliasCfg : { count : U8 ?? 1 }
             ^^^^^^^^^^^^^^^

Hint: A default belongs to one named type, so declare a nominal type (with :=)
whose backing record carries the default, and use that type here.

── ✗ default not allowed in structural record ─ record_default_structural_rejected.md:6:16

Field defaults (??) are only allowed on the fields of a nominal record type
declaration's backing record, not in structural record types (type aliases,
inline annotations, or nested records).

inline_cfg : { count : U8 ?? 2 }
               ^^^^^^^^^^^^^^^

Hint: A default belongs to one named type, so declare a nominal type (with :=)
whose backing record carries the default, and use that type here.

── ✗ default not allowed in structural record ─ record_default_structural_rejected.md:9:23

Field defaults (??) are only allowed on the fields of a nominal record type
declaration's backing record, not in structural record types (type aliases,
inline annotations, or nested records).

Nested := { inner : { count : U8 ?? 3 } }
                      ^^^^^^^^^^^^^^^

Hint: A default belongs to one named type, so declare a nominal type (with :=)
whose backing record carries the default, and use that type here.

# TOKENS
~~~zig
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,CloseCurly,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "AliasCfg")
				(args))
			(ty-record
				(anno-record-field (name "count")
					(ty (name "U8"))
					(default
						(e-int (raw "1"))))))
		(s-type-anno (name "alias_cfg")
			(ty (name "AliasCfg")))
		(s-decl
			(p-ident (raw "alias_cfg"))
			(e-record
				(field (field "count")
					(e-int (raw "1")))))
		(s-type-anno (name "inline_cfg")
			(ty-record
				(anno-record-field (name "count")
					(ty (name "U8"))
					(default
						(e-int (raw "2"))))))
		(s-decl
			(p-ident (raw "inline_cfg"))
			(e-record
				(field (field "count")
					(e-int (raw "2")))))
		(s-type-decl
			(header (name "Nested")
				(args))
			(ty-record
				(anno-record-field (name "inner")
					(ty-record
						(anno-record-field (name "count")
							(ty (name "U8"))
							(default
								(e-int (raw "3"))))))))
		(s-type-anno (name "nested")
			(ty (name "Nested")))
		(s-decl
			(p-ident (raw "nested"))
			(e-nominal-record
				(mapper (e-tag (raw "Nested")))
				(backing (e-record
						(field (field "inner")
							(e-record
								(field (field "count")
									(e-int (raw "3")))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "alias_cfg"))
		(e-record
			(fields
				(field (name "count")
					(e-num (value "1")))))
		(annotation
			(ty-lookup (name "AliasCfg") (local))))
	(d-let
		(p-assign (ident "inline_cfg"))
		(e-record
			(fields
				(field (name "count")
					(e-num (value "2")))))
		(annotation
			(ty-record
				(field (field "count")
					(ty-lookup (name "U8") (builtin))))))
	(d-let
		(p-assign (ident "nested"))
		(e-nominal (nominal "Nested")
			(e-record
				(fields
					(field (name "inner")
						(e-record
							(fields
								(field (name "count")
									(e-num (value "3")))))))))
		(annotation
			(ty-lookup (name "Nested") (local))))
	(s-alias-decl
		(ty-header (name "AliasCfg"))
		(ty-record
			(field (field "count")
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Nested"))
		(ty-record
			(field (field "inner")
				(ty-record
					(field (field "count")
						(ty-lookup (name "U8") (builtin))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "AliasCfg"))
		(patt (type "{ count: U8 }"))
		(patt (type "Nested")))
	(type_decls
		(alias (type "AliasCfg")
			(ty-header (name "AliasCfg")))
		(nominal (type "Nested")
			(ty-header (name "Nested"))))
	(expressions
		(expr (type "AliasCfg"))
		(expr (type "{ count: U8 }"))
		(expr (type "Nested"))))
~~~
