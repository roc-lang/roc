# META
~~~ini
description=Type shadowing across scopes should produce warning
type=snippet
~~~
# SOURCE
~~~roc
Try(a, b) : [Ok(a), Err(b)]

processData : Str -> Str
processData = |data|
    "processed"

# In a nested mod scope, redeclare Try
InnerMod : {
    Try : [Success, Failure]
}
~~~
# EXPECTED
EXPECTED TYPE FIELD - type_shadowing_across_scopes.md:9:5:9:8
EXPECTED RECORD TYPE SEPARATOR - type_shadowing_across_scopes.md:9:21:9:28
UNEXPECTED STATEMENT - type_shadowing_across_scopes.md:9:28:9:29
UNEXPECTED STATEMENT - type_shadowing_across_scopes.md:10:1:10:2
BUILTIN TYPE SHADOWED - type_shadowing_across_scopes.md:1:1:1:28
UNUSED VARIABLE - type_shadowing_across_scopes.md:4:16:4:20
MALFORMED TYPE - type_shadowing_across_scopes.md:9:21:9:28
# PROBLEMS

┌─────────────────────┐
│ EXPECTED TYPE FIELD ├─ I was parsing a record type, and I expected a ───────┐
└┬────────────────────┘  field name.                                          │
 │                                                                            │
 │  Try : [Success, Failure]                                                  │
 │  ‾‾‾                                                                       │
 └─────────────────────────────────────── type_shadowing_across_scopes.md:9:5 ┘

    Record type fields start with lowercase names, `_`, or named underscores,
    followed by `:` and the field type.

    For example:
        { name : Str, age : U64 }

    I found `Try` here.
    Names that start with uppercase letters are used for tags, type names, and
    mod names in Roc.


┌────────────────────────────────┐
│ EXPECTED RECORD TYPE SEPARATOR ├─ I was parsing a record type, and I ───────┐
└┬───────────────────────────────┘  expected `,` or `}`.                      │
 │                                                                            │
 │  Try : [Success, Failure]                                                  │
 │                  ‾‾‾‾‾‾‾                                                   │
 └────────────────────────────────────── type_shadowing_across_scopes.md:9:21 ┘

    Separate record type fields with commas and close the record type with `}`.

    For example:
        { name : Str, age : U64 }

    I found `Failure` here.
    Names that start with uppercase letters are used for tags, type names, and
    mod names in Roc.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  Try : [Success, Failure]                                                  │
 │                         ‾                                                  │
 └────────────────────────────────────── type_shadowing_across_scopes.md:9:28 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `]` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  }                                                                         │
 │  ‾                                                                         │
 └────────────────────────────────────── type_shadowing_across_scopes.md:10:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `}` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌───────────────────────┐
│ BUILTIN TYPE SHADOWED ├─ The type `Try` shadows a builtin type. ────────────┐
└┬──────────────────────┘                                                     │
 │                                                                            │
 │  Try(a, b) : [Ok(a), Err(b)]                                               │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                               │
 └─────────────────────────────────────── type_shadowing_across_scopes.md:1:1 ┘

    This may make the builtin type inaccessible in this scope.

    The new declaration is here:


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `data` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  processData = |data|                                                      │
 │                 ‾‾‾‾                                                       │
 └────────────────────────────────────── type_shadowing_across_scopes.md:4:16 ┘

    If you don't need this variable, prefix it with an underscore like `_data`
    to suppress this warning.


┌────────────────┐
│ MALFORMED TYPE ├─ This type annotation is malformed or contains invalid ────┐
└┬───────────────┘  syntax.                                                   │
 │                                                                            │
 │  Try : [Success, Failure]                                                  │
 │                  ‾‾‾‾‾‾‾                                                   │
 └────────────────────────────────────── type_shadowing_across_scopes.md:9:21 ┘


# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
StringStart,StringPart,StringEnd,
UpperIdent,OpColon,OpenCurly,
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Try")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Ok"))
						(ty-var (raw "a")))
					(ty-apply
						(ty (name "Err"))
						(ty-var (raw "b"))))))
		(s-type-anno (name "processData")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "processData"))
			(e-lambda
				(args
					(p-ident (raw "data")))
				(e-string
					(e-string-part (raw "processed")))))
		(s-type-decl
			(header (name "InnerMod")
				(args))
			(ty-malformed (tag "expected_ty_close_curly_or_comma")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
Try(a, b) : [Ok(a), Err(b)]

processData : Str -> Str
processData = |data|
	"processed"

# In a nested mod scope, redeclare Try
InnerMod : 

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "processData"))
		(e-lambda
			(args
				(p-assign (ident "data")))
			(e-string
				(e-literal (string "processed"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(s-alias-decl
		(ty-header (name "Try")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-tag-union
			(ty-tag-name (name "Ok")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-tag-name (name "Err")
				(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
	(s-alias-decl
		(ty-header (name "InnerMod"))
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Str")))
	(type_decls
		(alias (type "Try(a, b)")
			(ty-header (name "Try")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "InnerMod")
			(ty-header (name "InnerMod"))))
	(expressions
		(expr (type "Str -> Str"))))
~~~
