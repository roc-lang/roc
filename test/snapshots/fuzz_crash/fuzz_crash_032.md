# META
~~~ini
description=fuzz crash, unterminated single quote
type=snippet
~~~
# SOURCE
~~~roc
LocalStatus :lue => Loc= [Pending, Complete]

olor : _ -> tus
olor = |color| { import Color.RGB

    match color { RGB => LocalStatus.Pending
Green => LocalStatus-Complete
  B.Blue => LocalStatus.Pending
    }
}
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_032.md:1:24:1:25
PARSE ERROR - fuzz_crash_032.md:1:26:1:27
PARSE ERROR - fuzz_crash_032.md:1:34:1:35
PARSE ERROR - fuzz_crash_032.md:1:44:1:45
IMPORT MUST BE TOP LEVEL - fuzz_crash_032.md:4:18:4:24
UNEXPECTED TOKEN IN PATTERN - fuzz_crash_032.md:7:21:7:22
PARSE ERROR - fuzz_crash_032.md:7:22:7:22
UNDECLARED TYPE VARIABLE - fuzz_crash_032.md:1:14:1:17
UNDECLARED TYPE - fuzz_crash_032.md:1:21:1:24
NOT IMPLEMENTED - :0:0:0:0
UNDECLARED TYPE - fuzz_crash_032.md:4:25:4:30
EXPECTED NOMINAL TYPE - fuzz_crash_032.md:6:26:6:37
INVALID PATTERN - :0:0:0:0
UNDECLARED TYPE - fuzz_crash_032.md:8:3:8:4
EXPECTED NOMINAL TYPE - fuzz_crash_032.md:8:13:8:24
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_032.md:1:24:1:25:**
```roc
LocalStatus :lue => Loc= [Pending, Complete]
```
                       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_032.md:1:26:1:27:**
```roc
LocalStatus :lue => Loc= [Pending, Complete]
```
                         ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**fuzz_crash_032.md:1:34:1:35:**
```roc
LocalStatus :lue => Loc= [Pending, Complete]
```
                                 ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**fuzz_crash_032.md:1:44:1:45:**
```roc
LocalStatus :lue => Loc= [Pending, Complete]
```
                                           ^


**IMPORT MUST BE TOP LEVEL**
Import statements must appear at the top level of a module.
Move this import to the top of the file, after the module header but before any definitions.

**fuzz_crash_032.md:4:18:4:24:**
```roc
olor = |color| { import Color.RGB
```
                 ^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **-** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**fuzz_crash_032.md:7:21:7:22:**
```roc
Green => LocalStatus-Complete
```
                    ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_032.md:7:22:7:22:**
```roc
Green => LocalStatus-Complete
```
                     ^


**UNDECLARED TYPE VARIABLE**
The type variable _lue_ is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**fuzz_crash_032.md:1:14:1:17:**
```roc
LocalStatus :lue => Loc= [Pending, Complete]
```
             ^^^


**UNDECLARED TYPE**
The type _Loc_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_032.md:1:21:1:24:**
```roc
LocalStatus :lue => Loc= [Pending, Complete]
```
                    ^^^


**NOT IMPLEMENTED**
This feature is not yet implemented: statement type in block

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNDECLARED TYPE**
The type _Color_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_032.md:4:25:4:30:**
```roc
olor = |color| { import Color.RGB
```
                        ^^^^^


**EXPECTED NOMINAL TYPE**
You are using the type _LocalStatus_ like a nominal type, but it is an alias.

This type is referenced here:
**fuzz_crash_032.md:6:26:6:37:**
```roc
    match color { RGB => LocalStatus.Pending
```
                         ^^^^^^^^^^^


**Hint:** You can declare this type with `:=` to make it nominal.

**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNDECLARED TYPE**
The type _B_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_032.md:8:3:8:4:**
```roc
  B.Blue => LocalStatus.Pending
```
  ^


**EXPECTED NOMINAL TYPE**
You are using the type _LocalStatus_ like a nominal type, but it is an alias.

This type is referenced here:
**fuzz_crash_032.md:8:13:8:24:**
```roc
  B.Blue => LocalStatus.Pending
```
            ^^^^^^^^^^^


**Hint:** You can declare this type with `:=` to make it nominal.

# TOKENS
~~~zig
UpperIdent,OpColon,LowerIdent,OpFatArrow,UpperIdent,OpAssign,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,Underscore,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,KwImport,UpperIdent,NoSpaceDotUpperIdent,
KwMatch,LowerIdent,OpenCurly,UpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,OpFatArrow,UpperIdent,OpUnaryMinus,UpperIdent,
UpperIdent,NoSpaceDotUpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "LocalStatus")
				(args))
			(ty-fn
				(ty-var (raw "lue"))
				(ty (name "Loc"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-type-anno (name "olor")
			(ty-fn
				(_)
				(ty-var (raw "tus"))))
		(s-decl
			(p-ident (raw "olor"))
			(e-lambda
				(args
					(p-ident (raw "color")))
				(e-block
					(statements
						(s-malformed (tag "import_must_be_top_level"))
						(e-tag (raw "Color.RGB"))
						(e-match
							(e-ident (raw "color"))
							(branches
								(branch
									(p-tag (raw "RGB"))
									(e-tag (raw "LocalStatus.Pending")))
								(branch
									(p-tag (raw "Green"))
									(e-tag (raw "LocalStatus")))
								(branch
									(p-malformed (tag "pattern_unexpected_token"))
									(e-tag (raw "Complete")))
								(branch
									(p-tag (raw ".Blue"))
									(e-tag (raw "LocalStatus.Pending")))))))))))
~~~
# FORMATTED
~~~roc
LocalStatus : lue => Loc


olor : _ -> tus
olor = |color| {
		Color.RGB

	match color {
		RGB => LocalStatus.Pending
		Green => LocalStatus
		 => Complete
		B.Blue => LocalStatus.Pending
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "olor"))
		(e-lambda
			(args
				(p-assign (ident "color")))
			(e-block
				(s-runtime-error (tag "not_implemented"))
				(s-expr
					(e-runtime-error (tag "undeclared_type")))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "color"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-runtime-error (tag "type_alias_but_needed_nominal"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-tag (name "LocalStatus"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error (tag "pattern_not_canonicalized"))))
								(value
									(e-tag (name "Complete"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error (tag "undeclared_type"))))
								(value
									(e-runtime-error (tag "type_alias_but_needed_nominal")))))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-underscore)
					(ty-rigid-var (name "tus"))))))
	(s-alias-decl
		(ty-header (name "LocalStatus"))
		(ty-fn (effectful true)
			(ty-malformed)
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> Error")))
	(type_decls
		(alias (type "LocalStatus")
			(ty-header (name "LocalStatus"))))
	(expressions
		(expr (type "_arg -> Error"))))
~~~
