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
UpperIdent(1:1-1:12),OpColon(1:13-1:14),LowerIdent(1:14-1:17),OpFatArrow(1:18-1:20),UpperIdent(1:21-1:24),OpAssign(1:24-1:25),OpenSquare(1:26-1:27),UpperIdent(1:27-1:34),Comma(1:34-1:35),UpperIdent(1:36-1:44),CloseSquare(1:44-1:45),
LowerIdent(3:1-3:5),OpColon(3:6-3:7),Underscore(3:8-3:9),OpArrow(3:10-3:12),LowerIdent(3:13-3:16),
LowerIdent(4:1-4:5),OpAssign(4:6-4:7),OpBar(4:8-4:9),LowerIdent(4:9-4:14),OpBar(4:14-4:15),OpenCurly(4:16-4:17),KwImport(4:18-4:24),UpperIdent(4:25-4:30),NoSpaceDotUpperIdent(4:30-4:34),
KwMatch(6:5-6:10),LowerIdent(6:11-6:16),OpenCurly(6:17-6:18),UpperIdent(6:19-6:22),OpFatArrow(6:23-6:25),UpperIdent(6:26-6:37),NoSpaceDotUpperIdent(6:37-6:45),
UpperIdent(7:1-7:6),OpFatArrow(7:7-7:9),UpperIdent(7:10-7:21),OpUnaryMinus(7:21-7:22),UpperIdent(7:22-7:30),
UpperIdent(8:3-8:4),NoSpaceDotUpperIdent(8:4-8:9),OpFatArrow(8:10-8:12),UpperIdent(8:13-8:24),NoSpaceDotUpperIdent(8:24-8:32),
CloseCurly(9:5-9:6),
CloseCurly(10:1-10:2),
EndOfFile(11:1-11:1),
~~~
# PARSE
~~~clojure
(file @1.1-10.2
	(type-module @1.1-1.12)
	(statements
		(s-type-decl @1.1-1.24
			(header @1.1-1.12 (name "LocalStatus")
				(args))
			(ty-fn @1.14-1.24
				(ty-var @1.14-1.17 (raw "lue"))
				(ty @1.21-1.24 (name "Loc"))))
		(s-malformed @1.24-1.25 (tag "statement_unexpected_token"))
		(s-malformed @1.26-1.27 (tag "statement_unexpected_token"))
		(s-malformed @1.34-1.35 (tag "expected_colon_after_type_annotation"))
		(s-malformed @1.44-1.45 (tag "expected_colon_after_type_annotation"))
		(s-type-anno @3.1-3.16 (name "olor")
			(ty-fn @3.8-3.16
				(_)
				(ty-var @3.13-3.16 (raw "tus"))))
		(s-decl @4.1-10.2
			(p-ident @4.1-4.5 (raw "olor"))
			(e-lambda @4.8-10.2
				(args
					(p-ident @4.9-4.14 (raw "color")))
				(e-block @4.16-10.2
					(statements
						(s-malformed @4.18-4.24 (tag "import_must_be_top_level"))
						(e-tag @4.25-4.34 (raw "Color.RGB"))
						(e-match
							(e-ident @6.11-6.16 (raw "color"))
							(branches
								(branch @6.19-6.45
									(p-tag @6.19-6.22 (raw "RGB"))
									(e-tag @6.26-6.45 (raw "LocalStatus.Pending")))
								(branch @7.1-7.21
									(p-tag @7.1-7.6 (raw "Green"))
									(e-tag @7.10-7.21 (raw "LocalStatus")))
								(branch @7.21-7.30
									(p-malformed @7.21-7.22 (tag "pattern_unexpected_token"))
									(e-tag @7.22-7.30 (raw "Complete")))
								(branch @8.3-8.32
									(p-tag @8.3-8.9 (raw ".Blue"))
									(e-tag @8.13-8.32 (raw "LocalStatus.Pending")))))))))))
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
		(p-assign @4.1-4.5 (ident "olor"))
		(e-lambda @4.8-10.2
			(args
				(p-assign @4.9-4.14 (ident "color")))
			(e-block @4.16-10.2
				(s-runtime-error (tag "not_implemented"))
				(s-expr @4.25-4.34
					(e-runtime-error (tag "undeclared_type")))
				(e-match @6.5-9.6
					(match @6.5-9.6
						(cond
							(e-lookup-local @6.11-6.16
								(p-assign @4.9-4.14 (ident "color"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @6.19-6.22)))
								(value
									(e-runtime-error (tag "type_alias_but_needed_nominal"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @7.1-7.6)))
								(value
									(e-tag @7.10-7.21 (name "LocalStatus"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error @7.21-7.22 (tag "pattern_not_canonicalized"))))
								(value
									(e-tag @7.22-7.30 (name "Complete"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error @8.3-8.4 (tag "undeclared_type"))))
								(value
									(e-runtime-error (tag "type_alias_but_needed_nominal")))))))))
		(annotation @4.1-4.5
			(declared-type
				(ty-fn @3.8-3.16 (effectful false)
					(ty-underscore @1.1-1.1)
					(ty-rigid-var @3.13-3.16 (name "tus"))))))
	(s-alias-decl @1.1-1.24
		(ty-header @1.1-1.12 (name "LocalStatus"))
		(ty-fn @1.14-1.24 (effectful true)
			(ty-malformed @1.14-1.17)
			(ty-malformed @1.21-1.24))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.5 (type "_arg -> Error")))
	(type_decls
		(alias @1.1-1.24 (type "LocalStatus")
			(ty-header @1.1-1.12 (name "LocalStatus"))))
	(expressions
		(expr @4.8-10.2 (type "_arg -> Error"))))
~~~
