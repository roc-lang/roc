# META
~~~ini
description=fuzz crash, unterminated single quote
type=file
~~~
# SOURCE
~~~roc
module [tus,r]

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
PARSE ERROR - fuzz_crash_032.md:3:24:3:25
PARSE ERROR - fuzz_crash_032.md:3:26:3:27
PARSE ERROR - fuzz_crash_032.md:3:34:3:35
PARSE ERROR - fuzz_crash_032.md:3:44:3:45
IMPORT MUST BE TOP LEVEL - fuzz_crash_032.md:6:18:6:24
UNEXPECTED TOKEN IN PATTERN - fuzz_crash_032.md:9:21:9:22
PARSE ERROR - fuzz_crash_032.md:9:22:9:22
UNDECLARED TYPE VARIABLE - fuzz_crash_032.md:3:14:3:17
UNDECLARED TYPE - fuzz_crash_032.md:3:21:3:24
NOT IMPLEMENTED - :0:0:0:0
UNDECLARED TYPE - fuzz_crash_032.md:6:25:6:30
EXPECTED NOMINAL TYPE - fuzz_crash_032.md:8:26:8:37
INVALID PATTERN - :0:0:0:0
UNDECLARED TYPE - fuzz_crash_032.md:10:3:10:4
EXPECTED NOMINAL TYPE - fuzz_crash_032.md:10:13:10:24
EXPOSED BUT NOT DEFINED - fuzz_crash_032.md:1:13:1:14
EXPOSED BUT NOT DEFINED - fuzz_crash_032.md:1:9:1:12
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_032.md:3:24:3:25:**
```roc
LocalStatus :lue => Loc= [Pending, Complete]
```
                       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_032.md:3:26:3:27:**
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

Here is the problematic code:
**fuzz_crash_032.md:3:34:3:35:**
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

Here is the problematic code:
**fuzz_crash_032.md:3:44:3:45:**
```roc
LocalStatus :lue => Loc= [Pending, Complete]
```
                                           ^


**IMPORT MUST BE TOP LEVEL**
Import statements must appear at the top level of a module.
Move this import to the top of the file, after the module header but before any definitions.

Here is the problematic code:
**fuzz_crash_032.md:6:18:6:24:**
```roc
olor = |color| { import Color.RGB
```
                 ^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **-** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**fuzz_crash_032.md:9:21:9:22:**
```roc
Green => LocalStatus-Complete
```
                    ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_032.md:9:22:9:22:**
```roc
Green => LocalStatus-Complete
```
                     ^


**UNDECLARED TYPE VARIABLE**
The type variable _lue_ is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**fuzz_crash_032.md:3:14:3:17:**
```roc
LocalStatus :lue => Loc= [Pending, Complete]
```
             ^^^


**UNDECLARED TYPE**
The type _Loc_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_032.md:3:21:3:24:**
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
**fuzz_crash_032.md:6:25:6:30:**
```roc
olor = |color| { import Color.RGB
```
                        ^^^^^


**EXPECTED NOMINAL TYPE**
You are using the type _LocalStatus_ like a nominal type, but it is an alias.

This type is referenced here:
**fuzz_crash_032.md:8:26:8:37:**
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
**fuzz_crash_032.md:10:3:10:4:**
```roc
  B.Blue => LocalStatus.Pending
```
  ^


**EXPECTED NOMINAL TYPE**
You are using the type _LocalStatus_ like a nominal type, but it is an alias.

This type is referenced here:
**fuzz_crash_032.md:10:13:10:24:**
```roc
  B.Blue => LocalStatus.Pending
```
            ^^^^^^^^^^^


**Hint:** You can declare this type with `:=` to make it nominal.

**EXPOSED BUT NOT DEFINED**
The module header says that `r` is exposed, but it is not defined anywhere in this module.

**fuzz_crash_032.md:1:13:1:14:**
```roc
module [tus,r]
```
            ^
You can fix this by either defining `r` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `tus` is exposed, but it is not defined anywhere in this module.

**fuzz_crash_032.md:1:9:1:12:**
```roc
module [tus,r]
```
        ^^^
You can fix this by either defining `tus` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),Comma(1:12-1:13),LowerIdent(1:13-1:14),CloseSquare(1:14-1:15),
UpperIdent(3:1-3:12),OpColon(3:13-3:14),LowerIdent(3:14-3:17),OpFatArrow(3:18-3:20),UpperIdent(3:21-3:24),OpAssign(3:24-3:25),OpenSquare(3:26-3:27),UpperIdent(3:27-3:34),Comma(3:34-3:35),UpperIdent(3:36-3:44),CloseSquare(3:44-3:45),
LowerIdent(5:1-5:5),OpColon(5:6-5:7),Underscore(5:8-5:9),OpArrow(5:10-5:12),LowerIdent(5:13-5:16),
LowerIdent(6:1-6:5),OpAssign(6:6-6:7),OpBar(6:8-6:9),LowerIdent(6:9-6:14),OpBar(6:14-6:15),OpenCurly(6:16-6:17),KwImport(6:18-6:24),UpperIdent(6:25-6:30),NoSpaceDotUpperIdent(6:30-6:34),
KwMatch(8:5-8:10),LowerIdent(8:11-8:16),OpenCurly(8:17-8:18),UpperIdent(8:19-8:22),OpFatArrow(8:23-8:25),UpperIdent(8:26-8:37),NoSpaceDotUpperIdent(8:37-8:45),
UpperIdent(9:1-9:6),OpFatArrow(9:7-9:9),UpperIdent(9:10-9:21),OpUnaryMinus(9:21-9:22),UpperIdent(9:22-9:30),
UpperIdent(10:3-10:4),NoSpaceDotUpperIdent(10:4-10:9),OpFatArrow(10:10-10:12),UpperIdent(10:13-10:24),NoSpaceDotUpperIdent(10:24-10:32),
CloseCurly(11:5-11:6),
CloseCurly(12:1-12:2),EndOfFile(12:2-12:2),
~~~
# PARSE
~~~clojure
(file @1.1-12.2
	(module @1.1-1.15
		(exposes @1.8-1.15
			(exposed-lower-ident @1.9-1.12
				(text "tus"))
			(exposed-lower-ident @1.13-1.14
				(text "r"))))
	(statements
		(s-type-decl @3.1-3.24
			(header @3.1-3.12 (name "LocalStatus")
				(args))
			(ty-fn @3.14-3.24
				(ty-var @3.14-3.17 (raw "lue"))
				(ty @3.21-3.24 (name "Loc"))))
		(s-malformed @3.24-3.25 (tag "statement_unexpected_token"))
		(s-malformed @3.26-3.27 (tag "statement_unexpected_token"))
		(s-malformed @3.34-3.35 (tag "expected_colon_after_type_annotation"))
		(s-malformed @3.44-3.45 (tag "expected_colon_after_type_annotation"))
		(s-type-anno @5.1-5.16 (name "olor")
			(ty-fn @5.8-5.16
				(_)
				(ty-var @5.13-5.16 (raw "tus"))))
		(s-decl @6.1-12.2
			(p-ident @6.1-6.5 (raw "olor"))
			(e-lambda @6.8-12.2
				(args
					(p-ident @6.9-6.14 (raw "color")))
				(e-block @6.16-12.2
					(statements
						(s-malformed @6.18-6.24 (tag "import_must_be_top_level"))
						(e-tag @6.25-6.34 (raw "Color.RGB"))
						(e-match
							(e-ident @8.11-8.16 (raw "color"))
							(branches
								(branch @8.19-8.45
									(p-tag @8.19-8.22 (raw "RGB"))
									(e-tag @8.26-8.45 (raw "LocalStatus.Pending")))
								(branch @9.1-9.21
									(p-tag @9.1-9.6 (raw "Green"))
									(e-tag @9.10-9.21 (raw "LocalStatus")))
								(branch @9.21-9.30
									(p-malformed @9.21-9.22 (tag "pattern_unexpected_token"))
									(e-tag @9.22-9.30 (raw "Complete")))
								(branch @10.3-10.32
									(p-tag @10.3-10.9 (raw ".Blue"))
									(e-tag @10.13-10.32 (raw "LocalStatus.Pending")))))))))))
~~~
# FORMATTED
~~~roc
module [tus, r]

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
		(p-assign @6.1-6.5 (ident "olor"))
		(e-lambda @6.8-12.2
			(args
				(p-assign @6.9-6.14 (ident "color")))
			(e-block @6.16-12.2
				(s-expr @6.25-6.34
					(e-runtime-error (tag "undeclared_type")))
				(e-match @8.5-11.6
					(match @8.5-11.6
						(cond
							(e-lookup-local @8.11-8.16
								(p-assign @6.9-6.14 (ident "color"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @8.19-8.22)))
								(value
									(e-runtime-error (tag "type_alias_but_needed_nominal"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @9.1-9.6)))
								(value
									(e-tag @9.10-9.21 (name "LocalStatus"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error @9.21-9.22 (tag "pattern_not_canonicalized"))))
								(value
									(e-tag @9.22-9.30 (name "Complete"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error @10.3-10.4 (tag "undeclared_type"))))
								(value
									(e-runtime-error (tag "type_alias_but_needed_nominal")))))))))
		(annotation @6.1-6.5
			(declared-type
				(ty-fn @5.8-5.16 (effectful false)
					(ty-underscore @1.1-1.1)
					(ty-var @5.13-5.16 (name "tus"))))))
	(s-alias-decl @3.1-3.24
		(ty-header @3.1-3.12 (name "LocalStatus"))
		(ty-fn @3.14-3.24 (effectful true)
			(ty-malformed @3.14-3.17)
			(ty @3.21-3.24 (name "Loc")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.5 (type "Error -> Error")))
	(type_decls
		(alias @3.1-3.24 (type "LocalStatus")
			(ty-header @3.1-3.12 (name "LocalStatus"))))
	(expressions
		(expr @6.8-12.2 (type "Error -> Error"))))
~~~
