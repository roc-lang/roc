# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
Fli/main.roc" }

Pair(a, b+ : (
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - fuzz_crash_021.md:1:4:1:5
PARSE ERROR - fuzz_crash_021.md:1:5:1:9
PARSE ERROR - fuzz_crash_021.md:1:9:1:13
PARSE ERROR - fuzz_crash_021.md:1:13:1:14
PARSE ERROR - fuzz_crash_021.md:1:14:1:16
PARSE ERROR - fuzz_crash_021.md:1:16:1:16
PARSE ERROR - fuzz_crash_021.md:3:1:3:5
PARSE ERROR - fuzz_crash_021.md:4:1:4:1
MALFORMED TYPE - fuzz_crash_021.md:3:14:3:15
TYPE MODULE MISSING MATCHING TYPE - fuzz_crash_021.md:1:1:3:15
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

```roc
Fli/main.roc" }
```
            ^^^


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

**fuzz_crash_021.md:1:4:1:5:**
```roc
Fli/main.roc" }
```
   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:1:5:1:9:**
```roc
Fli/main.roc" }
```
    ^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:1:9:1:13:**
```roc
Fli/main.roc" }
```
        ^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:1:13:1:14:**
```roc
Fli/main.roc" }
```
            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:1:14:1:16:**
```roc
Fli/main.roc" }
```
             ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:1:16:1:16:**
```roc
Fli/main.roc" }
```
               ^


**PARSE ERROR**
A parsing error occurred: `expected_ty_anno_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:3:1:3:5:**
```roc
Pair(a, b+ : (
```
^^^^


**PARSE ERROR**
A parsing error occurred: `expected_ty_anno_close_round`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:4:1:4:1:**
```roc

```
^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**fuzz_crash_021.md:3:14:3:15:**
```roc
Pair(a, b+ : (
```
             ^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `fuzz_crash_021`.roc, but no top-level type declaration named `fuzz_crash_021` was found.

Add either:
`fuzz_crash_021 := ...` (nominal type)
or:
`fuzz_crash_021 : ...` (type alias)
**fuzz_crash_021.md:1:1:3:15:**
```roc
Fli/main.roc" }

Pair(a, b+ : (
```


# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpSlash(1:4-1:5),LowerIdent(1:5-1:9),NoSpaceDotLowerIdent(1:9-1:13),StringStart(1:13-1:14),StringPart(1:14-1:16),StringEnd(1:16-1:16),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),Comma(3:7-3:8),LowerIdent(3:9-3:10),OpPlus(3:10-3:11),OpColon(3:12-3:13),OpenRound(3:14-3:15),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.15
	(type-module @1.1-1.4)
	(statements
		(s-malformed @1.4-1.5 (tag "expected_colon_after_type_annotation"))
		(s-malformed @1.5-1.9 (tag "statement_unexpected_token"))
		(s-malformed @1.9-1.13 (tag "statement_unexpected_token"))
		(s-malformed @1.13-1.14 (tag "statement_unexpected_token"))
		(s-malformed @1.14-1.16 (tag "statement_unexpected_token"))
		(s-malformed @1.16-1.16 (tag "statement_unexpected_token"))
		(s-type-decl @3.1-3.15
			(header @3.1-3.11 (name "<malformed>")
				(args))
			(ty-malformed @3.14-3.15 (tag "expected_ty_anno_close_round")))))
~~~
# FORMATTED
~~~roc


<malformed> : 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @3.1-3.15
		(ty-header @3.1-3.11 (name ""))
		(ty-malformed @3.14-3.15)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @3.1-3.15 (type "")
			(ty-header @3.1-3.11 (name ""))))
	(expressions))
~~~
