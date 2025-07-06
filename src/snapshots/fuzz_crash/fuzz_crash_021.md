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
UNCLOSED STRING - fuzz_crash_021.md:1:1:1:5
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_021.md:1:4:1:9
PARSE ERROR - fuzz_crash_021.md:3:1:3:6
PARSE ERROR - fuzz_crash_021.md:3:15:3:15
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_021.md:1:1:1:5:**
```roc
Fli/main.roc" }
```
^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **/main** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_021.md:1:4:1:9:**
```roc
Fli/main.roc" }
```
   ^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_ty_anno_end`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_021.md:3:1:3:6:**
```roc
Pair(a, b+ : (
```
^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_ty_anno_end`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_021.md:3:15:3:15:**
```roc
Pair(a, b+ : (
```
              


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpSlash(1:4-1:5),LowerIdent(1:5-1:9),NoSpaceDotLowerIdent(1:9-1:13),StringStart(1:13-1:14),StringPart(1:14-1:16),StringEnd(1:16-1:16),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),Comma(3:7-3:8),LowerIdent(3:9-3:10),OpPlus(3:10-3:11),OpColon(3:12-3:13),OpenRound(3:14-3:15),EndOfFile(3:15-3:15),
~~~
# PARSE
~~~clojure
(file @1.1-3.15
	(malformed-header @1.1-1.5 (tag "missing_header"))
	(statements
		(e-malformed @1.4-1.9 (reason "expr_unexpected_token"))
		(e-field-access @1.5-1.14
			(e-ident @1.5-1.9 (raw "main"))
			(e-ident @1.9-1.13 (raw "roc")))
		(e-string @1.13-1.16
			(e-string-part @1.14-1.16 (raw " }")))
		(s-type-decl @3.1-3.15
			(header @3.1-3.13 (name "<malformed>")
				(args))
			(ty-malformed @3.14-3.15 (tag "expected_ty_anno_end")))))
~~~
# FORMATTED
~~~roc
main.roc" }"

<malformed> : 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @3.1-3.15
		(ty-header @3.1-3.13 (name "Fli"))
		(ty-malformed @3.14-3.15)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @3.1-3.15 (type "Fli")
			(ty-header @3.1-3.13 (name "Fli"))))
	(expressions))
~~~
