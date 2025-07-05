# META
~~~ini
description=Example of a nominal tag union import from a package
type=file
~~~
# SOURCE
~~~roc
module [blue]

# import the Color module from styles package as CC
import styles.Color as CC

# instantiating an RGB nominal tab union from the styles.Color module
blue : CC.RGB
blue = CC.RGB(0,0,255)
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - nominal_tag_package_import.md:9:1:9:3
UNEXPECTED TOKEN IN EXPRESSION - nominal_tag_package_import.md:9:2:9:4
UNEXPECTED TOKEN IN EXPRESSION - nominal_tag_package_import.md:9:3:9:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_package_import.md:9:1:9:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_package_import.md:9:2:9:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_package_import.md:9:3:9:4:**
```roc
~~~
```
  ^


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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:52),
KwImport(4:1-4:7),LowerIdent(4:8-4:14),NoSpaceDotUpperIdent(4:14-4:20),KwAs(4:21-4:23),UpperIdent(4:24-4:26),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(6:2-6:70),
LowerIdent(7:1-7:5),OpColon(7:6-7:7),UpperIdent(7:8-7:10),NoSpaceDotUpperIdent(7:10-7:14),Newline(1:1-1:1),
LowerIdent(8:1-8:5),OpAssign(8:6-8:7),UpperIdent(8:8-8:10),NoSpaceDotUpperIdent(8:10-8:14),NoSpaceOpenRound(8:14-8:15),Int(8:15-8:16),Comma(8:16-8:17),Int(8:17-8:18),Comma(8:18-8:19),Int(8:19-8:22),CloseRound(8:22-8:23),Newline(1:1-1:1),
MalformedUnknownToken(9:1-9:2),MalformedUnknownToken(9:2-9:3),MalformedUnknownToken(9:3-9:4),EndOfFile(9:4-9:4),
~~~
# PARSE
~~~clojure
(file @1.1-9.4
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-lower-ident (text "blue"))))
	(statements
		(s-import @4.1-4.26 (raw "styles.Color") (alias "CC"))
		(s-type-anno @1.1-1.1 (name "blue")
			(ty @7.8-7.14 (name "CC.RGB")))
		(s-decl @8.1-8.23
			(p-ident @8.1-8.5 (raw "blue"))
			(e-apply @8.8-8.23
				(e-tag @8.8-8.14 (raw "CC.RGB"))
				(e-int @8.15-8.16 (raw "0"))
				(e-int @8.17-8.18 (raw "0"))
				(e-int @8.19-8.22 (raw "255"))))
		(e-malformed @9.1-9.3 (reason "expr_unexpected_token"))
		(e-malformed @9.2-9.4 (reason "expr_unexpected_token"))
		(e-malformed @9.3-9.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [blue]

# import the Color module from styles package as CC
import styles.Color as CC

# instantiating an RGB nominal tab union from the styles.Color module
blue : CC.RGB
blue = RGB(0, 0, 255)

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @8.1-8.5 (ident "blue"))
		(e-tag @8.8-8.23 (name "RGB")
			(args
				(e-int @8.15-8.16 (value "0"))
				(e-int @8.17-8.18 (value "0"))
				(e-int @8.19-8.22 (value "255"))))
		(annotation @8.1-8.5
			(declared-type
				(ty-lookup-external @7.8-7.14
					(ext-decl @7.8-7.14 (ident "CC.RGB") (kind "type"))))))
	(s-import @4.1-4.26 (module "styles.Color") (qualifier "styles") (alias "CC")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.5 (type "CC.RGB")))
	(expressions
		(expr @8.8-8.23 (type "CC.RGB"))))
~~~
