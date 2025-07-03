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
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.RGB(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_package_import.md:8:10:8:15:**
```roc
blue = CC.RGB(0,0,255)
```
         ^^^^^


**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
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
LowerIdent(8:1-8:5),OpAssign(8:6-8:7),UpperIdent(8:8-8:10),NoSpaceDotUpperIdent(8:10-8:14),NoSpaceOpenRound(8:14-8:15),Int(8:15-8:16),Comma(8:16-8:17),Int(8:17-8:18),Comma(8:18-8:19),Int(8:19-8:22),CloseRound(8:22-8:23),EndOfFile(8:23-8:23),
~~~
# PARSE
~~~clojure
(file @1.1-8.23
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-lower-ident (text "blue"))))
	(statements
		(s-import @4.1-4.26 (module ".Color") (qualifier "styles") (alias "CC"))
		(s-type-anno @7.1-8.5 (name "blue")
			(ty-mod (module "RGB") (name "CC")))
		(s-decl @8.1-8.10
			(p-ident @8.1-8.5 (raw "blue"))
			(e-tag @8.8-8.10 (raw "CC")))
		(e-malformed @8.10-8.15 (reason "expr_unexpected_token"))
		(e-tuple @8.14-8.23
			(e-int @8.15-8.16 (raw "0"))
			(e-int @8.17-8.18 (raw "0"))
			(e-int @8.19-8.22 (raw "255")))))
~~~
# FORMATTED
~~~roc
module [blue]

# import the Color module from styles package as CC
import styles.Color as CC

# instantiating an RGB nominal tab union from the styles.Color module
blue : CC.RGB
blue = CC(0, 0, 255)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @8.1-8.5 (ident "blue"))
		(e-tag @8.8-8.10 (name "CC") (args "TODO"))
		(annotation @8.1-8.5
			(declared-type
				(ty-mod @7.8-7.14 (module "RGB") (type "CC")))))
	(s-import @4.1-4.26 (module "styles.Color") (qualifier "styles") (alias "CC")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.5 (type "[CC]a")))
	(expressions
		(expr @8.8-8.10 (type "[CC]a"))))
~~~
