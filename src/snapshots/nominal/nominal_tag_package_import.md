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
blue : CC.Color
blue = CC.Color.RGB(0,0,255)
~~~
# EXPECTED
MODULE NOT FOUND - nominal_tag_package_import.md:4:1:4:26
# PROBLEMS
**MODULE NOT FOUND**
The module `styles.Color` was not found in this Roc project.

You're attempting to use this module here:
**nominal_tag_package_import.md:4:1:4:26:**
```roc
import styles.Color as CC
```
^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),
KwImport(4:1-4:7),LowerIdent(4:8-4:14),NoSpaceDotUpperIdent(4:14-4:20),KwAs(4:21-4:23),UpperIdent(4:24-4:26),
LowerIdent(7:1-7:5),OpColon(7:6-7:7),UpperIdent(7:8-7:10),NoSpaceDotUpperIdent(7:10-7:16),
LowerIdent(8:1-8:5),OpAssign(8:6-8:7),UpperIdent(8:8-8:10),NoSpaceDotUpperIdent(8:10-8:16),NoSpaceDotUpperIdent(8:16-8:20),NoSpaceOpenRound(8:20-8:21),Int(8:21-8:22),Comma(8:22-8:23),Int(8:23-8:24),Comma(8:24-8:25),Int(8:25-8:28),CloseRound(8:28-8:29),EndOfFile(8:29-8:29),
~~~
# PARSE
~~~clojure
(file @1.1-8.29
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-lower-ident @1.9-1.13
				(text "blue"))))
	(statements
		(s-import @4.1-4.26 (raw "styles.Color") (alias "CC"))
		(s-type-anno @7.1-7.16 (name "blue")
			(ty @7.8-7.16 (name "CC.Color")))
		(s-decl @8.1-8.29
			(p-ident @8.1-8.5 (raw "blue"))
			(e-apply @8.8-8.29
				(e-tag @8.8-8.20 (raw "CC.Color.RGB"))
				(e-int @8.21-8.22 (raw "0"))
				(e-int @8.23-8.24 (raw "0"))
				(e-int @8.25-8.28 (raw "255"))))))
~~~
# FORMATTED
~~~roc
module [blue]

# import the Color module from styles package as CC
import styles.Color as CC

# instantiating an RGB nominal tab union from the styles.Color module
blue : CC.Color
blue = CC.Color.RGB(0, 0, 255)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @8.1-8.5 (ident "blue"))
		(e-nominal-external @8.8-8.20
			(module-idx "0")
			(target-node-idx "0")
			(e-tag @8.8-8.20 (name "RGB")
				(args
					(e-int @8.21-8.22 (value "0"))
					(e-int @8.23-8.24 (value "0"))
					(e-int @8.25-8.28 (value "255")))))
		(annotation @8.1-8.5
			(declared-type
				(ty-lookup-external @7.8-7.16
					(module-idx "0")
					(target-node-idx "0")))))
	(s-import @4.1-4.26 (module "styles.Color") (qualifier "styles") (alias "CC")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.5 (type "Error")))
	(expressions
		(expr @8.8-8.20 (type "Error"))))
~~~
