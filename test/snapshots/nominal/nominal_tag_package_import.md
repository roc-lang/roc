# META
~~~ini
description=Example of a nominal tag union import from a package
type=snippet
~~~
# SOURCE
~~~roc
# import the Color module from styles package as CC
import styles.Color as CC

# instantiating an RGB nominal tab union from the styles.Color module
blue : CC.Color
blue = CC.Color.RGB(0,0,255)
~~~
# EXPECTED
MODULE NOT FOUND - nominal_tag_package_import.md:2:1:2:26
# PROBLEMS
**MODULE NOT FOUND**
The module `styles.Color` was not found in this Roc project.

You're attempting to use this module here:
**nominal_tag_package_import.md:2:1:2:26:**
```roc
import styles.Color as CC
```
^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport(2:1-2:7),LowerIdent(2:8-2:14),NoSpaceDotUpperIdent(2:14-2:20),KwAs(2:21-2:23),UpperIdent(2:24-2:26),
LowerIdent(5:1-5:5),OpColon(5:6-5:7),UpperIdent(5:8-5:10),NoSpaceDotUpperIdent(5:10-5:16),
LowerIdent(6:1-6:5),OpAssign(6:6-6:7),UpperIdent(6:8-6:10),NoSpaceDotUpperIdent(6:10-6:16),NoSpaceDotUpperIdent(6:16-6:20),NoSpaceOpenRound(6:20-6:21),Int(6:21-6:22),Comma(6:22-6:23),Int(6:23-6:24),Comma(6:24-6:25),Int(6:25-6:28),CloseRound(6:28-6:29),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @2.1-6.29
	(type-module @2.1-2.7)
	(statements
		(s-import @2.1-2.26 (raw "styles.Color") (alias "CC"))
		(s-type-anno @5.1-5.16 (name "blue")
			(ty @5.8-5.16 (name "CC.Color")))
		(s-decl @6.1-6.29
			(p-ident @6.1-6.5 (raw "blue"))
			(e-apply @6.8-6.29
				(e-tag @6.8-6.20 (raw "CC.Color.RGB"))
				(e-int @6.21-6.22 (raw "0"))
				(e-int @6.23-6.24 (raw "0"))
				(e-int @6.25-6.28 (raw "255"))))))
~~~
# FORMATTED
~~~roc
# import the Color module from styles package as CC
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
		(p-assign @6.1-6.5 (ident "blue"))
		(e-nominal-external @6.8-6.29
			(module-idx "2")
			(target-node-idx "0")
			(e-tag @6.8-6.29 (name "RGB")
				(args
					(e-num @6.21-6.22 (value "0"))
					(e-num @6.23-6.24 (value "0"))
					(e-num @6.25-6.28 (value "255")))))
		(annotation @6.1-6.5
			(declared-type
				(ty-lookup @5.8-5.16 (name "Color") (external (module-idx "2") (target-node-idx "0"))))))
	(s-import @2.1-2.26 (module "styles.Color") (qualifier "styles") (alias "CC")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.5 (type "Error")))
	(expressions
		(expr @6.8-6.29 (type "Error"))))
~~~
