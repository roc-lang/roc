# META
~~~ini
description=Function type annotation returning record
type=statement
~~~
# SOURCE
~~~roc
create_user! : Str, U32 => { name : Str, age : U32, id : U64, active : Bool }
~~~
# EXPECTED
UNDECLARED TYPE - type_function_return_record.md:1:72:1:76
# PROBLEMS
**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**type_function_return_record.md:1:72:1:76:**
```roc
create_user! : Str, U32 => { name : Str, age : U32, id : U64, active : Bool }
```
                                                                       ^^^^


# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpFatArrow,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-type-anno (name "create_user!")
	(ty-fn
		(ty (name "Str"))
		(ty (name "U32"))
		(ty-record
			(anno-record-field (name "name")
				(ty (name "Str")))
			(anno-record-field (name "age")
				(ty (name "U32")))
			(anno-record-field (name "id")
				(ty (name "U64")))
			(anno-record-field (name "active")
				(ty (name "Bool"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
