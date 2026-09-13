# META
~~~ini
description=Bad record accessor syntax reports a targeted parse error
type=expr
~~~
# SOURCE
~~~roc
person.@
~~~
# EXPECTED
EXPECTED RECORD ACCESSOR - error_bad_record_accessor.md:1:7:1:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 1 7) (end 1 8))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "error_bad_record_accessor.md") (start 1 7) (end 1 8) (annotation error) (line-text "person.@")))))
~~~
# TOKENS
~~~zig
LowerIdent,Dot,MalformedOpaqueNameWithoutName,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expr_dot_suffix_not_allowed"))
~~~
# FORMATTED
~~~roc

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
