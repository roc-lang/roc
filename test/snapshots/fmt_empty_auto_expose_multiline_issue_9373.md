# META
~~~ini
description=Issue 9373 - Formatter must keep the imported alias on the same line as `as`, even with multiline qualifiers
type=snippet
~~~
# SOURCE
~~~roc
import A / B as
    X1
import A / B / C as
    X2
import A / B / C / D as
    X3
~~~
# EXPECTED
MOD NOT FOUND - fmt_empty_auto_expose_multiline_issue_9373.md:1:1:2:7
MOD NOT FOUND - fmt_empty_auto_expose_multiline_issue_9373.md:3:1:4:7
MOD NOT FOUND - fmt_empty_auto_expose_multiline_issue_9373.md:5:1:6:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 1 1) (end 2 7))
		(headline
			(text "The mod ")
			(annotated code "A/B")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fmt_empty_auto_expose_multiline_issue_9373.md") (start 1 1) (end 2 7) (annotation error) (line-text "import A / B as\n    X1"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 3 1) (end 4 7))
		(headline
			(text "The mod ")
			(annotated code "A/B/C")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fmt_empty_auto_expose_multiline_issue_9373.md") (start 3 1) (end 4 7) (annotation error) (line-text "import A / B / C as\n    X2"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 5 1) (end 6 7))
		(headline
			(text "The mod ")
			(annotated code "A/B/C/D")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fmt_empty_auto_expose_multiline_issue_9373.md") (start 5 1) (end 6 7) (annotation error) (line-text "import A / B / C / D as\n    X3")))))
~~~
# TOKENS
~~~zig
KwImport,UpperIdent,OpSlash,UpperIdent,KwAs,
UpperIdent,
KwImport,UpperIdent,OpSlash,UpperIdent,OpSlash,UpperIdent,KwAs,
UpperIdent,
KwImport,UpperIdent,OpSlash,UpperIdent,OpSlash,UpperIdent,OpSlash,UpperIdent,KwAs,
UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "A / B") (alias "X1"))
		(s-import (raw "A / B / C") (alias "X2"))
		(s-import (raw "A / B / C / D") (alias "X3"))))
~~~
# FORMATTED
~~~roc
import A/B as X1
import A/B/C as X2
import A/B/C/D as X3
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (mod "A/B")
		(exposes))
	(s-import (mod "A/B/C")
		(exposes))
	(s-import (mod "A/B/C/D")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
