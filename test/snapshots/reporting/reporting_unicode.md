# META
~~~ini
description=Renderer coverage for diagnostics whose source line contains multibyte Unicode content
type=reporting
~~~
# SOURCE
~~~roc
greeting : U64
greeting = "héllo 🐢"
~~~
# EXPECTED
TYPE MISMATCH - reporting_unicode.md:2:12:2:25
# REPORT
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 12) (end 2 25))
		(headline
			(reflow "This string literal is being used where a non-string type is needed."))
		(document
			(source-region (file "reporting_unicode.md") (start 2 12) (end 2 25) (annotation error) (line-text "greeting = \"héllo 🐢\""))
			(line-break)
			(reflow "The type was determined to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U64")
			(annotation-end))))
~~~
# CLI
~~~text

┌───────────────┐
│ TYPE MISMATCH ├─ This string literal is being used where a non-string ──────┐
└┬──────────────┘  type is needed.                                            │
 │                                                                            │
 │  greeting = "héllo 🐢"                                                     │
 │             ‾‾‾‾‾‾‾‾‾‾                                                     │
 └───────────────────────────────────────────────── reporting_unicode.md:2:12 ┘

    The type was determined to be:

        U64

~~~
# MARKDOWN
~~~markdown
**Type Mismatch**
This string literal is being used where a non-string type is needed.
**reporting_unicode.md:2:12:2:25:**
```roc
greeting = "héllo 🐢"
```
           ^^^^^^^^^^^^^

The type was determined to be:

    U64

~~~
# HTML
~~~html
<div class="report error">
<h1 class="report-title">TYPE MISMATCH</h1>
<div class="report-content">
This string literal is being used where a non-string type is needed.<br>
<div class="source-region"><span class="filename">reporting_unicode.md:2:12:2:25:</span> <pre class="error">greeting = &quot;héllo 🐢&quot;</pre></div><br>
The type was determined to be:<br>
<br>
<pre class="code-block">&nbsp;&nbsp;&nbsp;&nbsp;U64</pre></div>
</div>
~~~
# LSP
~~~text
TYPE MISMATCH

This string literal is being used where a non-string type is needed.
reporting_unicode.md:2:12:2:25: greeting = "héllo 🐢"

The type was determined to be:

  U64
~~~
