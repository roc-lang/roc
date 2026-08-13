# META
~~~ini
description=Renderer coverage for a simple canonicalization diagnostic with a single-line region
type=reporting
~~~
# SOURCE
~~~roc
main = foo
~~~
# EXPECTED
NAME NOT IN SCOPE - reporting_undefined_variable.md:1:8:1:11
# REPORT
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 1 8) (end 1 11))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "foo")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "reporting_undefined_variable.md") (start 1 8) (end 1 11) (annotation error) (line-text "main = foo")))))
~~~
# CLI
~~~text

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `foo` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  main = foo                                                                │
 │         ‾‾‾                                                                │
 └─────────────────────────────────────── reporting_undefined_variable.md:1:8 ┘

    Is it misspelled, or is there an import missing?

~~~
# MARKDOWN
~~~markdown
**Name Not In Scope**
Nothing is named `foo` in this scope.
Is it misspelled, or is there an import missing?

**reporting_undefined_variable.md:1:8:1:11:**
```roc
main = foo
```
       ^^^


~~~
# HTML
~~~html
<div class="report error">
<h1 class="report-title">NAME NOT IN SCOPE</h1>
<div class="report-content">
Nothing is named <span class="symbol-unqualified">foo</span> in this scope.<br>
Is it misspelled, or is there an import missing?<br>
<br>
<div class="source-region"><span class="filename">reporting_undefined_variable.md:1:8:1:11:</span> <pre class="error">main = foo</pre></div></div>
</div>
~~~
# LSP
~~~text
NAME NOT IN SCOPE

Nothing is named foo in this scope.
Is it misspelled, or is there an import missing?

reporting_undefined_variable.md:1:8:1:11: main = foo
~~~
