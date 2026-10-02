# META
~~~ini
description=Renderer coverage for a name bound more than once in a pattern
type=reporting
~~~
# SOURCE
~~~roc
connections_equal = |pair| match pair {
    (from, from) => True
    _ => False
}
~~~
# EXPECTED
DUPLICATE NAME IN PATTERN - reporting_duplicate_pattern_binder.md:2:12:2:16
UNUSED VARIABLE - reporting_duplicate_pattern_binder.md:2:6:2:10
# REPORT
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Duplicate Name In Pattern")
		(region (start 2 12) (end 2 16))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "from")
			(reflow " is bound more than once in this pattern."))
		(document
			(source-region (file "reporting_duplicate_pattern_binder.md") (start 2 12) (end 2 16) (annotation error) (line-text "    (from, from) => True"))
			(line-break)
			(reflow "It was first bound here:")
			(line-break)
			(source-region (file "reporting_duplicate_pattern_binder.md") (start 2 6) (end 2 10) (annotation dim) (line-text "    (from, from) => True"))
			(line-break)
			(reflow "Each name in a pattern must be different. To check whether two values are equal, give them different names and compare them in a guard:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(a, b) if a == b => ...")
			(annotation-end)))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 2 6) (end 2 10))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "from")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_from")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "reporting_duplicate_pattern_binder.md") (start 2 6) (end 2 10) (annotation error) (line-text "    (from, from) => True")))))
~~~
# CLI
~~~text
── ✗ duplicate name in pattern ────── reporting_duplicate_pattern_binder.md:2:12

The name from is bound more than once in this pattern.

(from, from) => True
       ^^^^

It was first bound here:

(from, from) => True
 ^^^^
Each name in a pattern must be different. To check whether two values are
equal, give them different names and compare them in a guard:

    (a, b) if a == b => ...

── ● unused variable ───────────────── reporting_duplicate_pattern_binder.md:2:6

Variable from is defined here and then never used:

(from, from) => True
 ^^^^

If you don't need this variable, prefix it with an underscore like _from to
suppress this warning.

~~~
# MARKDOWN
~~~markdown
**Duplicate Name In Pattern**
The name `from` is bound more than once in this pattern.
```roc
    (from, from) => True
```
           ^^^^

It was first bound here:
```roc
    (from, from) => True
```
     ^^^^

Each name in a pattern must be different. To check whether two values are equal, give them different names and compare them in a guard:

    (a, b) if a == b => ...

**Unused Variable**
Variable `from` is defined here and then never used:
If you don't need this variable, prefix it with an underscore like `_from` to suppress this warning.
```roc
    (from, from) => True
```
     ^^^^


~~~
# HTML
~~~html
<div class="report error">
<h1 class="report-title">duplicate name in pattern</h1>
<div class="report-content">
The name <span class="symbol-unqualified">from</span> is bound more than once in this pattern.<br>
<div class="source-region"><pre class="error">    (from, from) =&gt; True
           ^^^^
</pre></div><br>
It was first bound here:<br>
<div class="source-region"><pre class="dim">    (from, from) =&gt; True
     ^^^^
</pre></div><br>
Each name in a pattern must be different. To check whether two values are equal, give them different names and compare them in a guard:<br>
<br>
<pre class="code-block">&nbsp;&nbsp;&nbsp;&nbsp;(a, b) if a == b =&gt; ...</pre></div>
</div>
<div class="report warning">
<h1 class="report-title">unused variable</h1>
<div class="report-content">
Variable <span class="symbol-unqualified">from</span> is defined here and then never used:<br>
If you don&#39;t need this variable, prefix it with an underscore like <span class="symbol-unqualified">_from</span> to suppress this warning.<br>
<div class="source-region"><pre class="error">    (from, from) =&gt; True
     ^^^^
</pre></div></div>
</div>
~~~
# LSP
~~~text
duplicate name in pattern

The name from is bound more than once in this pattern.
    (from, from) => True
           ^^^^

It was first bound here:
    (from, from) => True
     ^^^^

Each name in a pattern must be different. To check whether two values are equal, give them different names and compare them in a guard:

  (a, b) if a == b => ...unused variable

Variable from is defined here and then never used:
If you don't need this variable, prefix it with an underscore like _from to suppress this warning.
    (from, from) => True
     ^^^^
~~~
