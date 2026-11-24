# META
~~~ini
description=A call passes a tag the match leaves uncovered when its other column is False
type=reporting
~~~
# SOURCE
~~~roc
pick : _, Bool -> U64
pick = |tag, flag| match (tag, flag) {
    (A, _) => 1
    (_, True) => 2
}

example = |_| pick(B, False)
~~~
# EXPECTED
TYPE MISMATCH - reporting_match_closed_tag_union_bool_call.md:7:15:7:15
# REPORT
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 7 20) (end 7 21))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument being passed to this function has the wrong type."))
		(document
			(source-underlines
				(display (file "reporting_match_closed_tag_union_bool_call.md") (start 7 15) (end 7 29) (annotation dim) (line-text "example = |_| pick(B, False)"))
				(underline (start 7 20) (end 7 21) (annotation error)))
			(line-break)
			(reflow "This argument has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[B]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But")
			(reflow " ")
			(annotated code "pick")
			(reflow " ")
			(reflow "needs the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[A]")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Maybe")
			(reflow " ")
			(annotated code "B")
			(reflow " ")
			(reflow "should be")
			(reflow " ")
			(annotated code "A")
			(reflow "?")
			(line-break)
			(line-break)
			(reflow "This")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "limits the tag union to the tags its branches name, because no branch handles")
			(reflow " ")
			(annotated code "(<other>, False)")
			(reflow ", where")
			(reflow " ")
			(annotated code "<other>")
			(reflow " ")
			(reflow "is a tag no branch names:")
			(line-break)
			(source-region (file "reporting_match_closed_tag_union_bool_call.md") (start 2 20) (end 5 2) (annotation dim) (line-text "pick = |tag, flag| match (tag, flag) {\n    (A, _) => 1\n    (_, True) => 2\n}")))))
~~~
# CLI
~~~text
── ✗ type mismatch ────────── reporting_match_closed_tag_union_bool_call.md:7:20

The first argument being passed to this function has the wrong type.

example = |_| pick(B, False)
                   ^

This argument has the type:

    [B]

But pick needs the first argument to be:

    [A]

Hint: Maybe B should be A?

This match limits the tag union to the tags its branches name, because no
branch handles (<other>, False), where <other> is a tag no branch names:

pick = |tag, flag| match (tag, flag) {
    (A, _) => 1
    (_, True) => 2
}

~~~
# MARKDOWN
~~~markdown
**Type Mismatch**
The first argument being passed to this function has the wrong type.
```roc
example = |_| pick(B, False)
```
                   ^

This argument has the type:

    [B]

But `pick` needs the first argument to be:

    [A]

**Hint:** Maybe `B` should be `A`?

This `match` limits the tag union to the tags its branches name, because no branch handles `(<other>, False)`, where `<other>` is a tag no branch names:
```roc
pick = |tag, flag| match (tag, flag) {
    (A, _) => 1
    (_, True) => 2
}
```


~~~
# HTML
~~~html
<div class="report error">
<h1 class="report-title">type mismatch</h1>
<div class="report-content">
The first argument being passed to this function has the wrong type.<br>
<div class="source-region"><pre class="source-code">example = |_| pick(B, False)
                   ^
</pre></div><br>
This argument has the type:<br>
<br>
<pre class="code-block">&nbsp;&nbsp;&nbsp;&nbsp;[B]</pre><br>
<br>
But <code class="code">pick</code> needs the first argument to be:<br>
<br>
<pre class="code-block">&nbsp;&nbsp;&nbsp;&nbsp;[A]</pre><br>
<br>
<strong class="emphasis">Hint:</strong> Maybe <code class="code">B</code> should be <code class="code">A</code>?<br>
<br>
This <code class="code">match</code> limits the tag union to the tags its branches name, because no branch handles <code class="code">(&lt;other&gt;, False)</code>, where <code class="code">&lt;other&gt;</code> is a tag no branch names:<br>
<div class="source-region"><pre class="dim">pick = |tag, flag| match (tag, flag) {
    (A, _) =&gt; 1
    (_, True) =&gt; 2
}
</pre></div></div>
</div>
~~~
# LSP
~~~text
The first argument being passed to this function has the wrong type.
example = |_| pick(B, False)
                   ^

This argument has the type:

  [B]

But pick needs the first argument to be:

  [A]

Hint: Maybe B should be A?

This match limits the tag union to the tags its branches name, because no branch handles (<other>, False), where <other> is a tag no branch names:
pick = |tag, flag| match (tag, flag) {
    (A, _) => 1
    (_, True) => 2
}
~~~
