# META
~~~ini
description=A call passes a tag the match whose branches name every tag leaves uncovered
type=reporting
~~~
# SOURCE
~~~roc
describe = |tag| match tag {
    A => 1.U64
}

example = |_| describe(B)
~~~
# EXPECTED
TYPE MISMATCH - reporting_match_closed_tag_union_named_tags.md:5:15:5:15
# REPORT
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 5 24) (end 5 25))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument being passed to this function has the wrong type."))
		(document
			(source-underlines
				(display (file "reporting_match_closed_tag_union_named_tags.md") (start 5 15) (end 5 26) (annotation dim) (line-text "example = |_| describe(B)"))
				(underline (start 5 24) (end 5 25) (annotation error)))
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
			(annotated code "describe")
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
			(reflow "limits the tag union to the tags its branches name, because no branch handles any other tag:")
			(line-break)
			(source-region (file "reporting_match_closed_tag_union_named_tags.md") (start 1 18) (end 3 2) (annotation dim) (line-text "describe = |tag| match tag {\n    A => 1.U64\n}")))))
~~~
# CLI
~~~text
── ✗ type mismatch ───────── reporting_match_closed_tag_union_named_tags.md:5:24

The first argument being passed to this function has the wrong type.

example = |_| describe(B)
                       ^

This argument has the type:

    [B]

But describe needs the first argument to be:

    [A]

Hint: Maybe B should be A?

This match limits the tag union to the tags its branches name, because no
branch handles any other tag:

describe = |tag| match tag {
    A => 1.U64
}

~~~
# MARKDOWN
~~~markdown
**Type Mismatch**
The first argument being passed to this function has the wrong type.
```roc
example = |_| describe(B)
```
                       ^

This argument has the type:

    [B]

But `describe` needs the first argument to be:

    [A]

**Hint:** Maybe `B` should be `A`?

This `match` limits the tag union to the tags its branches name, because no branch handles any other tag:
```roc
describe = |tag| match tag {
    A => 1.U64
}
```


~~~
# HTML
~~~html
<div class="report error">
<h1 class="report-title">type mismatch</h1>
<div class="report-content">
The first argument being passed to this function has the wrong type.<br>
<div class="source-region"><pre class="source-code">example = |_| describe(B)
                       ^
</pre></div><br>
This argument has the type:<br>
<br>
<pre class="code-block">&nbsp;&nbsp;&nbsp;&nbsp;[B]</pre><br>
<br>
But <code class="code">describe</code> needs the first argument to be:<br>
<br>
<pre class="code-block">&nbsp;&nbsp;&nbsp;&nbsp;[A]</pre><br>
<br>
<strong class="emphasis">Hint:</strong> Maybe <code class="code">B</code> should be <code class="code">A</code>?<br>
<br>
This <code class="code">match</code> limits the tag union to the tags its branches name, because no branch handles any other tag:<br>
<div class="source-region"><pre class="dim">describe = |tag| match tag {
    A =&gt; 1.U64
}
</pre></div></div>
</div>
~~~
# LSP
~~~text
The first argument being passed to this function has the wrong type.
example = |_| describe(B)
                       ^

This argument has the type:

  [B]

But describe needs the first argument to be:

  [A]

Hint: Maybe B should be A?

This match limits the tag union to the tags its branches name, because no branch handles any other tag:
describe = |tag| match tag {
    A => 1.U64
}
~~~
