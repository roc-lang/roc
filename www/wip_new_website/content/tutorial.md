# Tutorial

This tutorial will teach you how to build Roc applications. Along the way, you'll learn how to write tests, use the REPL, and much more!

## [Strings and Numbers](#strings-and-numbers) {#strings-and-numbers}

Let's start by getting acquainted with Roc's [_Read-Eval-Print-Loop_](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop), or **REPL** for short. Run this in a terminal:

<code class="block">roc repl</code>

If Roc is [installed](#installation), you should see this:

<pre><samp>The rockin’ roc repl</samp></pre>

So far, so good!

### [Hello, World!](#hello-world) {#hello-world}

Try typing this in the REPL and pressing Enter:

<samp class="repl-prompt">"Hello, World!"</samp>

The REPL should cheerfully display the following:

<pre><samp><span class="literal">"Hello, World!" </span><span class="colon">:</span> Str</samp></pre>

Congratulations! You've just written your first Roc code.

### [Naming Things](#naming-things) {#naming-things}

When you entered the _expression_ `"Hello, World!"`, the REPL printed it back out. It also printed `: Str`, because `Str` is that expression's type. We'll talk about types later; for now, let's ignore the `:` and whatever comes after it whenever we see them.

**TODO -- move tutorial here when this site is migrated.**
