# Fast

Roc code is designed to build fast and run fast...but what does "fast" mean here? And how close is Roc's current implementation to realizing that goal?

## [Fast programs](#fast-programs) {#fast-programs}

What "fast" means in embedded systems is different from what it means in games, which in turn is different from what it means on the Web. To better understand Roc's performance capabilities, let's look at the upper bound of how fast optimized Roc programs are capable of running, and the lower bound of what types of languages Roc should generally outperform.

### [Limiting factors: memory management and async I/O](#limiting-factors) {#limiting-factors}

<span class="nowrap">Roc is a</span> [memory-safe](https://en.wikipedia.org/wiki/Memory_safety) language with [automatic memory management](https://en.wikipedia.org/wiki/Garbage_collection_(computer_science)#Reference_counting). Automatic memory management has some unavoidable runtime overhead, and memory safety based on static analysis rules out certain performance optimizations—which is why [unsafe Rust](https://doc.rust-lang.org/book/ch19-01-unsafe-rust.html) can outperform safe Rust. This gives Roc a lower performance ceiling than languages which support memory unsafety and manual memory management, such as C, C++, Zig, and Rust.

Another part of Roc's design is that all I/O operations are done using a lightweight state machine so that they can be asynchronous. This has potential performance benefits compared to synchronous I/O, but it also has some unavoidable overhead.

### [Generally faster than dynamic or gradual languages](#faster-than) {#faster-than}

As a general rule, Roc programs should have almost strictly less runtime overhead than equivalent programs written in languages with dynamic types and automatic memory management. This doesn't mean all Roc programs will outperform all programs in these languages, but it does mean Roc should have a higher ceiling on what performance is achievable.

This is because dynamic typing (and gradual typing) requires tracking types at runtime, which has overhead. Roc tracks types only at compile time, and tends to have [minimal (often zero) runtime overhead](https://vimeo.com/653510682) for language constructs compared to the top performers in industry. For example, Roc's generics, records, functions, numbers, and tag unions have no more runtime overhead than they would in their Rust or C++ equivalents.

When [benchmarking compiled Roc programs](https://www.youtube.com/watch?v=vzfy4EKwG_Y), the goal is to have them normally outperform the fastest mainstream garbage-collected languages (for example, Go, C#, Java, and JavaScript), but it's a non-goal to outperform languages that support memory unsafety or manual memory management. There will always be some individual benchmarks where mainstream garbage-collected languages outperform Roc, but the goal is for these to be uncommon rather than the norm.

### [Domain-specific memory management](#domain-specific-memory-management) {#domain-specific-memory-management}

Roc's ["platforms and applications" design](/platforms) means its automatic memory management can take advantage of domain-specific properties to improve performance.

For example, if you build an application on the [`basic-cli` platform](https://github.com/roc-lang/basic-cli) compared to the [`basic-webserver` platform](https://github.com/roc-lang/basic-webserver), each of those platforms may use a different memory management strategy under the hood that's tailored to their respective use cases. Your application's performance can benefit from this, even though building on either of those platforms feels like using ordinary automatic memory management.

This is because Roc [platforms](/platforms) get to determine how memory gets allocated and deallocated in applications built on them. ([`basic-cli`](https://github.com/roc-lang/basic-cli) and [`basic-webserver`](https://github.com/roc-lang/basic-webserver) are examples of platforms, but anyone can build their own platform.) Here are some examples of how platforms can use this to improve application performance:

- A platform for noninteractive command-line scripts can skip deallocations altogether, since any allocated memory will be cheaply reclaimed by the operating system anyway once the script exits. (This strategy is domain-specific; it would not work well for a long-running, interactive program!)
- A platform for Web servers can put all allocations for each request into a particular [region of memory](https://en.wikipedia.org/wiki/Region-based_memory_management) (this is known as "arena allocation" or "bump allocation") and then deallocate the entire region in one cheap operation after the response has been sent. This would essentially drop memory reclamation times to zero. (This strategy relies on Web servers' request/response architecture, and wouldn't make sense in other use cases. [`nea`](https://github.com/tweedegolf/nea) is a platform in early development that is working towards implementing this.)
- A platform for applications that have very long-lived state could implement [meshing compaction](https://youtu.be/c1UBJbfR-H0?si=D9Gp0cdpjZ_Is5v8) to decrease memory fragmentation. (Compaction would probably be a net negative for performance in the previous two examples.)

[This talk](https://www.youtube.com/watch?v=cpQwtwVKAfU&t=75s) has more information about platforms and applications, including demos and examples of other benefits they unlock besides performance.

### [Current progress](#current-progress) {#current-progress}

Roc's "platforms and applications" design already works, including the domain-specific memory management. Most of Roc's data structures are already close to their theoretical limit in terms of performance, at least without changing their behavior or introducing memory unsafety. [This talk](https://vimeo.com/653510682) explains how they're implemented under the hood.

That said, [the current implementation](https://ayazhafiz.com/articles/23/a-lambda-calculus-with-coroutines-and-heapless-closures) of [defunctionalization](https://blog.sigplan.org/2019/12/30/defunctionalization-everybody-does-it-nobody-talks-about-it/) (based on [this paper](https://dl.acm.org/doi/pdf/10.1145/3591260))—which unlocks stack-allocated closures, among other optimizations—has [significant known gaps](https://github.com/roc-lang/roc/issues/5969), and has a ways to go before it works across the board. (If you're interested in getting involved in that implementation, [we'd love to hear from you](https://roc.zulipchat.com)!)

Current optimizations that are completely implemented (give or take the occasional bug) include [LLVM](https://llvm.org/), [Morphic](https://www.youtube.com/watch?v=F3z39M0gdJU&t=3547s), [Perceus](https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v1.pdf), and tail recursion optimization (including [modulo cons](https://en.wikipedia.org/wiki/Tail_call#Tail_recursion_modulo_cons)). Promising examples of potential future optimizations include closure-aware [inlining](https://en.wikipedia.org/wiki/Inline_expansion), [automatic deforestation](https://www.cs.tufts.edu/~nr/cs257/archive/duncan-coutts/stream-fusion.pdf), and full [compile-time evaluation](https://en.wikipedia.org/wiki/Constant_folding) of top-level declarations.

We're also interested in improving the performance of the Morphic alias analysis pass itself; if contributing to that project (or any other optimization project) interests you, please [let us know in the `#contributing` channel](https://roc.zulipchat.com/#narrow/stream/316715-contributing)!

## Fast Feedback Loops

One of Roc's goals is to provide fast feedback loops by making builds normally feel "instant" except on truly enormous projects.

It's a concrete goal to have them almost always complete in under 1 second on the median computer being used to write Roc (assuming that system is not bogged down with other programs using up its resources), and ideally under the threshold at which humans typically find latency perceptible (around 100 milliseconds). In the future, hot code loading can make the feedback loop even faster, by letting you see changes without having to restart your program.

Note that although having fast "clean" builds (without the benefit of caching) is a goal, the "normally feels instant" goal refers to builds where caching was involved. After all, the main downside of build latency is that it comes up over and over in a feedback loop; a fast initial "clean" build is valuable too, but it comes up rarely by comparison.

### Current Progress

`roc check` checks your code for errors (such as invalid syntax, naming errors, and type mismatches) and reports problems it finds. On typical development laptops, this usually takes well under 1 second for small projects (for very small projects, it can be around 10 milliseconds on some popular machines). To date, the largest known Roc projects have lines of code numbering in the low thousands, so there's no data yet on `roc check` times for larger projects.

`roc build` does everything `roc check` does, but it additionally builds a runnable binary of your program. You may notice that `roc build` takes much longer to complete! This is because
of two projects that are underway but not completed yet:
- *Development backend* refers to generating machine code directly instead of asking [LLVM](https://llvm.org/) to generate it. LLVM is great at generating optimized machine code, but it takes a long time to generate it—even if you turn off all the optimizations (and `roc` only has LLVM perform optimizations when the `--optimize` flag is set). The dev backend is currently implemented for WebAssembly, which you can see in the [Web REPL](https://www.roc-lang.org/repl), and in `roc repl` except on Windows. Work is underway to implement it for `roc build` and `roc run`, as well as macOS, Windows, and the ARM versions of all of these.
- *Surgical linking* refers to a fast way of combining the platform and application into one binary. Today, this works on x64 Linux, x64 Windows, and WebAssembly. `roc build` on macOS is noticeably slower because it falls back on non-surgical linking.

Here's a table summarizing the current progress:

Target      | Dev backend | Surgical linking  |
------------|-------------|-------------------|
WebAssembly |     yes     |        yes        |
macOS ARM   |  repl only  |                   |
macOS x64   |  repl only  |                   |
Linux ARM   |  repl only  |                   |
Linux x64   |  repl only  |        yes        |
Windows x64 |             |        yes        |
Windows ARM |             |                   |

Once we have full coverage, `roc build` (and `roc run` and `roc test`, which also perform builds) should take only a bit longer than `roc check`.

The next major performance improvement will be caching. Currently, `roc` always builds everything from scratch. Most of the time, it could benefit from caching some of the work it had done in a previous build, but today it doesn't do that. There's a design for the caching system, but essentially none of the implementation has started yet. Hot code loading will be the next major improvement after caching, but it requires full dev backend coverage, and does not have a concrete design yet.

## Friendly

In addition to being fast, Roc also aims to be a friendly programming language.

[What does _friendly_ mean here?](/friendly)
