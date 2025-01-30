# Platforms

Something that sets Roc apart from other programming languages is its <span class="nowrap">_platforms and applications_</span> architecture.

## [Applications](#applications) {#applications}

Here is a Roc application that prints `"Hello, World!"` to the command line:

```roc
app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout

main =
    Stdout.line "Hello, World!"
```

Every Roc application, including this one, is built on a _platform_. This application happens to be built on a platform called [basic-cli](https://github.com/roc-lang/basic-cli), which is a platform for building command-line interfaces.

## [Domain-specific functionality](#domain-specific) {#domain-specific}

Roc platforms provide domain-specific functionality that multiple applications can use as a foundation to build on, much like game engines and Web frameworks do.

Also like many game engines and Web frameworks, Roc platforms have a high-level Roc API which presents a nice interface to a lower-level implementation (written in a different language), which provides the foundational primitives that platform needs to operate—such as a C++ 3D rendering system in a game engine, or a Rust HTTP networking system in a Web framework.

Here are some example Roc platforms, and functionality they might provide:

-   A Roc game engine platform might provide functionality for rendering and sound.
-   A Roc Web server platform (like [basic-webserver](https://github.com/roc-lang/basic-webserver)) probably would not provide functionality for rendering and sound, but it might provide functionality for responding to incoming HTTP requests—which a game engine platform likely would not.
-   A Roc native [GUI](https://en.wikipedia.org/wiki/Graphical_user_interface) platform might provide functionality for defining native operating system UI elements, whereas a game engine platform might focus more on rendering with [shaders](https://en.wikipedia.org/wiki/Shader), and a Web server platform would not have GUI functionality at all.

These are broad domains, but platforms can be much more specific than this. For example, anyone could make a platform for writing [Vim](<https://en.wikipedia.org/wiki/Vim_(text_editor)>) plugins, or [Postgres](https://en.wikipedia.org/wiki/PostgreSQL) extensions, or robots ([which has already happened](https://roc.zulipchat.com/#narrow/stream/304902-show-and-tell/topic/Roc.20on.20a.20microcontroller/near/286678630)), or even [implementing servo logic for a clock that physically turns panels to simulate an LCD](https://roc.zulipchat.com/#narrow/stream/304641-ideas/topic/Roc.20Clock/near/327939600). You really can get as specific as you like!

Platforms can also be designed to have a single, specific application run on them. For example, you can make a platform that is essentially "your entire existing code base in another language," and then use Roc as an embedded language within that code base. For example, [Vendr](https://www.vendr.com/careers) is using this strategy to call Roc functions from their [Node.js](https://nodejs.org/en) backend using [roc-esbuild](https://github.com/vendrinc/roc-esbuild), as a way to incrementally transition code from Node to Roc.

## [Platform scope](#scope) {#scope}

Roc platforms have a broader scope of responsibility than game engines or Web frameworks. In addition to providing a nice domain-specific interface, platforms are also responsible for:

-   Tailoring memory management to that domain (more on this later)
-   Providing all I/O primitives

In most languages, I/O primitives come with the standard library. In Roc, the [standard library](https://www.roc-lang.org/builtins/) contains only data structures; an application gets all of its I/O primitives from its platform. For example, in the "Hello, World" application above, the `Stdout.line` function comes from the `basic-cli` platform itself, not from Roc's standard library.

This design has a few benefits.

### [Ecosystem benefits](#ecosystem) {#ecosystem}

Some I/O operations make sense in some use cases but not others.

For example, suppose I'm building an application on a platform for command-line interfaces, and I use a third-party package which sometimes blocks the program while it waits for [standard input](<https://en.wikipedia.org/wiki/Standard_streams#Standard_input_(stdin)>). This might be fine for my command-line application, but it would probably be a very bad fit if I'm using a webserver. Similarly, a package which does some occasional file I/O for caching might work fine on either of those platforms, but might break in surprising ways when used in a platform that's designed to run in a browser on WebAssembly—since browsers don't offer arbitrary file I/O access!

Because Roc's I/O primitives come from platforms, these mismatches can be prevented at build time. The browser-based platform would not expose file I/O primitives, the webserver wouldn't expose a way to block on reading from standard input, and so on. (Note that there's a design in the works for allowing packages which perform I/O to work across multiple platforms—but only platforms which support the I/O primitives it requires—but this design has not yet been implemented.)

### [Security benefits](#security) {#security}

Since platforms have exclusive control over all I/O primitives, one of the things they can do is create domain-specific security guarantees around them. For example, a platform for writing text editor plugins might want to display a prompt to the end user before performing any file I/O operations outside the directory that's currently open in the editor.

[This talk](https://www.youtube.com/watch?v=cpQwtwVKAfU&t=75s) shows an example of taking this idea a step further with a "safe scripting" platform for writing command-line scripts. The idea is that you could download a script from the Internet and run it on this platform without worrying that the script would do bad things to your computer, because the platform would (much like a Web browser) show you specific prompts before allowing the script to do potentially harmful I/O, such as filesystem operations.

These security guarantees can be relied on because platforms have _exclusive_ control over all I/O primitives, including how they are implemented. There are no escape hatches that a malicious program could use to get around these, either; for example, Roc programs that want to call functions in other languages must do so using primitives provided by the platform, which the platform can disallow (or sandbox with end-user prompts) in the same way.

### [Performance benefits](#performance) {#performance}

Many I/O operations can benefit from being run concurrently. Since platforms are in charge of how those I/O operations are implemented, they can also determine how they are scheduled. This means that both applications and packages can describe which operations they want to run concurrently, and then the platform can optimize the scheduling of these operations using its domain-specific knowledge.

For example, a command-line platform might schedule concurrent operations across all available cores (or some lower number specified by a command-line argument). In contrast, a Web server platform might try to balance available cores across multiple request handlers—to prevent undesirable scenarios like one handler getting all the cores (meaning none of the others can progress).

Note that although platform-implemented scheduling of concurrent operations is theoretically possible today, there are currently some missing pieces to make it practical for platform authors to implement. Implementing those missing pieces is already in progress, but is not yet complete.

## [How platforms are implemented](#implementation) {#implementation}

To understand how platforms can tailor automatic memory management to their particular domain, it's helpful to understand how platforms are implemented.

### [The Host and the Roc API](#host-and-roc-api) {#host-and-roc-api}

Each platform consists of two parts:

-   **The Roc API** is the part that application authors see. For example, `Stdout.line` is part of basic-cli's Roc API.
-   **The Host** is the under-the-hood implementation written in a language other than Roc. For example, basic-cli's host is written in Rust. It has a Rust function which implements the behavior of the `Stdout.line` operation, and all the other I/O operations it supports.

This design means that application authors don't necessarily need to know (or care) about the non-Roc language being used to implement the platform's host. That can be a behind-the-scenes implementation detail that only the platform's author(s) are concerned with. Application authors only interact with the public-facing Roc API.

### [Memory management](#memory) {#memory}

Host authors implement not only the platform's I/O primitives, but also functions for memory allocation and deallocation. In C terms, the host provides [`malloc` and `free`](https://en.wikipedia.org/wiki/C_dynamic_memory_allocation) implementations which the compiled Roc application will automatically call whenever it needs to allocate or deallocate memory.

[The same talk mentioned earlier](https://www.youtube.com/watch?v=cpQwtwVKAfU&t=75s) in the context of [security benefits](#security) demonstrates some benefits of this, such as being able to get accurate diagnostics on how much memory the Roc part (or even specific Roc parts) of a running program are using.

The bigger benefit is tailoring memory management itself based on the domain. For example, [nea](https://github.com/tweedegolf/nea/) is a work-in-progress Web server which performs [arena allocation](https://en.wikipedia.org/wiki/Region-based_memory_management) on each request handler. In Roc terms, this means the host's implementation of `malloc` can allocate into the current handler's arena, and `free` can be a no-op. Instead, the arena can be reset when the response has been sent.

In this design, heap allocations in a Web server running on `nea` are about as cheap as stack allocations, and deallocations are essentially free. This is much better for the server's throughput, latency, and predictability than (for example) having to pay for periodic garbage collection!

### [Program start](#program-start) {#program-start}

When a compiled Roc program runs, it's actually the host—not the Roc application—which starts running first. In C terms, the host implements `main()`, and then at some point it calls a function exposed by the compiled Roc application.

Knowing this, a useful mental model for how Roc platforms and applications interact at the implementation level is: the Roc application compiles down to a C library which the platform can choose to call (or not).

This is essentially what's happening behind the scenes when you run `roc build`. Specifically:

1. The Roc compiler builds the Roc application into a binary [object file](https://en.wikipedia.org/wiki/Object_file)
2. Since that application specified its platform, the compiler then looks up the platform's host implementation (which the platform will have provided as an already-compiled binary)
3. Now that it has a binary for the Roc application and a binary for the host, it links them together into one combined binary in which the host portion calls the application portion as many times as it likes.

This process works for small platforms and large applications (for example, a very large Web server application) as well as for large platforms and small applications (for example, a very large C++ game which serves as a platform for a small amount of Roc application code that the game uses for scripting).

## [Summary](#summary) {#summary}

Every Roc application has exactly one platform. That platform provides all the I/O primitives that the application can use; Roc's standard library provides no I/O operations, and the only way for a Roc application to execute functions in other languages is if the platform offers a way to do that.

This I/O design has [security benefits](#security), [ecosystem benefits](#ecosystem), and [performance benefits](#performance). The [domain-specific memory management](#memory) platforms can implement can offer additional benefits as well.

Applications only interact with the _Roc API_ portion of a platform, but there is also a _host_ portion (written in a different language) that works behind the scenes. The host determines how the program starts, how memory is allocated and deallocated, and how I/O primitives are implemented.

Anyone can implement their own platform. There isn't yet an official guide about how to do this, so the best way to get help if you'd like to create a platform is to [say hi in the `#beginners` channel](https://roc.zulipchat.com/#narrow/stream/231634-beginners) on [Roc Zulip!](https://roc.zulipchat.com)
