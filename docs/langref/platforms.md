# Platforms

Something that sets Roc apart from other programming languages is its _platforms and applications_ architecture. Every Roc application is built on exactly one _platform_, and that platform (not Roc's standard library) provides all of the application's I/O primitives.

## Applications

Here is a Roc application that prints `"Hello, World!"` to the command line:

```roc
app [main!] { pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/1.0.0/AnZoxzoGPtSGQ15EQh6pBeeaHJ7aizP9MQhK81dES3Uq.tar.zst" }

import pf.Stdout

main! = |_args| {
    Stdout.line!("Hello, World!")?
    Ok({})
}
```

This application is built on a platform called [roc-platform-template-zig](https://github.com/lukewilliamboswell/roc-platform-template-zig), a starter platform written in Zig and Roc. See [Application Modules](modules#application-modules) for the details of the `app` header, and [Platform Modules](modules#platform-modules) for how a platform declares the interface an application builds against.

## Domain-specific functionality

Roc platforms provide domain-specific functionality that multiple applications can use as a foundation to build on, much like game engines and web frameworks do.

Also like many game engines and web frameworks, a Roc platform has a high-level Roc API which presents a nice interface to a lower-level implementation (written in a different language). That lower-level implementation provides the foundational primitives the platform needs to operate, such as a C++ 3D rendering system in a game engine, or a Rust HTTP networking system in a web framework.

Some example platforms, and functionality they might provide:

- A game engine platform might provide functionality for rendering and sound.
- A web server platform probably would provide functionality for responding to incoming HTTP requests, which a game engine platform likely would not.
- A native [GUI](https://en.wikipedia.org/wiki/Graphical_user_interface) platform might provide functionality for defining native operating system UI elements, whereas a game engine platform might focus more on rendering with [shaders](https://en.wikipedia.org/wiki/Shader), and a web server platform would not have GUI functionality at all.

Platforms can be much more specific than these broad domains. For example, anyone could make a platform for writing [Vim](https://en.wikipedia.org/wiki/Vim_%28text_editor%29) plugins, or [Postgres](https://en.wikipedia.org/wiki/PostgreSQL) extensions, or robots ([which has already happened](https://roc.zulipchat.com/#narrow/stream/304902-show-and-tell/topic/Roc.20on.20a.20microcontroller/near/286678630)), or even [implementing servo logic for a clock that physically turns panels to simulate an LCD](https://roc.zulipchat.com/#narrow/stream/304641-ideas/topic/Roc.20Clock/near/327939600). You really can get as specific as you like.

Platforms can also be designed to have a single, specific application run on them. For example, you can make a platform that is essentially "your entire existing code base in another language," and then use Roc as an embedded language within that code base. [Vendr](https://www.vendr.com/careers) used this strategy to call Roc functions from their [Node.js](https://nodejs.org/en) backend using [roc-esbuild](https://github.com/vendrinc/roc-esbuild), as a way to incrementally transition code from Node to Roc.

## Platform scope

Roc platforms have a broader scope of responsibility than game engines or web frameworks. In addition to providing a nice domain-specific interface, platforms are also responsible for:

- Tailoring memory management to that domain (see [Memory management](#memory-management))
- Providing all I/O primitives

In most languages, I/O primitives come with the standard library. In Roc, the standard library contains only functions and data structures; an application gets all of its I/O primitives from its platform. For example, in the "Hello, World" application above, the `Stdout.line!` function comes from the `roc-platform-template-zig` platform itself, not from Roc's standard library.

This design has a few benefits.

### Ecosystem benefits

Some I/O operations make sense in some use cases but not others.

For example, suppose you are building an application on a command-line platform, and you use a third-party package which sometimes blocks the program while it waits for [standard input](https://en.wikipedia.org/wiki/Standard_streams#Standard_input_%28stdin%29). This might be fine for a command-line application, but it would probably be a poor fit for a web server. Similarly, a package which does some occasional file I/O for caching might work fine on either of those platforms, but might break in surprising ways when used on a platform designed to run in a browser on WebAssembly, since browsers do not offer arbitrary file I/O access.

Because Roc's I/O primitives come from platforms, these mismatches can be prevented at build time. A browser-based platform would not expose file I/O primitives, a web server would not expose a way to block on reading from standard input, and so on.

### Security benefits

Since platforms have exclusive control over all I/O primitives, one of the things they can do is create domain-specific security guarantees around them. For example, a platform for writing text editor plugins might display a prompt to the end user before performing any file I/O operations outside the directory that is currently open in the editor.

[This talk](https://www.youtube.com/watch?v=cpQwtwVKAfU&t=75s) shows an example of taking this idea a step further with a "safe scripting" platform for writing command-line scripts. The idea is that you could download a script from the Internet and run it on this platform without worrying that the script would do bad things to your computer, because the platform would (much like a web browser) show you specific prompts before allowing the script to do potentially harmful I/O, such as filesystem operations.

These security guarantees can be relied on because platforms have _exclusive_ control over all I/O primitives, including how they are implemented. There are no escape hatches that a malicious program could use to get around them. For example, Roc programs that want to call functions in other languages must do so using primitives provided by the platform, which the platform can disallow (or sandbox with end-user prompts) in the same way.

### Performance benefits

Many I/O operations can benefit from being run concurrently. Since platforms are in charge of how those I/O operations are implemented, they can also determine how they are scheduled. This means that both applications and packages can describe which operations they want to run concurrently, and then the platform can optimize the scheduling of these operations using its domain-specific knowledge.

For example, a command-line platform might schedule concurrent operations across all available cores (or some lower number specified by a command-line argument). In contrast, a web server platform might try to balance available cores across multiple request handlers, to prevent undesirable scenarios like one handler getting all the cores (meaning none of the others can progress).

> Note: although platform-implemented scheduling of concurrent operations is theoretically possible today, there are currently some missing pieces to make it practical for platform authors to implement. That work is already in progress, but is not yet complete.

## How platforms are implemented

To understand how platforms can tailor automatic memory management to their particular domain, it helps to understand how platforms are implemented.

### Host {#host}

Each platform consists of two parts:

- **The Roc API** is the part that application authors see. For example, `Stdout.line!` is part of the Roc API of roc-platform-template-zig. It is defined by the platform's [platform module](modules#platform-modules) and the [type modules](modules#type-modules) it exposes.
- **The host** is the under-the-hood implementation written in a language other than Roc. For example, the host for roc-platform-template-zig is written in Zig. It has a Zig function which implements the behavior of the `Stdout.line!` operation, and all the other I/O operations it supports.

This design means application authors do not necessarily need to know (or care) about the non-Roc language being used to implement the platform's host. That can be a behind-the-scenes implementation detail that only the platform's authors are concerned with. Application authors interact only with the public-facing Roc API.

The platform module's [`provides`](modules#provides) section is what maps the symbol names Roc links against in the host to the Roc functions exposed under those symbols.

### Memory management

Host authors implement not only the platform's I/O primitives, but also functions for memory allocation and deallocation. In C terms, the host provides [`malloc` and `free`](https://en.wikipedia.org/wiki/C_dynamic_memory_allocation) implementations which the compiled Roc application will automatically call whenever it needs to allocate or deallocate memory.

[The same talk mentioned earlier](https://www.youtube.com/watch?v=cpQwtwVKAfU&t=75s) demonstrates some benefits of this, such as being able to get accurate diagnostics on how much memory the Roc part (or even specific Roc parts) of a running program are using.

The bigger benefit is tailoring memory management itself based on the domain. For example, [nea](https://github.com/tweedegolf/nea/) is a work-in-progress web server which performs [arena allocation](https://en.wikipedia.org/wiki/Region-based_memory_management) on each request handler. In Roc terms, this means the host's implementation of `malloc` can allocate into the current handler's arena, and `free` can be a no-op. Instead, the arena is reset when the response has been sent.

In this design, heap allocations in a web server running on `nea` are about as cheap as stack allocations, and deallocations are essentially free. For the server's throughput, latency, and predictability, this is much better than (for example) having to pay for periodic garbage collection.

### Program start

When a compiled Roc program runs, it is actually the host, not the Roc application, which starts running first. In C terms, the host implements `main()`, and then at some point it calls a function exposed by the compiled Roc application.

Knowing this, a useful mental model for how Roc platforms and applications interact at the implementation level is: the Roc application compiles down to a C library which the platform can choose to call (or not).

This is essentially what happens behind the scenes when you run `roc build`. Specifically:

1. The Roc compiler builds the Roc application into a binary [object file](https://en.wikipedia.org/wiki/Object_file).
2. Since that application specified its platform, the compiler then looks up the platform's host implementation (which the platform will have provided as an already-compiled binary).
3. Now that it has a binary for the Roc application and a binary for the host, it links them together into one combined binary in which the host portion calls the application portion as many times as it likes.

This process works for small platforms and large applications (for example, a very large web server application) as well as for large platforms and small applications (for example, a very large C++ game which serves as a platform for a small amount of Roc application code that the game uses for scripting).

## Summary

Every Roc application has exactly one platform. That platform provides all the I/O primitives the application can use; Roc's standard library provides no I/O operations, and the only way for a Roc application to execute functions in other languages is if the platform offers a way to do that.

This I/O design has [security benefits](#security-benefits), [ecosystem benefits](#ecosystem-benefits), and [performance benefits](#performance-benefits). The [domain-specific memory management](#memory-management) that platforms can implement can offer additional benefits as well.

Applications interact only with the _Roc API_ portion of a platform, but there is also a _[host](#host)_ portion (written in a different language) that works behind the scenes. The host determines how the program starts, how memory is allocated and deallocated, and how I/O primitives are implemented.

Anyone can implement their own platform. There is not yet an official guide about how to do this, but there are some useful examples:

- For [Roc nightlies](https://github.com/roc-lang/nightlies/releases) using the new (Zig) compiler:
  - [Small Zig platform](https://github.com/lukewilliamboswell/roc-platform-template-zig)
  - [Small Rust platform](https://github.com/lukewilliamboswell/roc-platform-template-rust)
  - [Newest basic-cli (work in progress)](https://github.com/roc-lang/basic-cli/pull/423)
- For Roc version alpha4:
  - [basic-cli platform 0.20.0](https://github.com/roc-lang/basic-cli/tree/0.20.0)
  - [basic-webserver platform 0.13.1](https://github.com/roc-lang/basic-webserver/tree/0.13.1)
  - [Go platform](https://github.com/roc-lang/examples/tree/738b08558c656a11b69a1465b539456ae64605ec/examples/GoPlatform)
  - [.NET platform](https://github.com/roc-lang/examples/tree/738b08558c656a11b69a1465b539456ae64605ec/examples/DotNetPlatform)
