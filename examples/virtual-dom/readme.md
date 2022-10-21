# Use Roc for interactive web UIs using WebAssembly

This platform builds on the ideas in [Action-State in Roc (v5)](https://docs.google.com/document/d/16qY4NGVOHu8mvInVD-ddTajZYSsFvFBvQON_hmyHGfo/edit) about how UI frameworks in general should work in Roc.

> As with many aspects of Roc, this is a design that's both inspired by and similar to - but not quite the same as - [how Elm does it](https://guide.elm-lang.org/architecture/).

It is also inspired by the GUI example apps in this repo, particularly the clone of the Breakout game.

However there are some differences when working in the browser in WebAssembly.

WebAssembly does not have direct access to web APIs like the DOM. It needs to go through JavaScript. When I first started working with WebAssembly 5 years ago, these APIs were "coming soon"! Hopefully that will happen some day. But right now, we need to accept this situation and build for it.

We obviously want our UI library to go fast, and in my experience, this communication across the Wasm/JS boundary is the _single most important factor for performance_. So we need to design our system to be as efficient as possible about converting data back and forth. Several aspects of this platform are designed around this.

## Key design ideas
- Only call out to JavaScript when we really need to!
- Use integer enums for commonly-used strings like HTML tag names, attributes, etc. and use the integer as an index into an array of JS strings. This reduces the time we spend dereferencing pointers in the Wasm code, and converting UTF-8 to UTF-16 at the boundary.
- Use a virtual DOM to minimise the number of DOM operations we need to do.
- Maintain a JS array of all our DOM nodes so that Wasm can refer to them by index.
- Use an arena allocator when running the view code to generate the virtual DOM, and free it all at once on the next render.
- Don't try to serialize event handler functions. Instead keep them in a Roc Dict, and give each an ID. Then create a JS event listener that passes this ID to an event dispatcher in Wasm, which can look up the Roc handler and call it.
- When decoding DOM events, don't serialize the entire Event object, but only the pieces of it that the Roc handler function actually needs. (In fact, this is necessary since the whole Event object may not be serializable.)
- Provide a way to initialize the virtual DOM from a server-rendered DOM
