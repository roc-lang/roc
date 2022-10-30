# Use Roc for interactive web UIs using WebAssembly

This platform builds on the ideas in [Action-State in Roc (v5)](https://docs.google.com/document/d/16qY4NGVOHu8mvInVD-ddTajZYSsFvFBvQON_hmyHGfo/edit) about how UI frameworks in general should work in Roc.

> As with many aspects of Roc, this is a design that's both inspired by and similar to - but not quite the same as - [how Elm does it](https://guide.elm-lang.org/architecture/).

It is also inspired by the GUI example apps in this repo, particularly the clone of the Breakout game.

However there are some differences when working in the browser in WebAssembly.

WebAssembly does not have direct access to web APIs like the DOM. It needs to go through JavaScript. In my experience, this communication across the Wasm/JS boundary is the _single most important factor for performance_. So we need to design our system to be as efficient as possible about converting data back and forth. Several aspects of this platform are designed around this.

## Key design ideas
- Only call out to JavaScript when we really need to! Each effect function is a thin wrapper around a DOM API function (like `document.createElement()` or `element.appendChild()`).
- Use a virtual DOM to minimise the number of DOM operations we need to do.
- Maintain a JS array of all our DOM nodes so that Wasm can refer to them by index.
- Don't try to send event handler functions to JS. Instead keep them in a Roc Dict, and give each an ID. Then create a JS event listener that passes this ID to an event dispatcher in Wasm, which can look up the Roc handler and call it.
- When decoding DOM events, don't serialize the entire JavaScript `Event` object, but only the pieces of it that the Roc handler function actually needs. (In fact, `Event`s in general are not serializable, since they contain cyclic references, so we have to do this!)

## Goals
- Allow the app developer to run the same view code on the back end or front end, for server-side rendering.
- Provide a way to initialize the virtual DOM from server-rendered HTML.
- Make it easy for the app developer to avoid bloat by making most of the page static, with a few pockets of interactivity.

## Later possible improvements
- Use integer enums for commonly-used strings like HTML tag names, attributes, etc. and use the integer as an index into an array of JS strings. This reduces the time we spend dereferencing pointers in the Wasm code, and converting UTF-8 to UTF-16 at the boundary.
- Use an arena allocator when running the view code to generate the virtual DOM, and free it all at once on the next render.

## Short term TODO
- [x] JS to load and initialize WebAssembly
- [ ] Make the Wasm part of the host, for allocators and stuff (probably Zig?)
- [ ] Prototype without virtual DOM. Just call the Effects directly.
- [x] Events in Roc, with CyclicStructureAccessor type, etc.
- [x] Do something about TypeScript (switch to JS permanently? make a build script? No, use JSDoc)
- [ ] Implement Vdom diff in Roc
