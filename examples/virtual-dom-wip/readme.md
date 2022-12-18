# Server-side rendering

**NOT READY YET! WORK IN PROGRESS!**

The aim is to develop an example of a virtual DOM platform in Roc, with server-side rendering. The virtual DOM rendering is not working yet though!

This builds on the ideas in [Action-State in Roc (v5)](https://docs.google.com/document/d/16qY4NGVOHu8mvInVD-ddTajZYSsFvFBvQON_hmyHGfo/edit) about how UI frameworks in general should work in Roc.

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
- Support region-based memory allocation for the virtual DOM
  - The virtual DOM has a very predictable lifetime. It lasts until the next render, and can be dropped immediately after the diff. We can allocate the whole tree in its own [memory region](https://en.wikipedia.org/wiki/Region-based_memory_management) and then free it all at once rather that one value at a time.
  - We maintain a list of Roc event handler lambdas, and continually refresh it from the latest virtual DOM tree so that the old lambda values can be safely dropped.

## Goals

- Allow the app developer to run the same view code on the back end or front end, for server-side rendering.
- Provide a way to initialize the virtual DOM from server-rendered HTML.
- Make it easy for the app developer to avoid bloat by making most of the page static, with a few pockets of interactivity.

## Later possible improvements

- Use integer enums for commonly-used strings like HTML tag names, attributes, etc. and use the integer as an index into an array of JS strings. This reduces the time we spend dereferencing pointers in the Wasm code, and converting UTF-8 to UTF-16 at the boundary.
- Use an arena allocator when running the view code to generate the virtual DOM, and free it all at once on the next render.

## Server side rendering

### Principles
- We use the same library for static and dynamic HTML.
- Dynamic code produces `Html state` but the functions that render static HTML take `Html []`. The `[]` type is an empty tag union, and it's impossible to create a value of that type. If you want to pre-render a static version of a view, you can pass it to `translateStatic : Html state -> Html []`
- A static `Html []` tree cannot contain any event handlers.
- `lazy` nodes are fully expanded by `translateStatic`.
- A static `Html []` tree can only contain HTML _attributes_, not DOM _properties_. The difference between the two is rather [pedantic and crazy](https://github.com/elm/html/blob/master/properties-vs-attributes.md). There's usually a 1:1 correspondance, but sometimes attributes and properties have different names like `class` and `className`. This library prefers attribute names to property names, since they work in HTML and we can use `node.setAttribute()` in JS. I'm not sure if there's any practical situation where we _must_ use a DOM property rather than an attribute. We'll see.
- Event listeners and DOM properties will be inserted as part of the front-end initialization.

The app is represented as
```elm
App state initData : {
    init : initData -> state,
    render : state -> Html state,
    wasmUrl : Str,
}
```

### Steps
- Server side
  - Accept an `initData` value from user code, along with an `App`
  - Generate a static version of the view
    - Call `app.init` to convert `initData` to `state`
    - Call `app.render` to get the initial view
    - Call `translateStatic` on the view to get the static version (no event handlers or lazy nodes)
  - Insert a `<script>` to load and initialize the Wasm app from a JSON representation of `initData`
- Client side
  - JS
    - Load the Wasm module
    - Call `roc_alloc` in the Wasm module
    - Write the `initData` JSON string to Wasm memory
    - Call a JS function called `indexNodes`
        - Crawl the rendered DOM in a known order, storing a reference to each node into a JS array
        - We can now refer to each DOM node by an integer - its index in that array
  - Wasm
    - Decode `initData` from JSON
    - Recreate the same static view that the server generated, by running the same steps
      - Call `app.init` to convert `initData` to `state`
      - Call `app.render` to get the initial view
      - Call `translateStatic` on the view to get the static version (no event handlers or lazy nodes)
    - Call a Roc function called `indexNodes` (same name as the JS function mentioned above)
      - Crawl the _virtual_ tree in the same order as JS crawled the real DOM tree, assigning an index to each node
      - This index will match the index of the corresponding _real_ DOM node in the JS array
    - Run a diff between the dynamic and static versions of the virtual DOM
      - They will be almost identical, except for the event listeners (and perhaps some DOM properties)
      - The app is now fully initialised

## Short term TODO

- [x] JS to load and initialize WebAssembly
- [ ] Make the Wasm part of the host, for allocators and stuff (probably Zig?)
- [ ] Prototype without full diffing. Instead just always replace the whole view, for now.
- [x] Events in Roc, with CyclicStructureAccessor type, etc.
- [x] Rewrite TypeScript as JavaScript with JSDoc comments, since that still works 100% the same!
- [ ] Implement Vdom diff in Roc (WIP, depends on planned type checker improvements)
