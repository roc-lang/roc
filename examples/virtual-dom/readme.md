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

## Server side rendering

Principles
- We use the same library for static and dymnamic HTML.
- Dynamic code produces `Html state` but the functions that render static HTML take `Html []`. The `[]` type is an empty tag union, and it's impossible to create a value of that type. If you want to pre-render a static version of a view, you can pass it to `translateStatic : Html state -> Html []`
- A static `Html []` tree cannot contain any `Lazy` nodes or event handlers.
- A static `Html []` tree can only contain HTML _attributes_, not DOM _properties_. The difference between the two is rather [pedantic and crazy](https://github.com/elm/html/blob/master/properties-vs-attributes.md), and I'm not sure if it has any practical real-world implications or not.

The app is represented as
```elm
App state initData : {
    static : Html [],
    initDynamic : initData -> state,
    renderDynamic : state -> Dict HtmlId (Html state),
} | initData has Encoding
```

Steps
- Server side
  - Convert `initData` to `state`
  - Call `renderDynamic` to get the initial dynamic views
  - Call `translateStatic` on each dynamic view to get the static version
  - Find the relevant HTML IDs in the `static` HTML and replace those nodes with the initial dynamic views
  - Insert a `<script>` to load and initialize the Wasm app. This includes JSON for `initData` and HTML IDs for the dynamic roots.
- Client side
  - JS
    - Load the Wasm module
    - Call `roc_alloc` in the Wasm module
    - Write the `initData` JSON string to Wasm memory
    - Call a JS function called `indexNodes`
        - Find the dynamic subtrees by their HTML IDs
        - Crawl those subtrees in a well-defined order, storing a reference to each node into a JS array
        - We can now refer to each DOM node by an integer - its index in that array
  - Wasm
    - Decode `initData` from JSON
    - Transform `initData` into the initial `state`
    - Call `renderDynamic` to get the initial dynamic views
    - Call `translateStatic` on each dynamic view to get the static version that matches the initial HTML
    - Call a Roc function called `indexNodes` (same name as the JS function we discussed above)
        - Crawl the _virtual_ tree in the same order as JS crawled the real DOM tree, assigning an index to each node
        - This index will match the index of the corresponding _real_ DOM node in the JS array
    - Run a diff between the dynamic and static versions of the virtual DOM
        - They will be almost identical, except for the event listeners that we stripped out of the static HTML
        - The app is now fully initialised

## Short term TODO

- [x] JS to load and initialize WebAssembly
- [ ] Make the Wasm part of the host, for allocators and stuff (probably Zig?)
- [ ] Prototype without virtual DOM. Just call the Effects directly.
- [x] Events in Roc, with CyclicStructureAccessor type, etc.
- [x] Do something about TypeScript (switch to JS permanently? make a build script? No, use JSDoc)
- [ ] Implement Vdom diff in Roc
