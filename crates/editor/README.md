# :construction: Work In Progress :construction:

The editor is a work in progress, only a limited subset of Roc expressions are currently supported.

Unlike most editors, we use projectional or structural editing to edit the [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) directly. This will allow for cool features like excellent auto-complete, refactoring and never needing to format your code.

## Getting started

- Install the compiler, see [here](../BUILDING_FROM_SOURCE.md).
- Run the following from the roc folder:

```sh
cargo run edit
```

## Troubleshooting

If you encounter problems with integrated graphics hardware, install `mesa-vulkan-drivers` and `vulkan-tools`.

If you encounter an error like `gfx_backend_vulkan ... Failed to detect any valid GPUs in the current config ...` make sure the correct graphics card drivers are installed. On ubuntu `sudo ubuntu-drivers autoinstall` can resolve the problem.
If the error persists, take a look [here](https://www.techpowerup.com/gpu-specs/) to see if your GPU supports vulkan.
Use of OpenGL instead of vulkan should be available in several months.

Make sure to [create an issue](https://github.com/roc-lang/roc/issues/new/choose) if you encounter any problems not listed above.

## Inspiration

We thank the following open source projects in particular for inspiring us when designing the Roc editor:

- [learn-wgpu](https://github.com/sotrh/learn-wgpu)
- [rgx](https://github.com/cloudhead/rgx)
- [elm-editor](https://github.com/jxxcarlson/elm-editor)
- [iced](https://github.com/hecrj/iced)

## How does it work?

To take a look behind the scenes, open the editor with `./roc edit` or `cargo run edit` and press F11.
This debug view shows important data structures that can be found in `editor/src/editor/mvc/ed_model.rs`.
Add or delete some code to see how these data structures are updated.

From roc to render:

- `./roc edit` or `cargo run edit` is first handled in `cli/src/main.rs`, from there the editor's launch function is called (`editor/src/editor/main.rs`).
- In `run_event_loop` we initialize the winit window, wgpu, the editor's model(`EdModel`) and start the rendering loop.
- The `ed_model` is filled in part with data obtained by loading and typechecking the roc file with the same function (`load_and_typecheck`) that is used by the compiler.
- `ed_model` also contains an `EdModule`, which holds the parsed abstract syntax tree (AST).
- In the `init_model` function:
  - The AST is converted into a tree of `MarkupNode`. The different types of `MarkupNode` are similar to the elements/nodes in HTML. A line of roc code is represented as a nested `MarkupNode` containing mostly text `MarkupNode`s. The line `foo = "bar"` is represented as
    three text `MarkupNode`; representing `foo`, ` = ` and `bar`. Multiple lines of roc code are represented as nested `MarkupNode` that contain other nested `MarkupNode`.
  - `CodeLines` holds a `Vec` of `String`, each line of code is a `String`. When saving the file, the content of `CodeLines` is written to disk.
  - `GridNodeMap` maps every position of a char of roc code to a `MarkNodeId`, for easy interaction with the caret.
- Back in `editor/src/editor/main.rs` we convert the `EdModel` to `RenderedWgpu` by calling `model_to_wgpu`.
- The `RenderedWgpu` is passed to the `glyph_brush` to draw the characters(glyphs) on the screen.

### Important files

To understand how the editor works it is useful to know the most important files:

- editor/src/editor/main.rs
- editor/src/editor/mvc/ed_update.rs
- editor/src/editor/mvc/ed_model.rs
- editor/src/editor/mvc/ed_view.rs
- editor/src/editor/render_ast.rs
- editor/src/editor/render_debug.rs

Important folders/files outside the editor folder:

- code_markup/src/markup/convert
- code_markup/src/markup/nodes.rs
- ast/src/lang/core/def
- ast/src/lang/core/expr
- ast/src/lang/core/ast.rs
- ast/src/lang/env.rs

## Contributing

We welcome new contributors :heart: and are happy to help you get started.
Check [CONTRIBUTING.md](../CONTRIBUTING.md) for more info.
