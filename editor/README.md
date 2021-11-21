
The editor is a work in progress, only a limited subset of Roc expressions are currently supported.

Unlike most editors, we use projectional or structural editing to edit the [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) directly. This will allow for cool features like excellent auto-complete and refactoring.

## Getting started

- Install the compiler, see [here](../BUILDING_FROM_SOURCE.md).
- Run the following from the roc folder:

```
cargo run edit
```

## Troubleshooting

If you encounter problems with integrated graphics hardware, install `mesa-vulkan-drivers` and `vulkan-tools`.

If you encounter an error like `gfx_backend_vulkan ... Failed to detect any valid GPUs in the current config ...` make sure the correct graphics card drivers are installed. On ubuntu `sudo ubuntu-drivers autoinstall` can resolve the problem.
If the error persists, take a look [here](https://www.techpowerup.com/gpu-specs/) to see if your GPU supports vulkan.
Use of OpenGL instead of vulkan should be available in several months.

Make sure to [create an issue](https://github.com/rtfeldman/roc/issues/new/choose) if you encounter any problems not listed above.

## Inspiration

We thank the following open source projects in particular for inspiring us when designing the Roc editor:
- [learn-wgpu](https://github.com/sotrh/learn-wgpu)
- [rgx](https://github.com/cloudhead/rgx)
- [elm-editor](https://github.com/jxxcarlson/elm-editor)
- [iced](https://github.com/hecrj/iced)
