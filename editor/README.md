## Getting started

Run the following from the roc folder:

```
cargo run edit
```

## Troubleshooting

If you encounter an error like `gfx_backend_vulkan ... Failed to detect any valid GPUs in the current config ...` make sure the correct graphics card drivers are installed. On ubuntu `sudo ubuntu-drivers autoinstall` can resolve the problem.

## Inspiration

We thank the following open source projects in particular for inspiring us when designing the Roc editor:
- [learn-wgpu](https://github.com/sotrh/learn-wgpu)
- [rgx](https://github.com/cloudhead/rgx)
- [Elm](https://github.com/elm/compiler)
- [elm-editor](https://github.com/jxxcarlson/elm-editor)