## Getting started

- Install the compiler, see [here](../BUILDING_FROM_SOURCE)
- On ubuntu run the following to make the clipboard work:
```
sudo apt install libxcb-render0-dev libxcb-shape0-dev libxcb-xfixes0-dev
```
- Run the following from the roc folder:

```
cargo run edit
```

## Troubleshooting

If you encounter an error like `gfx_backend_vulkan ... Failed to detect any valid GPUs in the current config ...` make sure the correct graphics card drivers are installed. On ubuntu `sudo ubuntu-drivers autoinstall` can resolve the problem.

If you encounter problems with integrated graphics hardware, install `mesa-vulkan-drivers` and `vulkan-tools`.

## Inspiration

We thank the following open source projects in particular for inspiring us when designing the Roc editor:
- [learn-wgpu](https://github.com/sotrh/learn-wgpu)
- [rgx](https://github.com/cloudhead/rgx)
- [elm-editor](https://github.com/jxxcarlson/elm-editor)
- [iced](https://github.com/hecrj/iced)