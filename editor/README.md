## Getting started

Run the following from the roc folder:

```
cargo run edit
```

## Troubleshooting

If you encounter an error like `gfx_backend_vulkan ... Failed to detect any valid GPUs in the current config ...` make sure the correct graphics card drivers are installed. On ubuntu `sudo ubuntu-drivers autoinstall` can resolve the problem.