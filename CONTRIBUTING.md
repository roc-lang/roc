# Contributing

## Code of Conduct

We are committed to providing a friendly, safe and welcoming environment for all, make sure to take a look at the [Code of Conduct](CodeOfConduct.md)

## Building from Source

Check [Build from source](BUILDING_FROM_SOURCE.md) for instructions.

## Running Tests

To run all tests as they are run on CI, [install earthly](https://earthly.dev/get-earthly) and run:
```
mkdir -p sccache_dir
earthly +test-all
```

Earthly may temporarily use a lot of disk space, up to 90 GB. This disk space is available again after rebooting.

## Contribution Tips

- It is encouraged to open a pull request as you work on an issue. Do include WIP in the title of the PR as long as it is not ready for review.
- If you have an idea for a major change talk to us on [zulip](TODO link) to prevent your work from being rejected after it's done.

## Can we do better?

Feel free to open an issue if you think this document can be improved or is unclear in any way.