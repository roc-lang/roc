# Contributing

## Code of Conduct

We are committed to providing a friendly, safe and welcoming environment for all. Make sure to take a look at the [Code of Conduct](CodeOfConduct.md)!

## Building from Source

Check [Build from source](BUILDING_FROM_SOURCE.md) for instructions.

## Running Tests

To run all tests and checks as they are run on CI, [install earthly](https://earthly.dev/get-earthly) and run:
```
earthly +test-all
```

Earthly may temporarily use a lot of disk space, up to 90 GB. This disk space is available again after rebooting.

## Contribution Tips

- Before making your first pull request, definitely talk to an existing contributor on [Roc Zulip](https://roc.zulipchat.com) first about what you plan to do! This can not only avoid duplicated effort, it can also avoid making a whole PR only to discover it won't be accepted because the change doesn't fit with the goals of the language's design or implementation.
- It's a good idea to open a work-in-progress pull request as you begin working on something. This way, others can see that you're working on it, which avoids duplicate effort, and others can give feedback sooner rather than later if they notice a problem in the direction things are going. Be sure to include "WIP" in the title of the PR as long as it's not ready for review!
- Make sure to create a branch on the roc repository for your changes. We do not allow CI to be run on forks for security.
- You find good first issues [here](https://github.com/rtfeldman/roc/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

## Can we do better?

Feel free to open an issue if you think this document can be improved or is unclear in any way.
