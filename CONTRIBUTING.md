# Contributing

## Code of Conduct

We are committed to providing a friendly, safe and welcoming environment for all. Make sure to take a look at the [Code of Conduct](CodeOfConduct.md)!

## Building from Source

Check [Build from source](BUILDING_FROM_SOURCE.md) for instructions.

## Running Tests

Most contributors execute the following commands befor pushing their code:
```
cargo test
cargo fmt --all -- --check
cargo clippy -- -D warnings
```
Execute `cargo fmt --all` to fix the formatting.

If you want to run all tests and checks as they are run on CI, [install earthly](https://earthly.dev/get-earthly) and run:
```
earthly +test-all
```

Earthly may temporarily use a lot of disk space, up to 90 GB. This disk space is available again after rebooting.

## Contribution Tips

- Create an issue if the purpose of a struct/field/type/function/... is not immediately clear from its name or nearby comments.
- You find good first issues [here](https://github.com/rtfeldman/roc/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).
- Before making your first pull request, definitely talk to an existing contributor on [Roc Zulip](https://roc.zulipchat.com) first about what you plan to do! This can not only avoid duplicated effort, it can also avoid making a whole PR only to discover it won't be accepted because the change doesn't fit with the goals of the language's design or implementation.
- It's a good idea to open a work-in-progress pull request as you begin working on something. This way, others can see that you're working on it, which avoids duplicate effort, and others can give feedback sooner rather than later if they notice a problem in the direction things are going. Be sure to include "WIP" in the title of the PR as long as it's not ready for review!
- Make sure to create a branch on the roc repository for your changes. We do not allow CI to be run on forks for security.
- All your commits need to be signed to prevent impersonation:
  1. If you have a Yubikey, follow [guide 1](https://dev.to/paulmicheli/using-your-yubikey-to-get-started-with-gpg-3h4k), [guide 2](https://dev.to/paulmicheli/using-your-yubikey-for-signed-git-commits-4l73) and skip the steps below.
  2. [Make a key to sign your commits.](https://docs.github.com/en/authentication/managing-commit-signature-verification/generating-a-new-gpg-key).
  3. [Configure git to use your key.](https://docs.github.com/en/authentication/managing-commit-signature-verification/telling-git-about-your-signing-key)
  4. Make git sign your commits automatically:
   ```
   git config --global commit.gpgsign true
   ```

## Can we do better?

Feel free to open an issue if you think this document can be improved or is unclear in any way.
