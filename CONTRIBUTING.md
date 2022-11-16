# Contributing

## Code of Conduct

We are committed to providing a friendly, safe and welcoming environment for all. Make sure to take a look at the [Code of Conduct](CODE_OF_CONDUCT.md)!

## How to contribute

All contributions are appreciated! Typo fixes, bug fixes, feature requests,
bug reports are all helpful for the project.

If you are looking for a good place to start, consider reaching out on the `#contributing` channel on [Roc Zulip][roc-zulip].
Before making your first pull request, definitely talk to an existing contributor on [Roc Zulip][roc-zulip] first about what you plan to do! This can not only avoid duplicated effort, it can also avoid making a whole PR only to discover it won't be accepted because the change doesn't fit with the goals of the language's design or implementation.

If you are interested in larger, implementation- or research-heavy projects
related to Roc, check out [Roc Project Ideas][project-ideas] and reach out to us
on Zulip! These projects may be suitable for academic theses, independent
research, or even just valuable projects to learn from and improve Roc with.

## Building from Source

Check [Building from source](BUILDING_FROM_SOURCE.md) for instructions.

## Running Tests

Most contributors execute the following commands before pushing their code:

```sh
cargo test
cargo fmt --all -- --check
cargo clippy --workspace --tests -- --deny warnings
```

Execute `cargo fmt --all` to fix the formatting.

## Generating Docs

If you make changes to [Roc's Standard Library](https://www.roc-lang.org/builtins/Str), you can add comments to the code following [the CommonMark Spec](https://spec.commonmark.org/current/) to further explain your intentions. You can view these changes locally with:

```sh
cargo run docs crates/compiler/builtins/roc
```

This command will generate the documentation in the [`generated-docs`](generated-docs) directory.

## Contribution Tips

- If you've never made a pull request on github before, [this](https://www.freecodecamp.org/news/how-to-make-your-first-pull-request-on-github-3/) will be a good place to start.
- Create an issue if the purpose of a struct/field/type/function/... is not immediately clear from its name or nearby comments.
- You can find good first issues [here][good-first-issues]. Once you have gained some experience you can take a look at the [intermediate issues](https://github.com/roc-lang/roc/issues?q=is%3Aopen+is%3Aissue+label%3A%22intermediate+issue%22).
- [Fork](https://github.com/roc-lang/roc/fork) the repo so that you can apply your changes first on your own copy of the roc repo.
- It's a good idea to open a draft pull request as you begin working on something. This way, others can see that you're working on it, which avoids duplicate effort, and others can give feedback sooner rather than later if they notice a problem in the direction things are going. Click the button "ready for review" when it's ready.
- All your commits need to be signed [to prevent impersonation](https://dev.to/martiliones/how-i-got-linus-torvalds-in-my-contributors-on-github-3k4g):
  - If you don't have signing set up on your device and you only want to change a single file, it will be easier to use [github's edit button](https://docs.github.com/en/repositories/working-with-files/managing-files/editing-files). This will sign your commit automatically.
  - For multi-file or complex changes you will want to set up signing on your device:
    1. If you have a Yubikey, follow [guide 1](https://dev.to/paulmicheli/using-your-yubikey-to-get-started-with-gpg-3h4k), [guide 2](https://dev.to/paulmicheli/using-your-yubikey-for-signed-git-commits-4l73) and skip the steps below.
    2. [Make a key to sign your commits.](https://docs.github.com/en/authentication/managing-commit-signature-verification/generating-a-new-gpg-key)
    3. [Configure git to use your key.](https://docs.github.com/en/authentication/managing-commit-signature-verification/telling-git-about-your-signing-key)
    4. Make git sign your commits automatically:

     ```sh
     git config --global commit.gpgsign true
     ```

### Forgot to sign commits?

You can view your commits on github, those without the "Verified" badge still need to be signed.
If any of those is a merge commit, follow [these steps](https://stackoverflow.com/a/9958215/4200103) instead of the ones below.

If you have only one commit, running `git commit --amend --no-edit -S` would sign the latest commit 🚀.

In case you have multiple commits, you can sign them in two ways:
 1. Switching to interactive rebase mode and editing the file:
       - Enter into interactive mode, by running `git rebase -i HEAD~n` where `n` is the number of commits up to the most current commit you would like to see.
       - This would display a set of commits in a text file like below:
           ```
           pick hash2 commit message 2
           pick hash1 commit message 1
           ```
       - On a new line below a commit you want to sign, add `exec git commit --amend --no-edit -S`. Do this for all your unsigned commits.
 2. Or run git rebase recursively:
       - Find the oldest commit you want to sign, using the `git log --show-signature` command. 
       - Run the command `git rebase --exec 'git commit --amend --no-edit -n -S' -i HASH` which would sign all commits up to commit `HASH`.

If you already pushed unsigned commits, you mmay have to do a force push with `git push origin -f <branch_name>`.

## Can we do better?

Feel free to open an issue if you think this document can be improved or is unclear in any way.

[roc-zulip]: https://roc.zulipchat.com
[good-first-issues]: https://github.com/roc-lang/roc/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22
[project-ideas]: https://docs.google.com/document/d/1mMaxIi7vxyUyNAUCs98d68jYj6C9Fpq4JIZRU735Kwg/edit?usp=sharing
