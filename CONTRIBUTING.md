# Contributing

## Code of Conduct

We are committed to providing a friendly, safe and welcoming environment for all. See our [Code of Conduct](code_of_conduct.md) for details.

## How to contribute

All contributions are appreciated! Typo fixes, bug fixes, feature requests,
bug reports...

For ideas, proposals and feature request, [click here](https://www.roc-lang.org/community#ideas), for all other contributions, read on.

For a good place to start, check out [good first issues](https://github.com/roc-lang/roc/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22) or reach out on the `#contributing` channel on [our zulip group chat][roc-zulip].
Before making your first pull request, talk to an existing contributor on [zulip][roc-zulip] about what you plan to do! This can avoid duplicated effort or a rejection because the change doesn't fit with the goals of the language.

If you are interested in larger, implementation- or research-heavy projects
related to Roc, check out [Roc Project Ideas][project-ideas] and reach out to us
on [zulip][roc-zulip]! These projects may be suitable for academic theses, internships,
independent research, or just valuable projects to learn from and improve Roc with.

## Building from Source

Check [Building from source](BUILDING_FROM_SOURCE.md) for instructions.

## Running Tests

Most contributors execute the following commands before pushing their code:

```sh
cargo test --release
cargo fmt --all -- --check
cargo clippy --workspace --tests -- --deny warnings
```

Execute `cargo fmt --all` to fix the formatting.

## Contribution Tips

- If you've never made a pull request on github before, [this](https://www.freecodecamp.org/news/how-to-make-your-first-pull-request-on-github-3/) will be a good place to start.
- You can find good first issues [here][good-first-issues]. Once you have gained some experience you can take a look at the [intermediate issues](https://github.com/roc-lang/roc/issues?q=is%3Aopen+is%3Aissue+label%3A%22intermediate+issue%22).
- It's a good idea to open a draft pull request as you begin working on something. This way, others can see that you're working on it, which avoids duplicate effort, and others can give important feedback sooner rather than later. Click the button "ready for review" when it's ready.
- The [compiler's README](https://github.com/roc-lang/roc/tree/main/crates/compiler) contains important info.
- The AI chat in the [cursor editor](https://www.cursor.com/) can also help you find your way in the codebase.

### Debugging tips

If you need to do some debugging, check out [our tips](devtools/debug_tips.md).

### Commit signing

All your commits need to be signed [to prevent impersonation](https://dev.to/martiliones/how-i-got-linus-torvalds-in-my-contributors-on-github-3k4g).  
Check out [our guide for commit signing](devtools/signing.md).

<details>
<summary>⚠️ Tip for NixOS users</summary>

On NixOS pinentry can cause problems, the following setup works well for those with a KDE desktop. From `/etc/nixos/configuration.nix`:
```
programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "qt";
    enableSSHSupport = true;
  };
```
</details>

<details>
<summary>Forgot to sign commits?</summary>

:exclamation: Make sure [to set up signing on your device](devtools/signing.md) first, then continue below.

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

If you already pushed unsigned commits, you may have to do a force push with `git push origin -f <branch_name>`.

</details>

## Can we do better?

Feel free to open an issue if you think this document can be improved!

[roc-zulip]: https://roc.zulipchat.com
[good-first-issues]: https://github.com/roc-lang/roc/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22
[project-ideas]: https://docs.google.com/document/d/1mMaxIi7vxyUyNAUCs98d68jYj6C9Fpq4JIZRU735Kwg/edit?usp=sharing
