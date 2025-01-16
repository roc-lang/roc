# Commit Signing Guide

If you don't have signing set up on your device and you only want to make simple changes, it will be easier to use [github's edit button](https://docs.github.com/en/repositories/working-with-files/managing-files/editing-files) for single file changes or [github's online VSCode editor](https://docs.github.com/en/codespaces/the-githubdev-web-based-editor#opening-the-githubdev-editor) for multi-file changes. These tools will sign your commit automatically.

For complex changes you will want to set up signing on your device.
Follow along with the subsection below that applies to you.

If your situation is not listed below, consider adding your steps to help out others.

## Setting up commit signing for the first time

If you have a Yubikey, and use macOS or Linux, follow [guide 1](https://dev.to/paulmicheli/using-your-yubikey-to-get-started-with-gpg-3h4k) and [guide 2](https://dev.to/paulmicheli/using-your-yubikey-for-signed-git-commits-4l73).
For windows with a Yubikey, follow [this guide](https://scatteredcode.net/signing-git-commits-using-yubikey-on-windows/).

Without a Yubikey:
  1. [Make a key to sign your commits.](https://docs.github.com/en/authentication/managing-commit-signature-verification/generating-a-new-gpg-key)
  2. [Configure git to use your key.](https://docs.github.com/en/authentication/managing-commit-signature-verification/telling-git-about-your-signing-key)
  3. Make git sign your commits automatically:

     ```sh
     git config --global commit.gpgsign true
     ```

## Transferring existing key from Linux to Windows

### With Yubikey

This explanation was based on the steps outlined [here](https://scatteredcode.net/signing-git-commits-using-yubikey-on-windows/).

On linux, run:
```sh
gpg --list-keys --keyid-format SHORT | grep ^pub
gpg --export --armor [Your_Key_ID] > public.asc
```

Copy the public.asc file to windows.

Download and install [Gpg4win](https://www.gpg4win.org/get-gpg4win.html).

Open the program Kleopatra (installed with gpg4win) and go to Smartcards.
You should see your Yubikey there, it should also say something like `failed to find public key locally`. Click the import button and open the `public.asc` file you created earlier.
Close Kleopatra.

Install the [YubiKey Minidriver for 64-bit systems – Windows Installer](https://www.yubico.com/support/download/smart-card-drivers-tools/).

Insert your Yubikey and check if it is mentioned in the output of `gpg --card-status` (powershell).

Open powershell and execute:
```sh
git config --global gpg.program "c:\Program Files (x86)\GnuPG\bin\gpg.exe"
git config --global commit.gpgsign true
gpg --list-secret-keys --keyid-format LONG
```
The last command will show your keyid. On the line that says `[SC]`, copy the id.
In the example below the id is 683AB68D867FEB5C
```sh
sec>  rsa4096/683AB68D867FEB5C 2020-02-02 [SC] [expires: 2022-02-02]
```

Tell git your keyid:
```sh
>git config --global user.signingkey YOUR_KEY_ID_HERE
```

That's it!

### Without Yubikey

TODO

 
