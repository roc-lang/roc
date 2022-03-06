To run this example, please make sure to install the following prerequisites for each respective platform.

### Linux

- [speech-dispatcher](https://freebsoft.org/speechd)

### Important note for Linux users

You can get the required dependencies by running (sudo may be required):

```
apt install speech-dispatcher libspeechd-dev libspeechd2
```

Please also check out [these](https://htmlpreview.github.io/?https://github.com/brailcom/speechd/blob/master/doc/speech-dispatcher.html#Running) instructions to make sure speech-dispatcher is working on your distro.

This example must also be run using `--roc-linker` in order to correctly link to the required dependencies as so:

```
cargo run -- --roc-linker ./examples/speak-aloud/Speak.roc
```
