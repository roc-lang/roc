# www.roc-lang.org

## Prerequisites

- Linux or MacOS operating system, Windows users can use linux through WSL.
- Install [git](https://chat.openai.com/share/71fb3ae6-80d7-478c-8a27-a36aaa5ba921)
- Install [nix](https://github.com/roc-lang/roc/blob/main/BUILDING_FROM_SOURCE.md#installing-nix)

## Building the website from scratch

```bash
git clone https://github.com/roc-lang/roc.git
cd roc
nix develop
./www/build.sh
# make the roc command available 
export PATH="$(pwd)/target/release/:$PATH"
bash ./www/build-dev-local.sh
```

Open http://0.0.0.0:8080 in your browser.

If you want to build the repl as well, check out crates/repl_wasm/README.md.

## After you've made a change locally

In the terminal where the web server is running:
1. kill the server with Ctrl+C
2. re-run the build script 
3. refresh the page in your browser

To view the website after you've made a change, execute:
```bash
bash build-dev-local.sh
```
Open http://0.0.0.0:8080 in your browser.



