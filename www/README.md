# www.roc-lang.org

To view the website after you've made a change, execute:
```bash
./www/build.sh
cd www/build
simple-http-server --nocache # If you're using the nix flake simple-http-server will already be installed. Without nix you can install it with `cargo install simple-http-server`.
```
Open http://0.0.0.0:8000 in your browser.



