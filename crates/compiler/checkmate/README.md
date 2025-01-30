# `checkmate`

A tool to debug the solver (checker + inference + specialization engine).

## Usage

If you run into a typechecking bug and want to try to diagnose it, after minimizing, you can run a debug version of the compiler with `ROC_CHECKMATE=1` set.
That will spit out a JSON file of the form `checkmate_<timestamp>.json`.

You can then load them into the UI and investigate the unifications. The UI can
be started with `npm start` in the `www` directory.

Note that you will need Node.js and NPM installed to run the UI. Unlike other
subprojects, this project requires external dependencies because it is fully
optional, as an internal developer tool. A motivated contributor is free to
rewrite the UI in a different framework, including Roc itself.
