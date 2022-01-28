app "hello-gui"
    packages { pf: "platform" }
    imports []
    provides [ render ] to pf

render = "Hello Roc GUI"
