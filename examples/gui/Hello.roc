app "hello-gui"
    packages { pf: "platform" }
    imports []
    provides [ render ] to pf

render = { content: "Hello, World!", title: "Roc GUI" }
