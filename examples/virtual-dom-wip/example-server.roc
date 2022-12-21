app "example-server"
    packages { pf: "platform/server-side.roc" }
    imports [ExampleApp.{ exampleApp }]
    provides [app] to pf

app = exampleApp
