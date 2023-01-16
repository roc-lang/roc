app "example-client"
    packages { pf: "platform/client-side.roc" }
    imports [ExampleApp.{ exampleApp }]
    provides [app] to pf

app = exampleApp
