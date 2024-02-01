app "example-server"
    packages { pf: "platform/server-side.roc" }
    provides [app] to pf

import ExampleApp exposing [exampleApp]

app = exampleApp
