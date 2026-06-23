app [Model, program] { pf: platform "./platform/platform.roc" }

import pf.App

Model : {}

program = { init!: init! }

init! : App.Init(Model)
init! = App.init(|_host| Err(StartupFailed))
