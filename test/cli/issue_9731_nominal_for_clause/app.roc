app [Model, program] { pf: platform "./platform/platform.roc" }
import pf.App
Model := { count : U64 }
program = { init!: init! }
init! : App.Init(Model)
init! = App.init(make_model)
make_model : {} => Model
make_model = |_host| { count: 0 }
