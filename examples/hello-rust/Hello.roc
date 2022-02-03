app "hello-rust"
    packages { pf: "platform" }
    imports [ Rbt.{ Rbt, init, Job, job } ]
    provides [ main ] to pf

main = "Hello, World!"

# todo: bikeshed "init" name more
init : Rbt
init =
    Rbt.init bundle

bundle : Job
bundle =
    job Command []
