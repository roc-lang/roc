app "hello-rust"
    packages { pf: "platform" }
    imports [ Rbt.{ Rbt, Job, job } ]
    provides [ main ] to pf

main = "Hello, World!"

# todo: bikeshed "init" name more
init : Rbt
init =
    Rbt bundle

bundle : Job
bundle =
    job Command []
