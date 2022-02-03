app "hello-rust"
    packages { pf: "platform" }
    imports [ Rbt.{ Rbt, Job, job, exec } ]
    provides [ main ] to pf

main = "Hello, World!"

# todo: bikeshed "init" name more
init : Rbt
init =
    Rbt.init { default: bundle }

bundle : Job
bundle =
    job
        {
            command: exec,
            inputs: [],
        }
