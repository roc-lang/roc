app "hello-rust"
    packages { pf: "platform" }
    imports [ Rbt.{ Rbt, Tool, tool, systemTool, Job, job, exec } ]
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
            command:
                exec (systemTool "esbuild")
                    [],
            inputs: [],
        }
