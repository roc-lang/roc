app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main = {
    default: Job {
        command: Command {
            tool: SystemTool { name: "test" },
            args: [],
        },
        job: [],
        inputFiles : ["foo"],
    }
}

