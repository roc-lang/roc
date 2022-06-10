app "app"
    packages { pf: "." }
    imports []
    provides [main] to pf

main = {
    default: Job {
        command: Command {
            tool: SystemTool { name: "test", num: 42 }
        },
        inputFiles : ["foo"]
    }
}

