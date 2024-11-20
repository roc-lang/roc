app [main] { pf: platform "platform.roc" }

main = {
    default: Job {
        command: Command {
            tool: SystemTool { name: "test", num: 42 },
        },
        inputFiles: ["foo"],
    },
}

