platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

Tool : [
    SystemTool { name : Str, num : U32 },
    FromJob { job : Job, num : U32 }
]

Command : [Command { tool : Tool }]

Job : [
    Job { command : Command, inputFiles : List Str },
    Foo Str,
    # WithTool Tool # Mutual recursion; Tool also references Job
]

Rbt : { default: Job }

mainForHost : Rbt
mainForHost = main
