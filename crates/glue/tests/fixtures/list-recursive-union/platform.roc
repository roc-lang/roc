platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

Tool : [SystemTool { name : Str }]

Command : [Command { tool : Tool, args : List Str }]

Job : [Job { command : Command, job : List Job, inputFiles : List Str }, Blah Str]

Rbt : { default : Job }

mainForHost : Rbt
mainForHost = main
