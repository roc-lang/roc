platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

Tool : [SystemTool { name : Str }]

Command : [Command { tool : Tool, args : List Str }]

Job : [Job { command : Command, job : List Job, input_files : List Str }, Blah Str]

Rbt : { default : Job }

main_for_host : Rbt
main_for_host = main
