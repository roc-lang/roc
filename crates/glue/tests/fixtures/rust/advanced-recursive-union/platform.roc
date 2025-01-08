platform "test-platform"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

Tool : [
    SystemTool { name : Str, num : U32 },
    FromJob { job : Job, num : U32 },
]

Command : [Command { tool : Tool }]

Job : [
    Job { command : Command, input_files : List Str },
    Foo Str,
    # TODO make a recursive tag union test that doesn't try to do mutual recursion,
    # just so I can get a PR up.
    # WithTool Tool # Mutual recursion; Tool also references Job
]

Rbt : { default : Job }

main_for_host : {} -> Rbt
main_for_host = \{} -> main
