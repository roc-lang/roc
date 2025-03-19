# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +ir -print
# cor +eval -print

# https://github.com/roc-lang/roc/issues/5464
Op a : [
    StdoutLine Str ({} -> Op a),
    StdinLine (Str -> Op a),
    Done a,
]

Task ok err op : ([ Ok ok, Err err ] -> op) -> op

sig succeed : ok -> Task ok * *
let succeed = \ok -> \toNext -> toNext (Ok ok);;

sig fail : err -> Task * err *
let fail = \err-> \toNext -> toNext (Err err);;

sig await : Task ok1 err op -> (ok1 -> Task ok2 err op) -> Task ok2 err op
let await = \fromResult -> \next ->
    \continue -> fromResult (\result ->
        let inner = when result is
            | Ok v -> next v
            | Err e -> fail e
        end
        in
        inner continue)
;;


sig outLine : Str -> Task {} * (Op *)
let outLine = \s -> (\toNext -> StdoutLine s (\x -> toNext (Ok x)));;

sig inLine : Task Str * (Op *)
let inLine = \toNext -> StdinLine (\s -> toNext (Ok s));;

sig main : Task {} * (Op *)
let main =
    await (outLine "What's your first name?")
        (\x -> await (inLine)
            (\firstName -> await (outLine "What's your last name?")
                (\y -> await (inLine)
                    (\lastName -> outLine (~str_concat "Hello " firstName " " lastName "!")))))
;;

run main_handler =
#   ^^^^^^^^^^^^
    let op = main (\x -> Done x) in
#       ^^
    let handle = \op -> \i -> \t -> when op is
#       ^^^^^^
        | StdinLine f -> handle (f (~str_concat "stdin" (~itos i))) (~add i 1) (Stdin t)
        | StdoutLine s f -> handle (f {}) (~add i 1) (Stdout s t)
        | Done x -> Done x t
    end
    in
    handle op 0 EntryPoint
;;

> cor-out +monotype -print
> let fail1: []
>              -> ([Err [], Ok {}]
>                   -> [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -> <rec>),
>                        StdoutLine Str ({} -> <rec>)
>                        ])
>                   -> [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -> <rec>),
>                        StdoutLine Str ({} -> <rec>)
>                        ] = \err ->
>   \toNext1 -> (toNext1 (Err err))
> let inLine1: ([Err [], Ok Str]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ] = \toNext3 ->
>   StdinLine \s1 -> (toNext3 (Ok s1))
> let await2: (([Err [], Ok Str]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> (Str
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ] = \fromResult ->
>   \next ->
>     \continue ->
>       (fromResult
>          \result ->
>            (let inner: ([Err [], Ok {}]
>                          -> [
>                               Done [Err [], Ok {}],
>                               StdinLine (Str -> <rec>),
>                               StdoutLine Str ({} -> <rec>)
>                               ])
>                          -> [
>                               Done [Err [], Ok {}],
>                               StdinLine (Str -> <rec>),
>                               StdoutLine Str ({} -> <rec>)
>                               ] =
>               when result is
>                 | Ok v -> next v
>                 | Err e -> fail1 e
>               end
>            in
>            inner continue))
> let outLine1: Str
>                 -> ([Err [], Ok {}]
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ])
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ] = \s ->
>   \toNext2 -> (StdoutLine s \x -> (toNext2 (Ok x)))
> let await1: (([Err [], Ok {}]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> ({}
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ] = \fromResult ->
>   \next ->
>     \continue ->
>       (fromResult
>          \result ->
>            (let inner: ([Err [], Ok {}]
>                          -> [
>                               Done [Err [], Ok {}],
>                               StdinLine (Str -> <rec>),
>                               StdoutLine Str ({} -> <rec>)
>                               ])
>                          -> [
>                               Done [Err [], Ok {}],
>                               StdinLine (Str -> <rec>),
>                               StdoutLine Str ({} -> <rec>)
>                               ] =
>               when result is
>                 | Ok v -> next v
>                 | Err e -> fail1 e
>               end
>            in
>            inner continue))
> let main: ([Err [], Ok {}]
>             -> [
>                  Done [Err [], Ok {}],
>                  StdinLine (Str -> <rec>),
>                  StdoutLine Str ({} -> <rec>)
>                  ])
>             -> [
>                  Done [Err [], Ok {}],
>                  StdinLine (Str -> <rec>),
>                  StdoutLine Str ({} -> <rec>)
>                  ] =
>   (await1 (outLine1 "What's your first name?"))
>     \x1 ->
>       ((await2 inLine1)
>          \firstName ->
>            ((await1 (outLine1 "What's your last name?"))
>               \y ->
>                 ((await2 inLine1)
>                    \lastName ->
>                      (outLine1 ~str_concat "Hello " firstName " " lastName "!"))))
> run main_handler: [
>                     Done [Err [], Ok {}]
>                       [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                     ] =
>   let op: [
>             Done [Err [], Ok {}],
>             StdinLine (Str -> <rec>),
>             StdoutLine Str ({} -> <rec>)
>             ] =
>     main \x2 -> (Done x2)
>   in
>   let rec handle: [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ]
>                     -> Int
>                          -> [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                               -> [
>                                    Done [Err [], Ok {}]
>                                      [
>                                        EntryPoint,
>                                        Stdin <rec>,
>                                        Stdout Str <rec>
>                                        ]
>                                    ] = \op1 ->
>     \i ->
>       \t ->
>         when op1 is
>           | StdinLine f ->
>             ((handle (f ~str_concat "stdin" ~itos i)) ~add i 1) (Stdin t)
>           | StdoutLine s2 f1 -> ((handle (f1 {})) ~add i 1) (Stdout s2 t)
>           | Done x3 -> Done x3 t
>         end
>   in
>   ((handle op) 0) (EntryPoint )

> cor-out +monotype_lifted -print
> let clos(err: []): ([Err [], Ok {}]
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ])
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ] = \toNext1 ->
>   toNext1 (Err err)
> let fail1: []
>              -> ([Err [], Ok {}]
>                   -> [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -> <rec>),
>                        StdoutLine Str ({} -> <rec>)
>                        ])
>                   -> [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -> <rec>),
>                        StdoutLine Str ({} -> <rec>)
>                        ] = \err ->
>   clos
> let clos1(toNext3:
>             [Err [], Ok Str]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]): Str
>                          -> [
>                               Done [Err [], Ok {}],
>                               StdinLine <rec>,
>                               StdoutLine Str ({} -> <rec>)
>                               ] = \s1 ->
>   toNext3 (Ok s1)
> let inLine1: ([Err [], Ok Str]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ] = \toNext3 ->
>   StdinLine clos1
> let clos2(continue:
>             [Err [], Ok {}]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]
>            next:
>              Str
>                -> ([Err [], Ok {}]
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ])
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ]): [Err [], Ok Str]
>                                -> [
>                                     Done [Err [], Ok {}],
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str ({} -> <rec>)
>                                     ] = \result ->
>   let inner: ([Err [], Ok {}]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ] =
>     when result is
>       | Ok v -> next v
>       | Err e -> fail1 e
>     end
>   in
>   inner continue
> let clos3(fromResult:
>             ([Err [], Ok Str]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]
>            next:
>              Str
>                -> ([Err [], Ok {}]
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ])
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ]): ([Err [], Ok {}]
>                                -> [
>                                     Done [Err [], Ok {}],
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str ({} -> <rec>)
>                                     ])
>                                -> [
>                                     Done [Err [], Ok {}],
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str ({} -> <rec>)
>                                     ] = \continue ->
>   fromResult clos2
> let clos4(fromResult:
>             ([Err [], Ok Str]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]): (Str
>                          -> ([Err [], Ok {}]
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ])
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ])
>                          -> ([Err [], Ok {}]
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ])
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ] = \next ->
>   clos3
> let await2: (([Err [], Ok Str]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> (Str
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ] = \fromResult ->
>   clos4
> let clos5(toNext2:
>             [Err [], Ok {}]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]): {}
>                          -> [
>                               Done [Err [], Ok {}],
>                               StdinLine (Str -> <rec>),
>                               StdoutLine Str <rec>
>                               ] = \x ->
>   toNext2 (Ok x)
> let clos6(s: Str): ([Err [], Ok {}]
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ])
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ] = \toNext2 ->
>   StdoutLine s clos5
> let outLine1: Str
>                 -> ([Err [], Ok {}]
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ])
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ] = \s ->
>   clos6
> let clos7(continue:
>             [Err [], Ok {}]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]
>            next:
>              {}
>                -> ([Err [], Ok {}]
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ])
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ]): [Err [], Ok {}]
>                                -> [
>                                     Done [Err [], Ok {}],
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str ({} -> <rec>)
>                                     ] = \result ->
>   let inner: ([Err [], Ok {}]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ] =
>     when result is
>       | Ok v -> next v
>       | Err e -> fail1 e
>     end
>   in
>   inner continue
> let clos8(fromResult:
>             ([Err [], Ok {}]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]
>            next:
>              {}
>                -> ([Err [], Ok {}]
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ])
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ]): ([Err [], Ok {}]
>                                -> [
>                                     Done [Err [], Ok {}],
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str ({} -> <rec>)
>                                     ])
>                                -> [
>                                     Done [Err [], Ok {}],
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str ({} -> <rec>)
>                                     ] = \continue ->
>   fromResult clos7
> let clos9(fromResult:
>             ([Err [], Ok {}]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]): ({}
>                          -> ([Err [], Ok {}]
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ])
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ])
>                          -> ([Err [], Ok {}]
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ])
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ] = \next ->
>   clos8
> let await1: (([Err [], Ok {}]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> ({}
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ] = \fromResult ->
>   clos9
> let clos10(firstName: Str): Str
>                               -> ([Err [], Ok {}]
>                                    -> [
>                                         Done [Err [], Ok {}],
>                                         StdinLine (Str -> <rec>),
>                                         StdoutLine Str ({} -> <rec>)
>                                         ])
>                                    -> [
>                                         Done [Err [], Ok {}],
>                                         StdinLine (Str -> <rec>),
>                                         StdoutLine Str ({} -> <rec>)
>                                         ] = \lastName ->
>   outLine1 ~str_concat "Hello " firstName " " lastName "!"
> let clos11(firstName: Str): {}
>                               -> ([Err [], Ok {}]
>                                    -> [
>                                         Done [Err [], Ok {}],
>                                         StdinLine (Str -> <rec>),
>                                         StdoutLine Str ({} -> <rec>)
>                                         ])
>                                    -> [
>                                         Done [Err [], Ok {}],
>                                         StdinLine (Str -> <rec>),
>                                         StdoutLine Str ({} -> <rec>)
>                                         ] = \y ->
>   (await2 inLine1) clos10
> let clos12: Str
>               -> ([Err [], Ok {}]
>                    -> [
>                         Done [Err [], Ok {}],
>                         StdinLine (Str -> <rec>),
>                         StdoutLine Str ({} -> <rec>)
>                         ])
>                    -> [
>                         Done [Err [], Ok {}],
>                         StdinLine (Str -> <rec>),
>                         StdoutLine Str ({} -> <rec>)
>                         ] = \firstName ->
>   (await1 (outLine1 "What's your last name?")) clos11
> let clos13: {}
>               -> ([Err [], Ok {}]
>                    -> [
>                         Done [Err [], Ok {}],
>                         StdinLine (Str -> <rec>),
>                         StdoutLine Str ({} -> <rec>)
>                         ])
>                    -> [
>                         Done [Err [], Ok {}],
>                         StdinLine (Str -> <rec>),
>                         StdoutLine Str ({} -> <rec>)
>                         ] = \x1 ->
>   (await2 inLine1) clos12
> let main: ([Err [], Ok {}]
>             -> [
>                  Done [Err [], Ok {}],
>                  StdinLine (Str -> <rec>),
>                  StdoutLine Str ({} -> <rec>)
>                  ])
>             -> [
>                  Done [Err [], Ok {}],
>                  StdinLine (Str -> <rec>),
>                  StdoutLine Str ({} -> <rec>)
>                  ] =
>   (await1 (outLine1 "What's your first name?")) clos13
> let clos14: [Err [], Ok {}]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ] = \x2 ->
>   Done x2
> let clos15(i: Int
>             op1:
>               [
>                 Done [Err [], Ok {}],
>                 StdinLine (Str -> <rec>),
>                 StdoutLine Str ({} -> <rec>)
>                 ]): [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                       -> [
>                            Done [Err [], Ok {}]
>                              [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                            ] = \t ->
>   when op1 is
>     | StdinLine f ->
>       ((handle1 (f ~str_concat "stdin" ~itos i)) ~add i 1) (Stdin t)
>     | StdoutLine s2 f1 -> ((handle1 (f1 {})) ~add i 1) (Stdout s2 t)
>     | Done x3 -> Done x3 t
>   end
> let clos16(op1:
>              [
>                Done [Err [], Ok {}],
>                StdinLine (Str -> <rec>),
>                StdoutLine Str ({} -> <rec>)
>                ]): Int
>                      -> [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                           -> [
>                                Done [Err [], Ok {}]
>                                  [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                                ] = \i ->
>   clos15
> let handle1: [
>                Done [Err [], Ok {}],
>                StdinLine (Str -> <rec>),
>                StdoutLine Str ({} -> <rec>)
>                ]
>                -> Int
>                     -> [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                          -> [
>                               Done [Err [], Ok {}]
>                                 [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                               ] = \op1 ->
>   clos16
> run main_handler: [
>                     Done [Err [], Ok {}]
>                       [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                     ] =
>   let op: [
>             Done [Err [], Ok {}],
>             StdinLine (Str -> <rec>),
>             StdoutLine Str ({} -> <rec>)
>             ] =
>     main clos14
>   in
>   ((handle1 op) 0) (EntryPoint )

> cor-out +lambdasolved -print
> let clos(err: []): [Err [], Ok {}]
>                      -<'3170>-> [
>                                   Done [Err [], Ok {}],
>                                   StdinLine (Str -<'3175>-> <rec>),
>                                   StdoutLine Str ({} -<'3172>-> <rec>)
>                                   ]
>                      -[clos (err: [])]-> [
>                                            Done [Err [], Ok {}],
>                                            StdinLine (Str -<'3175>-> <rec>),
>                                            StdoutLine Str ({} -<'3172>-> <rec>)
>                                            ] = \toNext1 ->
>   toNext1 (Err err)
> let fail1: []
>              -[fail1]-> [Err [], Ok {}]
>                           -<'3252>-> [
>                                        Done [Err [], Ok {}],
>                                        StdinLine (Str -<'3257>-> <rec>),
>                                        StdoutLine Str ({} -<'3254>-> <rec>)
>                                        ]
>                           -[clos (err: [])]-> [
>                                                 Done [Err [], Ok {}],
>                                                 StdinLine
>                                                   (Str -<'3257>-> <rec>),
>                                                 StdoutLine Str
>                                                   ({} -<'3254>-> <rec>)
>                                                 ] = \err ->
>   clos
> let clos14: [Err [], Ok {}]
>               -[clos14]-> [
>                             Done [Err [], Ok {}],
>                             StdinLine (Str -<'3284>-> <rec>),
>                             StdoutLine Str ({} -<'3281>-> <rec>)
>                             ] = \x2 ->
>   Done x2
> let clos5(toNext2:
>             [Err [], Ok {}]
>               -<'3311>-> [
>                            Done [Err [], Ok {}],
>                            StdinLine (Str -<'3332>-> <rec>),
>                            StdoutLine Str
>                              ({} -[clos5 (toNext2: <rec>)]-> <rec>)
>                            ]): {}
>                                  -[
>                                     clos5
>                                       (toNext2: ([Err [], Ok {}]
>                                                   -<'3311>-> [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'3332>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  <rec>
>                                                                ]))
>                                     ]-> [
>                                           Done [Err [], Ok {}],
>                                           StdinLine (Str -<'3332>-> <rec>),
>                                           StdoutLine Str <rec>
>                                           ] = \x ->
>   toNext2 (Ok x)
> let clos6(s: Str): [Err [], Ok {}]
>                      -<'3438>-> [
>                                   Done [Err [], Ok {}],
>                                   StdinLine (Str -<'3442>-> <rec>),
>                                   StdoutLine Str
>                                     ({} -[clos5 (toNext2: <rec>)]-> <rec>)
>                                   ]
>                      -[clos6 (s: Str)]-> [
>                                            Done [Err [], Ok {}],
>                                            StdinLine (Str -<'3442>-> <rec>),
>                                            StdoutLine Str
>                                              ({}
>                                                -[
>                                                   clos5
>                                                     (toNext2: ([Err [], Ok {}]
>                                                                 -<'3438>-> 
>                                                                 <rec>))
>                                                   ]-> <rec>)
>                                            ] = \toNext2 ->
>   StdoutLine s clos5
> let outLine1: Str
>                 -[outLine1]-> [Err [], Ok {}]
>                                 -<'3533>-> [
>                                              Done [Err [], Ok {}],
>                                              StdinLine (Str -<'3537>-> <rec>),
>                                              StdoutLine Str
>                                                ({}
>                                                  -[clos5 (toNext2: <rec>)]-> 
>                                                  <rec>)
>                                              ]
>                                 -[clos6 (s: Str)]-> [
>                                                       Done [Err [], Ok {}],
>                                                       StdinLine
>                                                         (Str -<'3537>-> <rec>),
>                                                       StdoutLine Str
>                                                         ({}
>                                                           -[
>                                                              clos5
>                                                                (toNext2: 
>                                                                ([Err [], Ok {}]
>                                                                  -<'3533>-> 
>                                                                  <rec>))
>                                                              ]-> <rec>)
>                                                       ] = \s ->
>   clos6
> let clos1(toNext3:
>             [Err [], Ok Str]
>               -<'3568>-> [
>                            Done [Err [], Ok {}],
>                            StdinLine (Str -[clos1 (toNext3: <rec>)]-> <rec>),
>                            StdoutLine Str ({} -<'3587>-> <rec>)
>                            ]): Str
>                                  -[
>                                     clos1
>                                       (toNext3: ([Err [], Ok Str]
>                                                   -<'3568>-> [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine <rec>,
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -<'3587>-> 
>                                                                    <rec>)
>                                                                ]))
>                                     ]-> [
>                                           Done [Err [], Ok {}],
>                                           StdinLine <rec>,
>                                           StdoutLine Str ({} -<'3587>-> <rec>)
>                                           ] = \s1 ->
>   toNext3 (Ok s1)
> let inLine1: [Err [], Ok Str]
>                -<'3693>-> [
>                             Done [Err [], Ok {}],
>                             StdinLine (Str -[clos1 (toNext3: <rec>)]-> <rec>),
>                             StdoutLine Str ({} -<'3696>-> <rec>)
>                             ]
>                -[inLine1]-> [
>                               Done [Err [], Ok {}],
>                               StdinLine
>                                 (Str
>                                   -[
>                                      clos1
>                                        (toNext3: ([Err [], Ok Str]
>                                                    -<'3693>-> <rec>))
>                                      ]-> <rec>),
>                               StdoutLine Str ({} -<'3696>-> <rec>)
>                               ] = \toNext3 ->
>   StdinLine clos1
> let clos10(firstName: Str): Str
>                               -[clos10 (firstName: Str)]-> [Err [], Ok {}]
>                                                              -<'3834>-> 
>                                                              [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'3839>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                    <rec>)
>                                                                ]
>                                                              -[clos6 (s: Str)]-> 
>                                                              [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'3839>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'3834>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                    <rec>)
>                                                                ] = \lastName ->
>   outLine1 ~str_concat "Hello " firstName " " lastName "!"
> let clos2(continue:
>             [Err [], Ok {}]
>               -<'4065>-> [
>                            Done [Err [], Ok {}],
>                            StdinLine (Str -<'4107>-> <rec>),
>                            StdoutLine Str ({} -<'4105>-> <rec>)
>                            ], next:
>                                 Str
>                                   -<'3876>-> [Err [], Ok {}]
>                                                -<'4065>-> [
>                                                             Done [Err [], Ok {}],
>                                                             StdinLine
>                                                               (Str
>                                                                 -<'4107>-> 
>                                                                 <rec>),
>                                                             StdoutLine Str
>                                                               ({}
>                                                                 -<'4105>-> 
>                                                                 <rec>)
>                                                             ]
>                                                -[clos (err: [])]-> [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'4107>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4105>-> 
>                                                                     <rec>)
>                                                                     ]): 
>   [Err [], Ok Str]
>     -[
>        clos2
>          (continue: ([Err [], Ok {}]
>                       -<'4065>-> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -<'4107>-> <rec>),
>                                    StdoutLine Str ({} -<'4105>-> <rec>)
>                                    ]))
>          (next: (Str
>                   -<'3876>-> [Err [], Ok {}]
>                                -<'4065>-> [
>                                             Done [Err [], Ok {}],
>                                             StdinLine (Str -<'4107>-> <rec>),
>                                             StdoutLine Str
>                                               ({} -<'4105>-> <rec>)
>                                             ]
>                                -[clos (err: [])]-> [
>                                                      Done [Err [], Ok {}],
>                                                      StdinLine
>                                                        (Str -<'4107>-> <rec>),
>                                                      StdoutLine Str
>                                                        ({} -<'4105>-> <rec>)
>                                                      ]))
>        ]-> [
>              Done [Err [], Ok {}],
>              StdinLine (Str -<'4107>-> <rec>),
>              StdoutLine Str ({} -<'4105>-> <rec>)
>              ] = \result ->
>   let inner: [Err [], Ok {}]
>                -<'4065>-> [
>                             Done [Err [], Ok {}],
>                             StdinLine (Str -<'4107>-> <rec>),
>                             StdoutLine Str ({} -<'4105>-> <rec>)
>                             ]
>                -[clos (err: [])]-> [
>                                      Done [Err [], Ok {}],
>                                      StdinLine (Str -<'4107>-> <rec>),
>                                      StdoutLine Str ({} -<'4105>-> <rec>)
>                                      ] =
>     when result is
>       | Ok v -> next v
>       | Err e -> fail1 e
>     end
>   in
>   inner continue
> let clos3(fromResult:
>             [Err [], Ok Str]
>               -[
>                  clos2
>                    (continue: ([Err [], Ok {}]
>                                 -<'4315>-> [
>                                              Done [Err [], Ok {}],
>                                              StdinLine (Str -<'4320>-> <rec>),
>                                              StdoutLine Str
>                                                ({} -<'4317>-> <rec>)
>                                              ]))
>                    (next: (Str
>                             -<'4254>-> [Err [], Ok {}]
>                                          -<'4315>-> [
>                                                       Done [Err [], Ok {}],
>                                                       StdinLine
>                                                         (Str -<'4320>-> <rec>),
>                                                       StdoutLine Str
>                                                         ({} -<'4317>-> <rec>)
>                                                       ]
>                                          -[clos (err: [])]-> [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'4320>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -<'4317>-> 
>                                                                    <rec>)
>                                                                ]))
>                  ]-> [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -<'4320>-> <rec>),
>                        StdoutLine Str ({} -<'4317>-> <rec>)
>                        ]
>               -<'4297>-> [
>                            Done [Err [], Ok {}],
>                            StdinLine (Str -<'4320>-> <rec>),
>                            StdoutLine Str ({} -<'4317>-> <rec>)
>                            ], next:
>                                 Str
>                                   -<'4254>-> [Err [], Ok {}]
>                                                -<'4315>-> [
>                                                             Done [Err [], Ok {}],
>                                                             StdinLine
>                                                               (Str
>                                                                 -<'4320>-> 
>                                                                 <rec>),
>                                                             StdoutLine Str
>                                                               ({}
>                                                                 -<'4317>-> 
>                                                                 <rec>)
>                                                             ]
>                                                -[clos (err: [])]-> [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'4320>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4317>-> 
>                                                                     <rec>)
>                                                                     ]): 
>   [Err [], Ok {}]
>     -<'4315>-> [
>                  Done [Err [], Ok {}],
>                  StdinLine (Str -<'4320>-> <rec>),
>                  StdoutLine Str ({} -<'4317>-> <rec>)
>                  ]
>     -[
>        clos3
>          (fromResult: ([Err [], Ok Str]
>                          -[
>                             clos2
>                               (continue: ([Err [], Ok {}]
>                                            -<'4315>-> [
>                                                         Done [Err [], Ok {}],
>                                                         StdinLine
>                                                           (Str
>                                                             -<'4320>-> 
>                                                             <rec>),
>                                                         StdoutLine Str
>                                                           ({} -<'4317>-> <rec>)
>                                                         ]))
>                               (next: (Str
>                                        -<'4254>-> [Err [], Ok {}]
>                                                     -<'4315>-> [
>                                                                  Done
>                                                                    [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                  StdinLine
>                                                                    (Str
>                                                                     -<'4320>-> 
>                                                                     <rec>),
>                                                                  StdoutLine Str
>                                                                    ({}
>                                                                     -<'4317>-> 
>                                                                     <rec>)
>                                                                  ]
>                                                     -[clos (err: [])]-> 
>                                                     [
>                                                       Done [Err [], Ok {}],
>                                                       StdinLine
>                                                         (Str -<'4320>-> <rec>),
>                                                       StdoutLine Str
>                                                         ({} -<'4317>-> <rec>)
>                                                       ]))
>                             ]-> [
>                                   Done [Err [], Ok {}],
>                                   StdinLine (Str -<'4320>-> <rec>),
>                                   StdoutLine Str ({} -<'4317>-> <rec>)
>                                   ]
>                         -<'4297>-> [
>                                      Done [Err [], Ok {}],
>                                      StdinLine (Str -<'4320>-> <rec>),
>                                      StdoutLine Str ({} -<'4317>-> <rec>)
>                                      ]))
>          (next: (Str
>                   -<'4254>-> [Err [], Ok {}]
>                                -<'4315>-> [
>                                             Done [Err [], Ok {}],
>                                             StdinLine (Str -<'4320>-> <rec>),
>                                             StdoutLine Str
>                                               ({} -<'4317>-> <rec>)
>                                             ]
>                                -[clos (err: [])]-> [
>                                                      Done [Err [], Ok {}],
>                                                      StdinLine
>                                                        (Str -<'4320>-> <rec>),
>                                                      StdoutLine Str
>                                                        ({} -<'4317>-> <rec>)
>                                                      ]))
>        ]-> [
>              Done [Err [], Ok {}],
>              StdinLine (Str -<'4320>-> <rec>),
>              StdoutLine Str ({} -<'4317>-> <rec>)
>              ] = \continue ->
>   fromResult clos2
> let clos4(fromResult:
>             [Err [], Ok Str]
>               -[
>                  clos2
>                    (continue: ([Err [], Ok {}]
>                                 -<'4545>-> [
>                                              Done [Err [], Ok {}],
>                                              StdinLine (Str -<'4549>-> <rec>),
>                                              StdoutLine Str
>                                                ({} -<'4547>-> <rec>)
>                                              ]))
>                    (next: (Str
>                             -<'4541>-> [Err [], Ok {}]
>                                          -<'4545>-> [
>                                                       Done [Err [], Ok {}],
>                                                       StdinLine
>                                                         (Str -<'4549>-> <rec>),
>                                                       StdoutLine Str
>                                                         ({} -<'4547>-> <rec>)
>                                                       ]
>                                          -[clos (err: [])]-> [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'4549>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -<'4547>-> 
>                                                                    <rec>)
>                                                                ]))
>                  ]-> [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -<'4549>-> <rec>),
>                        StdoutLine Str ({} -<'4547>-> <rec>)
>                        ]
>               -<'4505>-> [
>                            Done [Err [], Ok {}],
>                            StdinLine (Str -<'4549>-> <rec>),
>                            StdoutLine Str ({} -<'4547>-> <rec>)
>                            ]): Str
>                                  -<'4541>-> [Err [], Ok {}]
>                                               -<'4545>-> [
>                                                            Done [Err [], Ok {}],
>                                                            StdinLine
>                                                              (Str
>                                                                -<'4549>-> 
>                                                                <rec>),
>                                                            StdoutLine Str
>                                                              ({}
>                                                                -<'4547>-> 
>                                                                <rec>)
>                                                            ]
>                                               -[clos (err: [])]-> [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'4549>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4547>-> 
>                                                                     <rec>)
>                                                                     ]
>                                  -[
>                                     clos4
>                                       (fromResult: ([Err [], Ok Str]
>                                                       -[
>                                                          clos2
>                                                            (continue: 
>                                                            ([Err [], Ok {}]
>                                                              -<'4545>-> 
>                                                              [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'4549>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -<'4547>-> 
>                                                                    <rec>)
>                                                                ]))
>                                                            (next: (Str
>                                                                     -
>                                                                     <'4541>-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'4545>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'4549>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4547>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     [])
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'4549>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4547>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                          ]-> [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'4549>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -<'4547>-> 
>                                                                    <rec>)
>                                                                ]
>                                                      -<'4505>-> [
>                                                                   Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                   StdinLine
>                                                                     (Str
>                                                                     -<'4549>-> 
>                                                                     <rec>),
>                                                                   StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4547>-> 
>                                                                     <rec>)
>                                                                   ]))
>                                     ]-> [Err [], Ok {}]
>                                           -<'4545>-> [
>                                                        Done [Err [], Ok {}],
>                                                        StdinLine
>                                                          (Str -<'4549>-> <rec>),
>                                                        StdoutLine Str
>                                                          ({} -<'4547>-> <rec>)
>                                                        ]
>                                           -[
>                                              clos3
>                                                (fromResult: ([Err [], Ok Str]
>                                                                -[
>                                                                   clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'4545>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'4549>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4547>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -<'4541>-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'4545>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'4549>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4547>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     [])
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'4549>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4547>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                   ]-> 
>                                                                [
>                                                                  Done
>                                                                    [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                  StdinLine
>                                                                    (Str
>                                                                     -<'4549>-> 
>                                                                     <rec>),
>                                                                  StdoutLine Str
>                                                                    ({}
>                                                                     -<'4547>-> 
>                                                                     <rec>)
>                                                                  ]
>                                                               -<'4505>-> 
>                                                               [
>                                                                 Done
>                                                                   [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                 StdinLine
>                                                                   (Str
>                                                                     -
>                                                                     <'4549>-> 
>                                                                     <rec>),
>                                                                 StdoutLine Str
>                                                                   ({}
>                                                                     -
>                                                                     <'4547>-> 
>                                                                     <rec>)
>                                                                 ]))
>                                                (next: (Str
>                                                         -<'4541>-> [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'4545>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'4549>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4547>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     [])
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'4549>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4547>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                              ]-> [
>                                                    Done [Err [], Ok {}],
>                                                    StdinLine
>                                                      (Str -<'4549>-> <rec>),
>                                                    StdoutLine Str
>                                                      ({} -<'4547>-> <rec>)
>                                                    ] = \next ->
>   clos3
> let await2: [Err [], Ok Str]
>               -[
>                  clos2
>                    (continue: ([Err [], Ok {}]
>                                 -<'4778>-> [
>                                              Done [Err [], Ok {}],
>                                              StdinLine (Str -<'4769>-> <rec>),
>                                              StdoutLine Str
>                                                ({} -<'4767>-> <rec>)
>                                              ]))
>                    (next: (Str
>                             -<'4777>-> [Err [], Ok {}]
>                                          -<'4778>-> [
>                                                       Done [Err [], Ok {}],
>                                                       StdinLine
>                                                         (Str -<'4769>-> <rec>),
>                                                       StdoutLine Str
>                                                         ({} -<'4767>-> <rec>)
>                                                       ]
>                                          -[clos (err: [])]-> [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'4769>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -<'4767>-> 
>                                                                    <rec>)
>                                                                ]))
>                  ]-> [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -<'4769>-> <rec>),
>                        StdoutLine Str ({} -<'4767>-> <rec>)
>                        ]
>               -<'4774>-> [
>                            Done [Err [], Ok {}],
>                            StdinLine (Str -<'4769>-> <rec>),
>                            StdoutLine Str ({} -<'4767>-> <rec>)
>                            ]
>               -[await2]-> Str
>                             -<'4777>-> [Err [], Ok {}]
>                                          -<'4778>-> [
>                                                       Done [Err [], Ok {}],
>                                                       StdinLine
>                                                         (Str -<'4769>-> <rec>),
>                                                       StdoutLine Str
>                                                         ({} -<'4767>-> <rec>)
>                                                       ]
>                                          -[clos (err: [])]-> [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'4769>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -<'4767>-> 
>                                                                    <rec>)
>                                                                ]
>                             -[
>                                clos4
>                                  (fromResult: ([Err [], Ok Str]
>                                                  -[
>                                                     clos2
>                                                       (continue: ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                    -<'4778>-> 
>                                                                    [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'4769>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4767>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                       (next: (Str
>                                                                -<'4777>-> 
>                                                                [Err [], Ok {}]
>                                                                  -<'4778>-> 
>                                                                  [
>                                                                    Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -<'4769>-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4767>-> 
>                                                                     <rec>)
>                                                                    ]
>                                                                  -[
>                                                                     clos
>                                                                     (err: 
>                                                                     [])
>                                                                     ]-> 
>                                                                  [
>                                                                    Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -<'4769>-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4767>-> 
>                                                                     <rec>)
>                                                                    ]))
>                                                     ]-> [
>                                                           Done [Err [], Ok {}],
>                                                           StdinLine
>                                                             (Str
>                                                               -<'4769>-> 
>                                                               <rec>),
>                                                           StdoutLine Str
>                                                             ({}
>                                                               -<'4767>-> 
>                                                               <rec>)
>                                                           ]
>                                                 -<'4774>-> [
>                                                              Done
>                                                                [Err [], Ok {}],
>                                                              StdinLine
>                                                                (Str
>                                                                  -<'4769>-> 
>                                                                  <rec>),
>                                                              StdoutLine Str
>                                                                ({}
>                                                                  -<'4767>-> 
>                                                                  <rec>)
>                                                              ]))
>                                ]-> [Err [], Ok {}]
>                                      -<'4778>-> [
>                                                   Done [Err [], Ok {}],
>                                                   StdinLine
>                                                     (Str -<'4769>-> <rec>),
>                                                   StdoutLine Str
>                                                     ({} -<'4767>-> <rec>)
>                                                   ]
>                                      -[
>                                         clos3
>                                           (fromResult: ([Err [], Ok Str]
>                                                           -[
>                                                              clos2
>                                                                (continue: 
>                                                                ([Err [], Ok {}]
>                                                                  -<'4778>-> 
>                                                                  [
>                                                                    Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -<'4769>-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4767>-> 
>                                                                     <rec>)
>                                                                    ]))
>                                                                (next: 
>                                                                (Str
>                                                                  -<'4777>-> 
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ]
>                                                                    -<'4778>-> 
>                                                                    [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'4769>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4767>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                    -[
>                                                                     clos
>                                                                     (err: 
>                                                                     [])
>                                                                     ]-> 
>                                                                    [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'4769>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4767>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                              ]-> [
>                                                                    Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -<'4769>-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4767>-> 
>                                                                     <rec>)
>                                                                    ]
>                                                          -<'4774>-> [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'4769>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4767>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                           (next: (Str
>                                                    -<'4777>-> [Err [], Ok {}]
>                                                                 -<'4778>-> 
>                                                                 [
>                                                                   Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                   StdinLine
>                                                                     (Str
>                                                                     -<'4769>-> 
>                                                                     <rec>),
>                                                                   StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4767>-> 
>                                                                     <rec>)
>                                                                   ]
>                                                                 -[
>                                                                    clos
>                                                                     (err: 
>                                                                     [])
>                                                                    ]-> 
>                                                                 [
>                                                                   Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                   StdinLine
>                                                                     (Str
>                                                                     -<'4769>-> 
>                                                                     <rec>),
>                                                                   StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'4767>-> 
>                                                                     <rec>)
>                                                                   ]))
>                                         ]-> [
>                                               Done [Err [], Ok {}],
>                                               StdinLine (Str -<'4769>-> <rec>),
>                                               StdoutLine Str
>                                                 ({} -<'4767>-> <rec>)
>                                               ] = \fromResult ->
>   clos4
> let clos11(firstName: Str): {}
>                               -[clos11 (firstName: Str)]-> [Err [], Ok {}]
>                                                              -<'5212>-> 
>                                                              [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str)
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                    <rec>)
>                                                                ]
>                                                              -[
>                                                                 clos3
>                                                                   (fromResult: 
>                                                                   ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5212>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str)
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5212>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5212>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5212>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -
>                                                                     [inLine1]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5212>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str)
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5212>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5212>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                   (next: 
>                                                                   (Str
>                                                                     -
>                                                                     [
>                                                                     clos10
>                                                                     (firstName: Str)
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5212>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5212>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5212>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                 ]-> [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5212>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str)
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5212>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5212>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ] = \y ->
>   (await2 inLine1) clos10
> let clos7(continue:
>             [Err [], Ok {}]
>               -<'5443>-> [
>                            Done [Err [], Ok {}],
>                            StdinLine (Str -<'5485>-> <rec>),
>                            StdoutLine Str ({} -<'5482>-> <rec>)
>                            ], next:
>                                 {}
>                                   -<'5254>-> [Err [], Ok {}]
>                                                -<'5443>-> [
>                                                             Done [Err [], Ok {}],
>                                                             StdinLine
>                                                               (Str
>                                                                 -<'5485>-> 
>                                                                 <rec>),
>                                                             StdoutLine Str
>                                                               ({}
>                                                                 -<'5482>-> 
>                                                                 <rec>)
>                                                             ]
>                                                -[clos (err: [])]-> [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'5485>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'5482>-> 
>                                                                     <rec>)
>                                                                     ]): 
>   [Err [], Ok {}]
>     -[
>        clos7
>          (continue: ([Err [], Ok {}]
>                       -<'5443>-> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -<'5485>-> <rec>),
>                                    StdoutLine Str ({} -<'5482>-> <rec>)
>                                    ]))
>          (next: ({}
>                   -<'5254>-> [Err [], Ok {}]
>                                -<'5443>-> [
>                                             Done [Err [], Ok {}],
>                                             StdinLine (Str -<'5485>-> <rec>),
>                                             StdoutLine Str
>                                               ({} -<'5482>-> <rec>)
>                                             ]
>                                -[clos (err: [])]-> [
>                                                      Done [Err [], Ok {}],
>                                                      StdinLine
>                                                        (Str -<'5485>-> <rec>),
>                                                      StdoutLine Str
>                                                        ({} -<'5482>-> <rec>)
>                                                      ]))
>        ]-> [
>              Done [Err [], Ok {}],
>              StdinLine (Str -<'5485>-> <rec>),
>              StdoutLine Str ({} -<'5482>-> <rec>)
>              ] = \result ->
>   let inner: [Err [], Ok {}]
>                -<'5443>-> [
>                             Done [Err [], Ok {}],
>                             StdinLine (Str -<'5485>-> <rec>),
>                             StdoutLine Str ({} -<'5482>-> <rec>)
>                             ]
>                -[clos (err: [])]-> [
>                                      Done [Err [], Ok {}],
>                                      StdinLine (Str -<'5485>-> <rec>),
>                                      StdoutLine Str ({} -<'5482>-> <rec>)
>                                      ] =
>     when result is
>       | Ok v -> next v
>       | Err e -> fail1 e
>     end
>   in
>   inner continue
> let clos8(fromResult:
>             [Err [], Ok {}]
>               -[
>                  clos7
>                    (continue: ([Err [], Ok {}]
>                                 -<'5693>-> [
>                                              Done [Err [], Ok {}],
>                                              StdinLine (Str -<'5698>-> <rec>),
>                                              StdoutLine Str
>                                                ({} -<'5695>-> <rec>)
>                                              ]))
>                    (next: ({}
>                             -<'5632>-> [Err [], Ok {}]
>                                          -<'5693>-> [
>                                                       Done [Err [], Ok {}],
>                                                       StdinLine
>                                                         (Str -<'5698>-> <rec>),
>                                                       StdoutLine Str
>                                                         ({} -<'5695>-> <rec>)
>                                                       ]
>                                          -[clos (err: [])]-> [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'5698>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -<'5695>-> 
>                                                                    <rec>)
>                                                                ]))
>                  ]-> [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -<'5698>-> <rec>),
>                        StdoutLine Str ({} -<'5695>-> <rec>)
>                        ]
>               -<'5675>-> [
>                            Done [Err [], Ok {}],
>                            StdinLine (Str -<'5698>-> <rec>),
>                            StdoutLine Str ({} -<'5695>-> <rec>)
>                            ], next:
>                                 {}
>                                   -<'5632>-> [Err [], Ok {}]
>                                                -<'5693>-> [
>                                                             Done [Err [], Ok {}],
>                                                             StdinLine
>                                                               (Str
>                                                                 -<'5698>-> 
>                                                                 <rec>),
>                                                             StdoutLine Str
>                                                               ({}
>                                                                 -<'5695>-> 
>                                                                 <rec>)
>                                                             ]
>                                                -[clos (err: [])]-> [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'5698>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'5695>-> 
>                                                                     <rec>)
>                                                                     ]): 
>   [Err [], Ok {}]
>     -<'5693>-> [
>                  Done [Err [], Ok {}],
>                  StdinLine (Str -<'5698>-> <rec>),
>                  StdoutLine Str ({} -<'5695>-> <rec>)
>                  ]
>     -[
>        clos8
>          (fromResult: ([Err [], Ok {}]
>                          -[
>                             clos7
>                               (continue: ([Err [], Ok {}]
>                                            -<'5693>-> [
>                                                         Done [Err [], Ok {}],
>                                                         StdinLine
>                                                           (Str
>                                                             -<'5698>-> 
>                                                             <rec>),
>                                                         StdoutLine Str
>                                                           ({} -<'5695>-> <rec>)
>                                                         ]))
>                               (next: ({}
>                                        -<'5632>-> [Err [], Ok {}]
>                                                     -<'5693>-> [
>                                                                  Done
>                                                                    [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                  StdinLine
>                                                                    (Str
>                                                                     -<'5698>-> 
>                                                                     <rec>),
>                                                                  StdoutLine Str
>                                                                    ({}
>                                                                     -<'5695>-> 
>                                                                     <rec>)
>                                                                  ]
>                                                     -[clos (err: [])]-> 
>                                                     [
>                                                       Done [Err [], Ok {}],
>                                                       StdinLine
>                                                         (Str -<'5698>-> <rec>),
>                                                       StdoutLine Str
>                                                         ({} -<'5695>-> <rec>)
>                                                       ]))
>                             ]-> [
>                                   Done [Err [], Ok {}],
>                                   StdinLine (Str -<'5698>-> <rec>),
>                                   StdoutLine Str ({} -<'5695>-> <rec>)
>                                   ]
>                         -<'5675>-> [
>                                      Done [Err [], Ok {}],
>                                      StdinLine (Str -<'5698>-> <rec>),
>                                      StdoutLine Str ({} -<'5695>-> <rec>)
>                                      ]))
>          (next: ({}
>                   -<'5632>-> [Err [], Ok {}]
>                                -<'5693>-> [
>                                             Done [Err [], Ok {}],
>                                             StdinLine (Str -<'5698>-> <rec>),
>                                             StdoutLine Str
>                                               ({} -<'5695>-> <rec>)
>                                             ]
>                                -[clos (err: [])]-> [
>                                                      Done [Err [], Ok {}],
>                                                      StdinLine
>                                                        (Str -<'5698>-> <rec>),
>                                                      StdoutLine Str
>                                                        ({} -<'5695>-> <rec>)
>                                                      ]))
>        ]-> [
>              Done [Err [], Ok {}],
>              StdinLine (Str -<'5698>-> <rec>),
>              StdoutLine Str ({} -<'5695>-> <rec>)
>              ] = \continue ->
>   fromResult clos7
> let clos9(fromResult:
>             [Err [], Ok {}]
>               -[
>                  clos7
>                    (continue: ([Err [], Ok {}]
>                                 -<'5922>-> [
>                                              Done [Err [], Ok {}],
>                                              StdinLine (Str -<'5927>-> <rec>),
>                                              StdoutLine Str
>                                                ({} -<'5924>-> <rec>)
>                                              ]))
>                    (next: ({}
>                             -<'5919>-> [Err [], Ok {}]
>                                          -<'5922>-> [
>                                                       Done [Err [], Ok {}],
>                                                       StdinLine
>                                                         (Str -<'5927>-> <rec>),
>                                                       StdoutLine Str
>                                                         ({} -<'5924>-> <rec>)
>                                                       ]
>                                          -[clos (err: [])]-> [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'5927>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -<'5924>-> 
>                                                                    <rec>)
>                                                                ]))
>                  ]-> [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -<'5927>-> <rec>),
>                        StdoutLine Str ({} -<'5924>-> <rec>)
>                        ]
>               -<'5883>-> [
>                            Done [Err [], Ok {}],
>                            StdinLine (Str -<'5927>-> <rec>),
>                            StdoutLine Str ({} -<'5924>-> <rec>)
>                            ]): {}
>                                  -<'5919>-> [Err [], Ok {}]
>                                               -<'5922>-> [
>                                                            Done [Err [], Ok {}],
>                                                            StdinLine
>                                                              (Str
>                                                                -<'5927>-> 
>                                                                <rec>),
>                                                            StdoutLine Str
>                                                              ({}
>                                                                -<'5924>-> 
>                                                                <rec>)
>                                                            ]
>                                               -[clos (err: [])]-> [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'5927>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'5924>-> 
>                                                                     <rec>)
>                                                                     ]
>                                  -[
>                                     clos9
>                                       (fromResult: ([Err [], Ok {}]
>                                                       -[
>                                                          clos7
>                                                            (continue: 
>                                                            ([Err [], Ok {}]
>                                                              -<'5922>-> 
>                                                              [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'5927>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -<'5924>-> 
>                                                                    <rec>)
>                                                                ]))
>                                                            (next: ({}
>                                                                     -
>                                                                     <'5919>-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5922>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'5927>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'5924>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     [])
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'5927>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'5924>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                          ]-> [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'5927>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -<'5924>-> 
>                                                                    <rec>)
>                                                                ]
>                                                      -<'5883>-> [
>                                                                   Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                   StdinLine
>                                                                     (Str
>                                                                     -<'5927>-> 
>                                                                     <rec>),
>                                                                   StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'5924>-> 
>                                                                     <rec>)
>                                                                   ]))
>                                     ]-> [Err [], Ok {}]
>                                           -<'5922>-> [
>                                                        Done [Err [], Ok {}],
>                                                        StdinLine
>                                                          (Str -<'5927>-> <rec>),
>                                                        StdoutLine Str
>                                                          ({} -<'5924>-> <rec>)
>                                                        ]
>                                           -[
>                                              clos8
>                                                (fromResult: ([Err [], Ok {}]
>                                                                -[
>                                                                   clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5922>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'5927>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'5924>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -<'5919>-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5922>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'5927>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'5924>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     [])
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'5927>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'5924>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                   ]-> 
>                                                                [
>                                                                  Done
>                                                                    [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                  StdinLine
>                                                                    (Str
>                                                                     -<'5927>-> 
>                                                                     <rec>),
>                                                                  StdoutLine Str
>                                                                    ({}
>                                                                     -<'5924>-> 
>                                                                     <rec>)
>                                                                  ]
>                                                               -<'5883>-> 
>                                                               [
>                                                                 Done
>                                                                   [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                 StdinLine
>                                                                   (Str
>                                                                     -
>                                                                     <'5927>-> 
>                                                                     <rec>),
>                                                                 StdoutLine Str
>                                                                   ({}
>                                                                     -
>                                                                     <'5924>-> 
>                                                                     <rec>)
>                                                                 ]))
>                                                (next: ({}
>                                                         -<'5919>-> [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<'5922>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'5927>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'5924>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     [])
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'5927>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'5924>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                              ]-> [
>                                                    Done [Err [], Ok {}],
>                                                    StdinLine
>                                                      (Str -<'5927>-> <rec>),
>                                                    StdoutLine Str
>                                                      ({} -<'5924>-> <rec>)
>                                                    ] = \next ->
>   clos8
> let await1: [Err [], Ok {}]
>               -[
>                  clos7
>                    (continue: ([Err [], Ok {}]
>                                 -<'6156>-> [
>                                              Done [Err [], Ok {}],
>                                              StdinLine (Str -<'6147>-> <rec>),
>                                              StdoutLine Str
>                                                ({} -<'6144>-> <rec>)
>                                              ]))
>                    (next: ({}
>                             -<'6155>-> [Err [], Ok {}]
>                                          -<'6156>-> [
>                                                       Done [Err [], Ok {}],
>                                                       StdinLine
>                                                         (Str -<'6147>-> <rec>),
>                                                       StdoutLine Str
>                                                         ({} -<'6144>-> <rec>)
>                                                       ]
>                                          -[clos (err: [])]-> [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'6147>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -<'6144>-> 
>                                                                    <rec>)
>                                                                ]))
>                  ]-> [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -<'6147>-> <rec>),
>                        StdoutLine Str ({} -<'6144>-> <rec>)
>                        ]
>               -<'6152>-> [
>                            Done [Err [], Ok {}],
>                            StdinLine (Str -<'6147>-> <rec>),
>                            StdoutLine Str ({} -<'6144>-> <rec>)
>                            ]
>               -[await1]-> {}
>                             -<'6155>-> [Err [], Ok {}]
>                                          -<'6156>-> [
>                                                       Done [Err [], Ok {}],
>                                                       StdinLine
>                                                         (Str -<'6147>-> <rec>),
>                                                       StdoutLine Str
>                                                         ({} -<'6144>-> <rec>)
>                                                       ]
>                                          -[clos (err: [])]-> [
>                                                                Done
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ],
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'6147>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  ({}
>                                                                    -<'6144>-> 
>                                                                    <rec>)
>                                                                ]
>                             -[
>                                clos9
>                                  (fromResult: ([Err [], Ok {}]
>                                                  -[
>                                                     clos7
>                                                       (continue: ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                    -<'6156>-> 
>                                                                    [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'6147>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'6144>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                       (next: ({}
>                                                                -<'6155>-> 
>                                                                [Err [], Ok {}]
>                                                                  -<'6156>-> 
>                                                                  [
>                                                                    Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -<'6147>-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'6144>-> 
>                                                                     <rec>)
>                                                                    ]
>                                                                  -[
>                                                                     clos
>                                                                     (err: 
>                                                                     [])
>                                                                     ]-> 
>                                                                  [
>                                                                    Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -<'6147>-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'6144>-> 
>                                                                     <rec>)
>                                                                    ]))
>                                                     ]-> [
>                                                           Done [Err [], Ok {}],
>                                                           StdinLine
>                                                             (Str
>                                                               -<'6147>-> 
>                                                               <rec>),
>                                                           StdoutLine Str
>                                                             ({}
>                                                               -<'6144>-> 
>                                                               <rec>)
>                                                           ]
>                                                 -<'6152>-> [
>                                                              Done
>                                                                [Err [], Ok {}],
>                                                              StdinLine
>                                                                (Str
>                                                                  -<'6147>-> 
>                                                                  <rec>),
>                                                              StdoutLine Str
>                                                                ({}
>                                                                  -<'6144>-> 
>                                                                  <rec>)
>                                                              ]))
>                                ]-> [Err [], Ok {}]
>                                      -<'6156>-> [
>                                                   Done [Err [], Ok {}],
>                                                   StdinLine
>                                                     (Str -<'6147>-> <rec>),
>                                                   StdoutLine Str
>                                                     ({} -<'6144>-> <rec>)
>                                                   ]
>                                      -[
>                                         clos8
>                                           (fromResult: ([Err [], Ok {}]
>                                                           -[
>                                                              clos7
>                                                                (continue: 
>                                                                ([Err [], Ok {}]
>                                                                  -<'6156>-> 
>                                                                  [
>                                                                    Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -<'6147>-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'6144>-> 
>                                                                     <rec>)
>                                                                    ]))
>                                                                (next: 
>                                                                ({}
>                                                                  -<'6155>-> 
>                                                                  [
>                                                                    Err [],
>                                                                    Ok {}
>                                                                    ]
>                                                                    -<'6156>-> 
>                                                                    [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'6147>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'6144>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                    -[
>                                                                     clos
>                                                                     (err: 
>                                                                     [])
>                                                                     ]-> 
>                                                                    [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'6147>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'6144>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                              ]-> [
>                                                                    Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -<'6147>-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'6144>-> 
>                                                                     <rec>)
>                                                                    ]
>                                                          -<'6152>-> [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'6147>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'6144>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                           (next: ({}
>                                                    -<'6155>-> [Err [], Ok {}]
>                                                                 -<'6156>-> 
>                                                                 [
>                                                                   Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                   StdinLine
>                                                                     (Str
>                                                                     -<'6147>-> 
>                                                                     <rec>),
>                                                                   StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'6144>-> 
>                                                                     <rec>)
>                                                                   ]
>                                                                 -[
>                                                                    clos
>                                                                     (err: 
>                                                                     [])
>                                                                    ]-> 
>                                                                 [
>                                                                   Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                   StdinLine
>                                                                     (Str
>                                                                     -<'6147>-> 
>                                                                     <rec>),
>                                                                   StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -<'6144>-> 
>                                                                     <rec>)
>                                                                   ]))
>                                         ]-> [
>                                               Done [Err [], Ok {}],
>                                               StdinLine (Str -<'6147>-> <rec>),
>                                               StdoutLine Str
>                                                 ({} -<'6144>-> <rec>)
>                                               ] = \fromResult ->
>   clos9
> let clos12: Str
>               -[clos10 (firstName: Str), clos12]-> [Err [], Ok {}]
>                                                      -[
>                                                         clos14,
>                                                         clos7 (continue: 
>                                                           <rec>)
>                                                           (next: ({}
>                                                                    -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                    <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[inLine1]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                         ]-> [
>                                                               Done
>                                                                 [Err [], Ok {}],
>                                                               StdinLine
>                                                                 (Str
>                                                                   -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>),
>                                                               StdoutLine Str
>                                                                 ({}
>                                                                   -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>)
>                                                               ]
>                                                      -[
>                                                         clos (err: []),
>                                                         clos6 (s: Str),
>                                                         clos8
>                                                           (fromResult: 
>                                                           ([Err [], Ok {}]
>                                                              -[
>                                                                 clos14,
>                                                                 clos7
>                                                                   (continue: 
>                                                                   ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -
>                                                                     <rec>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                   (next: 
>                                                                   ({}
>                                                                     -
>                                                                     [
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[inLine1]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                 ]-> [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                             -[clos6 (s: Str)]-> 
>                                                             [
>                                                               Done
>                                                                 [Err [], Ok {}],
>                                                               StdinLine
>                                                                 (Str
>                                                                   -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>),
>                                                               StdoutLine Str
>                                                                 ({}
>                                                                   -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>)
>                                                               ]))
>                                                           (next: ({}
>                                                                    -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                    [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[inLine1]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                         ]-> [
>                                                               Done
>                                                                 [Err [], Ok {}],
>                                                               StdinLine
>                                                                 (Str
>                                                                   -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>),
>                                                               StdoutLine Str
>                                                                 ({}
>                                                                   -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>)
>                                                               ] = \firstName ->
>   (await1 (outLine1 "What's your last name?")) clos11
> let clos13: {}
>               -[clos11 (firstName: Str), clos13]-> [Err [], Ok {}]
>                                                      -[
>                                                         clos14,
>                                                         clos7 (continue: 
>                                                           <rec>) (next: 
>                                                           <rec>)
>                                                         ]-> [
>                                                               Done
>                                                                 [Err [], Ok {}],
>                                                               StdinLine
>                                                                 (Str
>                                                                   -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>),
>                                                               StdoutLine Str
>                                                                 ({}
>                                                                   -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>)
>                                                               ]
>                                                      -[
>                                                         clos (err: []),
>                                                         clos3
>                                                           (fromResult: 
>                                                           ([Err [], Ok Str]
>                                                              -[
>                                                                 clos2
>                                                                   (continue: 
>                                                                   ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -
>                                                                     [
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                   (next: 
>                                                                   (Str
>                                                                     -
>                                                                     [
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                 ]-> [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                             -[inLine1]-> 
>                                                             [
>                                                               Done
>                                                                 [Err [], Ok {}],
>                                                               StdinLine
>                                                                 (Str
>                                                                   -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>),
>                                                               StdoutLine Str
>                                                                 ({}
>                                                                   -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>)
>                                                               ]))
>                                                           (next: (Str
>                                                                    -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                    [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                         ]-> [
>                                                               Done
>                                                                 [Err [], Ok {}],
>                                                               StdinLine
>                                                                 (Str
>                                                                   -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>),
>                                                               StdoutLine Str
>                                                                 ({}
>                                                                   -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>)
>                                                               ] = \x1 ->
>   (await2 inLine1) clos12
> let main: [Err [], Ok {}]
>             -[
>                clos14,
>                clos7 (continue: <rec>)
>                  (next: ({}
>                           -[clos11 (firstName: Str), clos13]-> <rec>
>                                                                  -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[inLine1]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     ]-> 
>                                                                  [
>                                                                    Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                    ]))
>                ]-> [
>                      Done [Err [], Ok {}],
>                      StdinLine
>                        (Str
>                          -[
>                             clos1
>                               (toNext3: ([Err [], Ok Str]
>                                           -[
>                                              clos2 (continue: <rec>)
>                                                (next: (Str
>                                                         -[
>                                                            clos10
>                                                              (firstName: Str),
>                                                            clos12
>                                                            ]-> <rec>
>                                                                  -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                  <rec>))
>                                              ]-> <rec>))
>                             ]-> <rec>),
>                      StdoutLine Str
>                        ({}
>                          -[
>                             clos5
>                               (toNext2: ([Err [], Ok {}]
>                                           -[
>                                              clos14,
>                                              clos7 (continue: <rec>)
>                                                (next: ({}
>                                                         -[
>                                                            clos11
>                                                              (firstName: Str),
>                                                            clos13
>                                                            ]-> <rec>
>                                                                  -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                  <rec>))
>                                              ]-> <rec>))
>                             ]-> <rec>)
>                      ]
>             -[
>                clos8
>                  (fromResult: ([Err [], Ok {}]
>                                  -[
>                                     clos14,
>                                     clos7
>                                       (continue: ([Err [], Ok {}]
>                                                    -<rec>-> [
>                                                               Done
>                                                                 [Err [], Ok {}],
>                                                               StdinLine
>                                                                 (Str
>                                                                   -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>),
>                                                               StdoutLine Str
>                                                                 ({}
>                                                                   -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                   <rec>)
>                                                               ]))
>                                       (next: ({}
>                                                -[
>                                                   clos11 (firstName: Str),
>                                                   clos13
>                                                   ]-> [Err [], Ok {}]
>                                                         -<rec>-> [
>                                                                    Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                    ]
>                                                         -[
>                                                            clos (err: []),
>                                                            clos3
>                                                              (fromResult: 
>                                                              ([Err [], Ok Str]
>                                                                 -[
>                                                                    clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                    ]-> 
>                                                                 [
>                                                                   Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                   StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                   StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                   ]
>                                                                -[inLine1]-> 
>                                                                [
>                                                                  Done
>                                                                    [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                  StdinLine
>                                                                    (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                  StdoutLine Str
>                                                                    ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                  ]))
>                                                              (next: (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                            ]-> [
>                                                                  Done
>                                                                    [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                  StdinLine
>                                                                    (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                  StdoutLine Str
>                                                                    ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                  ]))
>                                     ]-> [
>                                           Done [Err [], Ok {}],
>                                           StdinLine
>                                             (Str
>                                               -[
>                                                  clos1
>                                                    (toNext3: ([Err [], Ok Str]
>                                                                -[
>                                                                   clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                   ]-> 
>                                                                <rec>))
>                                                  ]-> <rec>),
>                                           StdoutLine Str
>                                             ({}
>                                               -[clos5 (toNext2: <rec>)]-> 
>                                               <rec>)
>                                           ]
>                                 -[clos6 (s: Str)]-> [
>                                                       Done [Err [], Ok {}],
>                                                       StdinLine
>                                                         (Str
>                                                           -[
>                                                              clos1
>                                                                (toNext3: 
>                                                                ([
>                                                                   Err [],
>                                                                   Ok Str
>                                                                   ]
>                                                                  -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                  <rec>))
>                                                              ]-> <rec>),
>                                                       StdoutLine Str
>                                                         ({}
>                                                           -[
>                                                              clos5
>                                                                (toNext2: 
>                                                                ([Err [], Ok {}]
>                                                                  -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                  <rec>))
>                                                              ]-> <rec>)
>                                                       ]))
>                  (next: ({}
>                           -[clos11 (firstName: Str), clos13]-> [Err [], Ok {}]
>                                                                  -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                  [
>                                                                    Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                    ]
>                                                                  -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[inLine1]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     [
>                                                                     Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     ]-> 
>                                                                  [
>                                                                    Done
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ],
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -[
>                                                                     clos1
>                                                                     (toNext3: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     ({}
>                                                                     -[
>                                                                     clos5
>                                                                     (toNext2: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                    ]))
>                ]-> [
>                      Done [Err [], Ok {}],
>                      StdinLine
>                        (Str
>                          -[
>                             clos1
>                               (toNext3: ([Err [], Ok Str]
>                                           -[
>                                              clos2
>                                                (continue: ([Err [], Ok {}]
>                                                             -[
>                                                                clos14,
>                                                                clos7
>                                                                  (continue: 
>                                                                  <rec>)
>                                                                  (next: 
>                                                                  ({}
>                                                                    -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                    <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                ]-> <rec>))
>                                                (next: (Str
>                                                         -[
>                                                            clos10
>                                                              (firstName: Str),
>                                                            clos12
>                                                            ]-> [Err [], Ok {}]
>                                                                  -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                  <rec>
>                                                                  -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                  <rec>))
>                                              ]-> <rec>))
>                             ]-> <rec>),
>                      StdoutLine Str
>                        ({}
>                          -[
>                             clos5
>                               (toNext2: ([Err [], Ok {}]
>                                           -[
>                                              clos14,
>                                              clos7
>                                                (continue: ([Err [], Ok {}]
>                                                             -<rec>-> 
>                                                             <rec>))
>                                                (next: ({}
>                                                         -[
>                                                            clos11
>                                                              (firstName: Str),
>                                                            clos13
>                                                            ]-> [Err [], Ok {}]
>                                                                  -<rec>-> 
>                                                                  <rec>
>                                                                  -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok Str
>                                                                     ]
>                                                                     -[
>                                                                     clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                  <rec>))
>                                              ]-> <rec>))
>                             ]-> <rec>)
>                      ] =
>   (await1 (outLine1 "What's your first name?")) clos13
> let clos15(i: Int, op1:
>                      [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -<'7930>-> <rec>),
>                        StdoutLine Str ({} -<'7927>-> <rec>)
>                        ]): [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                              -[
>                                 clos15 (i: Int)
>                                   (op1: [
>                                           Done [Err [], Ok {}],
>                                           StdinLine (Str -<'7930>-> <rec>),
>                                           StdoutLine Str ({} -<'7927>-> <rec>)
>                                           ])
>                                 ]-> [
>                                       Done [Err [], Ok {}]
>                                         [
>                                           EntryPoint,
>                                           Stdin <rec>,
>                                           Stdout Str <rec>
>                                           ]
>                                       ] = \t ->
>   when op1 is
>     | StdinLine f ->
>       ((handle1 (f ~str_concat "stdin" ~itos i)) ~add i 1) (Stdin t)
>     | StdoutLine s2 f1 -> ((handle1 (f1 {})) ~add i 1) (Stdout s2 t)
>     | Done x3 -> Done x3 t
>   end
> let clos16(op1:
>              [
>                Done [Err [], Ok {}],
>                StdinLine (Str -<'7930>-> <rec>),
>                StdoutLine Str ({} -<'7927>-> <rec>)
>                ]): Int
>                      -[
>                         clos16
>                           (op1: [
>                                   Done [Err [], Ok {}],
>                                   StdinLine (Str -<'7930>-> <rec>),
>                                   StdoutLine Str ({} -<'7927>-> <rec>)
>                                   ])
>                         ]-> [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                               -[
>                                  clos15 (i: Int)
>                                    (op1: [
>                                            Done [Err [], Ok {}],
>                                            StdinLine (Str -<'7930>-> <rec>),
>                                            StdoutLine Str ({} -<'7927>-> <rec>)
>                                            ])
>                                  ]-> [
>                                        Done [Err [], Ok {}]
>                                          [
>                                            EntryPoint,
>                                            Stdin <rec>,
>                                            Stdout Str <rec>
>                                            ]
>                                        ] = \i ->
>   clos15
> let handle1: [
>                Done [Err [], Ok {}],
>                StdinLine (Str -<'7930>-> <rec>),
>                StdoutLine Str ({} -<'7927>-> <rec>)
>                ]
>                -[handle1]-> Int
>                               -[
>                                  clos16
>                                    (op1: [
>                                            Done [Err [], Ok {}],
>                                            StdinLine (Str -<'7930>-> <rec>),
>                                            StdoutLine Str ({} -<'7927>-> <rec>)
>                                            ])
>                                  ]-> [
>                                        EntryPoint,
>                                        Stdin <rec>,
>                                        Stdout Str <rec>
>                                        ]
>                                        -[
>                                           clos15 (i: Int)
>                                             (op1: [
>                                                     Done [Err [], Ok {}],
>                                                     StdinLine
>                                                       (Str -<'7930>-> <rec>),
>                                                     StdoutLine Str
>                                                       ({} -<'7927>-> <rec>)
>                                                     ])
>                                           ]-> [
>                                                 Done [Err [], Ok {}]
>                                                   [
>                                                     EntryPoint,
>                                                     Stdin <rec>,
>                                                     Stdout Str <rec>
>                                                     ]
>                                                 ] = \op1 ->
>   clos16
> run main_handler: [
>                     Done [Err [], Ok {}]
>                       [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                     ] =
>   let op: [
>             Done [Err [], Ok {}],
>             StdinLine
>               (Str
>                 -[
>                    clos1
>                      (toNext3: ([Err [], Ok Str]
>                                  -[
>                                     clos2
>                                       (continue: ([Err [], Ok {}]
>                                                    -[
>                                                       clos14,
>                                                       clos7 (continue: 
>                                                         <rec>)
>                                                         (next: ({}
>                                                                  -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                  <rec>
>                                                                    -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                    <rec>))
>                                                       ]-> <rec>))
>                                       (next: (Str
>                                                -[
>                                                   clos10 (firstName: Str),
>                                                   clos12
>                                                   ]-> [Err [], Ok {}]
>                                                         -[
>                                                            clos14,
>                                                            clos7
>                                                              (continue: 
>                                                              <rec>)
>                                                              (next: ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                            ]-> <rec>
>                                                         -[
>                                                            clos (err: []),
>                                                            clos6 (s: Str),
>                                                            clos8
>                                                              (fromResult: 
>                                                              ([Err [], Ok {}]
>                                                                 -[
>                                                                    clos14,
>                                                                    clos7
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                    ]-> 
>                                                                 <rec>
>                                                                -[
>                                                                   clos6 (s: Str)
>                                                                   ]-> 
>                                                                <rec>))
>                                                              (next: ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -[
>                                                                     clos14,
>                                                                     clos7
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos3
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[inLine1]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                            ]-> <rec>))
>                                     ]-> <rec>))
>                    ]-> <rec>),
>             StdoutLine Str
>               ({}
>                 -[
>                    clos5
>                      (toNext2: ([Err [], Ok {}]
>                                  -[
>                                     clos14,
>                                     clos7
>                                       (continue: ([Err [], Ok {}]
>                                                    -<rec>-> <rec>))
>                                       (next: ({}
>                                                -[
>                                                   clos11 (firstName: Str),
>                                                   clos13
>                                                   ]-> [Err [], Ok {}]
>                                                         -<rec>-> <rec>
>                                                         -[
>                                                            clos (err: []),
>                                                            clos3
>                                                              (fromResult: 
>                                                              ([Err [], Ok Str]
>                                                                 -[
>                                                                    clos2
>                                                                     (continue: 
>                                                                     ([
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                    ]-> 
>                                                                 <rec>
>                                                                -[inLine1]-> 
>                                                                <rec>))
>                                                              (next: (Str
>                                                                     -[
>                                                                     clos10
>                                                                     (firstName: Str),
>                                                                     clos12
>                                                                     ]-> 
>                                                                     [
>                                                                     Err [],
>                                                                     Ok {}
>                                                                     ]
>                                                                     -<rec>-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos
>                                                                     (err: 
>                                                                     []),
>                                                                     clos6
>                                                                     (s: Str),
>                                                                     clos8
>                                                                     (fromResult: 
>                                                                     (<rec>
>                                                                     -[
>                                                                     clos6
>                                                                     (s: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos11
>                                                                     (firstName: Str),
>                                                                     clos13
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                     <rec>))
>                                                            ]-> <rec>))
>                                     ]-> <rec>))
>                    ]-> <rec>)
>             ] =
>     main clos14
>   in
>   ((handle1 op) 0) (EntryPoint )

> cor-out +ir -print
> fn clos34(next: [ `0 { { str } }, `1 {} ], captures47: { [ `0 {} ] }):
>   [ `0 { { [] } }, `1 { { [ `0 {} ], [ `0 { { str } }, `1 {} ] } } ]
> {
>   let fromResult: [ `0 {} ] = @get_struct_field<captures47, 0>;
>   let var: { [ `0 {} ], [ `0 { { str } }, `1 {} ] }
>     = @make_struct{ fromResult, next };
>   let struct: { { [ `0 {} ], [ `0 { { str } }, `1 {} ] } }
>     = @make_struct{ var };
>   let var1: [ `0 { { [] } }, `1 { { [ `0 {} ], [ `0 { { str } }, `1 {} ] } } ]
>     = @make_union<1, struct>;
>   return var1;
> }
> 
> fn await4(fromResult: [ `0 {} ]): [ `0 { { [ `0 {} ] } } ]
> {
>   let var2: { [ `0 {} ] } = @make_struct{ fromResult };
>   let struct1: { { [ `0 {} ] } } = @make_struct{ var2 };
>   let var3: [ `0 { { [ `0 {} ] } } ] = @make_union<0, struct1>;
>   return var3;
> }
> 
> fn outLine3(s: str):
>   [
>      `0 { { [] } },
>      `1 { { str } },
>      `2 { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } }
>   ]
> {
>   let var4: { str } = @make_struct{ s };
>   let struct2: { { str } } = @make_struct{ var4 };
>   let var5:
>         [
>            `0 { { [] } },
>            `1 { { str } },
>            `2 { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } }
>         ]
>     = @make_union<1, struct2>;
>   return var5;
> }
> 
> fn clos33(next: [ `0 { { str } }, `1 {} ], captures38: { [ `0 { { str } } ] }):
>   [
>      `0 { { [] } },
>      `1 { { str } },
>      `2 { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } }
>   ]
> {
>   let fromResult: [ `0 { { str } } ] = @get_struct_field<captures38, 0>;
>   let var6: { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] }
>     = @make_struct{ fromResult, next };
>   let struct3: { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } }
>     = @make_struct{ var6 };
>   let var7:
>         [
>            `0 { { [] } },
>            `1 { { str } },
>            `2 { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } }
>         ]
>     = @make_union<2, struct3>;
>   return var7;
> }
> 
> fn clos32(
>   toNext2: Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>    captures34: { str }):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 {
>          [
>             `0 {
>                 {
>                  [
>                     `0 {
>                         {
>                          Box!a([
>                                   `0 {},
>                                   `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                ]),
>                           [ `0 { { str } }, `1 {} ]
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                  ,
>                 }
>                 ,
>                }
>          ]
>          ,
>         },
>      `2 {
>          str,
>           [
>              `0 {
>                  { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) }
>                  ,
>                 }
>           ]
>          ,
>         }
>   ]
> {
>   let s: str = @get_struct_field<captures34, 0>;
>   let var8: { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) }
>     = @make_struct{ toNext2 };
>   let struct5:
>         { { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) } }
>     = @make_struct{ var8 };
>   let var9:
>         [ `0 { { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) } }
>         ]
>     = @make_union<0, struct5>;
>   let struct4:
>         {
>          str,
>           [
>              `0 {
>                  { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) }
>                  ,
>                 }
>           ]
>          ,
>         }
>     = @make_struct{ s, var9 };
>   let var10:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 {
>                [
>                   `0 {
>                       {
>                        [
>                           `0 {
>                               {
>                                Box!a([
>                                         `0 {},
>                                         `1 { { !a, [ `0 { { str } }, `1 {} ] } ,
>                                            }
>                                      ]),
>                                 [ `0 { { str } }, `1 {} ]
>                                ,
>                               }
>                               ,
>                              }
>                        ]
>                        ,
>                       }
>                       ,
>                      }
>                ]
>                ,
>               },
>            `2 {
>                str,
>                 [
>                    `0 {
>                        {
>                         Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                               ])
>                         ,
>                        }
>                        ,
>                       }
>                 ]
>                ,
>               }
>         ]
>     = @make_union<2, struct4>;
>   return var10;
> }
> 
> fn fail3(err: []):
>   [
>      `0 { { [] } },
>      `1 { { str } },
>      `2 { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } }
>   ]
> {
>   let var11: { [] } = @make_struct{ err };
>   let struct6: { { [] } } = @make_struct{ var11 };
>   let var12:
>         [
>            `0 { { [] } },
>            `1 { { str } },
>            `2 { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } }
>         ]
>     = @make_union<0, struct6>;
>   return var12;
> }
> 
> fn clos31(firstName: str):
>   [
>      `0 { { [] } },
>      `1 { { str } },
>      `2 { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } }
>   ]
> {
>   let struct7: {} = @make_struct{};
>   let var13: [ `0 {} ] = @make_union<0, struct7>;
>   let discr: int = @get_union_id<var13>;
>   switch discr {
>   0 -> {
>     let struct8: {} = @make_struct{};
>     let var14: [ `0 {} ] = @make_union<0, struct8>;
>     let discr1: int = @get_union_id<var14>;
>     switch discr1 {
>     0 -> {
>       let var15: str = "What's your last name?";
>       @call_direct(outLine2, var15)
>     }
>     } in join join;
>     @call_direct(await3, join)
>   }
>   } in join join1;
>   let discr2: int = @get_union_id<join1>;
>   switch discr2 {
>   0 -> {
>     let payload: { { [ `0 { { str } } ] } } = @get_union_struct<join1>;
>     let captures37: { [ `0 { { str } } ] } = @get_struct_field<payload, 0>;
>     let var16: { str } = @make_struct{ firstName };
>     let struct9: { { str } } = @make_struct{ var16 };
>     let var17: [ `0 { { str } }, `1 {} ] = @make_union<0, struct9>;
>     @call_direct(clos33, var17, captures37)
>   }
>   } in join join2;
>   return join2;
> }
> 
> fn clos30(lastName: str, captures40: { str }):
>   [
>      `0 { { [] } },
>      `1 { { str } },
>      `2 { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } }
>   ]
> {
>   let firstName: str = @get_struct_field<captures40, 0>;
>   let struct10: {} = @make_struct{};
>   let var18: [ `0 {} ] = @make_union<0, struct10>;
>   let discr3: int = @get_union_id<var18>;
>   switch discr3 {
>   0 -> {
>     let var19: str = "Hello ";
>     let var20: str = " ";
>     let var21: str = "!";
>     let var22: str
>       = @call_kfn(str_concat, var19, firstName, var20, lastName, var21);
>     @call_direct(outLine3, var22)
>   }
>   } in join join3;
>   return join3;
> }
> 
> fn inLine2(
>   toNext3:
>     [
>        `0 {
>            {
>             Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>              [ `0 { { str } }, `1 {} ]
>             ,
>            }
>            ,
>           }
>     ]):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 {
>          [
>             `0 {
>                 {
>                  [
>                     `0 {
>                         {
>                          Box!a([
>                                   `0 {},
>                                   `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                ]),
>                           [ `0 { { str } }, `1 {} ]
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                  ,
>                 }
>                 ,
>                }
>          ]
>          ,
>         },
>      `2 {
>          str,
>           [
>              `0 {
>                  {
>                   [
>                      `0 {},
>                      `1 {
>                          {
>                           Box!a([
>                                    `0 {},
>                                    `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                 ]),
>                            [ `0 { { str } }, `1 {} ]
>                           ,
>                          }
>                          ,
>                         }
>                   ]
>                   ,
>                  }
>                  ,
>                 }
>           ]
>          ,
>         }
>   ]
> {
>   let var23:
>         {
>          [
>             `0 {
>                 {
>                  Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>                   [ `0 { { str } }, `1 {} ]
>                  ,
>                 }
>                 ,
>                }
>          ]
>          ,
>         }
>     = @make_struct{ toNext3 };
>   let struct12:
>         {
>          {
>           [
>              `0 {
>                  {
>                   Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>                    [ `0 { { str } }, `1 {} ]
>                   ,
>                  }
>                  ,
>                 }
>           ]
>           ,
>          }
>          ,
>         }
>     = @make_struct{ var23 };
>   let var24:
>         [
>            `0 {
>                {
>                 [
>                    `0 {
>                        {
>                         Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                               ]),
>                          [ `0 { { str } }, `1 {} ]
>                         ,
>                        }
>                        ,
>                       }
>                 ]
>                 ,
>                }
>                ,
>               }
>         ]
>     = @make_union<0, struct12>;
>   let struct11:
>         {
>          [
>             `0 {
>                 {
>                  [
>                     `0 {
>                         {
>                          Box!a([
>                                   `0 {},
>                                   `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                ]),
>                           [ `0 { { str } }, `1 {} ]
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                  ,
>                 }
>                 ,
>                }
>          ]
>          ,
>         }
>     = @make_struct{ var24 };
>   let var25:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 {
>                [
>                   `0 {
>                       {
>                        [
>                           `0 {
>                               {
>                                Box!a([
>                                         `0 {},
>                                         `1 { { !a, [ `0 { { str } }, `1 {} ] } ,
>                                            }
>                                      ]),
>                                 [ `0 { { str } }, `1 {} ]
>                                ,
>                               }
>                               ,
>                              }
>                        ]
>                        ,
>                       }
>                       ,
>                      }
>                ]
>                ,
>               },
>            `2 {
>                str,
>                 [
>                    `0 {
>                        {
>                         [
>                            `0 {},
>                            `1 {
>                                {
>                                 Box!a([
>                                          `0 {},
>                                          `1 { { !a, [ `0 { { str } }, `1 {} ] } ,
>                                             }
>                                       ]),
>                                  [ `0 { { str } }, `1 {} ]
>                                 ,
>                                }
>                                ,
>                               }
>                         ]
>                         ,
>                        }
>                        ,
>                       }
>                 ]
>                ,
>               }
>         ]
>     = @make_union<1, struct11>;
>   return var25;
> }
> 
> fn clos29(
>   result: [ `0 { [] }, `1 { str } ],
>    captures33:
>      {
>       Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>        [ `0 { { str } }, `1 {} ]
>       ,
>      }):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 {
>          [
>             `0 {
>                 {
>                  [
>                     `0 {
>                         {
>                          Box!a([
>                                   `0 {},
>                                   `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                ]),
>                           [ `0 { { str } }, `1 {} ]
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                  ,
>                 }
>                 ,
>                }
>          ]
>          ,
>         },
>      `2 {
>          str,
>           [
>              `0 {
>                  {
>                   [
>                      `0 {},
>                      `1 {
>                          {
>                           Box!a([
>                                    `0 {},
>                                    `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                 ]),
>                            [ `0 { { str } }, `1 {} ]
>                           ,
>                          }
>                          ,
>                         }
>                   ]
>                   ,
>                  }
>                  ,
>                 }
>           ]
>          ,
>         }
>   ]
> {
>   let next: [ `0 { { str } }, `1 {} ] = @get_struct_field<captures33, 1>;
>   let continue: Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ])
>     = @get_struct_field<captures33, 0>;
>   let discr4: int = @get_union_id<result>;
>   switch discr4 {
>   0 -> {
>     let payload3: { [] } = @get_union_struct<result>;
>     let e: [] = @get_struct_field<payload3, 0>;
>     let struct13: {} = @make_struct{};
>     let var26: [ `0 {} ] = @make_union<0, struct13>;
>     let discr6: int = @get_union_id<var26>;
>     switch discr6 {
>     0 -> {
>       @call_direct(fail3, e)
>     }
>     } in join join5;
>     join5
>   }
>   1 -> {
>     let payload1: { str } = @get_union_struct<result>;
>     let v: str = @get_struct_field<payload1, 0>;
>     let discr5: int = @get_union_id<next>;
>     switch discr5 {
>     0 -> {
>       let payload2: { { str } } = @get_union_struct<next>;
>       let captures27: { str } = @get_struct_field<payload2, 0>;
>       @call_direct(clos30, v, captures27)
>     }
>     1 -> {
>       @call_direct(clos31, v)
>     }
>     } in join join4;
>     join4
>   }
>   } in join join6;
>   let inner:
>         [
>            `0 { { [] } },
>            `1 { { str } },
>            `2 { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } }
>         ]
>     = join6;
>   let discr7: int = @get_union_id<inner>;
>   switch discr7 {
>   0 -> {
>     let payload4: { { [] } } = @get_union_struct<inner>;
>     let captures30: { [] } = @get_struct_field<payload4, 0>;
>     @call_direct(clos27, continue, captures30)
>   }
>   1 -> {
>     let payload5: { { str } } = @get_union_struct<inner>;
>     let captures31: { str } = @get_struct_field<payload5, 0>;
>     @call_direct(clos32, continue, captures31)
>   }
>   2 -> {
>     let payload6: { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } }
>       = @get_union_struct<inner>;
>     let captures32: { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] }
>       = @get_struct_field<payload6, 0>;
>     @call_direct(clos20, continue, captures32)
>   }
>   } in join join7;
>   return join7;
> }
> 
> fn clos28(
>   continue: Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>    captures26: { [ `0 {} ], [ `0 { { str } }, `1 {} ] }):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 {
>          [
>             `0 {
>                 {
>                  [
>                     `0 {
>                         {
>                          Box!a([
>                                   `0 {},
>                                   `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                ]),
>                           [ `0 { { str } }, `1 {} ]
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                  ,
>                 }
>                 ,
>                }
>          ]
>          ,
>         },
>      `2 {
>          str,
>           [
>              `0 {
>                  {
>                   [
>                      `0 {},
>                      `1 {
>                          {
>                           Box!a([
>                                    `0 {},
>                                    `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                 ]),
>                            [ `0 { { str } }, `1 {} ]
>                           ,
>                          }
>                          ,
>                         }
>                   ]
>                   ,
>                  }
>                  ,
>                 }
>           ]
>          ,
>         }
>   ]
> {
>   let next: [ `0 { { str } }, `1 {} ] = @get_struct_field<captures26, 1>;
>   let fromResult: [ `0 {} ] = @get_struct_field<captures26, 0>;
>   let discr8: int = @get_union_id<fromResult>;
>   switch discr8 {
>   0 -> {
>     let var27:
>           {
>            Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>             [ `0 { { str } }, `1 {} ]
>            ,
>           }
>       = @make_struct{ continue, next };
>     let struct14:
>           {
>            {
>             Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>              [ `0 { { str } }, `1 {} ]
>             ,
>            }
>            ,
>           }
>       = @make_struct{ var27 };
>     let var28:
>           [
>              `0 {
>                  {
>                   Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>                    [ `0 { { str } }, `1 {} ]
>                   ,
>                  }
>                  ,
>                 }
>           ]
>       = @make_union<0, struct14>;
>     @call_direct(inLine2, var28)
>   }
>   } in join join8;
>   return join8;
> }
> 
> fn clos27(
>   toNext1: Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>    captures43: { [] }):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 {
>          [
>             `0 {
>                 {
>                  [
>                     `0 {
>                         {
>                          Box!a([
>                                   `0 {},
>                                   `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                ]),
>                           [ `0 { { str } }, `1 {} ]
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                  ,
>                 }
>                 ,
>                }
>          ]
>          ,
>         },
>      `2 {
>          str,
>           [
>              `0 {
>                  { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) }
>                  ,
>                 }
>           ]
>          ,
>         }
>   ]
> {
>   let err: [] = @get_struct_field<captures43, 0>;
>   let discr9: int = @get_union_id<toNext1>;
>   switch discr9 {
>   0 -> {
>     let struct15: { [] } = @make_struct{ err };
>     let var29: [ `0 { [] }, `1 { {} } ] = @make_union<0, struct15>;
>     @call_direct(clos19, var29)
>   }
>   1 -> {
>     let payload7:
>           {
>            {
>             Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>              [ `0 { { str } }, `1 {} ]
>             ,
>            }
>            ,
>           }
>       = @get_union_struct<toNext1>;
>     let captures42:
>           {
>            Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>             [ `0 { { str } }, `1 {} ]
>            ,
>           }
>       = @get_struct_field<payload7, 0>;
>     let struct16: { [] } = @make_struct{ err };
>     let var30: [ `0 { [] }, `1 { {} } ] = @make_union<0, struct16>;
>     @call_direct(clos25, var30, captures42)
>   }
>   } in join join9;
>   return join9;
> }
> 
> fn fail2(err: []):
>   [ `0 { { [] } }, `1 { { [ `0 {} ], [ `0 { { str } }, `1 {} ] } } ]
> {
>   let var31: { [] } = @make_struct{ err };
>   let struct17: { { [] } } = @make_struct{ var31 };
>   let var32: [ `0 { { [] } }, `1 { { [ `0 {} ], [ `0 { { str } }, `1 {} ] } } ]
>     = @make_union<0, struct17>;
>   return var32;
> }
> 
> fn clos26(y: {}, captures46: { str }):
>   [ `0 { { [] } }, `1 { { [ `0 {} ], [ `0 { { str } }, `1 {} ] } } ]
> {
>   let firstName: str = @get_struct_field<captures46, 0>;
>   let struct18: {} = @make_struct{};
>   let var33: [ `0 {} ] = @make_union<0, struct18>;
>   let discr10: int = @get_union_id<var33>;
>   switch discr10 {
>   0 -> {
>     let struct19: {} = @make_struct{};
>     let var34: [ `0 {} ] = @make_union<0, struct19>;
>     @call_direct(await4, var34)
>   }
>   } in join join10;
>   let discr11: int = @get_union_id<join10>;
>   switch discr11 {
>   0 -> {
>     let payload8: { { [ `0 {} ] } } = @get_union_struct<join10>;
>     let captures45: { [ `0 {} ] } = @get_struct_field<payload8, 0>;
>     let var35: { str } = @make_struct{ firstName };
>     let struct20: { { str } } = @make_struct{ var35 };
>     let var36: [ `0 { { str } }, `1 {} ] = @make_union<0, struct20>;
>     @call_direct(clos34, var36, captures45)
>   }
>   } in join join11;
>   return join11;
> }
> 
> fn clos25(
>   result: [ `0 { [] }, `1 { {} } ],
>    captures24:
>      {
>       Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>        [ `0 { { str } }, `1 {} ]
>       ,
>      }):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 {
>          [
>             `0 {
>                 {
>                  [
>                     `0 {
>                         {
>                          Box!a([
>                                   `0 {},
>                                   `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                ]),
>                           [ `0 { { str } }, `1 {} ]
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                  ,
>                 }
>                 ,
>                }
>          ]
>          ,
>         },
>      `2 {
>          str,
>           [
>              `0 {
>                  {
>                   [
>                      `0 {},
>                      `1 {
>                          {
>                           Box!a([
>                                    `0 {},
>                                    `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                 ]),
>                            [ `0 { { str } }, `1 {} ]
>                           ,
>                          }
>                          ,
>                         }
>                   ]
>                   ,
>                  }
>                  ,
>                 }
>           ]
>          ,
>         }
>   ]
> {
>   let next: [ `0 { { str } }, `1 {} ] = @get_struct_field<captures24, 1>;
>   let continue: Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ])
>     = @get_struct_field<captures24, 0>;
>   let discr12: int = @get_union_id<result>;
>   switch discr12 {
>   0 -> {
>     let payload11: { [] } = @get_union_struct<result>;
>     let e: [] = @get_struct_field<payload11, 0>;
>     let struct21: {} = @make_struct{};
>     let var37: [ `0 {} ] = @make_union<0, struct21>;
>     let discr14: int = @get_union_id<var37>;
>     switch discr14 {
>     0 -> {
>       @call_direct(fail2, e)
>     }
>     } in join join13;
>     join13
>   }
>   1 -> {
>     let payload9: { {} } = @get_union_struct<result>;
>     let v: {} = @get_struct_field<payload9, 0>;
>     let discr13: int = @get_union_id<next>;
>     switch discr13 {
>     0 -> {
>       let payload10: { { str } } = @get_union_struct<next>;
>       let captures19: { str } = @get_struct_field<payload10, 0>;
>       @call_direct(clos26, v, captures19)
>     }
>     1 -> {
>       @call_direct(clos17, v)
>     }
>     } in join join12;
>     join12
>   }
>   } in join join14;
>   let inner: [ `0 { { [] } }, `1 { { [ `0 {} ], [ `0 { { str } }, `1 {} ] } } ]
>     = join14;
>   let discr15: int = @get_union_id<inner>;
>   switch discr15 {
>   0 -> {
>     let payload12: { { [] } } = @get_union_struct<inner>;
>     let captures22: { [] } = @get_struct_field<payload12, 0>;
>     @call_direct(clos27, continue, captures22)
>   }
>   1 -> {
>     let payload13: { { [ `0 {} ], [ `0 { { str } }, `1 {} ] } }
>       = @get_union_struct<inner>;
>     let captures23: { [ `0 {} ], [ `0 { { str } }, `1 {} ] }
>       = @get_struct_field<payload13, 0>;
>     @call_direct(clos28, continue, captures23)
>   }
>   } in join join15;
>   return join15;
> }
> 
> fn clos24(
>   x: {},
>    captures18: { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) }):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 {
>          [
>             `0 {
>                 {
>                  [
>                     `0 {
>                         {
>                          Box!a([
>                                   `0 {},
>                                   `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                ]),
>                           [ `0 { { str } }, `1 {} ]
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                  ,
>                 }
>                 ,
>                }
>          ]
>          ,
>         },
>      `2 {
>          str,
>           [
>              `0 {
>                  { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) }
>                  ,
>                 }
>           ]
>          ,
>         }
>   ]
> {
>   let toNext2: Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ])
>     = @get_struct_field<captures18, 0>;
>   let discr16: int = @get_union_id<toNext2>;
>   switch discr16 {
>   0 -> {
>     let struct22: { {} } = @make_struct{ x };
>     let var38: [ `0 { [] }, `1 { {} } ] = @make_union<1, struct22>;
>     @call_direct(clos19, var38)
>   }
>   1 -> {
>     let payload14:
>           {
>            {
>             Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>              [ `0 { { str } }, `1 {} ]
>             ,
>            }
>            ,
>           }
>       = @get_union_struct<toNext2>;
>     let captures17:
>           {
>            Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>             [ `0 { { str } }, `1 {} ]
>            ,
>           }
>       = @get_struct_field<payload14, 0>;
>     let struct23: { {} } = @make_struct{ x };
>     let var39: [ `0 { [] }, `1 { {} } ] = @make_union<1, struct23>;
>     @call_direct(clos25, var39, captures17)
>   }
>   } in join join16;
>   return join16;
> }
> 
> fn clos23(
>   s1: str,
>    captures49:
>      {
>       [
>          `0 {
>              {
>               Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>                [ `0 { { str } }, `1 {} ]
>               ,
>              }
>              ,
>             }
>       ]
>       ,
>      }):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 {
>          [
>             `0 {
>                 {
>                  [
>                     `0 {
>                         {
>                          Box!a([
>                                   `0 {},
>                                   `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                ]),
>                           [ `0 { { str } }, `1 {} ]
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                  ,
>                 }
>                 ,
>                }
>          ]
>          ,
>         },
>      `2 {
>          str,
>           [
>              `0 {
>                  { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) }
>                  ,
>                 }
>           ]
>          ,
>         }
>   ]
> {
>   let toNext3:
>         [
>            `0 {
>                {
>                 Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>                  [ `0 { { str } }, `1 {} ]
>                 ,
>                }
>                ,
>               }
>         ]
>     = @get_struct_field<captures49, 0>;
>   let discr17: int = @get_union_id<toNext3>;
>   switch discr17 {
>   0 -> {
>     let payload15:
>           {
>            {
>             Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>              [ `0 { { str } }, `1 {} ]
>             ,
>            }
>            ,
>           }
>       = @get_union_struct<toNext3>;
>     let captures48:
>           {
>            Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>             [ `0 { { str } }, `1 {} ]
>            ,
>           }
>       = @get_struct_field<payload15, 0>;
>     let struct24: { str } = @make_struct{ s1 };
>     let var40: [ `0 { [] }, `1 { str } ] = @make_union<1, struct24>;
>     @call_direct(clos29, var40, captures48)
>   }
>   } in join join17;
>   return join17;
> }
> 
> fn clos22(
>   t: Box!a([ `0 {}, `1 { !a }, `2 { str, !a } ]),
>    captures15:
>      {
>       int,
>        [
>           `0 { [ `0 { [] }, `1 { {} } ] },
>           `1 {
>               [
>                  `0 {
>                      {
>                       [
>                          `0 {
>                              {
>                               Box!a([
>                                        `0 {},
>                                        `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                     ]),
>                                [ `0 { { str } }, `1 {} ]
>                               ,
>                              }
>                              ,
>                             }
>                       ]
>                       ,
>                      }
>                      ,
>                     }
>               ]
>               ,
>              },
>           `2 {
>               str,
>                [
>                   `0 {
>                       {
>                        Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                              ])
>                        ,
>                       }
>                       ,
>                      }
>                ]
>               ,
>              }
>        ]
>       ,
>      }):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ], Box!a([ `0 {}, `1 { !a }, `2 { str, !a } ]) ,
>         }
>   ]
> {
>   let op1:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 {
>                [
>                   `0 {
>                       {
>                        [
>                           `0 {
>                               {
>                                Box!a([
>                                         `0 {},
>                                         `1 { { !a, [ `0 { { str } }, `1 {} ] } ,
>                                            }
>                                      ]),
>                                 [ `0 { { str } }, `1 {} ]
>                                ,
>                               }
>                               ,
>                              }
>                        ]
>                        ,
>                       }
>                       ,
>                      }
>                ]
>                ,
>               },
>            `2 {
>                str,
>                 [
>                    `0 {
>                        {
>                         Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                               ])
>                         ,
>                        }
>                        ,
>                       }
>                 ]
>                ,
>               }
>         ]
>     = @get_struct_field<captures15, 1>;
>   let i: int = @get_struct_field<captures15, 0>;
>   let discr18: int = @get_union_id<op1>;
>   switch discr18 {
>   0 -> {
>     let payload24: { [ `0 { [] }, `1 { {} } ] } = @get_union_struct<op1>;
>     let x3: [ `0 { [] }, `1 { {} } ] = @get_struct_field<payload24, 0>;
>     let struct29:
>           {
>            [ `0 { [] }, `1 { {} } ],
>             Box!a([ `0 {}, `1 { !a }, `2 { str, !a } ])
>            ,
>           }
>       = @make_struct{ x3, t };
>     @make_union<0, struct29>
>   }
>   1 -> {
>     let payload16:
>           {
>            [
>               `0 {
>                   {
>                    [
>                       `0 {
>                           {
>                            Box!a([
>                                     `0 {},
>                                     `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                  ]),
>                             [ `0 { { str } }, `1 {} ]
>                            ,
>                           }
>                           ,
>                          }
>                    ]
>                    ,
>                   }
>                   ,
>                  }
>            ]
>            ,
>           }
>       = @get_union_struct<op1>;
>     let f:
>           [
>              `0 {
>                  {
>                   [
>                      `0 {
>                          {
>                           Box!a([
>                                    `0 {},
>                                    `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                 ]),
>                            [ `0 { { str } }, `1 {} ]
>                           ,
>                          }
>                          ,
>                         }
>                   ]
>                   ,
>                  }
>                  ,
>                 }
>           ]
>       = @get_struct_field<payload16, 0>;
>     let struct25: {} = @make_struct{};
>     let var41: [ `0 {} ] = @make_union<0, struct25>;
>     let discr19: int = @get_union_id<var41>;
>     switch discr19 {
>     0 -> {
>       let discr20: int = @get_union_id<f>;
>       switch discr20 {
>       0 -> {
>         let payload17:
>               {
>                {
>                 [
>                    `0 {
>                        {
>                         Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                               ]),
>                          [ `0 { { str } }, `1 {} ]
>                         ,
>                        }
>                        ,
>                       }
>                 ]
>                 ,
>                }
>                ,
>               }
>           = @get_union_struct<f>;
>         let captures7:
>               {
>                [
>                   `0 {
>                       {
>                        Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                              ]),
>                         [ `0 { { str } }, `1 {} ]
>                        ,
>                       }
>                       ,
>                      }
>                ]
>                ,
>               }
>           = @get_struct_field<payload17, 0>;
>         let var42: str = "stdin";
>         let var43: str = @call_kfn(itos, i);
>         let var44: str = @call_kfn(str_concat, var42, var43);
>         @call_direct(clos23, var44, captures7)
>       }
>       } in join join18;
>       @call_direct(handle2, join18)
>     }
>     } in join join19;
>     let discr21: int = @get_union_id<join19>;
>     switch discr21 {
>     0 -> {
>       let payload18:
>             {
>              {
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 {
>                      [
>                         `0 {
>                             {
>                              [
>                                 `0 {
>                                     {
>                                      Box!a([
>                                               `0 {},
>                                               `1 {
>                                                   {
>                                                    !a,
>                                                     [ `0 { { str } }, `1 {} ]
>                                                    ,
>                                                   }
>                                                   ,
>                                                  }
>                                            ]),
>                                       [ `0 { { str } }, `1 {} ]
>                                      ,
>                                     }
>                                     ,
>                                    }
>                              ]
>                              ,
>                             }
>                             ,
>                            }
>                      ]
>                      ,
>                     },
>                  `2 {
>                      str,
>                       [
>                          `0 {
>                              {
>                               Box!a([
>                                        `0 {},
>                                        `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                     ])
>                               ,
>                              }
>                              ,
>                             }
>                       ]
>                      ,
>                     }
>               ]
>               ,
>              }
>              ,
>             }
>         = @get_union_struct<join19>;
>       let captures9:
>             {
>              [
>                 `0 { [ `0 { [] }, `1 { {} } ] },
>                 `1 {
>                     [
>                        `0 {
>                            {
>                             [
>                                `0 {
>                                    {
>                                     Box!a([
>                                              `0 {},
>                                              `1 {
>                                                  {
>                                                   !a,
>                                                    [ `0 { { str } }, `1 {} ]
>                                                   ,
>                                                  }
>                                                  ,
>                                                 }
>                                           ]),
>                                      [ `0 { { str } }, `1 {} ]
>                                     ,
>                                    }
>                                    ,
>                                   }
>                             ]
>                             ,
>                            }
>                            ,
>                           }
>                     ]
>                     ,
>                    },
>                 `2 {
>                     str,
>                      [
>                         `0 {
>                             {
>                              Box!a([
>                                       `0 {},
>                                       `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                    ])
>                              ,
>                             }
>                             ,
>                            }
>                      ]
>                     ,
>                    }
>              ]
>              ,
>             }
>         = @get_struct_field<payload18, 0>;
>       let var45: int = 1;
>       let var46: int = @call_kfn(add, i, var45);
>       @call_direct(clos21, var46, captures9)
>     }
>     } in join join20;
>     let discr22: int = @get_union_id<join20>;
>     switch discr22 {
>     0 -> {
>       let payload19:
>             {
>              {
>               int,
>                [
>                   `0 { [ `0 { [] }, `1 { {} } ] },
>                   `1 {
>                       [
>                          `0 {
>                              {
>                               [
>                                  `0 {
>                                      {
>                                       Box!a([
>                                                `0 {},
>                                                `1 {
>                                                    {
>                                                     !a,
>                                                      [ `0 { { str } }, `1 {} ]
>                                                     ,
>                                                    }
>                                                    ,
>                                                   }
>                                             ]),
>                                        [ `0 { { str } }, `1 {} ]
>                                       ,
>                                      }
>                                      ,
>                                     }
>                               ]
>                               ,
>                              }
>                              ,
>                             }
>                       ]
>                       ,
>                      },
>                   `2 {
>                       str,
>                        [
>                           `0 {
>                               {
>                                Box!a([
>                                         `0 {},
>                                         `1 { { !a, [ `0 { { str } }, `1 {} ] } ,
>                                            }
>                                      ])
>                                ,
>                               }
>                               ,
>                              }
>                        ]
>                       ,
>                      }
>                ]
>               ,
>              }
>              ,
>             }
>         = @get_union_struct<join20>;
>       let captures10:
>             {
>              int,
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 {
>                      [
>                         `0 {
>                             {
>                              [
>                                 `0 {
>                                     {
>                                      Box!a([
>                                               `0 {},
>                                               `1 {
>                                                   {
>                                                    !a,
>                                                     [ `0 { { str } }, `1 {} ]
>                                                    ,
>                                                   }
>                                                   ,
>                                                  }
>                                            ]),
>                                       [ `0 { { str } }, `1 {} ]
>                                      ,
>                                     }
>                                     ,
>                                    }
>                              ]
>                              ,
>                             }
>                             ,
>                            }
>                      ]
>                      ,
>                     },
>                  `2 {
>                      str,
>                       [
>                          `0 {
>                              {
>                               Box!a([
>                                        `0 {},
>                                        `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                     ])
>                               ,
>                              }
>                              ,
>                             }
>                       ]
>                      ,
>                     }
>               ]
>              ,
>             }
>         = @get_struct_field<payload19, 0>;
>       let struct26: { Box!a([ `0 {}, `1 { !a }, `2 { str, !a } ]) }
>         = @make_struct{ t };
>       let var47: Box!a([ `0 {}, `1 { !a }, `2 { str, !a } ])
>         = @make_union<1, struct26>;
>       @call_direct(clos22, var47, captures10)
>     }
>     } in join join21;
>     join21
>   }
>   2 -> {
>     let payload20:
>           {
>            str,
>             [
>                `0 {
>                    { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) ,
>                    }
>                    ,
>                   }
>             ]
>            ,
>           }
>       = @get_union_struct<op1>;
>     let s2: str = @get_struct_field<payload20, 0>;
>     let f1:
>           [
>              `0 {
>                  { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) }
>                  ,
>                 }
>           ]
>       = @get_struct_field<payload20, 1>;
>     let struct27: {} = @make_struct{};
>     let var48: [ `0 {} ] = @make_union<0, struct27>;
>     let discr23: int = @get_union_id<var48>;
>     switch discr23 {
>     0 -> {
>       let discr24: int = @get_union_id<f1>;
>       switch discr24 {
>       0 -> {
>         let payload21:
>               { { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) } ,
>               }
>           = @get_union_struct<f1>;
>         let captures11:
>               { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) }
>           = @get_struct_field<payload21, 0>;
>         let var49: {} = @make_struct{};
>         @call_direct(clos24, var49, captures11)
>       }
>       } in join join22;
>       @call_direct(handle2, join22)
>     }
>     } in join join23;
>     let discr25: int = @get_union_id<join23>;
>     switch discr25 {
>     0 -> {
>       let payload22:
>             {
>              {
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 {
>                      [
>                         `0 {
>                             {
>                              [
>                                 `0 {
>                                     {
>                                      Box!a([
>                                               `0 {},
>                                               `1 {
>                                                   {
>                                                    !a,
>                                                     [ `0 { { str } }, `1 {} ]
>                                                    ,
>                                                   }
>                                                   ,
>                                                  }
>                                            ]),
>                                       [ `0 { { str } }, `1 {} ]
>                                      ,
>                                     }
>                                     ,
>                                    }
>                              ]
>                              ,
>                             }
>                             ,
>                            }
>                      ]
>                      ,
>                     },
>                  `2 {
>                      str,
>                       [
>                          `0 {
>                              {
>                               Box!a([
>                                        `0 {},
>                                        `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                     ])
>                               ,
>                              }
>                              ,
>                             }
>                       ]
>                      ,
>                     }
>               ]
>               ,
>              }
>              ,
>             }
>         = @get_union_struct<join23>;
>       let captures13:
>             {
>              [
>                 `0 { [ `0 { [] }, `1 { {} } ] },
>                 `1 {
>                     [
>                        `0 {
>                            {
>                             [
>                                `0 {
>                                    {
>                                     Box!a([
>                                              `0 {},
>                                              `1 {
>                                                  {
>                                                   !a,
>                                                    [ `0 { { str } }, `1 {} ]
>                                                   ,
>                                                  }
>                                                  ,
>                                                 }
>                                           ]),
>                                      [ `0 { { str } }, `1 {} ]
>                                     ,
>                                    }
>                                    ,
>                                   }
>                             ]
>                             ,
>                            }
>                            ,
>                           }
>                     ]
>                     ,
>                    },
>                 `2 {
>                     str,
>                      [
>                         `0 {
>                             {
>                              Box!a([
>                                       `0 {},
>                                       `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                    ])
>                              ,
>                             }
>                             ,
>                            }
>                      ]
>                     ,
>                    }
>              ]
>              ,
>             }
>         = @get_struct_field<payload22, 0>;
>       let var50: int = 1;
>       let var51: int = @call_kfn(add, i, var50);
>       @call_direct(clos21, var51, captures13)
>     }
>     } in join join24;
>     let discr26: int = @get_union_id<join24>;
>     switch discr26 {
>     0 -> {
>       let payload23:
>             {
>              {
>               int,
>                [
>                   `0 { [ `0 { [] }, `1 { {} } ] },
>                   `1 {
>                       [
>                          `0 {
>                              {
>                               [
>                                  `0 {
>                                      {
>                                       Box!a([
>                                                `0 {},
>                                                `1 {
>                                                    {
>                                                     !a,
>                                                      [ `0 { { str } }, `1 {} ]
>                                                     ,
>                                                    }
>                                                    ,
>                                                   }
>                                             ]),
>                                        [ `0 { { str } }, `1 {} ]
>                                       ,
>                                      }
>                                      ,
>                                     }
>                               ]
>                               ,
>                              }
>                              ,
>                             }
>                       ]
>                       ,
>                      },
>                   `2 {
>                       str,
>                        [
>                           `0 {
>                               {
>                                Box!a([
>                                         `0 {},
>                                         `1 { { !a, [ `0 { { str } }, `1 {} ] } ,
>                                            }
>                                      ])
>                                ,
>                               }
>                               ,
>                              }
>                        ]
>                       ,
>                      }
>                ]
>               ,
>              }
>              ,
>             }
>         = @get_union_struct<join24>;
>       let captures14:
>             {
>              int,
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 {
>                      [
>                         `0 {
>                             {
>                              [
>                                 `0 {
>                                     {
>                                      Box!a([
>                                               `0 {},
>                                               `1 {
>                                                   {
>                                                    !a,
>                                                     [ `0 { { str } }, `1 {} ]
>                                                    ,
>                                                   }
>                                                   ,
>                                                  }
>                                            ]),
>                                       [ `0 { { str } }, `1 {} ]
>                                      ,
>                                     }
>                                     ,
>                                    }
>                              ]
>                              ,
>                             }
>                             ,
>                            }
>                      ]
>                      ,
>                     },
>                  `2 {
>                      str,
>                       [
>                          `0 {
>                              {
>                               Box!a([
>                                        `0 {},
>                                        `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                     ])
>                               ,
>                              }
>                              ,
>                             }
>                       ]
>                      ,
>                     }
>               ]
>              ,
>             }
>         = @get_struct_field<payload23, 0>;
>       let struct28: { str, Box!a([ `0 {}, `1 { !a }, `2 { str, !a } ]) }
>         = @make_struct{ s2, t };
>       let var52: Box!a([ `0 {}, `1 { !a }, `2 { str, !a } ])
>         = @make_union<2, struct28>;
>       @call_direct(clos22, var52, captures14)
>     }
>     } in join join25;
>     join25
>   }
>   } in join join26;
>   return join26;
> }
> 
> fn clos21(
>   i: int,
>    captures50:
>      {
>       [
>          `0 { [ `0 { [] }, `1 { {} } ] },
>          `1 {
>              [
>                 `0 {
>                     {
>                      [
>                         `0 {
>                             {
>                              Box!a([
>                                       `0 {},
>                                       `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                    ]),
>                               [ `0 { { str } }, `1 {} ]
>                              ,
>                             }
>                             ,
>                            }
>                      ]
>                      ,
>                     }
>                     ,
>                    }
>              ]
>              ,
>             },
>          `2 {
>              str,
>               [
>                  `0 {
>                      {
>                       Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ])
>                       ,
>                      }
>                      ,
>                     }
>               ]
>              ,
>             }
>       ]
>       ,
>      }):
>   [
>      `0 {
>          {
>           int,
>            [
>               `0 { [ `0 { [] }, `1 { {} } ] },
>               `1 {
>                   [
>                      `0 {
>                          {
>                           [
>                              `0 {
>                                  {
>                                   Box!a([
>                                            `0 {},
>                                            `1 {
>                                                { !a, [ `0 { { str } }, `1 {} ] ,
>                                                }
>                                                ,
>                                               }
>                                         ]),
>                                    [ `0 { { str } }, `1 {} ]
>                                   ,
>                                  }
>                                  ,
>                                 }
>                           ]
>                           ,
>                          }
>                          ,
>                         }
>                   ]
>                   ,
>                  },
>               `2 {
>                   str,
>                    [
>                       `0 {
>                           {
>                            Box!a([
>                                     `0 {},
>                                     `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                  ])
>                            ,
>                           }
>                           ,
>                          }
>                    ]
>                   ,
>                  }
>            ]
>           ,
>          }
>          ,
>         }
>   ]
> {
>   let op1:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 {
>                [
>                   `0 {
>                       {
>                        [
>                           `0 {
>                               {
>                                Box!a([
>                                         `0 {},
>                                         `1 { { !a, [ `0 { { str } }, `1 {} ] } ,
>                                            }
>                                      ]),
>                                 [ `0 { { str } }, `1 {} ]
>                                ,
>                               }
>                               ,
>                              }
>                        ]
>                        ,
>                       }
>                       ,
>                      }
>                ]
>                ,
>               },
>            `2 {
>                str,
>                 [
>                    `0 {
>                        {
>                         Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                               ])
>                         ,
>                        }
>                        ,
>                       }
>                 ]
>                ,
>               }
>         ]
>     = @get_struct_field<captures50, 0>;
>   let var53:
>         {
>          int,
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 {
>                  [
>                     `0 {
>                         {
>                          [
>                             `0 {
>                                 {
>                                  Box!a([
>                                           `0 {},
>                                           `1 {
>                                               { !a, [ `0 { { str } }, `1 {} ] }
>                                               ,
>                                              }
>                                        ]),
>                                   [ `0 { { str } }, `1 {} ]
>                                  ,
>                                 }
>                                 ,
>                                }
>                          ]
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                  ,
>                 },
>              `2 {
>                  str,
>                   [
>                      `0 {
>                          {
>                           Box!a([
>                                    `0 {},
>                                    `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                 ])
>                           ,
>                          }
>                          ,
>                         }
>                   ]
>                  ,
>                 }
>           ]
>          ,
>         }
>     = @make_struct{ i, op1 };
>   let struct30:
>         {
>          {
>           int,
>            [
>               `0 { [ `0 { [] }, `1 { {} } ] },
>               `1 {
>                   [
>                      `0 {
>                          {
>                           [
>                              `0 {
>                                  {
>                                   Box!a([
>                                            `0 {},
>                                            `1 {
>                                                { !a, [ `0 { { str } }, `1 {} ] ,
>                                                }
>                                                ,
>                                               }
>                                         ]),
>                                    [ `0 { { str } }, `1 {} ]
>                                   ,
>                                  }
>                                  ,
>                                 }
>                           ]
>                           ,
>                          }
>                          ,
>                         }
>                   ]
>                   ,
>                  },
>               `2 {
>                   str,
>                    [
>                       `0 {
>                           {
>                            Box!a([
>                                     `0 {},
>                                     `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                  ])
>                            ,
>                           }
>                           ,
>                          }
>                    ]
>                   ,
>                  }
>            ]
>           ,
>          }
>          ,
>         }
>     = @make_struct{ var53 };
>   let var54:
>         [
>            `0 {
>                {
>                 int,
>                  [
>                     `0 { [ `0 { [] }, `1 { {} } ] },
>                     `1 {
>                         [
>                            `0 {
>                                {
>                                 [
>                                    `0 {
>                                        {
>                                         Box!a([
>                                                  `0 {},
>                                                  `1 {
>                                                      {
>                                                       !a,
>                                                        [ `0 { { str } }, `1 {}
>                                                        ]
>                                                       ,
>                                                      }
>                                                      ,
>                                                     }
>                                               ]),
>                                          [ `0 { { str } }, `1 {} ]
>                                         ,
>                                        }
>                                        ,
>                                       }
>                                 ]
>                                 ,
>                                }
>                                ,
>                               }
>                         ]
>                         ,
>                        },
>                     `2 {
>                         str,
>                          [
>                             `0 {
>                                 {
>                                  Box!a([
>                                           `0 {},
>                                           `1 {
>                                               { !a, [ `0 { { str } }, `1 {} ] }
>                                               ,
>                                              }
>                                        ])
>                                  ,
>                                 }
>                                 ,
>                                }
>                          ]
>                         ,
>                        }
>                  ]
>                 ,
>                }
>                ,
>               }
>         ]
>     = @make_union<0, struct30>;
>   return var54;
> }
> 
> fn handle2(
>   op1:
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 {
>            [
>               `0 {
>                   {
>                    [
>                       `0 {
>                           {
>                            Box!a([
>                                     `0 {},
>                                     `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                  ]),
>                             [ `0 { { str } }, `1 {} ]
>                            ,
>                           }
>                           ,
>                          }
>                    ]
>                    ,
>                   }
>                   ,
>                  }
>            ]
>            ,
>           },
>        `2 {
>            str,
>             [
>                `0 {
>                    { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) ,
>                    }
>                    ,
>                   }
>             ]
>            ,
>           }
>     ]):
>   [
>      `0 {
>          {
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 {
>                  [
>                     `0 {
>                         {
>                          [
>                             `0 {
>                                 {
>                                  Box!a([
>                                           `0 {},
>                                           `1 {
>                                               { !a, [ `0 { { str } }, `1 {} ] }
>                                               ,
>                                              }
>                                        ]),
>                                   [ `0 { { str } }, `1 {} ]
>                                  ,
>                                 }
>                                 ,
>                                }
>                          ]
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                  ,
>                 },
>              `2 {
>                  str,
>                   [
>                      `0 {
>                          {
>                           Box!a([
>                                    `0 {},
>                                    `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                 ])
>                           ,
>                          }
>                          ,
>                         }
>                   ]
>                  ,
>                 }
>           ]
>           ,
>          }
>          ,
>         }
>   ]
> {
>   let var55:
>         {
>          [
>             `0 { [ `0 { [] }, `1 { {} } ] },
>             `1 {
>                 [
>                    `0 {
>                        {
>                         [
>                            `0 {
>                                {
>                                 Box!a([
>                                          `0 {},
>                                          `1 { { !a, [ `0 { { str } }, `1 {} ] } ,
>                                             }
>                                       ]),
>                                  [ `0 { { str } }, `1 {} ]
>                                 ,
>                                }
>                                ,
>                               }
>                         ]
>                         ,
>                        }
>                        ,
>                       }
>                 ]
>                 ,
>                },
>             `2 {
>                 str,
>                  [
>                     `0 {
>                         {
>                          Box!a([
>                                   `0 {},
>                                   `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                ])
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                 ,
>                }
>          ]
>          ,
>         }
>     = @make_struct{ op1 };
>   let struct31:
>         {
>          {
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 {
>                  [
>                     `0 {
>                         {
>                          [
>                             `0 {
>                                 {
>                                  Box!a([
>                                           `0 {},
>                                           `1 {
>                                               { !a, [ `0 { { str } }, `1 {} ] }
>                                               ,
>                                              }
>                                        ]),
>                                   [ `0 { { str } }, `1 {} ]
>                                  ,
>                                 }
>                                 ,
>                                }
>                          ]
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                  ,
>                 },
>              `2 {
>                  str,
>                   [
>                      `0 {
>                          {
>                           Box!a([
>                                    `0 {},
>                                    `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                 ])
>                           ,
>                          }
>                          ,
>                         }
>                   ]
>                  ,
>                 }
>           ]
>           ,
>          }
>          ,
>         }
>     = @make_struct{ var55 };
>   let var56:
>         [
>            `0 {
>                {
>                 [
>                    `0 { [ `0 { [] }, `1 { {} } ] },
>                    `1 {
>                        [
>                           `0 {
>                               {
>                                [
>                                   `0 {
>                                       {
>                                        Box!a([
>                                                 `0 {},
>                                                 `1 {
>                                                     {
>                                                      !a,
>                                                       [ `0 { { str } }, `1 {} ]
>                                                      ,
>                                                     }
>                                                     ,
>                                                    }
>                                              ]),
>                                         [ `0 { { str } }, `1 {} ]
>                                        ,
>                                       }
>                                       ,
>                                      }
>                                ]
>                                ,
>                               }
>                               ,
>                              }
>                        ]
>                        ,
>                       },
>                    `2 {
>                        str,
>                         [
>                            `0 {
>                                {
>                                 Box!a([
>                                          `0 {},
>                                          `1 { { !a, [ `0 { { str } }, `1 {} ] } ,
>                                             }
>                                       ])
>                                 ,
>                                }
>                                ,
>                               }
>                         ]
>                        ,
>                       }
>                 ]
>                 ,
>                }
>                ,
>               }
>         ]
>     = @make_union<0, struct31>;
>   return var56;
> }
> 
> fn clos20(
>   continue: Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>    captures52: { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] }):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 {
>          [
>             `0 {
>                 {
>                  [
>                     `0 {
>                         {
>                          Box!a([
>                                   `0 {},
>                                   `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                ]),
>                           [ `0 { { str } }, `1 {} ]
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                  ,
>                 }
>                 ,
>                }
>          ]
>          ,
>         },
>      `2 {
>          str,
>           [
>              `0 {
>                  { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) }
>                  ,
>                 }
>           ]
>          ,
>         }
>   ]
> {
>   let next: [ `0 { { str } }, `1 {} ] = @get_struct_field<captures52, 1>;
>   let fromResult: [ `0 { { str } } ] = @get_struct_field<captures52, 0>;
>   let discr27: int = @get_union_id<fromResult>;
>   switch discr27 {
>   0 -> {
>     let payload25: { { str } } = @get_union_struct<fromResult>;
>     let captures51: { str } = @get_struct_field<payload25, 0>;
>     let var57:
>           {
>            Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>             [ `0 { { str } }, `1 {} ]
>            ,
>           }
>       = @make_struct{ continue, next };
>     let struct32:
>           {
>            {
>             Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]),
>              [ `0 { { str } }, `1 {} ]
>             ,
>            }
>            ,
>           }
>       = @make_struct{ var57 };
>     let var58: Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ])
>       = @make_union<1, struct32>;
>     @call_direct(clos32, var58, captures51)
>   }
>   } in join join27;
>   return join27;
> }
> 
> fn clos19(x2: [ `0 { [] }, `1 { {} } ]):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 {
>          [
>             `0 {
>                 {
>                  [
>                     `0 {
>                         {
>                          Box!a([
>                                   `0 {},
>                                   `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                ]),
>                           [ `0 { { str } }, `1 {} ]
>                          ,
>                         }
>                         ,
>                        }
>                  ]
>                  ,
>                 }
>                 ,
>                }
>          ]
>          ,
>         },
>      `2 {
>          str,
>           [
>              `0 {
>                  { Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ]) }
>                  ,
>                 }
>           ]
>          ,
>         }
>   ]
> {
>   let struct33: { [ `0 { [] }, `1 { {} } ] } = @make_struct{ x2 };
>   let var59:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 {
>                [
>                   `0 {
>                       {
>                        [
>                           `0 {
>                               {
>                                Box!a([
>                                         `0 {},
>                                         `1 { { !a, [ `0 { { str } }, `1 {} ] } ,
>                                            }
>                                      ]),
>                                 [ `0 { { str } }, `1 {} ]
>                                ,
>                               }
>                               ,
>                              }
>                        ]
>                        ,
>                       }
>                       ,
>                      }
>                ]
>                ,
>               },
>            `2 {
>                str,
>                 [
>                    `0 {
>                        {
>                         Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                               ])
>                         ,
>                        }
>                        ,
>                       }
>                 ]
>                ,
>               }
>         ]
>     = @make_union<0, struct33>;
>   return var59;
> }
> 
> fn clos18(next: [ `0 { { str } }, `1 {} ], captures53: { [ `0 { { str } } ] }):
>   [ `0 { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } } ]
> {
>   let fromResult: [ `0 { { str } } ] = @get_struct_field<captures53, 0>;
>   let var60: { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] }
>     = @make_struct{ fromResult, next };
>   let struct34: { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } }
>     = @make_struct{ var60 };
>   let var61: [ `0 { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } } ]
>     = @make_union<0, struct34>;
>   return var61;
> }
> 
> fn clos17(x1: {}):
>   [ `0 { { [] } }, `1 { { [ `0 {} ], [ `0 { { str } }, `1 {} ] } } ]
> {
>   let struct35: {} = @make_struct{};
>   let var62: [ `0 {} ] = @make_union<0, struct35>;
>   let discr28: int = @get_union_id<var62>;
>   switch discr28 {
>   0 -> {
>     let struct36: {} = @make_struct{};
>     let var63: [ `0 {} ] = @make_union<0, struct36>;
>     @call_direct(await4, var63)
>   }
>   } in join join28;
>   let discr29: int = @get_union_id<join28>;
>   switch discr29 {
>   0 -> {
>     let payload26: { { [ `0 {} ] } } = @get_union_struct<join28>;
>     let captures55: { [ `0 {} ] } = @get_struct_field<payload26, 0>;
>     let struct37: {} = @make_struct{};
>     let var64: [ `0 { { str } }, `1 {} ] = @make_union<1, struct37>;
>     @call_direct(clos34, var64, captures55)
>   }
>   } in join join29;
>   return join29;
> }
> 
> fn outLine2(s: str): [ `0 { { str } } ]
> {
>   let var65: { str } = @make_struct{ s };
>   let struct38: { { str } } = @make_struct{ var65 };
>   let var66: [ `0 { { str } } ] = @make_union<0, struct38>;
>   return var66;
> }
> 
> fn await3(fromResult: [ `0 { { str } } ]): [ `0 { { [ `0 { { str } } ] } } ]
> {
>   let var67: { [ `0 { { str } } ] } = @make_struct{ fromResult };
>   let struct39: { { [ `0 { { str } } ] } } = @make_struct{ var67 };
>   let var68: [ `0 { { [ `0 { { str } } ] } } ] = @make_union<0, struct39>;
>   return var68;
> }
> 
> fn main_thunk(): [ `0 { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } } ]
> {
>   let struct40: {} = @make_struct{};
>   let var69: [ `0 {} ] = @make_union<0, struct40>;
>   let discr30: int = @get_union_id<var69>;
>   switch discr30 {
>   0 -> {
>     let struct41: {} = @make_struct{};
>     let var70: [ `0 {} ] = @make_union<0, struct41>;
>     let discr31: int = @get_union_id<var70>;
>     switch discr31 {
>     0 -> {
>       let var71: str = "What's your first name?";
>       @call_direct(outLine2, var71)
>     }
>     } in join join30;
>     @call_direct(await3, join30)
>   }
>   } in join join31;
>   let discr32: int = @get_union_id<join31>;
>   switch discr32 {
>   0 -> {
>     let payload27: { { [ `0 { { str } } ] } } = @get_union_struct<join31>;
>     let captures2: { [ `0 { { str } } ] } = @get_struct_field<payload27, 0>;
>     let struct42: {} = @make_struct{};
>     let var72: [ `0 { { str } }, `1 {} ] = @make_union<1, struct42>;
>     @call_direct(clos18, var72, captures2)
>   }
>   } in join join32;
>   return join32;
> }
> 
> global main:
>   [ `0 { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } } ]
>   = @call_direct(main_thunk);
> 
> fn main_handler_thunk():
>   [
>      `0 { [ `0 { [] }, `1 { {} } ], Box!a([ `0 {}, `1 { !a }, `2 { str, !a } ]) ,
>         }
>   ]
> {
>   let discr33: int = @get_union_id<main>;
>   switch discr33 {
>   0 -> {
>     let payload28: { { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] } }
>       = @get_union_struct<main>;
>     let captures3: { [ `0 { { str } } ], [ `0 { { str } }, `1 {} ] }
>       = @get_struct_field<payload28, 0>;
>     let struct43: {} = @make_struct{};
>     let var73: Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } } ])
>       = @make_union<0, struct43>;
>     @call_direct(clos20, var73, captures3)
>   }
>   } in join join33;
>   let op:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 {
>                [
>                   `0 {
>                       {
>                        [
>                           `0 {
>                               {
>                                Box!a([
>                                         `0 {},
>                                         `1 { { !a, [ `0 { { str } }, `1 {} ] } ,
>                                            }
>                                      ]),
>                                 [ `0 { { str } }, `1 {} ]
>                                ,
>                               }
>                               ,
>                              }
>                        ]
>                        ,
>                       }
>                       ,
>                      }
>                ]
>                ,
>               },
>            `2 {
>                str,
>                 [
>                    `0 {
>                        {
>                         Box!a([ `0 {}, `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                               ])
>                         ,
>                        }
>                        ,
>                       }
>                 ]
>                ,
>               }
>         ]
>     = join33;
>   let struct44: {} = @make_struct{};
>   let var74: [ `0 {} ] = @make_union<0, struct44>;
>   let discr34: int = @get_union_id<var74>;
>   switch discr34 {
>   0 -> {
>     @call_direct(handle2, op)
>   }
>   } in join join34;
>   let discr35: int = @get_union_id<join34>;
>   switch discr35 {
>   0 -> {
>     let payload29:
>           {
>            {
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 {
>                    [
>                       `0 {
>                           {
>                            [
>                               `0 {
>                                   {
>                                    Box!a([
>                                             `0 {},
>                                             `1 {
>                                                 { !a, [ `0 { { str } }, `1 {} ] ,
>                                                 }
>                                                 ,
>                                                }
>                                          ]),
>                                     [ `0 { { str } }, `1 {} ]
>                                    ,
>                                   }
>                                   ,
>                                  }
>                            ]
>                            ,
>                           }
>                           ,
>                          }
>                    ]
>                    ,
>                   },
>                `2 {
>                    str,
>                     [
>                        `0 {
>                            {
>                             Box!a([
>                                      `0 {},
>                                      `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                   ])
>                             ,
>                            }
>                            ,
>                           }
>                     ]
>                    ,
>                   }
>             ]
>             ,
>            }
>            ,
>           }
>       = @get_union_struct<join34>;
>     let captures5:
>           {
>            [
>               `0 { [ `0 { [] }, `1 { {} } ] },
>               `1 {
>                   [
>                      `0 {
>                          {
>                           [
>                              `0 {
>                                  {
>                                   Box!a([
>                                            `0 {},
>                                            `1 {
>                                                { !a, [ `0 { { str } }, `1 {} ] ,
>                                                }
>                                                ,
>                                               }
>                                         ]),
>                                    [ `0 { { str } }, `1 {} ]
>                                   ,
>                                  }
>                                  ,
>                                 }
>                           ]
>                           ,
>                          }
>                          ,
>                         }
>                   ]
>                   ,
>                  },
>               `2 {
>                   str,
>                    [
>                       `0 {
>                           {
>                            Box!a([
>                                     `0 {},
>                                     `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                  ])
>                            ,
>                           }
>                           ,
>                          }
>                    ]
>                   ,
>                  }
>            ]
>            ,
>           }
>       = @get_struct_field<payload29, 0>;
>     let var75: int = 0;
>     @call_direct(clos21, var75, captures5)
>   }
>   } in join join35;
>   let discr36: int = @get_union_id<join35>;
>   switch discr36 {
>   0 -> {
>     let payload30:
>           {
>            {
>             int,
>              [
>                 `0 { [ `0 { [] }, `1 { {} } ] },
>                 `1 {
>                     [
>                        `0 {
>                            {
>                             [
>                                `0 {
>                                    {
>                                     Box!a([
>                                              `0 {},
>                                              `1 {
>                                                  {
>                                                   !a,
>                                                    [ `0 { { str } }, `1 {} ]
>                                                   ,
>                                                  }
>                                                  ,
>                                                 }
>                                           ]),
>                                      [ `0 { { str } }, `1 {} ]
>                                     ,
>                                    }
>                                    ,
>                                   }
>                             ]
>                             ,
>                            }
>                            ,
>                           }
>                     ]
>                     ,
>                    },
>                 `2 {
>                     str,
>                      [
>                         `0 {
>                             {
>                              Box!a([
>                                       `0 {},
>                                       `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                    ])
>                              ,
>                             }
>                             ,
>                            }
>                      ]
>                     ,
>                    }
>              ]
>             ,
>            }
>            ,
>           }
>       = @get_union_struct<join35>;
>     let captures6:
>           {
>            int,
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 {
>                    [
>                       `0 {
>                           {
>                            [
>                               `0 {
>                                   {
>                                    Box!a([
>                                             `0 {},
>                                             `1 {
>                                                 { !a, [ `0 { { str } }, `1 {} ] ,
>                                                 }
>                                                 ,
>                                                }
>                                          ]),
>                                     [ `0 { { str } }, `1 {} ]
>                                    ,
>                                   }
>                                   ,
>                                  }
>                            ]
>                            ,
>                           }
>                           ,
>                          }
>                    ]
>                    ,
>                   },
>                `2 {
>                    str,
>                     [
>                        `0 {
>                            {
>                             Box!a([
>                                      `0 {},
>                                      `1 { { !a, [ `0 { { str } }, `1 {} ] } }
>                                   ])
>                             ,
>                            }
>                            ,
>                           }
>                     ]
>                    ,
>                   }
>             ]
>            ,
>           }
>       = @get_struct_field<payload30, 0>;
>     let struct45: {} = @make_struct{};
>     let var76: Box!a([ `0 {}, `1 { !a }, `2 { str, !a } ])
>       = @make_union<0, struct45>;
>     @call_direct(clos22, var76, captures6)
>   }
>   } in join join36;
>   return join36;
> }
> 
> entry main_handler:
>   [
>      `0 { [ `0 { [] }, `1 { {} } ], Box!a([ `0 {}, `1 { !a }, `2 { str, !a } ]) ,
>         }
>   ]
>   = @call_direct(main_handler_thunk);

> cor-out +eval -print
> main_handler = [0 [1 []]
>                [2
>                [72 101 108 108 111 32 115 116 100 105 110 49 32 115 116 100 105
>                110 51 33]
>                [1
>                [2
>                [87 104 97 116 39 115 32 121 111 117 114 32 108 97 115 116 32
>                110 97 109 101 63]
>                [1
>                [2
>                [87 104 97 116 39 115 32 121 111 117 114 32 102 105 114 115 116
>                32 110 97 109 101 63] [0]]]]]]]
>              > Done (Ok {})
>                  (Stdout "Hello stdin1 stdin3!"
>                     (Stdin
>                        (Stdout "What's your last name?"
>                           (Stdin
>                              (Stdout "What's your first name?" (EntryPoint ))))))