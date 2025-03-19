# cor +solve -elab
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

Task v op : (v -> op) -> op

sig await : Task a op -> (a -> Task b op) -> Task b op
let await = \fromResult -> \next ->
#            ^^^^^^^^^^     ^^^^
    \continue ->
#    ^^^^^^^^
        fromResult (\result ->
#                    ^^^^^^
            let inner = next result in
#               ^^^^^
            inner continue)
;;

OpIn a b : [
    StdinLine (Str -> OpIn a b),
    Done a,
]b

sig lineIn : Task Str (OpIn * *)
let lineIn = \toNext -> StdinLine (\s -> toNext s)
;;

OpOut a b : [
    StdoutLine Str (Str -> OpOut a b),
    Done a,
]b

sig lineOut : Str -> Task Str (OpOut * *)
let lineOut = \s -> (\toNext -> StdoutLine s (\x -> toNext x))
;;

Op a : [
    StdinLine (Str -> Op a),
    StdoutLine Str (Str -> Op a),
    Done a,
]

sig main : Task Str (Op *)
let main = await lineIn (\s -> lineOut s)
#   ^^^^   ^^^^^ ^^^^^^
;;

run main_handler =
    let op = main (\x -> Done x) in
#            ^^^^
    let handle = \op -> when op is
#       ^^^^^^
        | StdinLine f -> handle (f "hello")
        | StdoutLine s f -> handle (f s)
        | Done x -> x
    end
    in
    handle op
;;

> cor-out +solve -elab
> 
> Task v op : (v -> op) -> op
> 
> sig await : Task a op -> (a -> Task b op) -> Task b op
> let await = \fromResult -> \next ->
> #                           ^^^^ 'a -> Task 'b 'op
> #            ^^^^^^^^^^ Task 'a 'op
>     \continue ->
> #    ^^^^^^^^ 'b -> 'op
>         fromResult (\result ->
> #                    ^^^^^^ 'a
>             let inner = next result in
> #               ^^^^^ Task 'b 'op
>             inner continue)
> ;;
> 
> OpIn a b : [
>     StdinLine (Str -> OpIn a b),
>     Done a,
> ]b
> 
> sig lineIn : Task Str (OpIn * *)
> let lineIn = \toNext -> StdinLine (\s -> toNext s)
> ;;
> 
> OpOut a b : [
>     StdoutLine Str (Str -> OpOut a b),
>     Done a,
> ]b
> 
> sig lineOut : Str -> Task Str (OpOut * *)
> let lineOut = \s -> (\toNext -> StdoutLine s (\x -> toNext x))
> ;;
> 
> Op a : [
>     StdinLine (Str -> Op a),
>     StdoutLine Str (Str -> Op a),
>     Done a,
> ]
> 
> sig main : Task Str (Op *)
> let main = await lineIn (\s -> lineOut s)
> #                ^^^^^^ Task Str (Op Str)
> #          ^^^^^ (Task Str (Op Str))
> #          ^^^^^   -> (Str -> Task Str (Op Str))
> #          ^^^^^        -> Task Str (Op Str)
> #   ^^^^ Task Str (Op Str)
> ;;
> 
> run main_handler =
>     let op = main (\x -> Done x) in
> #            ^^^^ Task Str (Op Str)
>     let handle = \op -> when op is
> #       ^^^^^^ (Op Str) -> Str
>         | StdinLine f -> handle (f "hello")
>         | StdoutLine s f -> handle (f s)
>         | Done x -> x
>     end
>     in
>     handle op
> ;;
> 

> cor-out +monotype -print
> let lineOut1: Str
>                 -> (Str
>                      -> [
>                           Done Str,
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str (Str -> <rec>)
>                           ])
>                      -> [
>                           Done Str,
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str (Str -> <rec>)
>                           ] = \s1 ->
>   \toNext1 -> (StdoutLine s1 \x -> (toNext1 x))
> let lineIn1: (Str
>                -> [
>                     Done Str,
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str (Str -> <rec>)
>                     ])
>                -> [
>                     Done Str,
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str (Str -> <rec>)
>                     ] = \toNext ->
>   StdinLine \s -> (toNext s)
> let await1: ((Str
>                -> [
>                     Done Str,
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str (Str -> <rec>)
>                     ])
>               -> [
>                    Done Str,
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str (Str -> <rec>)
>                    ])
>               -> (Str
>                    -> (Str
>                         -> [
>                              Done Str,
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str (Str -> <rec>)
>                              ])
>                         -> [
>                              Done Str,
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str (Str -> <rec>)
>                              ])
>                    -> (Str
>                         -> [
>                              Done Str,
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str (Str -> <rec>)
>                              ])
>                         -> [
>                              Done Str,
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str (Str -> <rec>)
>                              ] = \fromResult ->
>   \next ->
>     \continue ->
>       (fromResult
>          \result ->
>            (let inner: (Str
>                          -> [
>                               Done Str,
>                               StdinLine (Str -> <rec>),
>                               StdoutLine Str (Str -> <rec>)
>                               ])
>                          -> [
>                               Done Str,
>                               StdinLine (Str -> <rec>),
>                               StdoutLine Str (Str -> <rec>)
>                               ] =
>               next result
>            in
>            inner continue))
> let main: (Str
>             -> [
>                  Done Str,
>                  StdinLine (Str -> <rec>),
>                  StdoutLine Str (Str -> <rec>)
>                  ])
>             -> [
>                  Done Str,
>                  StdinLine (Str -> <rec>),
>                  StdoutLine Str (Str -> <rec>)
>                  ] =
>   (await1 lineIn1) \s2 -> (lineOut1 s2)
> run main_handler: Str =
>   let op: [Done Str, StdinLine (Str -> <rec>), StdoutLine Str (Str -> <rec>)] =
>     main \x1 -> (Done x1)
>   in
>   let rec handle: [
>                     Done Str,
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str (Str -> <rec>)
>                     ] -> Str = \op1 ->
>     when op1 is
>       | StdinLine f -> handle (f "hello")
>       | StdoutLine s3 f1 -> handle (f1 s3)
>       | Done x2 -> x2
>     end
>   in
>   handle op

> cor-out +monotype_lifted -print
> let clos(toNext1:
>            Str
>              -> [
>                   Done Str,
>                   StdinLine (Str -> <rec>),
>                   StdoutLine Str (Str -> <rec>)
>                   ]): Str
>                         -> [
>                              Done Str,
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str <rec>
>                              ] = \x ->
>   toNext1 x
> let clos1(s1: Str): (Str
>                       -> [
>                            Done Str,
>                            StdinLine (Str -> <rec>),
>                            StdoutLine Str (Str -> <rec>)
>                            ])
>                       -> [
>                            Done Str,
>                            StdinLine (Str -> <rec>),
>                            StdoutLine Str (Str -> <rec>)
>                            ] = \toNext1 ->
>   StdoutLine s1 clos
> let lineOut1: Str
>                 -> (Str
>                      -> [
>                           Done Str,
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str (Str -> <rec>)
>                           ])
>                      -> [
>                           Done Str,
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str (Str -> <rec>)
>                           ] = \s1 ->
>   clos1
> let clos2(toNext:
>             Str
>               -> [
>                    Done Str,
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str (Str -> <rec>)
>                    ]): Str
>                          -> [
>                               Done Str,
>                               StdinLine <rec>,
>                               StdoutLine Str (Str -> <rec>)
>                               ] = \s ->
>   toNext s
> let lineIn1: (Str
>                -> [
>                     Done Str,
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str (Str -> <rec>)
>                     ])
>                -> [
>                     Done Str,
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str (Str -> <rec>)
>                     ] = \toNext ->
>   StdinLine clos2
> let clos3(continue:
>             Str
>               -> [
>                    Done Str,
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str (Str -> <rec>)
>                    ]
>            next:
>              Str
>                -> (Str
>                     -> [
>                          Done Str,
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str (Str -> <rec>)
>                          ])
>                     -> [
>                          Done Str,
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str (Str -> <rec>)
>                          ]): Str
>                                -> [
>                                     Done Str,
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str (Str -> <rec>)
>                                     ] = \result ->
>   let inner: (Str
>                -> [
>                     Done Str,
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str (Str -> <rec>)
>                     ])
>                -> [
>                     Done Str,
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str (Str -> <rec>)
>                     ] =
>     next result
>   in
>   inner continue
> let clos4(fromResult:
>             (Str
>               -> [
>                    Done Str,
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str (Str -> <rec>)
>                    ])
>               -> [
>                    Done Str,
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str (Str -> <rec>)
>                    ]
>            next:
>              Str
>                -> (Str
>                     -> [
>                          Done Str,
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str (Str -> <rec>)
>                          ])
>                     -> [
>                          Done Str,
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str (Str -> <rec>)
>                          ]): (Str
>                                -> [
>                                     Done Str,
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str (Str -> <rec>)
>                                     ])
>                                -> [
>                                     Done Str,
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str (Str -> <rec>)
>                                     ] = \continue ->
>   fromResult clos3
> let clos5(fromResult:
>             (Str
>               -> [
>                    Done Str,
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str (Str -> <rec>)
>                    ])
>               -> [
>                    Done Str,
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str (Str -> <rec>)
>                    ]): (Str
>                          -> (Str
>                               -> [
>                                    Done Str,
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str (Str -> <rec>)
>                                    ])
>                               -> [
>                                    Done Str,
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str (Str -> <rec>)
>                                    ])
>                          -> (Str
>                               -> [
>                                    Done Str,
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str (Str -> <rec>)
>                                    ])
>                               -> [
>                                    Done Str,
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str (Str -> <rec>)
>                                    ] = \next ->
>   clos4
> let await1: ((Str
>                -> [
>                     Done Str,
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str (Str -> <rec>)
>                     ])
>               -> [
>                    Done Str,
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str (Str -> <rec>)
>                    ])
>               -> (Str
>                    -> (Str
>                         -> [
>                              Done Str,
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str (Str -> <rec>)
>                              ])
>                         -> [
>                              Done Str,
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str (Str -> <rec>)
>                              ])
>                    -> (Str
>                         -> [
>                              Done Str,
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str (Str -> <rec>)
>                              ])
>                         -> [
>                              Done Str,
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str (Str -> <rec>)
>                              ] = \fromResult ->
>   clos5
> let clos6: Str
>              -> (Str
>                   -> [
>                        Done Str,
>                        StdinLine (Str -> <rec>),
>                        StdoutLine Str (Str -> <rec>)
>                        ])
>                   -> [
>                        Done Str,
>                        StdinLine (Str -> <rec>),
>                        StdoutLine Str (Str -> <rec>)
>                        ] = \s2 ->
>   lineOut1 s2
> let main: (Str
>             -> [
>                  Done Str,
>                  StdinLine (Str -> <rec>),
>                  StdoutLine Str (Str -> <rec>)
>                  ])
>             -> [
>                  Done Str,
>                  StdinLine (Str -> <rec>),
>                  StdoutLine Str (Str -> <rec>)
>                  ] =
>   (await1 lineIn1) clos6
> let clos7: Str
>              -> [
>                   Done Str,
>                   StdinLine (Str -> <rec>),
>                   StdoutLine Str (Str -> <rec>)
>                   ] = \x1 ->
>   Done x1
> let handle1: [
>                Done Str,
>                StdinLine (Str -> <rec>),
>                StdoutLine Str (Str -> <rec>)
>                ] -> Str = \op1 ->
>   when op1 is
>     | StdinLine f -> handle1 (f "hello")
>     | StdoutLine s3 f1 -> handle1 (f1 s3)
>     | Done x2 -> x2
>   end
> run main_handler: Str =
>   let op: [Done Str, StdinLine (Str -> <rec>), StdoutLine Str (Str -> <rec>)] =
>     main clos7
>   in
>   handle1 op

> cor-out +lambdasolved -print
> let clos2(toNext:
>             Str
>               -<'1687>-> [
>                            Done Str,
>                            StdinLine (Str -[clos2 (toNext: <rec>)]-> <rec>),
>                            StdoutLine Str (Str -<'1700>-> <rec>)
>                            ]): Str
>                                  -[
>                                     clos2
>                                       (toNext: (Str
>                                                  -<'1687>-> [
>                                                               Done Str,
>                                                               StdinLine <rec>,
>                                                               StdoutLine Str
>                                                                 (Str
>                                                                   -<'1700>-> 
>                                                                   <rec>)
>                                                               ]))
>                                     ]-> [
>                                           Done Str,
>                                           StdinLine <rec>,
>                                           StdoutLine Str (Str -<'1700>-> <rec>)
>                                           ] = \s ->
>   toNext s
> let lineIn1: Str
>                -<'1769>-> [
>                             Done Str,
>                             StdinLine (Str -[clos2 (toNext: <rec>)]-> <rec>),
>                             StdoutLine Str (Str -<'1770>-> <rec>)
>                             ]
>                -[lineIn1]-> [
>                               Done Str,
>                               StdinLine
>                                 (Str
>                                   -[clos2 (toNext: (Str -<'1769>-> <rec>))]-> 
>                                   <rec>),
>                               StdoutLine Str (Str -<'1770>-> <rec>)
>                               ] = \toNext ->
>   StdinLine clos2
> let clos(toNext1:
>            Str
>              -<'1793>-> [
>                           Done Str,
>                           StdinLine (Str -<'1808>-> <rec>),
>                           StdoutLine Str (Str -[clos (toNext1: <rec>)]-> <rec>)
>                           ]): Str
>                                 -[
>                                    clos
>                                      (toNext1: (Str
>                                                  -<'1793>-> [
>                                                               Done Str,
>                                                               StdinLine
>                                                                 (Str
>                                                                   -<'1808>-> 
>                                                                   <rec>),
>                                                               StdoutLine Str
>                                                                 <rec>
>                                                               ]))
>                                    ]-> [
>                                          Done Str,
>                                          StdinLine (Str -<'1808>-> <rec>),
>                                          StdoutLine Str <rec>
>                                          ] = \x ->
>   toNext1 x
> let clos1(s1: Str): Str
>                       -<'1877>-> [
>                                    Done Str,
>                                    StdinLine (Str -<'1879>-> <rec>),
>                                    StdoutLine Str
>                                      (Str -[clos (toNext1: <rec>)]-> <rec>)
>                                    ]
>                       -[clos1 (s1: Str)]-> [
>                                              Done Str,
>                                              StdinLine (Str -<'1879>-> <rec>),
>                                              StdoutLine Str
>                                                (Str
>                                                  -[
>                                                     clos
>                                                       (toNext1: (Str
>                                                                   -<'1877>-> 
>                                                                   <rec>))
>                                                     ]-> <rec>)
>                                              ] = \toNext1 ->
>   StdoutLine s1 clos
> let lineOut1: Str
>                 -[lineOut1]-> Str
>                                 -<'1946>-> [
>                                              Done Str,
>                                              StdinLine (Str -<'1949>-> <rec>),
>                                              StdoutLine Str
>                                                (Str
>                                                  -[clos (toNext1: <rec>)]-> 
>                                                  <rec>)
>                                              ]
>                                 -[clos1 (s1: Str)]-> [
>                                                        Done Str,
>                                                        StdinLine
>                                                          (Str -<'1949>-> <rec>),
>                                                        StdoutLine Str
>                                                          (Str
>                                                            -[
>                                                               clos
>                                                                 (toNext1: 
>                                                                 (Str
>                                                                   -<'1946>-> 
>                                                                   <rec>))
>                                                               ]-> <rec>)
>                                                        ] = \s1 ->
>   clos1
> let clos6: Str
>              -[clos6]-> Str
>                           -<'2043>-> [
>                                        Done Str,
>                                        StdinLine (Str -<'2046>-> <rec>),
>                                        StdoutLine Str
>                                          (Str -[clos (toNext1: <rec>)]-> <rec>)
>                                        ]
>                           -[clos1 (s1: Str)]-> [
>                                                  Done Str,
>                                                  StdinLine
>                                                    (Str -<'2046>-> <rec>),
>                                                  StdoutLine Str
>                                                    (Str
>                                                      -[
>                                                         clos
>                                                           (toNext1: (Str
>                                                                     -<'2043>-> 
>                                                                     <rec>))
>                                                         ]-> <rec>)
>                                                  ] = \s2 ->
>   lineOut1 s2
> let clos3(continue:
>             Str
>               -<'2113>-> [
>                            Done Str,
>                            StdinLine (Str -<'2141>-> <rec>),
>                            StdoutLine Str (Str -<'2139>-> <rec>)
>                            ], next:
>                                 Str
>                                   -<'2070>-> Str
>                                                -<'2113>-> [
>                                                             Done Str,
>                                                             StdinLine
>                                                               (Str
>                                                                 -<'2141>-> 
>                                                                 <rec>),
>                                                             StdoutLine Str
>                                                               (Str
>                                                                 -<'2139>-> 
>                                                                 <rec>)
>                                                             ]
>                                                -<'2120>-> [
>                                                             Done Str,
>                                                             StdinLine
>                                                               (Str
>                                                                 -<'2141>-> 
>                                                                 <rec>),
>                                                             StdoutLine Str
>                                                               (Str
>                                                                 -<'2139>-> 
>                                                                 <rec>)
>                                                             ]): Str
>                                                                   -[
>                                                                     clos3
>                                                                     (continue: 
>                                                                     (Str
>                                                                     -<'2113>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2141>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2139>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -<'2070>-> 
>                                                                     Str
>                                                                     -<'2113>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2141>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2139>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -<'2120>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2141>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2139>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     ]-> 
>                                                                   [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2141>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2139>-> 
>                                                                     <rec>)
>                                                                     ] = \result ->
>   let inner: Str
>                -<'2113>-> [
>                             Done Str,
>                             StdinLine (Str -<'2141>-> <rec>),
>                             StdoutLine Str (Str -<'2139>-> <rec>)
>                             ]
>                -<'2120>-> [
>                             Done Str,
>                             StdinLine (Str -<'2141>-> <rec>),
>                             StdoutLine Str (Str -<'2139>-> <rec>)
>                             ] =
>     next result
>   in
>   inner continue
> let clos4(fromResult:
>             Str
>               -[
>                  clos3
>                    (continue: (Str
>                                 -<'2275>-> [
>                                              Done Str,
>                                              StdinLine (Str -<'2278>-> <rec>),
>                                              StdoutLine Str
>                                                (Str -<'2276>-> <rec>)
>                                              ]))
>                    (next: (Str
>                             -<'2234>-> Str
>                                          -<'2275>-> [
>                                                       Done Str,
>                                                       StdinLine
>                                                         (Str -<'2278>-> <rec>),
>                                                       StdoutLine Str
>                                                         (Str -<'2276>-> <rec>)
>                                                       ]
>                                          -<'2242>-> [
>                                                       Done Str,
>                                                       StdinLine
>                                                         (Str -<'2278>-> <rec>),
>                                                       StdoutLine Str
>                                                         (Str -<'2276>-> <rec>)
>                                                       ]))
>                  ]-> [
>                        Done Str,
>                        StdinLine (Str -<'2278>-> <rec>),
>                        StdoutLine Str (Str -<'2276>-> <rec>)
>                        ]
>               -<'2263>-> [
>                            Done Str,
>                            StdinLine (Str -<'2278>-> <rec>),
>                            StdoutLine Str (Str -<'2276>-> <rec>)
>                            ], next:
>                                 Str
>                                   -<'2234>-> Str
>                                                -<'2275>-> [
>                                                             Done Str,
>                                                             StdinLine
>                                                               (Str
>                                                                 -<'2278>-> 
>                                                                 <rec>),
>                                                             StdoutLine Str
>                                                               (Str
>                                                                 -<'2276>-> 
>                                                                 <rec>)
>                                                             ]
>                                                -<'2242>-> [
>                                                             Done Str,
>                                                             StdinLine
>                                                               (Str
>                                                                 -<'2278>-> 
>                                                                 <rec>),
>                                                             StdoutLine Str
>                                                               (Str
>                                                                 -<'2276>-> 
>                                                                 <rec>)
>                                                             ]): Str
>                                                                   -<'2275>-> 
>                                                                   [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2278>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2276>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                   -[
>                                                                     clos4
>                                                                     (fromResult: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos3
>                                                                     (continue: 
>                                                                     (Str
>                                                                     -<'2275>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2278>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2276>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -<'2234>-> 
>                                                                     Str
>                                                                     -<'2275>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2278>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2276>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -<'2242>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2278>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2276>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     ]-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2278>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2276>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -<'2263>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2278>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2276>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -<'2234>-> 
>                                                                     Str
>                                                                     -<'2275>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2278>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2276>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -<'2242>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2278>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2276>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     ]-> 
>                                                                   [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2278>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2276>-> 
>                                                                     <rec>)
>                                                                     ] = \continue ->
>   fromResult clos3
> let clos5(fromResult:
>             Str
>               -[
>                  clos3
>                    (continue: (Str
>                                 -<'2437>-> [
>                                              Done Str,
>                                              StdinLine (Str -<'2440>-> <rec>),
>                                              StdoutLine Str
>                                                (Str -<'2438>-> <rec>)
>                                              ]))
>                    (next: (Str
>                             -<'2436>-> Str
>                                          -<'2437>-> [
>                                                       Done Str,
>                                                       StdinLine
>                                                         (Str -<'2440>-> <rec>),
>                                                       StdoutLine Str
>                                                         (Str -<'2438>-> <rec>)
>                                                       ]
>                                          -<'2444>-> [
>                                                       Done Str,
>                                                       StdinLine
>                                                         (Str -<'2440>-> <rec>),
>                                                       StdoutLine Str
>                                                         (Str -<'2438>-> <rec>)
>                                                       ]))
>                  ]-> [
>                        Done Str,
>                        StdinLine (Str -<'2440>-> <rec>),
>                        StdoutLine Str (Str -<'2438>-> <rec>)
>                        ]
>               -<'2408>-> [
>                            Done Str,
>                            StdinLine (Str -<'2440>-> <rec>),
>                            StdoutLine Str (Str -<'2438>-> <rec>)
>                            ]): Str
>                                  -<'2436>-> Str
>                                               -<'2437>-> [
>                                                            Done Str,
>                                                            StdinLine
>                                                              (Str
>                                                                -<'2440>-> 
>                                                                <rec>),
>                                                            StdoutLine Str
>                                                              (Str
>                                                                -<'2438>-> 
>                                                                <rec>)
>                                                            ]
>                                               -<'2444>-> [
>                                                            Done Str,
>                                                            StdinLine
>                                                              (Str
>                                                                -<'2440>-> 
>                                                                <rec>),
>                                                            StdoutLine Str
>                                                              (Str
>                                                                -<'2438>-> 
>                                                                <rec>)
>                                                            ]
>                                  -[
>                                     clos5
>                                       (fromResult: (Str
>                                                       -[
>                                                          clos3
>                                                            (continue: 
>                                                            (Str
>                                                              -<'2437>-> 
>                                                              [
>                                                                Done Str,
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'2440>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  (Str
>                                                                    -<'2438>-> 
>                                                                    <rec>)
>                                                                ]))
>                                                            (next: (Str
>                                                                     -
>                                                                     <'2436>-> 
>                                                                     Str
>                                                                     -<'2437>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2440>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2438>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -<'2444>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2440>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2438>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                          ]-> [
>                                                                Done Str,
>                                                                StdinLine
>                                                                  (Str
>                                                                    -<'2440>-> 
>                                                                    <rec>),
>                                                                StdoutLine Str
>                                                                  (Str
>                                                                    -<'2438>-> 
>                                                                    <rec>)
>                                                                ]
>                                                      -<'2408>-> [
>                                                                   Done Str,
>                                                                   StdinLine
>                                                                     (Str
>                                                                     -<'2440>-> 
>                                                                     <rec>),
>                                                                   StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2438>-> 
>                                                                     <rec>)
>                                                                   ]))
>                                     ]-> Str
>                                           -<'2437>-> [
>                                                        Done Str,
>                                                        StdinLine
>                                                          (Str -<'2440>-> <rec>),
>                                                        StdoutLine Str
>                                                          (Str -<'2438>-> <rec>)
>                                                        ]
>                                           -[
>                                              clos4
>                                                (fromResult: (Str
>                                                                -[
>                                                                   clos3
>                                                                     (continue: 
>                                                                     (Str
>                                                                     -<'2437>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2440>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2438>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -<'2436>-> 
>                                                                     Str
>                                                                     -<'2437>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2440>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2438>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -<'2444>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2440>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2438>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                                   ]-> 
>                                                                [
>                                                                  Done Str,
>                                                                  StdinLine
>                                                                    (Str
>                                                                     -<'2440>-> 
>                                                                     <rec>),
>                                                                  StdoutLine Str
>                                                                    (Str
>                                                                     -<'2438>-> 
>                                                                     <rec>)
>                                                                  ]
>                                                               -<'2408>-> 
>                                                               [
>                                                                 Done Str,
>                                                                 StdinLine
>                                                                   (Str
>                                                                     -
>                                                                     <'2440>-> 
>                                                                     <rec>),
>                                                                 StdoutLine Str
>                                                                   (Str
>                                                                     -
>                                                                     <'2438>-> 
>                                                                     <rec>)
>                                                                 ]))
>                                                (next: (Str
>                                                         -<'2436>-> Str
>                                                                     -<'2437>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2440>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2438>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                     -<'2444>-> 
>                                                                     [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2440>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2438>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                              ]-> [
>                                                    Done Str,
>                                                    StdinLine
>                                                      (Str -<'2440>-> <rec>),
>                                                    StdoutLine Str
>                                                      (Str -<'2438>-> <rec>)
>                                                    ] = \next ->
>   clos4
> let await1: Str
>               -[
>                  clos3
>                    (continue: (Str
>                                 -<'2603>-> [
>                                              Done Str,
>                                              StdinLine (Str -<'2595>-> <rec>),
>                                              StdoutLine Str
>                                                (Str -<'2593>-> <rec>)
>                                              ]))
>                    (next: (Str
>                             -<'2602>-> Str
>                                          -<'2603>-> [
>                                                       Done Str,
>                                                       StdinLine
>                                                         (Str -<'2595>-> <rec>),
>                                                       StdoutLine Str
>                                                         (Str -<'2593>-> <rec>)
>                                                       ]
>                                          -<'2605>-> [
>                                                       Done Str,
>                                                       StdinLine
>                                                         (Str -<'2595>-> <rec>),
>                                                       StdoutLine Str
>                                                         (Str -<'2593>-> <rec>)
>                                                       ]))
>                  ]-> [
>                        Done Str,
>                        StdinLine (Str -<'2595>-> <rec>),
>                        StdoutLine Str (Str -<'2593>-> <rec>)
>                        ]
>               -<'2599>-> [
>                            Done Str,
>                            StdinLine (Str -<'2595>-> <rec>),
>                            StdoutLine Str (Str -<'2593>-> <rec>)
>                            ]
>               -[await1]-> Str
>                             -<'2602>-> Str
>                                          -<'2603>-> [
>                                                       Done Str,
>                                                       StdinLine
>                                                         (Str -<'2595>-> <rec>),
>                                                       StdoutLine Str
>                                                         (Str -<'2593>-> <rec>)
>                                                       ]
>                                          -<'2605>-> [
>                                                       Done Str,
>                                                       StdinLine
>                                                         (Str -<'2595>-> <rec>),
>                                                       StdoutLine Str
>                                                         (Str -<'2593>-> <rec>)
>                                                       ]
>                             -[
>                                clos5
>                                  (fromResult: (Str
>                                                  -[
>                                                     clos3
>                                                       (continue: (Str
>                                                                    -<'2603>-> 
>                                                                    [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2595>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2593>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                       (next: (Str
>                                                                -<'2602>-> 
>                                                                Str
>                                                                  -<'2603>-> 
>                                                                  [
>                                                                    Done Str,
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -<'2595>-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2593>-> 
>                                                                     <rec>)
>                                                                    ]
>                                                                  -<'2605>-> 
>                                                                  [
>                                                                    Done Str,
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -<'2595>-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2593>-> 
>                                                                     <rec>)
>                                                                    ]))
>                                                     ]-> [
>                                                           Done Str,
>                                                           StdinLine
>                                                             (Str
>                                                               -<'2595>-> 
>                                                               <rec>),
>                                                           StdoutLine Str
>                                                             (Str
>                                                               -<'2593>-> 
>                                                               <rec>)
>                                                           ]
>                                                 -<'2599>-> [
>                                                              Done Str,
>                                                              StdinLine
>                                                                (Str
>                                                                  -<'2595>-> 
>                                                                  <rec>),
>                                                              StdoutLine Str
>                                                                (Str
>                                                                  -<'2593>-> 
>                                                                  <rec>)
>                                                              ]))
>                                ]-> Str
>                                      -<'2603>-> [
>                                                   Done Str,
>                                                   StdinLine
>                                                     (Str -<'2595>-> <rec>),
>                                                   StdoutLine Str
>                                                     (Str -<'2593>-> <rec>)
>                                                   ]
>                                      -[
>                                         clos4
>                                           (fromResult: (Str
>                                                           -[
>                                                              clos3
>                                                                (continue: 
>                                                                (Str
>                                                                  -<'2603>-> 
>                                                                  [
>                                                                    Done Str,
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -<'2595>-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2593>-> 
>                                                                     <rec>)
>                                                                    ]))
>                                                                (next: 
>                                                                (Str
>                                                                  -<'2602>-> 
>                                                                  Str
>                                                                    -<'2603>-> 
>                                                                    [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2595>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2593>-> 
>                                                                     <rec>)
>                                                                     ]
>                                                                    -<'2605>-> 
>                                                                    [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2595>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2593>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                                              ]-> [
>                                                                    Done Str,
>                                                                    StdinLine
>                                                                     (Str
>                                                                     -<'2595>-> 
>                                                                     <rec>),
>                                                                    StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2593>-> 
>                                                                     <rec>)
>                                                                    ]
>                                                          -<'2599>-> [
>                                                                     Done Str,
>                                                                     StdinLine
>                                                                     (Str
>                                                                     -<'2595>-> 
>                                                                     <rec>),
>                                                                     StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2593>-> 
>                                                                     <rec>)
>                                                                     ]))
>                                           (next: (Str
>                                                    -<'2602>-> Str
>                                                                 -<'2603>-> 
>                                                                 [
>                                                                   Done Str,
>                                                                   StdinLine
>                                                                     (Str
>                                                                     -<'2595>-> 
>                                                                     <rec>),
>                                                                   StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2593>-> 
>                                                                     <rec>)
>                                                                   ]
>                                                                 -<'2605>-> 
>                                                                 [
>                                                                   Done Str,
>                                                                   StdinLine
>                                                                     (Str
>                                                                     -<'2595>-> 
>                                                                     <rec>),
>                                                                   StdoutLine
>                                                                     Str
>                                                                     (Str
>                                                                     -<'2593>-> 
>                                                                     <rec>)
>                                                                   ]))
>                                         ]-> [
>                                               Done Str,
>                                               StdinLine (Str -<'2595>-> <rec>),
>                                               StdoutLine Str
>                                                 (Str -<'2593>-> <rec>)
>                                               ] = \fromResult ->
>   clos5
> let main: Str
>             -[clos7]-> [
>                          Done Str,
>                          StdinLine
>                            (Str
>                              -[
>                                 clos2
>                                   (toNext: (Str
>                                              -[
>                                                 clos3 (continue: <rec>)
>                                                   (next: (Str
>                                                            -[clos6]-> 
>                                                            <rec>
>                                                              -[clos1 (s1: Str)]-> 
>                                                              <rec>))
>                                                 ]-> <rec>))
>                                 ]-> <rec>),
>                          StdoutLine Str (Str -[clos (toNext1: <rec>)]-> <rec>)
>                          ]
>             -[
>                clos4
>                  (fromResult: (Str
>                                  -[
>                                     clos3
>                                       (continue: (Str
>                                                    -[clos7]-> [
>                                                                 Done Str,
>                                                                 StdinLine
>                                                                   (Str
>                                                                     -
>                                                                     [
>                                                                     clos2
>                                                                     (toNext: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>),
>                                                                 StdoutLine Str
>                                                                   (Str
>                                                                     -
>                                                                     [
>                                                                     clos
>                                                                     (toNext1: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>)
>                                                                 ]))
>                                       (next: (Str
>                                                -[clos6]-> Str
>                                                             -[clos7]-> 
>                                                             [
>                                                               Done Str,
>                                                               StdinLine
>                                                                 (Str
>                                                                   -[
>                                                                     clos2
>                                                                     (toNext: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                   <rec>),
>                                                               StdoutLine Str
>                                                                 (Str
>                                                                   -[
>                                                                     clos
>                                                                     (toNext1: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                   <rec>)
>                                                               ]
>                                                             -[clos1 (s1: Str)]-> 
>                                                             [
>                                                               Done Str,
>                                                               StdinLine
>                                                                 (Str
>                                                                   -[
>                                                                     clos2
>                                                                     (toNext: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                   <rec>),
>                                                               StdoutLine Str
>                                                                 (Str
>                                                                   -[
>                                                                     clos
>                                                                     (toNext1: 
>                                                                     (Str
>                                                                     -[clos7]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>)
>                                                               ]))
>                                     ]-> [
>                                           Done Str,
>                                           StdinLine
>                                             (Str
>                                               -[clos2 (toNext: <rec>)]-> 
>                                               <rec>),
>                                           StdoutLine Str
>                                             (Str
>                                               -[
>                                                  clos
>                                                    (toNext1: (Str
>                                                                -[clos7]-> 
>                                                                <rec>))
>                                                  ]-> <rec>)
>                                           ]
>                                 -[lineIn1]-> [
>                                                Done Str,
>                                                StdinLine
>                                                  (Str
>                                                    -[
>                                                       clos2
>                                                         (toNext: (Str
>                                                                    -[
>                                                                     clos3
>                                                                     (continue: 
>                                                                     (Str
>                                                                     -[clos7]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     (Str
>                                                                     -[clos6]-> 
>                                                                     Str
>                                                                     -[clos7]-> 
>                                                                     <rec>
>                                                                     -[
>                                                                     clos1
>                                                                     (s1: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                    <rec>))
>                                                       ]-> <rec>),
>                                                StdoutLine Str
>                                                  (Str
>                                                    -[
>                                                       clos
>                                                         (toNext1: (Str
>                                                                     -
>                                                                     [clos7]-> 
>                                                                     <rec>))
>                                                       ]-> <rec>)
>                                                ]))
>                  (next: (Str
>                           -[clos6]-> Str
>                                        -[clos7]-> [
>                                                     Done Str,
>                                                     StdinLine
>                                                       (Str
>                                                         -[
>                                                            clos2
>                                                              (toNext: 
>                                                              (Str
>                                                                -[
>                                                                   clos3
>                                                                     (continue: 
>                                                                     <rec>)
>                                                                     (next: 
>                                                                     <rec>)
>                                                                   ]-> 
>                                                                <rec>))
>                                                            ]-> <rec>),
>                                                     StdoutLine Str
>                                                       (Str
>                                                         -[
>                                                            clos (toNext1: <rec>)
>                                                            ]-> <rec>)
>                                                     ]
>                                        -[clos1 (s1: Str)]-> [
>                                                               Done Str,
>                                                               StdinLine
>                                                                 (Str
>                                                                   -[
>                                                                     clos2
>                                                                     (toNext: 
>                                                                     (Str
>                                                                     -[
>                                                                     clos3
>                                                                     (continue: 
>                                                                     (Str
>                                                                     -[clos7]-> 
>                                                                     <rec>))
>                                                                     (next: 
>                                                                     <rec>)
>                                                                     ]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>),
>                                                               StdoutLine Str
>                                                                 (Str
>                                                                   -[
>                                                                     clos
>                                                                     (toNext1: 
>                                                                     (Str
>                                                                     -[clos7]-> 
>                                                                     <rec>))
>                                                                     ]-> 
>                                                                   <rec>)
>                                                               ]))
>                ]-> [
>                      Done Str,
>                      StdinLine
>                        (Str
>                          -[
>                             clos2
>                               (toNext: (Str
>                                          -[
>                                             clos3
>                                               (continue: (Str -[clos7]-> <rec>))
>                                               (next: (Str
>                                                        -[clos6]-> Str
>                                                                     -
>                                                                     [clos7]-> 
>                                                                     <rec>
>                                                                     -
>                                                                     [
>                                                                     clos1
>                                                                     (s1: Str)
>                                                                     ]-> 
>                                                                     <rec>))
>                                             ]-> <rec>))
>                             ]-> <rec>),
>                      StdoutLine Str
>                        (Str -[clos (toNext1: (Str -[clos7]-> <rec>))]-> <rec>)
>                      ] =
>   (await1 lineIn1) clos6
> let handle1: [
>                Done Str,
>                StdinLine (Str -<'3036>-> <rec>),
>                StdoutLine Str (Str -<'3034>-> <rec>)
>                ] -[handle1]-> Str = \op1 ->
>   when op1 is
>     | StdinLine f -> handle1 (f "hello")
>     | StdoutLine s3 f1 -> handle1 (f1 s3)
>     | Done x2 -> x2
>   end
> let clos7: Str
>              -[clos7]-> [
>                           Done Str,
>                           StdinLine (Str -<'3053>-> <rec>),
>                           StdoutLine Str (Str -<'3051>-> <rec>)
>                           ] = \x1 ->
>   Done x1
> run main_handler: Str =
>   let op: [
>             Done Str,
>             StdinLine
>               (Str
>                 -[
>                    clos2
>                      (toNext: (Str
>                                 -[
>                                    clos3 (continue: (Str -[clos7]-> <rec>))
>                                      (next: (Str
>                                               -[clos6]-> Str -[clos7]-> <rec>
>                                                            -[clos1 (s1: Str)]-> 
>                                                            <rec>))
>                                    ]-> <rec>))
>                    ]-> <rec>),
>             StdoutLine Str
>               (Str -[clos (toNext1: (Str -[clos7]-> <rec>))]-> <rec>)
>             ] =
>     main clos7
>   in
>   handle1 op

> cor-out +lambdamono -print
> fn lineOut2(s1: Str): [Clos1 {s1: Str}] =
>   Clos1 {s1: s1}
> fn clos15(toNext1: [Clos7], captures15: {s1: Str}): [
>                                                       Done Str,
>                                                       StdinLine
>                                                         [
>                                                           Clos2
>                                                             {
>                                                               toNext: 
>                                                                 [
>                                                                   Clos3
>                                                                     {
>                                                                     continue: 
>                                                                     [
>                                                                     Clos7
>                                                                     ],
>                                                                     next: 
>                                                                     [
>                                                                     Clos6
>                                                                     ]
>                                                                     }
>                                                                   ]
>                                                               }
>                                                           ],
>                                                       StdoutLine Str
>                                                         [
>                                                           Clos
>                                                             {toNext1: [Clos7]}
>                                                           ]
>                                                       ] =
>   let s1: Str = captures15.s1 in
>   StdoutLine s1 (Clos {toNext1: toNext1})
> fn clos14(result: Str, captures14: {continue: [Clos7], next: [Clos6]}): 
>   [
>     Done Str,
>     StdinLine [Clos2 {toNext: [Clos3 {continue: [Clos7], next: [Clos6]}]}],
>     StdoutLine Str [Clos {toNext1: [Clos7]}]
>     ] =
>   let next: [Clos6] = captures14.next in
>   let continue: [Clos7] = captures14.continue in
>   let inner: [Clos1 {s1: Str}] = when next is
>                                    | Clos6 -> clos8(result)
>                                  end in
>   when inner is
>     | Clos1 captures13 -> clos15(continue, captures13)
>   end
> fn clos13(x: Str, captures9: {toNext1: [Clos7]}): [
>                                                     Done Str,
>                                                     StdinLine
>                                                       [
>                                                         Clos2
>                                                           {
>                                                             toNext: [
>                                                                     Clos3
>                                                                     {
>                                                                     continue: 
>                                                                     [
>                                                                     Clos7
>                                                                     ],
>                                                                     next: 
>                                                                     [
>                                                                     Clos6
>                                                                     ]
>                                                                     }
>                                                                     ]
>                                                             }
>                                                         ],
>                                                     StdoutLine Str
>                                                       [Clos {toNext1: [Clos7]}]
>                                                     ] =
>   let toNext1: [Clos7] = captures9.toNext1 in
>   when toNext1 is
>     | Clos7 -> clos10(x)
>   end
> fn clos12(s: Str, captures11:
>                     {toNext: [Clos3 {continue: [Clos7], next: [Clos6]}]}): 
>   [
>     Done Str,
>     StdinLine [Clos2 {toNext: [Clos3 {continue: [Clos7], next: [Clos6]}]}],
>     StdoutLine Str [Clos {toNext1: [Clos7]}]
>     ] =
>   let toNext: [Clos3 {continue: [Clos7], next: [Clos6]}] = captures11.toNext in
>   when toNext is
>     | Clos3 captures10 -> clos14(s, captures10)
>   end
> fn handle2(op1:
>              [
>                Done Str,
>                StdinLine
>                  [Clos2 {toNext: [Clos3 {continue: [Clos7], next: [Clos6]}]}],
>                StdoutLine Str [Clos {toNext1: [Clos7]}]
>                ]): Str =
>   when op1 is
>     | StdinLine f ->
>       when Handle1 is
>         | Handle1 ->
>           handle2(when f is
>                     | Clos2 captures4 -> clos12("hello", captures4)
>                   end)
>       end
>     | StdoutLine s3 f1 ->
>       when Handle1 is
>         | Handle1 ->
>           handle2(when f1 is
>                     | Clos captures6 -> clos13(s3, captures6)
>                   end)
>       end
>     | Done x2 -> x2
>   end
> fn clos11(continue: [Clos7], captures17: {fromResult: [LineIn1], next: [Clos6]}): 
>   [
>     Done Str,
>     StdinLine [Clos2 {toNext: [Clos3 {continue: [Clos7], next: [Clos6]}]}],
>     StdoutLine Str [Clos {toNext1: [Clos7]}]
>     ] =
>   let next: [Clos6] = captures17.next in
>   let fromResult: [LineIn1] = captures17.fromResult in
>   when fromResult is
>     | LineIn1 -> lineIn2(Clos3 {continue: continue, next: next})
>   end
> fn clos10(x1: Str): [
>                       Done Str,
>                       StdinLine
>                         [
>                           Clos2
>                             {
>                               toNext: [Clos3 {continue: [Clos7], next: [Clos6]}]
>                               }
>                           ],
>                       StdoutLine Str [Clos {toNext1: [Clos7]}]
>                       ] =
>   Done x1
> fn clos9(next: [Clos6], captures18: {fromResult: [LineIn1]}): [
>                                                                 Clos4
>                                                                   {
>                                                                     fromResult: 
>                                                                     [
>                                                                     LineIn1
>                                                                     ],
>                                                                     next: 
>                                                                     [
>                                                                     Clos6
>                                                                     ]
>                                                                     }
>                                                                 ] =
>   let fromResult: [LineIn1] = captures18.fromResult in
>   Clos4 {fromResult: fromResult, next: next}
> fn clos8(s2: Str): [Clos1 {s1: Str}] =
>   when LineOut1 is
>     | LineOut1 -> lineOut2(s2)
>   end
> fn lineIn2(toNext: [Clos3 {continue: [Clos7], next: [Clos6]}]): [
>                                                                   Done Str,
>                                                                   StdinLine
>                                                                     [
>                                                                     Clos2
>                                                                     {
>                                                                     toNext: 
>                                                                     [
>                                                                     Clos3
>                                                                     {
>                                                                     continue: 
>                                                                     [
>                                                                     Clos7
>                                                                     ],
>                                                                     next: 
>                                                                     [
>                                                                     Clos6
>                                                                     ]
>                                                                     }
>                                                                     ]
>                                                                     }
>                                                                     ],
>                                                                   StdoutLine
>                                                                     Str
>                                                                     [
>                                                                     Clos
>                                                                     {
>                                                                     toNext1: 
>                                                                     [
>                                                                     Clos7
>                                                                     ]
>                                                                     }
>                                                                     ]
>                                                                   ] =
>   StdinLine (Clos2 {toNext: toNext})
> fn await2(fromResult: [LineIn1]): [Clos5 {fromResult: [LineIn1]}] =
>   Clos5 {fromResult: fromResult}
> let main: [Clos4 {fromResult: [LineIn1], next: [Clos6]}] =
>   when when Await1 is
>          | Await1 -> await2(LineIn1)
>        end is
>     | Clos5 captures1 -> clos9(Clos6, captures1)
>   end
> run main_handler: Str =
>   let op: [
>             Done Str,
>             StdinLine
>               [Clos2 {toNext: [Clos3 {continue: [Clos7], next: [Clos6]}]}],
>             StdoutLine Str [Clos {toNext1: [Clos7]}]
>             ] = when main is
>                   | Clos4 captures2 -> clos11(Clos7, captures2)
>                 end
>   in
>   when Handle1 is
>     | Handle1 -> handle2(op)
>   end

> cor-out +ir -print
> fn lineOut2(s1: str): [ `0 { { str } } ]
> {
>   let var: { str } = @make_struct{ s1 };
>   let struct: { { str } } = @make_struct{ var };
>   let var1: [ `0 { { str } } ] = @make_union<0, struct>;
>   return var1;
> }
> 
> fn clos15(toNext1: [ `0 {} ], captures15: { str }):
>   [
>      `0 { str },
>      `1 { [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ] },
>      `2 { str, [ `0 { { [ `0 {} ] } } ] }
>   ]
> {
>   let s1: str = @get_struct_field<captures15, 0>;
>   let var2: { [ `0 {} ] } = @make_struct{ toNext1 };
>   let struct2: { { [ `0 {} ] } } = @make_struct{ var2 };
>   let var3: [ `0 { { [ `0 {} ] } } ] = @make_union<0, struct2>;
>   let struct1: { str, [ `0 { { [ `0 {} ] } } ] } = @make_struct{ s1, var3 };
>   let var4:
>         [
>            `0 { str },
>            `1 { [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ] },
>            `2 { str, [ `0 { { [ `0 {} ] } } ] }
>         ]
>     = @make_union<2, struct1>;
>   return var4;
> }
> 
> fn clos14(result: str, captures14: { [ `0 {} ], [ `0 {} ] }):
>   [
>      `0 { str },
>      `1 { [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ] },
>      `2 { str, [ `0 { { [ `0 {} ] } } ] }
>   ]
> {
>   let next: [ `0 {} ] = @get_struct_field<captures14, 1>;
>   let continue: [ `0 {} ] = @get_struct_field<captures14, 0>;
>   let discr: int = @get_union_id<next>;
>   switch discr {
>   0 -> {
>     @call_direct(clos8, result)
>   }
>   } in join join;
>   let inner: [ `0 { { str } } ] = join;
>   let discr1: int = @get_union_id<inner>;
>   switch discr1 {
>   0 -> {
>     let payload: { { str } } = @get_union_struct<inner>;
>     let captures13: { str } = @get_struct_field<payload, 0>;
>     @call_direct(clos15, continue, captures13)
>   }
>   } in join join1;
>   return join1;
> }
> 
> fn clos13(x: str, captures9: { [ `0 {} ] }):
>   [
>      `0 { str },
>      `1 { [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ] },
>      `2 { str, [ `0 { { [ `0 {} ] } } ] }
>   ]
> {
>   let toNext1: [ `0 {} ] = @get_struct_field<captures9, 0>;
>   let discr2: int = @get_union_id<toNext1>;
>   switch discr2 {
>   0 -> {
>     @call_direct(clos10, x)
>   }
>   } in join join2;
>   return join2;
> }
> 
> fn clos12(s: str, captures11: { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] }):
>   [
>      `0 { str },
>      `1 { [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ] },
>      `2 { str, [ `0 { { [ `0 {} ] } } ] }
>   ]
> {
>   let toNext: [ `0 { { [ `0 {} ], [ `0 {} ] } } ]
>     = @get_struct_field<captures11, 0>;
>   let discr3: int = @get_union_id<toNext>;
>   switch discr3 {
>   0 -> {
>     let payload1: { { [ `0 {} ], [ `0 {} ] } } = @get_union_struct<toNext>;
>     let captures10: { [ `0 {} ], [ `0 {} ] } = @get_struct_field<payload1, 0>;
>     @call_direct(clos14, s, captures10)
>   }
>   } in join join3;
>   return join3;
> }
> 
> fn handle2(
>   op1:
>     [
>        `0 { str },
>        `1 { [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ] },
>        `2 { str, [ `0 { { [ `0 {} ] } } ] }
>     ]):
>   str
> {
>   let discr4: int = @get_union_id<op1>;
>   switch discr4 {
>   0 -> {
>     let payload6: { str } = @get_union_struct<op1>;
>     let x2: str = @get_struct_field<payload6, 0>;
>     x2
>   }
>   1 -> {
>     let payload2: { [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ] }
>       = @get_union_struct<op1>;
>     let f: [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ]
>       = @get_struct_field<payload2, 0>;
>     let struct3: {} = @make_struct{};
>     let var5: [ `0 {} ] = @make_union<0, struct3>;
>     let discr5: int = @get_union_id<var5>;
>     switch discr5 {
>     0 -> {
>       let discr6: int = @get_union_id<f>;
>       switch discr6 {
>       0 -> {
>         let payload3: { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } }
>           = @get_union_struct<f>;
>         let captures4: { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] }
>           = @get_struct_field<payload3, 0>;
>         let var6: str = "hello";
>         @call_direct(clos12, var6, captures4)
>       }
>       } in join join4;
>       @call_direct(handle2, join4)
>     }
>     } in join join5;
>     join5
>   }
>   2 -> {
>     let payload4: { str, [ `0 { { [ `0 {} ] } } ] } = @get_union_struct<op1>;
>     let s3: str = @get_struct_field<payload4, 0>;
>     let f1: [ `0 { { [ `0 {} ] } } ] = @get_struct_field<payload4, 1>;
>     let struct4: {} = @make_struct{};
>     let var7: [ `0 {} ] = @make_union<0, struct4>;
>     let discr7: int = @get_union_id<var7>;
>     switch discr7 {
>     0 -> {
>       let discr8: int = @get_union_id<f1>;
>       switch discr8 {
>       0 -> {
>         let payload5: { { [ `0 {} ] } } = @get_union_struct<f1>;
>         let captures6: { [ `0 {} ] } = @get_struct_field<payload5, 0>;
>         @call_direct(clos13, s3, captures6)
>       }
>       } in join join6;
>       @call_direct(handle2, join6)
>     }
>     } in join join7;
>     join7
>   }
>   } in join join8;
>   return join8;
> }
> 
> fn clos11(continue: [ `0 {} ], captures17: { [ `0 {} ], [ `0 {} ] }):
>   [
>      `0 { str },
>      `1 { [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ] },
>      `2 { str, [ `0 { { [ `0 {} ] } } ] }
>   ]
> {
>   let next: [ `0 {} ] = @get_struct_field<captures17, 1>;
>   let fromResult: [ `0 {} ] = @get_struct_field<captures17, 0>;
>   let discr9: int = @get_union_id<fromResult>;
>   switch discr9 {
>   0 -> {
>     let var8: { [ `0 {} ], [ `0 {} ] } = @make_struct{ continue, next };
>     let struct5: { { [ `0 {} ], [ `0 {} ] } } = @make_struct{ var8 };
>     let var9: [ `0 { { [ `0 {} ], [ `0 {} ] } } ] = @make_union<0, struct5>;
>     @call_direct(lineIn2, var9)
>   }
>   } in join join9;
>   return join9;
> }
> 
> fn clos10(x1: str):
>   [
>      `0 { str },
>      `1 { [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ] },
>      `2 { str, [ `0 { { [ `0 {} ] } } ] }
>   ]
> {
>   let struct6: { str } = @make_struct{ x1 };
>   let var10:
>         [
>            `0 { str },
>            `1 { [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ] },
>            `2 { str, [ `0 { { [ `0 {} ] } } ] }
>         ]
>     = @make_union<0, struct6>;
>   return var10;
> }
> 
> fn clos9(next: [ `0 {} ], captures18: { [ `0 {} ] }):
>   [ `0 { { [ `0 {} ], [ `0 {} ] } } ]
> {
>   let fromResult: [ `0 {} ] = @get_struct_field<captures18, 0>;
>   let var11: { [ `0 {} ], [ `0 {} ] } = @make_struct{ fromResult, next };
>   let struct7: { { [ `0 {} ], [ `0 {} ] } } = @make_struct{ var11 };
>   let var12: [ `0 { { [ `0 {} ], [ `0 {} ] } } ] = @make_union<0, struct7>;
>   return var12;
> }
> 
> fn clos8(s2: str): [ `0 { { str } } ]
> {
>   let struct8: {} = @make_struct{};
>   let var13: [ `0 {} ] = @make_union<0, struct8>;
>   let discr10: int = @get_union_id<var13>;
>   switch discr10 {
>   0 -> {
>     @call_direct(lineOut2, s2)
>   }
>   } in join join10;
>   return join10;
> }
> 
> fn lineIn2(toNext: [ `0 { { [ `0 {} ], [ `0 {} ] } } ]):
>   [
>      `0 { str },
>      `1 { [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ] },
>      `2 { str, [ `0 { { [ `0 {} ] } } ] }
>   ]
> {
>   let var14: { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } = @make_struct{ toNext };
>   let struct10: { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } }
>     = @make_struct{ var14 };
>   let var15: [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ]
>     = @make_union<0, struct10>;
>   let struct9: { [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ] }
>     = @make_struct{ var15 };
>   let var16:
>         [
>            `0 { str },
>            `1 { [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ] },
>            `2 { str, [ `0 { { [ `0 {} ] } } ] }
>         ]
>     = @make_union<1, struct9>;
>   return var16;
> }
> 
> fn await2(fromResult: [ `0 {} ]): [ `0 { { [ `0 {} ] } } ]
> {
>   let var17: { [ `0 {} ] } = @make_struct{ fromResult };
>   let struct11: { { [ `0 {} ] } } = @make_struct{ var17 };
>   let var18: [ `0 { { [ `0 {} ] } } ] = @make_union<0, struct11>;
>   return var18;
> }
> 
> fn main_thunk(): [ `0 { { [ `0 {} ], [ `0 {} ] } } ]
> {
>   let struct12: {} = @make_struct{};
>   let var19: [ `0 {} ] = @make_union<0, struct12>;
>   let discr11: int = @get_union_id<var19>;
>   switch discr11 {
>   0 -> {
>     let struct13: {} = @make_struct{};
>     let var20: [ `0 {} ] = @make_union<0, struct13>;
>     @call_direct(await2, var20)
>   }
>   } in join join11;
>   let discr12: int = @get_union_id<join11>;
>   switch discr12 {
>   0 -> {
>     let payload7: { { [ `0 {} ] } } = @get_union_struct<join11>;
>     let captures1: { [ `0 {} ] } = @get_struct_field<payload7, 0>;
>     let struct14: {} = @make_struct{};
>     let var21: [ `0 {} ] = @make_union<0, struct14>;
>     @call_direct(clos9, var21, captures1)
>   }
>   } in join join12;
>   return join12;
> }
> 
> global main: [ `0 { { [ `0 {} ], [ `0 {} ] } } ] = @call_direct(main_thunk);
> 
> fn main_handler_thunk(): str
> {
>   let discr13: int = @get_union_id<main>;
>   switch discr13 {
>   0 -> {
>     let payload8: { { [ `0 {} ], [ `0 {} ] } } = @get_union_struct<main>;
>     let captures2: { [ `0 {} ], [ `0 {} ] } = @get_struct_field<payload8, 0>;
>     let struct15: {} = @make_struct{};
>     let var22: [ `0 {} ] = @make_union<0, struct15>;
>     @call_direct(clos11, var22, captures2)
>   }
>   } in join join13;
>   let op:
>         [
>            `0 { str },
>            `1 { [ `0 { { [ `0 { { [ `0 {} ], [ `0 {} ] } } ] } } ] },
>            `2 { str, [ `0 { { [ `0 {} ] } } ] }
>         ]
>     = join13;
>   let struct16: {} = @make_struct{};
>   let var23: [ `0 {} ] = @make_union<0, struct16>;
>   let discr14: int = @get_union_id<var23>;
>   switch discr14 {
>   0 -> {
>     @call_direct(handle2, op)
>   }
>   } in join join14;
>   return join14;
> }
> 
> entry main_handler: str = @call_direct(main_handler_thunk);

> cor-out +eval -print
> main_handler = [104 101 108 108 111]
>              > "hello"