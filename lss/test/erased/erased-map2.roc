# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print

Map2 a : [
  Map2 {
    force_a: {} -> Erased,
    force_b: {} -> Erased,
    map2: Erased -> Erased -> a,
  }
]

sig build_map2: a -> b -> (a -> b -> c) -> Map2 c
let build_map2 = \a -> \b -> \f ->
  Map2 {
    force_a: \r -> ~erase a,
    force_b: \r -> ~erase b,
    map2: \a -> \b -> f (~unerase a) (~unerase b),
  }
;;

sig eval_map2: Map2 a -> a
let eval_map2 = \m ->
  when m is
  | Map2 m1 ->
    let a = m1.force_a {} in
    let b = m1.force_b {} in
    m1.map2 a b
  end
;;

run main1 =
    let mapper = build_map2 1 2 (\x -> \y -> x + y) in
    eval_map2 mapper
;;

run main2 =
    let a = \x -> 1 in
    let b = \x -> 2 in
    let f = \x -> \y -> (x {}) + (y {}) in
    let mapper = build_map2 a b f in
    eval_map2 mapper
;;

> cor-out +monotype -print
> let eval_map22: [
>                   Map2
>                     {
>                       force_a: {} -> Erased,
>                       force_b: {} -> Erased,
>                       map2: Erased -> Erased -> Int
>                       }
>                   ] -> Int = \m ->
>   when m is
>     | Map2 m1 ->
>       let a2: Erased =
>         m1 .force_a {} in
>       let b2: Erased =
>         m1 .force_b {} in
>       (m1 .map2 a2) b2
>   end
> let build_map22: ([] -> Int)
>                    -> ([] -> Int)
>                         -> (({} -> Int) -> ({} -> Int) -> Int)
>                              -> [
>                                   Map2
>                                     {
>                                       force_a: {} -> Erased,
>                                       force_b: {} -> Erased,
>                                       map2: Erased -> Erased -> Int
>                                       }
>                                   ] = \a ->
>   \b ->
>     \f ->
>       (Map2
>          {force_a:
>            \r1 -> ~erase a,
>            force_b:
>            \r -> ~erase b,
>            map2:
>            \a1 -> \b1 -> ((f ~unerase a1) ~unerase b1)})
> let eval_map21: [
>                   Map2
>                     {
>                       force_a: {} -> Erased,
>                       force_b: {} -> Erased,
>                       map2: Erased -> Erased -> Int
>                       }
>                   ] -> Int = \m ->
>   when m is
>     | Map2 m1 ->
>       let a2: Erased =
>         m1 .force_a {} in
>       let b2: Erased =
>         m1 .force_b {} in
>       (m1 .map2 a2) b2
>   end
> let build_map21: Int
>                    -> Int
>                         -> (Int -> Int -> Int)
>                              -> [
>                                   Map2
>                                     {
>                                       force_a: {} -> Erased,
>                                       force_b: {} -> Erased,
>                                       map2: Erased -> Erased -> Int
>                                       }
>                                   ] = \a ->
>   \b ->
>     \f ->
>       (Map2
>          {force_a:
>            \r1 -> ~erase a,
>            force_b:
>            \r -> ~erase b,
>            map2:
>            \a1 -> \b1 -> ((f ~unerase a1) ~unerase b1)})
> run main1: Int =
>   let mapper: [
>                 Map2
>                   {
>                     force_a: {} -> Erased,
>                     force_b: {} -> Erased,
>                     map2: Erased -> Erased -> Int
>                     }
>                 ] =
>     ((build_map21 1) 2) \x -> \y -> ~add x y
>   in
>   eval_map21 mapper
> run main2: Int =
>   let a3: [] -> Int = \x1 ->
>     1 in
>   let b3: [] -> Int = \x2 ->
>     2 in
>   let f1: ({} -> Int) -> ({} -> Int) -> Int = \x3 ->
>     \y1 -> ~add (x3 {}) (y1 {})
>   in
>   let mapper1: [
>                  Map2
>                    {
>                      force_a: {} -> Erased,
>                      force_b: {} -> Erased,
>                      map2: Erased -> Erased -> Int
>                      }
>                  ] =
>     ((build_map22 a3) b3) f1
>   in
>   eval_map22 mapper1

> cor-out +monotype_lifted -print
> let eval_map22: [
>                   Map2
>                     {
>                       force_a: {} -> Erased,
>                       force_b: {} -> Erased,
>                       map2: Erased -> Erased -> Int
>                       }
>                   ] -> Int = \m ->
>   when m is
>     | Map2 m1 ->
>       let a2: Erased =
>         m1.force_a {} in
>       let b2: Erased =
>         m1.force_b {} in
>       (m1.map2 a2) b2
>   end
> let clos(a: [] -> Int): {} -> Erased = \r1 ->
>   ~erase a
> let clos1(b: [] -> Int): {} -> Erased = \r ->
>   ~erase b
> let clos2(a1: Erased f: ({} -> Int) -> ({} -> Int) -> Int): Erased -> Int = \b1 ->
>   (f ~unerase a1) ~unerase b1
> let clos3(f: ({} -> Int) -> ({} -> Int) -> Int): Erased -> Erased -> Int = \a1 ->
>   clos2
> let clos4(a: [] -> Int b: [] -> Int): (({} -> Int) -> ({} -> Int) -> Int)
>                                         -> [
>                                              Map2
>                                                {
>                                                  force_a: {} -> Erased,
>                                                  force_b: {} -> Erased,
>                                                  map2: Erased -> Erased -> Int
>                                                  }
>                                              ] = \f ->
>   Map2 {force_a: clos, force_b: clos1, map2: clos3}
> let clos5(a: [] -> Int): ([] -> Int)
>                            -> (({} -> Int) -> ({} -> Int) -> Int)
>                                 -> [
>                                      Map2
>                                        {
>                                          force_a: {} -> Erased,
>                                          force_b: {} -> Erased,
>                                          map2: Erased -> Erased -> Int
>                                          }
>                                      ] = \b ->
>   clos4
> let build_map22: ([] -> Int)
>                    -> ([] -> Int)
>                         -> (({} -> Int) -> ({} -> Int) -> Int)
>                              -> [
>                                   Map2
>                                     {
>                                       force_a: {} -> Erased,
>                                       force_b: {} -> Erased,
>                                       map2: Erased -> Erased -> Int
>                                       }
>                                   ] = \a ->
>   clos5
> let eval_map21: [
>                   Map2
>                     {
>                       force_a: {} -> Erased,
>                       force_b: {} -> Erased,
>                       map2: Erased -> Erased -> Int
>                       }
>                   ] -> Int = \m ->
>   when m is
>     | Map2 m1 ->
>       let a2: Erased =
>         m1.force_a {} in
>       let b2: Erased =
>         m1.force_b {} in
>       (m1.map2 a2) b2
>   end
> let clos6(a: Int): {} -> Erased = \r1 ->
>   ~erase a
> let clos7(b: Int): {} -> Erased = \r ->
>   ~erase b
> let clos8(a1: Erased f: Int -> Int -> Int): Erased -> Int = \b1 ->
>   (f ~unerase a1) ~unerase b1
> let clos9(f: Int -> Int -> Int): Erased -> Erased -> Int = \a1 ->
>   clos8
> let clos10(a: Int b: Int): (Int -> Int -> Int)
>                              -> [
>                                   Map2
>                                     {
>                                       force_a: {} -> Erased,
>                                       force_b: {} -> Erased,
>                                       map2: Erased -> Erased -> Int
>                                       }
>                                   ] = \f ->
>   Map2 {force_a: clos6, force_b: clos7, map2: clos9}
> let clos11(a: Int): Int
>                       -> (Int -> Int -> Int)
>                            -> [
>                                 Map2
>                                   {
>                                     force_a: {} -> Erased,
>                                     force_b: {} -> Erased,
>                                     map2: Erased -> Erased -> Int
>                                     }
>                                 ] = \b ->
>   clos10
> let build_map21: Int
>                    -> Int
>                         -> (Int -> Int -> Int)
>                              -> [
>                                   Map2
>                                     {
>                                       force_a: {} -> Erased,
>                                       force_b: {} -> Erased,
>                                       map2: Erased -> Erased -> Int
>                                       }
>                                   ] = \a ->
>   clos11
> let clos12(x: Int): Int -> Int = \y ->
>   ~add x y
> let clos13: Int -> Int -> Int = \x ->
>   clos12
> run main1: Int =
>   let mapper: [
>                 Map2
>                   {
>                     force_a: {} -> Erased,
>                     force_b: {} -> Erased,
>                     map2: Erased -> Erased -> Int
>                     }
>                 ] =
>     ((build_map21 1) 2) clos13
>   in
>   eval_map21 mapper
> let clos14(x3: {} -> Int): ({} -> Int) -> Int = \y1 ->
>   ~add (x3 {}) (y1 {})
> let f2: ({} -> Int) -> ({} -> Int) -> Int = \x3 ->
>   clos14
> let b4: [] -> Int = \x2 ->
>   2
> let a4: [] -> Int = \x1 ->
>   1
> run main2: Int =
>   let mapper1: [
>                  Map2
>                    {
>                      force_a: {} -> Erased,
>                      force_b: {} -> Erased,
>                      map2: Erased -> Erased -> Int
>                      }
>                  ] =
>     ((build_map22 a4) b4) f2
>   in
>   eval_map22 mapper1

> cor-out +lambdasolved -print
> let clos14(x3: {} -<'2304>-> Int): {} -<'2326>-> Int
>                                      -[clos14 (x3: ({} -<'2304>-> Int))]-> Int = \y1 ->
>   ~add (x3 {}) (y1 {})
> let clos2(a1: Erased, f:
>                         {} -Erased-> Int
>                           -<'2356>-> {} -Erased-> Int -<'2376>-> Int): 
>   Erased
>     -[
>        clos2 (a1: Erased)
>          (f: ({} -Erased-> Int -<'2356>-> {} -Erased-> Int -<'2376>-> Int))
>        ]-> Int = \b1 ->
>   (f ~unerase a1) ~unerase b1
> let clos3(f: {} -<'2461>-> Int -<'2464>-> {} -<'2466>-> Int -<'2468>-> Int): 
>   Erased
>     -[
>        clos3
>          (f: ({} -<'2461>-> Int -<'2464>-> {} -<'2466>-> Int -<'2468>-> Int))
>        ]-> Erased
>              -[
>                 clos2 (a1: Erased)
>                   (f: ({} -<'2461>-> Int
>                         -<'2464>-> {} -<'2466>-> Int -<'2468>-> Int))
>                 ]-> Int = \a1 ->
>   clos2
> let clos7(b: Int): {} -[clos7 (b: Int)]-> Erased = \r ->
>   ~erase b
> let b4: [] -[b4]-> Int = \x2 ->
>   2
> let clos8(a1: Erased, f: Int -<'2524>-> Int -<'2538>-> Int): Erased
>                                                                -[
>                                                                   clos8
>                                                                     (a1: Erased)
>                                                                     (f: 
>                                                                     (Int
>                                                                     -<'2524>-> 
>                                                                     Int
>                                                                     -<'2538>-> Int))
>                                                                   ]-> Int = \b1 ->
>   (f ~unerase a1) ~unerase b1
> let clos9(f: Int -<'2596>-> Int -<'2597>-> Int): Erased
>                                                    -[
>                                                       clos9
>                                                         (f: (Int
>                                                               -<'2596>-> 
>                                                               Int
>                                                                 -<'2597>-> Int))
>                                                       ]-> Erased
>                                                             -[
>                                                                clos8
>                                                                  (a1: Erased)
>                                                                  (f: 
>                                                                  (Int
>                                                                    -<'2596>-> 
>                                                                    Int
>                                                                     -<'2597>-> Int))
>                                                                ]-> Int = \a1 ->
>   clos8
> let clos6(a: Int): {} -[clos6 (a: Int)]-> Erased = \r1 ->
>   ~erase a
> let clos10(a: Int, b: Int): Int -<'2759>-> Int -<'2760>-> Int
>                               -[clos10 (a: Int) (b: Int)]-> [
>                                                               Map2
>                                                                 {
>                                                                   force_a: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos6
>                                                                     (a: Int)
>                                                                     ]-> Erased),
>                                                                   force_b: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos7
>                                                                     (b: Int)
>                                                                     ]-> Erased),
>                                                                   map2: 
>                                                                     (Erased
>                                                                     -[
>                                                                     clos9
>                                                                     (f: 
>                                                                     (Int
>                                                                     -<'2759>-> 
>                                                                     Int
>                                                                     -<'2760>-> Int))
>                                                                     ]-> 
>                                                                     Erased
>                                                                     -[
>                                                                     clos8
>                                                                     (a1: Erased)
>                                                                     (f: 
>                                                                     (Int
>                                                                     -<'2759>-> 
>                                                                     Int
>                                                                     -<'2760>-> Int))
>                                                                     ]-> Int)
>                                                                   }
>                                                               ] = \f ->
>   Map2 {force_a: clos6, force_b: clos7, map2: clos9}
> let clos11(a: Int): Int
>                       -[clos11 (a: Int)]-> Int -<'2909>-> Int -<'2910>-> Int
>                                              -[clos10 (a: Int) (b: Int)]-> 
>                                              [
>                                                Map2
>                                                  {
>                                                    force_a: ({}
>                                                               -[clos6 (a: Int)]-> Erased),
>                                                    force_b: ({}
>                                                               -[clos7 (b: Int)]-> Erased),
>                                                    map2: (Erased
>                                                            -[
>                                                               clos9
>                                                                 (f: (Int
>                                                                     -<'2909>-> 
>                                                                     Int
>                                                                     -<'2910>-> Int))
>                                                               ]-> Erased
>                                                                     -
>                                                                     [
>                                                                     clos8
>                                                                     (a1: Erased)
>                                                                     (f: 
>                                                                     (Int
>                                                                     -<'2909>-> 
>                                                                     Int
>                                                                     -<'2910>-> Int))
>                                                                     ]-> Int)
>                                                    }
>                                                ] = \b ->
>   clos10
> let clos(a: [] -Erased-> Int): {} -[clos (a: ([] -Erased-> Int))]-> Erased = \r1 ->
>   ~erase a
> let clos1(b: [] -Erased-> Int): {} -[clos1 (b: ([] -Erased-> Int))]-> Erased = \r ->
>   ~erase b
> let f2: {} -<'3018>-> Int
>           -[f2]-> {} -<'3023>-> Int -[clos14 (x3: ({} -<'3018>-> Int))]-> Int = \x3 ->
>   clos14
> let eval_map22: [
>                   Map2
>                     {
>                       force_a: ({} -<'3195>-> Erased),
>                       force_b: ({} -<'3192>-> Erased),
>                       map2: (Erased -<'3185>-> Erased -<'3187>-> Int)
>                       }
>                   ] -[eval_map22]-> Int = \m ->
>   when m is
>     | Map2 m1 ->
>       let a2: Erased =
>         m1.force_a {} in
>       let b2: Erased =
>         m1.force_b {} in
>       (m1.map2 a2) b2
>   end
> let clos4(a: [] -<'3229>-> Int, b: [] -<'3263>-> Int): {} -<'3435>-> Int
>                                                          -<'3438>-> {}
>                                                                     -<'3440>-> Int
>                                                                     -<'3442>-> Int
>                                                          -[
>                                                             clos4
>                                                               (a: ([]
>                                                                     -
>                                                                     <'3229>-> Int))
>                                                               (b: ([]
>                                                                     -
>                                                                     <'3263>-> Int))
>                                                             ]-> [
>                                                                   Map2
>                                                                     {
>                                                                     force_a: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos
>                                                                     (a: 
>                                                                     ([]
>                                                                     -<'3229>-> Int))
>                                                                     ]-> Erased),
>                                                                     force_b: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos1
>                                                                     (b: 
>                                                                     ([]
>                                                                     -<'3263>-> Int))
>                                                                     ]-> Erased),
>                                                                     map2: 
>                                                                     (Erased
>                                                                     -[
>                                                                     clos3
>                                                                     (f: 
>                                                                     ({}
>                                                                     -<'3435>-> Int
>                                                                     -<'3438>-> 
>                                                                     {}
>                                                                     -<'3440>-> Int
>                                                                     -<'3442>-> Int))
>                                                                     ]-> 
>                                                                     Erased
>                                                                     -[
>                                                                     clos2
>                                                                     (a1: Erased)
>                                                                     (f: 
>                                                                     ({}
>                                                                     -<'3435>-> Int
>                                                                     -<'3438>-> 
>                                                                     {}
>                                                                     -<'3440>-> Int
>                                                                     -<'3442>-> Int))
>                                                                     ]-> Int)
>                                                                     }
>                                                                   ] = \f ->
>   Map2 {force_a: clos, force_b: clos1, map2: clos3}
> let clos5(a: [] -<'3612>-> Int): [] -<'3651>-> Int
>                                    -[clos5 (a: ([] -<'3612>-> Int))]-> 
>                                    {} -<'3656>-> Int
>                                      -<'3659>-> {} -<'3661>-> Int
>                                                   -<'3663>-> Int
>                                      -[
>                                         clos4 (a: ([] -<'3612>-> Int))
>                                           (b: ([] -<'3651>-> Int))
>                                         ]-> [
>                                               Map2
>                                                 {
>                                                   force_a: ({}
>                                                              -[
>                                                                 clos
>                                                                   (a: 
>                                                                   ([]
>                                                                     -
>                                                                     <'3612>-> Int))
>                                                                 ]-> Erased),
>                                                   force_b: ({}
>                                                              -[
>                                                                 clos1
>                                                                   (b: 
>                                                                   ([]
>                                                                     -
>                                                                     <'3651>-> Int))
>                                                                 ]-> Erased),
>                                                   map2: (Erased
>                                                           -[
>                                                              clos3
>                                                                (f: ({}
>                                                                     -<'3656>-> Int
>                                                                     -<'3659>-> 
>                                                                     {}
>                                                                     -<'3661>-> Int
>                                                                     -<'3663>-> Int))
>                                                              ]-> Erased
>                                                                    -[
>                                                                     clos2
>                                                                     (a1: Erased)
>                                                                     (f: 
>                                                                     ({}
>                                                                     -<'3656>-> Int
>                                                                     -<'3659>-> 
>                                                                     {}
>                                                                     -<'3661>-> Int
>                                                                     -<'3663>-> Int))
>                                                                     ]-> Int)
>                                                   }
>                                               ] = \b ->
>   clos4
> let build_map22: [] -<'3894>-> Int
>                    -[build_map22]-> [] -<'3899>-> Int
>                                       -[clos5 (a: ([] -<'3894>-> Int))]-> 
>                                       {} -<'3904>-> Int
>                                         -<'3907>-> {} -<'3909>-> Int
>                                                      -<'3911>-> Int
>                                         -[
>                                            clos4 (a: ([] -<'3894>-> Int))
>                                              (b: ([] -<'3899>-> Int))
>                                            ]-> [
>                                                  Map2
>                                                    {
>                                                      force_a: ({}
>                                                                 -[
>                                                                    clos
>                                                                     (a: 
>                                                                     ([]
>                                                                     -<'3894>-> Int))
>                                                                    ]-> Erased),
>                                                      force_b: ({}
>                                                                 -[
>                                                                    clos1
>                                                                     (b: 
>                                                                     ([]
>                                                                     -<'3899>-> Int))
>                                                                    ]-> Erased),
>                                                      map2: (Erased
>                                                              -[
>                                                                 clos3
>                                                                   (f: 
>                                                                   ({}
>                                                                     -<'3904>-> Int
>                                                                     -
>                                                                     <'3907>-> 
>                                                                     {}
>                                                                     -<'3909>-> Int
>                                                                     -<'3911>-> Int))
>                                                                 ]-> Erased
>                                                                     -[
>                                                                     clos2
>                                                                     (a1: Erased)
>                                                                     (f: 
>                                                                     ({}
>                                                                     -<'3904>-> Int
>                                                                     -<'3907>-> 
>                                                                     {}
>                                                                     -<'3909>-> Int
>                                                                     -<'3911>-> Int))
>                                                                     ]-> Int)
>                                                      }
>                                                  ] = \a ->
>   clos5
> let a4: [] -[a4]-> Int = \x1 ->
>   1
> run main2: Int =
>   let mapper1: [
>                  Map2
>                    {
>                      force_a: ({} -[clos (a: ([] -[a4]-> Int))]-> Erased),
>                      force_b: ({} -[clos1 (b: ([] -[b4]-> Int))]-> Erased),
>                      map2: (Erased
>                              -[
>                                 clos3
>                                   (f: ({} -<?4300>-> Int
>                                         -[f2]-> {} -<?4305>-> Int
>                                                   -[
>                                                      clos14
>                                                        (x3: ({} -<?4300>-> Int))
>                                                      ]-> Int))
>                                 ]-> Erased
>                                       -[
>                                          clos2 (a1: Erased)
>                                            (f: ({} -<?4300>-> Int
>                                                  -[f2]-> {} -<?4305>-> Int
>                                                            -[
>                                                               clos14
>                                                                 (x3: 
>                                                                 ({}
>                                                                   -<?4300>-> Int))
>                                                               ]-> Int))
>                                          ]-> Int)
>                      }
>                  ] =
>     ((build_map22 a4) b4) f2
>   in
>   eval_map22 mapper1
> let build_map21: Int
>                    -[build_map21]-> Int
>                                       -[clos11 (a: Int)]-> Int
>                                                              -<'4601>-> 
>                                                              Int -<'4602>-> Int
>                                                              -[
>                                                                 clos10 (a: Int)
>                                                                   (b: Int)
>                                                                 ]-> [
>                                                                     Map2
>                                                                     {
>                                                                     force_a: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos6
>                                                                     (a: Int)
>                                                                     ]-> Erased),
>                                                                     force_b: 
>                                                                     ({}
>                                                                     -[
>                                                                     clos7
>                                                                     (b: Int)
>                                                                     ]-> Erased),
>                                                                     map2: 
>                                                                     (Erased
>                                                                     -[
>                                                                     clos9
>                                                                     (f: 
>                                                                     (Int
>                                                                     -<'4601>-> 
>                                                                     Int
>                                                                     -<'4602>-> Int))
>                                                                     ]-> 
>                                                                     Erased
>                                                                     -[
>                                                                     clos8
>                                                                     (a1: Erased)
>                                                                     (f: 
>                                                                     (Int
>                                                                     -<'4601>-> 
>                                                                     Int
>                                                                     -<'4602>-> Int))
>                                                                     ]-> Int)
>                                                                     }
>                                                                     ] = \a ->
>   clos11
> let eval_map21: [
>                   Map2
>                     {
>                       force_a: ({} -<'4790>-> Erased),
>                       force_b: ({} -<'4787>-> Erased),
>                       map2: (Erased -<'4780>-> Erased -<'4782>-> Int)
>                       }
>                   ] -[eval_map21]-> Int = \m ->
>   when m is
>     | Map2 m1 ->
>       let a2: Erased =
>         m1.force_a {} in
>       let b2: Erased =
>         m1.force_b {} in
>       (m1.map2 a2) b2
>   end
> let clos12(x: Int): Int -[clos12 (x: Int)]-> Int = \y ->
>   ~add x y
> let clos13: Int -[clos13]-> Int -[clos12 (x: Int)]-> Int = \x ->
>   clos12
> run main1: Int =
>   let mapper: [
>                 Map2
>                   {
>                     force_a: ({} -[clos6 (a: Int)]-> Erased),
>                     force_b: ({} -[clos7 (b: Int)]-> Erased),
>                     map2: (Erased
>                             -[
>                                clos9
>                                  (f: (Int
>                                        -[clos13]-> Int -[clos12 (x: Int)]-> Int))
>                                ]-> Erased
>                                      -[
>                                         clos8 (a1: Erased)
>                                           (f: (Int
>                                                 -[clos13]-> Int
>                                                               -[
>                                                                  clos12 (x: Int)
>                                                                  ]-> Int))
>                                         ]-> Int)
>                     }
>                 ] =
>     ((build_map21 1) 2) clos13
>   in
>   eval_map21 mapper

> cor-out +lambdamono -print
> fn clos60(y1: Erased, captures74: {x3: Erased}): Int =
>   let x3: Erased = captures74.x3 in
>   ~add (x3({})), (y1({}))
> fn clos59(y1: Erased, captures73: {x3: Erased}): Int =
>   let x3: Erased = captures73.x3 in
>   ~add (x3({})), (y1({}))
> fn f8(x3: Erased): [Clos14 {x3: Erased}] =
>   Clos14 {x3: x3}
> fn clos58(b1: Erased, captures72: {a1: Erased, f: [F2]}): Int =
>   let f: [F2] = captures72.f in
>   let a1: Erased = captures72.a1 in
>   when when f is
>          | F2 -> f8(~unerase a1)
>        end is
>     | Clos14 captures71 -> clos59(~unerase b1, captures71)
>   end
> fn clos57(a1: Erased, captures69: {f: [F2]}): [Clos2 {a1: Erased, f: [F2]}] =
>   let f: [F2] = captures69.f in
>   Clos2 {a1: a1, f: f}
> fn clos56(r: {}, captures75: {b: Erased}): Erased =
>   let b: Erased = captures75.b in
>   ~erase b
> fn clos55(r1: {}, captures76: {a: Erased}): Erased =
>   let a: Erased = captures76.a in
>   ~erase a
> fn clos54(f: [F2], captures68: {a: Erased, b: Erased}): [
>                                                           Map2
>                                                             {
>                                                               force_a: 
>                                                                 [
>                                                                   Clos
>                                                                     {a: Erased}
>                                                                   ],
>                                                               force_b: 
>                                                                 [
>                                                                   Clos1
>                                                                     {b: Erased}
>                                                                   ],
>                                                               map2: [
>                                                                     Clos3
>                                                                     {f: [F2]}
>                                                                     ]
>                                                               }
>                                                           ] =
>   let b: Erased = captures68.b in
>   let a: Erased = captures68.a in
>   Map2 {force_a: Clos {a: a}, force_b: Clos1 {b: b}, map2: Clos3 {f: f}}
> fn clos53(b: Erased, captures67: {a: Erased}): [Clos4 {a: Erased, b: Erased}] =
>   let a: Erased = captures67.a in
>   Clos4 {a: a, b: b}
> fn clos52(y1: Erased, captures64: {x3: Erased}): Int =
>   let x3: Erased = captures64.x3 in
>   ~add (x3({})), (y1({}))
> fn clos51(y1: Erased, captures63: {x3: Erased}): Int =
>   let x3: Erased = captures63.x3 in
>   ~add (x3({})), (y1({}))
> fn f7(x3: Erased): [Clos14 {x3: Erased}] =
>   Clos14 {x3: x3}
> fn clos50(b1: Erased, captures62: {a1: Erased, f: [F2]}): Int =
>   let f: [F2] = captures62.f in
>   let a1: Erased = captures62.a1 in
>   when when f is
>          | F2 -> f7(~unerase a1)
>        end is
>     | Clos14 captures61 -> clos51(~unerase b1, captures61)
>   end
> fn clos49(a1: Erased, captures59: {f: [F2]}): [Clos2 {a1: Erased, f: [F2]}] =
>   let f: [F2] = captures59.f in
>   Clos2 {a1: a1, f: f}
> fn clos48(r: {}, captures65: {b: Erased}): Erased =
>   let b: Erased = captures65.b in
>   ~erase b
> fn clos47(r1: {}, captures66: {a: Erased}): Erased =
>   let a: Erased = captures66.a in
>   ~erase a
> fn clos46(f: [F2], captures58: {a: Erased, b: Erased}): [
>                                                           Map2
>                                                             {
>                                                               force_a: 
>                                                                 [
>                                                                   Clos
>                                                                     {a: Erased}
>                                                                   ],
>                                                               force_b: 
>                                                                 [
>                                                                   Clos1
>                                                                     {b: Erased}
>                                                                   ],
>                                                               map2: [
>                                                                     Clos3
>                                                                     {f: [F2]}
>                                                                     ]
>                                                               }
>                                                           ] =
>   let b: Erased = captures58.b in
>   let a: Erased = captures58.a in
>   Map2 {force_a: Clos {a: a}, force_b: Clos1 {b: b}, map2: Clos3 {f: f}}
> fn clos45(y1: Erased, captures56: {x3: Erased}): Int =
>   let x3: Erased = captures56.x3 in
>   ~add (x3({})), (y1({}))
> fn clos44(y1: Erased, captures53: {x3: Erased}): Int =
>   let x3: Erased = captures53.x3 in
>   ~add (x3({})), (y1({}))
> fn clos43(y1: Erased, captures52: {x3: Erased}): Int =
>   let x3: Erased = captures52.x3 in
>   ~add (x3({})), (y1({}))
> fn f6(x3: Erased): [Clos14 {x3: Erased}] =
>   Clos14 {x3: x3}
> fn clos42(b1: Erased, captures51: {a1: Erased, f: [F2]}): Int =
>   let f: [F2] = captures51.f in
>   let a1: Erased = captures51.a1 in
>   when when f is
>          | F2 -> f6(~unerase a1)
>        end is
>     | Clos14 captures50 -> clos43(~unerase b1, captures50)
>   end
> fn clos41(a1: Erased, captures48: {f: [F2]}): [Clos2 {a1: Erased, f: [F2]}] =
>   let f: [F2] = captures48.f in
>   Clos2 {a1: a1, f: f}
> fn clos40(r: {}, captures54: {b: Erased}): Erased =
>   let b: Erased = captures54.b in
>   ~erase b
> fn clos39(r1: {}, captures55: {a: Erased}): Erased =
>   let a: Erased = captures55.a in
>   ~erase a
> fn clos38(y1: Erased, captures44: {x3: Erased}): Int =
>   let x3: Erased = captures44.x3 in
>   ~add (x3({})), (y1({}))
> fn clos37(y1: Erased, captures43: {x3: Erased}): Int =
>   let x3: Erased = captures43.x3 in
>   ~add (x3({})), (y1({}))
> fn f5(x3: Erased): [Clos14 {x3: Erased}] =
>   Clos14 {x3: x3}
> fn clos36(b1: Erased, captures42: {a1: Erased, f: [F2]}): Int =
>   let f: [F2] = captures42.f in
>   let a1: Erased = captures42.a1 in
>   when when f is
>          | F2 -> f5(~unerase a1)
>        end is
>     | Clos14 captures41 -> clos37(~unerase b1, captures41)
>   end
> fn clos35(y1: Erased, captures38: {x3: Erased}): Int =
>   let x3: Erased = captures38.x3 in
>   ~add (x3({})), (y1({}))
> fn clos34(y1: Erased, captures37: {x3: Erased}): Int =
>   let x3: Erased = captures37.x3 in
>   ~add (x3({})), (y1({}))
> fn f4(x3: Erased): [Clos14 {x3: Erased}] =
>   Clos14 {x3: x3}
> fn clos33(b1: Erased, captures36: {a1: Erased, f: [F2]}): Int =
>   let f: [F2] = captures36.f in
>   let a1: Erased = captures36.a1 in
>   when when f is
>          | F2 -> f4(~unerase a1)
>        end is
>     | Clos14 captures35 -> clos34(~unerase b1, captures35)
>   end
> fn clos32(a1: Erased, captures39: {f: [F2]}): [Clos2 {a1: Erased, f: [F2]}] =
>   let f: [F2] = captures39.f in
>   Clos2 {a1: a1, f: f}
> fn clos31(r: {}, captures45: {b: Erased}): Erased =
>   let b: Erased = captures45.b in
>   ~erase b
> fn clos30(r1: {}, captures46: {a: Erased}): Erased =
>   let a: Erased = captures46.a in
>   ~erase a
> fn clos29(b1: Erased, captures26: {a1: Erased, f: [Clos13]}): Int =
>   let f: [Clos13] = captures26.f in
>   let a1: Erased = captures26.a1 in
>   when when f is
>          | Clos13 -> clos18(~unerase a1)
>        end is
>     | Clos12 captures25 -> clos24(~unerase b1, captures25)
>   end
> fn clos28(a1: Erased, captures23: {f: [Clos13]}): [
>                                                     Clos8
>                                                       {a1: Erased, f: [Clos13]}
>                                                     ] =
>   let f: [Clos13] = captures23.f in
>   Clos8 {a1: a1, f: f}
> fn clos27(r: {}, captures27: {b: Int}): Erased =
>   let b: Int = captures27.b in
>   ~erase b
> fn clos26(r1: {}, captures28: {a: Int}): Erased =
>   let a: Int = captures28.a in
>   ~erase a
> fn clos25(b1: Erased, captures19: {a1: Erased, f: [Clos13]}): Int =
>   let f: [Clos13] = captures19.f in
>   let a1: Erased = captures19.a1 in
>   when when f is
>          | Clos13 -> clos18(~unerase a1)
>        end is
>     | Clos12 captures18 -> clos24(~unerase b1, captures18)
>   end
> fn clos24(y: Int, captures15: {x: Int}): Int =
>   let x: Int = captures15.x in
>   ~add x, y
> fn clos23(b1: Erased, captures14: {a1: Erased, f: [Clos13]}): Int =
>   let f: [Clos13] = captures14.f in
>   let a1: Erased = captures14.a1 in
>   when when f is
>          | Clos13 -> clos18(~unerase a1)
>        end is
>     | Clos12 captures13 -> clos24(~unerase b1, captures13)
>   end
> fn clos22(a1: Erased, captures16: {f: [Clos13]}): [
>                                                     Clos8
>                                                       {a1: Erased, f: [Clos13]}
>                                                     ] =
>   let f: [Clos13] = captures16.f in
>   Clos8 {a1: a1, f: f}
> fn clos21(r: {}, captures20: {b: Int}): Erased =
>   let b: Int = captures20.b in
>   ~erase b
> fn clos20(r1: {}, captures21: {a: Int}): Erased =
>   let a: Int = captures21.a in
>   ~erase a
> fn eval_map24(m:
>                 [
>                   Map2
>                     {
>                       force_a: [Clos6 {a: Int}],
>                       force_b: [Clos7 {b: Int}],
>                       map2: [Clos9 {f: [Clos13]}]
>                       }
>                   ]): Int =
>   when m is
>     | Map2 m1 ->
>       let a2: Erased =
>         when m1.force_a is
>           | Clos6 captures8 -> clos20({}, captures8)
>         end
>       in
>       let b2: Erased =
>         when m1.force_b is
>           | Clos7 captures9 -> clos21({}, captures9)
>         end
>       in
>       when when m1.map2 is
>              | Clos9 captures10 -> clos22(a2, captures10)
>            end is
>         | Clos8 captures11 -> clos23(b2, captures11)
>       end
>   end
> fn clos19(f: [Clos13], captures22: {a: Int, b: Int}): [
>                                                         Map2
>                                                           {
>                                                             force_a: 
>                                                               [
>                                                                 Clos6 {a: Int}
>                                                                 ],
>                                                             force_b: 
>                                                               [
>                                                                 Clos7 {b: Int}
>                                                                 ],
>                                                             map2: [
>                                                                     Clos9
>                                                                     {
>                                                                     f: [Clos13]
>                                                                     }
>                                                                     ]
>                                                             }
>                                                         ] =
>   let b: Int = captures22.b in
>   let a: Int = captures22.a in
>   Map2 {force_a: Clos6 {a: a}, force_b: Clos7 {b: b}, map2: Clos9 {f: f}}
> fn clos18(x: Int): [Clos12 {x: Int}] =
>   Clos12 {x: x}
> fn clos17(b: Int, captures29: {a: Int}): [Clos10 {a: Int, b: Int}] =
>   let a: Int = captures29.a in
>   Clos10 {a: a, b: b}
> fn build_map24(a: Int): [Clos11 {a: Int}] =
>   Clos11 {a: a}
> fn eval_map23(m:
>                 [
>                   Map2
>                     {
>                       force_a: [Clos {a: [A4]}],
>                       force_b: [Clos1 {b: [B4]}],
>                       map2: [Clos3 {f: [F2]}]
>                       }
>                   ]): Int =
>   when m is
>     | Map2 m1 ->
>       let a2: Erased =
>         when m1.force_a is
>           | Clos captures30 -> clos30({}, captures30)
>         end
>       in
>       let b2: Erased =
>         when m1.force_b is
>           | Clos1 captures31 -> clos31({}, captures31)
>         end
>       in
>       when when m1.map2 is
>              | Clos3 captures32 -> clos32(a2, captures32)
>            end is
>         | Clos2 captures33 -> clos33(b2, captures33)
>       end
>   end
> fn clos16(f: [F2], captures47: {a: Erased, b: Erased}): [
>                                                           Map2
>                                                             {
>                                                               force_a: 
>                                                                 [
>                                                                   Clos
>                                                                     {a: Erased}
>                                                                   ],
>                                                               force_b: 
>                                                                 [
>                                                                   Clos1
>                                                                     {b: Erased}
>                                                                   ],
>                                                               map2: [
>                                                                     Clos3
>                                                                     {f: [F2]}
>                                                                     ]
>                                                               }
>                                                           ] =
>   let b: Erased = captures47.b in
>   let a: Erased = captures47.a in
>   Map2 {force_a: Clos {a: a}, force_b: Clos1 {b: b}, map2: Clos3 {f: f}}
> fn f3(x3: Erased): [Clos14 {x3: Erased}] =
>   Clos14 {x3: x3}
> fn clos15(b: Erased, captures57: {a: Erased}): [Clos4 {a: Erased, b: Erased}] =
>   let a: Erased = captures57.a in
>   Clos4 {a: a, b: b}
> fn b5(x2: []): Int =
>   2
> fn a5(x1: []): Int =
>   1
> fn build_map23(a: Erased): [Clos5 {a: Erased}] =
>   Clos5 {a: a}
> run main2: Int =
>   let mapper1: [
>                  Map2
>                    {
>                      force_a: [Clos {a: [A4]}],
>                      force_b: [Clos1 {b: [B4]}],
>                      map2: [Clos3 {f: [F2]}]
>                      }
>                  ] =
>     when when when Build_map22 is
>                 | Build_map22 -> build_map23(A4)
>               end is
>            | Clos5 captures1 -> clos15(B4, captures1)
>          end is
>       | Clos4 captures2 -> clos16(F2, captures2)
>     end
>   in
>   when Eval_map22 is
>     | Eval_map22 -> eval_map23(mapper1)
>   end
> run main1: Int =
>   let mapper: [
>                 Map2
>                   {
>                     force_a: [Clos6 {a: Int}],
>                     force_b: [Clos7 {b: Int}],
>                     map2: [Clos9 {f: [Clos13]}]
>                     }
>                 ] =
>     when when when Build_map21 is
>                 | Build_map21 -> build_map24(1)
>               end is
>            | Clos11 captures5 -> clos17(2, captures5)
>          end is
>       | Clos10 captures6 -> clos19(Clos13, captures6)
>     end
>   in
>   when Eval_map21 is
>     | Eval_map21 -> eval_map24(mapper)
>   end

> cor-out +ir -print
> fn clos60(y1: *opaque, captures74: { *opaque }): int
> {
>   let x3: *opaque = @get_struct_field<captures74, 0>;
>   let var: {} = @make_struct{};
>   let var1: int = @call_indirect(x3, var);
>   let var2: {} = @make_struct{};
>   let var3: int = @call_indirect(y1, var2);
>   let var4: int = @call_kfn(add, var1, var3);
>   return var4;
> }
> 
> fn clos59(y1: *opaque, captures73: { *opaque }): int
> {
>   let x3: *opaque = @get_struct_field<captures73, 0>;
>   let var5: {} = @make_struct{};
>   let var6: int = @call_indirect(x3, var5);
>   let var7: {} = @make_struct{};
>   let var8: int = @call_indirect(y1, var7);
>   let var9: int = @call_kfn(add, var6, var8);
>   return var9;
> }
> 
> fn f8(x3: *opaque): [ `0 { { *opaque } } ]
> {
>   let var10: { *opaque } = @make_struct{ x3 };
>   let struct: { { *opaque } } = @make_struct{ var10 };
>   let var11: [ `0 { { *opaque } } ] = @make_union<0, struct>;
>   return var11;
> }
> 
> fn clos58(b1: *opaque, captures72: { *opaque, [ `0 {} ] }): int
> {
>   let f: [ `0 {} ] = @get_struct_field<captures72, 1>;
>   let a1: *opaque = @get_struct_field<captures72, 0>;
>   let discr: int = @get_union_id<f>;
>   switch discr {
>   0 -> {
>     let var12: *opaque = @call_kfn(unerase, a1);
>     @call_direct(f8, var12)
>   }
>   } in join join;
>   let discr1: int = @get_union_id<join>;
>   switch discr1 {
>   0 -> {
>     let payload: { { *opaque } } = @get_union_struct<join>;
>     let captures71: { *opaque } = @get_struct_field<payload, 0>;
>     let var13: *opaque = @call_kfn(unerase, b1);
>     @call_direct(clos59, var13, captures71)
>   }
>   } in join join1;
>   return join1;
> }
> 
> fn clos57(a1: *opaque, captures69: { [ `0 {} ] }):
>   [ `0 { { *opaque, [ `0 {} ] } } ]
> {
>   let f: [ `0 {} ] = @get_struct_field<captures69, 0>;
>   let var14: { *opaque, [ `0 {} ] } = @make_struct{ a1, f };
>   let struct1: { { *opaque, [ `0 {} ] } } = @make_struct{ var14 };
>   let var15: [ `0 { { *opaque, [ `0 {} ] } } ] = @make_union<0, struct1>;
>   return var15;
> }
> 
> fn clos56(r: {}, captures75: { *opaque }): *opaque
> {
>   let b: *opaque = @get_struct_field<captures75, 0>;
>   let var16: *opaque = @call_kfn(erase, b);
>   return var16;
> }
> 
> fn clos55(r1: {}, captures76: { *opaque }): *opaque
> {
>   let a: *opaque = @get_struct_field<captures76, 0>;
>   let var17: *opaque = @call_kfn(erase, a);
>   return var17;
> }
> 
> fn clos54(f: [ `0 {} ], captures68: { *opaque, *opaque }):
>   [
>      `0 {
>          {
>           [ `0 { { *opaque } } ],
>            [ `0 { { *opaque } } ],
>            [ `0 { { [ `0 {} ] } } ]
>           ,
>          }
>          ,
>         }
>   ]
> {
>   let b: *opaque = @get_struct_field<captures68, 1>;
>   let a: *opaque = @get_struct_field<captures68, 0>;
>   let var18: { *opaque } = @make_struct{ a };
>   let struct3: { { *opaque } } = @make_struct{ var18 };
>   let var19: [ `0 { { *opaque } } ] = @make_union<0, struct3>;
>   let var20: { *opaque } = @make_struct{ b };
>   let struct4: { { *opaque } } = @make_struct{ var20 };
>   let var21: [ `0 { { *opaque } } ] = @make_union<0, struct4>;
>   let var22: { [ `0 {} ] } = @make_struct{ f };
>   let struct5: { { [ `0 {} ] } } = @make_struct{ var22 };
>   let var23: [ `0 { { [ `0 {} ] } } ] = @make_union<0, struct5>;
>   let var24:
>         {
>          [ `0 { { *opaque } } ],
>           [ `0 { { *opaque } } ],
>           [ `0 { { [ `0 {} ] } } ]
>          ,
>         }
>     = @make_struct{ var19, var21, var23 };
>   let struct2:
>         {
>          {
>           [ `0 { { *opaque } } ],
>            [ `0 { { *opaque } } ],
>            [ `0 { { [ `0 {} ] } } ]
>           ,
>          }
>          ,
>         }
>     = @make_struct{ var24 };
>   let var25:
>         [
>            `0 {
>                {
>                 [ `0 { { *opaque } } ],
>                  [ `0 { { *opaque } } ],
>                  [ `0 { { [ `0 {} ] } } ]
>                 ,
>                }
>                ,
>               }
>         ]
>     = @make_union<0, struct2>;
>   return var25;
> }
> 
> fn clos53(b: *opaque, captures67: { *opaque }): [ `0 { { *opaque, *opaque } } ]
> {
>   let a: *opaque = @get_struct_field<captures67, 0>;
>   let var26: { *opaque, *opaque } = @make_struct{ a, b };
>   let struct6: { { *opaque, *opaque } } = @make_struct{ var26 };
>   let var27: [ `0 { { *opaque, *opaque } } ] = @make_union<0, struct6>;
>   return var27;
> }
> 
> fn clos52(y1: *opaque, captures64: { *opaque }): int
> {
>   let x3: *opaque = @get_struct_field<captures64, 0>;
>   let var28: {} = @make_struct{};
>   let var29: int = @call_indirect(x3, var28);
>   let var30: {} = @make_struct{};
>   let var31: int = @call_indirect(y1, var30);
>   let var32: int = @call_kfn(add, var29, var31);
>   return var32;
> }
> 
> fn clos51(y1: *opaque, captures63: { *opaque }): int
> {
>   let x3: *opaque = @get_struct_field<captures63, 0>;
>   let var33: {} = @make_struct{};
>   let var34: int = @call_indirect(x3, var33);
>   let var35: {} = @make_struct{};
>   let var36: int = @call_indirect(y1, var35);
>   let var37: int = @call_kfn(add, var34, var36);
>   return var37;
> }
> 
> fn f7(x3: *opaque): [ `0 { { *opaque } } ]
> {
>   let var38: { *opaque } = @make_struct{ x3 };
>   let struct7: { { *opaque } } = @make_struct{ var38 };
>   let var39: [ `0 { { *opaque } } ] = @make_union<0, struct7>;
>   return var39;
> }
> 
> fn clos50(b1: *opaque, captures62: { *opaque, [ `0 {} ] }): int
> {
>   let f: [ `0 {} ] = @get_struct_field<captures62, 1>;
>   let a1: *opaque = @get_struct_field<captures62, 0>;
>   let discr2: int = @get_union_id<f>;
>   switch discr2 {
>   0 -> {
>     let var40: *opaque = @call_kfn(unerase, a1);
>     @call_direct(f7, var40)
>   }
>   } in join join2;
>   let discr3: int = @get_union_id<join2>;
>   switch discr3 {
>   0 -> {
>     let payload1: { { *opaque } } = @get_union_struct<join2>;
>     let captures61: { *opaque } = @get_struct_field<payload1, 0>;
>     let var41: *opaque = @call_kfn(unerase, b1);
>     @call_direct(clos51, var41, captures61)
>   }
>   } in join join3;
>   return join3;
> }
> 
> fn clos49(a1: *opaque, captures59: { [ `0 {} ] }):
>   [ `0 { { *opaque, [ `0 {} ] } } ]
> {
>   let f: [ `0 {} ] = @get_struct_field<captures59, 0>;
>   let var42: { *opaque, [ `0 {} ] } = @make_struct{ a1, f };
>   let struct8: { { *opaque, [ `0 {} ] } } = @make_struct{ var42 };
>   let var43: [ `0 { { *opaque, [ `0 {} ] } } ] = @make_union<0, struct8>;
>   return var43;
> }
> 
> fn clos48(r: {}, captures65: { *opaque }): *opaque
> {
>   let b: *opaque = @get_struct_field<captures65, 0>;
>   let var44: *opaque = @call_kfn(erase, b);
>   return var44;
> }
> 
> fn clos47(r1: {}, captures66: { *opaque }): *opaque
> {
>   let a: *opaque = @get_struct_field<captures66, 0>;
>   let var45: *opaque = @call_kfn(erase, a);
>   return var45;
> }
> 
> fn clos46(f: [ `0 {} ], captures58: { *opaque, *opaque }):
>   [
>      `0 {
>          {
>           [ `0 { { *opaque } } ],
>            [ `0 { { *opaque } } ],
>            [ `0 { { [ `0 {} ] } } ]
>           ,
>          }
>          ,
>         }
>   ]
> {
>   let b: *opaque = @get_struct_field<captures58, 1>;
>   let a: *opaque = @get_struct_field<captures58, 0>;
>   let var46: { *opaque } = @make_struct{ a };
>   let struct10: { { *opaque } } = @make_struct{ var46 };
>   let var47: [ `0 { { *opaque } } ] = @make_union<0, struct10>;
>   let var48: { *opaque } = @make_struct{ b };
>   let struct11: { { *opaque } } = @make_struct{ var48 };
>   let var49: [ `0 { { *opaque } } ] = @make_union<0, struct11>;
>   let var50: { [ `0 {} ] } = @make_struct{ f };
>   let struct12: { { [ `0 {} ] } } = @make_struct{ var50 };
>   let var51: [ `0 { { [ `0 {} ] } } ] = @make_union<0, struct12>;
>   let var52:
>         {
>          [ `0 { { *opaque } } ],
>           [ `0 { { *opaque } } ],
>           [ `0 { { [ `0 {} ] } } ]
>          ,
>         }
>     = @make_struct{ var47, var49, var51 };
>   let struct9:
>         {
>          {
>           [ `0 { { *opaque } } ],
>            [ `0 { { *opaque } } ],
>            [ `0 { { [ `0 {} ] } } ]
>           ,
>          }
>          ,
>         }
>     = @make_struct{ var52 };
>   let var53:
>         [
>            `0 {
>                {
>                 [ `0 { { *opaque } } ],
>                  [ `0 { { *opaque } } ],
>                  [ `0 { { [ `0 {} ] } } ]
>                 ,
>                }
>                ,
>               }
>         ]
>     = @make_union<0, struct9>;
>   return var53;
> }
> 
> fn clos45(y1: *opaque, captures56: { *opaque }): int
> {
>   let x3: *opaque = @get_struct_field<captures56, 0>;
>   let var54: {} = @make_struct{};
>   let var55: int = @call_indirect(x3, var54);
>   let var56: {} = @make_struct{};
>   let var57: int = @call_indirect(y1, var56);
>   let var58: int = @call_kfn(add, var55, var57);
>   return var58;
> }
> 
> fn clos44(y1: *opaque, captures53: { *opaque }): int
> {
>   let x3: *opaque = @get_struct_field<captures53, 0>;
>   let var59: {} = @make_struct{};
>   let var60: int = @call_indirect(x3, var59);
>   let var61: {} = @make_struct{};
>   let var62: int = @call_indirect(y1, var61);
>   let var63: int = @call_kfn(add, var60, var62);
>   return var63;
> }
> 
> fn clos43(y1: *opaque, captures52: { *opaque }): int
> {
>   let x3: *opaque = @get_struct_field<captures52, 0>;
>   let var64: {} = @make_struct{};
>   let var65: int = @call_indirect(x3, var64);
>   let var66: {} = @make_struct{};
>   let var67: int = @call_indirect(y1, var66);
>   let var68: int = @call_kfn(add, var65, var67);
>   return var68;
> }
> 
> fn f6(x3: *opaque): [ `0 { { *opaque } } ]
> {
>   let var69: { *opaque } = @make_struct{ x3 };
>   let struct13: { { *opaque } } = @make_struct{ var69 };
>   let var70: [ `0 { { *opaque } } ] = @make_union<0, struct13>;
>   return var70;
> }
> 
> fn clos42(b1: *opaque, captures51: { *opaque, [ `0 {} ] }): int
> {
>   let f: [ `0 {} ] = @get_struct_field<captures51, 1>;
>   let a1: *opaque = @get_struct_field<captures51, 0>;
>   let discr4: int = @get_union_id<f>;
>   switch discr4 {
>   0 -> {
>     let var71: *opaque = @call_kfn(unerase, a1);
>     @call_direct(f6, var71)
>   }
>   } in join join4;
>   let discr5: int = @get_union_id<join4>;
>   switch discr5 {
>   0 -> {
>     let payload2: { { *opaque } } = @get_union_struct<join4>;
>     let captures50: { *opaque } = @get_struct_field<payload2, 0>;
>     let var72: *opaque = @call_kfn(unerase, b1);
>     @call_direct(clos43, var72, captures50)
>   }
>   } in join join5;
>   return join5;
> }
> 
> fn clos41(a1: *opaque, captures48: { [ `0 {} ] }):
>   [ `0 { { *opaque, [ `0 {} ] } } ]
> {
>   let f: [ `0 {} ] = @get_struct_field<captures48, 0>;
>   let var73: { *opaque, [ `0 {} ] } = @make_struct{ a1, f };
>   let struct14: { { *opaque, [ `0 {} ] } } = @make_struct{ var73 };
>   let var74: [ `0 { { *opaque, [ `0 {} ] } } ] = @make_union<0, struct14>;
>   return var74;
> }
> 
> fn clos40(r: {}, captures54: { *opaque }): *opaque
> {
>   let b: *opaque = @get_struct_field<captures54, 0>;
>   let var75: *opaque = @call_kfn(erase, b);
>   return var75;
> }
> 
> fn clos39(r1: {}, captures55: { *opaque }): *opaque
> {
>   let a: *opaque = @get_struct_field<captures55, 0>;
>   let var76: *opaque = @call_kfn(erase, a);
>   return var76;
> }
> 
> fn clos38(y1: *opaque, captures44: { *opaque }): int
> {
>   let x3: *opaque = @get_struct_field<captures44, 0>;
>   let var77: {} = @make_struct{};
>   let var78: int = @call_indirect(x3, var77);
>   let var79: {} = @make_struct{};
>   let var80: int = @call_indirect(y1, var79);
>   let var81: int = @call_kfn(add, var78, var80);
>   return var81;
> }
> 
> fn clos37(y1: *opaque, captures43: { *opaque }): int
> {
>   let x3: *opaque = @get_struct_field<captures43, 0>;
>   let var82: {} = @make_struct{};
>   let var83: int = @call_indirect(x3, var82);
>   let var84: {} = @make_struct{};
>   let var85: int = @call_indirect(y1, var84);
>   let var86: int = @call_kfn(add, var83, var85);
>   return var86;
> }
> 
> fn f5(x3: *opaque): [ `0 { { *opaque } } ]
> {
>   let var87: { *opaque } = @make_struct{ x3 };
>   let struct15: { { *opaque } } = @make_struct{ var87 };
>   let var88: [ `0 { { *opaque } } ] = @make_union<0, struct15>;
>   return var88;
> }
> 
> fn clos36(b1: *opaque, captures42: { *opaque, [ `0 {} ] }): int
> {
>   let f: [ `0 {} ] = @get_struct_field<captures42, 1>;
>   let a1: *opaque = @get_struct_field<captures42, 0>;
>   let discr6: int = @get_union_id<f>;
>   switch discr6 {
>   0 -> {
>     let var89: *opaque = @call_kfn(unerase, a1);
>     @call_direct(f5, var89)
>   }
>   } in join join6;
>   let discr7: int = @get_union_id<join6>;
>   switch discr7 {
>   0 -> {
>     let payload3: { { *opaque } } = @get_union_struct<join6>;
>     let captures41: { *opaque } = @get_struct_field<payload3, 0>;
>     let var90: *opaque = @call_kfn(unerase, b1);
>     @call_direct(clos37, var90, captures41)
>   }
>   } in join join7;
>   return join7;
> }
> 
> fn clos35(y1: *opaque, captures38: { *opaque }): int
> {
>   let x3: *opaque = @get_struct_field<captures38, 0>;
>   let var91: {} = @make_struct{};
>   let var92: int = @call_indirect(x3, var91);
>   let var93: {} = @make_struct{};
>   let var94: int = @call_indirect(y1, var93);
>   let var95: int = @call_kfn(add, var92, var94);
>   return var95;
> }
> 
> fn clos34(y1: *opaque, captures37: { *opaque }): int
> {
>   let x3: *opaque = @get_struct_field<captures37, 0>;
>   let var96: {} = @make_struct{};
>   let var97: int = @call_indirect(x3, var96);
>   let var98: {} = @make_struct{};
>   let var99: int = @call_indirect(y1, var98);
>   let var100: int = @call_kfn(add, var97, var99);
>   return var100;
> }
> 
> fn f4(x3: *opaque): [ `0 { { *opaque } } ]
> {
>   let var101: { *opaque } = @make_struct{ x3 };
>   let struct16: { { *opaque } } = @make_struct{ var101 };
>   let var102: [ `0 { { *opaque } } ] = @make_union<0, struct16>;
>   return var102;
> }
> 
> fn clos33(b1: *opaque, captures36: { *opaque, [ `0 {} ] }): int
> {
>   let f: [ `0 {} ] = @get_struct_field<captures36, 1>;
>   let a1: *opaque = @get_struct_field<captures36, 0>;
>   let discr8: int = @get_union_id<f>;
>   switch discr8 {
>   0 -> {
>     let var103: *opaque = @call_kfn(unerase, a1);
>     @call_direct(f4, var103)
>   }
>   } in join join8;
>   let discr9: int = @get_union_id<join8>;
>   switch discr9 {
>   0 -> {
>     let payload4: { { *opaque } } = @get_union_struct<join8>;
>     let captures35: { *opaque } = @get_struct_field<payload4, 0>;
>     let var104: *opaque = @call_kfn(unerase, b1);
>     @call_direct(clos34, var104, captures35)
>   }
>   } in join join9;
>   return join9;
> }
> 
> fn clos32(a1: *opaque, captures39: { [ `0 {} ] }):
>   [ `0 { { *opaque, [ `0 {} ] } } ]
> {
>   let f: [ `0 {} ] = @get_struct_field<captures39, 0>;
>   let var105: { *opaque, [ `0 {} ] } = @make_struct{ a1, f };
>   let struct17: { { *opaque, [ `0 {} ] } } = @make_struct{ var105 };
>   let var106: [ `0 { { *opaque, [ `0 {} ] } } ] = @make_union<0, struct17>;
>   return var106;
> }
> 
> fn clos31(r: {}, captures45: { *opaque }): *opaque
> {
>   let b: *opaque = @get_struct_field<captures45, 0>;
>   let var107: *opaque = @call_kfn(erase, b);
>   return var107;
> }
> 
> fn clos30(r1: {}, captures46: { *opaque }): *opaque
> {
>   let a: *opaque = @get_struct_field<captures46, 0>;
>   let var108: *opaque = @call_kfn(erase, a);
>   return var108;
> }
> 
> fn clos29(b1: *opaque, captures26: { *opaque, [ `0 {} ] }): int
> {
>   let f: [ `0 {} ] = @get_struct_field<captures26, 1>;
>   let a1: *opaque = @get_struct_field<captures26, 0>;
>   let discr10: int = @get_union_id<f>;
>   switch discr10 {
>   0 -> {
>     let var109: int = @call_kfn(unerase, a1);
>     @call_direct(clos18, var109)
>   }
>   } in join join10;
>   let discr11: int = @get_union_id<join10>;
>   switch discr11 {
>   0 -> {
>     let payload5: { { int } } = @get_union_struct<join10>;
>     let captures25: { int } = @get_struct_field<payload5, 0>;
>     let var110: int = @call_kfn(unerase, b1);
>     @call_direct(clos24, var110, captures25)
>   }
>   } in join join11;
>   return join11;
> }
> 
> fn clos28(a1: *opaque, captures23: { [ `0 {} ] }):
>   [ `0 { { *opaque, [ `0 {} ] } } ]
> {
>   let f: [ `0 {} ] = @get_struct_field<captures23, 0>;
>   let var111: { *opaque, [ `0 {} ] } = @make_struct{ a1, f };
>   let struct18: { { *opaque, [ `0 {} ] } } = @make_struct{ var111 };
>   let var112: [ `0 { { *opaque, [ `0 {} ] } } ] = @make_union<0, struct18>;
>   return var112;
> }
> 
> fn clos27(r: {}, captures27: { int }): *opaque
> {
>   let b: int = @get_struct_field<captures27, 0>;
>   let var113: *opaque = @call_kfn(erase, b);
>   return var113;
> }
> 
> fn clos26(r1: {}, captures28: { int }): *opaque
> {
>   let a: int = @get_struct_field<captures28, 0>;
>   let var114: *opaque = @call_kfn(erase, a);
>   return var114;
> }
> 
> fn clos25(b1: *opaque, captures19: { *opaque, [ `0 {} ] }): int
> {
>   let f: [ `0 {} ] = @get_struct_field<captures19, 1>;
>   let a1: *opaque = @get_struct_field<captures19, 0>;
>   let discr12: int = @get_union_id<f>;
>   switch discr12 {
>   0 -> {
>     let var115: int = @call_kfn(unerase, a1);
>     @call_direct(clos18, var115)
>   }
>   } in join join12;
>   let discr13: int = @get_union_id<join12>;
>   switch discr13 {
>   0 -> {
>     let payload6: { { int } } = @get_union_struct<join12>;
>     let captures18: { int } = @get_struct_field<payload6, 0>;
>     let var116: int = @call_kfn(unerase, b1);
>     @call_direct(clos24, var116, captures18)
>   }
>   } in join join13;
>   return join13;
> }
> 
> fn clos24(y: int, captures15: { int }): int
> {
>   let x: int = @get_struct_field<captures15, 0>;
>   let var117: int = @call_kfn(add, x, y);
>   return var117;
> }
> 
> fn clos23(b1: *opaque, captures14: { *opaque, [ `0 {} ] }): int
> {
>   let f: [ `0 {} ] = @get_struct_field<captures14, 1>;
>   let a1: *opaque = @get_struct_field<captures14, 0>;
>   let discr14: int = @get_union_id<f>;
>   switch discr14 {
>   0 -> {
>     let var118: int = @call_kfn(unerase, a1);
>     @call_direct(clos18, var118)
>   }
>   } in join join14;
>   let discr15: int = @get_union_id<join14>;
>   switch discr15 {
>   0 -> {
>     let payload7: { { int } } = @get_union_struct<join14>;
>     let captures13: { int } = @get_struct_field<payload7, 0>;
>     let var119: int = @call_kfn(unerase, b1);
>     @call_direct(clos24, var119, captures13)
>   }
>   } in join join15;
>   return join15;
> }
> 
> fn clos22(a1: *opaque, captures16: { [ `0 {} ] }):
>   [ `0 { { *opaque, [ `0 {} ] } } ]
> {
>   let f: [ `0 {} ] = @get_struct_field<captures16, 0>;
>   let var120: { *opaque, [ `0 {} ] } = @make_struct{ a1, f };
>   let struct19: { { *opaque, [ `0 {} ] } } = @make_struct{ var120 };
>   let var121: [ `0 { { *opaque, [ `0 {} ] } } ] = @make_union<0, struct19>;
>   return var121;
> }
> 
> fn clos21(r: {}, captures20: { int }): *opaque
> {
>   let b: int = @get_struct_field<captures20, 0>;
>   let var122: *opaque = @call_kfn(erase, b);
>   return var122;
> }
> 
> fn clos20(r1: {}, captures21: { int }): *opaque
> {
>   let a: int = @get_struct_field<captures21, 0>;
>   let var123: *opaque = @call_kfn(erase, a);
>   return var123;
> }
> 
> fn eval_map24(
>   m:
>     [
>        `0 {
>            { [ `0 { { int } } ], [ `0 { { int } } ], [ `0 { { [ `0 {} ] } } ] }
>            ,
>           }
>     ]):
>   int
> {
>   let discr16: int = @get_union_id<m>;
>   switch discr16 {
>   0 -> {
>     let payload8:
>           {
>            { [ `0 { { int } } ], [ `0 { { int } } ], [ `0 { { [ `0 {} ] } } ] }
>            ,
>           }
>       = @get_union_struct<m>;
>     let m1:
>           { [ `0 { { int } } ], [ `0 { { int } } ], [ `0 { { [ `0 {} ] } } ] }
>       = @get_struct_field<payload8, 0>;
>     let var124: [ `0 { { int } } ] = @get_struct_field<m1, 0>;
>     let discr17: int = @get_union_id<var124>;
>     switch discr17 {
>     0 -> {
>       let payload9: { { int } } = @get_union_struct<var124>;
>       let captures8: { int } = @get_struct_field<payload9, 0>;
>       let var125: {} = @make_struct{};
>       @call_direct(clos20, var125, captures8)
>     }
>     } in join join16;
>     let a2: *opaque = join16;
>     let var126: [ `0 { { int } } ] = @get_struct_field<m1, 1>;
>     let discr18: int = @get_union_id<var126>;
>     switch discr18 {
>     0 -> {
>       let payload10: { { int } } = @get_union_struct<var126>;
>       let captures9: { int } = @get_struct_field<payload10, 0>;
>       let var127: {} = @make_struct{};
>       @call_direct(clos21, var127, captures9)
>     }
>     } in join join17;
>     let b2: *opaque = join17;
>     let var128: [ `0 { { [ `0 {} ] } } ] = @get_struct_field<m1, 2>;
>     let discr19: int = @get_union_id<var128>;
>     switch discr19 {
>     0 -> {
>       let payload11: { { [ `0 {} ] } } = @get_union_struct<var128>;
>       let captures10: { [ `0 {} ] } = @get_struct_field<payload11, 0>;
>       @call_direct(clos22, a2, captures10)
>     }
>     } in join join18;
>     let discr20: int = @get_union_id<join18>;
>     switch discr20 {
>     0 -> {
>       let payload12: { { *opaque, [ `0 {} ] } } = @get_union_struct<join18>;
>       let captures11: { *opaque, [ `0 {} ] } = @get_struct_field<payload12, 0>;
>       @call_direct(clos23, b2, captures11)
>     }
>     } in join join19;
>     join19
>   }
>   } in join join20;
>   return join20;
> }
> 
> fn clos19(f: [ `0 {} ], captures22: { int, int }):
>   [ `0 { { [ `0 { { int } } ], [ `0 { { int } } ], [ `0 { { [ `0 {} ] } } ] } }
>   ]
> {
>   let b: int = @get_struct_field<captures22, 1>;
>   let a: int = @get_struct_field<captures22, 0>;
>   let var129: { int } = @make_struct{ a };
>   let struct21: { { int } } = @make_struct{ var129 };
>   let var130: [ `0 { { int } } ] = @make_union<0, struct21>;
>   let var131: { int } = @make_struct{ b };
>   let struct22: { { int } } = @make_struct{ var131 };
>   let var132: [ `0 { { int } } ] = @make_union<0, struct22>;
>   let var133: { [ `0 {} ] } = @make_struct{ f };
>   let struct23: { { [ `0 {} ] } } = @make_struct{ var133 };
>   let var134: [ `0 { { [ `0 {} ] } } ] = @make_union<0, struct23>;
>   let var135:
>         { [ `0 { { int } } ], [ `0 { { int } } ], [ `0 { { [ `0 {} ] } } ] }
>     = @make_struct{ var130, var132, var134 };
>   let struct20:
>         { { [ `0 { { int } } ], [ `0 { { int } } ], [ `0 { { [ `0 {} ] } } ] } ,
>         }
>     = @make_struct{ var135 };
>   let var136:
>         [
>            `0 {
>                {
>                 [ `0 { { int } } ],
>                  [ `0 { { int } } ],
>                  [ `0 { { [ `0 {} ] } } ]
>                 ,
>                }
>                ,
>               }
>         ]
>     = @make_union<0, struct20>;
>   return var136;
> }
> 
> fn clos18(x: int): [ `0 { { int } } ]
> {
>   let var137: { int } = @make_struct{ x };
>   let struct24: { { int } } = @make_struct{ var137 };
>   let var138: [ `0 { { int } } ] = @make_union<0, struct24>;
>   return var138;
> }
> 
> fn clos17(b: int, captures29: { int }): [ `0 { { int, int } } ]
> {
>   let a: int = @get_struct_field<captures29, 0>;
>   let var139: { int, int } = @make_struct{ a, b };
>   let struct25: { { int, int } } = @make_struct{ var139 };
>   let var140: [ `0 { { int, int } } ] = @make_union<0, struct25>;
>   return var140;
> }
> 
> fn build_map24(a: int): [ `0 { { int } } ]
> {
>   let var141: { int } = @make_struct{ a };
>   let struct26: { { int } } = @make_struct{ var141 };
>   let var142: [ `0 { { int } } ] = @make_union<0, struct26>;
>   return var142;
> }
> 
> fn eval_map23(
>   m:
>     [
>        `0 {
>            {
>             [ `0 { { [ `0 {} ] } } ],
>              [ `0 { { [ `0 {} ] } } ],
>              [ `0 { { [ `0 {} ] } } ]
>             ,
>            }
>            ,
>           }
>     ]):
>   int
> {
>   let discr21: int = @get_union_id<m>;
>   switch discr21 {
>   0 -> {
>     let payload13:
>           {
>            {
>             [ `0 { { [ `0 {} ] } } ],
>              [ `0 { { [ `0 {} ] } } ],
>              [ `0 { { [ `0 {} ] } } ]
>             ,
>            }
>            ,
>           }
>       = @get_union_struct<m>;
>     let m1:
>           {
>            [ `0 { { [ `0 {} ] } } ],
>             [ `0 { { [ `0 {} ] } } ],
>             [ `0 { { [ `0 {} ] } } ]
>            ,
>           }
>       = @get_struct_field<payload13, 0>;
>     let var143: [ `0 { { [ `0 {} ] } } ] = @get_struct_field<m1, 0>;
>     let discr22: int = @get_union_id<var143>;
>     switch discr22 {
>     0 -> {
>       let payload14: { { [ `0 {} ] } } = @get_union_struct<var143>;
>       let captures30: { [ `0 {} ] } = @get_struct_field<payload14, 0>;
>       let var144: {} = @make_struct{};
>       @call_direct(clos30, var144, captures30)
>     }
>     } in join join21;
>     let a2: *opaque = join21;
>     let var145: [ `0 { { [ `0 {} ] } } ] = @get_struct_field<m1, 1>;
>     let discr23: int = @get_union_id<var145>;
>     switch discr23 {
>     0 -> {
>       let payload15: { { [ `0 {} ] } } = @get_union_struct<var145>;
>       let captures31: { [ `0 {} ] } = @get_struct_field<payload15, 0>;
>       let var146: {} = @make_struct{};
>       @call_direct(clos31, var146, captures31)
>     }
>     } in join join22;
>     let b2: *opaque = join22;
>     let var147: [ `0 { { [ `0 {} ] } } ] = @get_struct_field<m1, 2>;
>     let discr24: int = @get_union_id<var147>;
>     switch discr24 {
>     0 -> {
>       let payload16: { { [ `0 {} ] } } = @get_union_struct<var147>;
>       let captures32: { [ `0 {} ] } = @get_struct_field<payload16, 0>;
>       @call_direct(clos32, a2, captures32)
>     }
>     } in join join23;
>     let discr25: int = @get_union_id<join23>;
>     switch discr25 {
>     0 -> {
>       let payload17: { { *opaque, [ `0 {} ] } } = @get_union_struct<join23>;
>       let captures33: { *opaque, [ `0 {} ] } = @get_struct_field<payload17, 0>;
>       @call_direct(clos33, b2, captures33)
>     }
>     } in join join24;
>     join24
>   }
>   } in join join25;
>   return join25;
> }
> 
> fn clos16(f: [ `0 {} ], captures47: { *opaque, *opaque }):
>   [
>      `0 {
>          {
>           [ `0 { { *opaque } } ],
>            [ `0 { { *opaque } } ],
>            [ `0 { { [ `0 {} ] } } ]
>           ,
>          }
>          ,
>         }
>   ]
> {
>   let b: *opaque = @get_struct_field<captures47, 1>;
>   let a: *opaque = @get_struct_field<captures47, 0>;
>   let var148: { *opaque } = @make_struct{ a };
>   let struct28: { { *opaque } } = @make_struct{ var148 };
>   let var149: [ `0 { { *opaque } } ] = @make_union<0, struct28>;
>   let var150: { *opaque } = @make_struct{ b };
>   let struct29: { { *opaque } } = @make_struct{ var150 };
>   let var151: [ `0 { { *opaque } } ] = @make_union<0, struct29>;
>   let var152: { [ `0 {} ] } = @make_struct{ f };
>   let struct30: { { [ `0 {} ] } } = @make_struct{ var152 };
>   let var153: [ `0 { { [ `0 {} ] } } ] = @make_union<0, struct30>;
>   let var154:
>         {
>          [ `0 { { *opaque } } ],
>           [ `0 { { *opaque } } ],
>           [ `0 { { [ `0 {} ] } } ]
>          ,
>         }
>     = @make_struct{ var149, var151, var153 };
>   let struct27:
>         {
>          {
>           [ `0 { { *opaque } } ],
>            [ `0 { { *opaque } } ],
>            [ `0 { { [ `0 {} ] } } ]
>           ,
>          }
>          ,
>         }
>     = @make_struct{ var154 };
>   let var155:
>         [
>            `0 {
>                {
>                 [ `0 { { *opaque } } ],
>                  [ `0 { { *opaque } } ],
>                  [ `0 { { [ `0 {} ] } } ]
>                 ,
>                }
>                ,
>               }
>         ]
>     = @make_union<0, struct27>;
>   return var155;
> }
> 
> fn f3(x3: *opaque): [ `0 { { *opaque } } ]
> {
>   let var156: { *opaque } = @make_struct{ x3 };
>   let struct31: { { *opaque } } = @make_struct{ var156 };
>   let var157: [ `0 { { *opaque } } ] = @make_union<0, struct31>;
>   return var157;
> }
> 
> fn clos15(b: *opaque, captures57: { *opaque }): [ `0 { { *opaque, *opaque } } ]
> {
>   let a: *opaque = @get_struct_field<captures57, 0>;
>   let var158: { *opaque, *opaque } = @make_struct{ a, b };
>   let struct32: { { *opaque, *opaque } } = @make_struct{ var158 };
>   let var159: [ `0 { { *opaque, *opaque } } ] = @make_union<0, struct32>;
>   return var159;
> }
> 
> fn b5(x2: []): int
> {
>   let var160: int = 2;
>   return var160;
> }
> 
> fn a5(x1: []): int
> {
>   let var161: int = 1;
>   return var161;
> }
> 
> fn build_map23(a: *opaque): [ `0 { { *opaque } } ]
> {
>   let var162: { *opaque } = @make_struct{ a };
>   let struct33: { { *opaque } } = @make_struct{ var162 };
>   let var163: [ `0 { { *opaque } } ] = @make_union<0, struct33>;
>   return var163;
> }
> 
> fn main2_thunk(): int
> {
>   let struct34: {} = @make_struct{};
>   let var164: [ `0 {} ] = @make_union<0, struct34>;
>   let discr26: int = @get_union_id<var164>;
>   switch discr26 {
>   0 -> {
>     let struct35: {} = @make_struct{};
>     let var165: [ `0 {} ] = @make_union<0, struct35>;
>     @call_direct(build_map23, var165)
>   }
>   } in join join26;
>   let discr27: int = @get_union_id<join26>;
>   switch discr27 {
>   0 -> {
>     let payload18: { { [ `0 {} ] } } = @get_union_struct<join26>;
>     let captures1: { [ `0 {} ] } = @get_struct_field<payload18, 0>;
>     let struct36: {} = @make_struct{};
>     let var166: [ `0 {} ] = @make_union<0, struct36>;
>     @call_direct(clos15, var166, captures1)
>   }
>   } in join join27;
>   let discr28: int = @get_union_id<join27>;
>   switch discr28 {
>   0 -> {
>     let payload19: { { [ `0 {} ], [ `0 {} ] } } = @get_union_struct<join27>;
>     let captures2: { [ `0 {} ], [ `0 {} ] } = @get_struct_field<payload19, 0>;
>     let struct37: {} = @make_struct{};
>     let var167: [ `0 {} ] = @make_union<0, struct37>;
>     @call_direct(clos16, var167, captures2)
>   }
>   } in join join28;
>   let mapper1:
>         [
>            `0 {
>                {
>                 [ `0 { { [ `0 {} ] } } ],
>                  [ `0 { { [ `0 {} ] } } ],
>                  [ `0 { { [ `0 {} ] } } ]
>                 ,
>                }
>                ,
>               }
>         ]
>     = join28;
>   let struct38: {} = @make_struct{};
>   let var168: [ `0 {} ] = @make_union<0, struct38>;
>   let discr29: int = @get_union_id<var168>;
>   switch discr29 {
>   0 -> {
>     @call_direct(eval_map23, mapper1)
>   }
>   } in join join29;
>   return join29;
> }
> 
> entry main2: int = @call_direct(main2_thunk);
> 
> fn main1_thunk(): int
> {
>   let struct39: {} = @make_struct{};
>   let var169: [ `0 {} ] = @make_union<0, struct39>;
>   let discr30: int = @get_union_id<var169>;
>   switch discr30 {
>   0 -> {
>     let var170: int = 1;
>     @call_direct(build_map24, var170)
>   }
>   } in join join30;
>   let discr31: int = @get_union_id<join30>;
>   switch discr31 {
>   0 -> {
>     let payload20: { { int } } = @get_union_struct<join30>;
>     let captures5: { int } = @get_struct_field<payload20, 0>;
>     let var171: int = 2;
>     @call_direct(clos17, var171, captures5)
>   }
>   } in join join31;
>   let discr32: int = @get_union_id<join31>;
>   switch discr32 {
>   0 -> {
>     let payload21: { { int, int } } = @get_union_struct<join31>;
>     let captures6: { int, int } = @get_struct_field<payload21, 0>;
>     let struct40: {} = @make_struct{};
>     let var172: [ `0 {} ] = @make_union<0, struct40>;
>     @call_direct(clos19, var172, captures6)
>   }
>   } in join join32;
>   let mapper:
>         [
>            `0 {
>                {
>                 [ `0 { { int } } ],
>                  [ `0 { { int } } ],
>                  [ `0 { { [ `0 {} ] } } ]
>                 ,
>                }
>                ,
>               }
>         ]
>     = join32;
>   let struct41: {} = @make_struct{};
>   let var173: [ `0 {} ] = @make_union<0, struct41>;
>   let discr33: int = @get_union_id<var173>;
>   switch discr33 {
>   0 -> {
>     @call_direct(eval_map24, mapper)
>   }
>   } in join join33;
>   return join33;
> }
> 
> entry main1: int = @call_direct(main1_thunk);