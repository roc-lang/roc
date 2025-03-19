# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

let map = \f -> \xs ->
  let go = \xs ->
    when xs is
      | Nil -> Nil
      | Cons x xs -> Cons (f x) (go xs)
    end
  in go xs
;;

run main1 =
    let f = \x -> A x in
    map f (Cons 1 (Cons 2 Nil));;

run main2 =
    let f = \x -> x + 1 in
    map f (Cons 1 (Cons 2 Nil));;

> cor-out +monotype -print
> let map2: (Int -> Int) -> [Cons Int <rec>, Nil] -> [Cons Int <rec>, Nil] = \f ->
>   \xs ->
>     (let rec go: [Cons Int <rec>, Nil] -> [Cons Int <rec>, Nil] = \xs1 ->
>        when xs1 is
>          | Nil -> Nil 
>          | Cons x xs2 -> Cons (f x) (go xs2)
>        end
>     in
>     go xs)
> let map1: (Int -> [A Int])
>             -> [Cons Int <rec>, Nil] -> [Cons [A Int] <rec>, Nil] = \f ->
>   \xs ->
>     (let rec go: [Cons Int <rec>, Nil] -> [Cons [A Int] <rec>, Nil] = \xs1 ->
>        when xs1 is
>          | Nil -> Nil 
>          | Cons x xs2 -> Cons (f x) (go xs2)
>        end
>     in
>     go xs)
> run main1: [Cons [A Int] <rec>, Nil] =
>   let f1: Int -> [A Int] = \x1 ->
>     A x1 in
>   (map1 f1) (Cons 1 (Cons 2 (Nil )))
> run main2: [Cons Int <rec>, Nil] =
>   let f2: Int -> Int = \x2 ->
>     ~add x2 1 in
>   (map2 f2) (Cons 1 (Cons 2 (Nil )))

> cor-out +monotype_lifted -print
> let go1(f: Int -> Int): [Cons Int <rec>, Nil] -> [Cons Int <rec>, Nil] = \xs1 ->
>   when xs1 is
>     | Nil -> Nil 
>     | Cons x xs2 -> Cons (f x) (go1 xs2)
>   end
> let clos(f: Int -> Int): [Cons Int <rec>, Nil] -> [Cons Int <rec>, Nil] = \xs ->
>   let go2: [Cons Int <rec>, Nil] -> [Cons Int <rec>, Nil] =
>     go1 in
>   go2 xs
> let map2: (Int -> Int) -> [Cons Int <rec>, Nil] -> [Cons Int <rec>, Nil] = \f ->
>   clos
> let go3(f: Int -> [A Int]): [Cons Int <rec>, Nil] -> [Cons [A Int] <rec>, Nil] = \xs1 ->
>   when xs1 is
>     | Nil -> Nil 
>     | Cons x xs2 -> Cons (f x) (go3 xs2)
>   end
> let clos1(f: Int -> [A Int]): [Cons Int <rec>, Nil]
>                                 -> [Cons [A Int] <rec>, Nil] = \xs ->
>   let go4: [Cons Int <rec>, Nil] -> [Cons [A Int] <rec>, Nil] =
>     go3 in
>   go4 xs
> let map1: (Int -> [A Int])
>             -> [Cons Int <rec>, Nil] -> [Cons [A Int] <rec>, Nil] = \f ->
>   clos1
> let f3: Int -> [A Int] = \x1 ->
>   A x1
> run main1: [Cons [A Int] <rec>, Nil] =
>   (map1 f3) (Cons 1 (Cons 2 (Nil )))
> let f4: Int -> Int = \x2 ->
>   ~add x2 1
> run main2: [Cons Int <rec>, Nil] =
>   (map2 f4) (Cons 1 (Cons 2 (Nil )))

> cor-out +lambdasolved -print
> let go3(f: Int -<'1318>-> [A Int]): [Cons Int <rec>, Nil]
>                                       -[go3 (f: (Int -<'1318>-> [A Int]))]-> 
>                                       [
>                                         Cons [A Int] <rec>,
>                                         Nil
>                                         ] = \xs1 ->
>   when xs1 is
>     | Nil -> Nil 
>     | Cons x xs2 -> Cons (f x) (go3 xs2)
>   end
> let f4: Int -[f4]-> Int = \x2 ->
>   ~add x2 1
> let go1(f: Int -<'1396>-> Int): [Cons Int <rec>, Nil]
>                                   -[go1 (f: (Int -<'1396>-> Int))]-> 
>                                   [
>                                     Cons Int <rec>,
>                                     Nil
>                                     ] = \xs1 ->
>   when xs1 is
>     | Nil -> Nil 
>     | Cons x xs2 -> Cons (f x) (go1 xs2)
>   end
> let clos(f: Int -<'1472>-> Int): [Cons Int <rec>, Nil]
>                                    -[clos (f: (Int -<'1472>-> Int))]-> 
>                                    [
>                                      Cons Int <rec>,
>                                      Nil
>                                      ] = \xs ->
>   let go2: [Cons Int <rec>, Nil]
>              -[go1 (f: (Int -<'1472>-> Int))]-> [Cons Int <rec>, Nil] =
>     go1
>   in
>   go2 xs
> let clos1(f: Int -<'1560>-> [A Int]): [Cons Int <rec>, Nil]
>                                         -[clos1 (f: (Int -<'1560>-> [A Int]))]-> 
>                                         [
>                                           Cons [A Int] <rec>,
>                                           Nil
>                                           ] = \xs ->
>   let go4: [Cons Int <rec>, Nil]
>              -[go3 (f: (Int -<'1560>-> [A Int]))]-> [Cons [A Int] <rec>, Nil] =
>     go3
>   in
>   go4 xs
> let map2: Int -<'1673>-> Int
>             -[map2]-> [Cons Int <rec>, Nil]
>                         -[clos (f: (Int -<'1673>-> Int))]-> [
>                                                               Cons Int <rec>,
>                                                               Nil
>                                                               ] = \f ->
>   clos
> run main2: [Cons Int <rec>, Nil] =
>   (map2 f4) (Cons 1 (Cons 2 (Nil )))
> let map1: Int -<'1848>-> [A Int]
>             -[map1]-> [Cons Int <rec>, Nil]
>                         -[clos1 (f: (Int -<'1848>-> [A Int]))]-> [
>                                                                    Cons 
>                                                                     [A Int]
>                                                                     <rec>,
>                                                                    Nil
>                                                                    ] = \f ->
>   clos1
> let f3: Int -[f3]-> [A Int] = \x1 ->
>   A x1
> run main1: [Cons [A Int] <rec>, Nil] =
>   (map1 f3) (Cons 1 (Cons 2 (Nil )))

> cor-out +lambdamono -print
> fn go6(xs1: [Cons Int <rec>, Nil], captures13: {f: [F4]}): [
>                                                              Cons Int <rec>,
>                                                              Nil
>                                                              ] =
>   let f: [F4] = captures13.f in
>   when xs1 is
>     | Nil -> Nil
>     | Cons x xs2 ->
>       Cons
>         when f is
>           | F4 -> f5(x)
>         end
>         
>         when Go1 {f: f} is
>           | Go1 captures12 -> go6(xs2, captures12)
>         end
>   end
> fn go5(xs1: [Cons Int <rec>, Nil], captures8: {f: [F3]}): [
>                                                             Cons [A Int] <rec>,
>                                                             Nil
>                                                             ] =
>   let f: [F3] = captures8.f in
>   when xs1 is
>     | Nil -> Nil
>     | Cons x xs2 ->
>       Cons
>         when f is
>           | F3 -> f6(x)
>         end
>         
>         when Go3 {f: f} is
>           | Go3 captures7 -> go5(xs2, captures7)
>         end
>   end
> fn clos3(xs: [Cons Int <rec>, Nil], captures5: {f: [F3]}): [
>                                                              Cons [A Int] <rec>,
>                                                              Nil
>                                                              ] =
>   let f: [F3] = captures5.f in
>   let go4: [Go3 {f: [F3]}] = Go3 {f: f} in
>   when go4 is
>     | Go3 captures4 -> go5(xs, captures4)
>   end
> fn f6(x1: Int): [A Int] =
>   A x1
> fn map4(f: [F3]): [Clos1 {f: [F3]}] =
>   Clos1 {f: f}
> fn clos2(xs: [Cons Int <rec>, Nil], captures10: {f: [F4]}): [
>                                                               Cons Int <rec>,
>                                                               Nil
>                                                               ] =
>   let f: [F4] = captures10.f in
>   let go2: [Go1 {f: [F4]}] = Go1 {f: f} in
>   when go2 is
>     | Go1 captures9 -> go6(xs, captures9)
>   end
> fn f5(x2: Int): Int =
>   ~add x2, 1
> fn map3(f: [F4]): [Clos {f: [F4]}] =
>   Clos {f: f}
> run main2: [Cons Int <rec>, Nil] =
>   when when Map2 is
>          | Map2 -> map3(F4)
>        end is
>     | Clos captures1 -> clos2(Cons 1 (Cons 2 (Nil)), captures1)
>   end
> run main1: [Cons [A Int] <rec>, Nil] =
>   when when Map1 is
>          | Map1 -> map4(F3)
>        end is
>     | Clos1 captures3 -> clos3(Cons 1 (Cons 2 (Nil)), captures3)
>   end

> cor-out +ir -print
> fn go6(xs1: Box!a([ `0 { int, !a }, `1 {} ]), captures13: { [ `0 {} ] }):
>   Box!a([ `0 { int, !a }, `1 {} ])
> {
>   let f: [ `0 {} ] = @get_struct_field<captures13, 0>;
>   let discr: int = @get_union_id<xs1>;
>   switch discr {
>   0 -> {
>     let payload: { int, Box!a([ `0 { int, !a }, `1 {} ]) }
>       = @get_union_struct<xs1>;
>     let x: int = @get_struct_field<payload, 0>;
>     let xs2: Box!a([ `0 { int, !a }, `1 {} ]) = @get_struct_field<payload, 1>;
>     let discr1: int = @get_union_id<f>;
>     switch discr1 {
>     0 -> {
>       @call_direct(f5, x)
>     }
>     } in join join;
>     let var: { [ `0 {} ] } = @make_struct{ f };
>     let struct2: { { [ `0 {} ] } } = @make_struct{ var };
>     let var1: [ `0 { { [ `0 {} ] } } ] = @make_union<0, struct2>;
>     let discr2: int = @get_union_id<var1>;
>     switch discr2 {
>     0 -> {
>       let payload1: { { [ `0 {} ] } } = @get_union_struct<var1>;
>       let captures12: { [ `0 {} ] } = @get_struct_field<payload1, 0>;
>       @call_direct(go6, xs2, captures12)
>     }
>     } in join join1;
>     let struct1: { int, Box!a([ `0 { int, !a }, `1 {} ]) }
>       = @make_struct{ join, join1 };
>     @make_union<0, struct1>
>   }
>   1 -> {
>     let struct: {} = @make_struct{};
>     @make_union<1, struct>
>   }
>   } in join join2;
>   return join2;
> }
> 
> fn go5(xs1: Box!a([ `0 { int, !a }, `1 {} ]), captures8: { [ `0 {} ] }):
>   Box!a([ `0 { [ `0 { int } ], !a }, `1 {} ])
> {
>   let f: [ `0 {} ] = @get_struct_field<captures8, 0>;
>   let discr3: int = @get_union_id<xs1>;
>   switch discr3 {
>   0 -> {
>     let payload2: { int, Box!a([ `0 { int, !a }, `1 {} ]) }
>       = @get_union_struct<xs1>;
>     let x: int = @get_struct_field<payload2, 0>;
>     let xs2: Box!a([ `0 { int, !a }, `1 {} ]) = @get_struct_field<payload2, 1>;
>     let discr4: int = @get_union_id<f>;
>     switch discr4 {
>     0 -> {
>       @call_direct(f6, x)
>     }
>     } in join join3;
>     let var2: { [ `0 {} ] } = @make_struct{ f };
>     let struct5: { { [ `0 {} ] } } = @make_struct{ var2 };
>     let var3: [ `0 { { [ `0 {} ] } } ] = @make_union<0, struct5>;
>     let discr5: int = @get_union_id<var3>;
>     switch discr5 {
>     0 -> {
>       let payload3: { { [ `0 {} ] } } = @get_union_struct<var3>;
>       let captures7: { [ `0 {} ] } = @get_struct_field<payload3, 0>;
>       @call_direct(go5, xs2, captures7)
>     }
>     } in join join4;
>     let struct4:
>           { [ `0 { int } ], Box!a([ `0 { [ `0 { int } ], !a }, `1 {} ]) }
>       = @make_struct{ join3, join4 };
>     @make_union<0, struct4>
>   }
>   1 -> {
>     let struct3: {} = @make_struct{};
>     @make_union<1, struct3>
>   }
>   } in join join5;
>   return join5;
> }
> 
> fn clos3(xs: Box!a([ `0 { int, !a }, `1 {} ]), captures5: { [ `0 {} ] }):
>   Box!a([ `0 { [ `0 { int } ], !a }, `1 {} ])
> {
>   let f: [ `0 {} ] = @get_struct_field<captures5, 0>;
>   let var4: { [ `0 {} ] } = @make_struct{ f };
>   let struct6: { { [ `0 {} ] } } = @make_struct{ var4 };
>   let go4: [ `0 { { [ `0 {} ] } } ] = @make_union<0, struct6>;
>   let discr6: int = @get_union_id<go4>;
>   switch discr6 {
>   0 -> {
>     let payload4: { { [ `0 {} ] } } = @get_union_struct<go4>;
>     let captures4: { [ `0 {} ] } = @get_struct_field<payload4, 0>;
>     @call_direct(go5, xs, captures4)
>   }
>   } in join join6;
>   return join6;
> }
> 
> fn f6(x1: int): [ `0 { int } ]
> {
>   let struct7: { int } = @make_struct{ x1 };
>   let var5: [ `0 { int } ] = @make_union<0, struct7>;
>   return var5;
> }
> 
> fn map4(f: [ `0 {} ]): [ `0 { { [ `0 {} ] } } ]
> {
>   let var6: { [ `0 {} ] } = @make_struct{ f };
>   let struct8: { { [ `0 {} ] } } = @make_struct{ var6 };
>   let var7: [ `0 { { [ `0 {} ] } } ] = @make_union<0, struct8>;
>   return var7;
> }
> 
> fn clos2(xs: Box!a([ `0 { int, !a }, `1 {} ]), captures10: { [ `0 {} ] }):
>   Box!a([ `0 { int, !a }, `1 {} ])
> {
>   let f: [ `0 {} ] = @get_struct_field<captures10, 0>;
>   let var8: { [ `0 {} ] } = @make_struct{ f };
>   let struct9: { { [ `0 {} ] } } = @make_struct{ var8 };
>   let go2: [ `0 { { [ `0 {} ] } } ] = @make_union<0, struct9>;
>   let discr7: int = @get_union_id<go2>;
>   switch discr7 {
>   0 -> {
>     let payload5: { { [ `0 {} ] } } = @get_union_struct<go2>;
>     let captures9: { [ `0 {} ] } = @get_struct_field<payload5, 0>;
>     @call_direct(go6, xs, captures9)
>   }
>   } in join join7;
>   return join7;
> }
> 
> fn f5(x2: int): int
> {
>   let var9: int = 1;
>   let var10: int = @call_kfn(add, x2, var9);
>   return var10;
> }
> 
> fn map3(f: [ `0 {} ]): [ `0 { { [ `0 {} ] } } ]
> {
>   let var11: { [ `0 {} ] } = @make_struct{ f };
>   let struct10: { { [ `0 {} ] } } = @make_struct{ var11 };
>   let var12: [ `0 { { [ `0 {} ] } } ] = @make_union<0, struct10>;
>   return var12;
> }
> 
> fn main2_thunk(): Box!a([ `0 { int, !a }, `1 {} ])
> {
>   let struct11: {} = @make_struct{};
>   let var13: [ `0 {} ] = @make_union<0, struct11>;
>   let discr8: int = @get_union_id<var13>;
>   switch discr8 {
>   0 -> {
>     let struct12: {} = @make_struct{};
>     let var14: [ `0 {} ] = @make_union<0, struct12>;
>     @call_direct(map3, var14)
>   }
>   } in join join8;
>   let discr9: int = @get_union_id<join8>;
>   switch discr9 {
>   0 -> {
>     let payload6: { { [ `0 {} ] } } = @get_union_struct<join8>;
>     let captures1: { [ `0 {} ] } = @get_struct_field<payload6, 0>;
>     let var15: int = 1;
>     let var16: int = 2;
>     let struct15: {} = @make_struct{};
>     let var17: [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {} ]
>       = @make_union<1, struct15>;
>     let struct14:
>           { int, [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {} ] }
>       = @make_struct{ var16, var17 };
>     let var18: [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {} ]
>       = @make_union<0, struct14>;
>     let struct13:
>           { int, [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {} ] }
>       = @make_struct{ var15, var18 };
>     let var19: Box!a([ `0 { int, !a }, `1 {} ]) = @make_union<0, struct13>;
>     @call_direct(clos2, var19, captures1)
>   }
>   } in join join9;
>   return join9;
> }
> 
> entry main2: Box!a([ `0 { int, !a }, `1 {} ]) = @call_direct(main2_thunk);
> 
> fn main1_thunk(): Box!a([ `0 { [ `0 { int } ], !a }, `1 {} ])
> {
>   let struct16: {} = @make_struct{};
>   let var20: [ `0 {} ] = @make_union<0, struct16>;
>   let discr10: int = @get_union_id<var20>;
>   switch discr10 {
>   0 -> {
>     let struct17: {} = @make_struct{};
>     let var21: [ `0 {} ] = @make_union<0, struct17>;
>     @call_direct(map4, var21)
>   }
>   } in join join10;
>   let discr11: int = @get_union_id<join10>;
>   switch discr11 {
>   0 -> {
>     let payload7: { { [ `0 {} ] } } = @get_union_struct<join10>;
>     let captures3: { [ `0 {} ] } = @get_struct_field<payload7, 0>;
>     let var22: int = 1;
>     let var23: int = 2;
>     let struct20: {} = @make_struct{};
>     let var24: [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {} ]
>       = @make_union<1, struct20>;
>     let struct19:
>           { int, [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {} ] }
>       = @make_struct{ var23, var24 };
>     let var25: [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {} ]
>       = @make_union<0, struct19>;
>     let struct18:
>           { int, [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {} ] }
>       = @make_struct{ var22, var25 };
>     let var26: Box!a([ `0 { int, !a }, `1 {} ]) = @make_union<0, struct18>;
>     @call_direct(clos3, var26, captures3)
>   }
>   } in join join11;
>   return join11;
> }
> 
> entry main1:
>   Box!a([ `0 { [ `0 { int } ], !a }, `1 {} ])
>   = @call_direct(main1_thunk);

> cor-out +eval -print
> main2 = [0 2 [0 3 [1]]]
>       > Cons 2 (Cons 3 (Nil ))
> main1 = [0 [0 1] [0 [0 2] [1]]]
>       > Cons (A 1) (Cons (A 2) (Nil ))