# cor +solve -elab
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

let f = \x -> \t ->
  let g = when t is
  #   ^
    | T1 -> \y -> x + y + 1
    | T2 -> \y -> x + y + 2
    | T3 -> \y -> x + y + 3
    end
  in g
;;

run x = f 1 T2 0
;;

> cor-out +solve -elab
> 
> let f = \x -> \t ->
>   let g = when t is
> #     ^ Int -> Int
>     | T1 -> \y -> x + y + 1
>     | T2 -> \y -> x + y + 2
>     | T3 -> \y -> x + y + 3
>     end
>   in g
> ;;
> 
> run x = f 1 T2 0
> ;;
> 

> cor-out +monotype -print
> let f1: Int -> [T1, T2, T3] -> Int -> Int = \x ->
>   \t ->
>     (let g: Int -> Int =
>        when t is
>          | T1 -> \y -> ~add x ~add y 1
>          | T2 -> \y1 -> ~add x ~add y1 2
>          | T3 -> \y2 -> ~add x ~add y2 3
>        end
>     in
>     g)
> run x1: Int =
>   ((f1 1) (T2 )) 0

> cor-out +monotype_lifted -print
> let clos(x: Int): Int -> Int = \y ->
>   ~add x ~add y 1
> let clos1(x: Int): Int -> Int = \y1 ->
>   ~add x ~add y1 2
> let clos2(x: Int): Int -> Int = \y2 ->
>   ~add x ~add y2 3
> let clos3(x: Int): [T1, T2, T3] -> Int -> Int = \t ->
>   let g: Int -> Int =
>     when t is
>       | T1 -> clos
>       | T2 -> clos1
>       | T3 -> clos2
>     end in
>   g
> let f1: Int -> [T1, T2, T3] -> Int -> Int = \x ->
>   clos3
> run x1: Int =
>   ((f1 1) (T2 )) 0

> cor-out +lambdasolved -print
> let clos2(x: Int): Int -[clos (x: Int), clos1 (x: Int), clos2 (x: Int)]-> Int = \y2 ->
>   ~add x ~add y2 3
> let clos1(x: Int): Int -[clos (x: Int), clos1 (x: Int), clos2 (x: Int)]-> Int = \y1 ->
>   ~add x ~add y1 2
> let clos(x: Int): Int -[clos (x: Int), clos1 (x: Int), clos2 (x: Int)]-> Int = \y ->
>   ~add x ~add y 1
> let clos3(x: Int): [T1, T2, T3]
>                      -[clos3 (x: Int)]-> Int
>                                            -[
>                                               clos (x: Int),
>                                               clos1 (x: Int),
>                                               clos2 (x: Int)
>                                               ]-> Int = \t ->
>   let g: Int -[clos (x: Int), clos1 (x: Int), clos2 (x: Int)]-> Int =
>     when t is
>       | T1 -> clos
>       | T2 -> clos1
>       | T3 -> clos2
>     end
>   in
>   g
> let f1: Int
>           -[f1]-> [T1, T2, T3]
>                     -[clos3 (x: Int)]-> Int
>                                           -[
>                                              clos (x: Int),
>                                              clos1 (x: Int),
>                                              clos2 (x: Int)
>                                              ]-> Int = \x ->
>   clos3
> run x1: Int =
>   ((f1 1) (T2 )) 0

> cor-out +lambdamono -print
> fn clos7(y2: Int, captures5: {x: Int}): Int =
>   let x: Int = captures5.x in
>   ~add x, ~add y2, 3
> fn clos6(y1: Int, captures6: {x: Int}): Int =
>   let x: Int = captures6.x in
>   ~add x, ~add y1, 2
> fn clos5(y: Int, captures7: {x: Int}): Int =
>   let x: Int = captures7.x in
>   ~add x, ~add y, 1
> fn clos4(t: [T1, T2, T3], captures8: {x: Int}): [
>                                                   Clos {x: Int},
>                                                   Clos1 {x: Int},
>                                                   Clos2 {x: Int}
>                                                   ] =
>   let x: Int = captures8.x in
>   let g: [Clos {x: Int}, Clos1 {x: Int}, Clos2 {x: Int}] =
>     when t is
>       | T1 -> Clos {x: x}
>       | T2 -> Clos1 {x: x}
>       | T3 -> Clos2 {x: x}
>     end
>   in
>   g
> fn f2(x: Int): [Clos3 {x: Int}] =
>   Clos3 {x: x}
> run x1: Int =
>   when when when F1 is
>               | F1 -> f2(1)
>             end is
>          | Clos3 captures1 -> clos4(T2, captures1)
>        end is
>     | Clos captures2 -> clos5(0, captures2)
>     | Clos1 captures3 -> clos6(0, captures3)
>     | Clos2 captures4 -> clos7(0, captures4)
>   end

> cor-out +ir -print
> fn clos7(y2: int, captures5: { int }): int
> {
>   let x: int = @get_struct_field<captures5, 0>;
>   let var: int = 3;
>   let var1: int = @call_kfn(add, y2, var);
>   let var2: int = @call_kfn(add, x, var1);
>   return var2;
> }
> 
> fn clos6(y1: int, captures6: { int }): int
> {
>   let x: int = @get_struct_field<captures6, 0>;
>   let var3: int = 2;
>   let var4: int = @call_kfn(add, y1, var3);
>   let var5: int = @call_kfn(add, x, var4);
>   return var5;
> }
> 
> fn clos5(y: int, captures7: { int }): int
> {
>   let x: int = @get_struct_field<captures7, 0>;
>   let var6: int = 1;
>   let var7: int = @call_kfn(add, y, var6);
>   let var8: int = @call_kfn(add, x, var7);
>   return var8;
> }
> 
> fn clos4(t: [ `0 {}, `1 {}, `2 {} ], captures8: { int }):
>   [ `0 { { int } }, `1 { { int } }, `2 { { int } } ]
> {
>   let x: int = @get_struct_field<captures8, 0>;
>   let discr: int = @get_union_id<t>;
>   switch discr {
>   0 -> {
>     let var9: { int } = @make_struct{ x };
>     let struct: { { int } } = @make_struct{ var9 };
>     @make_union<0, struct>
>   }
>   1 -> {
>     let var10: { int } = @make_struct{ x };
>     let struct1: { { int } } = @make_struct{ var10 };
>     @make_union<1, struct1>
>   }
>   2 -> {
>     let var11: { int } = @make_struct{ x };
>     let struct2: { { int } } = @make_struct{ var11 };
>     @make_union<2, struct2>
>   }
>   } in join join;
>   let g: [ `0 { { int } }, `1 { { int } }, `2 { { int } } ] = join;
>   return g;
> }
> 
> fn f2(x: int): [ `0 { { int } } ]
> {
>   let var12: { int } = @make_struct{ x };
>   let struct3: { { int } } = @make_struct{ var12 };
>   let var13: [ `0 { { int } } ] = @make_union<0, struct3>;
>   return var13;
> }
> 
> fn x1_thunk(): int
> {
>   let struct4: {} = @make_struct{};
>   let var14: [ `0 {} ] = @make_union<0, struct4>;
>   let discr1: int = @get_union_id<var14>;
>   switch discr1 {
>   0 -> {
>     let var15: int = 1;
>     @call_direct(f2, var15)
>   }
>   } in join join1;
>   let discr2: int = @get_union_id<join1>;
>   switch discr2 {
>   0 -> {
>     let payload: { { int } } = @get_union_struct<join1>;
>     let captures1: { int } = @get_struct_field<payload, 0>;
>     let struct5: {} = @make_struct{};
>     let var16: [ `0 {}, `1 {}, `2 {} ] = @make_union<1, struct5>;
>     @call_direct(clos4, var16, captures1)
>   }
>   } in join join2;
>   let discr3: int = @get_union_id<join2>;
>   switch discr3 {
>   0 -> {
>     let payload1: { { int } } = @get_union_struct<join2>;
>     let captures2: { int } = @get_struct_field<payload1, 0>;
>     let var17: int = 0;
>     @call_direct(clos5, var17, captures2)
>   }
>   1 -> {
>     let payload2: { { int } } = @get_union_struct<join2>;
>     let captures3: { int } = @get_struct_field<payload2, 0>;
>     let var18: int = 0;
>     @call_direct(clos6, var18, captures3)
>   }
>   2 -> {
>     let payload3: { { int } } = @get_union_struct<join2>;
>     let captures4: { int } = @get_struct_field<payload3, 0>;
>     let var19: int = 0;
>     @call_direct(clos7, var19, captures4)
>   }
>   } in join join3;
>   return join3;
> }
> 
> entry x1: int = @call_direct(x1_thunk);

> cor-out +eval -print
> x1 = 3
>    > 3