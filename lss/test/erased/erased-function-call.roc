# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

run main1 =
    let x = 3 in
    let id = \z -> x + z in
    let g = \y -> id (x + y) in
    let g_erased = ~erase g in
    let g_unerased = ~unerase g_erased in
    when A is
        | A -> g_unerased 2
        | B -> g 2
    end
;;

> cor-out +monotype -print
> run main1: Int =
>   let x: Int =
>     3 in
>   let id: Int -> Int = \z ->
>     ~add x z in
>   let g: Int -> Int = \y ->
>     id ~add x y in
>   let g_erased: Erased =
>     ~erase g in
>   let g_unerased: Int -> Int =
>     ~unerase g_erased in
>   when A  is
>     | A -> g_unerased 2
>     | B -> g 2
>   end

> cor-out +monotype_lifted -print
> let g1(id2: Int -> Int x: Int): Int -> Int = \y ->
>   id2 ~add x y
> let id1(x: Int): Int -> Int = \z ->
>   ~add x z
> run main1: Int =
>   let x: Int =
>     3 in
>   let id2: Int -> Int =
>     id1 in
>   let g2: Int -> Int =
>     g1 in
>   let g_erased: Erased =
>     ~erase g2 in
>   let g_unerased: Int -> Int =
>     ~unerase g_erased in
>   when A  is
>     | A -> g_unerased 2
>     | B -> g2 2
>   end

> cor-out +lambdasolved -print
> let id1(x: Int): Int -[id1 (x: Int)]-> Int = \z ->
>   ~add x z
> let g1(id2: Int -<'1108>-> Int, x: Int): Int
>                                            -[
>                                               g1 (id2: (Int -<'1108>-> Int))
>                                                 (x: Int)
>                                               ]-> Int = \y ->
>   id2 ~add x y
> run main1: Int =
>   let x: Int =
>     3 in
>   let id2: Int -[id1 (x: Int)]-> Int =
>     id1 in
>   let g2: Int -Erased-> Int =
>     g1 in
>   let g_erased: Erased =
>     ~erase g2 in
>   let g_unerased: Int -Erased-> Int =
>     ~unerase g_erased in
>   when A  is
>     | A -> g_unerased 2
>     | B -> g2 2
>   end

> cor-out +lambdamono -print
> fn g3(y: Int, captures1: {id2: [Id1 {x: Int}], x: Int}): Int =
>   let x: Int = captures1.x in
>   let id2: [Id1 {x: Int}] = captures1.id2 in
>   when id2 is
>     | Id1 captures -> id3(~add x, y, captures)
>   end
> fn id3(z: Int, captures2: {x: Int}): Int =
>   let x: Int = captures2.x in
>   ~add x, z
> run main1: Int =
>   let x: Int = 3 in
>   let id2: [Id1 {x: Int}] = Id1 {x: x} in
>   let g2: Erased = PackedFn(g3, {id2: id2, x: x}) in
>   let g_erased: Erased = ~erase g2 in
>   let g_unerased: Erased = ~unerase g_erased in
>   when A is
>     | A -> g_unerased(2)
>     | B -> g2(2)
>   end

> cor-out +ir -print
> fn g3(y: int, captures1: { [ `0 { { int } } ], int }): int
> {
>   let x: int = @get_struct_field<captures1, 1>;
>   let id2: [ `0 { { int } } ] = @get_struct_field<captures1, 0>;
>   let discr: int = @get_union_id<id2>;
>   switch discr {
>   0 -> {
>     let payload: { { int } } = @get_union_struct<id2>;
>     let captures: { int } = @get_struct_field<payload, 0>;
>     let var: int = @call_kfn(add, x, y);
>     @call_direct(id3, var, captures)
>   }
>   } in join join;
>   return join;
> }
> 
> fn id3(z: int, captures2: { int }): int
> {
>   let x: int = @get_struct_field<captures2, 0>;
>   let var1: int = @call_kfn(add, x, z);
>   return var1;
> }
> 
> fn main1_thunk(): int
> {
>   let x: int = 3;
>   let var2: { int } = @make_struct{ x };
>   let struct: { { int } } = @make_struct{ var2 };
>   let id2: [ `0 { { int } } ] = @make_union<0, struct>;
>   let fn: *opaque = @fn<g3>;
>   let var3: { [ `0 { { int } } ], int } = @make_struct{ id2, x };
>   let g2: *opaque = @make_struct{ fn, var3 };
>   let g_erased: *opaque = @call_kfn(erase, g2);
>   let g_unerased: *opaque = @call_kfn(unerase, g_erased);
>   let struct1: {} = @make_struct{};
>   let var4: [ `0 {}, `1 {} ] = @make_union<0, struct1>;
>   let discr1: int = @get_union_id<var4>;
>   switch discr1 {
>   0 -> {
>     let var5: int = 2;
>     @call_indirect(g_unerased, var5)
>   }
>   1 -> {
>     let var6: int = 2;
>     @call_indirect(g2, var6)
>   }
>   } in join join1;
>   return join1;
> }
> 
> entry main1: int = @call_direct(main1_thunk);

> cor-out +eval -print
> main1 = 8
>       > 8