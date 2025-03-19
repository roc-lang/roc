# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

let f = \x ->
  let g = \t -> when t is
    | T -> x + 1
    | F -> g T
  end
  in g
;;

run main = f 2 F
;;

> cor-out +monotype -print
> let f1: Int -> [F, T] -> Int = \x ->
>   let rec g: [F, T] -> Int = \t ->
>     when t is
>       | T -> ~add x 1
>       | F -> g (T )
>     end
>   in
>   g
> run main: Int =
>   (f1 2) (F )

> cor-out +monotype_lifted -print
> let g1(x: Int): [F, T] -> Int = \t ->
>   when t is
>     | T -> ~add x 1
>     | F -> g1 (T )
>   end
> let f1: Int -> [F, T] -> Int = \x ->
>   let g2: [F, T] -> Int =
>     g1 in
>   g2
> run main: Int =
>   (f1 2) (F )

> cor-out +lambdasolved -print
> let g1(x: Int): [F, T] -[g1 (x: Int)]-> Int = \t ->
>   when t is
>     | T -> ~add x 1
>     | F -> g1 (T )
>   end
> let f1: Int -[f1]-> [F, T] -[g1 (x: Int)]-> Int = \x ->
>   let g2: [F, T] -[g1 (x: Int)]-> Int =
>     g1 in
>   g2
> run main: Int =
>   (f1 2) (F )

> cor-out +lambdamono -print
> fn g3(t: [F, T], captures3: {x: Int}): Int =
>   let x: Int = captures3.x in
>   when t is
>     | T -> ~add x, 1
>     | F -> when G1 {x: x} is
>              | G1 captures2 -> g3(T, captures2)
>            end
>   end
> fn f2(x: Int): [G1 {x: Int}] =
>   let g2: [G1 {x: Int}] = G1 {x: x} in
>   g2
> run main: Int =
>   when when F1 is
>          | F1 -> f2(2)
>        end is
>     | G1 captures1 -> g3(F, captures1)
>   end

> cor-out +ir -print
> fn g3(t: [ `0 {}, `1 {} ], captures3: { int }): int
> {
>   let x: int = @get_struct_field<captures3, 0>;
>   let discr: int = @get_union_id<t>;
>   switch discr {
>   0 -> {
>     let var1: { int } = @make_struct{ x };
>     let struct: { { int } } = @make_struct{ var1 };
>     let var2: [ `0 { { int } } ] = @make_union<0, struct>;
>     let discr1: int = @get_union_id<var2>;
>     switch discr1 {
>     0 -> {
>       let payload: { { int } } = @get_union_struct<var2>;
>       let captures2: { int } = @get_struct_field<payload, 0>;
>       let struct1: {} = @make_struct{};
>       let var3: [ `0 {}, `1 {} ] = @make_union<1, struct1>;
>       @call_direct(g3, var3, captures2)
>     }
>     } in join join;
>     join
>   }
>   1 -> {
>     let var: int = 1;
>     @call_kfn(add, x, var)
>   }
>   } in join join1;
>   return join1;
> }
> 
> fn f2(x: int): [ `0 { { int } } ]
> {
>   let var4: { int } = @make_struct{ x };
>   let struct2: { { int } } = @make_struct{ var4 };
>   let g2: [ `0 { { int } } ] = @make_union<0, struct2>;
>   return g2;
> }
> 
> fn main_thunk(): int
> {
>   let struct3: {} = @make_struct{};
>   let var5: [ `0 {} ] = @make_union<0, struct3>;
>   let discr2: int = @get_union_id<var5>;
>   switch discr2 {
>   0 -> {
>     let var6: int = 2;
>     @call_direct(f2, var6)
>   }
>   } in join join2;
>   let discr3: int = @get_union_id<join2>;
>   switch discr3 {
>   0 -> {
>     let payload1: { { int } } = @get_union_struct<join2>;
>     let captures1: { int } = @get_struct_field<payload1, 0>;
>     let struct4: {} = @make_struct{};
>     let var7: [ `0 {}, `1 {} ] = @make_union<0, struct4>;
>     @call_direct(g3, var7, captures1)
>   }
>   } in join join3;
>   return join3;
> }
> 
> entry main: int = @call_direct(main_thunk);

> cor-out +eval -print
> main = 3
>      > 3