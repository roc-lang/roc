# cor +canonicalize -print
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

let f = \t ->
  when t is
    | A -> \w -> w
    | B y -> \w -> w + y
    | C y z -> \w -> w + y + z
  end
;;

run main =
  let adder = f (B 10) in
  adder 12
;;

> cor-out +canonicalize -print
> let f = \t ->
>   when t is
>     | A -> \w -> w
>     | B y -> \w1 -> ~add w1 y
>     | C y1 z -> \w2 -> ~add w2 ~add y1 z
>   end
> let main =
>   let adder =
>     f (B 10) in
>   adder 12

> cor-out +monotype -print
> let f1: [A, B Int, C Int Int] -> Int -> Int = \t ->
>   when t is
>     | A -> \w -> w
>     | B y -> \w1 -> ~add w1 y
>     | C y1 z -> \w2 -> ~add w2 ~add y1 z
>   end
> run main: Int =
>   let adder: Int -> Int =
>     f1 (B 10) in
>   adder 12

> cor-out +monotype_lifted -print
> let clos: Int -> Int = \w ->
>   w
> let clos1(y: Int): Int -> Int = \w1 ->
>   ~add w1 y
> let clos2(y1: Int z: Int): Int -> Int = \w2 ->
>   ~add w2 ~add y1 z
> let f1: [A, B Int, C Int Int] -> Int -> Int = \t ->
>   when t is
>     | A -> clos
>     | B y -> clos1
>     | C y1 z -> clos2
>   end
> run main: Int =
>   let adder: Int -> Int =
>     f1 (B 10) in
>   adder 12

> cor-out +lambdasolved -print
> let clos2(y1: Int, z: Int): Int
>                               -[clos, clos1 (y: Int), clos2 (y1: Int) (z: Int)]-> Int = \w2 ->
>   ~add w2 ~add y1 z
> let clos1(y: Int): Int -[clos, clos1 (y: Int), clos2 (y1: Int) (z: Int)]-> Int = \w1 ->
>   ~add w1 y
> let clos: Int -[clos, clos1 (y: Int), clos2 (y1: Int) (z: Int)]-> Int = \w ->
>   w
> let f1: [A, B Int, C Int Int]
>           -[f1]-> Int -[clos, clos1 (y: Int), clos2 (y1: Int) (z: Int)]-> Int = \t ->
>   when t is
>     | A -> clos
>     | B y -> clos1
>     | C y1 z -> clos2
>   end
> run main: Int =
>   let adder: Int -[clos, clos1 (y: Int), clos2 (y1: Int) (z: Int)]-> Int =
>     f1 (B 10)
>   in
>   adder 12

> cor-out +lambdamono -print
> fn clos5(w2: Int, captures4: {y1: Int, z: Int}): Int =
>   let z: Int = captures4.z in
>   let y1: Int = captures4.y1 in
>   ~add w2, ~add y1, z
> fn clos4(w1: Int, captures5: {y: Int}): Int =
>   let y: Int = captures5.y in
>   ~add w1, y
> fn clos3(w: Int): Int =
>   w
> fn f2(t: [A, B Int, C Int Int]): [
>                                    Clos,
>                                    Clos1 {y: Int},
>                                    Clos2 {y1: Int, z: Int}
>                                    ] =
>   when t is
>     | A -> Clos
>     | B y -> Clos1 {y: y}
>     | C y1 z -> Clos2 {y1: y1, z: z}
>   end
> run main: Int =
>   let adder: [Clos, Clos1 {y: Int}, Clos2 {y1: Int, z: Int}] =
>     when F1 is
>       | F1 -> f2(B 10)
>     end
>   in
>   when adder is
>     | Clos -> clos3(12)
>     | Clos1 captures2 -> clos4(12, captures2)
>     | Clos2 captures3 -> clos5(12, captures3)
>   end

> cor-out +ir -print
> fn clos5(w2: int, captures4: { int, int }): int
> {
>   let z: int = @get_struct_field<captures4, 1>;
>   let y1: int = @get_struct_field<captures4, 0>;
>   let var: int = @call_kfn(add, y1, z);
>   let var1: int = @call_kfn(add, w2, var);
>   return var1;
> }
> 
> fn clos4(w1: int, captures5: { int }): int
> {
>   let y: int = @get_struct_field<captures5, 0>;
>   let var2: int = @call_kfn(add, w1, y);
>   return var2;
> }
> 
> fn clos3(w: int): int
> {
>   return w;
> }
> 
> fn f2(t: [ `0 {}, `1 { int }, `2 { int, int } ]):
>   [ `0 {}, `1 { { int } }, `2 { { int, int } } ]
> {
>   let discr: int = @get_union_id<t>;
>   switch discr {
>   0 -> {
>     let struct: {} = @make_struct{};
>     @make_union<0, struct>
>   }
>   1 -> {
>     let payload: { int } = @get_union_struct<t>;
>     let y: int = @get_struct_field<payload, 0>;
>     let var3: { int } = @make_struct{ y };
>     let struct1: { { int } } = @make_struct{ var3 };
>     @make_union<1, struct1>
>   }
>   2 -> {
>     let payload1: { int, int } = @get_union_struct<t>;
>     let y1: int = @get_struct_field<payload1, 0>;
>     let z: int = @get_struct_field<payload1, 1>;
>     let var4: { int, int } = @make_struct{ y1, z };
>     let struct2: { { int, int } } = @make_struct{ var4 };
>     @make_union<2, struct2>
>   }
>   } in join join;
>   return join;
> }
> 
> fn main_thunk(): int
> {
>   let struct3: {} = @make_struct{};
>   let var5: [ `0 {} ] = @make_union<0, struct3>;
>   let discr1: int = @get_union_id<var5>;
>   switch discr1 {
>   0 -> {
>     let var6: int = 10;
>     let struct4: { int } = @make_struct{ var6 };
>     let var7: [ `0 {}, `1 { int }, `2 { int, int } ] = @make_union<1, struct4>;
>     @call_direct(f2, var7)
>   }
>   } in join join1;
>   let adder: [ `0 {}, `1 { { int } }, `2 { { int, int } } ] = join1;
>   let discr2: int = @get_union_id<adder>;
>   switch discr2 {
>   0 -> {
>     let var8: int = 12;
>     @call_direct(clos3, var8)
>   }
>   1 -> {
>     let payload2: { { int } } = @get_union_struct<adder>;
>     let captures2: { int } = @get_struct_field<payload2, 0>;
>     let var9: int = 12;
>     @call_direct(clos4, var9, captures2)
>   }
>   2 -> {
>     let payload3: { { int, int } } = @get_union_struct<adder>;
>     let captures3: { int, int } = @get_struct_field<payload3, 0>;
>     let var10: int = 12;
>     @call_direct(clos5, var10, captures3)
>   }
>   } in join join2;
>   return join2;
> }
> 
> entry main: int = @call_direct(main_thunk);

> cor-out +eval -print
> main = 22
>      > 22