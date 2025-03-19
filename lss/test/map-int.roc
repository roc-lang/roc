# cor +canonicalize -print
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

let map = \x ->
  let f = \y -> x + y in
  f 2
;;

run main = map 1;;

> cor-out +canonicalize -print
> let map = \x ->
>   let f = \y ->
>     ~add x y in
>   f 2
> let main =
>   map 1

> cor-out +monotype -print
> let map1: Int -> Int = \x ->
>   let f: Int -> Int = \y ->
>     ~add x y in
>   f 2
> run main: Int =
>   map1 1

> cor-out +monotype_lifted -print
> let f1(x: Int): Int -> Int = \y ->
>   ~add x y
> let map1: Int -> Int = \x ->
>   let f2: Int -> Int =
>     f1 in
>   f2 2
> run main: Int =
>   map1 1

> cor-out +lambdasolved -print
> let f1(x: Int): Int -[f1 (x: Int)]-> Int = \y ->
>   ~add x y
> let map1: Int -[map1]-> Int = \x ->
>   let f2: Int -[f1 (x: Int)]-> Int =
>     f1 in
>   f2 2
> run main: Int =
>   map1 1

> cor-out +lambdamono -print
> fn f3(y: Int, captures2: {x: Int}): Int =
>   let x: Int = captures2.x in
>   ~add x, y
> fn map2(x: Int): Int =
>   let f2: [F1 {x: Int}] = F1 {x: x} in
>   when f2 is
>     | F1 captures1 -> f3(2, captures1)
>   end
> run main: Int =
>   when Map1 is
>     | Map1 -> map2(1)
>   end

> cor-out +ir -print
> fn f3(y: int, captures2: { int }): int
> {
>   let x: int = @get_struct_field<captures2, 0>;
>   let var: int = @call_kfn(add, x, y);
>   return var;
> }
> 
> fn map2(x: int): int
> {
>   let var1: { int } = @make_struct{ x };
>   let struct: { { int } } = @make_struct{ var1 };
>   let f2: [ `0 { { int } } ] = @make_union<0, struct>;
>   let discr: int = @get_union_id<f2>;
>   switch discr {
>   0 -> {
>     let payload: { { int } } = @get_union_struct<f2>;
>     let captures1: { int } = @get_struct_field<payload, 0>;
>     let var2: int = 2;
>     @call_direct(f3, var2, captures1)
>   }
>   } in join join;
>   return join;
> }
> 
> fn main_thunk(): int
> {
>   let struct1: {} = @make_struct{};
>   let var3: [ `0 {} ] = @make_union<0, struct1>;
>   let discr1: int = @get_union_id<var3>;
>   switch discr1 {
>   0 -> {
>     let var4: int = 1;
>     @call_direct(map2, var4)
>   }
>   } in join join1;
>   return join1;
> }
> 
> entry main: int = @call_direct(main_thunk);

> cor-out +eval -print
> main = 3
>      > 3