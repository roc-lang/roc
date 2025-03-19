# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

let poly = \x ->
  let f = \x -> x in
  f x
;;

run main =
  A (poly 1) (poly "")
;;

> cor-out +monotype -print
> let poly2: Str -> Str = \x ->
>   let f: Str -> Str = \x1 ->
>     x1 in
>   f x
> let poly1: Int -> Int = \x ->
>   let f: Int -> Int = \x1 ->
>     x1 in
>   f x
> run main: [A Int Str] =
>   A (poly1 1) (poly2 "")

> cor-out +monotype_lifted -print
> let f1: Str -> Str = \x1 ->
>   x1
> let poly2: Str -> Str = \x ->
>   f1 x
> let f2: Int -> Int = \x1 ->
>   x1
> let poly1: Int -> Int = \x ->
>   f2 x
> run main: [A Int Str] =
>   A (poly1 1) (poly2 "")

> cor-out +lambdasolved -print
> let f1: Str -[f1]-> Str = \x1 ->
>   x1
> let poly2: Str -[poly2]-> Str = \x ->
>   f1 x
> let f2: Int -[f2]-> Int = \x1 ->
>   x1
> let poly1: Int -[poly1]-> Int = \x ->
>   f2 x
> run main: [A Int Str] =
>   A (poly1 1) (poly2 "")

> cor-out +lambdamono -print
> fn f4(x1: Int): Int =
>   x1
> fn f3(x1: Str): Str =
>   x1
> fn poly4(x: Str): Str =
>   when F1 is
>     | F1 -> f3(x)
>   end
> fn poly3(x: Int): Int =
>   when F2 is
>     | F2 -> f4(x)
>   end
> run main: [A Int Str] =
>   A when Poly1 is
>       | Poly1 -> poly3(1)
>     end when Poly2 is
>           | Poly2 -> poly4("")
>         end

> cor-out +ir -print
> fn f4(x1: int): int
> {
>   return x1;
> }
> 
> fn f3(x1: str): str
> {
>   return x1;
> }
> 
> fn poly4(x: str): str
> {
>   let struct: {} = @make_struct{};
>   let var: [ `0 {} ] = @make_union<0, struct>;
>   let discr: int = @get_union_id<var>;
>   switch discr {
>   0 -> {
>     @call_direct(f3, x)
>   }
>   } in join join;
>   return join;
> }
> 
> fn poly3(x: int): int
> {
>   let struct1: {} = @make_struct{};
>   let var1: [ `0 {} ] = @make_union<0, struct1>;
>   let discr1: int = @get_union_id<var1>;
>   switch discr1 {
>   0 -> {
>     @call_direct(f4, x)
>   }
>   } in join join1;
>   return join1;
> }
> 
> fn main_thunk(): [ `0 { int, str } ]
> {
>   let struct3: {} = @make_struct{};
>   let var2: [ `0 {} ] = @make_union<0, struct3>;
>   let discr2: int = @get_union_id<var2>;
>   switch discr2 {
>   0 -> {
>     let var3: int = 1;
>     @call_direct(poly3, var3)
>   }
>   } in join join2;
>   let struct4: {} = @make_struct{};
>   let var4: [ `0 {} ] = @make_union<0, struct4>;
>   let discr3: int = @get_union_id<var4>;
>   switch discr3 {
>   0 -> {
>     let var5: str = "";
>     @call_direct(poly4, var5)
>   }
>   } in join join3;
>   let struct2: { int, str } = @make_struct{ join2, join3 };
>   let var6: [ `0 { int, str } ] = @make_union<0, struct2>;
>   return var6;
> }
> 
> entry main: [ `0 { int, str } ] = @call_direct(main_thunk);

> cor-out +eval -print
> main = [0 1 []]
>      > A 1 ""