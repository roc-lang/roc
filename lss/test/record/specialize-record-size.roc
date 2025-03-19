# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

let f = \x -> x.a;;

run main1 = f { a: 1, b: 2 };;
run main2 = f { a: "main2" };;

> cor-out +monotype -print
> let f2: {a: Str} -> Str = \x ->
>   x .a
> let f1: {a: Int, b: Int} -> Int = \x ->
>   x .a
> run main1: Int =
>   f1 {a: 1, b: 2}
> run main2: Str =
>   f2 {a: "main2"}

> cor-out +monotype_lifted -print
> let f2: {a: Str} -> Str = \x ->
>   x.a
> let f1: {a: Int, b: Int} -> Int = \x ->
>   x.a
> run main1: Int =
>   f1 {a: 1, b: 2}
> run main2: Str =
>   f2 {a: "main2"}

> cor-out +lambdasolved -print
> let f2: {a: Str} -[f2]-> Str = \x ->
>   x.a
> run main2: Str =
>   f2 {a: "main2"}
> let f1: {a: Int, b: Int} -[f1]-> Int = \x ->
>   x.a
> run main1: Int =
>   f1 {a: 1, b: 2}

> cor-out +lambdamono -print
> fn f4(x: {a: Int, b: Int}): Int =
>   x.a
> fn f3(x: {a: Str}): Str =
>   x.a
> run main2: Str =
>   when F2 is
>     | F2 -> f3({a: "main2"})
>   end
> run main1: Int =
>   when F1 is
>     | F1 -> f4({a: 1, b: 2})
>   end

> cor-out +ir -print
> fn f4(x: { int, int }): int
> {
>   let var: int = @get_struct_field<x, 0>;
>   return var;
> }
> 
> fn f3(x: { str }): str
> {
>   let var1: str = @get_struct_field<x, 0>;
>   return var1;
> }
> 
> fn main2_thunk(): str
> {
>   let struct: {} = @make_struct{};
>   let var2: [ `0 {} ] = @make_union<0, struct>;
>   let discr: int = @get_union_id<var2>;
>   switch discr {
>   0 -> {
>     let var3: str = "main2";
>     let var4: { str } = @make_struct{ var3 };
>     @call_direct(f3, var4)
>   }
>   } in join join;
>   return join;
> }
> 
> entry main2: str = @call_direct(main2_thunk);
> 
> fn main1_thunk(): int
> {
>   let struct1: {} = @make_struct{};
>   let var5: [ `0 {} ] = @make_union<0, struct1>;
>   let discr1: int = @get_union_id<var5>;
>   switch discr1 {
>   0 -> {
>     let var6: int = 1;
>     let var7: int = 2;
>     let var8: { int, int } = @make_struct{ var6, var7 };
>     @call_direct(f4, var8)
>   }
>   } in join join1;
>   return join1;
> }
> 
> entry main1: int = @call_direct(main1_thunk);

> cor-out +eval -print
> main2 = [109 97 105 110 50]
>       > "main2"
> main1 = 1
>       > 1