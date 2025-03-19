# cor +solve -elab
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

let f1 = \x -> x + 1;;
#   ^^
let f2 = \x -> x + 2;;
#   ^^
let f3 = \x -> x + 3;;
#   ^^

let f = \t -> when t is
  | T1 -> f1
  | T2 -> f2
  | T3 -> f3
  end
;;

run x = f T2 0
;;

> cor-out +solve -elab
> 
> let f1 = \x -> x + 1;;
> #   ^^ Int -> Int
> let f2 = \x -> x + 2;;
> #   ^^ Int -> Int
> let f3 = \x -> x + 3;;
> #   ^^ Int -> Int
> 
> let f = \t -> when t is
>   | T1 -> f1
>   | T2 -> f2
>   | T3 -> f3
>   end
> ;;
> 
> run x = f T2 0
> ;;
> 

> cor-out +monotype -print
> let f31: Int -> Int = \x2 ->
>   ~add x2 3
> let f21: Int -> Int = \x1 ->
>   ~add x1 2
> let f11: Int -> Int = \x ->
>   ~add x 1
> let f4: [T1, T2, T3] -> Int -> Int = \t ->
>   when t is
>     | T1 -> f11
>     | T2 -> f21
>     | T3 -> f31
>   end
> run x3: Int =
>   (f4 (T2 )) 0

> cor-out +monotype_lifted -print
> let f31: Int -> Int = \x2 ->
>   ~add x2 3
> let f21: Int -> Int = \x1 ->
>   ~add x1 2
> let f11: Int -> Int = \x ->
>   ~add x 1
> let f4: [T1, T2, T3] -> Int -> Int = \t ->
>   when t is
>     | T1 -> f11
>     | T2 -> f21
>     | T3 -> f31
>   end
> run x3: Int =
>   (f4 (T2 )) 0

> cor-out +lambdasolved -print
> let f31: Int -[f11, f21, f31]-> Int = \x2 ->
>   ~add x2 3
> let f21: Int -[f11, f21, f31]-> Int = \x1 ->
>   ~add x1 2
> let f11: Int -[f11, f21, f31]-> Int = \x ->
>   ~add x 1
> let f4: [T1, T2, T3] -[f4]-> Int -[f11, f21, f31]-> Int = \t ->
>   when t is
>     | T1 -> f11
>     | T2 -> f21
>     | T3 -> f31
>   end
> run x3: Int =
>   (f4 (T2 )) 0

> cor-out +lambdamono -print
> fn f32(x2: Int): Int =
>   ~add x2, 3
> fn f22(x1: Int): Int =
>   ~add x1, 2
> fn f12(x: Int): Int =
>   ~add x, 1
> fn f5(t: [T1, T2, T3]): [F11, F21, F31] =
>   when t is
>     | T1 -> F11
>     | T2 -> F21
>     | T3 -> F31
>   end
> run x3: Int =
>   when when F4 is
>          | F4 -> f5(T2)
>        end is
>     | F11 -> f12(0)
>     | F21 -> f22(0)
>     | F31 -> f32(0)
>   end

> cor-out +ir -print
> fn f32(x2: int): int
> {
>   let var: int = 3;
>   let var1: int = @call_kfn(add, x2, var);
>   return var1;
> }
> 
> fn f22(x1: int): int
> {
>   let var2: int = 2;
>   let var3: int = @call_kfn(add, x1, var2);
>   return var3;
> }
> 
> fn f12(x: int): int
> {
>   let var4: int = 1;
>   let var5: int = @call_kfn(add, x, var4);
>   return var5;
> }
> 
> fn f5(t: [ `0 {}, `1 {}, `2 {} ]): [ `0 {}, `1 {}, `2 {} ]
> {
>   let discr: int = @get_union_id<t>;
>   switch discr {
>   0 -> {
>     let struct: {} = @make_struct{};
>     @make_union<0, struct>
>   }
>   1 -> {
>     let struct1: {} = @make_struct{};
>     @make_union<1, struct1>
>   }
>   2 -> {
>     let struct2: {} = @make_struct{};
>     @make_union<2, struct2>
>   }
>   } in join join;
>   return join;
> }
> 
> fn x3_thunk(): int
> {
>   let struct3: {} = @make_struct{};
>   let var6: [ `0 {} ] = @make_union<0, struct3>;
>   let discr1: int = @get_union_id<var6>;
>   switch discr1 {
>   0 -> {
>     let struct4: {} = @make_struct{};
>     let var7: [ `0 {}, `1 {}, `2 {} ] = @make_union<1, struct4>;
>     @call_direct(f5, var7)
>   }
>   } in join join1;
>   let discr2: int = @get_union_id<join1>;
>   switch discr2 {
>   0 -> {
>     let var8: int = 0;
>     @call_direct(f12, var8)
>   }
>   1 -> {
>     let var9: int = 0;
>     @call_direct(f22, var9)
>   }
>   2 -> {
>     let var10: int = 0;
>     @call_direct(f32, var10)
>   }
>   } in join join2;
>   return join2;
> }
> 
> entry x3: int = @call_direct(x3_thunk);

> cor-out +eval -print
> x3 = 2
>    > 2