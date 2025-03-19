# cor +canonicalize -print
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

let len = \l ->
  when l is
    | Nil -> 0
    | Cons x xs -> 1 + (len xs)
  end
;;

run main = len (Cons 0 (Cons 0 (Cons 0 (Cons 0 Nil))));;

> cor-out +canonicalize -print
> let len = \l ->
>   when l is | Nil -> 0 | Cons x xs -> ~add 1 (len xs)
>   end
> let main =
>   len (Cons 0 (Cons 0 (Cons 0 (Cons 0 (Nil )))))

> cor-out +monotype -print
> let rec len1: [Cons Int <rec>, Nil] -> Int = \l ->
>   when l is
>     | Nil -> 0
>     | Cons x xs -> ~add 1 (len1 xs)
>   end
> run main: Int =
>   len1 (Cons 0 (Cons 0 (Cons 0 (Cons 0 (Nil )))))

> cor-out +monotype_lifted -print
> let len1: [Cons Int <rec>, Nil] -> Int = \l ->
>   when l is
>     | Nil -> 0
>     | Cons x xs -> ~add 1 (len1 xs)
>   end
> run main: Int =
>   len1 (Cons 0 (Cons 0 (Cons 0 (Cons 0 (Nil )))))

> cor-out +lambdasolved -print
> let len1: [Cons Int <rec>, Nil] -[len1]-> Int = \l ->
>   when l is
>     | Nil -> 0
>     | Cons x xs -> ~add 1 (len1 xs)
>   end
> run main: Int =
>   len1 (Cons 0 (Cons 0 (Cons 0 (Cons 0 (Nil )))))

> cor-out +lambdamono -print
> fn len2(l: [Cons Int <rec>, Nil]): Int =
>   when l is
>     | Nil -> 0
>     | Cons x xs -> ~add 1, when Len1 is
>                              | Len1 -> len2(xs)
>                            end
>   end
> run main: Int =
>   when Len1 is
>     | Len1 -> len2(Cons 0 (Cons 0 (Cons 0 (Cons 0 (Nil)))))
>   end

> cor-out +ir -print
> fn len2(l: Box!a([ `0 { int, !a }, `1 {} ])): int
> {
>   let discr: int = @get_union_id<l>;
>   switch discr {
>   0 -> {
>     let payload: { int, Box!a([ `0 { int, !a }, `1 {} ]) }
>       = @get_union_struct<l>;
>     let x: int = @get_struct_field<payload, 0>;
>     let xs: Box!a([ `0 { int, !a }, `1 {} ]) = @get_struct_field<payload, 1>;
>     let var: int = 1;
>     let struct: {} = @make_struct{};
>     let var1: [ `0 {} ] = @make_union<0, struct>;
>     let discr1: int = @get_union_id<var1>;
>     switch discr1 {
>     0 -> {
>       @call_direct(len2, xs)
>     }
>     } in join join;
>     @call_kfn(add, var, join)
>   }
>   1 -> {
>     0
>   }
>   } in join join1;
>   return join1;
> }
> 
> fn main_thunk(): int
> {
>   let struct1: {} = @make_struct{};
>   let var2: [ `0 {} ] = @make_union<0, struct1>;
>   let discr2: int = @get_union_id<var2>;
>   switch discr2 {
>   0 -> {
>     let var3: int = 0;
>     let var4: int = 0;
>     let var5: int = 0;
>     let var6: int = 0;
>     let struct6: {} = @make_struct{};
>     let var7:
>           [
>              `0 {
>                  int,
>                   [
>                      `0 {
>                          int,
>                           [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {}
>                           ]
>                          ,
>                         },
>                      `1 {}
>                   ]
>                  ,
>                 },
>              `1 {}
>           ]
>       = @make_union<1, struct6>;
>     let struct5:
>           {
>            int,
>             [
>                `0 {
>                    int,
>                     [
>                        `0 {
>                            int,
>                             [
>                                `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) },
>                                `1 {}
>                             ]
>                            ,
>                           },
>                        `1 {}
>                     ]
>                    ,
>                   },
>                `1 {}
>             ]
>            ,
>           }
>       = @make_struct{ var6, var7 };
>     let var8:
>           [
>              `0 { int, [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {} ] },
>              `1 {}
>           ]
>       = @make_union<0, struct5>;
>     let struct4:
>           {
>            int,
>             [
>                `0 {
>                    int,
>                     [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {} ]
>                    ,
>                   },
>                `1 {}
>             ]
>            ,
>           }
>       = @make_struct{ var5, var8 };
>     let var9: [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {} ]
>       = @make_union<0, struct4>;
>     let struct3:
>           { int, [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {} ] }
>       = @make_struct{ var4, var9 };
>     let var10: [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {} ]
>       = @make_union<0, struct3>;
>     let struct2:
>           { int, [ `0 { int, Box!a([ `0 { int, !a }, `1 {} ]) }, `1 {} ] }
>       = @make_struct{ var3, var10 };
>     let var11: Box!a([ `0 { int, !a }, `1 {} ]) = @make_union<0, struct2>;
>     @call_direct(len2, var11)
>   }
>   } in join join2;
>   return join2;
> }
> 
> entry main: int = @call_direct(main_thunk);

> cor-out +eval -print
> main = 4
>      > 4