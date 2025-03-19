# cor +solve -elab
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

let id = \x -> x;;
#   ^^

run idint = id 1;;
#   ^^^^^
run idstr = id "hello";;
#   ^^^^^

> cor-out +solve -elab
> 
> let id = \x -> x;;
> #   ^^ 'a -> 'a
> 
> run idint = id 1;;
> #   ^^^^^ Int
> run idstr = id "hello";;
> #   ^^^^^ Str
> 

> cor-out +monotype -print
> let id2: Str -> Str = \x ->
>   x
> let id1: Int -> Int = \x ->
>   x
> run idint: Int =
>   id1 1
> run idstr: Str =
>   id2 "hello"

> cor-out +monotype_lifted -print
> let id2: Str -> Str = \x ->
>   x
> let id1: Int -> Int = \x ->
>   x
> run idint: Int =
>   id1 1
> run idstr: Str =
>   id2 "hello"

> cor-out +lambdasolved -print
> let id1: Int -[id1]-> Int = \x ->
>   x
> run idint: Int =
>   id1 1
> let id2: Str -[id2]-> Str = \x ->
>   x
> run idstr: Str =
>   id2 "hello"

> cor-out +lambdamono -print
> fn id4(x: Str): Str =
>   x
> fn id3(x: Int): Int =
>   x
> run idint: Int =
>   when Id1 is
>     | Id1 -> id3(1)
>   end
> run idstr: Str =
>   when Id2 is
>     | Id2 -> id4("hello")
>   end

> cor-out +ir -print
> fn id4(x: str): str
> {
>   return x;
> }
> 
> fn id3(x: int): int
> {
>   return x;
> }
> 
> fn idint_thunk(): int
> {
>   let struct: {} = @make_struct{};
>   let var: [ `0 {} ] = @make_union<0, struct>;
>   let discr: int = @get_union_id<var>;
>   switch discr {
>   0 -> {
>     let var1: int = 1;
>     @call_direct(id3, var1)
>   }
>   } in join join;
>   return join;
> }
> 
> entry idint: int = @call_direct(idint_thunk);
> 
> fn idstr_thunk(): str
> {
>   let struct1: {} = @make_struct{};
>   let var2: [ `0 {} ] = @make_union<0, struct1>;
>   let discr1: int = @get_union_id<var2>;
>   switch discr1 {
>   0 -> {
>     let var3: str = "hello";
>     @call_direct(id4, var3)
>   }
>   } in join join1;
>   return join1;
> }
> 
> entry idstr: str = @call_direct(idstr_thunk);

> cor-out +eval -print
> idint = 1
>       > 1
> idstr = [104 101 108 108 111]
>       > "hello"