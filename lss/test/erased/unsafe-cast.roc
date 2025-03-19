# cor +solve -elab
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print

run main =
  let x = "x" in
  let y = ~erase x in
#     ^
  let z = ~unerase y in
#     ^
  z + 1;;

> cor-out +solve -elab
> 
> run main =
>   let x = "x" in
>   let y = ~erase x in
> #     ^ Erased
>   let z = ~unerase y in
> #     ^ Int
>   z + 1;;
> 

> cor-out +monotype -print
> run main: Int =
>   let x: Str =
>     "x" in
>   let y: Erased =
>     ~erase x in
>   let z: Int =
>     ~unerase y in
>   ~add z 1

> cor-out +monotype_lifted -print
> run main: Int =
>   let x: Str =
>     "x" in
>   let y: Erased =
>     ~erase x in
>   let z: Int =
>     ~unerase y in
>   ~add z 1

> cor-out +lambdasolved -print
> run main: Int =
>   let x: Str =
>     "x" in
>   let y: Erased =
>     ~erase x in
>   let z: Int =
>     ~unerase y in
>   ~add z 1

> cor-out +lambdamono -print
> run main: Int =
>   let x: Str = "x" in
>   let y: Erased = ~erase x in
>   let z: Int = ~unerase y in
>   ~add z, 1

> cor-out +ir -print
> fn main_thunk(): int
> {
>   let x: str = "x";
>   let y: *opaque = @call_kfn(erase, x);
>   let z: int = @call_kfn(unerase, y);
>   let var: int = 1;
>   let var1: int = @call_kfn(add, z, var);
>   return var1;
> }
> 
> entry main: int = @call_direct(main_thunk);