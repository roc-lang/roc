# cor +solve -elab
# cor +canonicalize -print
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

run main = {};;
#   ^^^^

> cor-out +solve -elab
> 
> run main = {};;
> #   ^^^^ {}?*
> 

> cor-out +canonicalize -print
> let main =
>   {}

> cor-out +monotype -print
> run main: {} =
>   {}

> cor-out +monotype_lifted -print
> run main: {} =
>   {}

> cor-out +lambdasolved -print
> run main: {} =
>   {}

> cor-out +lambdamono -print
> run main: {} =
>   {}

> cor-out +ir -print
> fn main_thunk(): {}
> {
>   let var: {} = @make_struct{};
>   return var;
> }
> 
> entry main: {} = @call_direct(main_thunk);

> cor-out +eval -print
> main = []
>      > {}