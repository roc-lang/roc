# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

run main_handler =
    let handle = \op -> when op is
        | StdinLine f -> handle (f "hello")
        | StdoutLine s f -> handle (f s)
        | Done x -> x
    end
    in
    handle (StdinLine (\s -> StdoutLine s (\x -> Done x)))
;;

> cor-out +monotype -print
> run main_handler: Str =
>   let rec handle: [
>                     Done Str,
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str (Str -> <rec>)
>                     ] -> Str = \op ->
>     when op is
>       | StdinLine f -> handle (f "hello")
>       | StdoutLine s f1 -> handle (f1 s)
>       | Done x -> x
>     end
>   in
>   handle (StdinLine \s1 -> (StdoutLine s1 \x1 -> (Done x1)))

> cor-out +monotype_lifted -print
> let clos: Str -> [Done Str, StdinLine (Str -> <rec>), StdoutLine Str <rec>] = \x1 ->
>   Done x1
> let clos1: Str -> [Done Str, StdinLine <rec>, StdoutLine Str (Str -> <rec>)] = \s1 ->
>   StdoutLine s1 clos
> let handle1: [
>                Done Str,
>                StdinLine (Str -> <rec>),
>                StdoutLine Str (Str -> <rec>)
>                ] -> Str = \op ->
>   when op is
>     | StdinLine f -> handle1 (f "hello")
>     | StdoutLine s f1 -> handle1 (f1 s)
>     | Done x -> x
>   end
> run main_handler: Str =
>   handle1 (StdinLine clos1)

> cor-out +lambdasolved -print
> let handle1: [
>                Done Str,
>                StdinLine (Str -<'1271>-> <rec>),
>                StdoutLine Str (Str -<'1269>-> <rec>)
>                ] -[handle1]-> Str = \op ->
>   when op is
>     | StdinLine f -> handle1 (f "hello")
>     | StdoutLine s f1 -> handle1 (f1 s)
>     | Done x -> x
>   end
> let clos: Str
>             -[clos]-> [
>                         Done Str,
>                         StdinLine (Str -<'1287>-> <rec>),
>                         StdoutLine Str <rec>
>                         ] = \x1 ->
>   Done x1
> let clos1: Str
>              -[clos1]-> [
>                           Done Str,
>                           StdinLine <rec>,
>                           StdoutLine Str (Str -[clos]-> <rec>)
>                           ] = \s1 ->
>   StdoutLine s1 clos
> run main_handler: Str =
>   handle1 (StdinLine clos1)

> cor-out +lambdamono -print
> fn clos3(x1: Str): [Done Str, StdinLine [Clos1], StdoutLine Str [Clos]] =
>   Done x1
> fn clos2(s1: Str): [Done Str, StdinLine [Clos1], StdoutLine Str [Clos]] =
>   StdoutLine s1 (Clos)
> fn handle2(op: [Done Str, StdinLine [Clos1], StdoutLine Str [Clos]]): Str =
>   when op is
>     | StdinLine f ->
>       when Handle1 is
>         | Handle1 -> handle2(when f is
>                                | Clos1 -> clos2("hello")
>                              end)
>       end
>     | StdoutLine s f1 ->
>       when Handle1 is
>         | Handle1 -> handle2(when f1 is
>                                | Clos -> clos3(s)
>                              end)
>       end
>     | Done x -> x
>   end
> run main_handler: Str =
>   when Handle1 is
>     | Handle1 -> handle2(StdinLine (Clos1))
>   end

> cor-out +ir -print
> fn clos3(x1: str): [ `0 { str }, `1 { [ `0 {} ] }, `2 { str, [ `0 {} ] } ]
> {
>   let struct: { str } = @make_struct{ x1 };
>   let var: [ `0 { str }, `1 { [ `0 {} ] }, `2 { str, [ `0 {} ] } ]
>     = @make_union<0, struct>;
>   return var;
> }
> 
> fn clos2(s1: str): [ `0 { str }, `1 { [ `0 {} ] }, `2 { str, [ `0 {} ] } ]
> {
>   let struct2: {} = @make_struct{};
>   let var1: [ `0 {} ] = @make_union<0, struct2>;
>   let struct1: { str, [ `0 {} ] } = @make_struct{ s1, var1 };
>   let var2: [ `0 { str }, `1 { [ `0 {} ] }, `2 { str, [ `0 {} ] } ]
>     = @make_union<2, struct1>;
>   return var2;
> }
> 
> fn handle2(op: [ `0 { str }, `1 { [ `0 {} ] }, `2 { str, [ `0 {} ] } ]): str
> {
>   let discr: int = @get_union_id<op>;
>   switch discr {
>   0 -> {
>     let payload2: { str } = @get_union_struct<op>;
>     let x: str = @get_struct_field<payload2, 0>;
>     x
>   }
>   1 -> {
>     let payload: { [ `0 {} ] } = @get_union_struct<op>;
>     let f: [ `0 {} ] = @get_struct_field<payload, 0>;
>     let struct3: {} = @make_struct{};
>     let var3: [ `0 {} ] = @make_union<0, struct3>;
>     let discr1: int = @get_union_id<var3>;
>     switch discr1 {
>     0 -> {
>       let discr2: int = @get_union_id<f>;
>       switch discr2 {
>       0 -> {
>         let var4: str = "hello";
>         @call_direct(clos2, var4)
>       }
>       } in join join;
>       @call_direct(handle2, join)
>     }
>     } in join join1;
>     join1
>   }
>   2 -> {
>     let payload1: { str, [ `0 {} ] } = @get_union_struct<op>;
>     let s: str = @get_struct_field<payload1, 0>;
>     let f1: [ `0 {} ] = @get_struct_field<payload1, 1>;
>     let struct4: {} = @make_struct{};
>     let var5: [ `0 {} ] = @make_union<0, struct4>;
>     let discr3: int = @get_union_id<var5>;
>     switch discr3 {
>     0 -> {
>       let discr4: int = @get_union_id<f1>;
>       switch discr4 {
>       0 -> {
>         @call_direct(clos3, s)
>       }
>       } in join join2;
>       @call_direct(handle2, join2)
>     }
>     } in join join3;
>     join3
>   }
>   } in join join4;
>   return join4;
> }
> 
> fn main_handler_thunk(): str
> {
>   let struct5: {} = @make_struct{};
>   let var6: [ `0 {} ] = @make_union<0, struct5>;
>   let discr5: int = @get_union_id<var6>;
>   switch discr5 {
>   0 -> {
>     let struct7: {} = @make_struct{};
>     let var7: [ `0 {} ] = @make_union<0, struct7>;
>     let struct6: { [ `0 {} ] } = @make_struct{ var7 };
>     let var8: [ `0 { str }, `1 { [ `0 {} ] }, `2 { str, [ `0 {} ] } ]
>       = @make_union<1, struct6>;
>     @call_direct(handle2, var8)
>   }
>   } in join join5;
>   return join5;
> }
> 
> entry main_handler: str = @call_direct(main_handler_thunk);

> cor-out +eval -print
> main_handler = [104 101 108 108 111]
>              > "hello"