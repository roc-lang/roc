# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

let nestForever =
  let nester = \x ->
    let f = \r ->
      let g = \t -> when t is
        | A -> B
        | B -> nester r A
        end
      in g
    in
    f {}
  in
  nester
;;

run main = nestForever {} B
;;

> cor-out +monotype -print
> let nestForever: {} -> [A, B] -> [B] =
>   let rec nester: {} -> [A, B] -> [B] = \x ->
>     let f: {} -> [A, B] -> [B] = \r ->
>       let g: [A, B] -> [B] = \t ->
>         when t is
>           | A -> B 
>           | B -> (nester r) (A )
>         end
>       in
>       g
>     in
>     f {}
>   in
>   nester
> run main: [B] =
>   (nestForever {}) (B )

> cor-out +monotype_lifted -print
> let g1(r: {}): [A, B] -> [B] = \t ->
>   when t is
>     | A -> B 
>     | B -> (nester1 r) (A )
>   end
> let f1: {} -> [A, B] -> [B] = \r ->
>   let g2: [A, B] -> [B] =
>     g1 in
>   g2
> let nester1: {} -> [A, B] -> [B] = \x ->
>   f1 {}
> let nestForever: {} -> [A, B] -> [B] =
>   nester1
> run main: [B] =
>   (nestForever {}) (B )

> cor-out +lambdasolved -print
> let f1: {} -[f1]-> [A, B] -[g1 (r: {})]-> [B] = \r ->
>   let g2: [A, B] -[g1 (r: {})]-> [B] =
>     g1 in
>   g2
> let g1(r: {}): [A, B] -[g1 (r: {})]-> [B] = \t ->
>   when t is
>     | A -> B 
>     | B -> (nester1 r) (A )
>   end
> let nester1: {} -[nester1]-> [A, B] -[g1 (r: {})]-> [B] = \x ->
>   f1 {}
> let nestForever: {} -[nester1]-> [A, B] -[g1 (r: {})]-> [B] =
>   nester1
> run main: [B] =
>   (nestForever {}) (B )

> cor-out +lambdamono -print
> fn f2(r: {}): [G1 {r: {}}] =
>   let g2: [G1 {r: {}}] = G1 {r: r} in
>   g2
> fn g3(t: [A, B], captures4: {r: {}}): [B] =
>   let r: {} = captures4.r in
>   when t is
>     | A -> B
>     | B ->
>       when when Nester1 is
>              | Nester1 -> nester2(r)
>            end is
>         | G1 captures3 -> g3(A, captures3)
>       end
>   end
> fn nester2(x: {}): [G1 {r: {}}] =
>   when F1 is
>     | F1 -> f2({})
>   end
> let nestForever: [Nester1] =
>   Nester1
> run main: [B] =
>   when when nestForever is
>          | Nester1 -> nester2({})
>        end is
>     | G1 captures1 -> g3(B, captures1)
>   end

> cor-out +ir -print
> fn f2(r: {}): [ `0 { { {} } } ]
> {
>   let var: { {} } = @make_struct{ r };
>   let struct: { { {} } } = @make_struct{ var };
>   let g2: [ `0 { { {} } } ] = @make_union<0, struct>;
>   return g2;
> }
> 
> fn g3(t: [ `0 {}, `1 {} ], captures4: { {} }): [ `0 {} ]
> {
>   let r: {} = @get_struct_field<captures4, 0>;
>   let discr: int = @get_union_id<t>;
>   switch discr {
>   0 -> {
>     let struct1: {} = @make_struct{};
>     @make_union<0, struct1>
>   }
>   1 -> {
>     let struct2: {} = @make_struct{};
>     let var1: [ `0 {} ] = @make_union<0, struct2>;
>     let discr1: int = @get_union_id<var1>;
>     switch discr1 {
>     0 -> {
>       @call_direct(nester2, r)
>     }
>     } in join join;
>     let discr2: int = @get_union_id<join>;
>     switch discr2 {
>     0 -> {
>       let payload: { { {} } } = @get_union_struct<join>;
>       let captures3: { {} } = @get_struct_field<payload, 0>;
>       let struct3: {} = @make_struct{};
>       let var2: [ `0 {}, `1 {} ] = @make_union<0, struct3>;
>       @call_direct(g3, var2, captures3)
>     }
>     } in join join1;
>     join1
>   }
>   } in join join2;
>   return join2;
> }
> 
> fn nester2(x: {}): [ `0 { { {} } } ]
> {
>   let struct4: {} = @make_struct{};
>   let var3: [ `0 {} ] = @make_union<0, struct4>;
>   let discr3: int = @get_union_id<var3>;
>   switch discr3 {
>   0 -> {
>     let var4: {} = @make_struct{};
>     @call_direct(f2, var4)
>   }
>   } in join join3;
>   return join3;
> }
> 
> fn nestForever_thunk(): [ `0 {} ]
> {
>   let struct5: {} = @make_struct{};
>   let var5: [ `0 {} ] = @make_union<0, struct5>;
>   return var5;
> }
> 
> global nestForever: [ `0 {} ] = @call_direct(nestForever_thunk);
> 
> fn main_thunk(): [ `0 {} ]
> {
>   let discr4: int = @get_union_id<nestForever>;
>   switch discr4 {
>   0 -> {
>     let var6: {} = @make_struct{};
>     @call_direct(nester2, var6)
>   }
>   } in join join4;
>   let discr5: int = @get_union_id<join4>;
>   switch discr5 {
>   0 -> {
>     let payload1: { { {} } } = @get_union_struct<join4>;
>     let captures1: { {} } = @get_struct_field<payload1, 0>;
>     let struct6: {} = @make_struct{};
>     let var7: [ `0 {}, `1 {} ] = @make_union<1, struct6>;
>     @call_direct(g3, var7, captures1)
>   }
>   } in join join5;
>   return join5;
> }
> 
> entry main: [ `0 {} ] = @call_direct(main_thunk);

> cor-out +eval -print
> main = [0]
>      > B 