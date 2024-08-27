use roc_can::{
    expr::{
        AnnotatedMark, ClosureData,
        DeclarationTag::*,
        Declarations,
        Expr::{self, *},
    },
    module::ModuleParams,
    pattern::Pattern,
};
use roc_collections::VecMap;
use roc_module::symbol::{IdentId, IdentIds, ModuleId, Symbol};
use roc_region::all::Loc;
use roc_types::subs::{VarStore, Variable};
use roc_types::types::Type;

struct LowerParams<'a> {
    home_id: ModuleId,
    /// Top-level idents that we need to extend in a module with params. Empty if no params.
    home_top_level_idents: VecMap<IdentId, usize>,
    home_params: &'a Option<ModuleParams>,
    var_store: &'a mut VarStore,
    ident_ids: &'a mut IdentIds,
}

pub fn lower(
    home_id: ModuleId,
    home_params: &Option<ModuleParams>,
    decls: &mut Declarations,
    ident_ids: &mut IdentIds,
    var_store: &mut VarStore,
) {
    let mut home_top_level_idents = VecMap::new();

    if home_params.is_some() {
        for index in 0..decls.len() {
            match decls.declarations[index] {
                Function(fn_index) | Recursive(fn_index) | TailRecursive(fn_index) => {
                    let arity = decls.function_bodies[fn_index.index()]
                        .value
                        .arguments
                        .len();
                    home_top_level_idents.insert(decls.symbols[index].value.ident_id(), arity);
                }
                Value => {
                    home_top_level_idents.insert(decls.symbols[index].value.ident_id(), 0);
                }
                Destructure(_) | MutualRecursion { .. } | Expectation | ExpectationFx => {}
            }
        }
    }

    let mut env = LowerParams {
        home_id,
        home_params,
        home_top_level_idents,
        ident_ids,
        var_store,
    };

    env.lower_decls(decls);
}

impl<'a> LowerParams<'a> {
    fn lower_decls(&mut self, decls: &mut Declarations) {
        for index in 0..decls.len() {
            let tag = decls.declarations[index];

            match tag {
                Value => {
                    self.lower_expr(&mut decls.expressions[index].value);

                    if let Some(new_arg) = self.home_params_argument() {
                        // This module has params, and this is a top-level value,
                        // so we need to convert it into a function that takes them.
                        decls.convert_value_to_function(index, vec![new_arg], self.var_store);
                    }
                }
                Function(fn_def_index) | Recursive(fn_def_index) | TailRecursive(fn_def_index) => {
                    if let Some((var, mark, pattern)) = self.home_params_argument() {
                        // This module has params, and this is a top-level function,
                        // so we need to extend its definition to take them.

                        decls.function_bodies[fn_def_index.index()]
                            .value
                            .arguments
                            .push((var, mark, pattern));

                        if let Some(ann) = &mut decls.annotations[index] {
                            if let Type::Function(args, _, _) = &mut ann.signature {
                                args.push(Type::Variable(var))
                            }
                        }
                    }

                    self.lower_expr(&mut decls.expressions[index].value)
                }

                Destructure(_) | Expectation | ExpectationFx => {
                    self.lower_expr(&mut decls.expressions[index].value)
                }
                MutualRecursion { .. } => {}
            }
        }
    }

    fn lower_expr(&mut self, expr: &mut Expr) {
        let mut expr_stack = vec![expr];

        while let Some(expr) = expr_stack.pop() {
            match expr {
                // Nodes to lower
                ParamsVar {
                    symbol,
                    var,
                    params_symbol,
                    params_var,
                } => {
                    *expr = self.lower_naked_params_var(
                        // todo: get arity of imported symbol
                        0,
                        *symbol,
                        *var,
                        *params_symbol,
                        *params_var,
                    );
                }
                Var(symbol, var) => {
                    if let Some(arity) = self.params_extended_home_symbol(symbol) {
                        let params = self.home_params.as_ref().unwrap();

                        *expr = self.lower_naked_params_var(
                            *arity,
                            *symbol,
                            *var,
                            params.whole_symbol,
                            params.whole_var,
                        );
                    }
                }
                Call(fun, args, _called_via) => {
                    expr_stack.reserve(args.len() + 1);

                    match fun.1.value {
                        ParamsVar {
                            symbol,
                            var,
                            params_var,
                            params_symbol,
                        } => {
                            // Calling an imported function with params
                            args.push((params_var, Loc::at_zero(Var(params_symbol, params_var))));
                            fun.1.value = Var(symbol, var);
                        }
                        Var(symbol, _var) => {
                            if self.params_extended_home_symbol(&symbol).is_some() {
                                // Calling a top-level function in the current module with params
                                let params = self.home_params.as_ref().unwrap();
                                args.push((
                                    params.whole_var,
                                    Loc::at_zero(Var(params.whole_symbol, params.whole_var)),
                                ));
                            }
                        }
                        _ => expr_stack.push(&mut fun.1.value),
                    }

                    for (_, arg) in args.iter_mut() {
                        expr_stack.push(&mut arg.value);
                    }
                }

                // Nodes to walk
                Closure(ClosureData {
                    loc_body,
                    captured_symbols: _,
                    name: _,
                    function_type: _,
                    closure_type: _,
                    return_type: _,
                    recursive: _,
                    arguments: _,
                }) => {
                    expr_stack.push(&mut loc_body.value);
                }

                LetNonRec(def, cont) => {
                    expr_stack.reserve(2);
                    expr_stack.push(&mut def.loc_expr.value);
                    expr_stack.push(&mut cont.value);
                }

                LetRec(defs, cont, _cycle_mark) => {
                    expr_stack.reserve(defs.len() + 1);

                    for def in defs {
                        expr_stack.push(&mut def.loc_expr.value);
                    }

                    expr_stack.push(&mut cont.value);
                }
                When {
                    loc_cond,
                    branches,
                    cond_var: _,
                    expr_var: _,
                    region: _,
                    branches_cond_var: _,
                    exhaustive: _,
                } => {
                    expr_stack.reserve(branches.len() + 1);
                    expr_stack.push(&mut loc_cond.value);

                    for branch in branches.iter_mut() {
                        expr_stack.push(&mut branch.value.value);
                    }
                }
                If {
                    branches,
                    final_else,
                    cond_var: _,
                    branch_var: _,
                } => {
                    expr_stack.reserve(branches.len() * 2 + 1);

                    for (cond, ret) in branches.iter_mut() {
                        expr_stack.push(&mut cond.value);
                        expr_stack.push(&mut ret.value);
                    }

                    expr_stack.push(&mut final_else.value);
                }
                RunLowLevel {
                    args,
                    op: _,
                    ret_var: _,
                }
                | ForeignCall {
                    foreign_symbol: _,
                    args,
                    ret_var: _,
                } => {
                    expr_stack.extend(args.iter_mut().map(|(_, arg)| arg));
                }
                List {
                    elem_var: _,
                    loc_elems,
                } => {
                    expr_stack.extend(loc_elems.iter_mut().map(|loc_elem| &mut loc_elem.value));
                }
                Record {
                    record_var: _,
                    fields,
                } => {
                    expr_stack.extend(
                        fields
                            .iter_mut()
                            .map(|(_, field)| &mut field.loc_expr.value),
                    );
                }
                Tuple {
                    tuple_var: _,
                    elems,
                } => {
                    expr_stack.extend(elems.iter_mut().map(|(_, elem)| &mut elem.value));
                }
                ImportParams(_, _, Some((_, params_expr))) => {
                    expr_stack.push(params_expr);
                }
                Crash { msg, ret_var: _ } => {
                    expr_stack.push(&mut msg.value);
                }
                RecordAccess {
                    loc_expr,
                    record_var: _,
                    ext_var: _,
                    field_var: _,
                    field: _,
                } => expr_stack.push(&mut loc_expr.value),
                TupleAccess {
                    loc_expr,
                    tuple_var: _,
                    ext_var: _,
                    elem_var: _,
                    index: _,
                } => expr_stack.push(&mut loc_expr.value),
                RecordUpdate {
                    updates,
                    record_var: _,
                    ext_var: _,
                    symbol: _,
                } => expr_stack.extend(
                    updates
                        .iter_mut()
                        .map(|(_, field)| &mut field.loc_expr.value),
                ),
                Tag {
                    arguments,
                    tag_union_var: _,
                    ext_var: _,
                    name: _,
                } => expr_stack.extend(arguments.iter_mut().map(|(_, arg)| &mut arg.value)),
                OpaqueRef {
                    argument,
                    opaque_var: _,
                    name: _,
                    specialized_def_type: _,
                    type_arguments: _,
                    lambda_set_variables: _,
                } => expr_stack.push(&mut argument.1.value),
                Expect {
                    loc_condition,
                    loc_continuation,
                    lookups_in_cond: _,
                } => {
                    expr_stack.reserve(2);
                    expr_stack.push(&mut loc_condition.value);
                    expr_stack.push(&mut loc_continuation.value);
                }
                ExpectFx {
                    loc_condition,
                    loc_continuation,
                    lookups_in_cond: _,
                } => {
                    expr_stack.reserve(2);
                    expr_stack.push(&mut loc_condition.value);
                    expr_stack.push(&mut loc_continuation.value);
                }
                Dbg {
                    loc_message,
                    loc_continuation,
                    source_location: _,
                    source: _,
                    variable: _,
                    symbol: _,
                } => {
                    expr_stack.reserve(2);
                    expr_stack.push(&mut loc_message.value);
                    expr_stack.push(&mut loc_continuation.value);
                }
                RecordAccessor(_)
                | ImportParams(_, _, None)
                | ZeroArgumentTag {
                    closure_name: _,
                    variant_var: _,
                    ext_var: _,
                    name: _,
                }
                | OpaqueWrapFunction(_)
                | EmptyRecord
                | TypedHole(_)
                | RuntimeError(_)
                | Num(_, _, _, _)
                | Int(_, _, _, _, _)
                | Float(_, _, _, _, _)
                | Str(_)
                | SingleQuote(_, _, _, _)
                | IngestedFile(_, _, _)
                | AbilityMember(_, _, _) => { /* terminal */ }
            }
        }
    }

    fn unique_symbol(&mut self) -> Symbol {
        Symbol::new(self.home_id, self.ident_ids.gen_unique())
    }

    fn params_extended_home_symbol(&self, symbol: &Symbol) -> Option<&usize> {
        if symbol.module_id() == self.home_id {
            self.home_top_level_idents.get(&symbol.ident_id())
        } else {
            None
        }
    }

    fn lower_naked_params_var(
        &mut self,
        arity: usize,
        symbol: Symbol,
        var: Variable,
        params_symbol: Symbol,
        params_var: Variable,
    ) -> Expr {
        let params_arg = (params_var, Loc::at_zero(Var(params_symbol, params_var)));
        let call_fn = Box::new((
            self.var_store.fresh(),
            Loc::at_zero(Var(symbol, var)),
            self.var_store.fresh(),
            self.var_store.fresh(),
        ));

        if arity == 0 {
            // We are passing a top-level value that takes params, so we need to replace the Var
            // with a call that passes the params to get the final result.
            //
            // value = \#params -> #params.x * 2
            // record = \... #params -> { doubled: value }
            //                                       ↓
            //                                     value #params
            Call(
                call_fn,
                vec![params_arg],
                // todo: custom called via
                roc_module::called_via::CalledVia::Space,
            )
        } else {
            // We are passing a top-level function that takes params, so we need to replace
            // the Var with a closure that captures the params and passes them to the function.
            //
            // fn1 = \arg #params -> #params.x * arg
            // fn2 = \... #params -> List.map [1, 2] fn1
            //                                        ↓
            //                                       (\#1 -> fn1 #1 #params)
            //
            let mut arguments = Vec::with_capacity(arity);
            let mut call_arguments = Vec::with_capacity(arity + 1);

            for _ in 0..arity {
                let sym = self.unique_symbol();
                let var = self.var_store.fresh();

                arguments.push((
                    var,
                    AnnotatedMark::new(self.var_store),
                    Loc::at_zero(Pattern::Identifier(sym)),
                ));
                call_arguments.push((var, Loc::at_zero(Var(sym, var))));
            }

            call_arguments.push(params_arg);

            let body = Call(
                call_fn,
                call_arguments,
                // todo: custom called via
                roc_module::called_via::CalledVia::Space,
            );

            Closure(ClosureData {
                function_type: self.var_store.fresh(),
                closure_type: self.var_store.fresh(),
                return_type: self.var_store.fresh(),
                name: self.unique_symbol(),
                captured_symbols: vec![(params_symbol, params_var)],
                recursive: roc_can::expr::Recursive::NotRecursive,
                arguments,
                loc_body: Box::new(Loc::at_zero(body)),
            })
        }
    }
    fn home_params_argument(&mut self) -> Option<(Variable, AnnotatedMark, Loc<Pattern>)> {
        match &self.home_params {
            Some(module_params) => {
                let new_var = self.var_store.fresh();
                Some((
                    new_var,
                    AnnotatedMark::new(self.var_store),
                    module_params.pattern(),
                ))
            }
            None => None,
        }
    }
}
