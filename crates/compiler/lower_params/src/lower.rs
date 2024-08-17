use std::ops::Range;

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
use roc_module::symbol::{IdentId, ModuleId, Symbol};
use roc_region::all::Loc;
use roc_types::subs::{VarStore, Variable};
use roc_types::types::Type;

struct LowerParams<'a> {
    home_id: ModuleId,
    /// Top-level idents that we need to extend in a module with params. Empty if no params.
    home_top_level_idents: Vec<IdentId>,
    home_params: &'a Option<ModuleParams>,
    var_store: &'a mut VarStore,
}

pub fn lower(
    home_id: ModuleId,
    home_params: &Option<ModuleParams>,
    decls: &mut Declarations,
    var_store: &mut VarStore,
) {
    let home_top_level_idents = if home_params.is_some() {
        decls
            .symbols
            .iter()
            .map(|loc_sym| loc_sym.value.ident_id())
            .collect()
    } else {
        vec![]
    };

    let mut env = LowerParams {
        home_id,
        home_params,
        home_top_level_idents,
        var_store,
    };

    env.lower_decls(decls, 0..decls.len());
}

impl<'a> LowerParams<'a> {
    fn lower_decls(&mut self, decls: &mut Declarations, range: Range<usize>) {
        let mut index = range.start;

        while index < range.end {
            let tag = decls.declarations[index];

            match tag {
                Value => {
                    self.lower_expr(&mut decls.expressions[index].value);

                    if let Some(new_arg) = self.home_params_argument() {
                        decls.convert_value_to_function(index, vec![new_arg], self.var_store);
                    }
                }
                Function(fn_def_index) | Recursive(fn_def_index) | TailRecursive(fn_def_index) => {
                    if let Some((_, mark, pattern)) = self.home_params_argument() {
                        let var = self.var_store.fresh();

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

                MutualRecursion {
                    length,
                    cycle_mark: _,
                } => {
                    let length = length as usize;

                    self.lower_decls(decls, index + 1..index + 1 + length);

                    index += length;
                }
                Destructure(_) | Expectation | ExpectationFx => {
                    self.lower_expr(&mut decls.expressions[index].value)
                }
            }

            index += 1;
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
                    // A referece to a top-level value def in an imported module with params
                    *expr = self.call_params_var(*symbol, *var, *params_symbol, *params_var);
                }
                Var(symbol, var) => {
                    if self.is_params_extended_home_symbol(symbol) {
                        // A reference to a top-level value def in the home module with params
                        let params = self.home_params.as_ref().unwrap();
                        *expr = self.call_params_var(
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
                        // A call to a function in an imported module with params
                        ParamsVar {
                            symbol,
                            var,
                            params_var,
                            params_symbol,
                        } => {
                            args.push((params_var, Loc::at_zero(Var(params_symbol, params_var))));
                            fun.1.value = Var(symbol, var);
                        }
                        Var(symbol, _var) => {
                            if self.is_params_extended_home_symbol(&symbol) {
                                // A call to a top-level function in the home module with params
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

                // Nodes to walk
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

    fn is_params_extended_home_symbol(&self, symbol: &Symbol) -> bool {
        symbol.module_id() == self.home_id
            && self.home_top_level_idents.contains(&symbol.ident_id())
    }

    fn call_params_var(
        &mut self,
        symbol: Symbol,
        var: Variable,
        params_symbol: Symbol,
        params_var: Variable,
    ) -> Expr {
        let params_arg = (params_var, Loc::at_zero(Var(params_symbol, params_var)));

        Call(
            Box::new((
                self.var_store.fresh(),
                Loc::at_zero(Var(symbol, var)),
                self.var_store.fresh(),
                self.var_store.fresh(),
            )),
            vec![params_arg],
            // todo: custom called via
            roc_module::called_via::CalledVia::Space,
        )
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
