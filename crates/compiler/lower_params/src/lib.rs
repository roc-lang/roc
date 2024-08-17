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
    home_params: Option<ModuleParams>,
    var_store: &'a mut VarStore,
}

pub fn lower(
    home_id: ModuleId,
    home_params: Option<ModuleParams>,
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

    let mut index = 0;

    while index < decls.len() {
        let tag = decls.declarations[index];

        match tag {
            Value => {
                env.lower_expr(&mut decls.expressions[index].value);

                if let Some(new_arg) = env.home_params_argument() {
                    decls.convert_value_to_function(index, vec![new_arg], env.var_store);
                }
            }
            Function(fn_def_index) => {
                if let Some((_, mark, pattern)) = env.home_params_argument() {
                    let var = env.var_store.fresh();

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

                env.lower_expr(&mut decls.expressions[index].value)
            }

            Recursive(_) => { /* todo */ }
            MutualRecursion { length, .. } => {
                /* todo */
                index += length as usize;
            }
            TailRecursive(_) => { /* todo */ }
            Destructure(_) => { /* todo */ }
            Expectation => { /* todo */ }
            ExpectationFx => { /* todo */ }
        }

        index += 1;
    }
}

impl<'a> LowerParams<'a> {
    fn lower_expr(&mut self, expr: &mut Expr) {
        match expr {
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
                    *expr =
                        self.call_params_var(*symbol, *var, params.whole_symbol, params.whole_var);
                }
            }
            Call(fun, args, _called_via) => {
                for arg in args.iter_mut() {
                    // todo: params var in arg
                    self.lower_expr(&mut arg.1.value);
                }

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
                    _ => self.lower_expr(&mut fun.1.value),
                }
            }
            Closure(ClosureData {
                function_type: _,
                closure_type: _,
                return_type: _,
                name: _,
                captured_symbols: _,
                recursive: _,
                arguments: _,
                loc_body,
            }) => {
                // todo: capture params?

                self.lower_expr(&mut loc_body.value);
            }
            _ => { /* todo */ }
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
