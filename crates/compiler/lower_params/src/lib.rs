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
use roc_region::all::Loc;
use roc_types::subs::{VarStore, Variable};
use roc_types::types::Type;

struct LowerParams<'a> {
    // todo: remove var as we can't use it
    home_params: Option<ModuleParams>,
    var_store: &'a mut VarStore,
}

pub fn lower(
    home_params: Option<ModuleParams>,
    decls: &mut Declarations,
    var_store: &mut VarStore,
) {
    let mut env = LowerParams {
        home_params,
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

    fn lower_expr(&mut self, expr: &mut Expr) {
        match expr {
            ParamsVar {
                symbol,
                var,
                params_symbol,
                params_var,
            } => {
                let params_arg = (*params_var, Loc::at_zero(Var(*params_symbol, *params_var)));

                *expr = Call(
                    Box::new((
                        self.var_store.fresh(),
                        Loc::at_zero(Var(*symbol, *var)),
                        self.var_store.fresh(),
                        self.var_store.fresh(),
                    )),
                    vec![params_arg],
                    // todo: custom called via
                    roc_module::called_via::CalledVia::Space,
                );
            }
            Call(fun, args, _called_via) => {
                for arg in args.iter_mut() {
                    // todo: params var in arg
                    self.lower_expr(&mut arg.1.value);
                }

                match fun.1.value {
                    ParamsVar {
                        symbol,
                        var,
                        params_var,
                        params_symbol,
                    } => {
                        args.push((params_var, Loc::at_zero(Var(params_symbol, params_var))));
                        fun.1.value = Var(symbol, var);
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
}
