use roc_can::{
    expr::{
        AnnotatedMark, ClosureData,
        DeclarationTag::*,
        Declarations,
        Expr::{self, *},
    },
    pattern::Pattern,
};
use roc_region::all::Loc;
use roc_types::subs::{VarStore, Variable};
use roc_types::types::Type;

struct LowerParams {
    home_params: Option<HomeParams>,
}

type HomeParams = (Variable, AnnotatedMark, Loc<Pattern>);

pub fn lower(home_params: Option<HomeParams>, decls: &mut Declarations, var_store: &mut VarStore) {
    let env = LowerParams { home_params };

    let mut index = 0;

    while index < decls.len() {
        let tag = decls.declarations[index];

        match tag {
            Value => {
                // todo: thunk in module with params
                env.lower_expr(&mut decls.expressions[index].value)
            }
            Function(fn_def_index) => {
                if let Some((_, mark, pattern)) = env.home_params.clone() {
                    let var = var_store.fresh();

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

impl LowerParams {
    fn lower_expr(&self, expr: &mut Expr) {
        match expr {
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
