use inkwell::basic_block::BasicBlock;
use inkwell::module::Linkage;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum::{self, *};
use inkwell::values::{BasicValue, FunctionValue, IntValue, PointerValue};
use inkwell::{FloatPredicate, IntPredicate};

use crate::can::expr::Expr;
use crate::can::ident::Lowercase;
use crate::can::pattern::Pattern::{self, *};
use crate::can::symbol::Symbol;
use crate::collections::ImMap;
use crate::collections::MutMap;
use crate::gen::convert::content_to_basic_type;
use crate::gen::env::Env;
use crate::subs::{Content, FlatType, Subs};

fn extract_procs(loc_expr: Located<Expr>, module: &Module<'ctx>, name: Option<Lowercase>, procs, &mut Procs<'ctx>) -> Located<Expr> {
    let mut procs = Vec::new();

    match expr {
        LetNonRec(def, ret_expr, var) => {
            let loc_pattern = def.loc_pattern;
            let loc_expr = def.loc_expr;

            // If we're defining a named closure, insert it into Procs and then
            // remove the Let. When code later goes to look it up, it'll be in Procs!
            //
            // Before:
            //
            //     identity = \a -> a
            //
            //     identity 5
            //
            // After: (`identity` is now in Procs)
            //
            //     identity 5
            //
            let pattern = match loc_pattern.value {
                Identifier(name) => {
                    match &loc_expr.value {
                        Closure(_, _, _, _, _) => {
                            // Extract Procs, but discard the resulting Expr::Var.
                            // That Var looks up the pointer, which we won't use here!
                            extract_procs(loc_expr, Some(name), procs);

                            // Discard this LetNonRec by replacing it with its ret_expr.
                            return ret_expr;
                        }
                        _ => {
                            // If this isn't a Closure, proceed as normal.
                            Identifier(name)
                        }
                    }
                }
                pat => pat
            }

            // At this point, it's safe to assume we aren't assigning a Closure to a def.
            // Extract Procs from the def body and the ret expression, and return the result!
            let ret_expr = extract_procs(ret_expr, None, procs);
            let loc_expr = extract_procs(def.loc_expr, None, procs);
            let loc_pattern = Located { region: def.loc_pattern.region, value: pattern };
            let def = Def { loc_pattern, loc_expr, ..def };

            LetNonRec(def, ret_expr, var)
        }

        Closure(var, symbol, recursive, loc_args, boxed_ret) => {
            let (loc_ret, var) = boxed_ret;
            let name = match name {
                Some(name) => name.as_str(),
                None => {
                    // Give the closure a name like "_0" or "_1".
                    // We know procs.len() will be unique!
                    format!("_{}", procs.len()).as_str();
                }
            };

            let fn_val = module.add_function(name, fn_type, linkage);

            panic!("push to procs");
        }
    };
}

