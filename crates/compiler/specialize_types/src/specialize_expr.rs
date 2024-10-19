use crate::expr::{self, Declarations, Expr, FunctionDef};
use crate::specialize_type::{MonoCache, Problem};
use roc_collections::VecMap;
use roc_module::symbol::Symbol;
use roc_types::subs::{Subs, Variable};

struct Context {
    symbols: Symbol,
    fresh_tvar: Box<dyn Fn() -> Variable>,
    specializations: Specializations,
}

struct Specializations {
    symbols: Symbol,
    fenv: Vec<(Symbol, expr::FunctionDef)>,
    specializations: Vec<(SpecializationKey, NeededSpecialization)>,
}

#[derive(PartialEq, Eq)]
struct SpecializationKey(Symbol, Variable);

struct NeededSpecialization {
    def: expr::FunctionDef,
    name_new: Symbol,
    t_new: Variable,
    specialized: Option<expr::Def>,
}

impl Specializations {
    fn make(symbols: Symbol, program: &expr::Declarations) -> Self {
        let fenv = program
            .iter_top_down()
            .filter_map(|(_, tag)| match tag {
                expr::DeclarationTag::Function(idx)
                | expr::DeclarationTag::Recursive(idx)
                | expr::DeclarationTag::TailRecursive(idx) => {
                    let func = &program.function_bodies[idx.index()];
                    Some((func.value.name, func.value.clone()))
                }
                _ => None,
            })
            .collect();

        Specializations {
            symbols,
            fenv,
            specializations: Vec::new(),
        }
    }

    fn specialize_fn(
        &mut self,
        mono_cache: &mut MonoCache,
        name: Symbol,
        t_new: Variable,
    ) -> Option<Symbol> {
        let specialization = (name, t_new);

        if let Some((_, needed)) = self
            .specializations
            .iter()
            .find(|(key, _)| *key == specialization)
        {
            Some(needed.name_new)
        } else {
            let def = self.fenv.iter().find(|(n, _)| *n == name)?.1.clone();
            let name_new = self.symbols.fresh_symbol_named(name);
            let needed_specialization = NeededSpecialization {
                def,
                name_new,
                t_new,
                specialized: None,
            };
            self.specializations
                .push((specialization, needed_specialization));
            Some(name_new)
        }
    }

    fn next_needed_specialization(&mut self) -> Option<&mut NeededSpecialization> {
        self.specializations.iter_mut().find_map(|(_, ns)| {
            if ns.specialized.is_none() {
                Some(ns)
            } else {
                None
            }
        })
    }

    fn solved_specializations(&self) -> Vec<expr::Def> {
        self.specializations
            .iter()
            .filter_map(|(_, ns)| ns.specialized.clone())
            .collect()
    }
}

fn specialize_expr(
    ctx: &mut Context,
    ty_cache: &mut Subs,
    mono_cache: &mut MonoCache,
    expr: &Expr,
) -> Expr {
    match expr {
        Expr::Var(x) => {
            if let Some(y) = ctx
                .specializations
                .specialize_fn(mono_cache, *x, expr.get_type())
            {
                Expr::Var(y)
            } else {
                expr.clone()
            }
        }
        Expr::Int(i) => Expr::Int(*i),
        Expr::Str(s) => Expr::Str(s.clone()),
        Expr::Tag(t, args) => {
            let new_args = args
                .iter()
                .map(|a| specialize_expr(ctx, ty_cache, mono_cache, a))
                .collect();
            Expr::Tag(*t, new_args)
        }
        Expr::Record(fields) => {
            let new_fields = fields
                .iter()
                .map(|(f, e)| (*f, specialize_expr(ctx, ty_cache, mono_cache, e)))
                .collect();
            Expr::Record(new_fields)
        }
        Expr::Access(e, f) => {
            Expr::Access(Box::new(specialize_expr(ctx, ty_cache, mono_cache, e)), *f)
        }
        Expr::Let(def, rest) => {
            let new_def = match def {
                expr::Def::Letfn(f) => expr::Def::Letfn(FunctionDef {
                    recursive: f.recursive,
                    bind: (
                        mono_cache.monomorphize_var(ty_cache, &mut Vec::new(), f.bind.0),
                        f.bind.1,
                    ),
                    arg: (
                        mono_cache.monomorphize_var(ty_cache, &mut Vec::new(), f.arg.0),
                        f.arg.1,
                    ),
                    body: Box::new(specialize_expr(ctx, ty_cache, mono_cache, &f.body)),
                }),
                expr::Def::Letval(v) => expr::Def::Letval(expr::Letval {
                    bind: (
                        mono_cache.monomorphize_var(ty_cache, &mut Vec::new(), v.bind.0),
                        v.bind.1,
                    ),
                    body: Box::new(specialize_expr(ctx, ty_cache, mono_cache, &v.body)),
                }),
            };
            let new_rest = Box::new(specialize_expr(ctx, ty_cache, mono_cache, rest));
            Expr::Let(Box::new(new_def), new_rest)
        }
        Expr::Clos { arg, body } => {
            let new_arg = (
                mono_cache.monomorphize_var(ty_cache, &mut Vec::new(), arg.0),
                arg.1,
            );
            let new_body = Box::new(specialize_expr(ctx, ty_cache, mono_cache, body));
            Expr::Clos {
                arg: new_arg,
                body: new_body,
            }
        }
        Expr::Call(f, a) => {
            let new_f = Box::new(specialize_expr(ctx, ty_cache, mono_cache, f));
            let new_a = Box::new(specialize_expr(ctx, ty_cache, mono_cache, a));
            Expr::Call(new_f, new_a)
        }
        Expr::KCall(kfn, args) => {
            let new_args = args
                .iter()
                .map(|a| specialize_expr(ctx, ty_cache, mono_cache, a))
                .collect();
            Expr::KCall(*kfn, new_args)
        }
        Expr::When(e, branches) => {
            let new_e = Box::new(specialize_expr(ctx, ty_cache, mono_cache, e));
            let new_branches = branches
                .iter()
                .map(|(p, e)| {
                    let new_p = specialize_pattern(ctx, ty_cache, mono_cache, p);
                    let new_e = specialize_expr(ctx, ty_cache, mono_cache, e);
                    (new_p, new_e)
                })
                .collect();
            Expr::When(new_e, new_branches)
        }
    }
}

fn specialize_pattern(
    ctx: &mut Context,
    ty_cache: &mut Subs,
    mono_cache: &mut MonoCache,
    pattern: &expr::Pattern,
) -> expr::Pattern {
    match pattern {
        expr::Pattern::PVar(x) => expr::Pattern::PVar(*x),
        expr::Pattern::PTag(tag, args) => {
            let new_args = args
                .iter()
                .map(|a| specialize_pattern(ctx, ty_cache, mono_cache, a))
                .collect();
            expr::Pattern::PTag(*tag, new_args)
        }
    }
}

fn specialize_let_fn(
    ctx: &mut Context,
    ty_cache: &mut Subs,
    mono_cache: &mut MonoCache,
    t_new: Variable,
    name_new: Symbol,
    f: &expr::FunctionDef,
) -> expr::Def {
    mono_cache.monomorphize_var(ty_cache, &mut Vec::new(), t_new);
    let t = mono_cache.monomorphize_var(ty_cache, &mut Vec::new(), f.bind.0);
    let t_arg = mono_cache.monomorphize_var(ty_cache, &mut Vec::new(), f.arg.0);
    let body = specialize_expr(ctx, ty_cache, mono_cache, &f.body);

    expr::Def::Letfn(FunctionDef {
        recursive: f.recursive,
        bind: (t, name_new),
        arg: (t_arg, f.arg.1),
        body: Box::new(body),
    })
}

fn specialize_let_val(ctx: &mut Context, v: &expr::Letval) -> expr::Def {
    let mut ty_cache = Subs::new();
    let mut mono_cache = MonoCache::from_subs(&ty_cache);
    let t = mono_cache.monomorphize_var(&mut ty_cache, &mut Vec::new(), v.bind.0);
    let body = specialize_expr(ctx, &mut ty_cache, &mut mono_cache, &v.body);
    expr::Def::Letval(expr::Letval {
        bind: (t, v.bind.1),
        body: Box::new(body),
    })
}

fn specialize_run_def(ctx: &mut Context, run: &expr::Run) -> expr::Run {
    let mut ty_cache = Subs::new();
    let mut mono_cache = MonoCache::from_subs(&ty_cache);
    let t = mono_cache.monomorphize_var(&mut ty_cache, &mut Vec::new(), run.bind.0);
    let body = specialize_expr(ctx, &mut ty_cache, &mut mono_cache, &run.body);
    expr::Run {
        bind: (t, run.bind.1),
        body: Box::new(body),
        ty: run.ty,
    }
}

fn make_context(
    symbols: Symbol,
    fresh_tvar: Box<dyn Fn() -> Variable>,
    program: &expr::Declarations,
) -> Context {
    Context {
        symbols,
        fresh_tvar,
        specializations: Specializations::make(symbols, program),
    }
}

fn loop_specializations(ctx: &mut Context) {
    while let Some(needed) = ctx.specializations.next_needed_specialization() {
        let mut ty_cache = Subs::new();
        let mut mono_cache = MonoCache::from_subs(&ty_cache);
        let def = specialize_let_fn(
            ctx,
            &mut ty_cache,
            &mut mono_cache,
            needed.t_new,
            needed.name_new,
            &needed.def,
        );
        needed.specialized = Some(def);
    }
}

pub fn lower(ctx: &mut Context, program: &expr::Declarations) -> expr::Declarations {
    let mut new_program = expr::Declarations::new();

    for (idx, tag) in program.iter_top_down() {
        match tag {
            expr::DeclarationTag::Value => {
                let def = specialize_let_val(ctx, &program.expressions[idx]);
                new_program.push_def(def);
            }
            expr::DeclarationTag::Run(run_idx) => {
                let run = specialize_run_def(ctx, &program.expressions[run_idx.index()]);
                new_program.push_run(run);
            }
            _ => {}
        }
    }

    loop_specializations(ctx);

    let other_defs = ctx.specializations.solved_specializations();
    new_program.extend(other_defs);

    new_program
}
