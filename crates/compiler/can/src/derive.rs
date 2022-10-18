//! Derives parse trees for ability member impls of Opaques.
//! These are derived at canonicalization time rather than type-checking time,
//! as structural types are, due to the following reasons:
//!   - Derived impls for opaques are not generalizable, and hence cannot be owned by the Derived
//!     module, because they may require immediate specialization unknown to the Derived module.
//!   - Derived impls for opaques are typically very small, effectively deferring the
//!     implementation to the value they wrap.

use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_parse::ast;
use roc_region::all::{Loc, Region};

use crate::{env::Env, pattern::Pattern, scope::Scope};

fn to_encoder<'a>(env: &mut Env<'a>, at_opaque: &'a str) -> ast::Expr<'a> {
    let alloc_pat = |it| env.arena.alloc(Loc::at(DERIVED_REGION, it));
    let alloc_expr = |it| env.arena.alloc(Loc::at(DERIVED_REGION, it));

    let payload = env.arena.alloc_str("#payload");

    // \@Opaq payload
    let opaque_ref = alloc_pat(ast::Pattern::OpaqueRef(at_opaque));
    let opaque_apply_pattern = ast::Pattern::Apply(
        opaque_ref,
        &*env
            .arena
            .alloc([Loc::at(DERIVED_REGION, ast::Pattern::Identifier(payload))]),
    );

    // Encode.toEncoder payload
    let call_member = alloc_expr(ast::Expr::Apply(
        alloc_expr(ast::Expr::Var {
            module_name: "Encode",
            ident: "toEncoder",
        }),
        &*env.arena.alloc([&*alloc_expr(ast::Expr::Var {
            module_name: "",
            ident: payload,
        })]),
        roc_module::called_via::CalledVia::Space,
    ));

    // \@Opaq payload -> Encode.toEncoder payload
    ast::Expr::Closure(
        env.arena
            .alloc([Loc::at(DERIVED_REGION, opaque_apply_pattern)]),
        call_member,
    )
}

fn hash<'a>(env: &mut Env<'a>, at_opaque: &'a str) -> ast::Expr<'a> {
    let alloc_pat = |it| env.arena.alloc(Loc::at(DERIVED_REGION, it));
    let alloc_expr = |it| env.arena.alloc(Loc::at(DERIVED_REGION, it));
    let hasher = env.arena.alloc_str("#hasher");

    let payload = env.arena.alloc_str("#payload");

    // \@Opaq payload
    let opaque_ref = alloc_pat(ast::Pattern::OpaqueRef(at_opaque));
    let opaque_apply_pattern = ast::Pattern::Apply(
        opaque_ref,
        &*env
            .arena
            .alloc([Loc::at(DERIVED_REGION, ast::Pattern::Identifier(payload))]),
    );

    // Hash.hash hasher payload
    let call_member = alloc_expr(ast::Expr::Apply(
        alloc_expr(ast::Expr::Var {
            module_name: "Hash",
            ident: "hash",
        }),
        &*env.arena.alloc([
            &*alloc_expr(ast::Expr::Var {
                module_name: "",
                ident: hasher,
            }),
            &*alloc_expr(ast::Expr::Var {
                module_name: "",
                ident: payload,
            }),
        ]),
        roc_module::called_via::CalledVia::Space,
    ));

    // \hasher, @Opaq payload -> Hash.hash hasher payload
    ast::Expr::Closure(
        env.arena.alloc([
            Loc::at(DERIVED_REGION, ast::Pattern::Identifier(hasher)),
            Loc::at(DERIVED_REGION, opaque_apply_pattern),
        ]),
        call_member,
    )
}

fn is_eq<'a>(env: &mut Env<'a>, at_opaque: &'a str) -> ast::Expr<'a> {
    let alloc_pat = |it| env.arena.alloc(Loc::at(DERIVED_REGION, it));
    let alloc_expr = |it| env.arena.alloc(Loc::at(DERIVED_REGION, it));

    let payload1 = env.arena.alloc_str("#payload1");
    let payload2 = env.arena.alloc_str("#payload2");

    let opaque_ref = alloc_pat(ast::Pattern::OpaqueRef(at_opaque));
    // \@Opaq payload1
    let opaque1 = ast::Pattern::Apply(
        opaque_ref,
        &*env
            .arena
            .alloc([Loc::at(DERIVED_REGION, ast::Pattern::Identifier(payload1))]),
    );
    // \@Opaq payload2
    let opaque2 = ast::Pattern::Apply(
        opaque_ref,
        &*env
            .arena
            .alloc([Loc::at(DERIVED_REGION, ast::Pattern::Identifier(payload2))]),
    );

    // Bool.isEq payload1 payload2
    let call_member = alloc_expr(ast::Expr::Apply(
        alloc_expr(ast::Expr::Var {
            module_name: "Bool",
            ident: "isEq",
        }),
        &*env.arena.alloc([
            &*alloc_expr(ast::Expr::Var {
                module_name: "",
                ident: payload1,
            }),
            &*alloc_expr(ast::Expr::Var {
                module_name: "",
                ident: payload2,
            }),
        ]),
        roc_module::called_via::CalledVia::Space,
    ));

    // \@Opaq payload1, @Opaq payload2 -> Bool.isEq payload1 payload2
    ast::Expr::Closure(
        env.arena.alloc([
            Loc::at(DERIVED_REGION, opaque1),
            Loc::at(DERIVED_REGION, opaque2),
        ]),
        call_member,
    )
}

pub(crate) const DERIVED_REGION: Region = Region::zero();

pub(crate) fn synthesize_member_impl<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    opaque: Symbol,
    opaque_name: &'a str,
    ability_member: Symbol,
) -> (Symbol, Loc<Pattern>, &'a Loc<ast::Expr<'a>>) {
    // @Opaq
    let at_opaque = env.arena.alloc_str(&format!("@{}", opaque_name));

    let (impl_name, def_body): (String, ast::Expr<'a>) = match ability_member {
        Symbol::ENCODE_TO_ENCODER => (
            format!("#{}_toEncoder", opaque_name),
            to_encoder(env, at_opaque),
        ),
        Symbol::DECODE_DECODER => (format!("#{}_decoder", opaque_name), todo!()),
        Symbol::HASH_HASH => (format!("#{}_hash", opaque_name), hash(env, at_opaque)),
        Symbol::BOOL_IS_EQ => (format!("#{}_isEq", opaque_name), is_eq(env, at_opaque)),
        other => internal_error!("{:?} is not a derivable ability member!", other),
    };

    let impl_symbol = scope
        .introduce_str(&impl_name, DERIVED_REGION)
        .expect("this name is not unique");

    let def_pattern = Pattern::Identifier(impl_symbol);

    (
        impl_symbol,
        Loc::at(DERIVED_REGION, def_pattern),
        env.arena.alloc(Loc::at(DERIVED_REGION, def_body)),
    )
}
