//! Derives parse trees for ability member impls of Opaques.
//! These are derived at canonicalization time rather than type-checking time,
//! as structural types are, due to the following reasons:
//!   - Derived impls for opaques are not generalizable, and hence cannot be owned by the Derived
//!     module, because they may require immediate specialization unknown to the Derived module.
//!   - Derived impls for opaques are typically very small, effectively deferring the
//!     implementation to the value they wrap.

use roc_error_macros::internal_error;
use roc_module::{called_via::CalledVia, symbol::Symbol};
use roc_parse::ast;
use roc_region::all::{Loc, Region};

use crate::{env::Env, pattern::Pattern, scope::Scope};

fn to_encoder<'a>(env: &mut Env<'a>, at_opaque: &'a str) -> ast::Expr<'a> {
    let alloc_pat = |it| env.arena.alloc(Loc::at(DERIVED_REGION, it));
    let alloc_expr = |it| env.arena.alloc(Loc::at(DERIVED_REGION, it));

    let payload = "#payload";

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

fn decoder<'a>(env: &mut Env<'a>, at_opaque: &'a str) -> ast::Expr<'a> {
    let alloc_expr = |it| env.arena.alloc(Loc::at(DERIVED_REGION, it));

    let call_custom = {
        let bytes = "#bytes";
        let fmt = "#fmt";

        // Decode.decodeWith bytes Decode.decoder fmt
        let call_decode_with = ast::Expr::Apply(
            alloc_expr(ast::Expr::Var {
                module_name: "Decode",
                ident: "decodeWith",
            }),
            env.arena.alloc([
                &*alloc_expr(ast::Expr::Var {
                    module_name: "",
                    ident: bytes,
                }),
                alloc_expr(ast::Expr::Var {
                    module_name: "Decode",
                    ident: "decoder",
                }),
                alloc_expr(ast::Expr::Var {
                    module_name: "",
                    ident: fmt,
                }),
            ]),
            CalledVia::Space,
        );

        // Decode.mapResult (Decode.decodeWith bytes Decode.decoder fmt) @Opaq
        let call_map_result = ast::Expr::Apply(
            alloc_expr(ast::Expr::Var {
                module_name: "Decode",
                ident: "mapResult",
            }),
            env.arena.alloc([
                &*alloc_expr(call_decode_with),
                alloc_expr(ast::Expr::OpaqueRef(at_opaque)),
            ]),
            CalledVia::Space,
        );

        // \bytes, fmt ->
        //     Decode.mapResult (Decode.decodeWith bytes Decode.decoder fmt) @Opaq
        let custom_closure = ast::Expr::Closure(
            env.arena.alloc([
                Loc::at(DERIVED_REGION, ast::Pattern::Identifier(bytes)),
                Loc::at(DERIVED_REGION, ast::Pattern::Identifier(fmt)),
            ]),
            alloc_expr(call_map_result),
        );

        // Decode.custom \bytes, fmt -> ...
        ast::Expr::Apply(
            alloc_expr(ast::Expr::Var {
                module_name: "Decode",
                ident: "custom",
            }),
            env.arena.alloc([&*alloc_expr(custom_closure)]),
            CalledVia::Space,
        )
    };

    call_custom
}

fn hash<'a>(env: &mut Env<'a>, at_opaque: &'a str) -> ast::Expr<'a> {
    let alloc_pat = |it| env.arena.alloc(Loc::at(DERIVED_REGION, it));
    let alloc_expr = |it| env.arena.alloc(Loc::at(DERIVED_REGION, it));
    let hasher = "#hasher";

    let payload = "#payload";

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

    let payload1 = "#payload1";
    let payload2 = "#payload2";

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

pub const DERIVED_REGION: Region = Region::zero();

pub(crate) fn synthesize_member_impl<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    opaque_name: &'a str,
    ability_member: Symbol,
) -> (Symbol, Loc<Pattern>, &'a Loc<ast::Expr<'a>>) {
    // @Opaq
    let at_opaque = env.arena.alloc_str(&format!("@{opaque_name}"));

    let (impl_name, def_body): (String, ast::Expr<'a>) = match ability_member {
        Symbol::ENCODE_TO_ENCODER => (
            format!("#{opaque_name}_toEncoder"),
            to_encoder(env, at_opaque),
        ),
        Symbol::DECODE_DECODER => (format!("#{opaque_name}_decoder"), decoder(env, at_opaque)),
        Symbol::HASH_HASH => (format!("#{opaque_name}_hash"), hash(env, at_opaque)),
        Symbol::BOOL_IS_EQ => (format!("#{opaque_name}_isEq"), is_eq(env, at_opaque)),
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
