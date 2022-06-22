//! Auto-derivers of builtin ability methods.

use std::iter::once;

use bumpalo::Bump;
use roc_can::pattern::Pattern;
use roc_can::{def::Def, module::ExposedByModule};
use roc_derive_key::{DeriveKey, GlobalDerivedSymbols};
use roc_module::symbol::Symbol;
use roc_region::all::Loc;
use roc_types::subs::{Content, Descriptor, Mark, OptVariable, Rank, Subs, Variable};

pub fn synth_var(subs: &mut Subs, content: Content) -> Variable {
    let descriptor = Descriptor {
        content,
        // NOTE: this is incorrect, but that is irrelevant - derivers may only be called during
        // monomorphization (or later), at which point we do not care about variable
        // generalization. Hence ranks should not matter.
        rank: Rank::toplevel(),
        mark: Mark::NONE,
        copy: OptVariable::NONE,
    };
    subs.fresh(descriptor)
}

pub fn build_derived_body(
    subs: &mut Subs,
    arena: &Bump,
    derived_symbols: &GlobalDerivedSymbols,
    exposed_by_module: &ExposedByModule,
    derived_symbol: Symbol,
    derive_key: DeriveKey,
) -> Def {
    let (body, var) = match derive_key {
        DeriveKey::ToEncoder(to_encoder_key) => {
            let mut env = encoding::Env {
                arena,
                subs,
                exposed_types: exposed_by_module,
                derived_symbols,
            };
            encoding::derive_to_encoder(&mut env, to_encoder_key)
        }
        DeriveKey::Decoding => todo!(),
    };

    Def {
        loc_pattern: Loc::at_zero(Pattern::Identifier(derived_symbol)),
        loc_expr: Loc::at_zero(body),
        expr_var: var,
        pattern_vars: once((derived_symbol, var)).collect(),
        annotation: None,
    }
}

pub mod encoding;
