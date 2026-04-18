//! Auto-derivers of builtin ability methods.

use std::iter::once;
use std::sync::{Arc, Mutex};

use roc_can::abilities::SpecializationLambdaSets;
use roc_can::def::DefKind;
use roc_can::expr::Expr;
use roc_can::pattern::Pattern;
use roc_can::{def::Def, module::ExposedByModule};
use roc_collections::{MutMap, VecMap};
use roc_derive_key::DeriveKey;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_region::all::Loc;
use roc_types::subs::{
    copy_import_to, Content, Descriptor, Mark, OptVariable, Rank, Subs, Variable,
};
use util::Env;

mod decoding;
mod encoding;
mod hash;
mod inspect;
mod util;

pub(crate) const DERIVED_SYNTH: ModuleId = ModuleId::DERIVED_SYNTH;

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

/// Map of [`DeriveKey`]s to their derived symbols.
///
/// This represents the [`Derived_synth`][Symbol::DERIVED_SYNTH] module.
#[derive(Debug, Default)]
pub struct DerivedModule {
    map: MutMap<DeriveKey, (Symbol, Def, SpecializationLambdaSets)>,
    subs: Subs,
    derived_ident_ids: IdentIds,
}

pub(crate) struct DerivedBody {
    pub body: Expr,
    pub body_type: Variable,
    /// mapping of lambda set region -> the specialization lambda set for this derived body
    pub specialization_lambda_sets: SpecializationLambdaSets,
}

fn build_derived_body(
    derived_subs: &mut Subs,
    derived_ident_ids: &mut IdentIds,
    exposed_by_module: &ExposedByModule,
    derived_symbol: Symbol,
    derive_key: DeriveKey,
) -> (Def, SpecializationLambdaSets) {
    let mut env = Env {
        subs: derived_subs,
        exposed_types: exposed_by_module,
        derived_ident_ids,
    };

    let DerivedBody {
        body,
        body_type,
        specialization_lambda_sets,
    } = match derive_key {
        DeriveKey::ToEncoder(to_encoder_key) => {
            encoding::derive_to_encoder(&mut env, to_encoder_key, derived_symbol)
        }
        DeriveKey::Decoder(decoder_key) => {
            decoding::derive_decoder(&mut env, decoder_key, derived_symbol)
        }
        DeriveKey::Hash(hash_key) => hash::derive_hash(&mut env, hash_key, derived_symbol),
        DeriveKey::ToInspector(to_inspector_key) => {
            inspect::derive_to_inspector(&mut env, to_inspector_key, derived_symbol)
        }
    };

    let def = Def {
        loc_pattern: Loc::at_zero(Pattern::Identifier(derived_symbol)),
        loc_expr: Loc::at_zero(body),
        expr_var: body_type,
        pattern_vars: once((derived_symbol, body_type)).collect(),
        annotation: None,
        kind: DefKind::Let,
    };

    (def, specialization_lambda_sets)
}

impl DerivedModule {
    pub fn get_or_insert(
        &mut self,
        // TODO: we only need "exposed by builtin modules that expose builtin abilities"
        exposed_by_module: &ExposedByModule,
        key: DeriveKey,
    ) -> &(Symbol, Def, SpecializationLambdaSets) {
        if let Some(entry) = self.map.get(&key) {
            // rustc won't let us return an immutable reference *and* continue using
            // `self.map` immutably below, but this is safe, because we are not returning
            // an immutable reference to the entry.
            return unsafe { std::mem::transmute(entry) };
        }

        let ident_id = if cfg!(debug_assertions) || cfg!(feature = "debug-derived-symbols") {
            let debug_name = key.debug_name();
            let ident_id = self.derived_ident_ids.get_or_insert(&debug_name);

            // This is expensive, but yields much better symbols when debugging.
            // TODO: hide behind debug_flags?
            DERIVED_SYNTH.register_debug_idents(&self.derived_ident_ids);

            ident_id
        } else {
            self.derived_ident_ids.gen_unique()
        };

        let derived_symbol = Symbol::new(DERIVED_SYNTH, ident_id);
        let (derived_def, specialization_lsets) = build_derived_body(
            &mut self.subs,
            &mut self.derived_ident_ids,
            exposed_by_module,
            derived_symbol,
            key.clone(),
        );

        let triple = (derived_symbol, derived_def, specialization_lsets);
        self.map.entry(key).or_insert(triple)
    }

    pub fn is_derived_def(&self, def_symbol: Symbol) -> bool {
        self.map
            .iter()
            .any(|(_, (symbol, _, _))| *symbol == def_symbol)
    }

    pub fn iter_all(
        &self,
    ) -> impl Iterator<Item = (&DeriveKey, &(Symbol, Def, SpecializationLambdaSets))> {
        self.map.iter()
    }

    /// Generate a unique symbol. This should only be used when generating code inside the Derived
    /// module; other modules should use [`Self::get_or_insert`] to generate a symbol for a derived
    /// ability member usage.
    pub fn gen_unique(&mut self) -> Symbol {
        let ident_id = self.derived_ident_ids.gen_unique();
        Symbol::new(DERIVED_SYNTH, ident_id)
    }

    // TODO: just pass and copy the ambient function directly, don't pass the lambda set var.
    pub fn copy_lambda_set_ambient_function_to_subs(
        &self,
        lambda_set_var: Variable,
        target: &mut Subs,
        _target_rank: Rank,
    ) -> Variable {
        let ambient_function_var = self.subs.get_lambda_set(lambda_set_var).ambient_function;

        let copied_import = copy_import_to(
            &self.subs,
            target,
            // bookkeep unspecialized lambda sets of var - I think we want this here
            true,
            ambient_function_var,
            // TODO: I think this is okay because the only use of `copy_lambda_set_var_to_subs`
            // (at least right now) is for lambda set compaction, which will automatically unify
            // and lower ranks, and never generalize.
            //
            // However this is a bad coupling and maybe not a good assumption, we should revisit
            // this when possible.
            Rank::import(),
            // target_rank,
        );

        copied_import.variable
    }

    /// Gets the derived defs that should be loaded into the derived gen module, skipping over the
    /// defs that have already been loaded.
    pub fn iter_load_for_gen_module(
        &mut self,
        gen_subs: &mut Subs,
        should_load_def: impl Fn(Symbol) -> bool,
    ) -> VecMap<Symbol, (Expr, Variable)> {
        self.map
            .values()
            .filter_map(|(symbol, def, _)| {
                if should_load_def(*symbol) {
                    let (new_expr_var, new_expr) = roc_can::copy::deep_copy_expr_across_subs(
                        &mut self.subs,
                        gen_subs,
                        def.expr_var,
                        &def.loc_expr.value,
                    );
                    Some((*symbol, (new_expr, new_expr_var)))
                } else {
                    None
                }
            })
            .collect()
    }

    /// # Safety
    ///
    /// Prefer using a fresh Derived module with [`Derived::default`]. Use this only in testing.
    pub unsafe fn from_components(subs: Subs, ident_ids: IdentIds) -> Self {
        Self {
            map: Default::default(),
            subs,
            derived_ident_ids: ident_ids,
        }
    }

    pub fn decompose(self) -> (Subs, IdentIds) {
        (self.subs, self.derived_ident_ids)
    }
}

/// Thread-sharable [`DerivedModule`].
pub type SharedDerivedModule = Arc<Mutex<DerivedModule>>;
