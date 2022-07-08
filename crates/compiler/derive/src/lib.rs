//! Auto-derivers of builtin ability methods.

use std::iter::once;
use std::sync::{Arc, Mutex};

use roc_can::abilities::SpecializationLambdaSets;
use roc_can::expr::Expr;
use roc_can::pattern::Pattern;
use roc_can::{def::Def, module::ExposedByModule};
use roc_collections::MutMap;
use roc_derive_key::DeriveKey;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_region::all::Loc;
use roc_types::subs::{
    copy_import_to, Content, Descriptor, Mark, OptVariable, Rank, Subs, Variable,
};

mod encoding;

pub(crate) const DERIVED_MODULE: ModuleId = ModuleId::DERIVED;

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
#[derive(Debug, Default)]
pub struct DerivedModule {
    map: MutMap<DeriveKey, (Symbol, Def, SpecializationLambdaSets)>,
    subs: Subs,
    derived_ident_ids: IdentIds,

    /// Has someone stolen subs/ident ids from us?
    #[cfg(debug_assertions)]
    stolen: bool,
}

pub struct StolenFromDerived {
    pub subs: Subs,
    pub ident_ids: IdentIds,
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
    let DerivedBody {
        body,
        body_type,
        specialization_lambda_sets,
    } = match derive_key {
        DeriveKey::ToEncoder(to_encoder_key) => {
            let mut env = encoding::Env {
                subs: derived_subs,
                exposed_types: exposed_by_module,
                derived_ident_ids,
            };
            encoding::derive_to_encoder(&mut env, to_encoder_key, derived_symbol)
        }
        DeriveKey::Decoding => todo!(),
    };

    let def = Def {
        loc_pattern: Loc::at_zero(Pattern::Identifier(derived_symbol)),
        loc_expr: Loc::at_zero(body),
        expr_var: body_type,
        pattern_vars: once((derived_symbol, body_type)).collect(),
        annotation: None,
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
        #[cfg(debug_assertions)]
        {
            debug_assert!(!self.stolen, "attempting to add to stolen symbols!");
        }

        match self.map.get(&key) {
            Some(entry) => {
                // Might happen that during rollback in specializing the Derived module, we wink
                // away the variables associated with a derived impl. In this case, just rebuild
                // the impl.
                dbg!(entry.0);
                if entry.2.iter().all(|(_, var)| self.subs.contains(*var)) {
                    // rustc won't let us return an immutable reference *and* continue using
                    // `self.map` immutably below, but this is safe, because we are not returning
                    // an immutable reference to the entry.
                    return unsafe { std::mem::transmute(entry) };
                }
                dbg!(entry.0);
            }
            None => {}
        }

        let ident_id = if cfg!(debug_assertions) || cfg!(feature = "debug-derived-symbols") {
            let debug_name = key.debug_name();
            // debug_assert!(
            //     self.derived_ident_ids.get_id(&debug_name).is_none(),
            //     "duplicate debug name for different derive key"
            // );
            let ident_id = self.derived_ident_ids.get_or_insert(&debug_name);

            // This is expensive, but yields much better symbols when debugging.
            // TODO: hide behind debug_flags?
            DERIVED_MODULE.register_debug_idents(&self.derived_ident_ids);

            ident_id
        } else {
            // TODO this is WRONG when we're re-instantiating the derived impl
            self.derived_ident_ids.gen_unique()
        };

        let derived_symbol = Symbol::new(DERIVED_MODULE, ident_id);
        let (derived_def, specialization_lsets) = build_derived_body(
            &mut self.subs,
            &mut self.derived_ident_ids,
            exposed_by_module,
            derived_symbol,
            key.clone(),
        );

        let triple = (derived_symbol, derived_def, specialization_lsets);
        self.map.remove(&key);
        self.map.entry(key).or_insert(triple)
    }

    /// TERRIBLE HACK to deal with rollbacks: in specializing the Derived module, we snapshot and
    /// rollback Subs every time we specialize a particular procedure. However in specializing a
    /// procedure we may have added a new derived impl to the [`DerivedModule`] whose types are
    /// then rolled back! Hence, we need to re-initialize the derived impl in such cases.
    ///
    /// But there must be a better way!!
    pub fn refresh_stale_specializations(&mut self, exposed_by_module: &ExposedByModule) {
        let all_derived: Vec<_> = self.map.keys().cloned().collect();
        //dbg!(&all_derived);
        all_derived
            .into_iter()
            // get_or_insert will handle re-initializing stale defs
            .for_each(|derive_key| {
                let _ = self.get_or_insert(exposed_by_module, derive_key);
            });
    }

    pub fn iter_all(
        &self,
    ) -> impl Iterator<Item = (&DeriveKey, &(Symbol, Def, SpecializationLambdaSets))> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(!self.stolen);
        }

        self.map.iter()
    }

    /// Generate a unique symbol. This should only be used when generating code inside the Derived
    /// module; other modules should use [`Self::get_or_insert`] to generate a symbol for a derived
    /// ability member usage.
    pub fn gen_unique(&mut self) -> Symbol {
        #[cfg(debug_assertions)]
        {
            debug_assert!(!self.stolen);
        }

        let ident_id = self.derived_ident_ids.gen_unique();
        Symbol::new(DERIVED_MODULE, ident_id)
    }

    /// Steal all created derived ident Ids.
    /// After this is called, [`Self::get_or_insert`] may no longer be called.
    pub fn steal(&mut self) -> StolenFromDerived {
        let mut ident_ids = Default::default();
        std::mem::swap(&mut self.derived_ident_ids, &mut ident_ids);
        let mut subs = Default::default();
        std::mem::swap(&mut self.subs, &mut subs);

        #[cfg(debug_assertions)]
        {
            debug_assert!(!self.stolen);
            self.stolen = true;
        }

        StolenFromDerived { subs, ident_ids }
    }

    pub fn return_stolen(&mut self, stolen: StolenFromDerived) {
        #[cfg(debug_assertions)]
        {
            debug_assert!(self.stolen);
            self.stolen = false;
        }

        let StolenFromDerived { subs, ident_ids } = stolen;

        self.subs = subs;
        self.derived_ident_ids = ident_ids;
    }

    // TODO: just pass and copy the ambient function directly, don't pass the lambda set var.
    pub fn copy_lambda_set_ambient_function_to_subs(
        &self,
        lambda_set_var: Variable,
        target_subs_home: ModuleId,
        target: &mut Subs,
        target_rank: Rank,
    ) -> Variable {
        #[cfg(debug_assertions)]
        {
            debug_assert!(!self.stolen);
        }

        let ambient_function_var = self.subs.get_lambda_set(lambda_set_var).ambient_function;

        if target_subs_home == ModuleId::DERIVED {
            ambient_function_var
        } else {
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
    }
}

/// Thread-sharable [`DerivedMethods`].
pub type SharedDerivedModule = Arc<Mutex<DerivedModule>>;
