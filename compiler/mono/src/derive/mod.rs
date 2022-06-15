//! Auto-derivers of builtin ability methods.

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

pub mod deriver_hash;

pub mod encoding;
