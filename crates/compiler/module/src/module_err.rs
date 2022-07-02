use snafu::{Backtrace, Snafu};

use crate::symbol::IdentId;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum ModuleError {
    #[snafu(display(
        "ModuleIdNotFound: I could not find the ModuleId {} in Interns.all_ident_ids: {}.",
        module_id,
        all_ident_ids
    ))]
    ModuleIdNotFound {
        module_id: String,
        all_ident_ids: String,
        backtrace: Backtrace,
    },
    #[snafu(display(
        "IdentIdNotFound: I could not find IdentId {:?} in ident_ids {:?}.",
        ident_id,
        ident_ids_str
    ))]
    IdentIdNotFound {
        ident_id: IdentId,
        ident_ids_str: String,
        backtrace: Backtrace,
    },
}

pub type ModuleResult<T, E = ModuleError> = std::result::Result<T, E>;
