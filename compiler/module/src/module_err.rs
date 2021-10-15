use snafu::{Backtrace, Snafu};

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
}

pub type ModuleResult<T, E = ModuleError> = std::result::Result<T, E>;
