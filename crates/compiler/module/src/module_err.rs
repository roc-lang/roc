use crate::symbol::IdentId;

#[derive(Debug)]
pub enum ModuleError {
    ModuleIdNotFound {
        module_id: String,
        all_ident_ids: String,
    },
    IdentIdNotFound {
        ident_id: IdentId,
        ident_ids_str: String,
    },
}

impl std::fmt::Display for ModuleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ModuleIdNotFound {
                module_id,
                all_ident_ids,
            } => {
                write!(
                    f,
                    "ModuleIdNotFound: I could not find the ModuleId {} in Interns.all_ident_ids: {}.",
                    module_id,
                    all_ident_ids
                )
            }
            Self::IdentIdNotFound {
                ident_id,
                ident_ids_str,
            } => {
                write!(
                    f,
                    "IdentIdNotFound: I could not find IdentId {:?} in ident_ids {:?}.",
                    ident_id, ident_ids_str
                )
            }
        }
    }
}

impl std::error::Error for ModuleError {}

pub type ModuleResult<T, E = ModuleError> = std::result::Result<T, E>;
