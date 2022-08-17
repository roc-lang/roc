use roc_module::ident::Lowercase;
use roc_types::subs::{Content, Subs, Variable};

use crate::DeriveError;

pub(crate) fn check_derivable_ext_var(
    subs: &Subs,
    ext_var: Variable,
    is_empty_ext: impl Fn(&Content) -> bool,
) -> Result<(), DeriveError> {
    let ext_content = subs.get_content_without_compacting(ext_var);
    if is_empty_ext(ext_content)
        || matches!(
            ext_content,
            // It's fine to have either a flex/rigid or flex-able/rigid-able in the extension.
            // Since we don't know the rest of the type concretely, any implementation (derived or
            // not) would only be able to work on the concrete part of the type regardless. So,
            // just admit them, and they will be excluded from the deriving scheme.
            Content::FlexVar(_)
                | Content::FlexAbleVar(..)
                | Content::RigidVar(_)
                | Content::RigidAbleVar(..)
        )
    {
        Ok(())
    } else {
        match ext_content {
            Content::FlexVar(_) => Err(DeriveError::UnboundVar),
            _ => Err(DeriveError::Underivable),
        }
    }
}

pub(crate) fn debug_name_record(fields: &[Lowercase]) -> String {
    let mut str = String::from('{');
    fields.iter().enumerate().for_each(|(i, f)| {
        if i > 0 {
            str.push(',');
        }
        str.push_str(f.as_str());
    });
    str.push('}');
    str
}
