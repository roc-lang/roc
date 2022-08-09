use roc_module::ident::Lowercase;
use roc_types::subs::{Content, Subs, Variable};

use crate::DeriveError;

pub(crate) fn check_empty_ext_var(
    subs: &Subs,
    ext_var: Variable,
    is_empty_ext: impl Fn(&Content) -> bool,
) -> Result<(), DeriveError> {
    let ext_content = subs.get_content_without_compacting(ext_var);
    if is_empty_ext(ext_content) {
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
