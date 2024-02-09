use roc_module::symbol::{Interns, ModuleId};
use roc_types::subs::{Subs, Variable};

pub(super) fn format_var_type(
    var: Variable,
    subs: &mut Subs,
    module_id: &ModuleId,
    interns: &Interns,
) -> String {
    let snapshot = subs.snapshot();
    let type_str = roc_types::pretty_print::name_and_print_var(
        var,
        subs,
        *module_id,
        interns,
        roc_types::pretty_print::DebugPrint::NOTHING,
    );
    subs.rollback_to(snapshot);
    type_str
}

pub(super) fn is_roc_identifier_char(char: &char) -> bool {
    matches!(char,'a'..='z'|'A'..='Z'|'0'..='9'|'.')
}
