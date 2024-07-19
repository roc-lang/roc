use std::time::Duration;

use object::ObjectSymbol;

pub(crate) fn report_timing(label: &str, duration: Duration) {
    println!("\t{:9.3} ms   {}", duration.as_secs_f64() * 1000.0, label,);
}

fn is_roc_symbol(sym: &object::Symbol) -> bool {
    if let Ok(name) = sym.name() {
        name.trim_start_matches('_').starts_with("roc_")
    } else {
        false
    }
}

pub(crate) fn is_roc_definition(sym: &object::Symbol) -> bool {
    sym.is_definition() && is_roc_symbol(sym)
}

pub(crate) fn is_roc_undefined(sym: &object::Symbol) -> bool {
    sym.is_undefined() && is_roc_symbol(sym)
}
