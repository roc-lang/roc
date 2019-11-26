use bumpalo::collections::{String, Vec};
use fmt::def::fmt_def;
use fmt::spaces::{fmt_spaces, INDENT};
use parse::ast::{AppHeader, ExposesEntry, ImportsEntry, InterfaceHeader, Module};
use region::Located;

pub fn fmt_module<'a>(buf: &mut String<'a>, module: &'a Module<'a>) {
    match module {
        Module::Interface { header, defs } => {
            fmt_interface_header(buf, header);

            for loc_def in defs {
                fmt_def(buf, &loc_def.value, 0);
            }
        }
        Module::App { header, defs } => {
            fmt_app_header(buf, header);
            for loc_def in defs {
                fmt_def(buf, &loc_def.value, 0);
            }
        }
    }
}

pub fn fmt_interface_header<'a>(buf: &mut String<'a>, header: &'a InterfaceHeader<'a>) {
    buf.push_str("interface");

    // module name
    if header.after_interface.is_empty() {
        buf.push(' ');
    } else {
        fmt_spaces(buf, header.after_interface.iter(), INDENT);
    }

    buf.push_str(header.name.value.as_str());

    // exposes
    if header.before_exposes.is_empty() {
        buf.push(' ');
    } else {
        fmt_spaces(buf, header.before_exposes.iter(), INDENT);
    }

    buf.push_str("exposes");

    if header.after_exposes.is_empty() {
        buf.push(' ');
    } else {
        fmt_spaces(buf, header.after_exposes.iter(), INDENT);
    }

    fmt_exposes(buf, &header.exposes);

    // imports
    if header.before_imports.is_empty() {
        buf.push(' ');
    } else {
        fmt_spaces(buf, header.before_imports.iter(), INDENT);
    }

    buf.push_str("imports");

    if header.after_imports.is_empty() {
        buf.push(' ');
    } else {
        fmt_spaces(buf, header.after_imports.iter(), INDENT);
    }

    fmt_imports(buf, &header.imports);
}

pub fn fmt_app_header<'a>(buf: &mut String<'a>, header: &'a AppHeader<'a>) {
    buf.push_str("app");

    // imports
    fmt_spaces(buf, header.before_imports.iter(), INDENT);
    fmt_imports(buf, &header.imports);
    fmt_spaces(buf, header.after_imports.iter(), INDENT);
}

fn fmt_imports<'a>(buf: &mut String<'a>, loc_entries: &'a Vec<'a, Located<ImportsEntry<'a>>>) {
    buf.push('[');

    if !loc_entries.is_empty() {
        buf.push(' ');
    }

    for loc_entry in loc_entries {
        fmt_imports_entry(buf, &loc_entry.value);
    }

    if !loc_entries.is_empty() {
        buf.push(' ');
    }

    buf.push(']');
}

fn fmt_imports_entry<'a>(buf: &mut String<'a>, entry: &'a ImportsEntry<'a>) {
    panic!("TODO fmt import entry");
}

fn fmt_exposes<'a>(buf: &mut String<'a>, loc_entries: &'a Vec<'a, Located<ExposesEntry<'a>>>) {
    buf.push('[');

    if !loc_entries.is_empty() {
        buf.push(' ');
    }

    for loc_entry in loc_entries {
        fmt_exposes_entry(buf, &loc_entry.value);
    }

    if !loc_entries.is_empty() {
        buf.push(' ');
    }

    buf.push(']');
}

fn fmt_exposes_entry<'a>(buf: &mut String<'a>, entry: &'a ExposesEntry<'a>) {
    panic!("TODO fmt import entry");
}
