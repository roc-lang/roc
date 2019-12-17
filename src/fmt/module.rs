use crate::fmt::spaces::{fmt_spaces, INDENT};
use crate::parse::ast::{AppHeader, ExposesEntry, ImportsEntry, InterfaceHeader, Module};
use crate::region::Located;
use bumpalo::collections::{String, Vec};

pub fn fmt_module<'a>(buf: &mut String<'a>, module: &'a Module<'a>) {
    match module {
        Module::Interface { header } => {
            fmt_interface_header(buf, header);
        }
        Module::App { header } => {
            fmt_app_header(buf, header);
        }
    }
}

pub fn fmt_interface_header<'a>(buf: &mut String<'a>, header: &'a InterfaceHeader<'a>) {
    let indent = INDENT;

    buf.push_str("interface");

    // module name
    if header.after_interface.is_empty() {
        buf.push(' ');
    } else {
        fmt_spaces(buf, header.after_interface.iter(), indent);
    }

    buf.push_str(header.name.value.as_str());

    // exposes
    if header.before_exposes.is_empty() {
        buf.push(' ');
    } else {
        fmt_spaces(buf, header.before_exposes.iter(), indent);
    }

    buf.push_str("exposes");

    if header.after_exposes.is_empty() {
        buf.push(' ');
    } else {
        fmt_spaces(buf, header.after_exposes.iter(), indent);
    }

    fmt_exposes(buf, &header.exposes, indent);

    // imports
    if header.before_imports.is_empty() {
        buf.push(' ');
    } else {
        fmt_spaces(buf, header.before_imports.iter(), indent);
    }

    buf.push_str("imports");

    if header.after_imports.is_empty() {
        buf.push(' ');
    } else {
        fmt_spaces(buf, header.after_imports.iter(), indent);
    }

    fmt_imports(buf, &header.imports, indent);
}

pub fn fmt_app_header<'a>(buf: &mut String<'a>, header: &'a AppHeader<'a>) {
    let indent = INDENT;

    buf.push_str("app");

    // imports
    fmt_spaces(buf, header.before_imports.iter(), indent);
    fmt_imports(buf, &header.imports, indent);
    fmt_spaces(buf, header.after_imports.iter(), indent);
}

fn fmt_imports<'a>(
    buf: &mut String<'a>,
    loc_entries: &'a Vec<'a, Located<ImportsEntry<'a>>>,
    indent: u16,
) {
    buf.push('[');

    if !loc_entries.is_empty() {
        buf.push(' ');
    }

    for (index, loc_entry) in loc_entries.iter().enumerate() {
        if index > 0 {
            buf.push_str(", ");
        }

        fmt_imports_entry(buf, &loc_entry.value, indent);
    }

    if !loc_entries.is_empty() {
        buf.push(' ');
    }

    buf.push(']');
}

fn fmt_exposes<'a>(
    buf: &mut String<'a>,
    loc_entries: &'a Vec<'a, Located<ExposesEntry<'a>>>,
    indent: u16,
) {
    buf.push('[');

    if !loc_entries.is_empty() {
        buf.push(' ');
    }

    for (index, loc_entry) in loc_entries.iter().enumerate() {
        if index > 0 {
            buf.push_str(", ");
        }

        fmt_exposes_entry(buf, &loc_entry.value, indent);
    }

    if !loc_entries.is_empty() {
        buf.push(' ');
    }

    buf.push(']');
}

fn fmt_exposes_entry<'a>(buf: &mut String<'a>, entry: &'a ExposesEntry<'a>, indent: u16) {
    use crate::parse::ast::ExposesEntry::*;

    match entry {
        Ident(ident) => buf.push_str(ident),

        SpaceBefore(sub_entry, spaces) => {
            fmt_spaces(buf, spaces.iter(), indent);
            fmt_exposes_entry(buf, sub_entry, indent);
        }
        SpaceAfter(sub_entry, spaces) => {
            fmt_exposes_entry(buf, sub_entry, indent);
            fmt_spaces(buf, spaces.iter(), indent);
        }
    }
}

fn fmt_imports_entry<'a>(buf: &mut String<'a>, entry: &'a ImportsEntry<'a>, indent: u16) {
    use crate::parse::ast::ImportsEntry::*;

    match entry {
        Module(module, loc_exposes_entries) => {
            buf.push_str(module.as_str());

            if !loc_exposes_entries.is_empty() {
                buf.push_str(".{ ");

                for (index, loc_entry) in loc_exposes_entries.iter().enumerate() {
                    if index > 0 {
                        buf.push_str(", ");
                    }

                    fmt_exposes_entry(buf, &loc_entry.value, indent);
                }

                buf.push_str(" }");
            }
        }

        SpaceBefore(sub_entry, spaces) => {
            fmt_spaces(buf, spaces.iter(), indent);
            fmt_imports_entry(buf, sub_entry, indent);
        }
        SpaceAfter(sub_entry, spaces) => {
            fmt_imports_entry(buf, sub_entry, indent);
            fmt_spaces(buf, spaces.iter(), indent);
        }
    }
}
