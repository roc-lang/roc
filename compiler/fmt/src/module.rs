use crate::annotation::Formattable;
use crate::collection::{fmt_collection, CollectionConfig};
use crate::expr::fmt_str_literal;
use crate::spaces::{fmt_default_spaces, fmt_spaces, INDENT};
use bumpalo::collections::String;
use roc_parse::ast::{Collection, Module};
use roc_parse::header::{
    AppHeader, ExposesEntry, ImportsEntry, InterfaceHeader, PackageEntry, PackageOrPath,
    PlatformHeader, To,
};
use roc_region::all::Located;

pub fn fmt_module<'a>(buf: &mut String<'a>, module: &'a Module<'a>) {
    match module {
        Module::Interface { header } => {
            fmt_interface_header(buf, header);
        }
        Module::App { header } => {
            fmt_app_header(buf, header);
        }
        Module::Platform { header } => {
            fmt_platform_header(buf, header);
        }
    }
}

pub fn fmt_interface_header<'a>(buf: &mut String<'a>, header: &'a InterfaceHeader<'a>) {
    let indent = INDENT;

    buf.push_str("interface");

    // module name
    fmt_default_spaces(buf, header.after_interface_keyword, " ", indent);
    buf.push_str(header.name.value.as_str());

    // exposes
    fmt_default_spaces(buf, header.before_exposes, " ", indent);
    buf.push_str("exposes");
    fmt_default_spaces(buf, header.after_exposes, " ", indent);
    fmt_exposes(buf, &header.exposes, indent);

    // imports
    fmt_default_spaces(buf, header.before_imports, " ", indent);
    buf.push_str("imports");
    fmt_default_spaces(buf, header.after_imports, " ", indent);
    fmt_imports(buf, header.imports, indent);
}

pub fn fmt_app_header<'a>(buf: &mut String<'a>, header: &'a AppHeader<'a>) {
    let indent = INDENT;

    buf.push_str("app");

    fmt_default_spaces(buf, header.after_app_keyword, " ", indent);
    fmt_str_literal(buf, header.name.value, indent);

    // packages
    fmt_default_spaces(buf, header.before_packages, " ", indent);
    buf.push_str("packages");
    fmt_default_spaces(buf, header.after_packages, " ", indent);
    fmt_packages(buf, header.packages, indent);

    // imports
    fmt_default_spaces(buf, header.before_imports, " ", indent);
    buf.push_str("imports");
    fmt_default_spaces(buf, header.after_imports, " ", indent);
    fmt_imports(buf, header.imports, indent);

    // provides
    fmt_default_spaces(buf, header.before_provides, " ", indent);
    buf.push_str("provides");
    fmt_default_spaces(buf, header.after_provides, " ", indent);
    fmt_provides(buf, header.provides, indent);
    fmt_default_spaces(buf, header.before_to, " ", indent);
    buf.push_str("to");
    fmt_default_spaces(buf, header.after_to, " ", indent);
    fmt_to(buf, header.to.value, indent);
}

pub fn fmt_platform_header<'a>(_buf: &mut String<'a>, _header: &'a PlatformHeader<'a>) {
    todo!("TODO fmt platform header");
}

fn fmt_imports<'a>(
    buf: &mut String<'a>,
    loc_entries: Collection<'a, Located<ImportsEntry<'a>>>,
    indent: u16,
) {
    fmt_collection(
        buf,
        loc_entries,
        indent,
        CollectionConfig {
            begin: '[',
            end: ']',
            delimiter: ',',
        },
    );
}

fn fmt_provides<'a>(
    buf: &mut String<'a>,
    loc_entries: Collection<'a, Located<ExposesEntry<'a, &'a str>>>,
    indent: u16,
) {
    fmt_collection(
        buf,
        loc_entries,
        indent,
        CollectionConfig {
            begin: '[',
            end: ']',
            delimiter: ',',
        },
    );
}

fn fmt_to<'a>(buf: &mut String<'a>, to: To<'a>, indent: u16) {
    match to {
        To::ExistingPackage(name) => {
            buf.push_str(name);
        }
        To::NewPackage(package_or_path) => fmt_package_or_path(buf, &package_or_path, indent),
    }
}

fn fmt_exposes<'a>(
    buf: &mut String<'a>,
    loc_entries: &'a Collection<'a, Located<ExposesEntry<'a, &'a str>>>,
    indent: u16,
) {
    fmt_collection(
        buf,
        *loc_entries,
        indent,
        CollectionConfig {
            begin: '[',
            end: ']',
            delimiter: ',',
        },
    );
}

impl<'a> Formattable<'a> for ExposesEntry<'a, &'a str> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format(&self, buf: &mut String<'a>, indent: u16) {
        fmt_exposes_entry(buf, self, indent);
    }
}

fn fmt_exposes_entry<'a>(buf: &mut String<'a>, entry: &ExposesEntry<'a, &'a str>, indent: u16) {
    use roc_parse::header::ExposesEntry::*;

    match entry {
        Exposed(ident) => buf.push_str(ident),

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

fn fmt_packages<'a>(
    buf: &mut String<'a>,
    loc_entries: Collection<'a, Located<PackageEntry<'a>>>,
    indent: u16,
) {
    fmt_collection(
        buf,
        loc_entries,
        indent,
        CollectionConfig {
            begin: '{',
            end: '}',
            delimiter: ',',
        },
    );
}

impl<'a> Formattable<'a> for PackageEntry<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format(&self, buf: &mut String<'a>, indent: u16) {
        fmt_packages_entry(buf, self, indent);
    }
}

impl<'a> Formattable<'a> for ImportsEntry<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format(&self, buf: &mut String<'a>, indent: u16) {
        fmt_imports_entry(buf, self, indent);
    }
}
fn fmt_packages_entry<'a>(buf: &mut String<'a>, entry: &PackageEntry<'a>, indent: u16) {
    use PackageEntry::*;
    match entry {
        Entry {
            shorthand,
            spaces_after_shorthand,
            package_or_path,
        } => {
            buf.push_str(shorthand);
            buf.push(':');
            fmt_default_spaces(buf, spaces_after_shorthand, " ", indent);
            fmt_package_or_path(buf, &package_or_path.value, indent);
        }
        SpaceBefore(sub_entry, spaces) => {
            fmt_spaces(buf, spaces.iter(), indent);
            fmt_packages_entry(buf, sub_entry, indent);
        }
        SpaceAfter(sub_entry, spaces) => {
            fmt_packages_entry(buf, sub_entry, indent);
            fmt_spaces(buf, spaces.iter(), indent);
        }
    }
}

fn fmt_package_or_path<'a>(buf: &mut String<'a>, package_or_path: &PackageOrPath<'a>, indent: u16) {
    match package_or_path {
        PackageOrPath::Package(_name, _version) => {
            todo!("format package");
        }
        PackageOrPath::Path(str_literal) => fmt_str_literal(buf, *str_literal, indent),
    }
}

fn fmt_imports_entry<'a>(buf: &mut String<'a>, entry: &ImportsEntry<'a>, indent: u16) {
    use roc_parse::header::ImportsEntry::*;

    match entry {
        Module(module, loc_exposes_entries) => {
            buf.push_str(module.as_str());

            if !loc_exposes_entries.is_empty() {
                buf.push('.');

                fmt_collection(
                    buf,
                    *loc_exposes_entries,
                    indent,
                    CollectionConfig {
                        begin: '{',
                        end: '}',
                        delimiter: ',',
                    },
                );
            }
        }

        Package(_pkg, _name, _entries) => {
            todo!("TODO Format imported package");
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
