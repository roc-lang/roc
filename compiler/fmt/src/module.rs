use crate::annotation::Formattable;
use crate::collection::{fmt_collection, CollectionConfig};
use crate::expr::fmt_str_literal;
use crate::spaces::{fmt_default_spaces, fmt_spaces, INDENT};
use crate::Buf;
use roc_parse::ast::{Collection, Module};
use roc_parse::header::{
    AppHeader, Effects, ExposesEntry, ImportsEntry, InterfaceHeader, ModuleName, PackageEntry,
    PackageName, PackageOrPath, PlatformHeader, PlatformRequires, PlatformRigid, To, TypedIdent,
};
use roc_region::all::Located;

pub fn fmt_module<'a>(buf: &mut Buf<'a>, module: &'a Module<'a>) {
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

pub fn fmt_interface_header<'a>(buf: &mut Buf<'a>, header: &'a InterfaceHeader<'a>) {
    let indent = INDENT;

    buf.indent(0);
    buf.push_str("interface");

    // module name
    fmt_default_spaces(buf, header.after_interface_keyword, " ", indent);
    buf.push_str(header.name.value.as_str());

    // exposes
    fmt_default_spaces(buf, header.before_exposes, " ", indent);
    buf.indent(indent);
    buf.push_str("exposes");
    fmt_default_spaces(buf, header.after_exposes, " ", indent);
    fmt_exposes(buf, header.exposes, indent);

    // imports
    fmt_default_spaces(buf, header.before_imports, " ", indent);
    buf.indent(indent);
    buf.push_str("imports");
    fmt_default_spaces(buf, header.after_imports, " ", indent);
    fmt_imports(buf, header.imports, indent);
}

pub fn fmt_app_header<'a>(buf: &mut Buf<'a>, header: &'a AppHeader<'a>) {
    let indent = INDENT;
    buf.indent(0);
    buf.push_str("app");

    fmt_default_spaces(buf, header.after_app_keyword, " ", indent);
    fmt_str_literal(buf, header.name.value, indent);

    // packages
    fmt_default_spaces(buf, header.before_packages, " ", indent);
    buf.indent(indent);
    buf.push_str("packages");
    fmt_default_spaces(buf, header.after_packages, " ", indent);
    fmt_packages(buf, header.packages, indent);

    // imports
    fmt_default_spaces(buf, header.before_imports, " ", indent);
    buf.indent(indent);
    buf.push_str("imports");
    fmt_default_spaces(buf, header.after_imports, " ", indent);
    fmt_imports(buf, header.imports, indent);

    // provides
    fmt_default_spaces(buf, header.before_provides, " ", indent);
    buf.indent(indent);
    buf.push_str("provides");
    fmt_default_spaces(buf, header.after_provides, " ", indent);
    fmt_provides(buf, header.provides, indent);
    fmt_default_spaces(buf, header.before_to, " ", indent);
    buf.indent(indent);
    buf.push_str("to");
    fmt_default_spaces(buf, header.after_to, " ", indent);
    fmt_to(buf, header.to.value, indent);
}

pub fn fmt_platform_header<'a>(buf: &mut Buf<'a>, header: &'a PlatformHeader<'a>) {
    let indent = INDENT;

    buf.indent(0);
    buf.push_str("platform");

    fmt_default_spaces(buf, header.after_platform_keyword, " ", indent);
    fmt_package_name(buf, header.name.value);

    // requires
    fmt_default_spaces(buf, header.before_requires, " ", indent);
    buf.indent(indent);
    buf.push_str("requires");
    fmt_default_spaces(buf, header.after_requires, " ", indent);
    fmt_requires(buf, &header.requires, indent);

    // exposes
    fmt_default_spaces(buf, header.before_exposes, " ", indent);
    buf.indent(indent);
    buf.push_str("exposes");
    fmt_default_spaces(buf, header.after_exposes, " ", indent);
    fmt_exposes(buf, header.exposes, indent);

    // packages
    fmt_default_spaces(buf, header.before_packages, " ", indent);
    buf.indent(indent);
    buf.push_str("packages");
    fmt_default_spaces(buf, header.after_packages, " ", indent);
    fmt_packages(buf, header.packages, indent);

    // imports
    fmt_default_spaces(buf, header.before_imports, " ", indent);
    buf.indent(indent);
    buf.push_str("imports");
    fmt_default_spaces(buf, header.after_imports, " ", indent);
    fmt_imports(buf, header.imports, indent);

    // provides
    fmt_default_spaces(buf, header.before_provides, " ", indent);
    buf.indent(indent);
    buf.push_str("provides");
    fmt_default_spaces(buf, header.after_provides, " ", indent);
    fmt_provides(buf, header.provides, indent);

    fmt_effects(buf, &header.effects, indent);
}

fn fmt_requires<'a>(buf: &mut Buf<'a>, requires: &PlatformRequires<'a>, indent: u16) {
    fmt_collection(
        buf,
        requires.rigids,
        indent,
        CollectionConfig {
            begin: '{',
            end: '}',
            delimiter: ',',
        },
    );

    buf.push_str(" { ");
    fmt_typed_ident(buf, &requires.signature.value, indent);
    buf.push_str(" }");
}

fn fmt_effects<'a>(buf: &mut Buf<'a>, effects: &Effects<'a>, indent: u16) {
    fmt_default_spaces(buf, effects.spaces_before_effects_keyword, " ", indent);
    buf.indent(indent);
    buf.push_str("effects");
    fmt_default_spaces(buf, effects.spaces_after_effects_keyword, " ", indent);

    buf.indent(indent);
    buf.push_str(effects.effect_shortname);
    buf.push('.');
    buf.push_str(effects.effect_type_name);

    fmt_default_spaces(buf, effects.spaces_after_type_name, " ", indent);

    fmt_collection(
        buf,
        effects.entries,
        indent,
        CollectionConfig {
            begin: '{',
            end: '}',
            delimiter: ',',
        },
    );
}

fn fmt_typed_ident<'a>(buf: &mut Buf<'a>, entry: &TypedIdent<'a>, indent: u16) {
    use TypedIdent::*;
    match entry {
        Entry {
            ident,
            spaces_before_colon,
            ann,
        } => {
            buf.indent(indent);
            buf.push_str(ident.value);
            fmt_default_spaces(buf, spaces_before_colon, " ", indent);
            buf.push(':');
            ann.value.format(buf, indent);
        }
        SpaceBefore(sub_entry, spaces) => {
            fmt_spaces(buf, spaces.iter(), indent);
            fmt_typed_ident(buf, sub_entry, indent);
        }
        SpaceAfter(sub_entry, spaces) => {
            fmt_typed_ident(buf, sub_entry, indent);
            fmt_spaces(buf, spaces.iter(), indent);
        }
    }
}

impl<'a> Formattable<'a> for TypedIdent<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format(&self, buf: &mut Buf<'a>, indent: u16) {
        fmt_typed_ident(buf, self, indent);
    }
}

impl<'a> Formattable<'a> for PlatformRigid<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format(&self, buf: &mut Buf<'a>, indent: u16) {
        fmt_platform_rigid(buf, self, indent);
    }
}

fn fmt_package_name<'a>(buf: &mut Buf<'a>, name: PackageName) {
    buf.push_str(name.account);
    buf.push('/');
    buf.push_str(name.pkg);
}

fn fmt_platform_rigid<'a>(buf: &mut Buf<'a>, entry: &PlatformRigid<'a>, indent: u16) {
    use roc_parse::header::PlatformRigid::*;

    match entry {
        Entry { rigid, alias } => {
            buf.push_str(rigid);
            buf.push_str("=>");
            buf.push_str(alias);
        }

        SpaceBefore(sub_entry, spaces) => {
            fmt_spaces(buf, spaces.iter(), indent);
            fmt_platform_rigid(buf, sub_entry, indent);
        }
        SpaceAfter(sub_entry, spaces) => {
            fmt_platform_rigid(buf, sub_entry, indent);
            fmt_spaces(buf, spaces.iter(), indent);
        }
    }
}

fn fmt_imports<'a>(
    buf: &mut Buf<'a>,
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
    buf: &mut Buf<'a>,
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

fn fmt_to<'a>(buf: &mut Buf<'a>, to: To<'a>, indent: u16) {
    match to {
        To::ExistingPackage(name) => {
            buf.push_str(name);
        }
        To::NewPackage(package_or_path) => fmt_package_or_path(buf, &package_or_path, indent),
    }
}

fn fmt_exposes<'a, N: FormatName + 'a>(
    buf: &mut Buf<'a>,
    loc_entries: Collection<'_, Located<ExposesEntry<'_, N>>>,
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

impl<'a, 'b, N: FormatName> Formattable<'a> for ExposesEntry<'b, N> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format(&self, buf: &mut Buf<'a>, indent: u16) {
        fmt_exposes_entry(buf, self, indent);
    }
}

pub trait FormatName {
    fn format<'a>(&self, buf: &mut Buf<'a>);
}

impl<'a> FormatName for &'a str {
    fn format<'b>(&self, buf: &mut Buf<'b>) {
        buf.push_str(self)
    }
}

impl<'a> FormatName for ModuleName<'a> {
    fn format<'b>(&self, buf: &mut Buf<'b>) {
        buf.push_str(self.as_str());
    }
}

fn fmt_exposes_entry<'a, 'b, N: FormatName>(
    buf: &mut Buf<'a>,
    entry: &ExposesEntry<'b, N>,
    indent: u16,
) {
    use roc_parse::header::ExposesEntry::*;

    match entry {
        Exposed(ident) => ident.format(buf),

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
    buf: &mut Buf<'a>,
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

    fn format(&self, buf: &mut Buf<'a>, indent: u16) {
        fmt_packages_entry(buf, self, indent);
    }
}

impl<'a> Formattable<'a> for ImportsEntry<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format(&self, buf: &mut Buf<'a>, indent: u16) {
        fmt_imports_entry(buf, self, indent);
    }
}
fn fmt_packages_entry<'a>(buf: &mut Buf<'a>, entry: &PackageEntry<'a>, indent: u16) {
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

fn fmt_package_or_path<'a>(buf: &mut Buf<'a>, package_or_path: &PackageOrPath<'a>, indent: u16) {
    match package_or_path {
        PackageOrPath::Package(_name, _version) => {
            todo!("format package");
        }
        PackageOrPath::Path(str_literal) => fmt_str_literal(buf, *str_literal, indent),
    }
}

fn fmt_imports_entry<'a>(buf: &mut Buf<'a>, entry: &ImportsEntry<'a>, indent: u16) {
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

        Package(pkg, name, entries) => {
            buf.push_str(pkg);
            buf.push('.');
            buf.push_str(name.as_str());

            if !entries.is_empty() {
                buf.push('.');

                fmt_collection(
                    buf,
                    *entries,
                    indent,
                    CollectionConfig {
                        begin: '{',
                        end: '}',
                        delimiter: ',',
                    },
                );
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
