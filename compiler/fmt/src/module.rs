use crate::annotation::{Formattable, Newlines};
use crate::collection::fmt_collection;
use crate::expr::fmt_str_literal;
use crate::spaces::{fmt_default_spaces, fmt_spaces, INDENT};
use crate::Buf;
use roc_parse::ast::{Collection, Module, Spaced};
use roc_parse::header::{
    AppHeader, Effects, ExposedName, ImportsEntry, InterfaceHeader, ModuleName, PackageEntry,
    PackageName, PlatformHeader, PlatformRequires, PlatformRigid, To, TypedIdent,
};
use roc_region::all::Loc;

pub fn fmt_module<'a, 'buf>(buf: &mut Buf<'buf>, module: &'a Module<'a>) {
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

pub fn fmt_interface_header<'a, 'buf>(buf: &mut Buf<'buf>, header: &'a InterfaceHeader<'a>) {
    let indent = INDENT;

    buf.indent(0);
    buf.push_str("interface");

    // module name
    fmt_default_spaces(buf, header.after_interface_keyword, indent);
    buf.push_str(header.name.value.as_str());

    // exposes
    fmt_default_spaces(buf, header.before_exposes, indent);
    buf.indent(indent);
    buf.push_str("exposes");
    fmt_default_spaces(buf, header.after_exposes, indent);
    fmt_exposes(buf, header.exposes, indent);

    // imports
    fmt_default_spaces(buf, header.before_imports, indent);
    buf.indent(indent);
    buf.push_str("imports");
    fmt_default_spaces(buf, header.after_imports, indent);
    fmt_imports(buf, header.imports, indent);
}

pub fn fmt_app_header<'a, 'buf>(buf: &mut Buf<'buf>, header: &'a AppHeader<'a>) {
    let indent = INDENT;
    buf.indent(0);
    buf.push_str("app");

    fmt_default_spaces(buf, header.after_app_keyword, indent);
    fmt_str_literal(buf, header.name.value, indent);

    // packages
    fmt_default_spaces(buf, header.before_packages, indent);
    buf.indent(indent);
    buf.push_str("packages");
    fmt_default_spaces(buf, header.after_packages, indent);
    fmt_packages(buf, header.packages, indent);

    // imports
    fmt_default_spaces(buf, header.before_imports, indent);
    buf.indent(indent);
    buf.push_str("imports");
    fmt_default_spaces(buf, header.after_imports, indent);
    fmt_imports(buf, header.imports, indent);

    // provides
    fmt_default_spaces(buf, header.before_provides, indent);
    buf.indent(indent);
    buf.push_str("provides");
    fmt_default_spaces(buf, header.after_provides, indent);
    fmt_provides(buf, header.provides, indent);
    fmt_default_spaces(buf, header.before_to, indent);
    buf.indent(indent);
    buf.push_str("to");
    fmt_default_spaces(buf, header.after_to, indent);
    fmt_to(buf, header.to.value, indent);
}

pub fn fmt_platform_header<'a, 'buf>(buf: &mut Buf<'buf>, header: &'a PlatformHeader<'a>) {
    let indent = INDENT;

    buf.indent(0);
    buf.push_str("platform");

    fmt_default_spaces(buf, header.after_platform_keyword, indent);
    fmt_package_name(buf, header.name.value, indent);

    // requires
    fmt_default_spaces(buf, header.before_requires, indent);
    buf.indent(indent);
    buf.push_str("requires");
    fmt_default_spaces(buf, header.after_requires, indent);
    fmt_requires(buf, &header.requires, indent);

    // exposes
    fmt_default_spaces(buf, header.before_exposes, indent);
    buf.indent(indent);
    buf.push_str("exposes");
    fmt_default_spaces(buf, header.after_exposes, indent);
    fmt_exposes(buf, header.exposes, indent);

    // packages
    fmt_default_spaces(buf, header.before_packages, indent);
    buf.indent(indent);
    buf.push_str("packages");
    fmt_default_spaces(buf, header.after_packages, indent);
    fmt_packages(buf, header.packages, indent);

    // imports
    fmt_default_spaces(buf, header.before_imports, indent);
    buf.indent(indent);
    buf.push_str("imports");
    fmt_default_spaces(buf, header.after_imports, indent);
    fmt_imports(buf, header.imports, indent);

    // provides
    fmt_default_spaces(buf, header.before_provides, indent);
    buf.indent(indent);
    buf.push_str("provides");
    fmt_default_spaces(buf, header.after_provides, indent);
    fmt_provides(buf, header.provides, indent);

    fmt_effects(buf, &header.effects, indent);
}

fn fmt_requires<'a, 'buf>(buf: &mut Buf<'buf>, requires: &PlatformRequires<'a>, indent: u16) {
    fmt_collection(buf, indent, '{', '}', requires.rigids, Newlines::No);

    buf.push_str(" {");
    buf.spaces(1);
    requires.signature.value.format(buf, indent);
    buf.push_str(" }");
}

fn fmt_effects<'a, 'buf>(buf: &mut Buf<'buf>, effects: &Effects<'a>, indent: u16) {
    fmt_default_spaces(buf, effects.spaces_before_effects_keyword, indent);
    buf.indent(indent);
    buf.push_str("effects");
    fmt_default_spaces(buf, effects.spaces_after_effects_keyword, indent);

    buf.indent(indent);
    buf.push_str(effects.effect_shortname);
    buf.push('.');
    buf.push_str(effects.effect_type_name);

    fmt_default_spaces(buf, effects.spaces_after_type_name, indent);

    fmt_collection(buf, indent, '{', '}', effects.entries, Newlines::No)
}

impl<'a> Formattable for TypedIdent<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format<'buf>(&self, buf: &mut Buf<'buf>, indent: u16) {
        buf.indent(indent);
        buf.push_str(self.ident.value);
        fmt_default_spaces(buf, self.spaces_before_colon, indent);
        buf.push_str(":");
        buf.spaces(1);
        self.ann.value.format(buf, indent);
    }
}

fn fmt_package_name<'buf>(buf: &mut Buf<'buf>, name: PackageName, _indent: u16) {
    buf.push('"');
    buf.push_str_allow_spaces(name.0);
    buf.push('"');
}

impl<'a, T: Formattable> Formattable for Spaced<'a, T> {
    fn is_multiline(&self) -> bool {
        // TODO
        false
    }

    fn format_with_options<'buf>(
        &self,
        buf: &mut Buf<'buf>,
        parens: crate::annotation::Parens,
        newlines: Newlines,
        indent: u16,
    ) {
        match self {
            Spaced::Item(item) => {
                item.format_with_options(buf, parens, newlines, indent);
            }
            Spaced::SpaceBefore(item, spaces) => {
                fmt_spaces(buf, spaces.iter(), indent);
                item.format_with_options(buf, parens, newlines, indent);
            }
            Spaced::SpaceAfter(item, spaces) => {
                item.format_with_options(buf, parens, newlines, indent);
                fmt_spaces(buf, spaces.iter(), indent);
            }
        }
    }
}

impl<'a> Formattable for PlatformRigid<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format<'buf>(&self, buf: &mut Buf<'buf>, _indent: u16) {
        buf.push_str(self.rigid);
        buf.push_str("=>");
        buf.push_str(self.alias);
    }
}

fn fmt_imports<'a, 'buf>(
    buf: &mut Buf<'buf>,
    loc_entries: Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>,
    indent: u16,
) {
    fmt_collection(buf, indent, '[', ']', loc_entries, Newlines::No)
}

fn fmt_provides<'a, 'buf>(
    buf: &mut Buf<'buf>,
    loc_entries: Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    indent: u16,
) {
    fmt_collection(buf, indent, '[', ']', loc_entries, Newlines::No)
}

fn fmt_to<'buf>(buf: &mut Buf<'buf>, to: To, indent: u16) {
    match to {
        To::ExistingPackage(name) => {
            buf.push_str(name);
        }
        To::NewPackage(package_name) => fmt_package_name(buf, package_name, indent),
    }
}

fn fmt_exposes<'buf, N: Formattable + Copy>(
    buf: &mut Buf<'buf>,
    loc_entries: Collection<'_, Loc<Spaced<'_, N>>>,
    indent: u16,
) {
    fmt_collection(buf, indent, '[', ']', loc_entries, Newlines::No)
}

pub trait FormatName {
    fn format<'buf>(&self, buf: &mut Buf<'buf>);
}

impl<'a> FormatName for &'a str {
    fn format<'buf>(&self, buf: &mut Buf<'buf>) {
        buf.push_str(self)
    }
}

impl<'a> FormatName for ModuleName<'a> {
    fn format<'buf>(&self, buf: &mut Buf<'buf>) {
        buf.push_str(self.as_str());
    }
}

impl<'a> Formattable for ModuleName<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format<'buf>(&self, buf: &mut Buf<'buf>, _indent: u16) {
        buf.push_str(self.as_str());
    }
}

impl<'a> Formattable for ExposedName<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format<'buf>(&self, buf: &mut Buf<'buf>, _indent: u16) {
        buf.push_str(self.as_str());
    }
}

impl<'a> FormatName for ExposedName<'a> {
    fn format<'buf>(&self, buf: &mut Buf<'buf>) {
        buf.push_str(self.as_str());
    }
}

fn fmt_packages<'a, 'buf>(
    buf: &mut Buf<'buf>,
    loc_entries: Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>,
    indent: u16,
) {
    fmt_collection(buf, indent, '{', '}', loc_entries, Newlines::No)
}

impl<'a> Formattable for PackageEntry<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format<'buf>(&self, buf: &mut Buf<'buf>, indent: u16) {
        fmt_packages_entry(buf, self, indent);
    }
}

impl<'a> Formattable for ImportsEntry<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format<'buf>(&self, buf: &mut Buf<'buf>, indent: u16) {
        fmt_imports_entry(buf, self, indent);
    }
}
fn fmt_packages_entry<'a, 'buf>(buf: &mut Buf<'buf>, entry: &PackageEntry<'a>, indent: u16) {
    buf.push_str(entry.shorthand);
    buf.push(':');
    fmt_default_spaces(buf, entry.spaces_after_shorthand, indent);
    fmt_package_name(buf, entry.package_name.value, indent);
}

fn fmt_imports_entry<'a, 'buf>(buf: &mut Buf<'buf>, entry: &ImportsEntry<'a>, indent: u16) {
    use roc_parse::header::ImportsEntry::*;

    match entry {
        Module(module, loc_exposes_entries) => {
            buf.push_str(module.as_str());

            if !loc_exposes_entries.is_empty() {
                buf.push('.');

                fmt_collection(buf, indent, '{', '}', *loc_exposes_entries, Newlines::No)
            }
        }

        Package(pkg, name, entries) => {
            buf.push_str(pkg);
            buf.push('.');
            buf.push_str(name.as_str());

            if !entries.is_empty() {
                buf.push('.');

                fmt_collection(buf, indent, '{', '}', *entries, Newlines::No)
            }
        }
    }
}
