use std::cmp::max;

use crate::annotation::{is_collection_multiline, Formattable, Newlines, Parens};
use crate::collection::{fmt_collection, Braces};
use crate::expr::fmt_str_literal;
use crate::pattern::snakify_camel_ident;
use crate::spaces::{fmt_comments_only, fmt_default_spaces, fmt_spaces, NewlineAt, INDENT};
use crate::Buf;
use roc_parse::ast::{Collection, CommentOrNewline, Header, Spaced, Spaces, SpacesBefore};
use roc_parse::header::{
    AppHeader, ExposedName, ExposesKeyword, HostedHeader, ImportsEntry, ImportsKeyword, Keyword,
    KeywordItem, ModuleHeader, ModuleName, PackageEntry, PackageHeader, PackageKeyword,
    PackageName, PackagesKeyword, PlatformHeader, PlatformKeyword, PlatformRequires,
    ProvidesKeyword, ProvidesTo, RequiresKeyword, To, ToKeyword, TypedIdent,
};
use roc_parse::ident::UppercaseIdent;
use roc_region::all::Loc;

pub fn fmt_header<'a>(buf: &mut Buf<'_>, header: &'a SpacesBefore<'a, Header<'a>>) {
    fmt_comments_only(buf, header.before.iter(), NewlineAt::Bottom, 0);
    match &header.item {
        Header::Module(header) => {
            fmt_module_header(buf, header);
        }
        Header::App(header) => {
            fmt_app_header(buf, header);
        }
        Header::Package(header) => {
            fmt_package_header(buf, header);
        }
        Header::Platform(header) => {
            fmt_platform_header(buf, header);
        }
        Header::Hosted(header) => {
            fmt_hosted_header(buf, header);
        }
    }
}

macro_rules! keywords {
    ($($name:ident),* $(,)?) => {
        $(
            impl Formattable for $name {
                fn is_multiline(&self) -> bool {
                    false
                }

                fn format_with_options(
                    &self,
                    buf: &mut Buf<'_>,
                    _parens: crate::annotation::Parens,
                    _newlines: Newlines,
                    indent: u16,
                ) {
                    buf.indent(indent);
                    buf.push_str($name::KEYWORD);
                }
            }
        )*
    }
}

keywords! {
    ExposesKeyword,
    ImportsKeyword,
    PackageKeyword,
    PackagesKeyword,
    RequiresKeyword,
    ProvidesKeyword,
    ToKeyword,
    PlatformKeyword,
}

impl<V: Formattable> Formattable for Option<V> {
    fn is_multiline(&self) -> bool {
        if let Some(v) = self {
            v.is_multiline()
        } else {
            false
        }
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        parens: crate::annotation::Parens,
        newlines: Newlines,

        indent: u16,
    ) {
        if let Some(v) = self {
            v.format_with_options(buf, parens, newlines, indent);
        }
    }
}

impl<'a> Formattable for ProvidesTo<'a> {
    fn is_multiline(&self) -> bool {
        if let Some(types) = &self.types {
            if is_collection_multiline(types) {
                return true;
            }
        }
        self.provides_keyword.is_multiline()
            || is_collection_multiline(&self.entries)
            || self.to_keyword.is_multiline()
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: crate::annotation::Parens,
        _newlines: Newlines,

        indent: u16,
    ) {
        self.provides_keyword.format(buf, indent);
        fmt_provides(buf, self.entries, self.types, indent);
        self.to_keyword.format(buf, indent);
        fmt_to(buf, self.to.value, indent);
    }
}

impl<'a> Formattable for PlatformRequires<'a> {
    fn is_multiline(&self) -> bool {
        is_collection_multiline(&self.rigids) || is_collection_multiline(&self.signatures)
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: crate::annotation::Parens,
        _newlines: Newlines,

        indent: u16,
    ) {
        fmt_requires(buf, self, indent);
    }
}

impl<'a, V: Formattable> Formattable for Spaces<'a, V> {
    fn is_multiline(&self) -> bool {
        !self.before.is_empty() || !self.after.is_empty() || self.item.is_multiline()
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        parens: crate::annotation::Parens,
        newlines: Newlines,

        indent: u16,
    ) {
        fmt_default_spaces(buf, self.before, indent);
        self.item.format_with_options(buf, parens, newlines, indent);
        fmt_default_spaces(buf, self.after, indent);
    }
}

impl<'a, K: Formattable, V: Formattable> Formattable for KeywordItem<'a, K, V> {
    fn is_multiline(&self) -> bool {
        self.keyword.is_multiline() || self.item.is_multiline()
    }

    fn format_with_options(&self, buf: &mut Buf, parens: Parens, newlines: Newlines, indent: u16) {
        self.keyword
            .format_with_options(buf, parens, newlines, indent);
        self.item.format_with_options(buf, parens, newlines, indent);
    }
}

pub fn fmt_module_header<'a>(buf: &mut Buf, header: &'a ModuleHeader<'a>) {
    buf.indent(0);
    buf.push_str("module");

    let mut indent = fmt_spaces_with_outdent(buf, header.after_keyword, 0);

    if let Some(params) = &header.params {
        if is_collection_multiline(&params.pattern.value) {
            indent = INDENT;
        }

        fmt_collection(
            buf,
            indent,
            Braces::Curly,
            params.pattern.value,
            Newlines::Yes,
        );

        indent = fmt_spaces_with_outdent(buf, params.before_arrow, indent);
        buf.push_str("->");
        indent = fmt_spaces_with_outdent(buf, params.after_arrow, indent);
    }

    fmt_exposes(buf, header.exposes, indent);
}

pub fn fmt_hosted_header<'a>(buf: &mut Buf, header: &'a HostedHeader<'a>) {
    buf.indent(0);
    buf.push_str("hosted");

    let indent = fmt_spaces_with_outdent(buf, header.before_exposes, 0);

    fmt_exposes(buf, header.exposes, indent);
}

pub fn fmt_app_header<'a>(buf: &mut Buf, header: &'a AppHeader<'a>) {
    buf.indent(0);
    buf.push_str("app");

    let indent = fmt_spaces_with_outdent(buf, header.before_provides, 0);
    fmt_exposes(buf, header.provides, indent);

    let indent = fmt_spaces_with_outdent(buf, header.before_packages, indent);
    fmt_packages(buf, header.packages.value, indent);
}

pub fn fmt_spaces_with_outdent(buf: &mut Buf, spaces: &[CommentOrNewline], indent: u16) -> u16 {
    if spaces.iter().all(|c| c.is_newline()) {
        buf.spaces(1);
        indent
    } else {
        let indent = max(INDENT, indent + INDENT);
        fmt_default_spaces(buf, spaces, indent);
        indent
    }
}

pub fn fmt_package_header<'a>(buf: &mut Buf, header: &'a PackageHeader<'a>) {
    buf.indent(0);
    buf.push_str("package");

    let indent = fmt_spaces_with_outdent(buf, header.before_exposes, 0);
    fmt_exposes(buf, header.exposes, indent);

    let indent = fmt_spaces_with_outdent(buf, header.before_packages, indent);
    fmt_packages(buf, header.packages.value, indent);
}

pub fn fmt_platform_header<'a>(buf: &mut Buf, header: &'a PlatformHeader<'a>) {
    buf.indent(0);
    buf.push_str("platform");
    let indent = INDENT;
    fmt_default_spaces(buf, header.before_name, indent);

    fmt_package_name(buf, header.name.value, indent);

    header.requires.format(buf, indent);
    header.exposes.keyword.format(buf, indent);
    fmt_exposes(buf, header.exposes.item, indent);
    header.packages.keyword.format(buf, indent);
    fmt_packages(buf, header.packages.item, indent);
    header.imports.keyword.format(buf, indent);
    fmt_imports(buf, header.imports.item, indent);
    header.provides.keyword.format(buf, indent);
    fmt_provides(buf, header.provides.item, None, indent);
}

fn fmt_requires(buf: &mut Buf, requires: &PlatformRequires, indent: u16) {
    fmt_collection(buf, indent, Braces::Curly, requires.rigids, Newlines::No);

    buf.spaces(1);
    fmt_collection(
        buf,
        indent,
        Braces::Curly,
        requires.signatures,
        Newlines::No,
    );
}

impl<'a> Formattable for TypedIdent<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,

        indent: u16,
    ) {
        buf.indent(indent);
        buf.push_str(self.ident.value);
        fmt_default_spaces(buf, self.spaces_before_colon, indent);
        buf.push(':');
        buf.spaces(1);

        self.ann.value.format(buf, indent);
    }
}

fn fmt_package_name(buf: &mut Buf, name: PackageName, indent: u16) {
    buf.indent(indent);
    buf.push('"');
    buf.push_str_allow_spaces(name.to_str());
    buf.push('"');
}

impl<'a, T: Formattable> Formattable for Spaced<'a, T> {
    fn is_multiline(&self) -> bool {
        use Spaced::*;

        match self {
            Item(formattable) => formattable.is_multiline(),
            SpaceBefore(formattable, spaces) | SpaceAfter(formattable, spaces) => {
                !spaces.is_empty() || formattable.is_multiline()
            }
        }
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
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

fn fmt_imports<'a>(
    buf: &mut Buf,
    loc_entries: Collection<'a, Loc<Spaced<'a, ImportsEntry<'a>>>>,

    indent: u16,
) {
    fmt_collection(buf, indent, Braces::Square, loc_entries, Newlines::No)
}

fn fmt_provides<'a>(
    buf: &mut Buf,
    loc_exposed_names: Collection<'a, Loc<Spaced<'a, ExposedName<'a>>>>,
    loc_provided_types: Option<Collection<'a, Loc<Spaced<'a, UppercaseIdent<'a>>>>>,

    indent: u16,
) {
    fmt_collection(buf, indent, Braces::Square, loc_exposed_names, Newlines::No);
    if let Some(loc_provided) = loc_provided_types {
        fmt_default_spaces(buf, &[], indent);
        fmt_collection(buf, indent, Braces::Curly, loc_provided, Newlines::No);
    }
}

fn fmt_to(buf: &mut Buf, to: To, indent: u16) {
    match to {
        To::ExistingPackage(name) => {
            buf.push_str(name);
        }
        To::NewPackage(package_name) => fmt_package_name(buf, package_name, indent),
    }
}

fn fmt_exposes<N: Formattable + Copy + core::fmt::Debug>(
    buf: &mut Buf,
    loc_entries: Collection<'_, Loc<Spaced<'_, N>>>,

    indent: u16,
) {
    fmt_collection(buf, indent, Braces::Square, loc_entries, Newlines::No)
}

impl<'a> Formattable for ModuleName<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,
        _indent: u16,
    ) {
        buf.push_str(self.as_str());
    }
}

impl<'a> Formattable for ExposedName<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,
        indent: u16,
    ) {
        buf.indent(indent);
        if buf.flags().snakify
            && self
                .as_str()
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_lowercase())
        {
            snakify_camel_ident(buf, self.as_str());
        } else {
            buf.push_str(self.as_str());
        }
    }
}

fn fmt_packages<'a>(
    buf: &mut Buf,
    loc_entries: Collection<'a, Loc<Spaced<'a, PackageEntry<'a>>>>,
    indent: u16,
) {
    fmt_collection(buf, indent, Braces::Curly, loc_entries, Newlines::No)
}

impl<'a> Formattable for PackageEntry<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,

        indent: u16,
    ) {
        fmt_packages_entry(buf, self, indent);
    }
}

impl<'a> Formattable for ImportsEntry<'a> {
    fn is_multiline(&self) -> bool {
        false
    }

    fn format_with_options(
        &self,
        buf: &mut Buf,
        _parens: Parens,
        _newlines: Newlines,

        indent: u16,
    ) {
        fmt_imports_entry(buf, self, indent);
    }
}
fn fmt_packages_entry(buf: &mut Buf, entry: &PackageEntry, indent: u16) {
    buf.push_str(entry.shorthand);
    buf.push(':');
    fmt_default_spaces(buf, entry.spaces_after_shorthand, indent);

    let indent = indent + INDENT;

    if let Some(spaces_after) = entry.platform_marker {
        buf.indent(indent);
        buf.push_str(roc_parse::keyword::PLATFORM);
        fmt_default_spaces(buf, spaces_after, indent);
    }

    fmt_package_name(buf, entry.package_name.value, indent);
}

fn fmt_imports_entry(buf: &mut Buf, entry: &ImportsEntry, indent: u16) {
    use roc_parse::header::ImportsEntry::*;

    buf.indent(indent);

    match entry {
        Module(module, loc_exposes_entries) => {
            buf.push_str(module.as_str());

            if !loc_exposes_entries.is_empty() {
                buf.push('.');

                fmt_collection(
                    buf,
                    indent,
                    Braces::Curly,
                    *loc_exposes_entries,
                    Newlines::No,
                )
            }
        }

        Package(pkg, name, entries) => {
            buf.push_str(pkg);
            buf.push('.');
            buf.push_str(name.as_str());

            if !entries.is_empty() {
                buf.push('.');

                fmt_collection(buf, indent, Braces::Curly, *entries, Newlines::No)
            }
        }

        IngestedFile(file_name, typed_ident) => {
            fmt_str_literal(buf, *file_name, indent);
            buf.push_str_allow_spaces(" as ");
            typed_ident.format(buf, 0);
        }
    }
}
