use bumpalo::{collections::String, Bump};

pub struct DocUrl<'a> {
    pub url: &'a str,
    pub title: &'a str,
}

pub enum AutolinkProblem<'a> {
    // TODO: error message - Tried to generate an automatic link in docs for symbol `{ident}`,
    // [optionally print the module name if it was qualified]
    // but that symbol was not in scope in this module.
    AutolinkLookupNotFound {
        opt_module_name: Option<&'a str>,
        ident: &'a str,
    },
    AutolinkLookupToUnexposedIdent {
        // Note: You can do qualified lookups on your own module, e.g.
        // if I'm in the Foo module, I can do a `Foo.bar` lookup.
        opt_module_name: Option<&'a str>,
        ident: &'a str,
    },
}

pub struct ModuleInfo<'a> {
    pub module_name: &'a str,
    // TODO add a `--include-builtins` CLI flag for generating offline docs locally
    // which include builtins; if that flag is omitted, have this code path generate
    // a link directly to the builtin docs on roc-lang.org instead of to a localhost
    // URL that will 404.
    /// Note that this depends on what package the module is in!
    /// Also, it should *not* end in a '/' because we always add one.
    pub module_base_url: &'a str,
}

impl<'a> DocUrl<'a> {
    pub fn new(
        arena: &'a Bump,
        // If the ident was fully qualified, include the module name here.
        opt_module_name: Option<&'a str>,
        ident: &'a str,
        lookup: impl Fn(Option<&'a str>, &str) -> Result<ModuleInfo<'a>, AutolinkProblem<'a>>,
    ) -> Result<Self, AutolinkProblem<'a>> {
        let ModuleInfo {
            module_name,
            module_base_url,
        } = lookup(opt_module_name, ident)?;
        let url = {
            let mut buf = String::with_capacity_in(
                module_base_url.len() + module_name.len() + ident.len()
            // + 1 for the '/' after the base URL, and '#' before the ident
            + 2,
                arena,
            );

            // Example:
            //
            // base_url: "example.com", module_name: "Str", ident: "join" =>
            // "example.com/Str#join"
            buf.push_str(module_base_url);
            buf.push('/');
            buf.push_str(module_name);
            buf.push('#');
            buf.push_str(ident);

            buf
        };

        let title = {
            const PREFIX: &str = "Docs for ";

            let mut buf = String::with_capacity_in(
                PREFIX.len() + module_name.len() + ident.len()
            // + 1 for the '.'
            + 1,
                arena,
            );

            buf.push_str(PREFIX);
            buf.push_str(module_name);
            buf.push('.');
            buf.push_str(ident);

            buf
        };

        Ok(Self {
            url: url.into_bump_str(),
            title: title.into_bump_str(),
        })
    }
}
