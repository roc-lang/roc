# Documentation search

API names and signatures remain embedded in each page and filter immediately.
Prose is generated once in `search-index.html` and fetched on first search focus.
Language reference articles are indexed by section, including their code examples;
API module and entry documentation is indexed at its existing destination.

The dropdown keeps API matches in compact signature rows. Other matches appear in
separate groups, with five prose results per category in the initial All view.
“More” expands that group in place without changing the filter; “Less” collapses
it again. Each group expands independently. The snippet icon button
toggles context independently of the category, keeping the choice while searching.
Snippets highlight query words case-insensitively and keep only the title underlined
on hover. Toggling snippets preserves the top visible result where space allows.
API name/signature matches retain their compact layout.
Category filters disappear when only one category has entries. Loading the prose
index never replaces results while keyboard focus is in the result list.

## Configuration

Pass `--search-config=path/to/search.tsv` to `roc docs`. The file is UTF-8 with
literal tabs between fields, one record per line. Empty lines and lines starting
with `#` are ignored. Empty fields, extra fields, unknown categories, unreadable
source files, and invalid curated destinations fail generation.

Records:

| Record | Fields after the record name |
| --- | --- |
| `suggestion` | Search query, group label |
| `all` | Display label for the aggregate filter |
| `category` | Stable ID, display label |
| `document` | Category ID, title, destination URL, source file |
| `term` | Category ID, exact search term, destination URL, result title |

Suggestions sharing a group label appear on one line with individually clickable
queries. Groups and queries follow declaration order when search is empty. Selecting
one fills the field and searches all categories. They are optional; no Roc-specific
suggestions are added unless configured.

Built-in category IDs are `api` and `guides`. Change their display labels or add
categories such as `recipes` or `architecture`. Categories are shown in declaration
order, after any built-in categories that weren't explicitly configured. Empty
categories are hidden. Category membership is independent of API-row rendering:
a term pointing at an API entry uses its existing name/signature row.

A document source is plain text, Markdown, or code; it is indexed as text, not
executed or injected as HTML. Relative source paths resolve against the config
file. Destination URLs resolve against the generated docs root. Site-absolute
paths are supported for additional documents, so a website can contribute its
examples and other pages without depending on a particular website layout.
Additional documents use full browser navigation (`data-full-navigation` in the
HTML index), preserving browser history when their pages use a different layout.
The website build is responsible for publishing and checking those additional
pages. Documents with different source files may share a destination, e.g. an
example's README and its Roc source; search deduplicates destination links.

API results prioritize matches in type names, then value/function names, then
signatures. Within each name group, exact qualified names come first, then exact
unqualified names, then full-name prefixes, then other substring matches. Ties
retain generator order, and prose matches follow these compact API results.

Curated terms match case-insensitively and rank before incidental prose matches.
Exact aliases always match. Word aliases also match prefixes when there are no
API name or signature matches, displaying the complete alias alongside the result. Punctuation-only terms require an exact match.
For a curated punctuation-only query, incidental matches are suppressed.
Their destinations must be relative paths into the generated site, with an
optional fragment. Generation checks that the target file and anchor exist.
For an external website page, index it as a `document`; use a curated term to
point directly to its corresponding API or language-reference entry.

Example (replace the visible separators below with literal tabs):

```text
all             Everything
category        api           Reference
category        guides        Language guide
category        recipes       Examples
term            guides        :=                  langref/types/#nominal-types          Types › Nominal Types
term            guides        ::                  langref/types/#opaque-nominal-types   Types › Opaque Nominal Types
term            api           lreduce             List/#fold                            List.fold
document       recipes       Parse JSON          /examples/Json/README.html            examples/Json/README.md
```

`docs/search.tsv` is a ready-to-use configuration for
builtin docs with the language reference:

```sh
zig-out/bin/roc docs --builtins --with-lang-ref --search-config=docs/search.tsv
```

## Checks

The dependency-free JavaScript tests run in the PR static-checks job. Run them
locally with Node.js 18 or newer, and run the generator tests with Zig:

```sh
node --test test/docs-search/search.test.cjs
zig build run-test-zig-module-docs
```

Browser checks should cover dense API queries (`fold`), case-insensitive headings
(`Nominal`, `Opaque`), exact symbols (`:=`, `::`), synonyms (`lreduce`), custom
category labels, and keyboard selection while the additional index is loading.
