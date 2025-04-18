app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import cli.Stdout
import cli.Path
import "../Glossary.md" as glossary_as_str : Str

# This script checks if all markdown links that point to files or dirs are valid for the file Glossary.md
# TODO check links to other markdown headers as well, e.g. #tokenization

main! = |_args|
    glossary_as_str
    |> extract_md_links
    |> List.for_each_try!(
        |link_str|
            check_res = check_link!(link_str)

            when check_res is
                Ok(_) ->
                    # Stdout.line!("Check successful for $(link_str)")?
                    Ok({})
                Err(BadLink(bad_link_str)) ->
                    Stdout.line!("I could not find the path: $(bad_link_str). If you recently modified this path, please update $(bad_link_str) in Glossary.md at the root of the repo.")
    )?
    
    Stdout.line!("Check Successful.")

expect
    md_text = "foo [bar](src/baz.md)..."
    links = extract_md_links(md_text)

    links == ["src/baz.md"]

extract_md_links : Str -> List Str
extract_md_links = |md_content|
    split_list = Str.split_on(md_content, "](")

    links =
        split_list
        |> List.drop_first(1)
        |> List.map(
            |link|
                res = Str.split_on(link, ")") |> List.first()
                when res is
                    Ok(str) -> str
                    Err(_) -> ""
        )

    links

expect
    md_text = "foo [bar](src/baz.md)...[zod](boz.md)"
    links = extract_md_links(md_text)

    links == ["src/baz.md", "boz.md"]

expect
    md_text =
        """
        foo [bar](src/baz.md)...
        hey asjkl [zod](boz.md) asmd
        [bam](crates/check/can_solo/src/desugar.rs)
        """
    links = extract_md_links(md_text)

    links == ["src/baz.md", "boz.md", "crates/check/can_solo/src/desugar.rs"]


check_link! : Str => Result {} [BadLink Str]
check_link! = |link_str|
    if Str.starts_with(link_str, "http") || Str.starts_with(link_str, "#") then
        Ok({})
    else
        path = Path.from_str(link_str)

        when Path.type!(path) is
            Ok(_) ->
                Ok({})
            Err(_) ->
                Err(BadLink(link_str))
        
    
