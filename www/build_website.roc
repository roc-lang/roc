app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import cli.Stdout
import cli.Stderr
import cli.Arg exposing [Arg]
import cli.Dir
import cli.Cmd
import cli.File

main! : List Arg => Result {} _
main! = |_args|

    # get cwd to check if we are in the www dir
    pwd_cmd_output =
        Cmd.new("pwd")
        |> Cmd.output!()

    when pwd_cmd_output.status is
        Ok(0) ->
            pwd_bytes = pwd_cmd_output.stdout
            
            if !(List.ends_with(pwd_bytes, [119, 119, 119])) then
                pwd_str = Str.from_utf8(pwd_bytes)?
                Stderr.line!("You must run this script inside the 'www' directory, I am currently in: ${pwd_str}")?
            else
                {}
        _ ->
            Err(CommandPWDFailed(Inspect.to_str(pwd_cmd_output)))?

    # Clean up dirs from previous runs
    _ = Dir.delete_all!("build")
    _ = Dir.delete_all!("content/examples")
    _ = Dir.delete_all!("examples-main")

    Cmd.exec!("cp", ["-r", "public", "build"]) ? |err| CopyPublicToBuildFailed(err)


    # Download latest examples
    Cmd.exec!("curl", ["-fL", "-o", "examples-main.zip", "https://github.com/roc-lang/examples/archive/refs/heads/main.zip"]) ? |err| CurlDowloadExamplesRepoFailed(err)
    
    Cmd.exec!("unzip", ["-o", "-q", "examples-main.zip"]) ? |err| UnzipExamplesRepoFailed(err)

    Cmd.exec!("cp", ["-R", "examples-main/examples/", "content/examples/"]) ? |err| CopyExamplesToContentFailed(err)

    # replace links in content/examples/index.md to work on the WIP site
    # TODO do this in Roc
    Cmd.exec!("perl", ["-pi", "-e", "s|\\]\\(/|\\]\\(/examples/|g", "content/examples/index.md"]) ? |err| PerlReplaceFailed(err)

    Dir.delete_all!("examples-main") ? |err| DeleteExamplesMainDirFailed(err)
    File.delete!("examples-main.zip") ? |err| DeleteExamplesMainZipFailed(err)


    # download fonts just-in-time so we don't have to bloat the repo with them.
    design_assets_commit = "4d949642ebc56ca455cf270b288382788bce5873"
    design_assets_tarfile = "roc-lang-design-assets-4d94964.tar.gz"
    design_assets_dir = "roc-lang-design-assets-4d94964"

    Cmd.exec!("curl", ["-fLJO", "https://github.com/roc-lang/design-assets/tarball/${design_assets_commit}"]) ? |err| CurlDownloadDesignAssetsFailed(err)

    Cmd.exec!("tar", ["-xzf", design_assets_tarfile]) ? |err| TarExtractDesignAssetsFailed(err)

    Cmd.exec!("mv", ["${design_assets_dir}/fonts", "build/fonts"]) ? |err| MoveFontsToBuildFailed(err)

    # clean up
    Dir.delete_all!(design_assets_dir) ? |err| DeleteDesignAssetsDirFailed(err)
    File.delete!(design_assets_tarfile) ? |err| DeleteDesignAssetsTarFailed(err)


    repl_tarfile = "roc_repl_wasm.tar.gz"

    # Download the latest stable Web REPL as a zip file.
    Cmd.exec!("curl", ["-fLJO", "https://github.com/roc-lang/roc/releases/download/alpha3-rolling/${repl_tarfile}"]) ? |err| CurlDownloadReplFailed(err)
    
    Dir.create!("build/repl") ? |err| CreateReplDirFailed(err)

    Cmd.exec!("tar", ["-xzf", repl_tarfile, "-C", "build/repl"]) ? |err| TarExtractReplFailed(err)

    File.delete!(repl_tarfile) ? |err| DeleteReplTarFailed(err)

    # TODO generate docs for alpha3 instead of main branch
    Cmd.exec!("roc", ["docs", "--output", "build/builtins", "--root-dir", "builtins"]) ? |err| RocGenDocsBuiltinsFailed(err)


    # Manually add this tip to all the builtin docs.
    Cmd.exec!("find", ["www/build/builtins", "-type", "f", "-name", "index.html", "-exec", "sed", "-i", "'s!</nav>!<div class=\"builtins-tip\"><b>Tip:</b> <a href=\"/different-names\">Some names</a> differ from other languages.</div></nav>!'", "{}"]) ? |err| SedAddTipToBuiltinsDocsFailed(err)


    # Generating site markdown content
    Cmd.exec!("roc", ["build", "--linker", "legacy", "www/main.roc"]) ? |err| RocBuildStaticSiteMainFailed(err)
    
    Cmd.exec!("roc", ["www/main.roc", "www/content/", "www/build/"]) ? |err| RocRunStaticSiteMainFailed(err)


    # Add github link to examples
    examples_dir = "www/build/examples"
    examples_repo_link = "https://github.com/roc-lang/examples/tree/main/examples"

    github_logo_svg =
        """
        <svg viewBox="0 0 98 96" height="25" xmlns="http://www.w3.org/2000/svg" fill-rule="evenodd" clip-rule="evenodd" role="img" id="gh-logo">
        <path d='M48.854 0C21.839 0 0 22 0 49.217c0 21.756 13.993 40.172 33.405 46.69 2.427.49 3.316-1.059 3.316-2.362 0-1.141-.08-5.052-.08-9.127-13.59 2.934-16.42-5.867-16.42-5.867-2.184-5.704-5.42-7.17-5.42-7.17-4.448-3.015.324-3.015.324-3.015 4.934.326 7.523 5.052 7.523 5.052 4.367 7.496 11.404 5.378 14.235 4.074.404-3.178 1.699-5.378 3.074-6.6-10.839-1.141-22.243-5.378-22.243-24.283 0-5.378 1.94-9.778 5.014-13.2-.485-1.222-2.184-6.275.486-13.038 0 0 4.125-1.304 13.426 5.052a46.97 46.97 0 0 1 12.214-1.63c4.125 0 8.33.571 12.213 1.63 9.302-6.356 13.427-5.052 13.427-5.052 2.67 6.763.97 11.816.485 13.038 3.155 3.422 5.015 7.822 5.015 13.2 0 18.905-11.404 23.06-22.324 24.283 1.78 1.548 3.316 4.481 3.316 9.126 0 6.6-.08 11.897-.08 13.526 0 1.304.89 2.853 3.316 2.364 19.412-6.52 33.405-24.935 33.405-46.691C97.707 22 75.788 0 48.854 0z'></path>
        </svg>
        """

    find_readme_html_output =
        Cmd.new("find")
        |> Cmd.args([examples_dir, "-type", "f", "-name", "README.html", "-exec", "realpath", "{}", ";"])
        |> Cmd.output!()

    when find_readme_html_output.status is
        Ok(0) ->
            find_stdout = Str.from_utf8(find_readme_html_output.stdout)?
            clean_readme_paths = Str.split_on(find_stdout, "\n")
            # TODO check output as expected

            List.for_each_try!(
                clean_readme_paths,
                |readme_path|
                    readme_html_str = File.read_utf8!(readme_path)?

                    example_folder_name = Str.split_on(readme_path, "/") |> List.take_last(2) |> List.first()?
                    specific_example_link = Str.join_with([examples_repo_link, example_folder_name], "/")
                    readme_str_edited = Str.replace_each(readme_html_str, "</h1>", """</h1><a id="gh-example-link" href="${specific_example_link}" aria-label="view on github">${github_logo_svg}</a>""")

                    File.write_utf8!(readme_path, readme_str_edited)
            )?
        _ ->
            Err(FindExamplesReadmeHtmlFailed(Inspect.to_str(find_readme_html_output)))?

    Stdout.line!("Website built in dir 'www/build'.")