app "hello-rust"
    packages { pf: "platform" }
    imports [ Rbt.{ Rbt, Tool, tool, systemTool, Job, job, exec } ]
    provides [ main ] to pf

greeting =
    hi = "Hello"
    name = "World"

    "\(hi), \(name)!\n"

main = greeting

# todo: bikeshed "init" name more
init : Rbt
init =
    init { default: bundle }

# smallJob : Job
# smallJob =
#     job
#         {
#             command:
#                 exec (systemTool "printf")
#                     [ "Hello" ],
#             inputs: [],
#             inputFiles: [],
#             outputs: [],
#         }


# note: these rules could be much more compact but we're spelling them out
# explicitly for ease of understanding. Files using rbt do not have to be
# so verbose!
nixShell : Tool
nixShell =
    systemTool "nix-shell"


# wat2wasmBinary : Job
# wat2wasmBinary =
#     job
#         {
#             command: exec nixShell [ "-p", "wabt", "--run", "ln -s $(which wat2wasm) wat2wasm" ],
#             inputs: [],
#             inputFiles: [],
#             outputs: [ "wat2wasm" ]
#         }


wat2wasm : Tool
wat2wasm =
    # tool wat2wasmBinary "wat2wasm"
    systemTool "wat2wasm"


esbuildBinary : Job
esbuildBinary =
    job
        {
            command: exec nixShell [ "-p", "esbuild", "--run", "ln -s $(which esbuild) esbuild" ],
            inputs: [],
            inputFiles: [],
            outputs: [ "esbuild" ]
        }


esbuild : Tool
esbuild =
    # tool esbuildBinary "esbuild"
    systemTool "esbuild"


# #######################################
# # Done with tools, now for the build! #
# #######################################


addWasm : Job
addWasm =
    job
        {
            command: exec wat2wasm [ "hello.wat" ],
            inputs: [],
            inputFiles: [ "hello.wat" ],
            outputs: [ "hello.wasm" ],
        }


bundle : Job
bundle =
    job
        {
            command:
                exec esbuild
                    [
                        "--platform=node",
                        "--bundle",
                        "--loader:.wasm=binary",
                        "--minify",
                        "--sourcemap",
                        "--outfile=index.min.js",
                        "index.js",
                    ],
            inputs: [ addWasm ],
            inputFiles: [ "index.js" ],
            outputs: [ "index.min.js", "index.min.js.map" ],
        }
