module [
    append_rendered_static,
    init_server_app,
]

import Html.Internal.Shared exposing [Html, Attribute, App, translate_static, text, element]
import Json

# -------------------------------
#   STATIC HTML
# -------------------------------
append_rendered_static : Str, Html [] -> Str
append_rendered_static = \buffer, node ->
    when node is
        Text(content) ->
            Str.concat(buffer, content)

        Element(name, _, attrs, children) ->
            with_tag_name = "$(buffer)<$(name)"
            with_attrs =
                if List.is_empty(attrs) then
                    with_tag_name
                else
                    init = { buffer: Str.concat(with_tag_name, " "), styles: "" }
                    { buffer: attr_buffer, styles } =
                        List.walk(attrs, init, append_rendered_static_attr)

                    if Str.is_empty(styles) then
                        attr_buffer
                    else
                        "$(attr_buffer) style=\"$(styles)\""

            with_tag = Str.concat(with_attrs, ">")
            with_children = List.walk(children, with_tag, append_rendered_static)

            "$(with_children)</$(name)>"

        None -> buffer

append_rendered_static_attr : { buffer : Str, styles : Str }, Attribute [] -> { buffer : Str, styles : Str }
append_rendered_static_attr = \{ buffer, styles }, attr ->
    when attr is
        HtmlAttr(key, value) ->
            new_buffer = "$(buffer) $(key)=\"$(value)\""

            { buffer: new_buffer, styles }

        Style(key, value) ->
            new_styles = "$(styles) $(key): $(value);"

            { buffer, styles: new_styles }

        DomProp(_, _) -> { buffer, styles }

# -------------------------------
#   INITIALISATION
# -------------------------------
init_server_app : App state init_data, init_data, Str -> Result (Html []) [InvalidDocument] where init_data implements Encoding
init_server_app = \app, init_data, host_java_script ->
    init_data
    |> Ok
    |> app.init
    |> app.render
    |> translate_static
    |> insert_roc_script(init_data, app.wasm_url, host_java_script)

insert_roc_script : Html [], init_data, Str, Str -> Result (Html []) [InvalidDocument] where init_data implements Encoding
insert_roc_script = \document, init_data, wasm_url, host_java_script ->
    encode =
        \value ->
            value
            |> Encode.to_bytes(Json.json)
            |> Str.from_utf8
            |> Result.with_default("")

    # Convert initData to JSON as a Roc Str, then convert the Roc Str to a JS string.
    # JSON won't have invalid UTF-8 in it, since it would be escaped as part of JSON encoding.
    js_init_data =
        init_data |> encode |> encode

    js_wasm_url =
        encode(wasm_url)

    script : Html []
    script = element("script")([], [
        text(
            """
            $(host_java_script)
            (function(){
            const initData = $(js_init_data);
            const wasmUrl = $(js_wasm_url);
            window.roc = roc_init(initData, wasmUrl);
            })();
            """,
        ),
    ])

    # append the <script> to the end of the <body>
    when document is
        Element("html", h_size, h_attrs, h_children) ->
            empty = List.with_capacity(List.len(h_children))
            walk_result =
                List.walk(h_children, { new_html_children: empty, found_body: Bool.false }, \{ new_html_children, found_body }, h_child ->
                    when h_child is
                        Element("body", b_size, b_attrs, b_children) ->
                            {
                                new_html_children: List.append(new_html_children, Element("body", b_size, b_attrs, List.append(b_children, script))),
                                found_body: Bool.true,
                            }

                        _ ->
                            {
                                new_html_children: List.append(new_html_children, h_child),
                                found_body,
                            })

            if walk_result.found_body then
                Ok(Element("html", h_size, h_attrs, walk_result.new_html_children))
            else
                Err(InvalidDocument)

        _ -> Err(InvalidDocument)
