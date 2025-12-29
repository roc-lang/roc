app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# The recursive type
Node := [
    Element(Str, List(Node)),
    Text(Str),
]

main! = || {
    # Create an Element node
    element_val : Node
    element_val = Element("span", [])

    # Store in list
    element_list : List(Node)
    element_list = [element_val]

    # Get it back
    match List.first(element_list) {
        Ok(v) => {
            # Now do a match
            match v {
                Element(tag, _) => Stdout.line!("Got Element: ${tag}")
                Text(text) => Stdout.line!("Got Text: ${text}")
            }
        }
        Err(_) => Stdout.line!("Error")
    }
}
