import Attribute

Html := [Element(Attribute)].{
    input : Attribute -> Html
    input = |attr| Element(attr)

    render : Html -> Str
    render = |_| "<input>"
}
