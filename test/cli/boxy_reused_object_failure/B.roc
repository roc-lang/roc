import A
import Html

B := {}.{
    script : U64 -> Html
    script = |n| "<script>${A.other(n).to_str()}</script>"
}
