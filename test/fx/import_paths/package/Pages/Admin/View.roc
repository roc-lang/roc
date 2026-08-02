import Hello as Sibling
import ./Hello as ExplicitSibling
import ../Hello as Parent
import ../../Shared/Hello as RelativeRoot
import /Shared/Hello as Root
import /Shared/Hello.SubType as ST exposing [decode]

View := [].{
    identity : ST -> ST
    identity = |value| value

    result = |_| Sibling.value({}) + ExplicitSibling.value({}) + Parent.value({}) + RelativeRoot.value({}) + Root.value({}) + decode({})
}
