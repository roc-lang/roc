app [main!] { pf: platform "./platform/main.roc" }

# Regression test for issue #8665: InvalidMethodReceiver crash on static dispatch for Try type
# The traditional function call syntax works:
#     _str1 = Try.ok_or(List.get(list, 0), "")
# But the method call syntax crashes:
#     _str2 = List.get(list, 0).ok_or("")
main! = || {
    list = [""]
    _str1 = Try.ok_or(List.get(list, 0), "")
    _str2 = List.get(list, 0).ok_or("")
}
