app [main] { pf: platform "platform/main.roc" }

main : U64 -> Str
main = \num ->
    if num == 0 then
        "I need a positive number here!"
    else
        str = Num.to_str(num)

        "The number was $(str), OH YEAH!!! 🤘🤘"
