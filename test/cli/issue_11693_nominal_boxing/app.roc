app [increment!] { pf: platform "platform.roc" }

increment! : U64 => U64
increment! = |n| n + 1
