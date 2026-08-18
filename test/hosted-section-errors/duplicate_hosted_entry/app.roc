# Regression for a platform header whose hosted section names one hosted
# function twice: the section decides how many host dispatch slots there are,
# so the extra entry is its own slot and checking reports the duplicate.
app [main!] { pf: platform "platform/main.roc" }

import pf.Host

main! = |n| Host.double!(n)
