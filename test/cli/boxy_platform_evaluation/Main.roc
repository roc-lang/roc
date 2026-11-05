app [main!] { pf: platform "platform/main.roc" }
import Helper
main! = |expected| {
	if Helper.get() == expected {} else { crash "incorrect value" }
}
