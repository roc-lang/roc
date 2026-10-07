app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/11626
import pf.Stdout
find = |text, what| if text == "" Err(NotFound(what)) else Ok(text)

respond = |text| Ok(text)

stats_page = |text| {
	_ = find(text, IndexStats)?
	respond("stats")
}

lead_page = |text| {
	_ = find(text, LeadMissing)?
	respond("lead")
}

main! = |args| {
	# The platform includes argv[0]; no additional args exercises both errors.
	input = if List.len(args) == 1 "" else Str.join_with(args, " ")
	match stats_page(input) {
		Ok(page) => Stdout.line!(page)
		Err(_) => Stdout.line!("no stats")
	}
	match lead_page(input) {
		Ok(page) => Stdout.line!(page)
		Err(_) => Stdout.line!("no lead")
	}
	Ok({})
}
