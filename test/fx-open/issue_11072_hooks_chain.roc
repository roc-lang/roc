app [main!] { pf: platform "./platform/main.roc" }

# Regression test for https://github.com/roc-lang/roc/issues/11072
#
# A chain of effectful functions threads one record of hooks closures; every
# link unwraps a fallible hook with `?` and logs an interpolated string. Each
# interpolation mints a private iterator whose closure type shares one lambda
# set with every other link's, so lowering used to build the same recursive
# closure layout once per link and intern it once per copy, growing quartically
# with the chain length. The layout store must recognize the copies as one
# recursive layout.

import pf.Stdout

Worker := [].{
	start! : _ => Try({}, _)
	start! = |hooks| Worker.h6!(hooks)

	h1! : _ => Try({}, _)
	h1! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h1 ${items.len().to_str()}")
		Ok({})
	}

	h2! : _ => Try({}, _)
	h2! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h2 ${items.len().to_str()}")
		Worker.h1!(hooks)
	}

	h3! : _ => Try({}, _)
	h3! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h3 ${items.len().to_str()}")
		Worker.h2!(hooks)
	}

	h4! : _ => Try({}, _)
	h4! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h4 ${items.len().to_str()}")
		Worker.h3!(hooks)
	}

	h5! : _ => Try({}, _)
	h5! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h5 ${items.len().to_str()}")
		Worker.h4!(hooks)
	}

	h6! : _ => Try({}, _)
	h6! = |hooks| {
		info! = hooks.log.info!
		fetch! = hooks.fetch!
		items : List({ id : I64, name : Str })
		items = fetch!({})?
		info!("h6 ${items.len().to_str()}")
		Worker.h5!(hooks)
	}
}

main! = |_args| {
	line! = Stdout.line!
	_ = Worker.start!({ log: { info!: |m| line!(m) }, fetch!: |{}| Ok([{ id: 1, name: "a" }]) })?
	Ok({})
}
