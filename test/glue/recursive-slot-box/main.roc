platform "glue-recursive-slot-box"
	requires {
		unused : {} -> {}
	}
	exposes [Types]
	packages {}
	provides {
		"make_holder": make_holder,
		"make_tree": make_tree,
		"make_value": make_value,
		"make_node": make_node,
		"make_chain": make_chain,
		"make_wrapped": make_wrapped,
	}
	targets: {}

import Types

make_holder : {} -> { tree : Types.Tree }
make_holder = |_| { tree: Leaf }

make_tree : {} -> Types.Tree
make_tree = |_| Leaf

make_value : {} -> Types.Value
make_value = |_| { node: Leaf }

make_node : {} -> Types.Node
make_node = |_| Branch({ node: Leaf })

make_chain : {} -> Types.Chain
make_chain = |_| Nil

make_wrapped : {} -> { w : Types.Wrapper(U64) }
make_wrapped = |_| { w: Types.Wrapper.(1) }
