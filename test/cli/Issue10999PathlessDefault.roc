# Repro for https://github.com/roc-lang/roc/issues/10999: a well-typed program
# panics during Monotype interface replay with "postcheck invariant violated:
# instantiation unified two different primitive types". A numeric requirement
# reached through a constraint-callable argument (`group_chain`'s
# `input.edges.len() + 1`, discovered while draining the constraint fn variable
# inside `node_chain`) is published pathless, so checked-artifact evidence
# publication defaults it to `Dec` while Monotype specializes the same dispatch
# callable as the concrete `U64` request. Minimized from `roc-graph-layout`'s
# Route.roc/EdgeRoutes.roc/Geom.roc at commit bf8c02e1.

Router :: {}.{
	# A constraint-callable target: `node_chain` passes `input.edges.len() + 1`
	# through it, and the keep_if wrapper below makes the requirement reachable
	# only through nested constraint callables.
	group_chain = |groups, _fuel| groups

	node_chain = |_node, input| match input.memberships.get(0) {
		Ok(_) => Router.group_chain([], input.edges.len() + 1)
		Err(_) => []
	}

	resolve_group_attachments = |input, _settings| {
		uses = input.edges.fold_with_index(
			[],
			|acc, edge, edge_index| Router.node_chain(edge.to, input).keep_if(|_| !Router.node_chain(edge.from, input).is_empty()).fold(
				acc,
				|found, group| {
					found.append({ edge: edge_index, group, side: Top, attachment: Automatic })
				},
			),
		)
		uses.map(
			|use| { edge: use.edge, group: use.group, attachment: Fixed({ side: use.side, offset: 0 }) },
		)
	}

	resolve_input = |input, settings| {
		{
			..input,
			group_attachments: Router.resolve_group_attachments(input, settings),
		}
	}

	compute = |_input, _settings| {
		{ layout: { positions: [], bounds: { x: 0, y: 0, width: 0, height: 0 } } }
	}
}

Issue10999PathlessDefault :: {}.{
	Point : { x : F64, y : F64 }
	Rect : { x : F64, y : F64, width : F64, height : F64 }
	Side : [Top, Right, Bottom, Left]
	Attachment : [Automatic, On(Side), Fixed({ side : Side, offset : F64 })]
	Membership : { node : U64 }
	GroupAttachmentRule : { edge : U64, group : U64, attachment : Attachment }

	Input : { edges : List({ from : U64, to : U64 }), memberships : List(Membership), group_attachments : List(GroupAttachmentRule) }

	Settings : {}
	Result : { layout : { positions : List(Point), bounds : Rect } }

	default_input : Input
	default_input = { edges: [], memberships: [], group_attachments: [] }

	default_settings : Settings
	default_settings = {}

	layout : Input, Settings -> Result
	layout = |input, settings| {
		resolved = Router.resolve_input(input, settings)
		Router.compute(resolved, settings)
	}
}

expect Issue10999PathlessDefault.layout(Issue10999PathlessDefault.default_input, Issue10999PathlessDefault.default_settings) == { layout: { positions: [], bounds: { x: 0, y: 0, width: 0, height: 0 } } }
