app [main] { pf: platform "../platform/main.roc" }

import pf.Elem exposing [Elem]
import pf.Html
import pf.Signal
import pf.Ui

Column : {
	id : Str,
	title : Str,
	limit : U64,
}

Card : {
	id : Str,
	title : Str,
	owner : Str,
	priority : Str,
	tags : List(Str),
	estimate : U64,
	status : Str,
	notes : U64,
}

DragState := [Idle, Dragging(Str)]

HoverState := [NoHover, HoverEnd(Str), HoverBefore(Str, Str)]

Board : {
	cards : List(Card),
	dragging : DragState,
	hover : HoverState,
	reviewer : Str,
	focus_high_priority : Bool,
}

concat3 : Str, Str, Str -> Str
concat3 = |a, b, c| Str.concat(Str.concat(a, b), c)

concat4 : Str, Str, Str, Str -> Str
concat4 = |a, b, c, d| Str.concat(concat3(a, b, c), d)

backlog_id : Str
backlog_id = "backlog"

ready_id : Str
ready_id = "ready"

progress_id : Str
progress_id = "progress"

review_id : Str
review_id = "review"

done_id : Str
done_id = "done"

columns : List(Column)
columns = [
	{ id: backlog_id, title: "Backlog", limit: 6 },
	{ id: progress_id, title: "In Progress", limit: 3 },
	{ id: done_id, title: "Done", limit: 99 },
]

initial_cards : List(Card)
initial_cards = [
	{
		id: "CARD-101",
		title: "Design signal graph",
		owner: "Mara",
		priority: "High",
		tags: ["signals", "engine"],
		estimate: 5,
		status: backlog_id,
		notes: 0,
	},
	{
		id: "CARD-102",
		title: "Write platform glue",
		owner: "Noah",
		priority: "Medium",
		tags: ["wasm", "abi"],
		estimate: 3,
		status: backlog_id,
		notes: 0,
	},
	{
		id: "CARD-103",
		title: "Tune keyed diff",
		owner: "Ada",
		priority: "High",
		tags: ["ui.each", "metrics"],
		estimate: 2,
		status: backlog_id,
		notes: 0,
	},
	{
		id: "CARD-201",
		title: "QA browser runtime",
		owner: "Ilya",
		priority: "Low",
		tags: ["browser", "events"],
		estimate: 2,
		status: progress_id,
		notes: 0,
	},
	{
		id: "CARD-301",
		title: "Model drag payloads",
		owner: "Sam",
		priority: "High",
		tags: ["events", "state"],
		estimate: 4,
		status: progress_id,
		notes: 0,
	},
	{
		id: "CARD-401",
		title: "Review structural budgets",
		owner: "Rin",
		priority: "Medium",
		tags: ["spec", "metrics"],
		estimate: 1,
		status: progress_id,
		notes: 0,
	},
	{
		id: "CARD-501",
		title: "Document host contract",
		owner: "Lee",
		priority: "Low",
		tags: ["docs"],
		estimate: 1,
		status: done_id,
		notes: 0,
	},
]

initial_board : Board
initial_board = {
	cards: initial_cards,
	dragging: Idle,
	hover: NoHover,
	reviewer: "",
	focus_high_priority: False,
}

page_class : Str
page_class = "grid min-h-screen gap-5 bg-zinc-100 text-zinc-950"

toolbar_class : Str
toolbar_class = "grid gap-4 border border-zinc-200 bg-white p-5 shadow-sm"

toolbar_top_class : Str
toolbar_top_class = "grid gap-3 lg:grid-cols-3"

toolbar_title_class : Str
toolbar_title_class = "grid gap-1 lg:col-span-2"

eyebrow_class : Str
eyebrow_class = "text-xs font-semibold uppercase text-emerald-700"

toolbar_copy_class : Str
toolbar_copy_class = "text-sm text-zinc-600"

metric_grid_class : Str
metric_grid_class = "grid gap-2 sm:grid-cols-3"

metric_card_class : Str
metric_card_class = "grid gap-1 border border-zinc-200 bg-zinc-50 p-3"

metric_label_class : Str
metric_label_class = "text-xs font-semibold uppercase text-zinc-500"

metric_value_class : Str
metric_value_class = "text-lg font-semibold text-zinc-950"

controls_class : Str
controls_class = "flex flex-wrap items-end gap-3"

board_class : Str
board_class = "grid gap-4 lg:grid-cols-3"

column_class : Str
column_class = "grid min-h-32 content-start gap-3 border border-zinc-200 bg-zinc-50 p-3 shadow-sm"

column_heading_class : Str
column_heading_class = "text-sm font-semibold text-zinc-950"

column_summary_class : Str
column_summary_class = "text-xs font-medium uppercase text-zinc-500"

card_base_class : Str
card_base_class = "grid cursor-grab select-none touch-none gap-3 border border-zinc-200 bg-white p-4 text-left shadow-sm transition hover:border-zinc-400"

card_drag_class : Str
card_drag_class = "grid cursor-grabbing select-none touch-none gap-3 border border-emerald-500 bg-emerald-50 p-4 text-left shadow-sm"

card_hover_class : Str
card_hover_class = "grid cursor-grab select-none touch-none gap-3 border border-sky-500 bg-sky-50 p-4 text-left shadow-sm"

drop_zone_class : Str
drop_zone_class = "select-none touch-none border border-dashed border-zinc-300 bg-white p-4 text-center text-sm font-medium text-zinc-500"

drop_zone_active_class : Str
drop_zone_active_class = "select-none touch-none border border-dashed border-emerald-500 bg-emerald-50 p-4 text-center text-sm font-semibold text-emerald-800"

card_header_class : Str
card_header_class = "grid gap-1"

card_title_class : Str
card_title_class = "text-sm font-semibold leading-5 text-zinc-950"

card_id_class : Str
card_id_class = "text-xs font-semibold uppercase text-zinc-500"

card_meta_grid_class : Str
card_meta_grid_class = "grid gap-2 sm:grid-cols-2"

card_meta_class : Str
card_meta_class = "text-xs text-zinc-600"

card_tag_class : Str
card_tag_class = "text-xs font-medium text-emerald-700"

card_footer_class : Str
card_footer_class = "flex flex-wrap items-center justify-between gap-2 border-t border-zinc-100 pt-3"

note_text_class : Str
note_text_class = "text-xs font-medium text-zinc-500"

note_button_class : Str
note_button_class = "border-zinc-300 bg-zinc-50 text-zinc-800 hover:border-zinc-400 hover:bg-white"

button_class : Str
button_class = "border-zinc-300 bg-white text-zinc-900 hover:border-zinc-500"

primary_button_class : Str
primary_button_class = "border-emerald-700 bg-emerald-600 text-white hover:border-emerald-800 hover:bg-emerald-700"

input_class : Str
input_class = "w-full max-w-md"

column_title : Str -> Str
column_title = |column_id| {
	match List.find_first(columns, |column| column.id == column_id) {
		Ok(column) => column.title
		Err(_) => column_id
	}
}

card_title : List(Card), Str -> Str
card_title = |cards, card_id| {
	match List.find_first(cards, |card| card.id == card_id) {
		Ok(card) => card.title
		Err(_) => card_id
	}
}

join_tags : List(Str) -> Str
join_tags = |tags| {
	List.fold(
		tags,
		"",
		|acc, tag| if acc == "" {
			tag
		} else {
			concat3(acc, ", ", tag)
		},
	)
}

priority_visible : Bool, Card -> Bool
priority_visible = |focus_high_priority, card| {
	if focus_high_priority {
		card.priority == "High"
	} else {
		True
	}
}

column_cards : Board, Str -> List(Card)
column_cards = |board, column_id| {
	List.keep_if(
		board.cards,
		|card| (card.status == column_id) and priority_visible(board.focus_high_priority, card),
	)
}

visible_cards : Board -> List(Card)
visible_cards = |board| List.keep_if(board.cards, |card| priority_visible(board.focus_high_priority, card))

visible_card_count : Board -> U64
visible_card_count = |board| List.len(visible_cards(board))

visible_estimate : Board -> U64
visible_estimate = |board| List.fold(visible_cards(board), 0, |total, card| total + card.estimate)

column_summary : Board, Column -> Str
column_summary = |board, column| {
	count = List.len(column_cards(board, column.id))
	if column.id == done_id {
		concat3(count.to_str(), " completed", "")
	} else {
		concat4(count.to_str(), " / ", column.limit.to_str(), " cards")
	}
}

drag_status_text : Board -> Str
drag_status_text = |board| {
	match board.dragging {
		Idle => "Board ready"
		Dragging(card_id) => {
			title = card_title(board.cards, card_id)
			match board.hover {
				NoHover => concat3("Dragging ", title, "")
				HoverEnd(column_id) => concat4("Dragging ", title, " over ", column_title(column_id))
				HoverBefore(column_id, before_id) => {
					before_title = card_title(board.cards, before_id)
					concat4(concat4("Dragging ", title, " before ", before_title), " in ", column_title(column_id), "")
				}
			}
		}
	}
}

reviewer_text : Board -> Str
reviewer_text = |board| {
	if board.reviewer == "" {
		"No reviewer assigned"
	} else {
		Str.concat("Reviewing with ", board.reviewer)
	}
}

focus_text : Board -> Str
focus_text = |board| {
	if board.focus_high_priority {
		"High priority focus"
	} else {
		"All priorities"
	}
}

visible_cards_text : Board -> Str
visible_cards_text = |board| concat3(visible_card_count(board).to_str(), " cards", "")

visible_points_text : Board -> Str
visible_points_text = |board| concat3(visible_estimate(board).to_str(), " points", "")

done_cards_text : Board -> Str
done_cards_text = |board| concat3(List.len(column_cards(board, done_id)).to_str(), " done", "")

start_drag : Board, Str -> Board
start_drag = |board, card_id| {
	{ ..board, dragging: Dragging(card_id), hover: NoHover }
}

cancel_drag : Board -> Board
cancel_drag = |board| {
	{ ..board, dragging: Idle, hover: NoHover }
}

hover_end : Board, Str -> Board
hover_end = |board, column_id| {
	match board.dragging {
		Idle => board
		Dragging(_) => { ..board, hover: HoverEnd(column_id) }
	}
}

hover_before : Board, Str, Str -> Board
hover_before = |board, column_id, before_id| {
	match board.dragging {
		Idle => board
		Dragging(card_id) => if card_id == before_id {
			{ ..board, hover: NoHover }
		} else {
			{ ..board, hover: HoverBefore(column_id, before_id) }
		}
	}
}

clear_hover : Board -> Board
clear_hover = |board| {
	{ ..board, hover: NoHover }
}

increment_card_notes : Board, Str -> Board
increment_card_notes = |board, card_id| {
	{
		..board,
		cards: List.map(
			board.cards,
			|card| if card.id == card_id {
				{ ..card, notes: card.notes + 1 }
			} else {
				card
			},
		),
	}
}

insert_before : List(Card), Card, Str -> List(Card)
insert_before = |cards, moved, before_id| {
	state =
		List.fold(
			cards,
			{ output: [], inserted: False },
			|acc, card| {
				if (!acc.inserted) and card.id == before_id {
					{
						output: List.append(List.append(acc.output, moved), card),
						inserted: True,
					}
				} else {
					{
						output: List.append(acc.output, card),
						inserted: acc.inserted,
					}
				}
			},
		)

	if state.inserted {
		state.output
	} else {
		List.append(state.output, moved)
	}
}

move_dragging_card : Board, HoverState -> Board
move_dragging_card = |board, target| {
	match board.dragging {
		Idle => board
		Dragging(card_id) => {
			match List.find_first(board.cards, |card| card.id == card_id) {
				Ok(card) => {
					without_card = List.keep_if(board.cards, |candidate| candidate.id != card_id)
					next_cards =
						match target {
							NoHover => board.cards
							HoverEnd(column_id) => List.append(without_card, { ..card, status: column_id })
							HoverBefore(column_id, before_id) => {
								if card_id == before_id {
									board.cards
								} else {
									insert_before(without_card, { ..card, status: column_id }, before_id)
								}
							}
						}

					{ ..board, cards: next_cards, dragging: Idle, hover: NoHover }
				}
				Err(_) => cancel_drag(board)
			}
		}
	}
}

drop_on_end : Board, Str -> Board
drop_on_end = |board, column_id| move_dragging_card(board, HoverEnd(column_id))

drop_before : Board, Str, Str -> Board
drop_before = |board, column_id, before_id| move_dragging_card(board, HoverBefore(column_id, before_id))

card_class : Board, Card -> Str
card_class = |board, card| {
	match board.dragging {
		Dragging(card_id) => if card_id == card.id {
			card_drag_class
		} else {
			match board.hover {
				HoverBefore(_, before_id) => if before_id == card.id { card_hover_class } else { card_base_class }
				_ => card_base_class
			}
		}
		Idle => card_base_class
	}
}

drop_zone_class_for : Board, Str -> Str
drop_zone_class_for = |board, column_id| {
	match board.hover {
		HoverEnd(active_column) => if active_column == column_id { drop_zone_active_class } else { drop_zone_class }
		_ => drop_zone_class
	}
}

card_meta_text : Card -> Str
card_meta_text = |card| {
	concat4(
		concat4(card.owner, " owns ", card.id, " - "),
		card.priority,
		" priority - ",
		concat3(card.estimate.to_str(), " points", ""),
	)
}

card_status_text : Card -> Str
card_status_text = |card| Str.concat("Column: ", column_title(card.status))

card_priority_text : Card -> Str
card_priority_text = |card| Str.concat(card.priority, " priority")

card_estimate_text : Card -> Str
card_estimate_text = |card| concat3(card.estimate.to_str(), " pts", "")

card_tags_text : Card -> Str
card_tags_text = |card| Str.concat("Tags: ", join_tags(card.tags))

note_label : Card -> Str
note_label = |card| concat4("Notes on ", card.title, ": ", card.notes.to_str())

render_card : Ui.State(Board), Str, Str, Signal.Signal(Card) -> Elem
render_card = |board_state, column_id, card_id, card_signal| {
	board_signal = board_state.signal()
	class_signal = Signal.map2(board_signal, card_signal, card_class)
	title_signal = Signal.map(card_signal, |card| card.title)
	meta_signal = Signal.map(card_signal, card_meta_text)
	status_signal = Signal.map(card_signal, card_status_text)
	priority_signal = Signal.map(card_signal, card_priority_text)
	estimate_signal = Signal.map(card_signal, card_estimate_text)
	tags_signal = Signal.map(card_signal, card_tags_text)
	note_signal = Signal.map(card_signal, note_label)

	Html.section(
		Str.concat("Card: ", card_title(initial_cards, card_id)),
		[
			Html.class_attr_s(class_signal),
			Html.on_pointer_down(board_state.on_unit(|board| start_drag(board, card_id))),
			Html.on_pointer_enter(board_state.on_unit(|board| hover_before(board, column_id, card_id))),
			Html.on_pointer_up(board_state.on_unit(|board| drop_before(board, column_id, card_id))),
			Html.on_pointer_leave(board_state.on_unit(clear_hover)),
		],
		[
			Html.div_c(
				card_header_class,
				[
					Html.paragraph_c(card_id, card_id_class),
					Html.paragraph_s_c(title_signal, card_title_class),
				],
			),
			Html.div_c(
				card_meta_grid_class,
				[
					Html.paragraph_s_c(priority_signal, card_meta_class),
					Html.paragraph_s_c(estimate_signal, card_meta_class),
				],
			),
			Html.paragraph_s_c(meta_signal, card_meta_class),
			Html.paragraph_s_c(status_signal, card_meta_class),
			Html.paragraph_s_c(tags_signal, card_tag_class),
			Html.div_c(
				card_footer_class,
				[
					Html.paragraph_s_c(note_signal, note_text_class),
					Html.button_c(Str.concat("Add note ", card_title(initial_cards, card_id)), note_button_class, board_state.on_unit(|board| increment_card_notes(board, card_id))),
				],
			),
		],
	)
}

render_column : Ui.State(Board), Str, Signal.Signal(Column) -> Elem
render_column = |board_state, column_id, column_signal| {
	board_signal = board_state.signal()
	cards_signal = Signal.map(board_signal, |board| column_cards(board, column_id))
	summary_signal = Signal.map2(board_signal, column_signal, column_summary)
	drop_class_signal = Signal.map(board_signal, |board| drop_zone_class_for(board, column_id))
	end_label = concat3("Drop: ", column_title(column_id), " end")

	Html.section(
		column_title(column_id),
		[Html.class_attr(column_class)],
		[
			Html.heading_c(column_title(column_id), column_heading_class),
			Html.paragraph_s_c(summary_signal, column_summary_class),
			Ui.each_str(cards_signal, |card| card.id, |card_id, card| render_card(board_state, column_id, card_id, card)),
			Html.section(
				end_label,
				[
					Html.class_attr_s(drop_class_signal),
					Html.on_pointer_enter(board_state.on_unit(|board| hover_end(board, column_id))),
					Html.on_pointer_up(board_state.on_unit(|board| drop_on_end(board, column_id))),
					Html.on_pointer_leave(board_state.on_unit(clear_hover)),
				],
				[
					Html.paragraph("Drop card here"),
				],
			),
		],
	)
}

main : {} -> Elem
main = |_| {
	Ui.state(
		initial_board,
		|board| {
			board_signal = board.signal()
			drag_status = Signal.map(board_signal, drag_status_text)
			reviewer_label = Signal.map(board_signal, reviewer_text)
			filter_label = Signal.map(board_signal, focus_text)
			card_metric = Signal.map(board_signal, visible_cards_text)
			point_metric = Signal.map(board_signal, visible_points_text)
			done_metric = Signal.map(board_signal, done_cards_text)
			column_signal = Signal.const(columns)

			Html.div_c(
				page_class,
				[
					Html.section_c(
						"Board controls",
						toolbar_class,
						[
							Html.div_c(
								toolbar_top_class,
								[
									Html.div_c(
										toolbar_title_class,
										[
											Html.paragraph_c("Product delivery", eyebrow_class),
											Html.heading_c("Release board", "text-2xl font-semibold text-zinc-950"),
											Html.paragraph_c("Plan the next Signals milestone across backlog, active work, and completed cards.", toolbar_copy_class),
											Html.paragraph_s_c(drag_status, "text-sm font-medium text-emerald-700"),
										],
									),
									Html.div_c(
										metric_grid_class,
										[
											Html.div_c(
												metric_card_class,
												[
													Html.paragraph_c("Visible", metric_label_class),
													Html.paragraph_s_c(card_metric, metric_value_class),
												],
											),
											Html.div_c(
												metric_card_class,
												[
													Html.paragraph_c("Scope", metric_label_class),
													Html.paragraph_s_c(point_metric, metric_value_class),
												],
											),
											Html.div_c(
												metric_card_class,
												[
													Html.paragraph_c("Shipped", metric_label_class),
													Html.paragraph_s_c(done_metric, metric_value_class),
												],
											),
										],
									),
								],
							),
							Html.div_c(
								controls_class,
								[
									Html.text_input_c("Reviewer", Signal.map(board_signal, |value| value.reviewer), input_class, board.on_str(|state, value| { ..state, reviewer: value })),
									Html.paragraph_s_c(reviewer_label, toolbar_copy_class),
									Html.paragraph_s_c(filter_label, toolbar_copy_class),
									Html.button_c("Focus high priority", button_class, board.on_unit(|state| { ..state, focus_high_priority: !state.focus_high_priority })),
									Html.button_c("Clear drag", button_class, board.on_unit(cancel_drag)),
									Html.button_c("Reset demo", primary_button_class, board.on_unit(|_| initial_board)),
								],
							),
						],
					),
					Html.div_c(
						board_class,
						[
							Ui.each_str(column_signal, |column| column.id, |column_id, column| render_column(board, column_id, column)),
						],
					),
				],
			)
		},
	)
}
