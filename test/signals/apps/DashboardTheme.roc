DashboardTheme :: [].{
	Tone : [ToneNeutral, ToneInfo, ToneGood, ToneWatch, ToneBad, ToneError]

	page_class : Str
	page_class = "min-h-screen bg-zinc-50 text-zinc-950"

	shell_class : Str
	shell_class = "mx-auto grid max-w-7xl gap-4 px-4 py-4 sm:px-6 lg:px-8"

	toolbar_class : Str
	toolbar_class = "flex flex-wrap items-center justify-between gap-3 border-b border-zinc-200 bg-white px-4 py-3 sm:px-5"

	toolbar_text_class : Str
	toolbar_text_class = "grid gap-1"

	toolbar_status_class : Str
	toolbar_status_class = "text-sm text-zinc-600"

	toolbar_actions_class : Str
	toolbar_actions_class = "flex flex-wrap items-center gap-3 text-sm text-zinc-600"

	primary_button_class : Str
	primary_button_class = "rounded-md border border-zinc-900 bg-zinc-900 px-3 py-2 text-sm font-medium text-white hover:border-zinc-700 hover:bg-zinc-700"

	metric_grid_class : Str
	metric_grid_class = "grid gap-3 sm:grid-cols-2 xl:grid-cols-5"

	main_grid_class : Str
	main_grid_class = "grid gap-4 xl:grid-cols-3"

	wide_column_class : Str
	wide_column_class = "grid gap-4 xl:col-span-2"

	side_column_class : Str
	side_column_class = "grid gap-4"

	panel_class : Str
	panel_class = "grid gap-3 rounded-md border border-zinc-200 bg-white p-4"

	service_grid_class : Str
	service_grid_class = "grid gap-3 sm:grid-cols-2 xl:grid-cols-3"

	status_item_class : Str
	status_item_class = "grid gap-1"

	status_label_class : Str
	status_label_class = "text-xs font-semibold uppercase tracking-normal"

	status_value_class : Str
	status_value_class = "text-lg font-semibold"

	status_detail_class : Str
	status_detail_class = "text-xs leading-5"

	section_heading_class : Str
	section_heading_class = "text-sm font-semibold uppercase tracking-normal text-zinc-600"

	metric_label_class : Str
	metric_label_class = "text-xs font-semibold uppercase tracking-normal text-zinc-500"

	metric_value_class : Str
	metric_value_class = "text-2xl font-semibold"

	metric_detail_class : Str
	metric_detail_class = "text-sm leading-5"

	row_label_class : Str
	row_label_class = "text-xs font-semibold uppercase tracking-normal text-zinc-500"

	row_value_class : Str
	row_value_class = "font-mono text-sm text-zinc-950"

	row_bar_class : Str
	row_bar_class = "font-mono text-sm text-zinc-700"

	row_note_class : Str
	row_note_class = "text-sm text-zinc-600"

	traffic_row_class : Str
	traffic_row_class = "grid gap-1 border-t border-zinc-100 py-2 sm:grid-cols-4 sm:gap-3"

	app_heading_class : Str
	app_heading_class = "text-xl font-semibold text-zinc-950"

	row_header_class : Str
	row_header_class = "flex items-center justify-between gap-2"

	wrap_row_header_class : Str
	wrap_row_header_class = "flex flex-wrap items-center justify-between gap-2"

	inline_header_class : Str
	inline_header_class = "flex flex-wrap items-center gap-2"

	strong_text_class : Str
	strong_text_class = "font-semibold"

	mono_xs_class : Str
	mono_xs_class = "font-mono text-xs"

	mono_sm_class : Str
	mono_sm_class = "font-mono text-sm"

	text_sm_class : Str
	text_sm_class = "text-sm"

	text_xs_class : Str
	text_xs_class = "text-xs"

	job_detail_grid_class : Str
	job_detail_grid_class = "grid gap-1 sm:grid-cols-4"

	remote_card_class : Tone -> Str
	remote_card_class = |tone| "grid gap-2 rounded-md border p-4 text-sm ${tone_panel_class(tone)}"

	remote_inline_class : Tone -> Str
	remote_inline_class = |tone| "grid gap-1 py-2 text-sm ${remote_text_class(tone)}"

	status_strip_class : Tone -> Str
	status_strip_class = |tone| "grid gap-3 rounded-md border p-4 sm:grid-cols-2 xl:grid-cols-4 ${tone_panel_class(tone)}"

	metric_class : Tone -> Str
	metric_class = |tone| "grid min-h-32 gap-2 rounded-md border p-4 ${tone_panel_class(tone)}"

	service_class : Tone -> Str
	service_class = |tone|
		match tone {
			ToneBad => "grid gap-1 rounded-md border border-red-300 bg-red-50 p-3 text-red-950"
			ToneError => "grid gap-1 rounded-md border border-red-300 bg-red-50 p-3 text-red-950"
			ToneWatch => "grid gap-1 rounded-md border border-amber-300 bg-amber-50 p-3 text-amber-950"
			ToneGood => "grid gap-1 rounded-md border border-zinc-200 bg-white p-3 text-zinc-950"
			ToneInfo => "grid gap-1 rounded-md border border-zinc-200 bg-white p-3 text-zinc-950"
			ToneNeutral => "grid gap-1 rounded-md border border-zinc-200 bg-white p-3 text-zinc-500"
		}

	job_class : Tone -> Str
	job_class = |tone|
		match tone {
			ToneBad => "grid gap-2 rounded-md border border-red-300 bg-red-50 p-3 text-sm text-red-950"
			ToneError => "grid gap-2 rounded-md border border-red-300 bg-red-50 p-3 text-sm text-red-950"
			ToneWatch => "grid gap-2 rounded-md border border-amber-300 bg-amber-50 p-3 text-sm text-amber-950"
			ToneGood => "grid gap-2 rounded-md border border-zinc-200 bg-white p-3 text-sm text-zinc-900"
			ToneInfo => "grid gap-2 rounded-md border border-zinc-200 bg-white p-3 text-sm text-zinc-900"
			ToneNeutral => "grid gap-2 rounded-md border border-zinc-200 bg-white p-3 text-sm text-zinc-500"
		}

	alert_class : Tone -> Str
	alert_class = |tone|
		match tone {
			ToneBad => "grid gap-1 rounded-md border border-red-300 bg-red-50 p-3 text-sm text-red-950"
			ToneError => "grid gap-1 rounded-md border border-red-300 bg-red-50 p-3 text-sm text-red-950"
			ToneWatch => "grid gap-1 rounded-md border border-amber-300 bg-amber-50 p-3 text-sm text-amber-950"
			ToneGood => "grid gap-1 rounded-md border border-zinc-200 bg-white p-3 text-sm text-zinc-800"
			ToneInfo => "grid gap-1 rounded-md border border-zinc-200 bg-white p-3 text-sm text-zinc-800"
			ToneNeutral => "grid gap-1 rounded-md border border-zinc-200 bg-white p-3 text-sm text-zinc-500"
		}
}

tone_panel_class : DashboardTheme.Tone -> Str
tone_panel_class = |tone|
	match tone {
		ToneBad => "border-red-300 bg-red-50 text-red-950"
		ToneError => "border-red-300 bg-red-50 text-red-950"
		ToneWatch => "border-amber-300 bg-amber-50 text-amber-950"
		ToneGood => "border-emerald-200 bg-emerald-50 text-emerald-950"
		ToneInfo => "border-sky-200 bg-sky-50 text-zinc-950"
		ToneNeutral => "border-zinc-200 bg-white text-zinc-700"
	}

remote_text_class : DashboardTheme.Tone -> Str
remote_text_class = |tone|
	match tone {
		ToneBad => "text-red-950"
		ToneError => "text-red-950"
		ToneWatch => "text-amber-950"
		ToneGood => "text-emerald-950"
		ToneInfo => "text-zinc-700"
		ToneNeutral => "text-zinc-600"
	}
