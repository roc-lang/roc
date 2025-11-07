import ui.Elem

## Example:
## datepicker({ date, format, enabled: False })
Datepicker :: {
    ## The initial date
    date : Date,

    ## Format of the displayed date
    format : Format,

    ## True if the datepicker is interactive
    enabled : Bool = True,
}.{
    DateChangedEvent := [Opened, Closed, PickedDate(Date)]

    datepicker : Datepicker -> Elem(DateChangedEvent)
    datepicker = Elem.custom({ init, subs, handle, render })
}

State :: {
    selected: Date,
    opened: Bool,
}

init : Datepicker -> State
init = |config| {
    selected: config.initial_date,
    opened: False,
}

subs : Datepicker, State -> List(Sub(Event))
subs = |_config, _state| Sub.config_changed(ConfigChanged)

handle : Datepicker, State, Event -> Act(Datepicker, Event, DatepickerEvent)
handle = |config, state, event| match event {
    ChangedYear(year) => Act.update(state.map_selected(.with_year(year)) })
    PressedDay(day) => Act.update(state.map_selected(.with_day(day))
    PressedNextMonth => Act.update(state.map_selected(.next_month()) })
    PressedPrevMonth => Act.update(state.map_selected(.prev_month()) })
    PressedOpen => Act.update_emit({ ..state, opened: True }, Opened)
    PressedCancel => Act.update_emit({ opened: False, selected: config.initial_date }, Closed)
    PressedOk => Act.update_emit(
        { ..state, opened: False },
        PickedDate(Date.ymd(state.year, state.month, state.day)),
    )
    ConfigChanged => if (state.opened) Act.none() else Act.update({ ..state, selected: config.date })
}

render : Datepicker, State -> Elem(Event)
render = |config, state|
    if (state.opened)
        col().p2().grow([
            (0, top_bar(state.selected.month(), state.selected.year())),
            (1, days_grid(state.selected.day())),
            (0, bottom_bar),
        ])
    else
        row().p2().space_between(4, [
            text(date.format(config.format)),
            button(text("Change"), PressedOpen),
        ])

bottom_bar : Elem(Event)
bottom_bar =
    row().p4().justify_right([
        button(text("Ok"), PressedOk),
    ])

month_selector : Month -> Elem(Event)
month_selector = |month|
    row().p8().space_between(4, [
        button(icons.left_arrow, PressedPrevMonth),
        text(month.to_str()),
        button(icons.right_arrow, PressedNextMonth),
    ])

top_bar : U8, U16 -> Elem(Event)
top_bar = |month, year|
    row().grow([
        (1, month_selector(month)),
        (0, text_input({ text: year.to_str(), placeholder: "Year" }, ChangedYear))
    ])

days_grid : U8 -> Elem(Event)
days_grid = |day|
    # ...etc
