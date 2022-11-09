
pub fn run_event_loop(title: &str, window_bounds: crate::glue::Bounds) {
    
    let (mut model, mut elems) = crate::roc::init_and_render(window_bounds);

    crossterm::terminal::enable_raw_mode().unwrap();
    let mut stdout = std::io::stdout();
    crossterm::execute!(
        stdout, 
        crossterm::terminal::EnterAlternateScreen, 
        crossterm::event::EnableMouseCapture
    ).unwrap();
    let backend = tui::backend::CrosstermBackend::new(stdout);
    let mut terminal = tui::Terminal::new(backend).unwrap();

    let tick_rate = std::time::Duration::from_millis(200);
    let events = Events::new(tick_rate);
    
    loop {
        let mut appReturn = false;
        
        terminal.draw(|f| {
            for elem in &elems {
                renderWidget(f, f.size(), &elem)
            }
        }).unwrap();

        let result = match events.next().unwrap() {
            InputEvent::Input(key) => {
                appReturn = true;
            },
            InputEvent::Tick => {},
        };
        if appReturn {
            break;
        }
    }

    // restore terminal
    crossterm::terminal::disable_raw_mode().unwrap();
    crossterm::execute!(
        terminal.backend_mut(),
        crossterm::terminal::LeaveAlternateScreen,
        crossterm::event::DisableMouseCapture
    ).unwrap();
    terminal.show_cursor().unwrap();

}

pub enum InputEvent {
    Input(crossterm::event::KeyEvent),
    Tick,
}

pub struct Events {
    rx: std::sync::mpsc::Receiver<InputEvent>,
    _tx: std::sync::mpsc::Sender<InputEvent>,
}

impl Events {
    pub fn new(tick_rate: std::time::Duration) -> Events {
        let (tx, rx) = std::sync::mpsc::channel();

        let event_tx = tx.clone(); // the thread::spawn own event_tx 
        std::thread::spawn(move || {
            loop {
                // poll for tick rate duration, if no event, sent tick event.
                if crossterm::event::poll(tick_rate).unwrap() {
                    if let crossterm::event::Event::Key(key) = crossterm::event::read().unwrap() {
                        let key = crossterm::event::KeyEvent::from(key);
                        event_tx.send(InputEvent::Input(key)).unwrap();
                    }
                }
                event_tx.send(InputEvent::Tick).unwrap();
            }
        });

        Events { rx, _tx: tx }
    }

    /// Attempts to read an event.
    /// This function block the current thread.
    pub fn next(&self) -> Result<InputEvent, std::sync::mpsc::RecvError> {
        self.rx.recv()
    }
}

fn renderWidget<B: tui::backend::Backend>(f: &mut tui::Frame<B>, area : tui::layout::Rect, elem : &crate::glue::Elem){
    match elem.discriminant(){
        crate::glue::DiscriminantElem::Paragraph => renderParagraph(f, area, elem),
        crate::glue::DiscriminantElem::Layout => renderLayout(f, area, elem),
    }
}

fn renderLayout<B: tui::backend::Backend>(f: &mut tui::Frame<B>, area : tui::layout::Rect, layout : &crate::glue::Elem){
    let (mut elems, mut config) = unsafe { layout.as_Layout()};
    let layoutDirection = getLayoutDirection(config.direction);
    let mut constraints = getConstraints(&config.constraints);

    // check we have enough constriants otherwise add some default to stop tui from crashing
    while constraints.len() < elems.len() {
        constraints.push(tui::layout::Constraint::Ratio(1,1));
    }

    let chunks = tui::layout::Layout::default()
        .direction(layoutDirection)
        .horizontal_margin(config.hMargin)
        .vertical_margin(config.vMargin)
        .constraints(constraints)
        .split(area);

    let mut chunkIndex = 0;
    for elem in elems {
        renderWidget(f, chunks[chunkIndex], elem);
        chunkIndex += 1;
    }
}

fn renderParagraph<B: tui::backend::Backend>(f: &mut tui::Frame<B>, area : tui::layout::Rect, paragraph : &crate::glue::Elem){
    
    let (listSpans, config) = unsafe {paragraph.as_Paragraph()};

    // Build pargraph up from nested Span(s)
    let mut text = Vec::with_capacity(listSpans.len());
    for aSpans in listSpans {
        let mut spansElements = Vec::with_capacity(aSpans.len());
        for span in aSpans {
            let s = tui::text::Span::styled(span.text.as_str(),getStyle(&span.style));
            spansElements.push(s);  
        }
        text.push(tui::text::Spans::from(spansElements)); 
    }

    // Get pargraph properties from config etc
    let title = config.title.as_str();
    let titleAlignment = getAlignment(config.titleAlignment);
    let textAlignment = getAlignment(config.textAlignment);
    let borderType = getBorderType(config.borderType);
    let borders = getBorders(&config.borders);

    // Block window for the paragraph text to live in
    let block = tui::widgets::Block::default()
    .title(title)
    .title_alignment(titleAlignment)
    .borders(borders)
    .border_type(borderType);

    // Create the paragraph
    let p = tui::widgets::Paragraph::new(text)
    .block(block)
    .style(getStyle(&config.style))
    .alignment(textAlignment)
    .wrap(tui::widgets::Wrap { trim: true });

    // Render to the frame
    f.render_widget(p,area);
}

fn getStyle(rocStyle : &crate::glue::Styles) -> tui::style::Style {
    let mut style = tui::style::Style::default();

    if rocStyle.bg.discriminant() != crate::glue::DiscriminantColor::None {
        style = style.bg(getColor(rocStyle.bg));
    }

    if rocStyle.fg.discriminant() != crate::glue::DiscriminantColor::None {
        style = style.fg(getColor(rocStyle.fg));
    }

    let mut modifiers = tui::style::Modifier::empty();
    for modifier in &rocStyle.modifiers {
        match modifier {
            crate::glue::TextModifier::BOLD => {modifiers.insert(tui::style::Modifier::BOLD);},
            crate::glue::TextModifier::CROSSEDOUT => {modifiers.insert(tui::style::Modifier::CROSSED_OUT);},
            crate::glue::TextModifier::DIM => {modifiers.insert(tui::style::Modifier::DIM);},
            crate::glue::TextModifier::HIDDEN => {modifiers.insert(tui::style::Modifier::HIDDEN);},
            crate::glue::TextModifier::ITALIC => {modifiers.insert(tui::style::Modifier::ITALIC);},
            crate::glue::TextModifier::RAPIDBLINK => {modifiers.insert(tui::style::Modifier::RAPID_BLINK);},
            crate::glue::TextModifier::REVERSED => {modifiers.insert(tui::style::Modifier::REVERSED);},
            crate::glue::TextModifier::SLOWBLINK => {modifiers.insert(tui::style::Modifier::SLOW_BLINK);},
            crate::glue::TextModifier::UNDERLINED => {modifiers.insert(tui::style::Modifier::UNDERLINED);},
        }
    }
    style = style.add_modifier(modifiers);

    style
}

fn getColor(color : crate::glue::Color) -> tui::style::Color {
    match color.discriminant() {
        crate::glue::DiscriminantColor::None => tui::style::Color::Reset,
        crate::glue::DiscriminantColor::Black => tui::style::Color::Black,
        crate::glue::DiscriminantColor::Red => tui::style::Color::Red,
        crate::glue::DiscriminantColor::Green => tui::style::Color::Green,
        crate::glue::DiscriminantColor::Yellow => tui::style::Color::Yellow,
        crate::glue::DiscriminantColor::Blue => tui::style::Color::Blue,
        crate::glue::DiscriminantColor::Magenta => tui::style::Color::Magenta,
        crate::glue::DiscriminantColor::Cyan => tui::style::Color::Cyan,
        crate::glue::DiscriminantColor::Gray => tui::style::Color::Gray,
        crate::glue::DiscriminantColor::DarkGray => tui::style::Color::DarkGray,
        crate::glue::DiscriminantColor::LightRed => tui::style::Color::LightRed,
        crate::glue::DiscriminantColor::LightGreen => tui::style::Color::LightGreen,
        crate::glue::DiscriminantColor::LightYellow => tui::style::Color::LightYellow,
        crate::glue::DiscriminantColor::LightBlue => tui::style::Color::LightBlue,
        crate::glue::DiscriminantColor::LightMagenta => tui::style::Color::LightMagenta,
        crate::glue::DiscriminantColor::LightCyan => tui::style::Color::LightCyan,
        crate::glue::DiscriminantColor::White => tui::style::Color::White,
    }
}

fn getAlignment(rocAlignment : crate::glue::Alignment) -> tui::layout::Alignment {
    match rocAlignment {
        crate::glue::Alignment::Left => tui::layout::Alignment::Left,
        crate::glue::Alignment::Center => tui::layout::Alignment::Center,
        crate::glue::Alignment::Right => tui::layout::Alignment::Right,
    }
}

fn getBorderType(rocBorderType : crate::glue::BorderType) -> tui::widgets::BorderType {
    match rocBorderType {
        crate::glue::BorderType::Plain => tui::widgets::BorderType::Plain,
        crate::glue::BorderType::Rounded => tui::widgets::BorderType::Rounded,
        crate::glue::BorderType::Double => tui::widgets::BorderType::Double,
        crate::glue::BorderType::Thick => tui::widgets::BorderType::Thick,
    }
}

fn getBorders(rocBorders : &roc_std::RocList<crate::glue::BorderModifier>) -> tui::widgets::Borders {
    let mut borders = tui::widgets::Borders::empty();
    for border in rocBorders {
        match border {
            crate::glue::BorderModifier::ALL => borders.insert(tui::widgets::Borders::ALL),
            crate::glue::BorderModifier::BOTTOM => borders.insert(tui::widgets::Borders::BOTTOM),
            crate::glue::BorderModifier::LEFT => borders.insert(tui::widgets::Borders::LEFT),
            crate::glue::BorderModifier::NONE => borders.insert(tui::widgets::Borders::NONE),
            crate::glue::BorderModifier::RIGHT => borders.insert(tui::widgets::Borders::RIGHT),
            crate::glue::BorderModifier::TOP => borders.insert(tui::widgets::Borders::TOP),
        }
    }
    borders
}

fn getConstraints(rocConstraints : &roc_std::RocList<crate::glue::Constraint>) -> Vec<tui::layout::Constraint> {
    let mut constraints: Vec<tui::layout::Constraint> = Vec::with_capacity(rocConstraints.len());
    for constraint in rocConstraints {
        match constraint.discriminant() {
            crate::glue::DiscriminantConstraint::Length => {
                let l = unsafe {constraint.as_Length()};
                constraints.push(tui::layout::Constraint::Length(*l));
            },
            crate::glue::DiscriminantConstraint::Max => {
                let l = unsafe {constraint.as_Max()};
                constraints.push(tui::layout::Constraint::Max(*l));
            },
            crate::glue::DiscriminantConstraint::Min => {
                let l = unsafe {constraint.as_Min()};
                constraints.push(tui::layout::Constraint::Min(*l));
            },
            crate::glue::DiscriminantConstraint::Percentage => {
                let l = unsafe {constraint.as_Percentage()};
                constraints.push(tui::layout::Constraint::Percentage(*l));
            },
            crate::glue::DiscriminantConstraint::Ratio => {
                let (r1, r2) = unsafe { constraint.as_Ratio() };
                constraints.push(tui::layout::Constraint::Ratio(*r1,*r2));
            },
        }
    } 
    constraints
}

fn getLayoutDirection(direction : crate::glue::LayoutDirection) -> tui::layout::Direction {
    match direction {
        crate::glue::LayoutDirection::Horizontal => tui::layout::Direction::Horizontal,
        crate::glue::LayoutDirection::Vertical => tui::layout::Direction::Vertical,    
    }
}