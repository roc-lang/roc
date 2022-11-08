use crate::{
    glue::{
        Bounds, 
        Elem,
    },
};
use roc_std::{
    RocList,
    // RocStr,
};
use std::{
    io, 
    thread, 
    time::Duration, 
    sync::mpsc::{RecvError,Receiver,Sender}
};
use tui::{
    backend::{Backend, CrosstermBackend},
    widgets::{
        Widget, 
        Block, 
        Borders, 
        BorderType,
        List,
        Paragraph,
        ListItem,
        Wrap,
    },
    style::{
        Style,
        Color,
        Modifier,
    },
    text::{
        Span,
        Spans,
    },
    layout::{Layout, Constraint, Direction, Alignment, Rect},
    Terminal,
    Frame,
};
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, KeyEvent
        // Event, KeyCode
    },
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};

pub fn run_event_loop(title: &str, window_bounds: Bounds) {
    
    use crate::roc;
    let (mut model, mut elems) = roc::init_and_render(window_bounds);

    // dbg!(&elems);

    // macro_rules! update_and_rerender {
    //     ($event:expr) => {
    //         // TODO use (model, elems) =  ... once we've upgraded rust versions
    //         let pair = roc::update_and_render(model, $event);

    //         model = pair.0;
    //         elems = pair.1;

    //         window.request_redraw();
    //     };
    // }

    enable_raw_mode().unwrap();
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture).unwrap();
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend).unwrap();

    let tick_rate = Duration::from_millis(200);
    let events = Events::new(tick_rate);
    
    loop {

        let mut appReturn = false;

        // let blockText = unsafe {(*model).text.as_str()};

        // let mut frame = terminal.get_frame();

        // frame.render_widget(
            
        // );
        
        // terminal.draw(|f| ui(f, &app))?;
        terminal.draw(|f| buildWidgets(f, &elems)).unwrap();

        // terminal.draw(|f| {
        //     let size = f.size();

        //     // let chunks = Layout::default()
        //     //     .direction(Direction::Horizontal)
        //     //     .margin(1)
        //     //     .constraints(
        //     //         [
        //     //             Constraint::Percentage(10),
        //     //             Constraint::Percentage(80),
        //     //             Constraint::Percentage(10)
        //     //         ].as_ref()
        //     //     )
        //     //     .split(size);

        //     // f.render_widget(block, chunks[0]);
        //     // f.render_widget(list, chunks[2]);
        //     // f.render_widget(paragrph, chunks[1]);
            
            
            
        //     // let block = buildWidgets(&elems);
                
        //     // let items = [ListItem::new("Item 1"), ListItem::new("Item 2"), ListItem::new("Item 3")];
        //     // let list = List::new(items)
        //     //     .block(Block::default().title("List").borders(Borders::ALL))
        //     //     .style(Style::default().fg(Color::Magenta))
        //     //     .highlight_style(Style::default().add_modifier(Modifier::ITALIC))
        //     //     .highlight_symbol(">>");
            
            


        // }).unwrap();



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
        
    // thread::sleep(Duration::from_millis(5000));

    // restore terminal
    disable_raw_mode().unwrap();
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    ).unwrap();
    terminal.show_cursor().unwrap();

}

pub enum InputEvent {
    Input(KeyEvent),
    Tick,
}

pub struct Events {
    rx: Receiver<InputEvent>,
    _tx: Sender<InputEvent>,
}

impl Events {
    pub fn new(tick_rate: Duration) -> Events {
        let (tx, rx) = std::sync::mpsc::channel();

        let event_tx = tx.clone(); // the thread::spawn own event_tx 
        thread::spawn(move || {
            loop {
                // poll for tick rate duration, if no event, sent tick event.
                if crossterm::event::poll(tick_rate).unwrap() {
                    if let event::Event::Key(key) = event::read().unwrap() {
                        let key = KeyEvent::from(key);
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
    pub fn next(&self) -> Result<InputEvent, RecvError> {
        self.rx.recv()
    }
}

fn buildWidgets<B: Backend>(f: &mut Frame<B>, elems : &RocList<Elem>){
    let size = f.size();

    for elem in elems {
        renderParagraph(f, size, elem);
    }
}

fn renderParagraph<B: Backend>(f: &mut Frame<B>, area : Rect , paragraph : &Elem){
    
    // For now there is only one Elem type will change later
    // roc_std::RocList<roc_std::RocList<Span>>, ParagraphConfig
    let (listSpans, config) = paragraph.as_Paragraph();

    // Build pargraph up from nested Span(s)
    let mut text = Vec::with_capacity(listSpans.len());
    for aSpans in listSpans {
        let mut spansElements = Vec::with_capacity(aSpans.len());
        for span in aSpans {
            let s = Span::styled(span.text.as_str(),getStyle(&span.style));
            spansElements.push(s);  
        }
        text.push(Spans::from(spansElements)); 
    }

    // Get pargraph properties from config etc
    let title = config.title.as_str();
    let titleAlignment = getAlignment(config.titleAlignment);
    let textAlignment = getAlignment(config.textAlignment);
    let borderType = getBorderType(config.borderType);
    let borders = getBorders(&config.borders);

    // Block window for the paragraph text to live in
    let block = Block::default()
    .title(title)
    .title_alignment(titleAlignment)
    .borders(borders)
    .border_type(borderType);

    // Create the paragraph
    let p = Paragraph::new(text)
    .block(block)
    .style(getStyle(&config.style))
    .alignment(textAlignment)
    .wrap(Wrap { trim: true });

    // Render to the frame
    f.render_widget(p,area);
}

fn getStyle(rocStyle : &crate::glue::Styles) -> Style {
    let mut style = Style::default();

    if rocStyle.bg.discriminant() != crate::glue::DiscriminantColor::None {
        style = style.bg(getColor(rocStyle.bg));
    }

    if rocStyle.fg.discriminant() != crate::glue::DiscriminantColor::None {
        style = style.fg(getColor(rocStyle.fg));
    }

    let mut modifiers = Modifier::empty();
    for modifier in &rocStyle.modifiers {
        match modifier {
            crate::glue::TextModifier::BOLD => {modifiers.insert(Modifier::BOLD);},
            crate::glue::TextModifier::CROSSEDOUT => {modifiers.insert(Modifier::CROSSED_OUT);},
            crate::glue::TextModifier::DIM => {modifiers.insert(Modifier::DIM);},
            crate::glue::TextModifier::HIDDEN => {modifiers.insert(Modifier::HIDDEN);},
            crate::glue::TextModifier::ITALIC => {modifiers.insert(Modifier::ITALIC);},
            crate::glue::TextModifier::RAPIDBLINK => {modifiers.insert(Modifier::RAPID_BLINK);},
            crate::glue::TextModifier::REVERSED => {modifiers.insert(Modifier::REVERSED);},
            crate::glue::TextModifier::SLOWBLINK => {modifiers.insert(Modifier::SLOW_BLINK);},
            crate::glue::TextModifier::UNDERLINED => {modifiers.insert(Modifier::UNDERLINED);},
        }
    }
    style = style.add_modifier(modifiers);

    style
}

fn getColor(color : crate::glue::Color) -> Color {
    match color.discriminant() {
        crate::glue::DiscriminantColor::None => Color::Reset,
        crate::glue::DiscriminantColor::Black => Color::Black,
        crate::glue::DiscriminantColor::Red => Color::Red,
        crate::glue::DiscriminantColor::Green => Color::Green,
        crate::glue::DiscriminantColor::Yellow => Color::Yellow,
        crate::glue::DiscriminantColor::Blue => Color::Blue,
        crate::glue::DiscriminantColor::Magenta => Color::Magenta,
        crate::glue::DiscriminantColor::Cyan => Color::Cyan,
        crate::glue::DiscriminantColor::Gray => Color::Gray,
        crate::glue::DiscriminantColor::DarkGray => Color::DarkGray,
        crate::glue::DiscriminantColor::LightRed => Color::LightRed,
        crate::glue::DiscriminantColor::LightGreen => Color::LightGreen,
        crate::glue::DiscriminantColor::LightYellow => Color::LightYellow,
        crate::glue::DiscriminantColor::LightBlue => Color::LightBlue,
        crate::glue::DiscriminantColor::LightMagenta => Color::LightMagenta,
        crate::glue::DiscriminantColor::LightCyan => Color::LightCyan,
        crate::glue::DiscriminantColor::White => Color::White,
    }
}

fn getAlignment(rocAlignment : crate::glue::Alignment) -> Alignment {
    match rocAlignment {
        crate::glue::Alignment::Left => Alignment::Left,
        crate::glue::Alignment::Center => Alignment::Center,
        crate::glue::Alignment::Right => Alignment::Right,
    }
}

fn getBorderType(rocBorderType : crate::glue::BorderType) -> BorderType {
    match rocBorderType {
        crate::glue::BorderType::Plain => BorderType::Plain,
        crate::glue::BorderType::Rounded => BorderType::Rounded,
        crate::glue::BorderType::Double => BorderType::Double,
        crate::glue::BorderType::Thick => BorderType::Thick,
    }
}

fn getBorders(rocBorders : &roc_std::RocList<crate::glue::BorderModifier>) -> Borders {
    let mut borders = Borders::empty();
    for border in rocBorders {
        match border {
            crate::glue::BorderModifier::ALL => borders.insert(Borders::ALL),
            crate::glue::BorderModifier::BOTTOM => borders.insert(Borders::BOTTOM),
            crate::glue::BorderModifier::LEFT => borders.insert(Borders::LEFT),
            crate::glue::BorderModifier::NONE => borders.insert(Borders::NONE),
            crate::glue::BorderModifier::RIGHT => borders.insert(Borders::RIGHT),
            crate::glue::BorderModifier::TOP => borders.insert(Borders::TOP),
        }
    }
    borders
}