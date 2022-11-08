use crate::{
    glue::{
        Bounds, 
            // RocElem, RocElemTag, RocEvent
    },
};
use std::{
    io, 
    thread, 
    time::Duration, 
    sync::mpsc::{RecvError,Receiver,Sender}
};
use tui::{
    backend::CrosstermBackend,
    widgets::{
        // Widget, 
        Block, 
        Borders, 
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
    layout::{Layout, Constraint, Direction, Alignment},
    Terminal
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

        let blockText = unsafe {(*model).text.as_str()};

        terminal.draw(|f| {
            let size = f.size();

            let chunks = Layout::default()
                .direction(Direction::Horizontal)
                .margin(1)
                .constraints(
                    [
                        Constraint::Percentage(10),
                        Constraint::Percentage(80),
                        Constraint::Percentage(10)
                    ].as_ref()
                )
                .split(size);
            
            let text = vec![
                    Spans::from(vec![
                        Span::raw("First"),
                        Span::styled("line",Style::default().add_modifier(Modifier::ITALIC)),
                        Span::raw("asaf"),
                    ]),
                    Spans::from(Span::styled("Second line", Style::default().fg(Color::Red))),
                ];
            let paragrph = Paragraph::new(text)
                    .block(Block::default().title("Paragraph").borders(Borders::ALL))
                    .style(Style::default().fg(Color::White).bg(Color::Gray))
                    .alignment(Alignment::Left)
                    .wrap(Wrap { trim: true });

            let block = Block::default()
                .title(blockText)
                .style(Style::default().fg(Color::LightBlue))
                .borders(Borders::ALL)
                ;
                
            let items = [ListItem::new("Item 1"), ListItem::new("Item 2"), ListItem::new("Item 3")];
            let list = List::new(items)
                .block(Block::default().title("List").borders(Borders::ALL))
                .style(Style::default().fg(Color::Magenta))
                .highlight_style(Style::default().add_modifier(Modifier::ITALIC))
                .highlight_symbol(">>");
            
            f.render_widget(block, chunks[0]);
            f.render_widget(list, chunks[2]);
            f.render_widget(paragrph, chunks[1]);


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