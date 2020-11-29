#![warn(clippy::all, clippy::dbg_macro)]
// I'm skeptical that clippy:large_enum_variant is a good lint to have globally enabled.
//
// It warns about a performance problem where the only quick remediation is
// to allocate more on the heap, which has lots of tradeoffs - including making it
// long-term unclear which allocations *need* to happen for compilation's sake
// (e.g. recursive structures) versus those which were only added to appease clippy.
//
// Effectively optimizing data structure memory layout isn't a quick fix,
// and encouraging shortcuts here creates bad incentives. I would rather temporarily
// re-enable this when working on performance optimizations than have it block PRs.
#![allow(clippy::large_enum_variant)]

use iced::executor;
use iced::{
    button, keyboard, pane_grid, scrollable, Align, Application, Button, Column, Command,
    Container, Element, HorizontalAlignment, Length, PaneGrid, Scrollable, Settings, Subscription,
    Text,
};
use iced_native::{event, subscription, Event};
use std::path::Path;

pub mod ast;
pub mod text_state;

/// The editor is actually launched from the CLI if you pass it zero arguments,
/// or if you provide it 1 or more files or directories to open on launch.
pub fn launch(_filepaths: &[&Path]) -> iced::Result {
    Editor::run(Settings::default())
}

struct Editor<'a> {
    panes: pane_grid::State<Buffer<'a>>,
    panes_created: usize,
    focus: Option<pane_grid::Pane>,
}

#[derive(Debug, Clone, Copy)]
enum Message {
    Split(pane_grid::Axis, pane_grid::Pane),
    SplitFocused(pane_grid::Axis),
    FocusAdjacent(pane_grid::Direction),
    Clicked(pane_grid::Pane),
    Dragged(pane_grid::DragEvent),
    Resized(pane_grid::ResizeEvent),
    Close(pane_grid::Pane),
    CloseFocused,
}

impl<'a> Application for Editor<'a> {
    type Message = Message;
    type Executor = executor::Default;
    type Flags = ();

    fn new(_flags: ()) -> (Self, Command<Message>) {
        let initial_buffer = Buffer::new_empty(0);
        let (panes, _) = pane_grid::State::new(initial_buffer);

        let editor = Editor {
            panes,
            panes_created: 1,
            focus: None,
        };

        (editor, Command::none())
    }

    fn title(&self) -> String {
        String::from("The rockin’ roc editor")
    }

    fn update(&mut self, message: Message) -> Command<Message> {
        match message {
            Message::Split(axis, pane) => {
                let new_buffer = Buffer::new_empty(self.panes_created);

                let result = self.panes.split(axis, &pane, new_buffer);

                if let Some((pane, _)) = result {
                    self.focus = Some(pane);
                }

                self.panes_created += 1;
            }
            Message::SplitFocused(axis) => {
                if let Some(pane) = self.focus {
                    let new_buffer = Buffer::new_empty(self.panes_created);

                    let result = self.panes.split(axis, &pane, new_buffer);

                    if let Some((pane, _)) = result {
                        self.focus = Some(pane);
                    }

                    self.panes_created += 1;
                }
            }
            Message::FocusAdjacent(direction) => {
                if let Some(pane) = self.focus {
                    if let Some(adjacent) = self.panes.adjacent(&pane, direction) {
                        self.focus = Some(adjacent);
                    }
                }
            }
            Message::Clicked(pane) => {
                self.focus = Some(pane);
            }
            Message::Resized(pane_grid::ResizeEvent { split, ratio }) => {
                self.panes.resize(&split, ratio);
            }
            Message::Dragged(pane_grid::DragEvent::Dropped { pane, target }) => {
                self.panes.swap(&pane, &target);
            }
            Message::Dragged(_) => {}
            Message::Close(pane) => {
                if let Some((_, sibling)) = self.panes.close(&pane) {
                    self.focus = Some(sibling);
                }
            }
            Message::CloseFocused => {
                if let Some(pane) = self.focus {
                    if let Some((_, sibling)) = self.panes.close(&pane) {
                        self.focus = Some(sibling);
                    }
                }
            }
        }

        Command::none()
    }

    fn subscription(&self) -> Subscription<Message> {
        subscription::events_with(|event, status| {
            if let event::Status::Captured = status {
                return None;
            }

            match event {
                Event::Keyboard(keyboard::Event::KeyPressed {
                    modifiers,
                    key_code,
                }) if modifiers.is_command_pressed() => handle_hotkey(key_code),
                _ => None,
            }
        })
    }

    fn view(&mut self) -> Element<Message> {
        let focus = self.focus;
        let total_panes = self.panes.len();

        let pane_grid = PaneGrid::new(&mut self.panes, |pane, buffer| {
            let is_focused = focus == Some(pane);

            let title_bar = pane_grid::TitleBar::new(format!("Pane {}", buffer.id))
                .padding(10)
                .style(style::TitleBar { is_focused });

            pane_grid::Content::new(buffer.view(pane, total_panes))
                .title_bar(title_bar)
                .style(style::Pane { is_focused })
        })
        .width(Length::Fill)
        .height(Length::Fill)
        .spacing(10)
        .on_click(Message::Clicked)
        .on_drag(Message::Dragged)
        .on_resize(10, Message::Resized);

        Container::new(pane_grid)
            .width(Length::Fill)
            .height(Length::Fill)
            .padding(10)
            .into()
    }
}

struct Buffer<'a> {
    id: usize,
    is_file: bool,
    filepath: Option<&'a Path>,
    scroll: scrollable::State,
    split_horizontally: button::State,
    split_vertically: button::State,
    close: button::State,
}

impl<'a> Buffer<'a> {
    fn new(id: usize, is_file: bool, filepath: Option<&'a Path>) -> Self {
        Buffer {
            id,
            is_file,
            filepath,
            scroll: scrollable::State::new(),
            split_horizontally: button::State::new(),
            split_vertically: button::State::new(),
            close: button::State::new(),
        }
    }

    fn new_empty(id: usize) -> Self {
        Buffer {
            id,
            is_file: false,
            filepath: None,
            scroll: scrollable::State::new(),
            split_horizontally: button::State::new(),
            split_vertically: button::State::new(),
            close: button::State::new(),
        }
    }

    fn view(&mut self, pane: pane_grid::Pane, total_panes: usize) -> Element<Message> {
        let Buffer {
            scroll,
            split_horizontally,
            split_vertically,
            close,
            ..
        } = self;

        let button = |state, label, message, style| {
            Button::new(
                state,
                Text::new(label)
                    .width(Length::Fill)
                    .horizontal_alignment(HorizontalAlignment::Center)
                    .size(16),
            )
            .width(Length::Fill)
            .padding(8)
            .on_press(message)
            .style(style)
        };

        let mut controls = Column::new()
            .spacing(5)
            .max_width(150)
            .push(button(
                split_horizontally,
                "Split horizontally",
                Message::Split(pane_grid::Axis::Horizontal, pane),
                style::Button::Primary,
            ))
            .push(button(
                split_vertically,
                "Split vertically",
                Message::Split(pane_grid::Axis::Vertical, pane),
                style::Button::Primary,
            ));

        if total_panes > 1 {
            controls = controls.push(button(
                close,
                "Close",
                Message::Close(pane),
                style::Button::Destructive,
            ));
        }

        let content = Scrollable::new(scroll)
            .width(Length::Fill)
            .spacing(10)
            .align_items(Align::Center)
            .push(controls);

        Container::new(content)
            .width(Length::Fill)
            .height(Length::Fill)
            .padding(5)
            .center_y()
            .into()
    }
}

fn handle_hotkey(key_code: keyboard::KeyCode) -> Option<Message> {
    use keyboard::KeyCode;
    use pane_grid::{Axis, Direction};

    let direction = match key_code {
        KeyCode::Up => Some(Direction::Up),
        KeyCode::Down => Some(Direction::Down),
        KeyCode::Left => Some(Direction::Left),
        KeyCode::Right => Some(Direction::Right),
        _ => None,
    };

    match key_code {
        KeyCode::V => Some(Message::SplitFocused(Axis::Vertical)),
        KeyCode::H => Some(Message::SplitFocused(Axis::Horizontal)),
        KeyCode::W => Some(Message::CloseFocused),
        _ => direction.map(Message::FocusAdjacent),
    }
}

mod style {
    use iced::{button, container, Background, Color, Vector};

    const SURFACE: Color = Color::from_rgb(
        0xF2 as f32 / 255.0,
        0xF3 as f32 / 255.0,
        0xF5 as f32 / 255.0,
    );

    const ACTIVE: Color = Color::from_rgb(
        0x72 as f32 / 255.0,
        0x89 as f32 / 255.0,
        0xDA as f32 / 255.0,
    );

    const HOVERED: Color = Color::from_rgb(
        0x67 as f32 / 255.0,
        0x7B as f32 / 255.0,
        0xC4 as f32 / 255.0,
    );

    pub struct TitleBar {
        pub is_focused: bool,
    }

    impl container::StyleSheet for TitleBar {
        fn style(&self) -> container::Style {
            let pane = Pane {
                is_focused: self.is_focused,
            }
            .style();

            container::Style {
                text_color: Some(Color::WHITE),
                background: Some(pane.border_color.into()),
                ..Default::default()
            }
        }
    }

    pub struct Pane {
        pub is_focused: bool,
    }

    impl container::StyleSheet for Pane {
        fn style(&self) -> container::Style {
            container::Style {
                background: Some(Background::Color(SURFACE)),
                border_width: 2.0,
                border_color: if self.is_focused {
                    Color::BLACK
                } else {
                    Color::from_rgb(0.7, 0.7, 0.7)
                },
                ..Default::default()
            }
        }
    }

    pub enum Button {
        Primary,
        Destructive,
    }

    impl button::StyleSheet for Button {
        fn active(&self) -> button::Style {
            let (background, text_color) = match self {
                Button::Primary => (Some(ACTIVE), Color::WHITE),
                Button::Destructive => (None, Color::from_rgb8(0xFF, 0x47, 0x47)),
            };

            button::Style {
                text_color,
                background: background.map(Background::Color),
                border_radius: 5.0,
                shadow_offset: Vector::new(0.0, 0.0),
                ..button::Style::default()
            }
        }

        fn hovered(&self) -> button::Style {
            let active = self.active();

            let background = match self {
                Button::Primary => Some(HOVERED),
                Button::Destructive => Some(Color {
                    a: 0.2,
                    ..active.text_color
                }),
            };

            button::Style {
                background: background.map(Background::Color),
                ..active
            }
        }
    }
}

// In Linux, we get a '\u{8}' when you press backspace,
// but in macOS we get '\u{7f}'.
