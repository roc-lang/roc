use std::error::Error;

use roc_checkmate_schema::{AllEvents, Event};
use roc_types::subs as s;

use crate::convert::AsSchema;

#[derive(Debug)]
pub struct Collector {
    events: AllEvents,
    current_event_path: Vec<usize>,
}

impl Default for Collector {
    fn default() -> Self {
        Self::new()
    }
}

impl Collector {
    pub fn new() -> Self {
        Self {
            events: AllEvents(Vec::new()),
            current_event_path: Vec::new(),
        }
    }

    pub fn unify(&mut self, subs: &s::Subs, from: s::Variable, to: s::Variable) {
        let to = to.as_schema(subs);
        let from = from.as_schema(subs);
        self.add_event(Event::VariableUnified { to, from });
    }

    pub fn set_content(&mut self, subs: &s::Subs, var: s::Variable, content: s::Content) {
        let variable = var.as_schema(subs);
        let content = content.as_schema(subs);
        self.add_event(Event::VariableSetDescriptor {
            variable,
            content: Some(content),
            rank: None,
        });
    }

    pub fn set_rank(&mut self, subs: &s::Subs, var: s::Variable, rank: s::Rank) {
        let variable = var.as_schema(subs);
        let rank = rank.as_schema(subs);
        self.add_event(Event::VariableSetDescriptor {
            variable,
            rank: Some(rank),
            content: None,
        });
    }

    pub fn set_descriptor(&mut self, subs: &s::Subs, var: s::Variable, descriptor: s::Descriptor) {
        let variable = var.as_schema(subs);
        let rank = descriptor.rank.as_schema(subs);
        let content = descriptor.content.as_schema(subs);
        self.add_event(Event::VariableSetDescriptor {
            variable,
            rank: Some(rank),
            content: Some(content),
        });
    }

    pub fn start_unification(
        &mut self,
        subs: &s::Subs,
        left: s::Variable,
        right: s::Variable,
        mode: roc_solve_schema::UnificationMode,
    ) {
        let left = left.as_schema(subs);
        let right = right.as_schema(subs);
        let mode = mode.as_schema(subs);
        let subevents = Vec::new();
        self.add_event(Event::Unification {
            left,
            right,
            mode,
            subevents,
            success: None,
        });
    }

    pub fn end_unification(
        &mut self,
        subs: &s::Subs,
        left: s::Variable,
        right: s::Variable,
        success: bool,
    ) {
        let current_event = self.get_path_event();
        match current_event {
            EventW::Sub(Event::Unification {
                left: l,
                right: r,
                success: s,
                ..
            }) => {
                assert_eq!(left.as_schema(subs), *l);
                assert_eq!(right.as_schema(subs), *r);
                assert!(s.is_none());
                *s = Some(success);
            }
            _ => panic!("end_unification called when not in a unification"),
        }
        self.current_event_path.pop();
    }

    pub fn write(&self, writer: impl std::io::Write) -> Result<(), Box<dyn Error>> {
        self.events.write(writer)?;
        Ok(())
    }

    fn add_event(&mut self, event: impl Into<Event>) {
        let mut event = event.into();
        let is_appendable = EventW::Sub(&mut event).appendable();
        let event = event;

        let path_event = self.get_path_event();
        let new_event_index = path_event.append(event);
        if is_appendable {
            self.current_event_path.push(new_event_index);
        }
    }

    fn get_path_event(&mut self) -> EventW {
        let mut event = EventW::Top(&mut self.events);
        for i in &self.current_event_path {
            event = event.index(*i);
        }
        event
    }
}

enum EventW<'a> {
    Top(&'a mut AllEvents),
    Sub(&'a mut Event),
}

impl<'a> EventW<'a> {
    fn append(self, event: Event) -> usize {
        let list = self.subevents_mut().unwrap();
        let index = list.len();
        list.push(event);
        index
    }

    fn appendable(self) -> bool {
        self.subevents().is_some()
    }

    fn index(self, index: usize) -> EventW<'a> {
        Self::Sub(&mut self.subevents_mut().unwrap()[index])
    }
}

impl<'a> EventW<'a> {
    fn subevents(self) -> Option<&'a Vec<Event>> {
        use EventW::*;
        match self {
            Top(events) => Some(&events.0),
            Sub(Event::Unification { subevents, .. }) => Some(subevents),
            Sub(Event::VariableUnified { .. } | Event::VariableSetDescriptor { .. }) => None,
        }
    }
    fn subevents_mut(self) -> Option<&'a mut Vec<Event>> {
        use EventW::*;
        match self {
            Top(events) => Some(&mut events.0),
            Sub(Event::Unification { subevents, .. }) => Some(subevents),
            Sub(Event::VariableUnified { .. } | Event::VariableSetDescriptor { .. }) => None,
        }
    }
}
