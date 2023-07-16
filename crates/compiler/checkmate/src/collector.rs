use roc_checkmate_schema::{Event, VariableEvent};
use roc_types::subs as s;

use crate::convert::AsSchema;

pub struct Collector {
    events: Event,
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
            events: Event::Top(Vec::new()),
            current_event_path: Vec::new(),
        }
    }

    pub fn unify(&mut self, subs: &s::Subs, from: s::Variable, to: s::Variable) {
        let to = to.as_schema(subs);
        let from = from.as_schema(subs);
        self.add_event(VariableEvent::Unify { to, from });
    }

    pub fn set_content(&mut self, subs: &s::Subs, var: s::Variable, content: s::Content) {
        let variable = var.as_schema(subs);
        let content = content.as_schema(subs);
        self.add_event(VariableEvent::SetDescriptor {
            variable,
            content: Some(content),
            rank: None,
        });
    }

    pub fn set_rank(&mut self, subs: &s::Subs, var: s::Variable, rank: s::Rank) {
        let variable = var.as_schema(subs);
        let rank = rank.as_schema(subs);
        self.add_event(VariableEvent::SetDescriptor {
            variable,
            rank: Some(rank),
            content: None,
        });
    }

    pub fn set_descriptor(&mut self, subs: &s::Subs, var: s::Variable, descriptor: s::Descriptor) {
        let variable = var.as_schema(subs);
        let rank = descriptor.rank.as_schema(subs);
        let content = descriptor.content.as_schema(subs);
        self.add_event(VariableEvent::SetDescriptor {
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
            Event::Unification {
                left: l,
                right: r,
                success: s,
                ..
            } => {
                assert_eq!(left.as_schema(subs), *l);
                assert_eq!(right.as_schema(subs), *r);
                assert!(s.is_none());
                *s = Some(success);
            }
            _ => panic!("end_unification called when not in a unification"),
        }
        assert!(matches!(current_event, Event::Unification { .. }));
        self.current_event_path.pop();
    }

    fn add_event(&mut self, event: impl Into<Event>) {
        let event = event.into();
        let is_appendable = event.appendable();
        let path_event = self.get_path_event();
        let new_event_index = path_event.append(event);
        if is_appendable {
            self.current_event_path.push(new_event_index);
        }
    }

    fn get_path_event(&mut self) -> &mut Event {
        let mut event = &mut self.events;
        for i in &self.current_event_path {
            event = event.index(*i);
        }
        event
    }
}
