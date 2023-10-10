import { Event } from "../schema";

export function lastSubEvent(event: Event): Event {
  switch (event.type) {
    case "Unification": {
      const subevents = event.subevents;
      if (subevents.length === 0) {
        return event;
      }
      return lastSubEvent(event.subevents[event.subevents.length - 1]);
    }
    case "VariableUnified": {
      return event;
    }
    case "VariableSetDescriptor": {
      return event;
    }
  }
}
