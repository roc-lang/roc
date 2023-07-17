import { Event, Variable } from "../schema";
import { assertExhaustive } from "../utils/exhaustive";
import {
  ChangeEvent,
  makeDeleteVariable,
  makeRevertVariable,
  RollbackChange,
  Subs,
} from "./subs";

export type EventIndex = number & { __eventIndex: never };

function* flattenEvents(events: Event[]): Generator<Event> {
  for (const event of events) {
    yield event;
    switch (event.type) {
      case "Unification": {
        yield* flattenEvents(event.subevents);
        break;
      }
      case "VariableUnified":
      case "VariableSetDescriptor":
        break;
      default:
        assertExhaustive(event);
    }
  }
}

function getFlatEvents(events: Event[]): {
  flatEvents: Event[];
  map: Map<Event, EventIndex>;
} {
  const map = new Map<Event, EventIndex>();
  const flatEvents = Array.from(flattenEvents(events));
  let i = 0;
  for (const event of flatEvents) {
    map.set(event, i as EventIndex);
    i++;
  }
  return { flatEvents, map };
}

export class Engine {
  #eventIndexMap: Map<Event, EventIndex>;
  #events: Event[];
  #subs: Subs = Subs.new();
  #reverseEvents: Map<EventIndex, RollbackChange> = new Map();

  #nextIndexForward: EventIndex = 0 as EventIndex;

  constructor(events: Event[]) {
    const { flatEvents, map } = getFlatEvents(events);
    this.#eventIndexMap = map;
    this.#events = flatEvents;
  }

  getEventIndex(event: Event): EventIndex {
    const index = this.#eventIndexMap.get(event);
    if (index === undefined) {
      throw new Error("Event not found");
    }
    return index;
  }

  get step(): EventIndex {
    return this.#nextIndexForward;
  }

  stepTo(eventIndex: EventIndex): void {
    while (this.#nextIndexForward <= eventIndex) {
      this.stepForward(this.#nextIndexForward);
      ++this.#nextIndexForward;
    }
    while (this.#nextIndexForward > eventIndex) {
      --this.#nextIndexForward;
      this.stepBackward(this.#nextIndexForward);
    }
  }

  get subs(): Readonly<Subs> {
    return this.#subs;
  }

  lastEventIndex(): EventIndex {
    return (this.#events.length - 1) as EventIndex;
  }

  private stepForward(eventIndex: EventIndex): void {
    const event = this.#events[eventIndex];
    if (!isApplicable(event)) {
      return;
    }

    if (!this.#reverseEvents.has(eventIndex)) {
      const variable = applicableVariable(event);
      const current = this.#subs.get(variable);
      let revert: RollbackChange;
      if (!current) {
        revert = makeDeleteVariable({ variable });
      } else {
        revert = makeRevertVariable({ variable, to: current });
      }
      this.#reverseEvents.set(eventIndex, revert);
    }

    this.#subs.apply(event);
  }

  private stepBackward(eventIndex: EventIndex): void {
    const event = this.#events[eventIndex];
    if (!isApplicable(event)) {
      return;
    }

    const revert = this.#reverseEvents.get(eventIndex);
    if (!revert) {
      throw new Error("No revert found");
    }

    this.#subs.apply(revert);
  }
}

function isApplicable(event: Event): event is ChangeEvent {
  switch (event.type) {
    case "VariableUnified":
    case "VariableSetDescriptor":
      return true;
    case "Unification":
      return false;
    default:
      assertExhaustive(event);
  }
}

function applicableVariable(event: ChangeEvent): Variable {
  switch (event.type) {
    case "VariableUnified":
      return event.from;
    case "VariableSetDescriptor":
      return event.variable;
    default:
      assertExhaustive(event);
  }
}
