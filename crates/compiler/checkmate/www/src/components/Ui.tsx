import React from "react";
import { AllEvents, Event, UnificationMode } from "../schema";
import { Refine } from "../utils/refine";
import clsx from "clsx";

interface UiProps {
  events: AllEvents;
}

export default function Ui({ events }: UiProps): JSX.Element {
  return (
    <div className="font-mono">
      <EventList root events={events}></EventList>
    </div>
  );
}

interface EventListProps {
  events: Event[];
  root?: boolean;
}

function EventList({ events, root }: EventListProps): JSX.Element {
  return (
    <ul className={clsx(root ? "ml-2 mt-4" : "ml-[1.5em]", "relative")}>
      {events.map((event, i) => (
        <li key={i} className="mt-2">
          <OneEvent event={event} />
        </li>
      ))}
    </ul>
  );
}

interface OneEventProps {
  event: Event;
}

function OneEvent({ event }: OneEventProps): JSX.Element {
  switch (event.type) {
    case "Unification":
      return <Unification event={event} />;
    case "VariableUnified":
      return <></>;
    case "VariableSetDescriptor":
      return <></>;
  }
}

const DROPDOWN_CLOSED = "▶";
const DROPDOWN_OPEN = "▼";

const UN_UNKNOWN = "❔";
const UN_SUCCESS = "✅";
const UN_FAILURE = "❌";

function Unification({
  event,
}: {
  event: Refine<Event, "Unification">;
}): JSX.Element {
  const { left, right, mode, subevents, success } = event;

  const [isOpen, setIsOpen] = React.useState(false);

  const result = success ? UN_SUCCESS : UN_FAILURE;
  const modeIcon = <UnificationModeIcon mode={mode} />;
  const dropdownIcon = isOpen ? DROPDOWN_OPEN : DROPDOWN_CLOSED;

  const headLineIcon = isOpen ? UN_UNKNOWN : result;

  const headLine = (
    <button onClick={() => setIsOpen(!isOpen)} className="w-full text-left">
      <span className="text-slate-400 mr-2">{dropdownIcon}</span>
      {headLineIcon} {left} {modeIcon} {right}
    </button>
  );

  if (!isOpen) {
    return <div className="opacity-60">{headLine}</div>;
  } else {
    const dropdownTransparent = (
      <span className="text-transparent mr-2">{dropdownIcon}</span>
    );
    return (
      <div>
        <div>{headLine}</div>
        <EventList events={subevents} />
        <div className="mt-2">
          {dropdownTransparent}
          {result} {left} {modeIcon} {right}
        </div>
      </div>
    );
  }
}

function UnificationModeIcon({ mode }: { mode: UnificationMode }): JSX.Element {
  switch (mode.type) {
    case "Eq":
      return <>~</>;
    case "Present":
      return <>+=</>;
    case "LambdaSetSpecialization":
      return <>|~|</>;
  }
}
