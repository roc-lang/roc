import clsx from "clsx";
import React from "react";
import { EventIndex } from "../engine/engine";
import { lastSubEvent } from "../engine/event_util";
import { UnificationMode, Event } from "../schema";
import { Refine } from "../utils/refine";
import { CommonProps } from "./EventItem/types";
import { VariableEl } from "./EventItem/Variable";

interface EventListProps extends CommonProps {
  events: Event[];
  root?: boolean;
}

const MT = "mt-2.5";
const UNFOCUSED = "opacity-40";

export default function EventList(props: EventListProps): JSX.Element {
  const { events, root } = props;
  return (
    <ul className={clsx(MT, root ? "ml-2" : "ml-[1.5em]")}>
      {events.map((event, i) => (
        <li key={i} className={MT}>
          <OneEvent {...props} event={event} />
        </li>
      ))}
    </ul>
  );
}

interface OneEventProps extends CommonProps {
  event: Event;
}

function OneEvent(props: OneEventProps): JSX.Element {
  const { event } = props;
  switch (event.type) {
    case "Unification":
      return <Unification {...props} event={event} />;
    case "VariableUnified":
      return <></>;
    case "VariableSetDescriptor":
      return <></>;
  }
}

const DROPDOWN_CLOSED = "▶";
const DROPDOWN_OPEN = "▼";

const UN_UNKNOWN = "💭";
const UN_SUCCESS = "✅";
const UN_FAILURE = "❌";

interface UnificationProps extends CommonProps {
  event: Refine<Event, "Unification">;
}

function Unification(props: UnificationProps): JSX.Element {
  const { engine, event } = props;
  const { mode, subevents, success } = event;

  const beforeUnificationIndex = engine.getEventIndex(event);
  const afterUnificationIndex = engine.getEventIndex(lastSubEvent(event));

  const leftVar = (index: EventIndex) => (
    <VariableEl {...props} index={index} variable={event.left} />
  );
  const rightVar = (index: EventIndex) => (
    <VariableEl {...props} index={index} variable={event.right} />
  );

  const [isOpen, setIsOpen] = React.useState(false);

  const modeIcon = <UnificationModeIcon mode={mode} />;

  const resultIcon = success ? UN_SUCCESS : UN_FAILURE;
  const resultHeadline = <Headline icon={resultIcon}></Headline>;
  const topHeadline = (
    <Headline icon={isOpen ? UN_UNKNOWN : resultIcon}></Headline>
  );

  function getHeadline(index: EventIndex) {
    return (
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="w-full text-left whitespace-nowrap"
      >
        <span
          className={clsx("mr-2", isOpen ? "text-slate-500" : "text-slate-400")}
        >
          {isOpen ? DROPDOWN_OPEN : DROPDOWN_CLOSED}
        </span>
        {topHeadline} {leftVar(index)} {modeIcon} {rightVar(index)}
      </button>
    );
  }

  if (!isOpen) {
    const headLine = getHeadline(afterUnificationIndex);
    return <div className={UNFOCUSED}>{headLine}</div>;
  } else {
    const dropdownTransparent = (
      <span className="text-transparent mr-2">{DROPDOWN_OPEN}</span>
    );

    const headlineBefore = getHeadline(beforeUnificationIndex);

    const headlineAfter = (
      <div className={clsx(MT, "whitespace-nowrap")}>
        {dropdownTransparent}
        {resultHeadline} {leftVar(afterUnificationIndex)} {modeIcon}{" "}
        {rightVar(afterUnificationIndex)}
      </div>
    );

    return (
      <div
        className={clsx(
          "relative z-[1]",
          "before:content-[''] before:border-l before:border-slate-500 before:z-[-1]",
          "before:absolute before:w-0 before:h-[calc(100%-1.5rem)] before:top-[1rem] before:left-[0.3rem]"
        )}
      >
        <div>{headlineBefore}</div>
        <EventList {...props} root={false} engine={engine} events={subevents} />
        {headlineAfter}
      </div>
    );
  }
}

function Headline({ icon }: { icon: string }): JSX.Element {
  return <span className="">{icon}</span>;
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
