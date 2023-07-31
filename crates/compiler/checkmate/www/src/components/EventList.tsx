import clsx from "clsx";
import React, { useCallback, useMemo, useState } from "react";
import { EventEpoch } from "../engine/engine";
import { lastSubEvent } from "../engine/event_util";
import { UnificationMode, Event } from "../schema";
import { Refine } from "../utils/refine";
import EpochCell from "./Common/EpochCell";
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

function epochInRange(
  epoch: EventEpoch,
  [start, end]: [EventEpoch, EventEpoch]
): boolean {
  return epoch >= start && epoch <= end;
}

interface UnificationProps extends CommonProps {
  event: Refine<Event, "Unification">;
}

function Unification(props: UnificationProps): JSX.Element {
  const { engine, event, currentEpoch } = props;
  const { mode, subevents, success } = event;

  const beforeUnificationEpoch = engine.getEventIndex(event);
  const afterUnificationEpoch = engine.getEventIndex(lastSubEvent(event));

  const containsCurrentEpoch = epochInRange(currentEpoch, [
    beforeUnificationEpoch,
    afterUnificationEpoch,
  ]);

  const leftVar = useMemo(
    () => (epoch: EventEpoch) =>
      <VariableEl {...props} epoch={epoch} variable={event.left} />,
    [event.left, props]
  );
  const rightVar = useMemo(
    () => (epoch: EventEpoch) =>
      <VariableEl {...props} epoch={epoch} variable={event.right} />,
    [event.right, props]
  );

  const [isOpen, setIsOpen] = useState(false);

  const modeIcon = useMemo(() => <UnificationModeIcon mode={mode} />, [mode]);

  const resultIcon = success ? UN_SUCCESS : UN_FAILURE;
  const resultHeadline = <Headline icon={resultIcon}></Headline>;

  const epochCell = useMemo(() => {
    if (!containsCurrentEpoch) return null;
    return (
      <EpochCell
        noLeadingText
        epoch={currentEpoch}
        className="inline-block align-middle mr-2"
      ></EpochCell>
    );
  }, [containsCurrentEpoch, currentEpoch]);

  const getHeadline = useCallback(
    ({
      epoch,
      includeEpochIfInRange,
    }: {
      epoch: EventEpoch;
      includeEpochIfInRange: boolean;
    }) => {
      const topHeadline = (
        <Headline icon={isOpen ? UN_UNKNOWN : resultIcon}></Headline>
      );

      const optEpochCell =
        includeEpochIfInRange && containsCurrentEpoch && epochCell;

      return (
        <button
          onClick={() => setIsOpen(!isOpen)}
          className="w-full text-left whitespace-nowrap h-full"
        >
          {optEpochCell}
          <span
            className={clsx(
              "mr-2",
              isOpen ? "text-slate-500" : "text-slate-400"
            )}
          >
            {isOpen ? DROPDOWN_OPEN : DROPDOWN_CLOSED}
          </span>
          {topHeadline} {leftVar(epoch)} {modeIcon} {rightVar(epoch)}
        </button>
      );
    },
    [
      isOpen,
      resultIcon,
      containsCurrentEpoch,
      epochCell,
      leftVar,
      modeIcon,
      rightVar,
    ]
  );

  if (!isOpen) {
    const headLine = getHeadline({
      epoch: afterUnificationEpoch,
      includeEpochIfInRange: true,
    });
    return (
      <div className={clsx(!containsCurrentEpoch && UNFOCUSED)}>{headLine}</div>
    );
  } else {
    const optEpochCellAfter =
      afterUnificationEpoch === currentEpoch && epochCell;
    const optEpochCellBefore =
      beforeUnificationEpoch === currentEpoch && epochCell;

    const headlineBefore = getHeadline({
      epoch: beforeUnificationEpoch,
      includeEpochIfInRange: false,
    });

    const dropdownTransparent = (
      <span className="text-transparent mr-2">{DROPDOWN_OPEN}</span>
    );

    const headlineAfter = (
      <div className={clsx("whitespace-nowrap")}>
        {dropdownTransparent}
        {resultHeadline} {leftVar(afterUnificationEpoch)} {modeIcon}{" "}
        {rightVar(afterUnificationEpoch)}
      </div>
    );

    return (
      <div className="grid gap-0 grid-cols-[min-content_min-content_auto]">
        {/* Row 1: unification start */}
        <div className="row-start-1 col-start-1">{optEpochCellBefore}</div>
        <div className="row-start-1 col-start-3">{headlineBefore}</div>

        {/* Row 2: inner traces */}
        <div className="row-start-2 col-start-1"></div>
        <div className="row-start-2 col-start-3">
          <EventList
            {...props}
            root={false}
            engine={engine}
            events={subevents}
          />
        </div>

        {/* Row 3: inner traces */}
        <div className="row-start-3 col-start-1">{optEpochCellAfter}</div>
        <div className="row-start-3 col-start-3">{headlineAfter}</div>

        {/* Col 2: dropdown line */}
        <div
          className={clsx(
            "row-start-1 row-end-4 col-start-2 h-full",
            "relative z-[1] h-full",
            "before:content-[''] before:border-l before:border-slate-500 before:z-[-1]",
            "before:absolute before:w-0 before:h-[calc(100%-1.5rem)] before:top-[1rem] before:left-[0.3rem]"
          )}
        ></div>
      </div>
    );
  }
}

function Headline({ icon }: { icon: string }): JSX.Element {
  return (
    <div className="inline-block align-middle">
      <div className="">{icon}</div>
    </div>
  );
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
