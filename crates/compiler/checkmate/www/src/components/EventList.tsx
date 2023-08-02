import clsx from "clsx";
import React, { useCallback, useEffect, useMemo, useState } from "react";
import { TypedEmitter } from "tiny-typed-emitter";
import { EventEpoch } from "../engine/engine";
import { lastSubEvent } from "../engine/event_util";
import { UnificationMode, Event } from "../schema";
import { EventListMessage, GraphMessage } from "../utils/events";
import { Refine } from "../utils/refine";
import EpochCell from "./Common/EpochCell";
import { CommonProps } from "./EventItem/types";
import { VariableEl } from "./EventItem/Variable";

interface EventListProps extends CommonProps {
  events: Event[];
  eventListEe: TypedEmitter<EventListMessage>;
  root?: boolean;
}

const MT = "my-2.5";
const LOWER_OPACITY = "opacity-40";

export default function EventList(props: EventListProps): JSX.Element {
  const { events, root } = props;
  return (
    <ul className={clsx(MT, "space-y-2.5", root ? "" : "ml-[1em]")}>
      {events.map((event, i) => (
        <li key={i}>
          <OneEvent {...props} event={event} />
        </li>
      ))}
    </ul>
  );
}

interface OneEventProps extends CommonProps {
  event: Event;
  eventListEe: TypedEmitter<EventListMessage>;
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
  eventListEe: TypedEmitter<EventListMessage>;
}

const COL_1_P = "pl-1.5";

const COL_1_OUTLINE_STYLES = clsx(COL_1_P, "outline-event-col-1");
const COL_3_OUTLINE_STYLES = clsx("outline-event-col-3");

const COL_1_ROUNDED = "rounded-l-md";
const COL_3_ROUNDED = "rounded-r-md";

const UN_EXPANDED_OUTLINE_STYLES = clsx(
  COL_1_P,
  "ring-inset ring-2 ring-blue-500"
);

const TRANSITION_SHADOW = "transition-shadow ease-in-out duration-500";
const TRANSITION_OPACITY = "transition-opacity ease-in-out duration-150";

function Unification(props: UnificationProps): JSX.Element {
  const { engine, event, currentEpoch, graphEe, eventListEe } = props;
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
  const isOutlined = useFocusOutlineEvent({
    ee: eventListEe,
    epoch: currentEpoch,
  });

  const modeIcon = useMemo(() => <UnificationModeIcon mode={mode} />, [mode]);

  const resultIcon = success ? UN_SUCCESS : UN_FAILURE;
  const resultHeadline = <Headline icon={resultIcon}></Headline>;

  const epochCell = useMemo(() => {
    if (!containsCurrentEpoch) return null;
    return <EventListEpochCell epoch={currentEpoch} graphEe={graphEe} />;
  }, [containsCurrentEpoch, currentEpoch, graphEe]);

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
      <div
        className={clsx(
          TRANSITION_OPACITY,
          !containsCurrentEpoch && LOWER_OPACITY
        )}
      >
        <div
          className={clsx(
            "rounded-md",
            TRANSITION_SHADOW,
            containsCurrentEpoch && isOutlined
              ? UN_EXPANDED_OUTLINE_STYLES
              : COL_1_P
          )}
        >
          {headLine}
        </div>
      </div>
    );
  } else {
    const beforeIsCurrentEpoch = beforeUnificationEpoch === currentEpoch;
    const afterIsCurrentEpoch = afterUnificationEpoch === currentEpoch;

    const epochCellBefore = beforeIsCurrentEpoch && epochCell;
    const epochCellAfter = afterIsCurrentEpoch && epochCell;

    const outlineEpochCellAfter = afterIsCurrentEpoch && isOutlined;
    const outlineEpochCellBefore = beforeIsCurrentEpoch && isOutlined;

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
      <div
        className={clsx(
          "grid gap-0 grid-cols-[min-content_min-content_auto] opacity-100",
          TRANSITION_OPACITY
        )}
      >
        {/* Row 1: unification start */}
        <div
          className={clsx(
            "row-start-1 col-start-1",
            TRANSITION_SHADOW,
            COL_1_ROUNDED,
            outlineEpochCellBefore ? COL_1_OUTLINE_STYLES : COL_1_P
          )}
        >
          {epochCellBefore}
        </div>
        <div
          className={clsx(
            "row-start-1 col-start-3",
            TRANSITION_SHADOW,
            COL_3_ROUNDED,
            outlineEpochCellBefore && COL_3_OUTLINE_STYLES
          )}
        >
          {headlineBefore}
        </div>

        {/* Row 2: inner traces */}
        <div className={clsx("row-start-2 col-start-1")}></div>
        <div className={clsx("row-start-2 col-start-3")}>
          <EventList
            {...props}
            root={false}
            engine={engine}
            events={subevents}
          />
        </div>

        {/* Row 3: inner traces */}
        <div
          className={clsx(
            "row-start-3 col-start-1",
            TRANSITION_SHADOW,
            COL_1_ROUNDED,
            outlineEpochCellAfter ? COL_1_OUTLINE_STYLES : COL_1_P
          )}
        >
          {epochCellAfter}
        </div>
        <div
          className={clsx(
            "row-start-3 col-start-3",
            TRANSITION_SHADOW,
            COL_3_ROUNDED,
            outlineEpochCellAfter && COL_3_OUTLINE_STYLES
          )}
        >
          {headlineAfter}
        </div>

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

interface EventListEpochCellProps {
  epoch: EventEpoch;
  graphEe: TypedEmitter<GraphMessage>;
}

function EventListEpochCell({
  epoch,
  graphEe,
}: EventListEpochCellProps): JSX.Element {
  return (
    <span
      id={`events-${epoch}`}
      className={clsx("cursor-pointer rounded-md")}
      onClick={(e) => {
        e.stopPropagation();
        graphEe.emit("focusEpoch", epoch);
      }}
    >
      <EpochCell>{epoch}</EpochCell>
    </span>
  );
}

function useFocusOutlineEvent({
  epoch,
  ee,
}: {
  epoch: EventEpoch;
  ee: TypedEmitter<EventListMessage>;
}) {
  const [isOutlined, setIsOutlined] = useState(false);

  useEffect(() => {
    ee.on("focusEpoch", (focusEpoch: EventEpoch) => {
      if (focusEpoch !== epoch) return;
      setIsOutlined(true);
    });
  }, [ee, epoch]);

  useEffect(() => {
    if (!isOutlined) return;
    const timer = setTimeout(() => {
      setIsOutlined(false);
    }, 500);

    return () => {
      clearTimeout(timer);
    };
  }, [isOutlined]);

  return isOutlined;
}
