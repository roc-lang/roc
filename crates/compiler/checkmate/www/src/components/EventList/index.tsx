import clsx from "clsx";
import React, { useCallback, useMemo, useState } from "react";
import { TypedEmitter } from "tiny-typed-emitter";
import { EventEpoch } from "../../engine/engine";
import { lastSubEvent } from "../../engine/event_util";
import { useFocusOutlineEvent } from "../../hooks/useFocusOutlineEvent";
import { UnificationMode, Event } from "../../schema";
import {
  EventListMessage,
  GlobalMessage,
  GraphMessage,
  LoadEpochView,
} from "../../utils/events";
import { Refine } from "../../utils/refine";
import EpochCell from "../Common/EpochCell";
import { CommonProps } from "../EventItem/types";
import { VariableEl } from "../EventItem/Variable";
import { depthToGroupInfo } from "./depthGroup";

interface EventListProps extends CommonProps {
  events: Event[];
  eventListEe: TypedEmitter<EventListMessage>;
  globalEe: TypedEmitter<GlobalMessage>;
  depth: number;
}

const MT = "my-2.5";
const LOWER_OPACITY = "opacity-40";

export default function EventList(props: EventListProps): JSX.Element {
  const { events, depth } = props;
  return (
    <ul className={clsx(MT, "space-y-2.5", depth === 0 ? "" : "ml-[1em]")}>
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
  globalEe: TypedEmitter<GlobalMessage>;
  depth: number;
}

function OneEvent(props: OneEventProps): JSX.Element {
  const { event } = props;
  switch (event.type) {
    case "Unification":
      return <UnificationEvent {...props} event={event} />;
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
  globalEe: TypedEmitter<GlobalMessage>;
  depth: number;
}

const COL_1_P = "pl-1.5";

const COL_1_OUTLINE_STYLES = clsx(COL_1_P, "outline-event-col-1");
const COL_3_OUTLINE_STYLES = clsx("outline-event-col-3");

const COL_1_ROUNDED = "rounded-l-md";
const COL_3_ROUNDED = "rounded-r-md";

const UN_EXPANDED_OUTLINE_STYLES = clsx("ring-inset ring-2 ring-blue-500");

const TRANSITION_SHADOW = "transition-shadow ease-in-out duration-500";
const TRANSITION_OPACITY = "transition-opacity ease-in-out duration-150";

const GROUP_STLYES = "relative overflow-hidden";

// Space for the hover cells at the end of line.
const EOL_SPACE = "pr-12";

function UnificationEvent(props: UnificationProps): JSX.Element {
  const {
    engine,
    event,
    selectedEpochs,
    graphEe,
    eventListEe,
    depth,
    globalEe,
  } = props;
  const { mode, subevents, success } = event;

  const beforeUnificationEpoch = engine.getEventIndex(event);
  const afterUnificationEpoch = engine.getEventIndex(lastSubEvent(event));

  const containedEpoch = selectedEpochs.find((epoch) =>
    epochInRange(epoch, [beforeUnificationEpoch, afterUnificationEpoch])
  );

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
  const isOutlined = useFocusOutlineEvent<"focusEpoch", EventEpoch | undefined>(
    {
      ee: eventListEe,
      value: containedEpoch,
      event: "focusEpoch",
    }
  );

  const modeIcon = useMemo(() => <UnificationModeIcon mode={mode} />, [mode]);

  const resultIcon = success ? UN_SUCCESS : UN_FAILURE;
  const resultHeadline = <Headline icon={resultIcon}></Headline>;

  const epochCell = useMemo(() => {
    if (containedEpoch === undefined) return null;
    return <EventListEpochCell epoch={containedEpoch} graphEe={graphEe} />;
  }, [containedEpoch, graphEe]);

  const getBeforeUnificationHeadline = useCallback(
    ({
      epoch,
      collapsedMode,
    }: {
      epoch: EventEpoch;
      collapsedMode?: boolean;
    }) => {
      const topHeadline = (
        <Headline icon={isOpen ? UN_UNKNOWN : resultIcon}></Headline>
      );

      const optEpochCell =
        collapsedMode && containedEpoch !== undefined && epochCell;

      return (
        <button
          onClick={() => setIsOpen(!isOpen)}
          className={clsx(
            "w-full text-left whitespace-nowrap h-full overflow-scroll",
            collapsedMode && COL_1_P
          )}
        >
          <span className={EOL_SPACE}>
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
          </span>
        </button>
      );
    },
    [isOpen, resultIcon, containedEpoch, epochCell, leftVar, modeIcon, rightVar]
  );

  const { group, groupHover } = depthToGroupInfo(depth);

  if (!isOpen) {
    const headLine = getBeforeUnificationHeadline({
      epoch: afterUnificationEpoch,
      collapsedMode: true,
    });
    return (
      <div
        className={clsx(
          "rounded-md",
          TRANSITION_SHADOW,
          group,
          GROUP_STLYES,
          containedEpoch !== undefined &&
            isOutlined &&
            UN_EXPANDED_OUTLINE_STYLES
        )}
      >
        <div
          className={clsx(
            TRANSITION_OPACITY,
            containedEpoch === undefined && LOWER_OPACITY
          )}
        >
          {headLine}
        </div>
        <LoadEpochGraphLauncher
          groupHover={groupHover}
          epoch={afterUnificationEpoch}
          globalEe={globalEe}
          className="bottom-0 right-2"
        />
      </div>
    );
  } else {
    const beforeIsCurrentEpoch = beforeUnificationEpoch === containedEpoch;
    const afterIsCurrentEpoch = afterUnificationEpoch === containedEpoch;

    const epochCellBefore = beforeIsCurrentEpoch && epochCell;
    const epochCellAfter = afterIsCurrentEpoch && epochCell;

    const outlineEpochCellAfter = afterIsCurrentEpoch && isOutlined;
    const outlineEpochCellBefore = beforeIsCurrentEpoch && isOutlined;

    const headlineBefore = getBeforeUnificationHeadline({
      epoch: beforeUnificationEpoch,
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
      <div className={clsx(group, GROUP_STLYES)}>
        <div
          className={clsx(
            "grid gap-0 grid-cols-[min-content_min-content_1fr_auto] opacity-100",
            "overflow-scroll",
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
              "row-start-1 col-start-3 col-end-5",
              TRANSITION_SHADOW,
              COL_3_ROUNDED,
              outlineEpochCellBefore && COL_3_OUTLINE_STYLES,
              EOL_SPACE
            )}
          >
            {headlineBefore}
          </div>

          {/* Row 2: inner traces */}
          <div className={clsx("row-start-2 col-start-1")}></div>
          <div className={clsx("row-start-2col-start-3 col-end-4", "w-full")}>
            <EventList
              {...props}
              depth={depth + 1}
              engine={engine}
              events={subevents}
            />
          </div>

          {/* Row 3: unification end */}
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
              "row-start-3 col-start-3 col-end-5",
              TRANSITION_SHADOW,
              COL_3_ROUNDED,
              outlineEpochCellAfter && COL_3_OUTLINE_STYLES,
              EOL_SPACE
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

        <LoadEpochGraphLauncher
          groupHover={groupHover}
          epoch={beforeUnificationEpoch}
          globalEe={globalEe}
          className="top-0 right-2"
        />
        <LoadEpochGraphLauncher
          groupHover={groupHover}
          epoch={afterUnificationEpoch}
          globalEe={globalEe}
          className="bottom-0 right-2"
        />
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

interface LoadEpochGraphLauncherProps {
  groupHover: string;
  epoch: EventEpoch;
  globalEe: TypedEmitter<GlobalMessage>;
  className?: string;
}

function LoadEpochGraphLauncher({
  groupHover,
  epoch,
  className,
  globalEe,
}: LoadEpochGraphLauncherProps): JSX.Element {
  return (
    <div className={clsx("absolute opacity-0", groupHover, className)}>
      <span className="space-x-0.5 bg-gray-200 ring-1 ring-slate-300 rounded-sm px-1 opacity-80 hover:opacity-100">
        <span
          className="text-blue-400 hover:text-blue-500 cursor-pointer"
          onClick={() => {
            globalEe.emit("loadEpoch", epoch, LoadEpochView.Top);
          }}
        >
          ↑
        </span>
        <span
          className="text-blue-400 hover:text-blue-500 cursor-pointer"
          onClick={() => {
            globalEe.emit("loadEpoch", epoch, LoadEpochView.Bot);
          }}
        >
          ↓
        </span>
      </span>
    </div>
  );
}
