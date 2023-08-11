import React, { useEffect, useState } from "react";
import { AllEvents } from "../schema";
import { Engine, EventEpoch } from "../engine/engine";
import EventList from "./EventList/index";
import VariablesGraph from "./Graph/VariablesGraph";
import { TypedEmitter } from "tiny-typed-emitter";
import {
  EventListMessage,
  GlobalMessage,
  GraphMessage,
  LoadEpochView,
} from "../utils/events";
import { assertExhaustive } from "../utils/exhaustive";
import { SubsSnapshot } from "../engine/subs";

interface UiProps {
  events: AllEvents;
}

export default function Ui({ events }: UiProps): JSX.Element {
  const engine = React.useRef(new Engine(events));

  const graphEe = React.useRef(new TypedEmitter<GraphMessage>());
  const eventListEe = React.useRef(new TypedEmitter<EventListMessage>());
  const globalEe = React.useRef(new TypedEmitter<GlobalMessage>());

  const [subsTop, setSubsTop] = useState<SubsSnapshot | undefined>(undefined);
  const [subsBot, setSubsBot] = useState<SubsSnapshot | undefined>(undefined);

  useEffect(() => {
    globalEe.current.on("loadEpoch", (epoch, view) => {
      switch (view) {
        case LoadEpochView.Top: {
          setSubsTop(engine.current.stepToSnapshot(epoch));
          break;
        }
        case LoadEpochView.Bot: {
          setSubsBot(engine.current.stepToSnapshot(epoch));
          break;
        }
        default:
          assertExhaustive(view);
      }
    });
  }, []);

  const selectedEpochs = [subsTop?.epoch, subsBot?.epoch]
    .filter((x): x is EventEpoch => x !== undefined)
    .sort();

  return (
    <div
      className="flex flex-col md:flex-row gap-0 w-full h-full"
      onKeyDown={(e) => {
        graphEe.current.emit("keydown", e.key);
      }}
    >
      <div className="font-mono mt-2 text-lg md:flex-1 overflow-x-hidden overflow-y-scroll">
        <EventList
          engine={engine.current}
          depth={0}
          events={events}
          graphEe={graphEe.current}
          eventListEe={eventListEe.current}
          globalEe={globalEe.current}
          selectedEpochs={selectedEpochs}
        />
      </div>
      <div className="flex-1 min-h-[50%] h-full flex flex-col place-content-center shadow">
        {selectedEpochs.length === 0 && (
          <span className="text-center">
            <span className="p-2 border rounded-md bg-gray-200 inline-block">
              Select an event to view.
            </span>
          </span>
        )}
        {subsTop !== undefined && (
          <VariablesGraphView
            subs={subsTop}
            graphEe={graphEe.current}
            eventListEe={eventListEe.current}
          />
        )}
        {/* */}
        {subsBot !== undefined && (
          <VariablesGraphView
            subs={subsBot}
            graphEe={graphEe.current}
            eventListEe={eventListEe.current}
          />
        )}
      </div>
    </div>
  );
}

interface VariablesGraphViewProps {
  subs: SubsSnapshot;
  graphEe: TypedEmitter<GraphMessage>;
  eventListEe: TypedEmitter<EventListMessage>;
}

function VariablesGraphView({
  subs,
  graphEe,
  eventListEe,
}: VariablesGraphViewProps): JSX.Element {
  return (
    <VariablesGraph subs={subs} graphEe={graphEe} eventListEe={eventListEe} />
  );
}
