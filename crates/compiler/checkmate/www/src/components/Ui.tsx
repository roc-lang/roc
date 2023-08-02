import React, { useState } from "react";
import { AllEvents } from "../schema";
import { Engine } from "../engine/engine";
import EventList from "./EventList";
import VariablesGraph from "./Graph/VariablesGraph";
import { TypedEmitter } from "tiny-typed-emitter";
import { EventListMessage, GraphMessage } from "../utils/events";

interface UiProps {
  events: AllEvents;
}

export default function Ui({ events }: UiProps): JSX.Element {
  const engine = new Engine(events);

  const graphEe = React.useRef(new TypedEmitter<GraphMessage>());
  const eventListEe = React.useRef(new TypedEmitter<EventListMessage>());

  engine.stepTo(engine.lastEventIndex());
  const subs = engine.subsSnapshot();

  // _setEpoch to be used in the future!
  const [epoch, _setEpoch] = useState(subs.epoch);

  return (
    <div
      className="flex flex-col md:flex-row gap-0 w-full h-full"
      onKeyDown={(e) => {
        graphEe.current.emit("keydown", e.key);
      }}
    >
      <div className="font-mono mt-2 text-lg md:flex-1 overflow-scroll">
        <EventList
          engine={engine}
          root
          events={events}
          graphEe={graphEe.current}
          eventListEe={eventListEe.current}
          currentEpoch={epoch}
        />
      </div>
      <div className="flex-1 min-h-[50%] h-full">
        <VariablesGraph
          subs={subs}
          graphEe={graphEe.current}
          eventListEe={eventListEe.current}
        />
      </div>
    </div>
  );
}
