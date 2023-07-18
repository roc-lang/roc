import React from "react";
import { AllEvents, Variable } from "../schema";
import { Engine } from "../engine/engine";
import EventList from "./EventList";
import VariablesGraph from "./Graph/VariablesGraph";
import { TypedEmitter } from "tiny-typed-emitter";
import { KeydownHandler, ToggleVariableHandler } from "./Events";

interface UiProps {
  events: AllEvents;
}

interface MessageEvents {
  toggleVariable: ToggleVariableHandler;
  keydown: KeydownHandler;
}

export default function Ui({ events }: UiProps): JSX.Element {
  const engine = new Engine(events);

  const ee = new TypedEmitter<MessageEvents>();
  const toggleVariableHandlers: ToggleVariableHandler[] = [];
  const keydownHandlers: KeydownHandler[] = [];
  ee.on("toggleVariable", (variable: Variable) => {
    toggleVariableHandlers.forEach((handler) => handler(variable));
  });
  ee.on("keydown", (key: string) => {
    keydownHandlers.forEach((handler) => handler(key));
  });

  engine.stepTo(engine.lastEventIndex());
  const subs = engine.subs.snapshot();

  return (
    <div
      className="flex flex-col md:flex-row gap-0 w-full h-full"
      onKeyDown={(e) => {
        ee.emit("keydown", e.key);
      }}
    >
      <div className="font-mono mt-2 text-lg md:flex-1 overflow-scroll">
        <EventList
          engine={engine}
          root
          events={events}
          toggleVariableVis={(variable: Variable) =>
            ee.emit("toggleVariable", variable)
          }
        />
      </div>
      <div className="flex-1 min-h-[50%] h-full">
        <VariablesGraph
          subs={subs}
          onVariable={(handler) => {
            toggleVariableHandlers.push(handler);
          }}
          onKeydown={(handler) => {
            keydownHandlers.push(handler);
          }}
        />
      </div>
    </div>
  );
}
