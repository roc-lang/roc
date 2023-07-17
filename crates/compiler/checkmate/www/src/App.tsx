import React from "react";
import FileInput, { LoadedEvents } from "./components/FileInput";
import Ui from "./components/Ui";
import data from "./checkmate.json";
import { AllEvents } from "./schema";

export default function App() {
  const [events, setEvents] = React.useState<LoadedEvents | null>({
    kind: "ok",
    events: data as AllEvents,
  });

  return (
    <div className="container w-screen h-screen p-2">
      <FileInput setResult={setEvents} />
      <EventsWrapper events={events} />
    </div>
  );
}

interface EventsWrapperProps {
  events: LoadedEvents | null;
}

function EventsWrapper({ events }: EventsWrapperProps): JSX.Element {
  if (events === null) {
    return <div></div>;
  }
  switch (events.kind) {
    case "ok":
      return <Ui events={events.events} />;
    case "err":
      return <div className="text-red-400 text-lg">{events.error}</div>;
  }
}
