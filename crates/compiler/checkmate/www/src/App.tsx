import React from "react";
import FileInput, { LoadedEvents } from "./components/FileInput";
import Ui from "./components/Ui";
import { BrowserRouter } from "react-router-dom";

export default function App() {
  const [events, setEvents] = React.useState<LoadedEvents | null>(null);

  return (
    <BrowserRouter>
      <div className="w-screen h-screen p-2 bg-gray-100 flex flex-col">
        <div>
          <FileInput setResult={setEvents} />
        </div>
        <div className="flex-1 overflow-hidden">
          <EventsWrapper events={events} />
        </div>
      </div>
    </BrowserRouter>
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
