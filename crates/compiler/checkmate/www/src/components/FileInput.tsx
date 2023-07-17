import { AllEvents } from "../schema";

export type EventsOk = {
  kind: "ok";
  events: AllEvents;
};

export type EventsErr = {
  kind: "err";
  error: string;
};

export type LoadedEvents = EventsOk | EventsErr;

interface FileInputProps {
  setResult(result: LoadedEvents): void;
}

export default function FileInput({ setResult }: FileInputProps) {
  async function setFile(e: React.ChangeEvent<HTMLInputElement>) {
    e.preventDefault();
    const files = e.target.files;
    if (!files) {
      setResult({ kind: "err", error: "Please choose a checkmate file." });
      return;
    }
    const file = files[0];
    const buf = await file.arrayBuffer();
    try {
      const events: AllEvents = JSON.parse(new TextDecoder().decode(buf));
      setResult({ kind: "ok", events });
    } catch (e) {
      setResult({ kind: "err", error: "Invalid checkmate file." });
      return;
    }
  }

  return (
    <div>
      <label htmlFor="small-file-input" className="sr-only">
        Choose file
      </label>
      <input
        type="file"
        name="small-file-input"
        id="small-file-input"
        onChange={(e) => setFile(e)}
        className="block w-full border border-gray-200 shadow-sm rounded-md text-sm
    file:bg-roc-purple-bg file:border-0 file:mr-4 file:py-2 file:px-4"
      ></input>
    </div>
  );
}
