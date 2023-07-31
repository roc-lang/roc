import clsx from "clsx";
import { EventEpoch } from "../../engine/engine";
import { HashLink } from "react-router-hash-link";

export enum EpochCellView {
  Events,
  Graph,
}

function invert(cell: EpochCellView): EpochCellView {
  if (cell === EpochCellView.Events) {
    return EpochCellView.Graph;
  }
  return EpochCellView.Events;
}

function asStr(cell: EpochCellView): string {
  switch (cell) {
    case EpochCellView.Events:
      return "events";
    case EpochCellView.Graph:
      return "graph";
  }
}

interface EpochCellProps {
  view: EpochCellView;
  epoch: EventEpoch;
  className?: string;
}

const EPOCH_STYLES_ARRAY = [
  "text-slate-900",
  "font-mono",
  "bg-slate-200",
  "p-1",
  "py-0",
  "rounded-sm",
  "ring-1",
  "ring-slate-500",
  "text-sm",
];

export const EPOCH_STYLES = clsx(...EPOCH_STYLES_ARRAY);

export default function EpochCell({ epoch, className, view }: EpochCellProps) {
  const invertedView = invert(view);

  return (
    <HashLink smooth to={`#${asStr(invertedView)}-${epoch}`}>
      <div
        id={`${asStr(view)}-${epoch}`}
        className={clsx(EPOCH_STYLES, className)}
      >
        {view === EpochCellView.Graph ? "Epoch " : ""}
        {epoch}
      </div>
    </HashLink>
  );
}
