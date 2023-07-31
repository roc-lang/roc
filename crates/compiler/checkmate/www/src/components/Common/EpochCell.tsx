import clsx from "clsx";
import { EventEpoch } from "../../engine/engine";

interface EpochCellProps {
  noLeadingText?: boolean;
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

export default function EpochCell({
  epoch,
  className,
  noLeadingText,
}: EpochCellProps) {
  return (
    <div className={clsx(EPOCH_STYLES, className)}>
      {noLeadingText ? "" : "Epoch "}
      {epoch}
    </div>
  );
}
