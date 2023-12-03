import clsx from "clsx";

interface EpochCellProps {
  className?: string;
  children?: React.ReactNode;
  focus?: boolean;
}

export const EPOCH_STYLES =
  "text-slate-900 font-mono bg-slate-200 p-1 py-0 rounded-sm text-sm transition ease-in-out duration-700 mr-2";

export default function EpochCell({
  className,
  children,
  focus,
}: EpochCellProps) {
  return (
    <span
      className={clsx(
        EPOCH_STYLES,
        className,
        focus === true ? "ring-2 ring-blue-500" : "ring-1 ring-slate-500"
      )}
    >
      {children}
    </span>
  );
}
