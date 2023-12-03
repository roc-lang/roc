import clsx from "clsx";
import { Variable } from "../../schema";

export interface VariableNameProps {
  variable: Variable;
  onClick?: (variable: Variable) => void;
  className?: string;
}

export function VariableName({
  variable,
  onClick,
  className,
}: VariableNameProps): JSX.Element {
  return (
    <span
      className={clsx(
        "ring-1 ring-inset ring-black-100 px-1 bg-white rounded-md",
        onClick && "cursor-pointer",
        className
      )}
      onClick={(e) => {
        e.stopPropagation();
        onClick?.(variable);
      }}
    >
      {variable}
    </span>
  );
}
