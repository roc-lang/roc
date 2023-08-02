import clsx from "clsx";
import { Variable } from "../../schema";
import { VariableName } from "./VariableName";

export interface UnknownVariableProps {
  variable: Variable;
}

export function UnknownVariable({
  variable,
}: UnknownVariableProps): JSX.Element {
  return (
    <div className={clsx("rounded-md whitespace-nowrap space-x-1 pr-1")}>
      <VariableName className="inline-block" variable={variable} />
      <span>???</span>
    </div>
  );
}
