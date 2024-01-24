import clsx from "clsx";
import { QuerySubs } from "../../engine/subs";
import { Variable } from "../../schema";
import { VariableName } from "./VariableName";

export interface VariableLinkProps {
  variable: Variable;
  subs: QuerySubs;
  onClick?: (variable: Variable) => void;
}

export function VariableLink({
  variable,
  subs,
  onClick,
}: VariableLinkProps): JSX.Element {
  const root = subs.get_root_key(variable);

  if (variable === root) {
    throw new Error("VariableLink: variable is root");
  }

  return (
    <div className={clsx("rounded-md whitespace-nowrap space-x-1")}>
      <VariableName className="inline-block" variable={variable} />
      <span>→</span>
      <VariableName
        className="inline-block"
        variable={root}
        onClick={onClick}
      />
    </div>
  );
}
