import { EventIndex } from "../../engine/engine";
import { Variable } from "../../schema";
import { VariableElPretty } from "../Common/Variable";
import { CommonProps } from "./types";

interface VariableProps extends CommonProps {
  index: EventIndex;
  variable: Variable;
}

export function VariableEl({
  engine,
  toggleVariableVis,
  index,
  variable,
}: VariableProps): JSX.Element {
  engine.stepTo(index);
  return (
    <VariableElPretty
      variable={variable}
      subs={engine.subs}
      onClick={(variable: Variable) => {
        toggleVariableVis(variable);
      }}
    ></VariableElPretty>
  );
}
