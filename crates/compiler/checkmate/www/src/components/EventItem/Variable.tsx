import { EventIndex } from "../../engine/engine";
import { Variable } from "../../schema";
import { contentStyles } from "../Content";
import { VariableElHelp } from "../Common/Variable";
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
  const desc = engine.subs.get_root(variable);
  const styles = contentStyles(desc);
  return (
    <VariableElHelp
      variable={variable}
      styles={styles}
      onClick={() => {
        toggleVariableVis(variable);
      }}
    ></VariableElHelp>
  );
}
