import type { Engine } from "../../engine/engine";
import type { Variable } from "../../schema";

export interface CommonProps {
  engine: Engine;
  toggleVariableVis: (variable: Variable) => void;
}
