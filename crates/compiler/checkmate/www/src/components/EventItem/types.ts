import type { Engine, EventEpoch } from "../../engine/engine";
import type { Variable } from "../../schema";

export interface CommonProps {
  currentEpoch: EventEpoch;
  engine: Engine;
  toggleVariableVis: (variable: Variable) => void;
}
