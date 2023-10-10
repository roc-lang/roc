import { EventEpoch } from "../engine/engine";
import { Variable } from "../schema";

export interface VariableMessage {
  focus: (variable: Variable) => void;
}

export interface GraphMessage {
  focusEpoch: (epoch: EventEpoch) => void;
  focusVariable: (variable: Variable) => void;
  keydown: (key: string) => void;
}

export interface EventListMessage {
  focusEpoch: (epoch: EventEpoch) => void;
}

export enum LoadEpochView {
  Top,
  Bot,
}

export interface GlobalMessage {
  loadEpoch: (epoch: EventEpoch, view: LoadEpochView) => void;
}
