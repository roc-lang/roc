import { Content, Rank, Variable, Event } from "../schema";
import { assertExhaustive } from "../utils/exhaustive";
import { Refine } from "../utils/refine";

export type TypeLink = {
  type: "link";
  to: Variable;
};

function link({ to }: Omit<TypeLink, "type">): TypeLink {
  return { type: "link", to };
}

export type TypeDescriptor = {
  type: "descriptor";
  rank: Rank;
  content: Content;
};

function descriptor({
  rank,
  content,
}: Omit<TypeDescriptor, "type">): TypeDescriptor {
  return { type: "descriptor", rank, content };
}

export type VarType = TypeLink | TypeDescriptor;

export type RevertVariableChange = {
  type: "revertTo";
  variable: Variable;
  to: VarType;
};

export type DeleteVariableChange = {
  type: "delete";
  variable: Variable;
};

export type RollbackChange = RevertVariableChange | DeleteVariableChange;

export function makeRevertVariable({
  variable,
  to,
}: Omit<RevertVariableChange, "type">): RevertVariableChange {
  return { type: "revertTo", variable, to: { ...to } };
}

export function makeDeleteVariable({
  variable,
}: Omit<DeleteVariableChange, "type">): DeleteVariableChange {
  return { type: "delete", variable };
}

export type ChangeEvent =
  | Refine<Event, "VariableUnified">
  | Refine<Event, "VariableSetDescriptor">;

export type Change = ChangeEvent | RollbackChange;

export class Subs implements QuerySubs {
  #map: Map<Variable, VarType>;

  private constructor(map: Map<Variable, VarType>) {
    this.#map = map;
  }

  static new(): Subs {
    return new Subs(new Map());
  }

  get(variable: Variable): VarType | undefined {
    return this.#map.get(variable);
  }

  get_root(variable: Variable): TypeDescriptor | undefined {
    const type = this.get(variable);
    if (type === undefined) {
      return undefined;
    }
    switch (type.type) {
      case "descriptor":
        return type;
      case "link":
        return this.get_root(type.to);
      default:
        assertExhaustive(type);
    }
  }

  get_root_key(variable: Variable): Variable {
    const type = this.get(variable);
    if (type === undefined) {
      return variable;
    }
    switch (type.type) {
      case "descriptor":
        return variable;
      case "link":
        return this.get_root_key(type.to);
      default:
        assertExhaustive(type);
    }
  }

  snapshot(): SubsSnapshot {
    const snapshotMap = new Map<Variable, VarType>();
    for (const [key, value] of this.#map) {
      snapshotMap.set(key, { ...value });
    }
    const snapshot = new Subs(snapshotMap);
    return {
      get(variable: Variable): VarType | undefined {
        return snapshot.get(variable);
      },
      get_root(variable: Variable): TypeDescriptor | undefined {
        return snapshot.get_root(variable);
      },
      get_root_key(variable: Variable): Variable {
        return snapshot.get_root_key(variable);
      },
      __snapshot__: SnapshotSymbol,
    };
  }

  apply(change: Change): void {
    switch (change.type) {
      case "VariableUnified": {
        const { from, to } = change;
        if (from !== to) {
          this.#map.set(from, link({ to }));
        }
        break;
      }
      case "VariableSetDescriptor": {
        const { variable, rank, content } = change;
        const existing = this.get_root(variable);
        if (existing !== undefined) {
          const nu = descriptor({ ...existing });
          if (rank) nu.rank = rank;
          if (content) nu.content = content;
          this.#map.set(variable, nu);
        } else {
          if (typeof rank !== "number") throw new Error("rank is required");
          if (!content) throw new Error("content is required");
          this.#map.set(variable, descriptor({ rank, content }));
        }
        break;
      }
      case "revertTo": {
        const { variable, to } = change;
        this.#map.set(variable, { ...to });
        break;
      }
      case "delete": {
        const { variable } = change;
        this.#map.delete(variable);
        break;
      }
      default:
        assertExhaustive(change);
    }
  }
}

const SnapshotSymbol = Symbol("Snapshot");

export interface QuerySubs {
  get(variable: Variable): VarType | undefined;
  get_root(variable: Variable): TypeDescriptor | undefined;
  get_root_key(variable: Variable): Variable;
}

export interface SubsSnapshot extends QuerySubs {
  __snapshot__: typeof SnapshotSymbol;
}
