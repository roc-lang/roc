import clsx from "clsx";
import { Handle, Position } from "reactflow";
import { Variable } from "../../schema";
import { assertExhaustive } from "../../utils/exhaustive";
import { contentStyles } from "../Content";
import { VariableElPretty } from "../Common/Variable";
import { SubsSnapshot, TypeDescriptor } from "../../engine/subs";

type AddSubVariableLink = (from: Variable, subVariable: Variable) => void;

export interface VariableNodeProps {
  data: {
    subs: SubsSnapshot;
    variable: Variable;
    addSubVariableLink: AddSubVariableLink;
  };
}

export default function VariableNode({ data }: VariableNodeProps): JSX.Element {
  const { variable, subs, addSubVariableLink } = data;

  const desc = subs.get_root(variable);
  const styles = contentStyles(desc);
  const basis: BasisProps = {
    subs,
    origin: variable,
    addSubVariableLink,
  };

  const content = Object.entries(
    VariableNodeContent(variable, desc, basis)
  ).filter((el): el is [string, JSX.Element] => !!el[1]);

  let expandedContent = <></>;
  if (content.length > 0) {
    expandedContent = (
      <ul className="text-sm text-left mt-2 space-y-1">
        {content.map(([key, value], i) => (
          <li key={i} className="space-x-2">
            {key}: {value}
          </li>
        ))}
      </ul>
    );
  }

  return (
    <div
      className={clsx(
        styles.bg,
        "bg-opacity-50 py-2 px-4 rounded-lg border",
        "text-center font-mono"
      )}
    >
      <Handle type="target" position={Position.Top} isConnectable={false} />
      <div>
        <VariableElPretty variable={variable} subs={subs} />
      </div>
      {expandedContent}
      <Handle type="source" position={Position.Bottom} isConnectable={false} />
    </div>
  );
}

function VariableNodeContent(
  variable: Variable,
  desc: TypeDescriptor | undefined,
  basis: BasisProps
): Record<string, JSX.Element | null> {
  if (!desc) return {};
  const { content } = desc;

  switch (content.type) {
    case "Flex":
    case "Rigid": {
      const { name } = content;
      return { name: name ? <>{name}</> : null };
    }
    case "FlexAble":
    case "RigidAble": {
      const { name, abilities } = content;
      return {
        name: <>{name}</>,
        abilities: <>[{abilities.join(", ")}]</>,
      };
    }
    case "Recursive": {
      const { name, structure } = content;
      return {
        name: <>{name}</>,
        structure: <SubVariable {...basis} variable={structure} />,
      };
    }
    case "LambdaSet": {
      const { ambient_function, solved, unspecialized, recursion_var } =
        content;
      return {
        "^": <SubVariable {...basis} variable={ambient_function} />,
        as: recursion_var ? (
          <SubVariable {...basis} variable={recursion_var} />
        ) : null,
      };
    }
    case "ErasedLambda": {
      return {};
    }
    case "Alias": {
      const { name, real_variable, variables } = content;
      return {
        name: <>{name}</>,
      };
    }
    case "Apply": {
      const { name, variables } = content;
      return {
        name: <>{name}</>,
      };
    }
    case "Function": {
      const { arguments: args, lambda_type, ret } = content;
      return {
        args: (
          <>
            {args.map((arg, i) => (
              <SubVariable key={i} {...basis} variable={arg} />
            ))}
          </>
        ),
        "||": <SubVariable {...basis} variable={lambda_type} />,
        ret: <SubVariable {...basis} variable={ret} />,
      };
    }
    case "FunctionOrTagUnion": {
      const { tags, functions, extension } = content;
      return {
        tags: <>[{tags.join(", ")}]</>,
        fns: <>[{functions.join(", ")}]</>,
      };
    }
    case "TagUnion": {
      const { tags, extension } = content;
      return {};
    }
    case "RecursiveTagUnion": {
      const { recursion_var, extension, tags } = content;
      return {
        as: <SubVariable {...basis} variable={recursion_var} />,
      };
    }
    case "Record": {
      const { fields, extension } = content;
      return {};
    }
    case "Tuple": {
      const { elements, extension } = content;
      return {};
    }
    case "RangedNumber": {
      const { range } = content;
      return {};
    }
    case "EmptyRecord":
    case "EmptyTuple":
    case "EmptyTagUnion":
    case "Error": {
      return {};
    }
    default: {
      return assertExhaustive(content);
    }
  }
}

interface BasisProps {
  subs: SubsSnapshot;
  origin: Variable;
  addSubVariableLink: AddSubVariableLink;
}

function SubVariable({
  subs,
  origin,
  variable,
  addSubVariableLink,
}: {
  variable: Variable;
} & BasisProps): JSX.Element {
  return (
    <VariableElPretty
      variable={variable}
      subs={subs}
      onClick={() => addSubVariableLink(origin, variable)}
    />
  );
}
