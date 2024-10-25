import clsx from "clsx";
import { Handle, Position } from "reactflow";
import { Variable } from "../../schema";
import { assertExhaustive } from "../../utils/exhaustive";
import { contentStyles, LinkStyles } from "../Content";
import { VariableElPretty } from "../Common/Variable";
import { SubsSnapshot, TypeDescriptor } from "../../engine/subs";
import { TypedEmitter } from "tiny-typed-emitter";
import { VariableLink } from "../Common/VariableLink";
import { VariableMessage } from "../../utils/events";
import { useFocusOutlineEvent } from "../../hooks/useFocusOutlineEvent";
import { UnknownVariable } from "../Common/UnknownVariable";

type AddSubVariableLink = ({
  from,
  variable,
}: {
  from: Variable;
  variable: Variable;
}) => void;

export interface VariableNodeProps {
  data: {
    subs: SubsSnapshot;
    rawVariable: Variable;
    addSubVariableLink: AddSubVariableLink;
    isOutlined: boolean;
    ee: TypedEmitter<VariableMessage>;
  };
  targetPosition?: Position;
  sourcePosition?: Position;
}

export default function VariableNode({
  data,
  targetPosition,
  sourcePosition,
}: VariableNodeProps): JSX.Element {
  const {
    subs,
    rawVariable,
    addSubVariableLink,
    isOutlined: isOutlinedProp,
    ee: eeProp,
  } = data;

  const isOutlined = useFocusOutlineEvent({
    ee: eeProp,
    value: rawVariable,
    event: "focus",
    defaultIsOutlined: isOutlinedProp,
  });

  const varType = subs.get(rawVariable);

  let renderContent: JSX.Element;
  let bgStyles: string;
  const isContent = varType?.type === "descriptor";
  switch (varType?.type) {
    case undefined: {
      bgStyles = "bg-red-500";
      renderContent = <UnknownVariable variable={rawVariable} />;

      break;
    }
    case "link": {
      bgStyles = LinkStyles.bg;

      renderContent = (
        <VariableLink
          subs={subs}
          variable={rawVariable}
          onClick={() =>
            addSubVariableLink({
              from: rawVariable,
              variable: subs.get_root_key(rawVariable),
            })
          }
        />
      );

      break;
    }
    case "descriptor": {
      const variable = rawVariable;
      const desc: TypeDescriptor = varType;

      const styles = contentStyles(desc);
      bgStyles = styles.bg;
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

      renderContent = (
        <>
          <div>
            <VariableElPretty variable={variable} subs={subs} />
          </div>
          {expandedContent}
        </>
      );

      break;
    }
    default: {
      assertExhaustive(varType);
    }
  }

  return (
    <div
      className={clsx(
        bgStyles,
        "bg-opacity-50 rounded-md transition ease-in-out duration-700",
        isContent ? "py-2 px-4 border" : "p-0",
        isOutlined && "ring-2 ring-blue-500",
        "text-center font-mono"
      )}
    >
      <Handle
        type="target"
        position={targetPosition ?? Position.Top}
        isConnectable={false}
        style={{ background: "transparent", border: "none" }}
      />
      {renderContent}
      <Handle
        type="source"
        position={sourcePosition ?? Position.Bottom}
        isConnectable={false}
        style={{ background: "transparent", border: "none" }}
      />
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
      onClick={() => addSubVariableLink({ from: origin, variable })}
    />
  );
}
