import { ComponentProps } from "react";
import clsx from "clsx";
import { QuerySubs, TypeDescriptor } from "../../engine/subs";
import { Variable } from "../../schema";
import DrawHeadConstructor from "../Content/HeadConstructor";
import { contentStyles } from "./../Content";
import { VariableName } from "./VariableName";

interface VariableElProps {
  variable: Variable;
  subs: QuerySubs;
  onClick?: (variable: Variable) => void;
  nested?: boolean;
  raw?: boolean;
}

export function VariableElPretty(props: VariableElProps): JSX.Element {
  const { variable, subs } = props;
  const desc = subs.get_root(variable);
  const content = (
    <DrawHeadConstructor
      desc={desc}
      drawVariablePretty={(variable) => (
        <VariableElPretty {...props} variable={variable} nested />
      )}
      drawVariableRaw={(variable) => (
        <VariableElRaw {...props} variable={variable} nested raw />
      )}
    />
  );
  return (
    <Helper {...props} desc={desc}>
      {content}
    </Helper>
  );
}

function VariableElRaw(props: VariableElProps): JSX.Element {
  const desc = props.subs.get_root(props.variable);
  return <Helper {...props} desc={desc}></Helper>;
}

function Helper({
  children,
  variable,
  desc,
  onClick,
  nested,
  raw,
}: VariableElProps &
  Pick<ComponentProps<"div">, "children"> & {
    desc: TypeDescriptor | undefined;
  }): JSX.Element {
  const { bg } = contentStyles(desc);
  return (
    <span
      className={clsx(
        "rounded-md whitespace-nowrap",
        bg,
        nested ? "text-sm" : "p-0.5 pl-0 text-base"
      )}
    >
      {(!nested || raw) && (
        <VariableName
          variable={variable}
          onClick={onClick}
          className={nested ? "text-md" : "p-0.5"}
        />
      )}
      {children ? <span className="px-1">{children}</span> : <></>}
    </span>
  );
}
