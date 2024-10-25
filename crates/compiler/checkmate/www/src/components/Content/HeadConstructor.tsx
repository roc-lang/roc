import { TypeDescriptor } from "../../engine/subs";
import {
  ClosureType,
  RecordFieldKind,
  UnspecializedClosureType,
  Variable,
} from "../../schema";

type DrawVariable = (variable: Variable) => JSX.Element;

export interface DrawHeadConstructorProps {
  desc: TypeDescriptor | undefined;
  drawVariablePretty: DrawVariable;
  drawVariableRaw: DrawVariable;
}

export default function DrawHeadConstructor({
  desc,
  drawVariablePretty,
  drawVariableRaw,
}: DrawHeadConstructorProps): JSX.Element {
  if (!desc) {
    return <>???</>;
  }
  const content = desc.content;
  switch (content.type) {
    case "Flex":
    case "Rigid": {
      const { name } = content;
      return name ? <>{name}</> : <>_</>;
    }
    case "FlexAble":
    case "RigidAble": {
      const { name, abilities } = content;
      const nameEl = name ? <>{name}</> : <>_</>;
      return (
        <>
          {nameEl} has {abilities.join(", ")}
        </>
      );
    }
    case "Recursive": {
      const { name, structure } = content;
      const structureEl = drawVariableRaw(structure);
      const nameEl = name ? <>{name} to </> : <></>;
      return (
        <>
          &lt;{nameEl}
          {structureEl}&gt;
        </>
      );
    }
    case "LambdaSet": {
      const { ambient_function, solved, unspecialized, recursion_var } =
        content;
      const ambientFunctionEl = drawVariableRaw(ambient_function);
      const solvedEl = (
        <DrawSolved drawVariableRaw={drawVariableRaw} solved={solved} />
      );
      const unspecializedEl = (
        <DrawUnspecialized
          drawVariableRaw={drawVariableRaw}
          unspecialized={unspecialized}
        />
      );
      const recursionVarEl = recursion_var ? (
        <> as &lt;{drawVariableRaw(recursion_var)}&gt;</>
      ) : (
        <></>
      );
      return (
        <>
          [{solvedEl}
          {unspecializedEl}]{recursionVarEl} ^{ambientFunctionEl}
        </>
      );
    }
    case "ErasedLambda": {
      return <>?</>;
    }
    case "Alias": {
      const { kind, name, variables } = content;
      const prefix = kind.type === "Opaque" ? "@" : "";
      const variablesEl = (
        <DrawVarArgumentsList
          drawVariableRaw={drawVariableRaw}
          variables={variables.type_variables}
        />
      );
      return (
        <span>
          {prefix}
          {sym(name)}
          {variablesEl}
        </span>
      );
    }
    case "Apply": {
      const { symbol, variables } = content;
      const variablesEl = (
        <DrawVarArgumentsList
          drawVariableRaw={drawVariableRaw}
          variables={variables}
        />
      );
      return (
        <>
          {sym(symbol)}
          {variablesEl}
        </>
      );
    }
    case "Function": {
      const { arguments: args, lambda_type, ret } = content;
      const argsEl = args.map((arg, i) => (
        <span key={i}>
          {i !== 0 ? ", " : ""}
          {drawVariablePretty(arg)}
        </span>
      ));
      const lambdaTypeEl = drawVariablePretty(lambda_type);
      const retEl = drawVariablePretty(ret);
      return (
        <>
          {argsEl}
          {" -"}
          {lambdaTypeEl}
          {"-> "}
          {retEl}
        </>
      );
    }
    case "Record": {
      const { fields, extension } = content;
      const fieldsEl = Object.entries(fields).map(([key, value], i) => {
        const { field_type, kind } = value;
        return (
          <span key={i}>
            {i !== 0 ? ", " : ""}
            {key} {<DrawFieldKind kind={kind} />}{" "}
            {drawVariablePretty(field_type)}
          </span>
        );
      });
      return (
        <>
          {"{"}
          {fieldsEl}
          {"}"}
          {drawVariablePretty(extension)}
        </>
      );
    }
    case "Tuple": {
      const { elements, extension } = content;
      const elemsEl = Object.entries(elements).map(([key, value], i) => {
        return (
          <span key={i}>
            {i !== 0 ? ", " : ""}
            {key}: {drawVariablePretty(value)}
          </span>
        );
      });
      return (
        <>
          ({elemsEl}){drawVariablePretty(extension)}
        </>
      );
    }
    case "TagUnion": {
      const { tags, extension } = content;
      return (
        <>
          <DrawTags tags={tags} drawVariableRaw={drawVariableRaw} />
          {drawVariablePretty(extension.variable)}
        </>
      );
    }
    case "RecursiveTagUnion": {
      const { tags, extension, recursion_var } = content;
      return (
        <>
          (<DrawTags tags={tags} drawVariableRaw={drawVariableRaw} />
          {drawVariablePretty(extension.variable)} as &lt;
          {drawVariableRaw(recursion_var)}&gt;)
        </>
      );
    }
    case "FunctionOrTagUnion": {
      const { functions, tags, extension } = content;
      const functionsEl = functions.map((f, i) => (
        <span key={i}>
          {i !== 0 ? ", " : ""}
          {sym(f)}
        </span>
      ));
      const tagsEl = tags.map((t, i) => (
        <span key={i}>
          {i !== 0 ? ", " : ""}
          {t}
        </span>
      ));
      return (
        <>
          [{functionsEl} | {tagsEl}]{drawVariablePretty(extension.variable)}
        </>
      );
    }
    case "RangedNumber": {
      const {
        range: { kind, min_width, signed },
      } = content;
      switch (kind.type) {
        case "AnyNum":
          return <>ℚ{min_width}+</>;
        case "Int":
          return signed ? <>ℤ{min_width}+</> : <>ℕ{min_width}+</>;
      }
      break;
    }
    case "EmptyRecord": {
      return <>{"{}"}</>;
    }
    case "EmptyTagUnion": {
      return <>[]</>;
    }
    case "Error": {
      return <>⊥</>;
    }
  }
}

function DrawVarArgumentsList({
  variables,
  drawVariableRaw,
}: {
  variables: Variable[];
  drawVariableRaw: DrawVariable;
}): JSX.Element {
  return variables.length !== 0 ? (
    <>
      {" "}
      {variables.map((v, i) => (
        <span key={i}>{drawVariableRaw(v)}</span>
      ))}
    </>
  ) : (
    <></>
  );
}

function DrawSolved({
  solved,
  drawVariableRaw,
}: {
  solved: ClosureType[];
  drawVariableRaw: DrawVariable;
}): JSX.Element {
  const tags = solved.map(({ environment, function: fn }, i) => (
    <span key={i}>
      {i !== 0 ? ", " : ""}
      <DrawTag
        tag={sym(fn)}
        variables={environment}
        drawVariableRaw={drawVariableRaw}
      />
    </span>
  ));
  return <>[{tags}]</>;
}

function DrawTags({
  tags,
  drawVariableRaw,
}: {
  tags: Record<string, Variable[]>;
  drawVariableRaw: DrawVariable;
}): JSX.Element {
  const tagsEl = Object.entries(tags).map(([tag, vars], i) => (
    <span key={i}>
      {i !== 0 ? ", " : ""}
      <DrawTag tag={tag} variables={vars} drawVariableRaw={drawVariableRaw} />
    </span>
  ));
  return <>[{tagsEl}]</>;
}

function DrawUnspecialized({
  unspecialized,
  drawVariableRaw,
}: {
  unspecialized: UnspecializedClosureType[];
  drawVariableRaw: DrawVariable;
}): JSX.Element {
  const unspecs = unspecialized.map(
    ({ ability_member, lambda_set_region, specialization }, i) => (
      <span key={i}>
        {" + "}
        {drawVariableRaw(specialization)}:{sym(ability_member)}:
        {lambda_set_region}
      </span>
    )
  );
  return <>{unspecs}</>;
}

function DrawTag({
  tag,
  variables,
  drawVariableRaw,
}: {
  tag: string;
  variables: Variable[];
  drawVariableRaw: DrawVariable;
}): JSX.Element {
  return (
    <>
      {tag}
      <DrawVarArgumentsList
        drawVariableRaw={drawVariableRaw}
        variables={variables}
      />
    </>
  );
}

function DrawFieldKind({ kind }: { kind: RecordFieldKind }): JSX.Element {
  switch (kind.type) {
    case "Required":
    case "Demanded":
      return <>:</>;
    case "Optional":
      return <>?</>;
  }
}

function sym(symbol: string): string {
  if (symbol.startsWith("`")) symbol = symbol.slice(1);
  if (symbol.endsWith("`")) symbol = symbol.slice(0, -1);
  return symbol.split(".").at(-1)!;
}
