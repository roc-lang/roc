import { TypeDescriptor } from "../../engine/subs";
import { assertExhaustive } from "../../utils/exhaustive";

export interface ContentStyles {
  name: string;
  bg: string;
}

export function contentStyles(desc: TypeDescriptor | undefined): ContentStyles {
  if (!desc) {
    return { name: "???", bg: "bg-red-500" };
  }

  const content = desc.content;
  switch (content.type) {
    case "Flex":
      return { name: "Flex", bg: "bg-blue-300" };
    case "FlexAble":
      return { name: "FlexAble", bg: "bg-blue-400" };
    case "Rigid":
      return { name: "Rigid", bg: "bg-indigo-300" };
    case "RigidAble":
      return { name: "RigidAble", bg: "bg-indigo-400" };
    case "Recursive":
      return { name: "Rec", bg: "bg-blue-grey-500" };
    case "LambdaSet":
      return { name: "LambdaSet", bg: "bg-green-500" };
    case "ErasedLambda":
      return { name: "ErasedLambda", bg: "bg-green-700" };
    case "Alias": {
      switch (content.kind.type) {
        case "Structural":
          return { name: "Alias", bg: "bg-yellow-300" };
        case "Opaque":
          return { name: "Opaque", bg: "bg-amber-400" };
        default:
          assertExhaustive(content.kind);
      }
      break;
    }
    case "Apply":
      return { name: "Apply", bg: "bg-orange-500" };
    case "Function":
      return { name: "Func", bg: "bg-teal-400" };
    case "Record":
      return { name: "Record", bg: "bg-purple-400" };
    case "Tuple":
      return { name: "Tuple", bg: "bg-deep-purple-400" };
    case "TagUnion":
      return { name: "Tags", bg: "bg-cyan-200" };
    case "FunctionOrTagUnion":
      return { name: "Func|Tags", bg: "bg-cyan-300" };
    case "RecursiveTagUnion":
      return { name: "RecTags", bg: "bg-cyan-400" };
    case "RangedNumber":
      return { name: "ℕ", bg: "bg-lime-400" };
    case "EmptyRecord":
      return { name: "{}", bg: "bg-purple-400" };
    case "EmptyTagUnion":
      return { name: "[]", bg: "bg-cyan-200" };
    case "Error":
      return { name: "Error", bg: "bg-red-400" };
  }
}

export const LinkStyles: ContentStyles = { name: "Link", bg: "bg-slate-500" };
