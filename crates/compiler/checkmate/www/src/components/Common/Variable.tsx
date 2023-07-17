import clsx from "clsx";
import { Variable } from "../../schema";
import { ContentStyles } from "./../Content";

export function VariableElHelp({
  variable,
  styles,
  onClick,
}: {
  variable: Variable;
  styles: ContentStyles;
  onClick?: () => void;
}): JSX.Element {
  const { name, bg } = styles;
  return (
    <span className={clsx("py-0 pl-0 pr-1 rounded-md", bg)}>
      <span
        className="ring-1 ring-inset ring-black-100 mr-1 px-1 bg-white rounded-md cursor"
        onClick={(e) => {
          e.stopPropagation();
          onClick?.();
        }}
      >
        {variable}
      </span>
      {name}
    </span>
  );
}
