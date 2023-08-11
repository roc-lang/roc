import { useEffect, useState } from "react";
import type { TypedEmitter } from "tiny-typed-emitter";

type Events<Name extends string, T> = {
  [K in Name]: (value: T) => void;
};

interface UseFocusOutlineEventProps<Name extends string, T> {
  value: T;
  ee: TypedEmitter<Events<Name, T>>;
  event: Name;
  defaultIsOutlined?: boolean;
}

export function useFocusOutlineEvent<Name extends string, T>({
  value,
  ee,
  event,
  defaultIsOutlined = false,
}: UseFocusOutlineEventProps<Name, T>) {
  const [isOutlined, setIsOutlined] = useState(defaultIsOutlined);

  useEffect(() => {
    ee.on(event, (focusValue: T) => {
      if (focusValue !== value) return;
      setIsOutlined(true);
    });
  }, [ee, event, value]);

  useEffect(() => {
    if (!isOutlined) return;
    const timer = setTimeout(() => {
      setIsOutlined(false);
    }, 500);

    return () => {
      clearTimeout(timer);
    };
  }, [isOutlined]);

  return isOutlined;
}
