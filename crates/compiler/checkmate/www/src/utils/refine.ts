export type Refine<T extends { type: string }, Type extends string> = T & {
  type: Type;
};
