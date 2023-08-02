export interface GroupInfo {
  group: string;
  groupHover: string;
}

export function depthToGroupInfo(depth: number): GroupInfo {
  switch (depth) {
    case 0:
      return {
        group: `group/event-0`,
        groupHover: `group-hover/event-0:opacity-100`,
      };
    case 1:
      return {
        group: `group/event-1`,
        groupHover: `group-hover/event-1:opacity-100`,
      };
    case 2:
      return {
        group: `group/event-2`,
        groupHover: `group-hover/event-2:opacity-100`,
      };
    case 3:
      return {
        group: `group/event-3`,
        groupHover: `group-hover/event-3:opacity-100`,
      };
    case 4:
      return {
        group: `group/event-4`,
        groupHover: `group-hover/event-4:opacity-100`,
      };
    case 5:
      return {
        group: `group/event-5`,
        groupHover: `group-hover/event-5:opacity-100`,
      };
    case 6:
      return {
        group: `group/event-6`,
        groupHover: `group-hover/event-6:opacity-100`,
      };
    case 7:
      return {
        group: `group/event-7`,
        groupHover: `group-hover/event-7:opacity-100`,
      };
    case 8:
      return {
        group: `group/event-8`,
        groupHover: `group-hover/event-8:opacity-100`,
      };
    case 9:
      return {
        group: `group/event-9`,
        groupHover: `group-hover/event-9:opacity-100`,
      };
    case 10:
      return {
        group: `group/event-10`,
        groupHover: `group-hover/event-10:opacity-100`,
      };
    case 11:
      return {
        group: `group/event-11`,
        groupHover: `group-hover/event-11:opacity-100`,
      };
    case 12:
      return {
        group: `group/event-12`,
        groupHover: `group-hover/event-12:opacity-100`,
      };
    default:
      throw new Error(`Too deep: ${depth}`);
  }
}
