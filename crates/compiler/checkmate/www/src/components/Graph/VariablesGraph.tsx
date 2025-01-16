import ELK, {
  type ElkNode,
  type LayoutOptions,
} from "elkjs/lib/elk.bundled.js";
import ReactFlow, {
  Node,
  Edge,
  Background,
  BackgroundVariant,
  useReactFlow,
  ReactFlowProvider,
  NodeChange,
  applyNodeChanges,
  EdgeChange,
  applyEdgeChanges,
  Panel,
  NodeTypes,
  useStore,
  ReactFlowState,
  Position,
  MarkerType,
  EdgeMarkerType,
} from "reactflow";
import { useCallback, useEffect, useRef, useState } from "react";
import { Variable } from "../../schema";

import "reactflow/dist/style.css";
import clsx from "clsx";
import VariableNode, { VariableNodeProps } from "./VariableNode";
import { SubsSnapshot } from "../../engine/subs";
import { TypedEmitter } from "tiny-typed-emitter";
import EpochCell from "../Common/EpochCell";
import { HashLink } from "react-router-hash-link";
import {
  EventListMessage,
  GraphMessage,
  VariableMessage,
} from "../../utils/events";
import { useFocusOutlineEvent } from "../../hooks/useFocusOutlineEvent";

export interface VariablesGraphProps {
  subs: SubsSnapshot;
  graphEe: TypedEmitter<GraphMessage>;
  eventListEe: TypedEmitter<EventListMessage>;
}

function horizontalityToPositions(isHorizontal: boolean): {
  targetPosition: Position;
  sourcePosition: Position;
} {
  return {
    targetPosition: isHorizontal ? Position.Left : Position.Top,
    sourcePosition: isHorizontal ? Position.Right : Position.Bottom,
  };
}

interface LayoutedElements {
  nodes: Node[];
  edges: Edge[];
}

const LAYOUT_CONFIG_DOWN = {
  keypress: "j",
  emoji: "⬇️",
  elkLayoutOptions: {
    "elk.algorithm": "layered",
    "elk.direction": "DOWN",
  },
  isHorizontal: false,
} as const;
const LAYOUT_CONFIG_RIGHT = {
  keypress: "l",
  emoji: "➡️",
  elkLayoutOptions: {
    "elk.algorithm": "layered",
    "elk.direction": "RIGHT",
  },
  isHorizontal: true,
} as const;
const LAYOUT_CONFIG_RADIAL = {
  keypress: "r",
  emoji: "🌐",
  elkLayoutOptions: {
    "elk.algorithm": "radial",
  },
  isHorizontal: false,
} as const;
const LAYOUT_CONFIG_FORCE = {
  keypress: "f",
  emoji: "🧲",
  elkLayoutOptions: {
    "elk.algorithm": "force",
  },
  isHorizontal: false,
} as const;

type LayoutConfiguration =
  | typeof LAYOUT_CONFIG_DOWN
  | typeof LAYOUT_CONFIG_RIGHT
  | typeof LAYOUT_CONFIG_RADIAL
  | typeof LAYOUT_CONFIG_FORCE;

const LAYOUT_CONFIGURATIONS: LayoutConfiguration[] = [
  LAYOUT_CONFIG_DOWN,
  LAYOUT_CONFIG_RIGHT,
  LAYOUT_CONFIG_RADIAL,
  LAYOUT_CONFIG_FORCE,
];

type ComputeElkLayoutOptions = Pick<
  LayoutConfiguration,
  "elkLayoutOptions" | "isHorizontal"
>;

interface ComputeLayoutedElementsProps
  extends LayoutedElements,
    ComputeElkLayoutOptions {}

// Elk has a *huge* amount of options to configure. To see everything you can
// tweak check out:
//
// - https://www.eclipse.org/elk/reference/algorithms.html
// - https://www.eclipse.org/elk/reference/options.html
const baseElkOptions: LayoutOptions = {
  "elk.layered.spacing.nodeNodeBetweenLayers": "100",
  "elk.spacing.nodeNode": "80",
};

async function computeLayoutedElements({
  nodes,
  edges,
  elkLayoutOptions,
  isHorizontal,
}: ComputeLayoutedElementsProps): Promise<LayoutedElements> {
  if (nodes.length === 0) {
    return Promise.resolve({
      nodes: [],
      edges: [],
    });
  }

  const elk = new ELK();
  const graph: ElkNode = {
    id: "root",
    layoutOptions: {
      ...baseElkOptions,
      ...elkLayoutOptions,
    },
    //@ts-ignore
    children: nodes.map((node) => ({
      ...node,
      // Adjust the target and source handle positions based on the layout
      // direction.
      targetPosition: isHorizontal ? "left" : "top",
      sourcePosition: isHorizontal ? "right" : "bottom",

      // Hardcode a width and height for elk to use when layouting.
      //width: 150,
      //height: 50,
    })),
    //@ts-ignore
    edges: edges.map((edge) => ({
      ...edge,
    })),
  };

  const layoutedGraph = await elk.layout(graph);

  if (!layoutedGraph.children || !layoutedGraph.edges) {
    throw new Error("Elk did not return a valid graph");
  }

  return {
    //@ts-ignore
    nodes: layoutedGraph.children.map((node) => ({
      ...node,
      // React Flow expects a position property on the node instead of `x`
      // and `y` fields.
      position: { x: node.x, y: node.y },
    })),
    //@ts-ignore
    edges: layoutedGraph.edges,
  };
}

const NODE_TYPES: NodeTypes = {
  variable: VariableNode,
};

type VariableNodeData = VariableNodeProps["data"];
type RFVariableNode = Node<VariableNodeData>;

function newVariable(
  id: string,
  data: VariableNodeData,
  isHorizontal: boolean
): RFVariableNode {
  return {
    id,
    position: { x: 0, y: 0 },
    type: "variable",
    data,
    ...horizontalityToPositions(isHorizontal),
  };
}

function canAddVariable(variableName: string, existingNodes: Node[]): boolean {
  return !existingNodes.some((n) => n.id === variableName);
}

function canAddEdge(edgeName: string, existingEdges: Edge[]): boolean {
  return !existingEdges.some((e) => e.id === edgeName);
}

function addNode(node: Node): NodeChange {
  return {
    type: "add",
    item: node,
  };
}

function addEdge(edge: Edge): EdgeChange {
  return {
    type: "add",
    item: edge,
  };
}

// Auto-layout logic due in part to the `feldera/dbsp` project, licensed under
// the MIT license.
//
// The source code for the original project can be found at
//   https://github.com/feldera/dbsp/blob/585a1926a6d3a0f8176dc80db5e906ec7d095400/web-ui/src/streaming/builder/hooks/useAutoLayout.ts#L215
// and its license at
//   https://github.com/feldera/dbsp/blob/585a1926a6d3a0f8176dc80db5e906ec7d095400/LICENSE
const nodeCountSelector = (state: ReactFlowState) =>
  state.nodeInternals.size + state.edges.length;
const nodesSetInViewSelector = (state: ReactFlowState) =>
  Array.from(state.nodeInternals.values()).every(
    (node) => node.width && node.height
  );

type RedoLayoutFn = () => Promise<void>;
function useRedoLayout(options: ComputeElkLayoutOptions): RedoLayoutFn {
  const nodeCount = useStore(nodeCountSelector);
  const nodesInitialized = useStore(nodesSetInViewSelector);
  const { getNodes, setNodes, getEdges } = useReactFlow();
  const instance = useReactFlow();

  return useCallback(async () => {
    if (!nodeCount || !nodesInitialized) {
      return;
    }
    const { nodes } = await computeLayoutedElements({
      nodes: getNodes(),
      edges: getEdges(),
      ...options,
    });
    setNodes(nodes);
    window.requestAnimationFrame(() => {
      instance.fitView();
    });
  }, [
    nodeCount,
    nodesInitialized,
    getNodes,
    getEdges,
    options,
    setNodes,
    instance,
  ]);
}

// Does positioning of the nodes in the graph.
function useAutoLayout(options: ComputeElkLayoutOptions) {
  const redoLayout = useRedoLayout(options);

  useEffect(() => {
    // This wrapping is of course redundant, but exercised for the purpose of
    // explicitness.
    async function inner() {
      await redoLayout();
    }
    inner();
  }, [redoLayout]);
}

function useKeydown({
  layoutConfig,
  setLayoutConfig,
  graphEe,
}: {
  layoutConfig: LayoutConfiguration;
  setLayoutConfig: React.Dispatch<React.SetStateAction<LayoutConfiguration>>;
  graphEe: TypedEmitter<GraphMessage>;
}) {
  const redoLayout = useRedoLayout(layoutConfig);

  const keyDownHandler = useCallback(
    async (key: string) => {
      switch (key) {
        case "c": {
          await redoLayout();
          return;
        }
        default: {
          break;
        }
      }

      for (const config of LAYOUT_CONFIGURATIONS) {
        if (key === config.keypress) {
          setLayoutConfig(config);
          return;
        }
      }
    },
    [redoLayout, setLayoutConfig]
  );
  graphEe.on("keydown", async (key) => await keyDownHandler(key));
}

function Graph({
  subs,
  graphEe,
  eventListEe,
}: VariablesGraphProps): JSX.Element {
  const instance = useReactFlow();

  // We need to reset the graph when the subs snapshot changes. I'm not sure
  // why this isn't done by the existing state manager.
  useEffect(() => {
    instance.setNodes([]);
    instance.setEdges([]);
  }, [instance, subs.epoch]);

  const varEe = useRef(new TypedEmitter<VariableMessage>());
  // Allow an unbounded number of listeners since we attach a listener for each
  // variable.
  varEe.current.setMaxListeners(Infinity);

  const isOutlined = useFocusOutlineEvent({
    ee: graphEe,
    value: subs.epoch,
    event: "focusEpoch",
  });

  const [layoutConfig, setLayoutConfig] =
    useState<LayoutConfiguration>(LAYOUT_CONFIG_DOWN);

  const [elements, setElements] = useState<LayoutedElements>({
    nodes: [],
    edges: [],
  });

  const [variablesNeedingFocus, setVariablesNeedingFocus] = useState<
    Set<Variable>
  >(new Set());

  useEffect(() => {
    if (variablesNeedingFocus.size === 0) {
      return;
    }
    for (const variable of variablesNeedingFocus) {
      varEe.current.emit("focus", variable);
    }
    setVariablesNeedingFocus(new Set());
  }, [variablesNeedingFocus]);

  useAutoLayout(layoutConfig);
  useKeydown({
    layoutConfig,
    setLayoutConfig,
    graphEe,
  });

  const onNodesChange = useCallback((changes: NodeChange[]) => {
    setElements(({ nodes, edges }) => {
      return {
        nodes: applyNodeChanges(changes, nodes),
        edges,
      };
    });
  }, []);

  const onEdgesChange = useCallback((changes: EdgeChange[]) => {
    setElements(({ nodes, edges }) => {
      return {
        nodes,
        edges: applyEdgeChanges(changes, edges),
      };
    });
  }, []);

  interface AddNewVariableParams {
    from?: Variable;
    variable: Variable;
  }

  const addNewVariable = useCallback(
    ({ from, variable }: AddNewVariableParams) => {
      const variablesToFocus = new Set<Variable>();

      setElements(({ nodes, edges }) => {
        let fromVariable: Variable | undefined = from;
        let toVariable: Variable | undefined = variable;

        const nodeChanges: NodeChange[] = [];
        const edgeChanges: EdgeChange[] = [];

        while (toVariable !== undefined) {
          const toVariableName = toVariable.to_string();
          if (canAddVariable(toVariableName, nodes)) {
            const newVariableNode = newVariable(
              toVariable.to_string(),
              {
                subs,
                rawVariable: toVariable,
                addSubVariableLink: addNewVariable,
                isOutlined: true,
                ee: varEe.current,
              },
              layoutConfig.isHorizontal
            );

            nodeChanges.push(addNode(newVariableNode));
          }

          if (fromVariable !== undefined) {
            const edgeName = `${fromVariable}->${toVariable}`;
            if (canAddEdge(edgeName, edges)) {
              let markerEnd: EdgeMarkerType | undefined;
              if (subs.get_root_key(fromVariable) === toVariable) {
                markerEnd = {
                  type: MarkerType.ArrowClosed,
                  width: 20,
                  height: 20,
                };
              }

              const newEdge = addEdge({
                id: `${fromVariable}->${toVariable}`,
                source: fromVariable.to_string(),
                target: toVariableName,
                markerEnd,
              });

              edgeChanges.push(newEdge);
            }
          }

          variablesToFocus.add(toVariable);

          fromVariable = toVariable;
          const rootToVariable = subs.get_root_key(toVariable);
          if (toVariable !== rootToVariable) {
            toVariable = rootToVariable;
          } else {
            toVariable = undefined;
          }
        }

        const newNodes = applyNodeChanges(nodeChanges, nodes);
        const newEdges = applyEdgeChanges(edgeChanges, edges);

        return { nodes: newNodes, edges: newEdges };
      });

      setVariablesNeedingFocus(variablesToFocus);
    },
    [layoutConfig.isHorizontal, subs]
  );

  const addNewVariableNode = useCallback(
    (variable: Variable) => {
      addNewVariable({ variable });
    },
    [addNewVariable]
  );

  graphEe.on("focusVariable", addNewVariableNode);

  return (
    <ReactFlow
      nodes={elements.nodes}
      edges={elements.edges}
      onNodesChange={(e) => onNodesChange(e)}
      onEdgesChange={(e) => onEdgesChange(e)}
      fitView
      nodesDraggable
      nodesConnectable={false}
      nodeTypes={NODE_TYPES}
      proOptions={{
        // https://reactflow.dev/docs/guides/remove-attribution/
        hideAttribution: true,
      }}
      className={clsx(
        "ring-inset rounded-md transition ease-in-out duration-700",
        isOutlined && "ring-2 ring-blue-500"
      )}
    >
      <Panel position="top-left">
        <HashLink
          smooth
          to={`#events-${subs.epoch}`}
          onClick={() => {
            eventListEe.emit("focusEpoch", subs.epoch);
          }}
        >
          <EpochCell>Epoch {subs.epoch}</EpochCell>
        </HashLink>
      </Panel>
      <Panel position="top-right">
        <LayoutPanel
          layoutConfig={layoutConfig}
          setLayoutConfig={setLayoutConfig}
        />
      </Panel>

      <Background variant={BackgroundVariant.Dots} />
    </ReactFlow>
  );
}

interface LayoutPanelProps {
  layoutConfig: LayoutConfiguration;
  setLayoutConfig: React.Dispatch<React.SetStateAction<LayoutConfiguration>>;
}

function LayoutPanel({
  layoutConfig,
  setLayoutConfig,
}: LayoutPanelProps): JSX.Element {
  const commonStyle = "rounded cursor-pointer text-2xl select-none";

  return (
    <>
      {LAYOUT_CONFIGURATIONS.map((config, i) => (
        <span
          key={i}
          className={clsx(
            commonStyle,
            i !== 0 ? "ml-2" : "",
            config !== layoutConfig ? "opacity-50" : ""
          )}
          onClick={() => {
            setLayoutConfig(config);
          }}
        >
          {config.emoji}
        </span>
      ))}
    </>
  );
}

export default function VariablesGraph({
  subs,
  graphEe,
  eventListEe,
}: VariablesGraphProps) {
  return (
    <ReactFlowProvider>
      <Graph subs={subs} graphEe={graphEe} eventListEe={eventListEe} />
    </ReactFlowProvider>
  );
}
