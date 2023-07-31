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
} from "reactflow";
import { useCallback, useEffect, useState } from "react";
import { Variable } from "../../schema";

import "reactflow/dist/style.css";
import clsx from "clsx";
import VariableNode, { VariableNodeProps } from "./VariableNode";
import { SubsSnapshot } from "../../engine/subs";
import { KeydownHandler } from "../Events";

export interface VariablesGraphProps {
  subs: SubsSnapshot;
  onVariable: (handler: (variable: Variable) => void) => void;
  onKeydown: (handler: KeydownHandler) => void;
}

enum GraphDirection {
  LeftRight,
  TopBottom,
}

const DEFAULT_DIRECTION: GraphDirection = GraphDirection.TopBottom;

function directionToElkDirection(direction: GraphDirection): string {
  switch (direction) {
    case GraphDirection.TopBottom:
      return "DOWN";
    case GraphDirection.LeftRight:
      return "RIGHT";
  }
}

function horizontalDirectionality(direction: GraphDirection): boolean {
  switch (direction) {
    case GraphDirection.TopBottom:
      return false;
    case GraphDirection.LeftRight:
      return true;
  }
}

function directionalityToPositions(direction: GraphDirection): {
  targetPosition: Position;
  sourcePosition: Position;
} {
  const isHorizontal = horizontalDirectionality(direction);
  return {
    targetPosition: isHorizontal ? Position.Left : Position.Top,
    sourcePosition: isHorizontal ? Position.Right : Position.Bottom,
  };
}

interface LayoutedElements {
  nodes: Node[];
  edges: Edge[];
}

interface ComputeLayoutedElementsProps extends LayoutedElements {
  direction: GraphDirection;
}

// Elk has a *huge* amount of options to configure. To see everything you can
// tweak check out:
//
// - https://www.eclipse.org/elk/reference/algorithms.html
// - https://www.eclipse.org/elk/reference/options.html
const elkOptions: LayoutOptions = {
  "elk.algorithm": "layered",
  "elk.layered.spacing.nodeNodeBetweenLayers": "100",
  "elk.spacing.nodeNode": "80",
};

async function computeLayoutedElements({
  nodes,
  edges,
  direction,
}: ComputeLayoutedElementsProps): Promise<LayoutedElements> {
  if (nodes.length === 0) {
    return Promise.resolve({
      nodes: [],
      edges: [],
    });
  }

  const isHorizontal = horizontalDirectionality(direction);

  const elk = new ELK();
  const graph: ElkNode = {
    id: "root",
    layoutOptions: {
      ...elkOptions,
      "elk.direction": directionToElkDirection(direction),
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
    edges: edges,
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

function newVariable(
  id: string,
  data: VariableNodeProps["data"],
  direction: GraphDirection
): Node {
  return {
    id,
    position: { x: 0, y: 0 },
    type: "variable",
    data,
    ...directionalityToPositions(direction),
  };
}

function addNodeChange(node: Node, existingNodes: Node[]): NodeChange | null {
  if (existingNodes.some((n) => n.id === node.id)) {
    return null;
  }
  return {
    type: "add",
    item: node,
  };
}

function addEdgeChange(edge: Edge, existingEdges: Edge[]): EdgeChange | null {
  if (existingEdges.some((e) => e.id === edge.id)) {
    return null;
  }
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
function useRedoLayout({
  direction,
}: {
  direction: GraphDirection;
}): RedoLayoutFn {
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
      direction,
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
    direction,
    setNodes,
    instance,
  ]);
}

// Does positioning of the nodes in the graph.
function useAutoLayout({ direction }: { direction: GraphDirection }) {
  const redoLayout = useRedoLayout({ direction });

  useEffect(() => {
    // This wrapping is of course redundant, but exercised for the purpose of
    // explicitness.
    async function inner() {
      await redoLayout();
    }
    inner();
  }, [direction, redoLayout]);
}

function useKeydown({
  direction,
  onKeydown,
  setDirection,
}: {
  direction: GraphDirection;
  setDirection: React.Dispatch<React.SetStateAction<GraphDirection>>;
  onKeydown: (handler: KeydownHandler) => void;
}) {
  const redoLayout = useRedoLayout({ direction });

  const keyDownHandler = useCallback(
    async (key: string) => {
      switch (key) {
        case "c": {
          await redoLayout();
          break;
        }
        case "j": {
          setDirection(GraphDirection.TopBottom);
          break;
        }
        case "l": {
          setDirection(GraphDirection.LeftRight);
          break;
        }
      }
    },
    [redoLayout, setDirection]
  );
  onKeydown(async (key) => {
    await keyDownHandler(key);
  });
}

function Graph({
  subs,
  onVariable,
  onKeydown,
}: VariablesGraphProps): JSX.Element {
  const initialNodes: Node[] = [];
  const initialEdges: Edge[] = [];

  const [direction, setDirection] = useState(DEFAULT_DIRECTION);
  const [elements, setElements] = useState<LayoutedElements>({
    nodes: initialNodes,
    edges: initialEdges,
  });

  useAutoLayout({ direction });
  useKeydown({ direction, onKeydown, setDirection });

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

  const addSubVariableLink = useCallback(
    (fromN: Variable, subLinkN: Variable) => {
      fromN = subs.get_root_key(fromN);
      subLinkN = subs.get_root_key(subLinkN);
      const from = fromN.toString();
      const to = subLinkN.toString();

      setElements(({ nodes, edges }) => {
        const optNewNode = addNodeChange(
          newVariable(
            to,
            {
              subs,
              variable: subLinkN,
              addSubVariableLink,
            },
            direction
          ),
          nodes
        );
        const newNodes = optNewNode
          ? applyNodeChanges([optNewNode], nodes)
          : nodes;

        const optNewEdge = addEdgeChange(
          { id: `${from}->${to}`, source: from, target: to },
          edges
        );
        const newEdges = optNewEdge
          ? applyEdgeChanges([optNewEdge], edges)
          : edges;

        return { nodes: newNodes, edges: newEdges };
      });
    },
    [direction, subs]
  );

  const addNode = useCallback(
    (variableN: Variable) => {
      variableN = subs.get_root_key(variableN);
      const variable = variableN.toString();

      setElements(({ nodes, edges }) => {
        const optNewNode = addNodeChange(
          newVariable(
            variable,
            {
              subs,
              variable: variableN,
              addSubVariableLink,
            },
            direction
          ),
          nodes
        );
        const newNodes = optNewNode
          ? applyNodeChanges([optNewNode], nodes)
          : nodes;

        return { nodes: newNodes, edges: edges };
      });
    },
    [subs, addSubVariableLink, direction]
  );

  onVariable(addNode);

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
    >
      <Panel position="top-right">
        <DirectionPanel
          direction={direction}
          onChange={(e) => setDirection(e)}
        />
      </Panel>

      <Background variant={BackgroundVariant.Dots} />
    </ReactFlow>
  );
}

interface DirectionPanelProps {
  direction: GraphDirection;
  onChange: (direction: GraphDirection) => void;
}

function DirectionPanel({
  direction,
  onChange,
}: DirectionPanelProps): JSX.Element {
  const commonStyle = "rounded cursor-pointer text-2xl select-none";

  const dirs: { dir: GraphDirection; text: string }[] = [
    { dir: GraphDirection.TopBottom, text: "⬇️" },
    { dir: GraphDirection.LeftRight, text: "➡️" },
  ];

  return (
    <>
      {dirs.map(({ dir, text }, i) => (
        <span
          key={i}
          className={clsx(
            commonStyle,
            i !== 0 ? "ml-2" : "",
            dir !== direction ? "opacity-50" : ""
          )}
          onClick={() => {
            onChange(dir);
          }}
        >
          {text}
        </span>
      ))}
    </>
  );
}

export default function VariablesGraph(props: VariablesGraphProps) {
  return (
    <ReactFlowProvider>
      <Graph {...props} />
    </ReactFlowProvider>
  );
}
