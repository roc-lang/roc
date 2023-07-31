import Dagre from "@dagrejs/dagre";
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

type GraphDirection = "TB" | "BT" | "LR" | "RL";

const DEFAULT_DIRECTION: GraphDirection = "TB";

interface LayoutedElements {
  nodes: Node[];
  edges: Edge[];
}

interface ComputeLayoutedElementsProps extends LayoutedElements {
  direction: GraphDirection;
}

function computeLayoutedElements({
  nodes,
  edges,
  direction,
}: ComputeLayoutedElementsProps): LayoutedElements {
  if (nodes.length === 0) {
    return {
      nodes: [],
      edges: [],
    };
  }

  const g = new Dagre.graphlib.Graph().setDefaultEdgeLabel(() => ({}));
  g.setGraph({ rankdir: direction });

  edges.forEach((edge) => g.setEdge(edge.source, edge.target));
  nodes.forEach((node) => g.setNode(node.id, node));

  Dagre.layout(g);

  const result = {
    nodes: nodes.map((node) => {
      const { x, y } = g.node(node.id);

      return { ...node, position: { x, y } };
    }),
    edges,
  };
  return result;
}

const NODE_TYPES: NodeTypes = {
  variable: VariableNode,
};

function newVariable(id: string, data: VariableNodeProps["data"]): Node {
  return {
    id,
    position: { x: 0, y: 0 },
    type: "variable",
    data,
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

// Does positioning of the nodes in the graph.
function useRedoLayout({ direction }: { direction: GraphDirection }) {
  const nodeCount = useStore(nodeCountSelector);
  const nodesInitialized = useStore(nodesSetInViewSelector);
  const { getNodes, setNodes, getEdges } = useReactFlow();
  const instance = useReactFlow();

  return useCallback(() => {
    if (!nodeCount || !nodesInitialized) {
      return;
    }
    const { nodes } = computeLayoutedElements({
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
    redoLayout();
  }, [direction, redoLayout]);
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

  const redoLayout = useRedoLayout({ direction });
  useAutoLayout({ direction });

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
          newVariable(to, {
            subs,
            variable: subLinkN,
            addSubVariableLink,
          }),
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
    [subs]
  );

  const addNode = useCallback(
    (variableN: Variable) => {
      variableN = subs.get_root_key(variableN);
      const variable = variableN.toString();

      setElements(({ nodes, edges }) => {
        const optNewNode = addNodeChange(
          newVariable(variable, {
            subs,
            variable: variableN,
            addSubVariableLink,
          }),
          nodes
        );
        const newNodes = optNewNode
          ? applyNodeChanges([optNewNode], nodes)
          : nodes;

        return { nodes: newNodes, edges: edges };
      });
    },
    [subs, addSubVariableLink]
  );

  onVariable(addNode);
  onKeydown((key) => {
    switch (key) {
      case "c": {
        redoLayout();
      }
    }
  });

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
    { dir: "TB", text: "⬇️" },
    { dir: "LR", text: "➡️" },
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
