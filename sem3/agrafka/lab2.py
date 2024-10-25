from dimacs import loadDirectedWeightedGraph
from math import inf
from collections import deque
from tester import Tester
from graphClass import Graph

def edgeListToAdjMatrix(vertexAmount, edgeList, directed=True):
    adjMatrix = [[0 for _ in range(vertexAmount)] for _ in range(vertexAmount)]
    adjVertices = [set() for _ in range(vertexAmount)]
    for v, u, w in edgeList:
        v -= 1
        u -= 1
        adjMatrix[v][u] = w
        if not directed:
            adjMatrix[u][v] = w
        adjVertices[v].add(u)
        adjVertices[u].add(v)
    return adjMatrix, adjVertices

def getExtPathDFS(adjMatrix, adjVertices, startNode, endNode):
    parents = [None for _ in range(len(adjMatrix))]
    visited = [False for _ in range(len(adjMatrix))]

    q = deque()
    q.append((startNode, None))
    while len(q) > 0:
        v, vParent = q.pop()
        parents[v] = vParent
        visited[v] = True
        for u in adjVertices[v]:
            w = adjMatrix[v][u]
            if w != 0 and not visited[u]:
                if u == endNode:
                    parents[u] = v
                    return parents
                q.append((u, v))
    return None

def getExtPathBFS(adjMatrix, adjVertices, startNode, endNode):
    parents = [None for _ in range(len(adjMatrix))]
    visited = [False for _ in range(len(adjMatrix))]
    visited[startNode] = True

    q = deque()
    q.append(startNode)
    while len(q) > 0:
        v = q.popleft()
        for u in adjVertices[v]:
            w = adjMatrix[v][u]
            if w != 0 and not visited[u]:
                parents[u] = v
                if u == endNode:
                    return parents
                visited[u] = True
                q.append(u)
    return None

def getMaxFlow(graph: Graph, dfs=False, startNode=0, endNode=None):
    adjMatrix, adjVertices = graph.data
    if endNode is None: endNode = len(adjMatrix) - 1
    if dfs:
        pathParents = getExtPathDFS(adjMatrix, adjVertices, startNode, endNode)
    else:
        pathParents = getExtPathBFS(adjMatrix, adjVertices, startNode, endNode)
    maxFlow = 0
    while pathParents is not None:
        pathFlow = inf
        v = endNode
        while v != startNode:
            pathFlow = min(pathFlow, adjMatrix[pathParents[v]][v])
            v = pathParents[v]
        v = endNode
        maxFlow += pathFlow
        while v != startNode:
            u = pathParents[v]
            adjMatrix[u][v] -= pathFlow
            adjMatrix[v][u] += pathFlow
            v = u
        if dfs:
            pathParents = getExtPathDFS(adjMatrix, adjVertices, startNode, endNode)
        else:
            pathParents = getExtPathBFS(adjMatrix, adjVertices, startNode, endNode)
    return maxFlow

def loadGraph(graphPath):
    return edgeListToAdjMatrix(*loadDirectedWeightedGraph(graphPath))

if __name__ == "__main__":
    tester = Tester("graphs/lab2/flow/", loadGraph, getMaxFlow)
    tester.continousTesting()