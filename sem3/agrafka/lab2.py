from dimacs import loadDirectedWeightedGraph, readSolution
from math import inf
from os.path import isfile
from os import listdir
from collections import deque

def edgeListToAdjMatrix(vertexAmount, edgeList):
    adjMatrix = [[0 for _ in range(vertexAmount)] for _ in range(vertexAmount)]
    adjVertices = [set() for _ in range(vertexAmount)]
    for v, u, w in edgeList:
        v -= 1
        u -= 1
        adjMatrix[v][u] = w
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

def getMaxFlow(vertexAmount, edgeList, startNode=0, endNode=None, dfs=False):
    if endNode is None: endNode = vertexAmount - 1
    adjMatrix, adjVertices = edgeListToAdjMatrix(vertexAmount, edgeList)
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

def testGraph(graphName, dfs=False):
    graph = "graphs/lab2/flow/" + graphName
    if isfile(graph):
        vAmount, edgeList = loadDirectedWeightedGraph(graph)
        sol = int(readSolution(graph))
        res = getMaxFlow(vAmount, edgeList, dfs=dfs)
        print(f"Solution: {sol}")
        print(f"Result: {res} ({'ok' if sol == res else 'wrong'})")
    else:
        print("Graph doesn't exist")

inp = input("Graph name / all (:q to quit): ")
while inp != ":q":
    if inp == "all":
        for graph in listdir("graphs/lab2/flow"):
            print(graph)
            testGraph(graph)
    else:
        testGraph(inp)
    inp = input("Graph name (:q to quit): ")
