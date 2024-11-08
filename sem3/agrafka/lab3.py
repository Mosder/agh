from time import time
from queue import PriorityQueue
from typing import List
from dimacs import loadWeightedGraph
from graphClass import Graph
from lab2 import getMaxFlow, edgeListToAdjMatrix
from math import inf
from tester import Tester

# Ford-Fulkerson (bad)
def getEdgeConnectivity(graph: Graph):
    fileLoadData = loadWeightedGraph(graph.dir + graph.name)
    vAmount = len(graph.data[0])
    edgeConnectivity = inf
    for i in range(1, vAmount):
        edgeConnectivity = min(edgeConnectivity, getMaxFlow(graph, endNode=i))
        graph.data = edgeListToAdjMatrix(*fileLoadData, directed=False)
    return edgeConnectivity

def loadGraph(graphPath):
    return edgeListToAdjMatrix(*loadWeightedGraph(graphPath), directed=False)

# Stoer-Wagner (good)
class Vertex:
    def __init__(self):
        self.edges = {}
        self.merged = False
    
    def addEdge(self, edge, weight):
        self.edges[edge] = self.edges.get(edge, 0) + weight
    
    def delEdge(self, edge):
        del self.edges[edge]
    
    def deactivateVertex(self):
        self.merged = True


def mergeVertices(graph: List[Vertex], v, u):
    if u in graph[v].edges:
        graph[v].delEdge(u)
    if v in graph[u].edges:
        graph[u].delEdge(v)
    for vertex, weight in graph[u].edges.items():
        graph[v].addEdge(vertex, weight)
    graph[u].deactivateVertex()

def minimumCutPhase(graph: List[Vertex], startingVertex=0):
    S = [startingVertex]
    weightSumToS = [0 for _ in range(len(graph))]
    for v, w in graph[startingVertex].edges.items():
        if not graph[v].merged:
            weightSumToS[v] += w
    
    q = PriorityQueue()
    for vertex, weightSum in enumerate(weightSumToS):
        if not graph[vertex].merged:
            q.put((-weightSum, vertex))
    
    while not q.empty():
        w, v = q.get()
        w = abs(w)
        if not v in S and not graph[v].merged:
            S.append(v)
            for u, weight in graph[v].edges.items():
                if not graph[u].merged:
                    newWeight = weightSumToS[u] + weight
                    weightSumToS[u] = newWeight
                    q.put((-newWeight, u))
    
    potentialSolution = 0
    for v, w in graph[S[-1]].edges.items():
        if not graph[v].merged:
            potentialSolution += w
    
    mergeVertices(graph, S[-1], S[-2])
    return potentialSolution


def stoerWagner(graph: Graph):
    graphData = graph.data
    vertexAmount = len(graphData)
    solution = inf
    for _ in range(vertexAmount - 1):
        solution = min(solution, minimumCutPhase(graphData))
    return solution

def loadGraph2(graphPath):
    V, L = loadWeightedGraph(graphPath)
    graph = [Vertex() for _ in range(V)]
    for v, u, w in L:
        graph[v-1].addEdge(u-1, w)
        graph[u-1].addEdge(v-1, w)
    return graph

if __name__ == "__main__":
    # tester = Tester("graphs/lab3/", loadGraph, getEdgeConnectivity)
    tester = Tester("graphs/lab3/", loadGraph2, stoerWagner)
    tester.continousTesting()