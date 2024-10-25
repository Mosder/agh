from queue import PriorityQueue
from dimacs import loadWeightedGraph
from math import inf
from graphClass import Graph
from tester import Tester

class FindUnionNode:
    def __init__(self, value):
        self.val = value
        self.parent = self
        self.rank = 0
    
def find(x: FindUnionNode):
    if x.parent != x:
        x.parent = find(x.parent)
    return x.parent

def union(x: FindUnionNode, y: FindUnionNode):
    x = find(x)
    y = find(y)
    if x.rank > y.rank:
        y.parent = x
    else:
        x.parent = y
        if x.rank == y.rank:
            y.rank += 1

def printEdges(L):
    for edge in L:
        print(f"{edge[0]}-{edge[1]}, weight: {edge[2]}")
    
def sortEdgesByWeight(L, desc=True):
    return sorted(L,key=lambda x: x[2],reverse=desc)

def findUnionSolution(graph: Graph):
    V, L = graph.data
    L = sortEdgesByWeight(L)
    findUnionNodes = []
    for vertex in range(V):
        findUnionNodes.append(FindUnionNode(vertex))
    for edge in L:
        union(findUnionNodes[edge[0] - 1], findUnionNodes[edge[1] - 1])
        if find(findUnionNodes[0]).val == find(findUnionNodes[1]).val:
            return edge[2]

def edgeListToAdjList(V, L):
    adjList = [[] for _ in range(V)]
    for edge in L:
        adjList[edge[0] - 1].append((edge[1] - 1, edge[2]))
        adjList[edge[1] - 1].append((edge[0] - 1, edge[2]))
    return adjList

def dijkstraSolution(graph: Graph, start=0):
    V, L = graph.data
    adjList = edgeListToAdjList(V, L)
    cheapest = [0] * V
    q = PriorityQueue()
    q.put((-inf, start))
    while not q.empty():
        dist, vertex = q.get()
        dist *= -1
        for u, w in adjList[vertex]:
            mn = min(dist, w)
            if cheapest[u] < mn:
                cheapest[u] = mn
                q.put((-1 * mn, u))
    return cheapest[1]

if __name__ == "__main__":
    fuTester = Tester("graphs/lab1/", loadWeightedGraph, findUnionSolution)
    djTester = Tester("graphs/lab1/", loadWeightedGraph, dijkstraSolution)
    print("FindUnion tester:")
    fuTester.continousTesting()
    print("Dijkstra tester:")
    djTester.continousTesting()