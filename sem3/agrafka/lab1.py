from queue import PriorityQueue
from dimacs import loadWeightedGraph, readSolution
from math import inf
from os.path import isfile

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

def findUnionSolution(V, L):
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

def dijkstraSolution(V, L, start):
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

graph = "graphs/lab1/" + input("Graph name: ")
if isfile(graph):
    V, L = loadWeightedGraph(graph)
    exp = int(readSolution(graph))
    fu = findUnionSolution(V, L)
    dj = dijkstraSolution(V, L, 0)
    print(f"Expected solution: {exp}")
    print(f"Find union solution: {fu} ({'ok' if fu == exp else 'wrong'})")
    print(f"Dijkstra solution: {dj} ({'ok' if dj == exp else 'wrong'})")
else:
    print("Graph not found")
