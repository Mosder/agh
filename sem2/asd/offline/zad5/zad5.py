# Maciej Borowiec
#
# Algorytm modyfikuje graf poprzez złączenie wszystkich osobliwości w jeden nowy wierzchołek
# Ten wierzchołek posiada wszystkie połączenia z wszystkich osobliwości
# Na tak zmodyfikowanym grafie odpala algorytm dijkstry od wierzchołka a i zwraca odległość do wierzchołka b
#
# Złożoność czasowa: O(ElogV)

from zad5testy import runtests
from math import inf
from queue import PriorityQueue

def dijkstra(adjList, n, start):
    distances = [inf for _ in range(n)]
    distances[start] = 0
    q = PriorityQueue()
    q.put((0, start))
    while not q.empty():
        v = q.get()[1]
        for u in adjList[v]:
            newDist = distances[v] + u[1]
            if distances[u[0]] > newDist:
                distances[u[0]] = newDist
                q.put((newDist, u[0]))
    return distances

def spacetravel( n, E, S, a, b ):
    adjList = [[] for _ in range(n+1)]
    for teleport in S:
        adjList[teleport] = None
    if adjList[a] == None: a = n
    if adjList[b] == None: b = n
    for edge in E:
        v1 = edge[0] if adjList[edge[0]] != None else n
        v2 = edge[1] if adjList[edge[1]] != None else n
        if v1 != v2:
            adjList[v1].append((v2, edge[2]))
            adjList[v2].append((v1, edge[2]))
    result = dijkstra(adjList, n+1, a)[b]
    return None if result == inf else result

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( spacetravel, all_tests = True )
