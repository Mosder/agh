# Maciej Borowiec
# 
# Algorytm sortuje tablicę krawędzi w zależności od pułapu
# Następnie wykonuje algorytm BFS dla każdej takiej podtablicy,
#   po której na pewno może przelecieć po każdej krawędzi nie zmieniając pułapu,
#   tzn.: pułap ostatniego elementu - pułap pierwszego <= 2 * t
# Podtablice są przedstawiane jako lista adjacencji, do której dodaję i usuwam krawędzi, gdy zmienia się podtablica
# Algorytm znajduje te podtablice przesuwając indeks startowy i końcowy podtablicy po posortowanej tablicy krawędzi
# Jeśli w którejkolwiek z podtablic algorytm BFS dojdzie od wierzchołka x do wierzchołka y, to wtedy jest możliwy taki przelot
# W przeciwnym wypadku taki przelot nie jest możliwy
# 
# Złożoność czasowa: O(E*(V+E))

from zad4testy import runtests
from collections import deque

def sortByHeight(edge):
    return edge[2]

def findNewStartAndEnd(L, adjList, start, end, t):
    n = len(L)
    end += 1
    if end == n: return start, end
    while L[end][2] - L[start][2] > 2*t:
        edge = L[start]
        adjList[edge[0]].pop(0)
        adjList[edge[1]].pop(0)
        start += 1
    while end < n and L[end][2] - L[start][2] <= 2*t:
        edge = L[end]
        adjList[edge[0]].append(edge[1])
        adjList[edge[1]].append(edge[0])
        end += 1
    return start, end-1

def bfs(adjList, vAmount, x, y):
    visited = [False for _ in range(vAmount)]
    q = deque()
    q.append(x)
    while len(q) > 0:
        v = q.popleft()
        if v == y: return True
        visited[v] = True
        for u in adjList[v]:
            if not visited[u]: q.append(u)
    return False

def Flight(L,x,y,t):
    vAmount = -1
    for edge in L: vAmount = max(vAmount, edge[0], edge[1]) + 1
    n = len(L)
    adjList = [[] for _ in range(vAmount)]
    L = sorted(L, key=sortByHeight)
    start, end = 0, -1
    while end < n-1:
        start, end = findNewStartAndEnd(L, adjList, start, end, t)
        # Specjalny warunek, by nie wykonywać BFS'a, gdy x lub y nie mają sąsiadów
        if len(adjList[x]) > 0 and len(adjList[y]) > 0:
            if bfs(adjList, vAmount, x, y): return True
    return False

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( Flight, all_tests = True )
