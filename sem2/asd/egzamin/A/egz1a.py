# Maciej Borowiec
#
# Program "puszcza" 2 razy algorytm dijkstry - od początku i od końca drogi
# Oblicza wtedy najkrótsze drogi przy użyciu rowerów w następujący sposób:
#   Przechodzi po każdym wierzchołku:
#       1. Jeżeli nie ma tam rowera to idzie dalej
#       2. W przeciwnym wypadku oblicza najkrótszą drogę poprzez dodanie najkrótszej drogi od początku do rowera 
#           i najkrótszej drogi od końca do rowera przemnożonej przez jego efektywność (p/q rowera)
#
# Wynikiem będzie minimum z najkrótszych dróg przy użyciu rowera i drogi bez użycia żadnych rowerów
#
# Program na początku wybiera dla każdego wierzchołka najlepszy rower, żeby nie liczyć więcej ścieżek niż trzeba
# Program również oblicza minimalną ścieżkę nawet, gdy rower jest pogorszeniem (p > q),
#   aczkolwiek nie ma to znaczenia, ponieważ wtedy i tak znajdziemy najkrótszą drogę.
#   Jest tak, ponieważ bierzemy pod uwagę najkrótszą ścieżkę bez użycia żadnego roweru
#
# Złożoność czasowa: O(ElogV)

from egz1atesty import runtests
from queue import PriorityQueue
from math import inf

def dijkstra(G, start):
    n = len(G)
    distances = [inf] * n
    distances[start] = 0
    q = PriorityQueue()
    q.put((0, start))
    while not q.empty():
        dist, v = q.get()
        for u, weight in G[v]:
            newDist = dist + weight
            if newDist < distances[u]:
                distances[u] = newDist
                q.put((newDist, u))
    return distances

def armstrong(B, G, s, t):
    # Sprawdzamy ile jest wierzchołków w G 
    vertexAmount = 0
    for v1, v2, _ in G:
        vertexAmount = max(v1, v2, vertexAmount) + 1
    
    # Tworzymy listę sąsiedztwa
    adjList = [[] for _ in range(vertexAmount)]
    for v1, v2, weight in G:
        adjList[v1].append((v2, weight))
        adjList[v2].append((v1, weight))
    
    # Tworzymy listę najlepszych rowerów dla każdego wierzchołka
    bicycles = [None] * vertexAmount
    for v, p, q in B:
        if bicycles[v] == None or bicycles[v][0] / bicycles[v][1] > p / q:
            bicycles[v] = (p, q)
    
    # Odpalamy dwa razy dijkstrę
    dist1 = dijkstra(adjList, s)
    dist2 = dijkstra(adjList, t)

    # Znajdujemy i zwracamy najkrótszą ścieżkę
    minDist = dist1[t]
    for i in range(vertexAmount):
        if bicycles[i] != None and dist1[i] < inf and dist2[i] < inf:
            minDist = min(minDist, int(dist1[i] + dist2[i] * bicycles[i][0] / bicycles[i][1]))
    return minDist

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( armstrong, all_tests = True )
