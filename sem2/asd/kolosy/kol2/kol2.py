# Maciej Borowiec
#
# Aglorytm jest zmodyfikowanym algorytmem Dijkstry,
#   który "rozmnaża" liczbę wierzchołków grafu w zależności od tego ile wojownik ma energi, gdy się w nim znajdzie.
#   Energia to ile jeszcze godzin może iść bez odpoczynku (czyli jest równa 16 - droga przebyta od ostatniego odpoczynku)
# Przy wyjęciu wierzchołka (v) z kolejki i rozpatrywaniu jego sąsiadów (u) wykonujemy dwie operacje:
#   1. Jeśli możemy pokonać szlak bez wyczerpania energii (energia po przebyciu będzie >= 0)
#       to wykonujemy relaksację "podwierzchołka" u o energii jaka nam zostanie po przejściu danego szlaku
#   2. Niezależnie od tego czy możemy pokonać szlak bez odpoczynku, uznajemy, że odpoczywamy i dokonujemy relaksacji.
#       Tym razem będzie to "podwierzchołek" u o energii równej 16 - długość szlaku
# Po wykonaniu algorytmu dijkstry zwracamy minimum odległości wszystkich "podwierzchołków" wierzchołka t
#
# Złożoność pamięciowa: O(E + V)
# Złożoność czasowa to złożoność algorytmu Dijkstry pomnożona przez stałą, zatem: O(ElogV)

from kol2testy import runtests
from math import inf
from queue import PriorityQueue

def dijkstra(adjList, vertexAmount, start, end):
    distances = [[inf for _ in range(17)] for _ in range(vertexAmount)]
    for i in range(17):
        distances[start][i] = 0
    q = PriorityQueue()
    q.put((0, start, 16))
    while not q.empty():
        data = q.get()
        if data[0] >= min(distances[end]): # Kończymy algorytm, ponieważ już nie znajdziemy krótszej ścieżki
            return distances
        v = data[1]
        energy = data[2]
        for neighbor in adjList[v]:
            u = neighbor[0]
            weight = neighbor[1]
            energyAfter = energy - weight
            newDist = distances[v][energy] + weight
            if energyAfter >= 0:
                if distances[u][energyAfter] > newDist:
                    distances[u][energyAfter] = newDist
                    q.put((newDist, u, energyAfter))
            energyAfter = 16 - weight
            newDist += 8
            if distances[u][energyAfter] > newDist:
                distances[u][energyAfter] = newDist
                q.put((newDist, u, energyAfter))
    return distances


def warrior( G, s, t):
    vertexAmount = 0
    for edge in G:
        vertexAmount = max(vertexAmount, edge[0]+1, edge[1]+1)
    adjList = [[] for _ in range(vertexAmount)]
    for edge in G:
        adjList[edge[0]].append((edge[1], edge[2]))
        adjList[edge[1]].append((edge[0], edge[2]))
    dist = min(dijkstra(adjList, vertexAmount, s, t)[t])
    return dist if dist != inf else None

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( warrior, all_tests = True )
