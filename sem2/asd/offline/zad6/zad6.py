# Maciej Borowiec
#
# Algorytm rozdwaja wierzchołki na wierzchołek, na którym są możliwe do użycia buty oraz na wierzchołek tuż po użyciu butóœ
# Algorytm używa zmodyfikowanego algorytmu dijkstry, który gdy jest na wierzchołku z dostępnymi butami
#   idzie do sąsiada bez użycia butów lub idzie do wszystkich sąsiadów danego sąsiada z użyciem butów
# Natomiast, gdy jest na wierzchołku po użyciu butów, idzie jedynie do sąsiadów brz użycia butów, ponieważ nie może ich od razu użyć
#
# Złożoność czasowa: O(EVlogV)
# Złożoność pamięciowa: O(V)

from zad6testy import runtests
from math import inf
from queue import PriorityQueue

def dijkstra(G, start):
    n = len(G)
    distances = [[inf,inf] for _ in range(n)]
    for i in range(2):
        distances[start][i] = 0
    q = PriorityQueue()
    # 3 miejsce w krotkach wsadzanych do kolejki to status butów:
    #   0 - buty gotowe do użycia
    #   1 - buty właśnie użyte
    q.put((0, start, 0))
    while not q.empty():
        data = q.get()
        vertex = data[1]
        state = data[2]
        for i in range(n):
            edge = G[vertex][i]
            if edge > 0:
                if state == 0:
                    newDist = distances[vertex][0] + edge
                    if distances[i][0] > newDist:
                        distances[i][0] = newDist
                        q.put((newDist, i, 0))
                    for j in range(n):
                        if G[i][j] > 0:
                            newDist = distances[vertex][0] + max(edge, G[i][j])
                            if distances[j][1] > newDist:
                                distances[j][1] = newDist
                                q.put((newDist, j, 1))
                else:
                    newDist = distances[vertex][1] + edge
                    if distances[i][0] > newDist:
                        distances[i][0] = newDist
                        q.put((newDist, i, 0))
    return distances
                

def jumper( G, s, w ):
    distances = dijkstra(G, s)
    return min(distances[w])

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( jumper, all_tests = True )