# Maciej Borowiec
#
# Algorytm:
# 1. Znaleźć drzewo MST - jest to Królewski Trakt
# 2. Uruchomić BFS dla każdego lorda, by znaleźć zbiór wierzchołków zajętych przez niego (poddrzewo lorda) i sumę wag pokrytych krawędzi
# 3. Stworzenie grafu, który ma lordów jako wierzchołki i krawędzie między lordami, którzy mają ze sobą konflikt
# 4. Obliczenie maksymalnego, względem wagi wierzchołków, zbioru niezależnego dla stworzonego grafu.
#   Z definicji tego grafu łatwo zauważyć, że będzie to szukany wynik
#
# Znalezienie maksymalnego zbioru niezależnego dla grafu zostało opartę o pracę węgierskiego matematyka - András Frank.
# W pracy zatytułowanej "Some polynomial algorithms for certain graphs and hypergraphs" opisuje on sposób znalezienia
# niezależnego zbioru wierzchołków o maksymalnej sumie ich wag dla grafu przekątniowego (strona 214 w pracy).
# Zdefiniowany graf lordów jest grafem przekątniowym, więc możemy zastosować ten algorytm na tym grafie.
#
# Poniżej przedstawię próbę dowodu, tego, że graf lordów jest przekątniowy:
#   1. Załóżmy, że graf nie jest przekątniowy - wobec tego istnieje cykl o długości co najmniej 4 bez żadnego połączenia
#       pomiędzy wierzchołkami nie sąsiadującymi ze sobą w tym cyklu.
#   2. Wybierzmy dwa nie sąsiadujące ze sobą wierzchołki w takim cyklu - oznaczmy je jako v i u.
#   3. Wierzchołki w grafie lordów są połączone ze sobą tylko w przypadku konfliktów, zatem ścieżka pomiędzy dwoma wierzchołkami
#       w grafie lordów istnieje wtw., gdy istnieje ścieżka pomiędzy wierzchołkami poddrzew lordów w orginalnym drzewie
#   4. Istnieją dwie ścieżki z v do u w danym cyklu. Możemy więc wyróżnić dwa przypadki:
#       1) Istnieją dwie różne ścieżki z wierzchołków podgrafu v do podgrafu u w Królewskim Trakcie. Jest to sprzeczne,
#           gdyż Królewski Trakt jest drzewem, a dwie różne ścieżki pomiędzy tymi samymi wierzchołkami, by utworzyły cykl,
#           który w drzewie nie może się znajdować
#       2) Weźmy sąsiednie wierzchołki v w cyklu - v1 i v2, oraz sąsiednie wierzchołki u - u1 i u2. Jeśli ścieżka z v do u
#           w oryginalnym drzewie przechodzi przez wierzchołek v1 to albo v jest w konflikcie z u, albo v2 jest w konflikcie
#           z oboma wierzchołkami u1 i u2. Jest tak, gdyż v i v2 są w konflikcie i nie istnieje bezpośrednia ścieżka między nimi.
#           Zatem co najmniej jeden z wierzchołków v lub v2 musi mieć połączenie z conajmniej jednym z wierzchołków,
#           które nie sąsiadują z nim w cyklu. Jest to sprzeczne z założeniem, że ten cykl nie ma takich przekątnych.
#           Sytuacja wygląda analogicznie, gdy ścieżka z v do u w oryginalnym grafie przechodzi przez v2 zamiast v1.
#   5. W obu przypadkach otrzymujemy sprzeczność, więc graf lordów musi być grafem przekątniowym.

from data import runtests
from collections import deque

# find-union
class Node:
    def __init__(self):
        self.parent = self
        self.rank = 0

def find(x):
    if x.parent != x:
        x.parent = find(x.parent)
    return x.parent

def union(x,y):
    x = find(x)
    y = find(y)
    if x == y:
        return
    if x.rank > y.rank:
        y.parent = x
    else:
        x.parent = y
        if x.rank == y.rank:
            y.rank += 1

# zamiana listy krawędzi na listę sąsiedztwa
def edge_list_to_adjacency_list(vertex_amount, edge_list):
    adjacency_list = [[] for _ in range(vertex_amount)]
    for v, u, w in edge_list:
        adjacency_list[v].append((u, w))
        adjacency_list[u].append((v, w))
    return adjacency_list

# znajduje mst dla listy krawędzi i zwraca w postaci listy sąsiedztwa
def mst(vertex_amount, edge_list):
    edge_list.sort(key=lambda e: e[2])
    node_list = [Node() for _ in range(vertex_amount)]
    mst_edge_list = []
    for edge in edge_list:
        v, u, _ = edge
        if find(node_list[v]) != find(node_list[u]):
            mst_edge_list.append(edge)
            union(node_list[v], node_list[u])
    return edge_list_to_adjacency_list(vertex_amount, mst_edge_list)

# bfs zwracający tablicę rodziców (razem z wagą)
def bfs(adjacency_list, start_vertex):
    vertex_amount = len(adjacency_list)
    visited = [False for _ in range(vertex_amount)]
    visited[start_vertex] = True
    parents = [None for _ in range(vertex_amount)]
    queue = deque()
    queue.append(start_vertex)
    while len(queue) > 0:
        v = queue.popleft()
        for u, w in adjacency_list[v]:
            if not visited[u]:
                visited[u] = True
                parents[u] = (v, w)
                queue.append(u)
    return parents

# klasa posiadająca informacje o poddrzewie każdego lorda oraz sumę pokrytych ścieżek przez tego lorda
class LordInfo:
    def __init__(self, vertex_set, covered_sum):
        self.vertex_set = vertex_set
        self.covered_sum = covered_sum

# sprawdź rozłączność lordów
def disjointed_lords(lord1, lord2):
    if len(lord1.vertex_set & lord2.vertex_set) > 0:
        return False
    return True

# zwraca listę informacji o lordach (obiekty klasy Lord)
def get_lords_info(adjacency_list, lords):
    lords_info = []
    for lord in lords:
        vertex_set = set()
        covered_sum = 0
        parents = bfs(adjacency_list, lord[0])
        vertex_set.add(lord[0])
        for i in range(1, len(lord)):
            v = lord[i]
            while v not in vertex_set:
                v_parent, w = parents[v]
                vertex_set.add(v)
                covered_sum += w
                v = v_parent
        lords_info.append(LordInfo(vertex_set, covered_sum))
    return lords_info

# node dla grafu lordów
class LordNode:
    def __init__(self, index, val):
        self.index = index
        self.val = val
        self.neighbors = set()
    
    def add_neighbor(self, v_index):
        self.neighbors.add(v_index)
    
    @staticmethod
    def add_edge(v, u):
        v.add_neighbor(u.index)
        u.add_neighbor(v.index)

# buduje graf lordów
def build_lords_graph(lords_info):
    lords_graph = [LordNode(index, lord.covered_sum) for index, lord in enumerate(lords_info)]
    lord_amount = len(lords_info)
    for i in range(lord_amount):
        for j in range(i+1, lord_amount):
            lord1 = lords_info[i]
            lord2 = lords_info[j]
            if not disjointed_lords(lord1, lord2):
                LordNode.add_edge(lords_graph[i], lords_graph[j])
    return lords_graph

# lex bfs
def lex_bfs(graph):
    n = len(graph)
    visited = [False for _ in range(n)]
    visited_order = []
    lex_order_list = [set([v for v in range(n)])]
    for _ in range(n):
        vertex = lex_order_list[-1].pop()
        visited[vertex] = True
        visited_order.append(vertex)
        new_lex_order_list = []
        for s in lex_order_list:
            neighbors = s & graph[vertex].neighbors
            not_neighbors = s - graph[vertex].neighbors
            if len(not_neighbors) > 0:
                new_lex_order_list.append(not_neighbors)
            if len(neighbors) > 0:
                new_lex_order_list.append(neighbors)
        lex_order_list = new_lex_order_list
    return visited_order

# znalezienie maksymalnego zbioru niezależnego
def get_max_sum_of_independent_vertices(graph):
    n = len(graph)
    order = lex_bfs(graph)
    chosen_vertices = [False for _ in range(n)]
    checked_vertices = [False for _ in range(n)]
    result = 0
    for v_index in order[::-1]:
        v = graph[v_index]
        checked_vertices[v_index] = True
        if v.val > 0:
            result += v.val
            chosen_vertices[v_index] = True
            for u in v.neighbors:
                if not checked_vertices[u]:
                    graph[u].val -= v.val
    for v_index in order:
        if not chosen_vertices[v_index]:
            flag = True
            for u in graph[v_index].neighbors:
                if chosen_vertices[u]:
                    flag = False
                    break
            if flag:
                result += v.val
    return result

def my_solve(N, streets, lords):
    streets = [(v-1, u-1, w) for (u,v,w) in streets]
    lords = [[v-1 for v in lord] for lord in lords]
    tract = mst(N, streets)
    lords_info = get_lords_info(tract, lords)
    lords_graph = build_lords_graph(lords_info)
    return get_max_sum_of_independent_vertices(lords_graph)

runtests(my_solve)
