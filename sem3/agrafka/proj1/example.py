# Maciej Borowiec
# E - liczba krawędzi, V - liczba wierzchołków, L - liczba lordów
# 1. MST, by znaleźć Królewski Trakt (ElogV)
# 2. BFS dla każdego lorda, by znaleźć zbiór wierzchołków i
#   zbiór krawędzi zajętych przez niego + sumę pokrytych krawędzi (VL)
# 3. stworzenie grafu, który ma lordów jako wierzchołki i krawędzie między lordami bez konfliktów (?)
# 4. wzięcie maksymalnego niezależnego zbioru (?)
# WĘGIERERERERERER!!!!!!!!!!!!

from data import runtests
from collections import deque
from time import time

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

# klasa posiadająca informacje o zajętych wierzchołkach i krawędziach przez każdego lorda
# oraz sumę pokrytych ścieżek przed tego lorda
class LordInfo:
    def __init__(self, vertex_set, edge_set, covered_sum):
        self.vertex_set = vertex_set
        self.edge_set = edge_set
        self.covered_sum = covered_sum

# sprawdź rozłączność lordów
def disjointed_lords(lord1, lord2):
    if len(lord1.vertex_set & lord2.vertex_set) > 0:
        return False
    if len(lord1.edge_set & lord2.edge_set) > 0:
        return False
    return True

# zwraca krawędź (krotkę wierzchołków) dla dwóch wierzchołków w rosnącej kolejności indeksów
def get_edge(v, u):
    if v < u:
        return (v, u)
    return (u, v)

# zwraca listę informacji o lordach (obiekty klasy Lord)
def get_lords_info(adjacency_list, lords):
    lords_info = []
    for lord in lords:
        vertex_set = set()
        edge_set = set()
        covered_sum = 0
        parents = bfs(adjacency_list, lord[0])
        vertex_set.add(lord[0])
        for i in range(1, len(lord)):
            v = lord[i]
            while v not in vertex_set:
                v_parent, w = parents[v]
                vertex_set.add(v)
                edge_set.add(get_edge(v, v_parent))
                covered_sum += w
                v = v_parent
        lords_info.append(LordInfo(vertex_set, edge_set, covered_sum))
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
            if disjointed_lords(lord1, lord2):
                LordNode.add_edge(lords_graph[i], lords_graph[j])
    return lords_graph

def my_solve(N, streets, lords):
    streets = [(v-1, u-1, w) for (u,v,w) in streets]
    lords = [[v-1 for v in lord] for lord in lords]
    tract = mst(N, streets)
    lords_info = get_lords_info(tract, lords)
    return 0

runtests(my_solve)
