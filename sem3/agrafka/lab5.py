from graphClass import Graph
from tester import Tester
from dimacs import loadWeightedGraph

def load_graph(graph_path):
    V, L = loadWeightedGraph(graph_path)
    adjacency_list = [set() for _ in range(V)]
    for edge in L:
        adjacency_list[edge[0] - 1].add(edge[1] - 1)
        adjacency_list[edge[1] - 1].add(edge[0] - 1)
    return adjacency_list

def lex_bfs(graph: Graph):
    G = graph.data
    n = len(G)
    visited = [False for _ in range(n)]
    visited_order = []
    lex_order_list = [set([v for v in range(n)])]
    for _ in range(n):
        vertex = lex_order_list[-1].pop()
        visited[vertex] = True
        visited_order.append(vertex)
        new_lex_order_list = []
        for s in lex_order_list:
            neighbors = s & G[vertex]
            not_neighbors = s - G[vertex]
            if len(not_neighbors) > 0:
                new_lex_order_list.append(not_neighbors)
            if len(neighbors) > 0:
                new_lex_order_list.append(neighbors)
        lex_order_list = new_lex_order_list
    return visited_order

# rn(v) - neighbors of v before v in order given by lex bfs
# parent(v) - vertex of rn(v) that is last in the lex bfs order
def get_rn_and_parents(graph: Graph, order):
    n = len(graph.data)
    rn = [set() for _ in range(n)]
    parents = [None for _ in range(n)]
    checked = set()
    for v in order:
        checked.add(v)
        for u in graph.data[v] - checked:
            rn[u].add(v)
            parents[u] = v
    return rn, parents


def is_peo(graph: Graph, order):
    rn, parents = get_rn_and_parents(graph, order)
    for v in order:
        if (parents[v] is not None) and not (rn[v] - set([parents[v]]) <= rn[parents[v]]):
            return False
    return True

def is_chordal(graph: Graph):
    return is_peo(graph, lex_bfs(graph))

def max_clique_length(graph: Graph):
    rn, _ = get_rn_and_parents(graph, lex_bfs(graph))
    return max([len(x) for x in rn]) + 1

def get_chromatic_number(graph: Graph):
    order = lex_bfs(graph)
    colors = [None for _ in range(len(graph.data))]
    for v in order:
        used_colors = set([colors[u] for u in graph.data[v]])
        color = 0
        while color in used_colors:
            color += 1
        colors[v] = color
    return max(colors) + 1

def get_max_independent_set(graph: Graph, order):
    independent_set = set()
    for v in order[::-1]:
        if not (independent_set & graph.data[v]):
            independent_set.add(v)
    return independent_set

def max_vertex_cover(graph: Graph):
    return len(graph.data) - len(get_max_independent_set(graph, lex_bfs(graph)))

class Test:
    def __init__(self, dir, test_function):
        self.dir = dir
        self.test_function = test_function
        self.load_function = load_graph

if __name__ == "__main__":
    tests = [
        Test("chordal", is_chordal),
        Test("maxclique", max_clique_length),
        Test("coloring", get_chromatic_number),
        Test("vcover", max_vertex_cover)
    ]
    for test in tests:
        print(test.dir)
        tester = Tester(f"graphs/lab5/{test.dir}/", test.load_function, test.test_function)
        tester.continousTesting()
