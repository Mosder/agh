from data import runtests

# function that checks wheter given coordinates are in bounds of the board
def is_in_bounds(width, height, position):
    if 0 <= position[0] < height and 0 <= position[1] < width:
        return True
    return False

# function that parses width, height, list of holes and list of pieces to a board matrix
def input_data_to_matrix(width, height, holes, pieces):
    matrix = [[None for _ in range(width)] for _ in range(height)]

    for col, row in holes:
        if is_in_bounds(width, height, (row-1, col-1)): # check, because some holes are outside of the board
            matrix[row-1][col-1] = "hole"
    
    for piece, col, row in pieces:
        if is_in_bounds(width, height, (row-1, col-1)): # again, some pieces are outside of the board for some reason
            matrix[row-1][col-1] = piece
    
    return matrix

# function that parses a board matrix to a FEN string (only the pieces position of the FEN string)
def matrix_to_fen(matrix):
    fen = ""

    for row in matrix:
        empty_space_count = 0

        for tile in row:
            if tile is None or tile == "hole":
                empty_space_count += 1
            else:
                if empty_space_count > 0:
                    fen += str(empty_space_count)
                    empty_space_count = 0
                fen += tile
        
        if empty_space_count > 0:
            fen += str(empty_space_count)
        fen += "/"
    
    return fen[:-1]

# function that checks wheter a string contains an integer value
def is_int(string):
    try:
        int(string)
    except ValueError:
        return False
    else:
        return True

# function that returns the list of pieces from a FEN string
def pieces_from_fen(fen):
    pieces = []
    rows = fen.split("/")
    for row_index, row in enumerate(rows):
        col_index = 0
        for char in row:
            if is_int(char):
                col_index += int(char)
            else:
                pieces.append((char, (row_index, col_index)))
                col_index += 1
    return pieces

# function that checks wheter given position on a board is empty (not a piece nor a hole)
def is_empty(board, position):
    return board[position[0]][position[1]] is None

# function that returns movement vectors for the given piece
def get_movement_vectors(piece):
    # knight movement vectors
    if piece == "n":
        return [(2,1), (1,2), (-1,2), (-2,1), (-2,-1), (-1,-2), (1,-2), (2,-1)]
    
    movement_vectors = []
    # orthagonal movement vectors (king, queen, rook)
    if piece in ("k", "q", "r"):
        movement_vectors += [(1,0), (0,1), (-1,0), (0,-1)]
    # diagonal movement vectors (king, queen, bishop)
    if piece in ("k", "q", "b"):
        movement_vectors += [(1,1), (-1,1), (-1,-1), (1,-1)]

    return movement_vectors

# function that returns moves from movement vectors for a king and a knight
def get_moves_kn(board, position, movement_vectors):
    width = len(board[0])
    height = len(board)
    moves = []
    for vector in movement_vectors:
        move = (position[0] + vector[0], position[1] + vector[1])
        if is_in_bounds(width, height, move) and is_empty(board, move):
            moves.append(move)
    return moves

# function that returns moves from movement vectors for sliding pieces (queen, bishop, rook)
def get_moves_slide(board, position, movement_vectors):
    width = len(board[0])
    height = len(board)
    moves = []
    for vector in movement_vectors:
        move = (position[0] + vector[0], position[1] + vector[1])
        while is_in_bounds(width, height, move) and is_empty(board, move):
            moves.append(move)
            move = (move[0] + vector[0], move[1] + vector[1])
    return moves

# function that runs correct move getter for the given piece
def get_moves(piece, board, position):
    movement_vectors = get_movement_vectors(piece)
    if piece in ("k", "n"):
        return get_moves_kn(board, position, movement_vectors)
    else:
        return get_moves_slide(board, position, movement_vectors)

# function that returns a new board matrix after the given move
def board_after_move(board, start, destination):
    new_board = [[tile for tile in row] for row in board]
    new_board[destination[0]][destination[1]] = new_board[start[0]][start[1]]
    new_board[start[0]][start[1]] = None
    return new_board

# chess positions graph node class
class Node:
    def __init__(self):
        # set of indices of neighbors
        self.neighbors = set()
    
    # function that adds a neighbor of the node
    def add_neighbor(self, index):
        self.neighbors.add(index)

# chess positions graph class
class Graph:
    def __init__(self):
        # list of graph nodes
        self.vertices = []
        # map with keys being FENs and values being indices of the vertex in the graph
        self.fen_map = {}

    # function that checks wheter a vertex already exists
    def is_a_vertex(self, fen):
        if fen in self.fen_map.keys():
            return True
        return False
    
    # function that adds a vertex to the graph
    def add_vertex(self, fen):
        self.fen_map[fen] = len(self.vertices)
        self.vertices.append(Node())
    
    # function that adds an edge to the graph
    def add_edge(self, fen1, fen2):
        index1 = self.fen_map[fen1]
        index2 = self.fen_map[fen2]
        self.vertices[index1].add_neighbor(index2)
        self.vertices[index2].add_neighbor(index1)

# function that creates and returns a positions graph
def create_positions_graph(board):
    graph = Graph()
    fen = matrix_to_fen(board)
    graph.add_vertex(fen)
    queue = [(board, matrix_to_fen(board))]
    while len(queue) > 0:
        board, fen = queue.pop(0)
        pieces = pieces_from_fen(fen)
        for piece, position in pieces:
            moves = get_moves(piece, board, position)
            for move in moves:
                new_board = board_after_move(board, position, move)
                new_fen = matrix_to_fen(new_board)
                if not graph.is_a_vertex(new_fen):
                    graph.add_vertex(new_fen)
                    queue.append((new_board, new_fen))
                graph.add_edge(fen, new_fen)
    return graph

# function that finds the lowest common ancestor in the blossom tree
def lowest_common_ancestor(match, base, p, a, b):
    used = [False] * len(match)
    while True:
        a = base[a]
        used[a] = True
        if match[a] == -1:
            break
        a = p[match[a]]
    while True:
        b = base[b]
        if used[b]:
            return b
        b = p[match[b]]

# function that marks the path from the given vertex to the base of the blossom
def mark_path(match, base, blossom, p, v, b, children):
    while base[v] != b:
        blossom[base[v]] = blossom[base[match[v]]] = True
        p[v] = children
        children = match[v]
        v = p[match[v]]

# function for finging augmenting path in the blossom algorithm
def augmenting_path(graph, match, p, root):
    n = len(graph)
    used = [False] * n
    p[:] = [-1] * n
    base = list(range(n))
    used[root] = True
    q = [root]

    while q:
        v = q.pop(0)
        for to in graph[v]:
            if base[v] == base[to] or match[v] == to:
                continue
            if to == root or (match[to] != -1 and p[match[to]] != -1):
                curbase = lowest_common_ancestor(match, base, p, v, to)
                blossom = [False] * n
                mark_path(match, base, blossom, p, v, curbase, to)
                mark_path(match, base, blossom, p, to, curbase, v)
                for i in range(n):
                    if blossom[base[i]]:
                        base[i] = curbase
                        if not used[i]:
                            used[i] = True
                            q.append(i)
            elif p[to] == -1:
                p[to] = v
                if match[to] == -1:
                    return to
                to = match[to]
                used[to] = True
                q.append(to)
    return -1

# function implementing blossom algorithm for finding the max matching size
def max_matching(graph):
    n = len(graph)
    match = [-1] * n
    p = [0] * n
    for i in range(n):
        if match[i] == -1:
            v = augmenting_path(graph, match, p, i)
            while v != -1:
                pv = p[v]
                ppv = match[pv]
                match[v] = pv
                match[pv] = v
                v = ppv
    return sum(1 for x in match if x != -1) // 2

def my_solve(N, M, holes, pieces):
    starting_board = input_data_to_matrix(N, M, holes, pieces)
    graph = create_positions_graph(starting_board)
    # create adjacency list for the positions graph
    adjacency_list = [node.neighbors for node in graph.vertices]
    # create adjacency list for the positions graph without the starting position
    adjacency_list_without_starting_vertex = [set([neighbor - 1 for neighbor in node.neighbors if neighbor != 0]) for node in graph.vertices[1:]]
    # first player always wins iff max matching size is different for these graphs
    return max_matching(adjacency_list) != max_matching(adjacency_list_without_starting_vertex)

runtests(my_solve)
