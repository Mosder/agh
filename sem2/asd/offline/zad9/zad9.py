# Maciej Borowiec
#
# Funkcja oblicza dla każdego obszaru najdłuższą ścieżkę kończącą się na tym obszarze (z dowolnego startu)
# Oblicza ją poprzez wziącie maksimum z każdego sąsiadującego pola (o mniejszej wysokości) i dodanie 1
# Wynikiem jest maksimum z wszystkich obliczonych wartości
#
# Złożoność czasowa i pamięciowa: O(n*m)

from zad9testy import runtests

def trip(M):
    mx = 0
    rows = len(M)
    cols = len(M[0])
    F = [[1 for _ in range (cols)] for _ in range (rows)]
    
    def f(r, c):
        nonlocal M, F, rows, cols
        if F[r][c] != 1:
            return F[r][c]
        mx = 1
        if r > 0 and M[r-1][c] < M[r][c]:
            mx = max(mx, f(r-1, c) + 1)
        if r < rows - 1 and M[r+1][c] < M[r][c]:
            mx = max(mx, f(r+1, c) + 1)
        if c > 0 and M[r][c-1] < M[r][c]:
            mx = max(mx, f(r, c-1) + 1)
        if c < cols - 1 and M[r][c+1] < M[r][c]:
            mx = max(mx, f(r, c+1) + 1)
        F[r][c] = mx
        return mx

    for r in range(rows):
        for c in range(cols):
            mx = max(mx, f(r, c))
    return mx

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( trip, all_tests = True )
