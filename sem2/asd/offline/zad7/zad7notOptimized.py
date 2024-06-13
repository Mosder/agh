from zad7testy import runtests

def maze( L ):
    n = len(L)
    F = [[[-1, -1, -1] for _ in range(n)] for _ in range(n)]
    F[0][0] = [0,0,0]
    for col in range(n):
        for row in range(n):
            if L[row][col] == '#':
                continue
            if col > 0 and max(F[row][col-1]) > -1:
                F[row][col][0] = max(F[row][col-1]) + 1
            if row > 0 and max(F[row-1][col][0], F[row-1][col][1]) > -1:
                F[row][col][1] = max(F[row-1][col][0], F[row-1][col][1]) + 1
        for row in range(n-1, -1, -1):
            if L[row][col] == '#':
                continue
            if row < n-1 and max(F[row+1][col][0], F[row+1][col][2]) > -1:
                F[row][col][2] = max(F[row+1][col][0], F[row+1][col][2]) + 1
    return max(F[n-1][n-1])

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( maze, all_tests = True )
