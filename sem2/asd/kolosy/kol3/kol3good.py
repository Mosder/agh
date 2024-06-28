from kol3testy import runtests

def orchard(T, m):
    n = len(T)
    maxApplesMod = sum(T) % m
    F = [[n for _ in range(m)] for _ in range(n)]
    F[0][(maxApplesMod - T[0]) % m] = 1
    F[0][maxApplesMod] = 0
    for i in range(1, n):        
        for j in range(m):
            F[i][j] = min(F[i-1][j], F[i-1][(j + T[i]) % m] + 1)
    return F[n-1][0]

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests(orchard, all_tests=True)
