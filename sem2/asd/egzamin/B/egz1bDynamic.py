# Nieprzyjemny dynamik, na którego nie udało mi się wpaść podczas egzaminu

from egz1btesty import runtests

def kstrong( T, k):
    n = len(T)
    F = [[0 for _ in range(k+1)] for _ in range(n)]
    F[0][0] = T[0]
    for i in range(1, n):
        F[i][0] = max(F[i-1][0] + T[i], T[i])
    for i in range(1, n):
        for j in range(1, k+1):
            F[i][j] = max(F[i-1][j-1], F[i-1][j] + T[i])
    maxVal = 0
    for i in range(n):
        for j in range(k + 1):
            maxVal = max(maxVal, F[i][j])
    return maxVal

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( kstrong, all_tests = True )
