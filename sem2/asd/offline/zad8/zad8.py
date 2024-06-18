# Maciej Borowiec
#
# Algorytm dynamiczny jest oparty na funkcji f(i,j), która zwraca minimalną drogę do parkingów
#   dla i+1 pierwszych biurowców przy użyciu maksymalnie j+1 pierwszych działek
#
# f(i,j) = min( f(i,j-1) , f(i-1,j-1) + |xi - yj| )
# Bierzemy minimum z dwóch możliwości:
#   1. Albo przydzielamy biurowcowi o indeksie i działkę o indeksie j oraz dodajemy wartość f(i-1,j-1)
#   2. Albo nie przydzielamy tej działki temu biurowcowi i zostaje po prostu wartość f(i,j-1)
# Dla f(0,j) po prostu bierzemy odległość najbliższej z j+1 pierwszych działek do biurowca o indeksie 0,
#   a gdy i > j to nie mamy wystarczającej liczby działek i wartość jest równa nieskończoność
#
# Szukanym wynikiem jest f(n-1,m-1) (przypisujemy parkingi dla każdego biurowca mając do dyspozycji wszystkie działki)
#
# Złożoność czasowa i pamięciowa: O(n*m)

from zad8testy import runtests
from math import inf

def parking(X,Y):
    n = len(X)
    m = len(Y)
    F = [[inf for _ in range(m)] for _ in range(n)]
    F[0][0] = abs(X[0] - Y[0])
    for j in range(1, m):
        F[0][j] = min(abs(X[0] - Y[j]), F[0][j-1])
    for i in range(1, n):
        for j in range(i, m):
            F[i][j] = min(F[i][j-1], F[i-1][j-1] + abs(X[i] - Y[j]))
    return F[n-1][m-1]

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( parking, all_tests = True )
