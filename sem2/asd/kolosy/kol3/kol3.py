# Maciej Borowiec
#
# Algorytm korzysta z funkcji f, która dla 3 argumentów (i, j, k) sprawdza czy da się osiągnąć j (= pozostałe jabłka % m)
#   dla i+1 pierwszych drzew wycinając k z nich
# Wartość f(i,j,k) jest prawdziwa wtedy, gdy:
#   1. f(i-1, j, k) jest prawdziwa (sad się nie zmienia i zostawiamy i-te drzewo w spokoju)
#   2. f(i-1, (j+T[i]) % m, k-1) jest prawdziwa (wycinamy to drzewo)
#
# Niech a = wszystkie jabłka w sadzie % m
# Niech b = wszystkir jabłka w sadzie bez pierwszego % m
# f(0, a, 0) = True (nie wycinamy żadnego drzewa)
# f(0, b, 1) = True (wycinamy pierwsze drzewo)
# f(0, i, 0) = False; dla i != a
# f(0, i, 1) = False; dla j != b
# Resztę wartości obliczamy w potrójnej pętli
#
# Wynikiem jest najmniejsze i, dla którego f(n-1, 0, i) jest prawdziwe,
#   ponieważ mamy do dyspozycji wszystkie drzewa i liczba pozostałych jabłek przystaje do 0 mod m
#
# Złożoność czasowa i pamięciowa: O(n^2 * m) = O(n^3) (bo m <= 7n)

from kol3testy import runtests

def orchard(T, m):
    n = len(T)
    maxApplesMod = 0
    for i in range(n):
        maxApplesMod += T[i]
        maxApplesMod %= m
    F = [[[False for _ in range(n+1)] for _ in range(m)] for _ in range(n)]
    F[0][maxApplesMod][0] = True
    F[0][(maxApplesMod - T[0]) % m][1] = True
    for i in range(1, n):
        for j in range(m):
            for k in range(i+2):
                F[i][j][k] = F[i-1][j][k] or F[i-1][(j+T[i]) % m][k-1]
    for i in range(n):   
        if (F[n-1][0][i]):
            return i
    return n

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests(orchard, all_tests=True)
