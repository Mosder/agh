# Maciej Borowiec
#
# Algorytm dynamiczny wypełnia tablicę F o wymiarach n x n x 2
# Pierwsze dwa indeksy odpowiadają odpowiednio rzędowi i kolumnie komnaty
# Natomiast trzeci indeks odpowiada jednej z dwóch wartości:
#   1. Maksymalna wartość odwiedzonych komnat kończących się na tej po przyjściu z lewej lub góry.
#       Ta wartość może być, więc użyta przy pójściu w prawo lub w dół z tej komnaty
#   2. Maksymalna wartość odwiedzonych komnat kończących się na tej po przyjściu z lewej lub dołu.
#       Można zatem użyć tej wartości przy pójściu w prawo lub w górę z tej komnaty
#
# Dana wartość jest równa -1, gdy nie można się dostać do tej komnaty z tej strony
# Zatem komnaty niedostępne lub komnaty, do których nie da się dostać, mają obie wartości ustawione na -1
#
# Wartości są obliczane poprzez wzięcie maksimum z tych wartości w sąsiednich komnatach,
#   które pozwalają na dojście do tej komnaty, oraz dodanie 1
# Ważne jest to, że gdy dana wartość jest równa -1 to nie jest ona brana pod uwagę w obliczaniu kolejnych,
#   gdyż nie można się dostać do tej komnaty w ten sposób
#
# Rozwiązaniem jest max(F[n-1][n-1])
#
# Złożoność czasowa i pamięciowa: O(n^2)

from zad7testy import runtests

def maze( L ):
    n = len(L)
    # Dla bezpieczeństwa jeśli początkowa lub końcowa komnata jest niedostępna to zwracamy -1
    if L[0][0] == '#' or L[n-1][n-1] == '#':
        return -1
    F = [[[-1, -1] for _ in range(n)] for _ in range(n)]
    F[0][0] = [0,0]
    # Osobno kolumna o indeksie 0, by nie sprawdzać if col > 0 dla każdej iteracji
    for row in range(1, n):
        if L[row][0] == '.' and F[row-1][0][0] > -1:
            F[row][0][0] = F[row-1][0][0] + 1
    for col in range(1, n):
        # Przyjście z lewej do komnat w rzędzie o indeksie 0
        mxVal = max(F[0][col-1])
        if L[0][col] == '.' and mxVal > -1:
            F[0][col][0] = mxVal + 1
            F[0][col][1] = mxVal + 1
        # Przyjście z lewej i/lub z góry do komnat o rzędzie większym niż 0
        for row in range(1, n):
            if L[row][col] == '.':
                mxVal = max(F[row][col-1])
                if mxVal > -1:
                    F[row][col][0] = mxVal + 1
                    F[row][col][1] = mxVal + 1
                if F[row-1][col][0] > -1:
                    F[row][col][0] = max(F[row][col][0], F[row-1][col][0] + 1)
        # Przyjście z dołu do komnat w nieostatnim rzędzie
        for row in range(n-2, -1, -1):
            if L[row][col] == '.' and F[row+1][col][1] > -1:
                F[row][col][1] = max(F[row][col][1], F[row+1][col][1] + 1)
    return max(F[n-1][n-1])

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( maze, all_tests = True )
