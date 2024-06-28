# Maciej Borowiec
#
# Program sortuje każdy możliwy spójny podciąg i usuwa z niego do k pierwszych elementów ujemnych.
# W ten sposób otrzyma maksymalną wartość każdego k-spójnego podciągu.
# Wynikiem jest maksimum tych wartości
#
# Złożoność czasowa: O(n^3 logn)

from egz1btesty import runtests

def kstrong( T, k):
    n = len(T)
    maxVal = 0
    for i in range(n):
        for j in range(i+1, n+1):
            A = sorted(T[i:j])
            ind = 0
            while ind < k and ind < j - i and A[ind] < 0:
                ind += 1
            maxVal = max(maxVal, sum(A[ind:]))
    return maxVal


# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( kstrong, all_tests = True )
