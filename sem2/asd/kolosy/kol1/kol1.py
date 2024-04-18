# Maciej Borowiec
#
# Algorytm sprawdza rangę ostatniego elementu oraz znajduje element większy od niego o jak największym indeksie
# Powtarza tę czynność dla znalezionego elementu
# Kontynuuje to aż do wyczerpania elementów
#
# Złożoność czasowa: O(n^2)
# Złożoność pamięciowa: O(1)

from kol1testy import runtests

def maxrank(T):
    n = len(T)
    if n == 0: return 0
    mx, next = findAmountAndNext(T, n-1)
    while next != None:
        amount, next = findAmountAndNext(T, next)
        mx = max(mx, amount)
    return mx

def findAmountAndNext(arr, last):
    maxInd = None
    count = 0
    for j in range(last):
        if arr[j] > arr[last]:
            maxInd = j
        elif arr[j] < arr[last]:
            count += 1
    return count, maxInd

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( maxrank, all_tests = True )
