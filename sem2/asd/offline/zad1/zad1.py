# Maciej Borowiec
# Program jest zmodyfikowanym selection sort, w którym wewnętrzna pętla wykonuje się maksymalnie k razy
# Algorytm szuka najmniejszego elementu w k+1 pierwszych elementach listy i przypina go do ostatniego elementu wyniku
# Dzięki temu następny najmniejszy element również będzie musiał się znajdować w k+1 pierwszych elementach pozostałej listy
# Powtarzamy tę czynność aż wykorzystamy wszystkie elementy z listy
# Złożoność czasowa tego algorytmu wynosi Θ(nk), zatem jest rzędu:
# - Θ(n) dla k = Θ(1)
# - Θ(nlogn) dla k = Θ(logn)
# - Θ(n^2) dla k = Θ(n)

from zad1testy import Node, runtests


def SortH(p,k):
    result = Node()
    resultEnd = result
    sent = Node()
    sent.next = p
    while sent.next != None:
        beforeMin = sent
        counter = 0
        p = sent.next
        while counter < k and p.next != None:
            if p.next.val < beforeMin.next.val:
                beforeMin = p
            p = p.next
            counter += 1

        resultEnd.next = beforeMin.next
        resultEnd = resultEnd.next
        beforeMin.next = beforeMin.next.next

    return result.next

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( SortH, all_tests = True )

