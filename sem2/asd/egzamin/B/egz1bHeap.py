# Pisałem to 5 minut przed końcem i miałem jeden błąd w implementacji, więc nie zdążyłem wysłać :/
#
# Maciej Borowiec
#
# Program działa tworząc kopiec maksimum przechowujący ujemne liczby
# Tworzy on sumy dla spójnych podciągów idąc od 0 do n, 1 do n i tak dalej aż od n-1 do n
# Jeśli na czubku kopca jest wartość większa niż ta, na którą teraz patrzymy to zamieniamy je i naprawiamy kopiec
#   dodajemy wartość zdjętą z kopca do sumy
# W przeciwnym wypadku po prostu dodajemy wartość do sumy
#
# Wynikiem jest maksymalna z otrzymanych sum
#
# Złożoność czasowa: O(n^2 logk)

from egz1btesty import runtests

def left(i): return 2*i + 1
def right(i): return 2*i + 2
def parent(i): return (i-1) // 2

def heapify(A, n, i):
    l = left(i)
    r = right(i)
    max_ind = i
    if l < n and A[l] > A[max_ind]:
        max_ind = l
    if r < n and A[r] > A[max_ind]:
        max_ind = r
    if max_ind != i:
        A[i], A[max_ind] = A[max_ind], A[i]
        heapify(A, n, max_ind)

def kstrong( T, k):
    n = len(T)
    maxSum = 0
    for i in range(n):
        suma = 0
        heap = [0] * k
        for j in range(i, n):
            if heap[0] > T[j]:
                suma += heap[0]
                heap[0] = T[j]
                heapify(heap, k, 0)
            else:
                suma += T[j]
            maxSum = max(maxSum, suma)
    return maxSum

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( kstrong, all_tests = True )
