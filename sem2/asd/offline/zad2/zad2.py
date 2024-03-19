# Maciej Borowiec
#
# 1. Program tworzy kopiec maksimum, w którym przechowuje p-k liczb oraz kopiec minimum, w którem przechowuje k liczb
# 2. Zamieniamy pierwsze elementy pomiędzy kopcami (i je naprawiamy) aż na korzeniu kopca maksimum jest wartość mniejsza niż na korzeniu kopca minimum
#   Wtedy szukana wartość jest na korzeniu kopca minimum, ponieważ jest to najmniejsza wartość z k największych liczb, więć jest k-ta największa
# 3. Po znalezieniu szukanej wartości zamieniam element jednego z kopców, który nie należy do następnej grupy p-elementowej, z elementem następnym w tablicy
#   Jest to możliwe, ponieważ przechowuję informacje gdzie elementy się znajdują
# 4. Po zamienieniu elementów naprawiam dany kopiec i powtarzam czynności opisane w krokach 2 oraz 3 aż do wyczerpania elementów w tablicy
#
# Złożoność czasowa: O(nlogn)
# Złożoność pamięciowa: O(n)

from zad2testy import runtests

def ksum(T, k, p):
    n = len(T)
    # Tworzę tablice, które będą reprezentować kopce
    heapMax = [(T[i], i) for i in range(p-k)]
    heapMin = [(T[i], i) for i in range(p-k, p)]
    # Zapisuję gdzie znajdują się elementy w kopcach
    for i in range(p):
        T[i] = [T[i], i]
    # Naprawiam stworzone wcześniej kopce
    for i in range(parent(p-k-1), -1, -1):
        heapify(T, heapMax, i, 1)
    for i in range(parent(k-1), -1, -1):
        heapify(T, heapMin, i, -1)
    # Znajduję pierwszą szukaną wartość i zapisuję ją do wyniku
    result = findSought(T, heapMax, heapMin)
    # Pętla przechodząca po najstarszych elementach (te które będą zastąpione nowymi)
    for i in range(n-p):
        # Sprawdzam, w którym kopcu znajduje się stary element i zamieniam go nowym
        if T[i][1] < p-k:
            insertToHeap(T, heapMax, 1, i, i+p, 0)
        else:
            insertToHeap(T, heapMin, -1, i, i+p, -(p-k))
        # Ponownie znajduję wartość szukaną i dodaję ją do wyniku
        result += findSought(T, heapMax, heapMin)
    return result

# Wstawia nowy element w miejsce starego w kopcu oraz naprawia ten kopiec
def insertToHeap(T, heap, typ, rm, add, offset):
    T[add] = [T[add], T[rm][1]]
    ind = T[add][1] + offset
    heap[ind] = (T[add][0], add)
    while ind >= 0:
        heapify(T, heap, ind, typ)
        ind = parent(ind)

# Naprawia kopiec w zależności od jego typu
def heapify(T, heap, i, typ):
    n = len(heap)
    minMaxInd = i
    l = left(i)
    r = right(i)
    if l < n and typ * (heap[l][0] - heap[minMaxInd][0]) > 0:
        minMaxInd = l
    if r < n and typ * (heap[r][0] - heap[minMaxInd][0]) > 0:
        minMaxInd = r
    if minMaxInd != i:
        swapIn(T, heap, i, minMaxInd)
        heapify(T, heap, minMaxInd, typ) 

# Zamienia elementy znajdujące się w tym samym kopcu
def swapIn(T, heap, i, j):
    T[heap[i][1]][1], T[heap[j][1]][1] = T[heap[j][1]][1], T[heap[i][1]][1]
    heap[i], heap[j] = heap[j], heap[i]

# Znajduje szukaną wartość poprzez zamienianie korzeniu kopców i naprawianie ich
def findSought(T, heapMax, heapMin):
    maxLen = len(heapMax)
    while maxLen > 0 and heapMax[0] > heapMin[0]:
        swapTops(T, heapMax, heapMin)
        heapify(T, heapMax, 0, 1)
        heapify(T, heapMin, 0, -1)
    return heapMin[0][0]

# Zamienia korzenie kopców
def swapTops(T, heapMax, heapMin):
    T[heapMax[0][1]][1], T[heapMin[0][1]][1] = T[heapMin[0][1]][1], T[heapMax[0][1]][1]
    heapMax[0], heapMin[0] = heapMin[0], heapMax[0]

def left(i):
    return 2*i + 1

def right(i):
    return 2*i + 2

def parent(i): 
    return (i-1) // 2

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( ksum, all_tests=True )
