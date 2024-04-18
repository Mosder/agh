def find(T, x):
    TLen = len(T)
    i = 0
    j = TLen - 1
    while i < j:
        if T[j] - T[i] == x:
            return i,j
        if T[j] - T[i] > x:
            j -= 1
        else:
            i += 1
            j = TLen - 1
