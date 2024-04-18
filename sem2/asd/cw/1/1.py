def bubbleSort(arr):
    arrLen = len(arr)
    for i in range(arrLen-1):
        flag = True
        for j in range(arrLen-1-i):
            if arr[j] > arr[j+1]:
                arr[j], arr[j+1] = arr[j+1], arr[j]
                flag = False
        if flag:
            return arr
    return arr

arr = [3,6,4,6,4,5,6,2]
print(bubbleSort(arr))
