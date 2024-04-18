def quicksort(arr, start, end):
    if start < end:
        mid = partition(arr, start, end)
        quicksort(arr, start, mid-1)
        quicksort(arr, mid+1, end)

def partition(arr, start, end):
    pass