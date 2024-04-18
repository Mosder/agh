class Node:
    def __init__(self, val=None, next=None):
        self.val = val
        self.next = next

def merge(list1, list2):
    head = Node()
    tail = head
    while list1 != None and list2 != None:
        if list1.val < list2.val:
            tail.next = list1
            list1 = list1.next
        else:
            tail.next = list2
            list2 = list2.next
        tail = tail.next
    tail.next = list1 if list1 != None else list2
    return head.next

def splitList(list1):
    if list1 == None: return None, None
    cursor = list1
    while cursor.next != None and cursor.val < cursor.next.val:
        cursor = cursor.next
    list2 = cursor.next
    cursor.next = None
    return list1, list2

def mergeSort(list1):
    flag = True
    while flag:
        flag = False
        srtd = Node()
        tail = srtd
        while list1 != None:
            part1, list1 = splitList(list1)
            if list1 == None:
                tail.next = part1
            else:
                part2, list1 = splitList(list1)
                tail.next = merge(part1, part2)
                while tail != None:
                    tail = tail.next
                flag = True
        list1 = srtd.next
    return list1