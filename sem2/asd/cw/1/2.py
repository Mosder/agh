class Node:
    def __init__(self, val=None, next=None):
        self.val = val
        self.next = next

def selectSort(node):
    sent = Node(next=node)
    srt = Node()
    srtCurr = srt
    while sent.next != None:
        beforeMinVal = sent
        curr = sent
        while curr.next != None:
            if curr.next.val < beforeMinVal.next.val:
                beforeMinVal = curr
            curr = curr.next
        srtCurr.next = beforeMinVal.next
        srtCurr = srtCurr.next
        beforeMinVal.next = beforeMinVal.next.next
    return srt.next

d = Node(5)
c = Node(7,d)
b = Node(3,c)
a = Node(6,b)
s = selectSort(a)
print(s.val)
while s.next != None:
    print(s.next.val)
    s = s.next
