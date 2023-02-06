class Node:
    def __init__(self, data):
        self.data = data
        self.next = None

class CircularLinkedList:  
    def __init__(self):
        self.head = None
    
    def add(self, data):
        newNode = Node(data)
        newNode.next = self.head

        if self.head is not None:
            trav = self.head    
            while (trav.next != self.head):
                trav = trav.next
            trav.next = newNode
        else:
            newNode.next = newNode
        self.head = newNode

def createCircularList(n):
    clist = CircularLinkedList()

    while (n > 0):
        clist.add("P" + str(n))
        n -= 1
    return clist

# brute-force approach for the circular elimination game
def eliminationBruteForce(n):
    clist = createCircularList(n)
    curr = clist.head
    while curr.next != curr:
        print("%-4s eliminates %-4s" % (curr.data, curr.next.data))
        curr.next = curr.next.next
        curr = curr.next
    print("%-4s is the winner" % (curr.data))
    return curr.data

# decrease-and-conquer approach for the circular elimination game
def eliminationDecreaseAndConquer(n):
    base2 = 2   # becomes 2, 4, 8 ...
    first = 1   # first player 
    while n > 1:
        # if number of players is odd, then the first player is eliminated
        if n % 2 != 0:
            # new first player 
            first = first + base2
        base2 = base2 * 2
        # half of the players are eliminated
        n = n // 2

    return first 

n = 12 # number of players
print(f"number of players: {n}")
print(f"Brute Force Approach")
result1 = eliminationBruteForce(n)
result2 = eliminationDecreaseAndConquer(n)

print(f"Decrease and Conquer Approach: {result2}")