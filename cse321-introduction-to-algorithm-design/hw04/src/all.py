# Emirkan Burak Yılmaz 1901042659

import random

###################################### QUESTION 1 ######################################

def maxScore(gameMap):
    return maxScoreBrute(gameMap, 0, 0)

def maxScoreBrute(gameMap, r, c):
    currLoc = "A" + str(r + 1) + "B" + str(c + 1) 
    rightScore, rightPath = 0, []
    downScore, downPath = 0, []
    # right movement
    if c + 1 < len(gameMap[r]):
        rightScore, rightPath = maxScoreBrute(gameMap, r, c + 1)
    # down movement 
    if r + 1 < len(gameMap):
        downScore, downPath = maxScoreBrute(gameMap, r + 1, c)
    # select the path that contains more points 
    if rightScore > downScore:
        return rightScore + gameMap[r][c], [currLoc] + rightPath
    else:
        return downScore + gameMap[r][c], [currLoc] + downPath

def printMap(map):
    for row in map:
        for item in row:
            print("%2d" % (item), end=" ")
        print()      

###################################### QUESTION 2 ######################################

def partition(arr, first, last):
    piv = arr[first]
    right = first
    left = last

    while right < left:
        while right <= last and arr[right] <= piv:
            right += 1
        while left >= first and arr[left] > piv:
            left -= 1
        if right < left:
            arr[right], arr[left] = arr[left], arr[right]
    arr[first], arr[left] = arr[left], arr[first]
    return left

def selectHelper(arr, first, last, i):
    if first >= last:
        return arr[first]
    
    p = partition(arr, first, last)

    if p == i:  
        return arr[i]
    elif p > i:
        return selectHelper(arr, first, p - 1, i)
    else:
        return selectHelper(arr, p + 1, last, i)

def select(arr, i):
    return selectHelper(arr, 0, len(arr) - 1, i)

def median(arr):
    size = len(arr)
    i = size // 2

    if size % 2 == 0:
        return (select(arr, i - 1) + select(arr, i)) / 2 
    else:
        return select(arr, i) 

def createRandom(n, min = 0, max = 100):
    v = []
    for i in range(n):
        v.append(random.randint(min, max))
    return v

###################################### QUESTION 3 ######################################

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
            # set new first player 
            first = first + base2
        base2 = base2 * 2
        # half of the players are eliminated
        n = n // 2

    return first 

###################################### DRIVER ######################################

if __name__ == '__main__':
    while True:
        val = int(input("Press 1 for Q1, 2 for Q2 and 3 for Q3. Press 0 to terminate.\n> "))
        print()
        if val == 1:
            gameMap1 = [ [25, 30, 25],
                        [45, 15, 11],
                        [ 1, 88, 15],
                        [ 9,  4, 23]]

            gameMap2 = [ [ 5, 2,  4, 3],
                        [ 1, 1, 20, 10],
                        [ 1, 1, 20, 10],
                        [ 1, 1, 20, 10]]

            score, path = maxScore(gameMap1)
            printMap(gameMap1)
            print (f"score: {score}\npath: {path}\n")

            score, path = maxScore(gameMap2)
            printMap(gameMap2)
            print (f"score: {score}\npath: {path}\n")

        elif val == 2:
            sizes = [9, 20]
            for n in sizes:
                A = createRandom(n) # create size N randomly filled array
                print(f"Randomly generated array A with size {len(A)}")
                print(f"A = {A} (Unsorted)")
                print(f"Median of A is {median(A)}")
                print("Check the result with sorted A")
                A.sort()
                print(f"A = {A} (Sorted)\n")

        elif val == 3:
            n = 12 # number of players
            print(f"Number of players: {n}")
            result1 = eliminationBruteForce(n)
            result2 = eliminationDecreaseAndConquer(n)
            print(f"\nBrute Force Approach: {result1}")
            print(f"Decrease and Conquer Approach: P{result2}")
        else:
            break
        print()