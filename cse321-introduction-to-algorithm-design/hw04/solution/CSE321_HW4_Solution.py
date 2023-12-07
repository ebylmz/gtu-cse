## q1
def q1(map, i, j):
    n, m = len(map), len(map[0])
    if i == n or j == m:
        return 0, []
    # getting scores for 2 different directions
    down,  path_down = q1(map, i+1, j)
    right, path_right = q1(map, i, j+1)
    # choosing the path with higher score
    if down > right:
        return down + map[i][j], path_down + [(i,j)]  # adding map[i][j] to the path
    return right + map[i][j], path_right + [(i, j)] # adding map[i][j] to the path

def q1_print_results (map):
    print("Map:")
    for i in map:
        for j in i:
            print(j, end="\t")
        print()
    score, path = q1(map, 0, 0)
    print("Max score:", score, "\nPath: A1_B1 -> ", end="")
    for step in list(reversed(path))[:-1]:
        i, j = step
        print("A" + str(i+1) + "_B" + str(j+1) + " -> ", end = "")
    i, j = list(reversed(path))[-1]
    print("A" + str(i + 1) + "_B" + str(j + 1))
    return

##################################################
## q2

def q2 (arr):
    n = len(arr)
    d = dict()
    for element in arr:
        if element in d.keys():
            d[element] += 1
        else:
            d[element] = 1
    sum = 0
    while True:
        m = min(arr)
        while m in arr:
            arr.remove(m)
        sum += d[m]
        if n % 2 != 0: # if even
            if sum > n/2:
                return m
        else: # if odd
            if sum > n/2: # this means that both of the middle elements are the same
                return m  # (m+m)/2 = m
            if sum == n/2:
                return (m + min(arr))/2


##################################################
## q3 - a
def q3_dac (n):
    if n == 1:
        return 1
    elif n % 2 == 0: # if even
        return 2 * q3_dac(n/2) - 1
    else: # if odd
        return 2 * q3_dac(n//2) + 1

## q3 - b
class Player:
    def __init__ (self, data):
        self.data = data
        self.next = None
def q3_cll (n):
    players = [i+1 for i in range(n)]
    root = Player(1)
    previous_player = root
    # generating circular linked list
    for i in players[1:]:
        player = Player(i)
        previous_player.next = player
        previous_player = player
    player.next = root # connecting last element to root
    iter = root
    for i in range(n-1):
        iter.next = iter.next.next # eliminating the next player (removing it from the list)
        iter = iter.next # now it is the next player's turn
    return iter.data # the final player is the winner


## q1
map = [[25, 30, 25],
       [45, 15, 11],
       [1, 88, 15],
       [9, 4, 23],
       [1, 1, 1]]
print("...Question 1...")
q1_print_results(map)

## q2
array = [2, 1, 6, 10, 12, 12]
print("\n...Question 2...")
print("Median of", array, "is ", end="")
print(q2(array))


## q3
n = 12
print("\n...Question 3...")
print("3-a) The winner of the game for n =", n, "is Player", q3_dac(n))
print("3-b) The winner of the game for n =", n, "is Player", q3_cll(n))