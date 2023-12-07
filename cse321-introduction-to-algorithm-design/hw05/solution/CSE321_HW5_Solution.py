def q1(words, left, right):
    # if left index exceed its limit
    if left > right:
        return ""
    if left == right:
        return words[left]
    # we divide the solution into 2 parts and solve both
    mid = (left + right) // 2
    part1 = q1(words, left, mid)
    part2 = q1(words, mid + 1, right)
    return q1_helper(part1, part2)

def q1_helper(part1, part2):
    i, j , n, m = 0, 0, len(part1), len(part2)
    while True:
        # if characters are not the same, no need to continue
        if part1[i] != part2[j]:
            break
        # if characters are not the same, we check next characters
        i += 1
        j += 1
        if i == n or j == m:
            break # break if we are at the end of the part1 or part2
    return part1[:i] # return the common string (if the first letters are not the same, i = 0 and part[i:] is an empty string

def q2(array, left, right):
    # if there is only 1 element in the array
    if left == right:
        return 0, left, right

    # if there are only 2 elements in the array
    if left + 1 == right:
        return array[right] - array[left], left, right

    # we divide the solution into 2 parts and solve both
    mid = (left + right) // 2
    profit_of_left, start_left, end_left = q2(array, left, mid)
    profit_of_right, start_right, end_right = q2(array, mid+1, right)

    # the correct answer might be in the whole array, so we calculate it
    start_conquer = array[:mid+1].index(min(array[:mid+1]))
    end_conquer = array[mid+1:right+1].index(max(array[mid+1:right+1])) + mid + 1
    profit_of_conquer = array[end_conquer] - array[start_conquer]

    # now we find the maximum profit. is it from left part or right part or the merged part
    if profit_of_conquer > profit_of_left and profit_of_conquer > profit_of_right:
        return profit_of_conquer, start_conquer, end_conquer
    else:
        if profit_of_left > profit_of_right:
            return profit_of_left, start_left, end_left
        else:
            return profit_of_right, start_right, end_right

def q3 (array):
    map = [1]
    for ind in range(1, len(array)):
        if array[ind] > array[ind - 1]:
            map.append(map[ind - 1] + 1)
        else:
            map.append(1)
    return max(map)

def q4a(map, dp_table, i, j):
    # if we already calculated this value, then we return it
    if dp_table[i][j] != 0:
        return dp_table[i][j]
    # if we are at map[0][1] or map[1][0] then there is only one move left
    if (i == 1 and j == 0) or (i == 0 and j == 1):
        dp_table[i][j] = dp_table[0][0] + map[i][j]
    # if we are on the top row, there is nothing to calculate, previous step is the step on the upper side
    elif i == 0:
        dp_table[i][j - 1] = q4a(map, dp_table, i, j - 1)
        dp_table[i][j] = dp_table[i][j - 1] + map[i][j]
    # if we are on the leftmost column,  there is nothing to calculate, previous step is the step on the left side
    elif j == 0:
        dp_table[i - 1][j] = q4a(map, dp_table, i - 1, j)
        dp_table[i][j] = dp_table[i - 1][j] + map[i][j]
    # else, we calculate both steps: coming from left side or coming from upper side. then we take the maximum of it
    else:
        dp_table[i - 1][j] = q4a(map, dp_table, i - 1, j)
        dp_table[i][j - 1] = q4a(map, dp_table, i, j - 1)
        dp_table[i][j] = map[i][j] + max(dp_table[i - 1][j], dp_table[i][j - 1])
    return dp_table[i][j]

def q4b (map):
    score = map[0][0]
    n, m = len(map), len(map[0])
    i = 0
    j = 0
    while 1:
        # if we are not on the borders
        if i + 1 < n and j + 1 < m:
            # find the path with higher point
            if map[i + 1][j] > map[i][j + 1]:
                score += map[i + 1][j]
                i += 1
            else:
                score += map[i][j + 1]
                j += 1
        else:
            # if we are at the bottom right (end of the game)
            if i + 1 == n and j + 1 == m:
                break
            # if we are at the bottom line
            elif i + 1 == n:
                score += map[i][j + 1]
                j += 1
            # if we are at the rightmost column:
            elif j + 1 == m:
                score += map[i + 1][j]
                i += 1
    return score

def print_map(map, n, m):
    for i in range(n):
        for j in range(m):
            print(str(map[i][j]) + "\t", end="")
        print()

if __name__ == '__main__':
    words = ["programmable", "programming", "programmer", "programmatic", "programmability"]
    left, right = 0, len(words) - 1
    print("---------Q1---------\nThe words:", words, "\nThe longest common string is", q1(words, left, right))




    # array = [100, 110, 80, 90, 110, 70, 80, 80, 90]
    array = [10, 11, 10, 9, 8, 7, 9, 11]
    left, right = 0, len(array) - 1
    profit, buy, sell = q2(array, left, right)
    print("\n---------Q2---------\nThe price list:", array)
    print("The solution: buy on Day" + str(buy) + " for " + str(array[buy]) + " liras and sell on Day" + str(sell) + " for " + str(array[sell]) + " liras. The profit is:", profit, "liras.")




    # array = [1, 4, 5, 2, 4, 3, 6, 7, 1, 2, 3, 4, 7]
    array = [1, 2, 3, 4, 1, 2, 3, 5, 2, 3, 4]
    print("\n---------Q3---------\nThe integer array:", array)
    print("Length of the maximal increasing sub-array is", q3(array))





    map = [[25, 30, 25],
           [45, 15, 11],
           [1,  88, 15],
           [9,   4, 23],
           ]
    n, m = len(map), len(map[0])
    dp_table = [ [0] * m for _ in range(n)]
    # initializing the top left element of the dp_table as the top left element of the map.
    # because it doesn't require any calculations
    dp_table[0][0] = map[0][0]
    print("\n---------Q4---------\nThe map:")
    print_map(map, n, m)
    print("a) Max point: ", q4a(map, dp_table, n - 1, m - 1))
    print("b) Max point: ", q4b(map))





    map = [[1,  8,  4],
           [2,  3,  5],
           [17, 9, 15],
           [11, 5, 10],
           ]
    n, m = len(map), len(map[0])
    dp_table = [ [0] * m for _ in range(n)]
    # initializing the top left element of the dp_table as the top left element of the map.
    # because it doesn't require any calculations
    dp_table[0][0] = map[0][0]
    print("\n---------Q4---------\nThe map:")
    print_map(map, n, m)
    print("a) Max point: ", q4a(map, dp_table, n - 1, m - 1))
    print("b) Max point: ", q4b(map))