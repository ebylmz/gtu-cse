# Emirkan Burak Yılmaz 1901042659

###################################### QUESTION 1 ######################################

# compares two string and return the number of match from start to end
def compare(s1, s2):
    i = 0 
    n = min(len(s1), len(s2))
    while i < n and s1[i] == s2[i]:
        i = i + 1
    return i

# finds common prefix of the array string
def commonPrefix(arr, lo, hi):
    if lo == hi:
        return arr[lo]
    if lo < hi:
        # divide the array into halves and find the common prefix seperatly
        mid = lo + (hi - lo) // 2
        s1 = commonPrefix(arr, lo, mid)
        s2 = commonPrefix(arr, mid + 1, hi)
        # lastly compare the results of the halves to find the common prefix of whole array
        i = compare(s1, s2)
        return s1[:i] if i > 0 else ""

###################################### QUESTION 2 ######################################

# solution with divide-and-conquer approach
def maxProfitDAC(prices, lo, hi):
    if lo >= hi:
        return lo, lo, lo, lo

    # search most profitable days on halves
    mid = lo + (hi - lo) // 2
    lbuy, lsell, lmin, lmax = maxProfitDAC(prices, lo, mid)
    rbuy, rsell, rmin, rmax = maxProfitDAC(prices, mid + 1, hi)

    # min and max prices that encountered so far
    # this information will be used by the caller of this recursive call
    cmin = lmin if prices[lmin] < prices[rmin] else rmin
    cmax = lmax if prices[lmax] > prices[rmax] else rmax

    # max profit could came from left, right or mixture of them
    # by max price of right (sell) and min price of left (buy)
    lprofit = prices[lsell] - prices[lbuy]
    rprofit = prices[rsell] - prices[rbuy]
    mProfit = prices[rmax] - prices[lmin]
    maxProfit = max(mProfit, lprofit, rprofit)

    buy, sell = 0, 0
    if maxProfit == mProfit:
        buy, sell = cmin, cmax 
    elif maxProfit == lprofit:
        buy, sell = lbuy, lsell
    else:
        buy, sell = rbuy, rsell

    return buy, sell, cmin, cmax

# solution with non-divide-and-conquer approach
def maxProfitLinear(prices):
    mini = 0    # index of minimum value 
    buy = 0     
    sell = 1
    maxProfit = prices[sell] - prices[buy]

    for i in range(1, len(prices)):
        if prices[i] - prices[mini] > maxProfit:
            # more profitable days are found 
            maxProfit = prices[i] - prices[mini]
            sell = i
            buy = mini
        elif prices[i] < prices[mini]:
            # keep the minimum value to check if more profitable days are exist
            mini = i
    return buy, sell

###################################### QUESTION 3 ######################################

def increasingConsecutiveSubarray(arr):
    n = len(arr)
    length = [1] * n
    maxLenght = 0

    for i in range(1, n):
        # check if arr[i] could be added on the previos subarray
        if arr[i] > arr[i - 1]:
            length[i] = length[i - 1] + 1
            if length[i] > maxLenght:
                maxLenght = length[i]
    return maxLenght

###################################### QUESTION 4 ######################################

# solution with brute force approach
def maxScoreBF(grid, row, col):
    currLoc = convertPathFormat(row, col)
    rscore, rpath = 0, [] # right score & path
    dscore, dpath = 0, [] # down score & path
    
    # right movement
    if col + 1 < len(grid[row]):
        rscore, rpath = maxScoreBF(grid, row, col + 1)
    # down movement 
    if row + 1 < len(grid):
        dscore, dpath = maxScoreBF(grid, row + 1, col)

    # select the path that contains more points 
    if rscore > dscore:
        return rscore + grid[row][col], [currLoc] + rpath
    else:
        return dscore + grid[row][col], [currLoc] + dpath

# solution with dynamic programming approach
def maxScoreDP(grid):
    rows = len(grid)       # number of rows
    cols = len(grid[0])    # number of columns
    
    dp = [[0 for i in range(cols)] for j in range(rows)]

    dp[0][0] = grid[0][0]

    # fill for the first row
    for i in range(1, rows):
        dp[i][0] = dp[i - 1][0] + grid[i][0]
    # fill for the first column
    for i in range(1, cols):
        dp[0][i] = dp[0][i - 1] + grid[0][i]

    # fill for the intermediate cells
    for i in range(1, rows):
        for j in range(1, cols):
            # grid[i][j] can be reached from grid[i - 1][j] or grid[i][j - 1]
            dp[i][j] = grid[i][j] + max(dp[i - 1][j], dp[i][j - 1])
    
    # derive the path by following maximum points
    i, j = rows - 1, cols - 1
    path = [convertPathFormat(i, j)]
    while i > 0 and j > 0:
        if dp[i][j - 1] > dp[i - 1][j]:
            j = j - 1   # reverse move right
        else:
            i = i - 1   # reverse move down
        path.append(convertPathFormat(i, j))

    while i > 0:
        i = i - 1
        path.append(convertPathFormat(i, j))

    while j > 0:
        j = j - 1
        path.append(convertPathFormat(i, j))
    
    path.reverse()

    return dp[rows - 1][cols - 1], path

# solution with greedy approach
def maxScoreGreedy(grid, r, c):
    rsize = len(grid)
    csize = len(grid[r]) 

    points = 0
    path = []
    # select the cell that contains more points
    if r == rsize - 1 and c == csize - 1:
        pass
    elif r == rsize - 1:
        points, path = maxScoreGreedy(grid, r, c + 1)
    elif c == csize - 1:
        points, path = maxScoreGreedy(grid, r + 1, c)
    elif grid[r][c + 1] > grid[r + 1][c]:
        points, path = maxScoreGreedy(grid, r, c + 1)
    else:
        points, path = maxScoreGreedy(grid, r + 1, c)

    return points + grid[r][c], [convertPathFormat(r, c)] + path

def printGrid(grid):
    for row in grid:
        for item in row:
            print("%2d" % (item), end=" ")
        print()      

def convertPathFormat(r, c):
    return "A" + str(r + 1) + "B" + str(c + 1) 

###################################### DRIVER ######################################

if __name__ == '__main__':
    while True:
        val = int(input("Press 1 for Q1, 2 for Q2, 3 for Q3, 4 for Q4. Press 0 to terminate.\n> "))
        print()
        if val == 1:
            inputs = [
                ["programmable", "programming", "programmer", "programmatic", "programmability"],
                ["compute", "compatible", "computer", "compare", "compactness"]]

            for i in range(len(inputs)):
                arr = inputs[i]
                print("arr: ", arr)
                print("result: ", commonPrefix(arr, 0, len(arr) - 1), "\n")

        elif val == 2:
            inputs = [
                [10, 11, 10, 9, 8, 7, 9, 11],
                [10, 5, 100, 5, 110, 70, 80, 120],
                [10, 5, 100, 50, 110, 70, 80, 120, 200, 500, 100],
                [100, 110, 80, 90, 110, 70, 80, 80, 90]]

            for i in range(len(inputs)):
                prices = inputs[i]
                buyDAC, sellDAC, _min, _max = maxProfitDAC(prices, 0, len(prices) - 1)
                buyLinear, sellLinear = maxProfitLinear(prices)
                print("prices: ", prices)
                print("result (DAC)    : Buy on day", buyDAC, "for", prices[buyDAC], "tl",
                        "and sell on day", sellDAC, "for", prices[sellDAC], "tl.")
                print("result (Linear) : Buy on day", buyLinear, "for", prices[buyLinear], "tl",
                        "and sell on day", sellLinear, "for", prices[sellLinear], "tl.\n")

        elif val == 3:
            inputs = [
                [10, 22, 9, 33, 21, 50, 41, 60],
                [1, 2, 3, 4, 1, 2, 3, 5, 2, 3, 4],
                [1, 4, 5, 2, 4, 3, 6, 7, 1, 2, 3, 4, 7],
                [1, 2, 2, 3, 1, 4, 1, 2, 1, 4, 3, 5, 2, 4, 3, 5, 4]]

            for i in range(len(inputs)):
                arr = inputs[i]
                print("arr: ", arr)
                print("result: len:", increasingConsecutiveSubarray(arr), "\n")

        elif val == 4:
            inputs = [
                [
                    [25, 30, 25],
                    [45, 15, 11],
                    [ 1, 88, 15],
                    [ 9,  4, 23]],

                [   [25, 30, 25],
                    [15, 15, 11],
                    [ 1, 88, 15],
                    [ 9,  4, 23]],
                [ 
                    [ 5, 2,  4, 3],
                    [ 1, 1, 20, 10],
                    [ 1, 1, 20, 10],
                    [ 1, 1, 20, 10]]]

            for i in range(len(inputs)):
                grid = inputs[i]
                print("Game Map: ")
                printGrid(grid)
                print()
                scoreBF, pathBF = maxScoreBF(grid, 0, 0)
                scoreDP, pathDP = maxScoreDP(grid)
                scoreGreedy, pathGreedy = maxScoreGreedy(grid, 0, 0)
                print (f"score(Brute-Force): {scoreBF}\npath: {pathBF}\n")
                print (f"score(Dynamic-Programming): {scoreDP}\npath: {pathDP}\n")
                print (f"score(Greddy): {scoreGreedy}\npath: {pathGreedy}\n")

        else:
            break
        print()