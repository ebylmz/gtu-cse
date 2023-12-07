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