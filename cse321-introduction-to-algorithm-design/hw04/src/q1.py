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