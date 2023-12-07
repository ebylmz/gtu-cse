def isProper(board, size, row, col, num):
    # check if num is used or not in the same row and column
    for i in range(size):
        if board[row][i] == num or board[i][col] == num:
            return False
    # check if num is used or not in the current 3x3 matrix 
    startRow = row - row % 3
    startCol = col - col % 3

    for i in range(3):
        for j in range(3):
            if board[startRow + i][startCol + j] == num:
                return False
    
    # it's proper to place num to board[row][col] 
    return True

def sudokuSolver(board, size):
    return sudokuSolverHelper(board, size, 0 , 0)

def sudokuSolverHelper(board, size, row, col):
    # make sure not exceed the board boundries
    if col == size:
        # check if the sudoku is solved
        if row == size - 1:
            return True
        row += 1
        col = 0

    # if the current cell already contains a value, then continue with the next cell
    if board[row][col] > 0:
        return sudokuSolverHelper(board, size, row, col + 1)
    for num in range(1, size + 1):
        # check if num can be placed the current cell
        if isProper(board, size, row, col, num):
            # fill the cell with num and 
            board[row][col] = num
            # continue recursion with next cell and make sure our assumption is correct
            if sudokuSolverHelper(board, size, row, col + 1):
                return True
            # the assumption was wrong so try another proper value
            board[row][col] = 0
    return False

def printBoard(board, size):
    for i in range(size):
        for j in range(size):
            print(board[i][j], end = ' ')
        print()

# examples are taken from https://sandiway.arizona.edu/sudoku/examples.html

# design a 9x9 board (0 indicates unassigned cell)
size = 9 
board1 = [[3, 0, 6, 5, 0, 8, 4, 0, 0],
        [5, 2, 0, 0, 0, 0, 0, 0, 0],
        [0, 8, 7, 0, 0, 0, 0, 3, 1],
        [0, 0, 3, 0, 1, 0, 0, 8, 0],
        [9, 0, 0, 8, 6, 3, 0, 0, 5],
        [0, 5, 0, 0, 9, 0, 6, 0, 0],
        [1, 3, 0, 0, 0, 0, 2, 5, 0],
        [0, 0, 0, 0, 0, 0, 0, 7, 4],
        [0, 0, 5, 2, 0, 6, 3, 0, 0]]

board2 = [[0, 2, 0, 6, 0, 8, 0, 0, 0],
        [5, 8, 0, 0, 0, 9, 7, 0, 0],
        [0, 0, 0, 0, 4, 0, 0, 0, 0],
        [3, 7, 0, 0, 0, 0, 5, 0, 0],
        [6, 0, 0, 0, 0, 0, 0, 0, 4],
        [0, 0, 8, 0, 0, 0, 0, 1, 3],
        [0, 0, 0, 0, 2, 0, 0, 0, 0],
        [0, 0, 9, 8, 0, 0, 0, 3, 6],
        [0, 0, 0, 3, 0, 6, 0, 9, 0]]

board3 = [[2, 0, 0, 3, 0, 0, 0, 0, 0],
        [8, 0, 4, 0, 6, 2, 0, 0, 3],
        [0, 1, 3, 8, 0, 0, 2, 0, 0],
        [0, 0, 0, 0, 2, 0, 3, 9, 0],
        [5, 0, 7, 0, 0, 0, 6, 2, 1],
        [0, 3, 2, 0, 0, 6, 0, 0, 0],
        [0, 2, 0, 0, 0, 9, 1, 4, 0],
        [6, 0, 1, 2, 5, 0, 8, 0, 9],
        [0, 0, 0, 0, 0, 1, 0, 0, 2]]

boards = [board1, board2, board3]

for board in boards:
    if sudokuSolver(board, size):
        printBoard(board, size)
    else:
        print("No solution exists")
    print()
    