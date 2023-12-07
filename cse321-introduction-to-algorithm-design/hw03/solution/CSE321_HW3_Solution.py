import random
# Q1)
class Graph:
    def __init__(self):
        self.graph = dict()
        self.vertices = []

    def addVertices(self, vertices):
        self.vertices = vertices
        for vertex in vertices:
            self.graph[vertex] = []  # graph[a] = [b,c] means there are edges from a to b and from a to c

    def addEdge(self, a, b):
        self.graph[a].append(b)

    def printEdges(self):
        print("Printing the edges...")
        for a in self.vertices:
            for b in self.graph[a]:
                print(a, "->", b)


    # question 1) a)
    def recursiveFunction(self, a, sequence, visited):
        visited[a] = True # marking the vertex as visited
        for i in self.graph[a]: # among the neighbors of that vertex (the neighbors that vertex has an outgoing edge to)
            if visited[i] == False: # if we didn't visit that neighbor before
                self.recursiveFunction(self.vertices[i], sequence, visited) # we visit it by using the recursive function
        sequence.append(a)

    def topologicalSort_a(self):
        visited = [False] * len(self.vertices) # marking all vertices as unvisited
        sequence = [] # the output sequence
        for i in range(len(self.vertices)):
            if visited[i] == False: # if we didn't visit that vertex before
                self.recursiveFunction(self.vertices[i], sequence, visited) # calling recursive function to find the next move
        return sequence[::-1]

    # Question 1) b)
    def topologicalSort_b(self):
        in_degrees = [0] * len(self.vertices) # we will keep the number of indegrees of each vertex
        for a in self.graph.keys(): # filling the in_degree list
            for b in self.graph[a]:
                in_degrees[b] += 1

        zero_indegree_vertices = [] # list of the vertices with 0 in degree
        for a in range(len(self.vertices)):
            if in_degrees[a] == 0:
                zero_indegree_vertices.append(a)

        sequence = [] # output sequence

        # we will add a vertex with zero in degree to the sequence. then we will act like we delete it from the graph, so that we update in_degree list and keep adding the vertices with zero in degree to the sequence
        # this loop will add all vertices to the sequence, if it cannot add all of them, it means there is a cycle in the graph which is not considered to happen here, since the input is assumed to be proper
        while zero_indegree_vertices:

            a = zero_indegree_vertices[0] # vertex to work on
            sequence.append(a) # adding the vertex to the output sequence
            del(zero_indegree_vertices[0]) # deleting it from the zero_indegree_vertices list

            # updating in_degree and zero_indegree_vertices lists
            for b in self.graph[a]: # among the neighbors of that vertex (the neighbors that vertex has an outgoing edge to)
                in_degrees[b] -= 1 # decrease the indegree
                if in_degrees[b] == 0: # if in degree becomes 0, after the subtraction, we add it to zero_indegree_vertices list
                    zero_indegree_vertices.append(b)
        return sequence



# Q2
def exp(a, n):
    if n == 1:
        return a
    if(n % 2 == 1): #if n is odd
        return a*exp(a, n-1)
    e = exp(a, n/2)
    return e * e

# Q3
def print_sudoku(sudoku):
    print("-------------------------")
    for i in range(9):
        print("|",end = " ")
        for j in range(3):
            print(sudoku[i][j],end = " ")
        print("|",end = " ")
        for j in range(3,6):
            print(sudoku[i][j],end = " ")
        print("|",end = " ")
        for j in range(6,9):
            print(sudoku[i][j],end = " ")
        print("|")
        if i == 2 or i == 5 or i == 8:
            print("-------------------------")

def solve(puzzle, row, col, value):
    for x in range(9):
        if puzzle[row][x] == value:
            return False
    for x in range(9):
        if puzzle[x][col] == value:
            return False
    # these variables are needed to compute the correctness of 3x3 box
    sRow = row - row % 3
    sCol = col - col % 3
    for i in range(3):
        for j in range(3):
            if puzzle[sRow + i][sCol + j] == value:
                return False
    return True

def Sudoku(puzzle, i, j):
    if (i == 8 and j == 9):
        return True
    if j == 9: # if at the end of a row, go to the next row
        i += 1
        j = 0
    # if the cell already has a value, continue with the next cell's solution
    if puzzle[i][j] > 0:
        return Sudoku(puzzle, i, j + 1)
    # if the cell doesn't have a value, then calculate it
    for value in range(1, 10):
        if solve(puzzle, i, j, value): # find a solution for that cell, by trying each possible value (1,2,....9)
            puzzle[i][j] = value
            if Sudoku(puzzle, i, j + 1):
                return True # return True, if found a solution
        puzzle[i][j] = 0
    return False


if __name__ == '__main__':
    while True:
        val = int(input("Press 1 for Q1, 2 for Q2 and 3 for Q3.\nPress 0 to terminate.\n"))
        if val == 1:
            print("Question 1) a)")
            my_graph = Graph()
            my_graph.addVertices([0, 1, 2, 3])
            my_graph.addEdge(3, 1)
            my_graph.addEdge(3, 0)
            my_graph.addEdge(2, 3)
            my_graph.addEdge(0, 1)
            my_graph.addEdge(2, 1)
            my_graph.printEdges()
            print("(Q1 - a) Topological sorting:", my_graph.topologicalSort_a())
            print("(Q1 - b) Topological sorting:", my_graph.topologicalSort_b(),"\n")
        elif val == 2:
            a = 3
            n = 5
            print("3^5 =", exp(a,n))
        elif val == 3:
            puzzle = [[0 for i in range(9)] for j in range (9)] # 2D array of the empty puzzle,
            for k in range(1, 10): # randomly adding some numbers to the puzzle
                i = random.randint(0, 8)
                j = random.randint(0, 8)
                puzzle[i][j] = k
            print("Puzzle:")
            print_sudoku(puzzle)
            print("Solution:")
            if (Sudoku(puzzle, 0, 0)):
                print_sudoku(puzzle)
        else:
            break