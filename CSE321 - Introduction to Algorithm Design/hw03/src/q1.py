from collections import defaultdict
 
class Graph:
    def __init__(self, numV):
        self.graph = defaultdict(list)      # Dictionary containing adjacency List
        self.numV = numV                    # Number of vertices
 

    def addEdge(self, u, v):
        self.graph[u].append(v)
 

    def topologicalSortDFSHelper(self, v, visited, stack):
        # Mark the current node as visited
        visited[v] = True 
 
        # Recur for all the vertices adjacent to this vertex
        for i in self.graph[v]:
            if visited[i] == False:
                self.topologicalSortDFSHelper(i, visited, stack)
 
        # Push current vertex to result stack
        stack.insert(0, v)


    def topologicalSortDFS(self):
        # Mark all the vertices as not visited
        visited = [False] * self.numV
        stack = []      # Stack to keep the topological order
 
        # Get the topological order with DFS
        for i in range(self.numV):
            if visited[i] == False:
                self.topologicalSortDFSHelper(i, visited, stack)
 
        return stack


    def topologicalSortNonDFS(self):
        # Initiliaze an array to keep the inDegree's of vertices
        inDegree = [0]*(self.numV)

        # Traverse adjacency lists to fill indegrees of vertices
        for i in range(self.numV): 
            for j in self.graph[i]:
                inDegree[j] += 1

        # Create an queue and enqueue all vertices with indegree 0
        queue = []
        for i in range(self.numV):
            if inDegree[i] == 0:
                queue.append(i)

        topOrder = [] # Keeps the topological order

        while queue:
            u = queue.pop(0)
            topOrder.append(u)

            # Iterate through all the neighbour vertices 
            # and decrease their indegree by 1
            for i in self.graph[u]:
                inDegree[i] -= 1
                if (inDegree[i] == 0):
                    queue.append(i)

        return topOrder if len(topOrder) == self.numV else None

def printOrder(data, topOrder):
    for i in topOrder:
        print (data[i], end = '')
        if i + 1 < len(topOrder):
            print(" --> ", end = '')
    print()


courses = ["CSE 102", "CSE 241", "CSE 222", "CSE 211", "CSE 321", "CSE 422"]
g = Graph(6)
g.addEdge(0, 1)
g.addEdge(1, 2)
g.addEdge(3, 4)
g.addEdge(2, 4)
g.addEdge(4, 5)

topOrderDFS = g.topologicalSortDFS()
topOrderNonDFS = g.topologicalSortNonDFS()

print ("Topological Sort DFS :")
printOrder(courses, topOrderDFS)
print()
print ("Topological Sort Non DFS:")
printOrder(courses, topOrderNonDFS)