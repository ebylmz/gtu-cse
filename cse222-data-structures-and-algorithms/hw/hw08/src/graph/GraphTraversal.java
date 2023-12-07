package src.graph;

import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;

import src.entry.DestEntry;
import src.entry.EdgeEntry;
import src.entry.SourceEntry;

public class GraphTraversal {
    /**
     * Calculates the total distance of the path for accessing 
     * each vertex during the DFS and BFS traversals, and it 
     * returns the difference between the total distances of 
     * two traversal methods.
     * @param graph The graph to be traversed
     * @return Total distance difference of two traveral methods
     */
    public static double totalDistanceDifference(MyGraph graph) {
        System.out.printf("\n(BFS):\n\n");
        double totalDistBFS = breadthFirstSearch(graph);
        System.out.printf("\n(DFS):\n\n");
        double totalDistDFS = depthFirstSearch(graph);
        System.out.printf("\nTotal distance (BFS): %.1f\n", totalDistBFS);
        System.out.printf("Total distance (DFS): %.1f\n", totalDistDFS);
        return totalDistBFS > totalDistDFS ? 
            totalDistBFS - totalDistDFS : totalDistDFS - totalDistBFS;
    }

    /**
     * Perform a breadth-first search of a graph and
     * returns the total distance of the path for 
     * accessing each vertex during the traversal.
     * 
     * During the BFS, if there are more than one 
     * alternative to access a vertex at a specific level, 
     * the shortest alternative should be considered.
     * 
     * @param graph The graph to be traversed
     * @return Total distance during breadth-first search
     */
    public static double breadthFirstSearch(MyGraph graph) {
        // get the adjaceny list to reach the vertices
        var adjacencyList = graph.getAdjacencyList();
        
        // a set to indicate whether a vertex is identified or not
        Set<EdgeEntry> identified = new HashSet<>(graph.getNumV());
        // declare the map dist to record the distance for the specific vertex
        Map<SourceEntry, Double> dist = new HashMap<>(graph.getNumV());

        // put all the vertices to map with proper initial values
        for (var sEntry : adjacencyList)
            dist.put(sEntry, Double.POSITIVE_INFINITY);
        
        // the queue keeps the non-identified adjacents of a vertex during the BFS
        Queue<SourceEntry> queue = new LinkedList<>();
        // mark the start vertex as identified and insert it into the queue
        var start = adjacencyList.getFirst();
        identified.add(start); 
        queue.offer(start);
        dist.put(start, 0d);

        while (!queue.isEmpty()) {
            // take a vertex out of the queue (begin visiting current)
            var current = queue.poll();

            // check all the vertices that adjacent to current
            var it = current.getDestEntries().iterator();            
            while (it.hasNext()) {
                var dEntry = it.next();
                // find the source entry version of the target vertex 
                var neighbor = graph.getSourceEntry(dEntry.getDestVertex());
                
                // discover new vertices 
                if (! identified.contains(neighbor)) {
                    // the neighbor is identified/dicovered
                    identified.add(neighbor);
                    queue.offer(neighbor);
                    System.out.printf("identified v(%d)\n", neighbor.getSourceVertex().getID());
                }
                // keep the the distance minimum for neighbor  
                double newDist = dist.get(current) + dEntry.getEdgeWeight();
                if (newDist < dist.get(neighbor))
                    dist.put(neighbor, newDist);
            }
            // finished visiting current
        }

        // calculate the total distance of the path for accessing each vertex
        double totalDist = 0d;
        for (var sEntry : adjacencyList)
            totalDist += dist.get(sEntry); 

        return totalDist;
    }

    /**
     * Perform a depth-first search of a graph and
     * returns the total distance of the path for 
     * accessing each vertex during the traversal.
     * 
     * During the DFS, the vertices should be considered
     * in distance order. So, from a vertex v, DFS should 
     * continue with a vertex w which has the smallest edge 
     * from v, among all adjacent vertices of v.
     * 
     * @param graph The graph to be traversed
     * @return Total distance during depth-first search
     */
    public static double depthFirstSearch(MyGraph graph) {
        var adjacencyList = graph.getAdjacencyList();
        // a set to keep the visited vertices
        Set<EdgeEntry> visited = new HashSet<>(graph.getNumV());

        double totalDist = 0d;
        for (var sEntry : adjacencyList)
            if (! visited.contains(sEntry))
                totalDist += depthFirstSearch(graph, visited, sEntry, 0d);
            
        return totalDist;
    }

    /**
     * Applies recursive parth of depth-first algorithm
     * @param graph The graph to be traversed
     * @param visited A set to keep the visited vertices
     * @param current The vertex being visited
     * @return The total distance during the depth-first search
     */
    private static double depthFirstSearch(MyGraph graph, Set<EdgeEntry> visited, SourceEntry current, double dist) {
        // mark current vertex to visited
        visited.add(current);

        System.out.printf("visited  v(%d): dist(%.1f)\n", current.getSourceVertex().getID(), dist);

        // create a priority queue to keep the non-identified adjacent vertices in a priority
        PriorityQueue<DestEntry> heap = new PriorityQueue<>(new SortByEdgeWeight());

        // check all the neighbor vertices by iterating over adjacency list
        var it = current.getDestEntries().iterator();
        while (it.hasNext()) {
            var dEntry = it.next();
            // put the non-visited neighbor vertices into the priorty queue
            if (!visited.contains(dEntry)) {
                // System.out.println("put into the heap " + dEntry.getDestVertex().getID());
                heap.offer(dEntry);
            }
        }
        double currentDist = dist;
        while (! heap.isEmpty()) {
            var dEntry = heap.poll();
            if (!visited.contains(dEntry)) {
                var neighbor = graph.getSourceEntry(dEntry.getDestVertex());
                // add the distance of the vertices adjacent to neigbor vertex
                dist += depthFirstSearch(graph, visited, neighbor, currentDist + dEntry.getEdgeWeight()); 
            }
        }
        System.out.printf("finished v(%d): dist(%.1f)\n", current.getSourceVertex().getID(), dist);
        return dist;
    }

    /**
     * Comparator class to compare the edges based on their weight
     */
    private static class SortByEdgeWeight implements Comparator<DestEntry> {
        @Override
        public int compare(DestEntry arg0, DestEntry arg1) {
            if (arg0.getEdgeWeight() < arg1.getEdgeWeight())
                return -1;
            else if (arg0.getEdgeWeight() > arg1.getEdgeWeight())
                return 1;
            else
                return 0;
        }
    }
}
