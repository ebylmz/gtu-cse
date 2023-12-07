package src.graph;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import src.entry.EdgeEntry;
import src.entry.SourceEntry;

public class ModifiedDijkstrasAlgorithm {
    /** Boosting property that is used in the modified algorithm */
    public static final String BOOSTING_PROPERTY = "Boosting";

    /**
     * Applies Modified Dijkstra's Algorithm
     * @param graph the Graph to be traversed
     * @param start Start vertex
     * @param pred The predecessors
     * @param dist The shortest distances from start to particular vertex
     */
    public static void dijkstrasAlgorithm(
        MyGraph graph, SourceEntry start, Map<EdgeEntry, SourceEntry> pred, Map<EdgeEntry, Double> dist) {
        
        // the set notFixed to keep the vertices which the minimum distances are not found yet 
        Set<EdgeEntry> notFixed = new HashSet<>(graph.getNumV());
        // initilization process 
        for (var current : graph.getAdjacencyList()) {
            // set each vertex as not fixed 
            notFixed.add(current);
            // initially parent of each vertex is start vertex
            pred.put(current, start);
            // if there is a edge between start and current vertex,
            // then set the distance of current as weight of the edge
            // set the distance to infinity for no edge case
            var dEntry = graph.getEdge(start, current.getSourceVertex().getID());
            if (dEntry == null)
                dist.put(current, Double.POSITIVE_INFINITY);
            else 
                dist.put(current, dEntry.getEdgeWeight());
        }

        // set parent of start to null
        pred.put(start, null);
        // set the distance of start index fixed with value 0.0 
        dist.put(start, 0d); 
        notFixed.remove(start);

        // find minimum distance from the start vertex to other vertices
        while (! notFixed.isEmpty()) {
            SourceEntry current = null;
            double minDist = Double.POSITIVE_INFINITY;
            // find the vertex which smallest distance from the non-fixed vertices
            for (var sEntry : notFixed) {
                double nextDist = dist.get(sEntry);    
                if (nextDist < minDist) {
                    minDist = nextDist;
                    current = (SourceEntry) sEntry;
                }
            }
            // min distance for current is determined, fix its distance  
            notFixed.remove(current);
            System.out.printf("Fix vertex %d with shortest distance %.1f\n", 
                current.getSourceVertex().getID(), dist.get(current));
            for (var neigbor : current.getDestEntries()) {
                System.out.printf("\n\tCheck the neighbor vertex %d\n", neigbor.getVertex().getID());
                // calculate the distance of neighbor via current
                double newDist = dist.get(current) + neigbor.getEdgeWeight();
                // remove the boosting value(s) on the path if 
                // the current is not the head vertex of the path
                if (pred.get(current) != null) {
                    double boostVal = Double.parseDouble(current.getVertex().getProperty(BOOSTING_PROPERTY)); 
                    System.out.printf("\tRemove the boosting value %.1f from the distance %.1f\n", boostVal, newDist);
                    newDist -= boostVal;
                }
                // check if the distance to neighbor via current is shorter than the existing one
                if (newDist < dist.get(neigbor)) {
                    System.out.printf("\tUpdate the distance of vertex %d from %.1f to %.1f\n",
                        neigbor.getDestVertex().getID(), dist.get(neigbor), newDist);
                    // set the predecessor of the neighbor to current and update the distance
                    dist.put(neigbor, newDist);
                    pred.put(neigbor, current);
                }
            }
            System.out.println(); 
        }
    }
}
