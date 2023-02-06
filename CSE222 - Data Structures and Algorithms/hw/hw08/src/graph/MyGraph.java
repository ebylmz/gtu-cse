package src.graph;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Scanner;
import java.util.StringTokenizer;

import src.entry.DestEntry;
import src.entry.SourceEntry;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * An implementation of the DynamicGraph interface,
 * which uses adjacency list representation to handle 
 * the edges between vertices in the graph data structure.
 */
public class MyGraph extends AbstractGraph implements DynamicGraph {
    /** Adjaceny list for edges */
    private LinkedList<SourceEntry> adjacencyList;

    /**
     * Construct a graph which the vertices and edges set's are empty initially.
     * @param directed Flag to determine whether this is a digraph or graph 
     */
    public MyGraph(boolean directed) {
        super(0, directed); 
        adjacencyList = new LinkedList<SourceEntry>();
    }

    @Override
    public boolean containsEdge(int source, int dest) {
        return getEdge(source, dest) != null;
    }

    @Override 
    public boolean containsVertex(int vertexID) {
        return findSourceEntry(vertexID) != null;
    }

    @Override
    public Edge getEdge(int source, int dest) {
        // get the source entry by searching source vertex
        SourceEntry sEntry = findSourceEntry(source);
        if (sEntry != null)
            for (var dEntry : sEntry.getDestEntries())
                if (dEntry.getDestVertex().getID() == dest)
                    return new Edge(source, source, dEntry.getEdgeWeight());
        return null; 
    }

    @Override
    public DestEntry getEdge(SourceEntry sEntry, int dest) {
        for (var dEntry : sEntry.getDestEntries())
            if (dEntry.getDestVertex().getID() == dest)
                return dEntry;
        return null;
    }

    @Override
    public SourceEntry getSourceEntry(Vertex v) {
        for (var sEntry : adjacencyList)
            if (sEntry.getSourceVertex().equals(v))
                return sEntry;
        return null;
    }

    @Override
    public LinkedList<SourceEntry> getAdjacencyList() {
        return adjacencyList;
    }

    /**
     * Searches and returns the given vertex with it's edges as SourceEntry object
     * @param id Vertex ID
     * @return If the vertex is found, then returns it with it's 
     * edges as SourceEntry object. Otherwise returns null
     */
    private SourceEntry findSourceEntry(int id) {
        for (var sEntry : adjacencyList)
            if (sEntry.getSourceVertex().getID() == id)
                return sEntry;
        return null;
    }

    @Override
    public Iterator<DestEntry> edgeIterator(int source) {
        SourceEntry sEntry = findSourceEntry(source);
        return sEntry != null ? sEntry.getDestEntries().iterator() : null;
    }

    @Override
    public Vertex newVertex(String label, double weight) {
        //! create a new vertex which ID is number of vertices in the graph
        return new Vertex(numV, label, weight);
    }

    @Override
    public Vertex addVertex(Vertex v) {
        var sEntry = findSourceEntry(v.getID());
        // check whether the given vertex v is already exist by looking it's ID
        if (sEntry == null) {
            // add the vertex to the adjaceny list
            sEntry = new SourceEntry(v, new LinkedList<DestEntry>());
            adjacencyList.add(sEntry);
            // increase the number of vertices
            ++numV; 
        }
        else {
            // update the existing vertex
            var vertex = sEntry.getSourceVertex();
            vertex.setLabel(v.getLabel());
            vertex.setWeight(v.getWeight());
            vertex.setProperties(v.getProperties());
        }
        return sEntry.getSourceVertex();
    }

    @Override
    public Edge addEdge(int source, int dest, double weight) {
        // don't permit self edges
        if (source != dest) {
            var e1 = findSourceEntry(source);   
            var e2 = findSourceEntry(dest);
            // make sure the vertices given with IDs are contained in graph
            if (e1 != null && e2 != null) {
                var newDestEntry = new DestEntry(e2.getSourceVertex(), weight);
                // make sure the edge doesn't exist yet what if there is an already such edge?
                var dEntry = findInList(e1.getDestEntries(), dest); 
                if (dEntry == null) {
                    // add the edge to the adjaceny list
                    e1.getDestEntries().add(newDestEntry);
                    // don't forget to add the reverse edge for undirected graph
                    if (!isDirected()) 
                        e2.getDestEntries().add(new DestEntry(e1.getSourceVertex(), weight));
                } 
                else // update the already existing edge
                    dEntry.setEdgeWeight(weight);                
                return new Edge(source, dest, weight); 
            }
        }
        return null;
    }
    
    /**
     * Searches the vertex with given ID and returns it.
     * @param list The list of DestEntry objects
     * @param vertexID Vertex ID
     * @return If the vertex is found than returns the vertex inside in a DestEntry object.
     *         If not, returns null.
     */
    private DestEntry findInList(LinkedList<DestEntry> list, int vertexID) {
        for (var dEntry : list)
            if (dEntry.getDestVertex().getID() == vertexID)
                return dEntry;
        return null;
    }

    @Override
    public Edge addEdge(int source, int dest) {
        return addEdge(source, dest, Double.POSITIVE_INFINITY); 
    }

    @Override
    public boolean removeEdge(int source, int dest) {
        var e1 = findSourceEntry(source);   
        var e2 = findSourceEntry(dest);
        // make sure the vertices given with IDs are contained in graph
        if (e1 == null || e2 == null)
            return false;

        // determine the return value to indicate successfull remove execution
        var r = removeEdge(e1, e2.getSourceVertex());
        // don't forget to remove the reverse edge for undirected graph
        if (r != null && !isDirected())
            removeEdge(e2, e1.getSourceVertex());
        return r != null; //! better return type may be more suitable 
    }

    /**
     * Removes the given edge. 
     * <p>
     * Pre: sEntry and target are non-null objects. 
     * </p>
     * @param sEntry Source entry 
     * @param target Target vertex to remove 
     * @return Reference for removed edges DestEntry objects 
     *         or null if the edge does not exist.
     */
    private DestEntry removeEdge(SourceEntry sEntry, Vertex target) {
        // iterate over the part of adjacency list related to source vertex
        var it = sEntry.getDestEntries().listIterator();
        while (it.hasNext()) {
            var dEntry = it.next();
            if (target.equals(dEntry.getDestVertex())) {
                it.remove();
                return dEntry;
            }
        }
        return null;
    }

    @Override
    public Vertex removeVertex(int vertexID) {
        var target = findSourceEntry(vertexID);
        // make sure the vertices given with IDs are contained in graph
        if (target == null)
            return null;
        --numV;   
        return removeVertex(target);
    }

    /**
     * Removes the vertex from the graph with respect to the given vertex ID.
     * @param target The target vertex and it's edges inside a SourceEntry class 
     * @return if there is no vertex with given ID, then returns null.
     *         Otherwise returns a reference for the removed vertex.
     */
    private Vertex removeVertex(SourceEntry target) {
        // assign the return value
        Vertex targetVertex = target.getSourceVertex();
        // remove the vertex and all the related edges
        var sIter = adjacencyList.iterator(); 
        while (sIter.hasNext()) {
            var sEntry = sIter.next();
            if (sEntry == target) {
                // remove all the edges
                sEntry.getDestEntries().clear();
                // remove the vertex itself
                sIter.remove();
            }
            else {
                // remove all the edges that contain the target vertex 
                var dIter = sEntry.getDestEntries().iterator();
                while (dIter.hasNext()) {
                    var dEntry = dIter.next();
                    if (dEntry.getDestVertex().equals(targetVertex))
                        dIter.remove(); 
                }   
            }
        }
        return targetVertex;
    }

    @Override
    public Vertex removeVertex(String label) {
        // get the ID of the vertex and remove it
        return removeVertex(findVertex(label));
    }

    @Override
    public DynamicGraph filterVertices(String key, String filter) {
        MyGraph subgraph = new MyGraph(isDirected());
        // check the vertex property key and compare 
        // the pair-value with the filter value   
        // if their value is same as filter value then,
        // add that vertex to the subgraph 
        for (var sEntry : adjacencyList) {
            var vertex = sEntry.getSourceVertex();
            String value = vertex.getProperties().get(key);
            // apply filter to the target vertex
            if (value != null && value.equals(filter))
                subgraph.addVertex(vertex);
        }
        // after filtering vertices, add the edges of the new subgraph
        for (var sEntry : adjacencyList) 
            for (var dEntry : sEntry.getDestEntries())
                subgraph.addEdge(
                    sEntry.getSourceVertex().getID(), 
                    dEntry.getDestVertex().getID(), 
                    dEntry.getEdgeWeight());
        return subgraph;
    }

    /**
     * Maps each vertex ID from to numV - 1.
     * <p>
     * post: Maps each vetex ID to a new vertex ID.
     *       Each vertex ID is between 0 inclusive and numV exclusive.
     *       There will be no change in current graph representation. 
     * </p>
     * @return Map of mapped IDs, the key is current 
     *         ID and value is the mapped ID.
     */
    public Map<Integer, Integer> mapIDs() {
        // map vertex IDs from 0 to numV - 1 
        HashMap<Integer, Integer> mappedIDs = new HashMap<>(); 
        int i = 0;
        for (var sEntry : adjacencyList)
            mappedIDs.put(sEntry.getSourceVertex().getID(), i++); 
        return mappedIDs;
    }

    @Override
    public double[][] exportMatrix() {
        var mappedIDs = mapIDs();
        // create the matrix and initialize the cell values
        var matrix = new double[numV][numV];
        for (int i = 0; i < numV; ++i)
            for (int j = 0; j < numV; ++j)
                matrix[i][j] = Double.POSITIVE_INFINITY;

        for (var sEntry : adjacencyList) {
            // get the mapped ID of source vertex
            int source = mappedIDs.get(sEntry.getSourceVertex().getID());
            for (var dEntry : sEntry.getDestEntries()) {
                // get the mapped ID of destination vertex
                int dest = mappedIDs.get(dEntry.getDestVertex().getID());
                // fill the matrix cell with the weight of the edge
                matrix[source][dest] = dEntry.getEdgeWeight();
            }
        }
        // return the matrix representation of the graph        
        return matrix;
    }

    /**
     * Load the edges of a graph from the data in an input file.
     * The file should contain a series of lines, each line
     * with two or three data values. The first is the source,
     * the second is the destination, and the optional third
     * is the weight.
     * @param scan Scanner containing the edge data
     * @throws IOException if an I/O error occurs
     */
    public void loadEdgesFromFile(Scanner scan) throws IOException {
        while(scan.hasNext()) {
            StringTokenizer st = new StringTokenizer(scan.nextLine());
            // read the vertex IDs for source and destination of edge
            int source = Integer.parseInt(st.nextToken()); 
            int dest = Integer.parseInt(st.nextToken());
            
            var e1 = findSourceEntry(source);
            var e2 = findSourceEntry(dest);
            // make sure the graph has source and dest vertices
            if (e1 == null) addVertex(new Vertex(source));
            if (e2 == null) addVertex(new Vertex(dest));
            // read the optional weight field if it's supported and then add the edge
            if (st.hasMoreTokens())
                addEdge(source, dest, Double.parseDouble(st.nextToken()));  
            else
                addEdge(source, dest);
        }
    }

    /**
     * Prints the adjacency matrix equvalent of the graph
     * @param matrix The adjacency matrix
     */
    public static void displayMatrixRepresentation(double[][] matrix) {
            System.out.printf("    ");
        for (int i = 0; i < matrix.length; ++i)
            System.out.printf("%6d ", i);
        System.out.println("\n");
            
        for (int i = 0; i < matrix.length; ++i) {
            System.out.printf("%-2d  ", i);
            for (int j = 0; j < matrix.length; ++j)
                if (matrix[i][j] == Double.POSITIVE_INFINITY)
                    System.out.printf("%6s ", "x");
                else
                    System.out.printf("%6.1f ", matrix[i][j]);
            System.out.println("\n");
        }
    }

    @Override
    public boolean printGraph(String pathname) {
        try (var fw = new FileWriter(pathname);) {
            // prints all the edges
            for (var sEntry : adjacencyList) {
                int sourceID = sEntry.getSourceVertex().getID();
                for (var dEntry : sEntry.getDestEntries())
                    fw.append(String.format("%d %d %.1f\n", 
                        sourceID, dEntry.getDestVertex().getID(), dEntry.getEdgeWeight()));
            }
            return true;
        } catch (IOException e) {
            return false;
        }
    }

    /**
     * Searches the target vertex with given label
     * and returns it inside a SourceEntry object. 
     * @param label The label of vertex
     * @return If there is such a vertex, then returns it
     *         inside a SourceEntry of it, otherwise returns null
     */
    private SourceEntry findVertex(String label) {
        var sIter = adjacencyList.iterator();
        while(sIter.hasNext()) {
            var sEntry = sIter.next();
            if (sEntry.getSourceVertex().getLabel().equals(label))
                return sEntry;
        }
        return null;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (var sEntry : adjacencyList) {
            // sb.append(String.format("[%d]: ", sEntry.getSourceVertex().getID()));
            sb.append(String.format("%s: ", sEntry.getSourceVertex()));
            for (var dEntry : sEntry.getDestEntries()) {
                if (dEntry.getEdgeWeight() == Double.POSITIVE_INFINITY)
                    sb.append(String.format("--> [%s] ", 
                        dEntry.getDestVertex()));
                else
                    sb.append(String.format("--> [%s | %3.1f] ", 
                        dEntry.getDestVertex(), dEntry.getEdgeWeight()));
            }
            sb.append("\n\n");
        }
        return sb.toString();
    }
}