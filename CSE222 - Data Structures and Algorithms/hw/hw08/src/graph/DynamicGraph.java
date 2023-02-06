package src.graph;

import java.util.NoSuchElementException;

public interface DynamicGraph extends Graph {
    /**
     * Generate a new vertex by given parameters and
     * sets the vertex ID as number of vertices in the graph.
     * @param label Label of the vertex
     * @param weight Weight of the vertex
     * @return New created Vertex
     */
    Vertex newVertex(String label, double weight);

    /**
     * Adds the given vertex to the graph. If the vertex
     * already exist (same ID) then updates it and returns
     * the previous vertex.
     * @param v The vertex to be inserted
     * @return New added vertex or the previos vertex
     */
    Vertex addVertex(Vertex v);
    
    /**
     * Remove the vertex from the graph with respect to the given vertex ID.
     * @param vertexID The vertex ID
     * @return if there is no vertex with given ID, then returns null.
     *         Otherwise returns a reference for the removed vertex.
     */
    Vertex removeVertex(int vertexID);
    
    /**
     * Remove the vertices that have the given label from the graph.
     * @param label The label of the vertex
     * @return if there is no vertex with given label, then returns null.
     *         Otherwise returns a reference for the removed vertex.
     */
    Vertex removeVertex(String label) throws NoSuchElementException;

    /**
     * Filter the vertices by the given user-defined property and returns a subgraph of the graph.
     * @param key The Key 
     * @param filter The value for the given key
     * @return A subgraph of the graph, which contains the key value pairs.
     */
    DynamicGraph filterVertices(String key, String filter);

    /**
     * Generate the adjacency matrix representation of the graph and returns the matrix.
     * @return The adjacency matrix
     */
    double[][] exportMatrix();

    /**
     * Prints the graph in adjacency list format to the file with given path  
     * @pathname Name of the output file
     * @return True if the print process is finished successfully, otherwise false.
     */
    boolean printGraph(String pathname);
}

