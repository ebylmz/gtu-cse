package src.graph;

import java.util.Iterator;
import java.util.LinkedList;

import src.entry.DestEntry;
import src.entry.SourceEntry;

/** 
 * Interface to specify a Graph ADT. A graph is a set
 * of vertices and a set of edges. Vertices are
 * represented by integers from 0 to n - 1. Edges
 * are ordered pairs of vertices. Each implementation
 * of the Graph interface should provide a constructor
 * that specifies the number of vertices and whether
 * or not the graph is directed.
 * @author Koffman, Wolfgang, ebylmz
 */
public interface Graph {
    /**
     * Returns the number of vertices.
     * @return The number of vertices
     */
    int getNumV();

    /** 
     * Determines whether this is a directed graph.
     * @return true if this is a directed graph
     */
    boolean isDirected();

    /**
     * Adds a new edge or the modifies existing one between the given two
     * vertices in the graph.
     * @param source Source vertex ID
     * @param dest Destination vertex ID
     * @return The new/modified edge if the vertexes are exist, otherwise returns null
     */
    Edge addEdge(int source, int dest);

    /**
     * Adds a new edge or the modifies existing one between the given two
     * vertices in the graph.
     * @param source Source vertex ID
     * @param dest Destination vertex ID
     * @param weight Weight of the edge
     * @return The new/modified edge if the vertexes are exist, otherwise returns null
     */
    Edge addEdge(int source, int dest, double weight);

    /**
     * <p>
     * Removes the edge between the given two vertices. 
     * </p>
     * Post: If the graph is undirected and there is a edge from source to dest, then 
     *       both edges source to dest and dest to source are removed.
     * @param source Source vertex ID
     * @param dest Destination vertex ID
     * @return if the given vertex(s) is not found or there is no edge
     *         from source to dest vertices then returns false, otherwise returns true.
     */
    boolean removeEdge(int source, int dest);

    /** 
     * Determines whether the edge exists.
     * @param source The source vertex
     * @param dest The destination vertex
     * @return true if there is an edge from source to dest
     */
    boolean containsEdge(int source, int dest);

    /**
     * Determines whether the graph such a vertex with given ID
     * @param vertexID Vertex ID
     * @return True if the vertex is exist, otherwise false
     */
    boolean containsVertex(int vertexID);

    /** 
     * Get the edge between two vertices.
     * @param source The source vertex
     * @param dest The destination vertex
     * @return The Edge between these two vertices
     *         or null if there is no edge.
     */
    Edge getEdge(int source, int dest);

    /** 
     * Get the edge between two vertices.
     * @param sEntry The source vertex
     * @param dest The destination vertex
     * @return The Edge between these two vertices
     *         or null if there is no edge.
     */
    DestEntry getEdge(SourceEntry sEntry, int dest);

    /**
     * Returns the given vertex with it's edges inside a SourceEntry object
     * @param v The vertex that being search
     * @return Non-null SourceEntry object if the given vertex exist in the graph,
     *         otherwise returns null.
     */
    SourceEntry getSourceEntry(Vertex v);

    /**
     * Returns all the vertices with their edges. 
     * @return All the vertices with their edges.
     */
    LinkedList<SourceEntry> getAdjacencyList(); 

    /**
     * Returns an iterator to the edges connected to a given vertex.
     * @param source The source vertex
     * @return An Iterator<DestEntry> to the vertices connected to source. 
     *         However if the source vertex isn't found, then returns null.
     */
    Iterator<DestEntry> edgeIterator(int source);
}
