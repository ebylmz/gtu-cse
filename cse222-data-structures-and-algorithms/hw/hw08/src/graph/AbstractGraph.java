package src.graph;

import java.io.IOException;
import java.util.Scanner;

public abstract class AbstractGraph implements Graph {
    /** The number of vertices */
    protected int numV;

    /** Flag to indicate whether this is a directed graph */
    private boolean directed;

    /**
     * Construct a directed/undirected graph with specified number of vertices
     * @param numV The number of vertices
     * @param directed Flag to indicate whether this is a directed graph
     */
    public AbstractGraph(int numV, boolean directed) {
        this.numV = numV;
        this.directed = directed;
    }

    /**
     * Returns the number of vertices in this graph
     * @return The number of vertices
     */
    public int getNumV() {
        return numV;
    }

    /** 
     * Return whether this is a directed graph.
     * @return True if this is a directed graph
    */
    public boolean isDirected() {
        return directed;
    }

    /**
     * Creates a graph by reading the edges from the data in an input file.
     * @param scan Scanner containing the edge data
     * @param isDirected Is the new graph or undirected
     * @param type Type of the graph (only type list available)
     * @return The list graph for type list, matrix graph for type matrix.
     *         However if there is an error during the file the creation
     *         process terminated and returns null.
     */
    public static Graph createGraph(Scanner scan, boolean isDirected, String type) {
        if (type.equalsIgnoreCase("list")) {
            try {
                // MyGraph is an implementation of the DynamicGraph interface,
                // which uses adjacency list representation in the graph data structure.
                MyGraph g = new MyGraph(isDirected);
                g.loadEdgesFromFile(scan);
                return g;
            } catch (IOException e) {
                e.printStackTrace();
                return null;
            }
        }
        else if (type.equalsIgnoreCase("matrix")) {
            System.out.println("Matrix graph implementation is not implemented yet");
            return null;
        }
        else
            return null; 
    }
}
