package src.test;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Random;
import java.util.Scanner;

import src.graph.DynamicGraph;
import src.graph.MyGraph;
import src.graph.Vertex;

/**
 * Tests the class MyGraph
 * <p> TEST CASES: </p> 
 * <p> 1. Add vertex </p> 
 * <p> 2. Add edge </p> 
 * <p> 3. Remove vertex </p> 
 * <p> 4. Remove edge </p> 
 * <p> 5. Load edges from a file </p> 
 * <p> 6. Print the graph </p> 
 * <p> 7. Filter vertices </p> 
 * <p> 8. Export matrix representation </p> 
 */
public class TestMyGraph {
    /** Number of vertices in the graph during random graph generation */
    public static final int NUM_VERTICES = 15;
    /** Number of max edges for a vertex during random graph generation */
    public static final int NUM_MAX_EDGE = 5;

    /** 
     * Tests add/remove methods for edges and
     * vertices on a directed graph.
     */
    public static void test1() {
        // create a directed graph
        MyGraph graph = new MyGraph(true);

        // add some vertex
        for (int i = 0; i < NUM_VERTICES; ++i)
            t_addVertex(graph, i);
        System.out.println(graph);
        
        // add dublicated vertices
        t_addVertex(graph, 0);
        t_addVertex(graph, 5);
        t_addVertex(graph, 3);

        // add some edge with parallel arrays for representing the edges
        int[] source = {0, 15, 4, 9, 5, 1, 4, 12, 3, 7, 8, -1, 9, 7};
        int[] dest =   {1, 6, 3, 12, 0, 1, 4, 32, 2, 14, 3, 1, 5, 13};
        
        for (int i = 0; i < source.length; ++i)
            t_addEdge(graph, source[i], dest[i]);
        System.out.printf("\n%s\n", graph);
        
        // remove some vertices
        int[] remove = {1, 7, 5, 4, 14, 21, 43};
        for (int i = 0; i < remove.length; ++i)
            t_removeVertex(graph, remove[i]);
        // print current view of the graph
        System.out.printf("\n%s\n", graph);

        // remove all edges
        for (int i = 0; i < source.length; ++i)
            t_removeEdge(graph, source[i], dest[i]);
        System.out.printf("\n%s\n", graph);
    }

    /**
     * Tests add/remove methods for edges and vertices 
     * on an undirected graph. The edges are created
     * randomly and the result is printed into the
     * output file.
     */
    public static void test2() {
        // create an undirected graph
        MyGraph graph = new MyGraph(false);
        // add vertices
        for (int i = 0; i < NUM_VERTICES; ++i)
            graph.addVertex(new Vertex(i));

        // create random edges
        addRandomEdges(graph, NUM_VERTICES);

        // print the edges of the graph to the output file
        graph.printGraph("src/test/inputs/o1.txt");

        // remove all the edges by removing the vertices
        for (int i = 0; i < NUM_VERTICES; ++i)
            t_removeVertex(graph, i);
            System.out.printf("\n%s\n", graph);
    }

    /**
     * Tests createGraph method by loading edges from the file.
     * To check the validty of method containsEdge and containsVertex
     * methods are used. Laslty the current status of graph is 
     * exported as matrix representation by exportMatrix method.
     */
    public static void test3() {
        final String filename = "src/test/inputs/s2.txt";
        MyGraph graph = null;
        try {
            // load the edges from file
            graph = (MyGraph) MyGraph.createGraph(
                new Scanner(new File(filename)), true, "List");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            System.exit(1);
        }

        System.out.println("Edges are loaded\n");
        System.out.println(graph);

        // check if the given edges are exist in the graph
        t_containsEdge(graph, 1, 2);
        t_containsEdge(graph, 5, 6);
        t_containsEdge(graph, 3, 11);
        // check if the given vertices are exist in the graph
        t_containsVertex(graph, 13);
        t_containsVertex(graph, 3);
        t_containsVertex(graph, 34);
        t_containsVertex(graph, 5);
        System.out.println(graph);

        // remove some vertices and edges
        // get the matrix representation and print the matrix
        var matrix = graph.exportMatrix();
        MyGraph.displayMatrixRepresentation(matrix);
    }

    /**
     * Tests the filterVertices method of the graph.
     */
    public static void test4 () {
        String COLOR_PROPERTY = "color";
        String CHARGE_PROPERTY = "charge";

        // create a set of vertices
        Vertex[] vertices = new Vertex[NUM_VERTICES];
        for (int i = 0; i < vertices.length; ++i)
            vertices[i] = new Vertex(i);
        // add addtional propeties to the vertices
        vertices[0].addProperty(COLOR_PROPERTY, "red");
        vertices[1].addProperty(COLOR_PROPERTY, "green");
        vertices[2].addProperty(COLOR_PROPERTY, "red");
        vertices[3].addProperty(COLOR_PROPERTY, "blue");
        vertices[5].addProperty(COLOR_PROPERTY, "red");
        vertices[4].addProperty(COLOR_PROPERTY, "yellow");
        vertices[5].addProperty(CHARGE_PROPERTY, "blue");
        vertices[6].addProperty(CHARGE_PROPERTY, "purple");
        vertices[7].addProperty(CHARGE_PROPERTY, "red");
        vertices[8].addProperty(CHARGE_PROPERTY, "red");
        vertices[9].addProperty(CHARGE_PROPERTY, "blue");
        vertices[10].addProperty(CHARGE_PROPERTY, "orange");

        vertices[0].addProperty(CHARGE_PROPERTY, "low");
        vertices[1].addProperty(CHARGE_PROPERTY, "moderate");
        vertices[2].addProperty(CHARGE_PROPERTY, "high");
        vertices[3].addProperty(CHARGE_PROPERTY, "low");
        vertices[5].addProperty(CHARGE_PROPERTY, "low");
        vertices[4].addProperty(CHARGE_PROPERTY, "high");
        vertices[5].addProperty(CHARGE_PROPERTY, "low");
        vertices[6].addProperty(CHARGE_PROPERTY, "moderate");
        vertices[7].addProperty(CHARGE_PROPERTY, "high");
        vertices[8].addProperty(CHARGE_PROPERTY, "low");
        vertices[9].addProperty(CHARGE_PROPERTY, "moderate");
        vertices[10].addProperty(CHARGE_PROPERTY, "low");

        // create a undirected graph
        MyGraph graph = new MyGraph(false);
        // add the vertices to the graph 
        for (int i = 0; i < vertices.length; ++i)
            t_addVertex(graph, vertices[i]);
        // create random edges between the edges
        addRandomEdges(graph, vertices.length);

        // print current view of the graph
        System.out.printf("\n%s\n", graph);

        // filter the graph based on color property is red
        DynamicGraph redGraph = graph.filterVertices(COLOR_PROPERTY, "red");  
        // filter the graph based on charge property is low
        DynamicGraph lowGraph =  graph.filterVertices(CHARGE_PROPERTY, "low");

        // print the filtered graph according to red color
        System.out.printf("\nFiltered graph according to red color\n%s\n", redGraph);
        // print the filtered graph according to low charge
        System.out.printf("\nFiltered graph according to low charge\n%s\n", lowGraph);
    }

    /** 
     * Tests City-Map graph
     */
    public static void test5() {
        MyGraph graph = new MyGraph(false);
        
        graph.addVertex(new Vertex(0, "Ann Arbor"));
        graph.addVertex(new Vertex(4, "Detroit"));
        graph.addVertex(new Vertex(8, "Toledo"));
        graph.addVertex(new Vertex(1, "Chicago"));
        graph.addVertex(new Vertex(5, "Indianapolis"));
        graph.addVertex(new Vertex(9, "Fort Wayne"));
        graph.addVertex(new Vertex(3, "Columbus"));
        graph.addVertex(new Vertex(6, "Pitsburg"));
        graph.addVertex(new Vertex(2, "Clevland"));
        graph.addVertex(new Vertex(7, "Philadelphia"));

        final String filename = "src/test/inputs/s6.txt";
        try {
            graph.loadEdgesFromFile(new Scanner(new File(filename)));
        } catch (FileNotFoundException e1) {
            e1.printStackTrace();
        } catch (IOException e1) {
            e1.printStackTrace();
        }

        // adjacency list representation
        System.out.println(graph);
        // patrix representation
        MyGraph.displayMatrixRepresentation(graph.exportMatrix());
    }


    /**
     * Adds the edge to the graph and prints the operation result
     * @param graph The graph
     * @param source Source vertex ID
     * @param dest Destination vertex ID
     */
    private static void t_addEdge(MyGraph graph, int source, int dest) {
        System.out.printf("Add edge (%-2d, %-2d): ", source, dest);
        if (graph.addEdge(source, dest) != null)
            System.out.println("SUCCESS");
        else
            System.out.println("FAIL");
    }

    /**
     * Adds the edge to the graph and prints the operation result
     * @param graph The graph
     * @param source Source vertex ID
     * @param dest Destination vertex ID
     * @param weight Weight of the edge
     */
    private static void t_addEdge(MyGraph graph, int source, int dest, double weight) {
        System.out.printf("Add edge (%-2d, %-2d): ", source, dest);
        if (graph.addEdge(source, dest, weight) != null)
            System.out.println("SUCCESS");
        else
            System.out.println("FAIL");
    }

    /**
     * Adds the vertex to the graph and prints the operation result
     * @param graph The graph
     * @param vertexID Vertex ID
     */
    private static void t_addVertex(MyGraph graph, int vertexID) {
        System.out.printf("Add vertex %-2d: ", vertexID);
        if (graph.addVertex(new Vertex(vertexID)) != null)
            System.out.println("SUCCESS");
        else
            System.out.println("FAIL");
    }

    /**
     * Adds the vertex to the graph and prints the operation result
     * @param graph The graph
     * @param v The vertex to be inserted 
     */
    private static void t_addVertex(MyGraph graph, Vertex v) {
        System.out.printf("Add vertex %-2d: ", v.getID());
        if (graph.addVertex(v) != null)
            System.out.println("SUCCESS");
        else
            System.out.println("FAIL");
    }

    /**
     * Removes the edge from the graph and prints the operation result
     * @param graph The graph
     * @param source Source vertex ID
     * @param dest Destination vertex ID
     */
    private static void t_removeEdge(MyGraph graph, int source, int dest) {
        System.out.printf("Remove edge (%-2d, %-2d): ", source, dest);
        if (graph.removeEdge(source, dest))
            System.out.println("SUCCESS");
        else
            System.out.println("FAIL");
    }

    /**
     * Removes the vertex from the graph and prints the operation result
     * @param graph The graph
     * @param vertexID Vertex ID
     */
    private static void t_removeVertex(MyGraph graph, int vertexID) {
        System.out.printf("Remove vertex %-2d: ", vertexID);
        if (graph.removeVertex(vertexID) != null)
            System.out.println("SUCCESS");
        else
            System.out.println("FAIL");
    }

    /**
     * Determines whether the graph contains given edge and prints the operation result
     * @param graph The graph
     * @param source Source vertex ID
     * @param dest Destination vertex ID
     */
    private static void t_containsEdge(MyGraph graph, int source, int dest) {
        System.out.printf("containsEdge (%-2d, %-2d): %s\n", 
            source, dest, graph.containsEdge(source, dest));
    }

    /**
     * Determines whether the graph contains given vertex and prints the operation result
     * @param graph The graph
     * @param vertexID Vertex ID
     */
    private static void t_containsVertex(MyGraph graph, int vertexID) {
        System.out.printf("containsVertex (%-2d): %s\n", 
            vertexID, graph.containsVertex(vertexID));
    }

    /**
     * Adds randomly choosed edges.
     * <p>
     * pre: grap vertex IDs are from 0 to numV
     * </p>
     * @param graph The graph to insert the edges
     * @param numV Number of vertices that graph has
     */
    private static void addRandomEdges(MyGraph graph, int numV) {
        Random rand = new Random();
        for (int i = 0; i < numV; ++i) {
            // # of edges of vertex i determined randomly
            int n = rand.nextInt(NUM_MAX_EDGE);
            for (int j = 0; j < n; ++j)
                t_addEdge(graph, i, rand.nextInt(NUM_VERTICES), rand.nextInt(100) + 0.0);   
        }
        // print the graph
        System.out.printf("Random edges are inserted:\n%s\n", graph);
    }
}
