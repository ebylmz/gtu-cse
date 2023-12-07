package src.test;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.Scanner;

import src.entry.EdgeEntry;
import src.entry.SourceEntry;
import src.graph.ModifiedDijkstrasAlgorithm;
import src.graph.MyGraph;

public class TestModifiedDijkstrasAlgorithm {
    /**
     * Tests the Modified Dijkstra algoritm on a directed graph
     */
    public static void test1() {
        String filename = "src/test/inputs/s3.txt";
        var graph = loadBoostedGraph(filename, true, 1);

        var start = graph.getAdjacencyList().getFirst();
        Map<EdgeEntry, SourceEntry> pred = new HashMap<>(); 
        Map<EdgeEntry, Double> dist = new HashMap<>(); 
        ModifiedDijkstrasAlgorithm.dijkstrasAlgorithm(graph, start, pred, dist);
        printResult(graph, start, pred, dist);
    }

    /**
     * Tests the Modified Dijkstra algoritm on an undirected graph
     */
    public static void test2() {
        String filename = "src/test/inputs/s6.txt";
        var graph = loadBoostedGraph(filename, false, 10);

        var start = graph.getAdjacencyList().getFirst();
        Map<EdgeEntry, SourceEntry> pred = new HashMap<>(); 
        Map<EdgeEntry, Double> dist = new HashMap<>(); 
        ModifiedDijkstrasAlgorithm.dijkstrasAlgorithm(graph, start, pred, dist);
        printResult(graph, start, pred, dist);
    }

    /**
     * Tests the Modified Dijkstra algoritm on an undirected graph with random boosting values
     */
    public static void test3() {
        String filename = "src/test/inputs/s7.txt";
        var graph = loadBoostedGraphRandom(filename, false, 100);

        var start = graph.getAdjacencyList().getFirst();
        Map<EdgeEntry, SourceEntry> pred = new HashMap<>(); 
        Map<EdgeEntry, Double> dist = new HashMap<>(); 
        ModifiedDijkstrasAlgorithm.dijkstrasAlgorithm(graph, start, pred, dist);
        printResult(graph, start, pred, dist);
    }

    /**
     * Prints the result of the Dijkstra's algorithm
     * @param graph The graph
     * @param start Start vertex
     * @param pred Predecessors map
     * @param dist Shortest distance map
     */
    private static void printResult(MyGraph graph, SourceEntry start, Map<EdgeEntry, SourceEntry> pred, Map<EdgeEntry, Double> dist) {
        System.out.printf("Results:\n"); 
        System.out.printf("------------------------------\n"); 
        System.out.printf("ID\tpred\tdist\n\n"); 
        for (var sEntry : graph.getAdjacencyList()) {
            // print all the distances and parents
            if (sEntry != start)
                System.out.printf("%d\t%d\t%.1f\n", 
                    sEntry.getVertex().getID(), pred.get(sEntry).getVertex().getID(), dist.get(sEntry));
        }
    }

    /**
     * Loads the graph with boosting values 
     * @param filename Input file thath contains the graph
     * @param isDirected Incates directed or undirected graph
     * @param boostIncrement Boosting increment value
     * @return New graph
     */
    private static MyGraph loadBoostedGraph(String filename, boolean isDirected, int boostIncrement) {
        MyGraph graph = null;
        try {
            // load the edges from file
            graph = (MyGraph) MyGraph.createGraph(
                new Scanner(new File(filename)), isDirected, "List");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            System.exit(1);
        }        

        int i = 0;
        for (var sEntry : graph.getAdjacencyList()) {
            ++i;
            sEntry.getSourceVertex().addProperty(
                ModifiedDijkstrasAlgorithm.BOOSTING_PROPERTY, String.valueOf(i * boostIncrement));
        }
            // sEntry.getSourceVertex().addProperty("Boosting",  String.valueOf(rand.nextInt(MAX_RAND)));
            // sEntry.getSourceVertex().addProperty("Boosting", String.valueOf(sEntry.getSourceVertex().getWeight()));
        return graph;
    }

    /**
     * Loads the graph with random boosting values 
     * @param filename Input file thath contains the graph
     * @param isDirected Incates directed or undirected graph
     * @param maxBoostVal Max boosting value for edges
     * @return New graph
     */
    private static MyGraph loadBoostedGraphRandom(String filename, boolean isDirected, int maxBoostVal) {
        MyGraph graph = null;
        try {
            // load the edges from file
            graph = (MyGraph) MyGraph.createGraph(
                new Scanner(new File(filename)), isDirected, "List");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            System.exit(1);
        }        

        Random rand = new Random();
        for (var sEntry : graph.getAdjacencyList()) 
            sEntry.getSourceVertex().addProperty(
                ModifiedDijkstrasAlgorithm.BOOSTING_PROPERTY, String.valueOf(rand.nextInt(maxBoostVal)));
            // sEntry.getSourceVertex().addProperty("Boosting",  String.valueOf(rand.nextInt(MAX_RAND)));
            // sEntry.getSourceVertex().addProperty("Boosting", String.valueOf(sEntry.getSourceVertex().getWeight()));
        return graph;
    }
}
