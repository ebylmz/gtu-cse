package src.test;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

import src.graph.GraphTraversal;
import src.graph.MyGraph;

public class TestGraphTraversal {
    /**
     * Tests totalDistance on a directed graph
     */
    public static void test1() {
        String filename = "src/test/inputs/s3.txt";
        MyGraph graph = null;
        try {
            // load the edges from file
            graph = (MyGraph) MyGraph.createGraph(
                new Scanner(new File(filename)), true, "List");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            System.exit(1);
        }

        double totalDist = GraphTraversal.totalDistanceDifference(graph);
        System.out.printf("Total distance difference: %.1f\n", totalDist);
    }

    /**
     * Tests totalDistance on an undirected graph
     */
    public static void test2() {
        String filename = "src/test/inputs/s4.txt";
        MyGraph graph = null;
        try {
            // load the edges from file
            graph = (MyGraph) MyGraph.createGraph(
                new Scanner(new File(filename)), false, "List");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            System.exit(1);
        }

        double totalDist = GraphTraversal.totalDistanceDifference(graph);
        System.out.printf("Total distance difference: %.1f\n", totalDist);
    }
}
