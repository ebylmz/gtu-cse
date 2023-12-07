package src.test;

public class Driver {
    public static void main(String[] args) {
        beautify("TestMyGraph", 1);
        TestMyGraph.test1();
        beautify("TestMyGraph", 2);
        TestMyGraph.test2();
        beautify("TestMyGraph", 3);
        TestMyGraph.test3();
        beautify("TestMyGraph", 4);
        TestMyGraph.test4();
        beautify("TestMyGraph", 5);
        TestMyGraph.test5();

        beautify("TestGraphTraversal", 1);
        TestGraphTraversal.test1();
        beautify("TestGraphTraversal", 2);
        TestGraphTraversal.test2();

        beautify("TestModifiedDijkstrasAlgorithm", 1);
        TestModifiedDijkstrasAlgorithm.test1();
        beautify("TestModifiedDijkstrasAlgorithm", 2);
        TestModifiedDijkstrasAlgorithm.test2();
        beautify("TestModifiedDijkstrasAlgorithm", 3);
        TestModifiedDijkstrasAlgorithm.test3();
    }

    private static void beautify(String testCase, int testID) {
        System.out.printf("\n>>>>>>>>>>>>>>>>>>>>> %s %d <<<<<<<<<<<<<<<<<<<<<\n\n", testCase, testID);
    }
}
