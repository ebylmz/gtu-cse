package src;

public class Test {
    public static void main(String[] args) {
        startTest("QuadTree TEST1");
        TestQuadTree.test1();

        startTest("BinaryHeap TEST1");
        TestBinaryHeap.test1();
        startTest("BinaryHeap TEST2");
        TestBinaryHeap.test2();

        startTest("BinarySearchTree TEST1");
        TestBinarySearchTree.test1();
        startTest("BinarySearchTree TEST2");
        TestBinarySearchTree.test2();
    }

    private static void startTest(String s) {
        System.out.printf("\n\n================ %s ================\n\n", s);
    }
}
