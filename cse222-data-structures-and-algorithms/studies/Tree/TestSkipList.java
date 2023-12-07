import java.util.Random;

public class TestSkipList {
    /** Maximum number (exclusive) than can be inside the set */
    private static final int RAND_MAX = 100;
    /** Set size of an data set */
    private static final int SET_SIZE = 15;

    public static void main(String[] args) {
        test1();
        // test2();
    }

    /**
     * Tests SkipList class with specific data set
     */
    public static void test1() {
        Integer[] dataset = {35, 5, 20, 15, 45, 30, 50, 10, 40, 25};
        SkipList<Integer> skipList = new SkipList<>();
        // add the dataset items into the skip-list
        for (var e : dataset)
            skipList.add(e);
        // display the skip-list
        System.out.println(skipList);

        // search items
        for (var e : dataset)
            System.out.printf("Find %-10s: %b\n", e, skipList.find(e) != null);
        Random rand = new Random();
        for (int i = 0; i < 10; ++i) {
            int e = rand.nextInt(RAND_MAX);
            System.out.printf("Find %-10s: %b\n", e, skipList.find(e) != null);
        }


        // remove all the items
        for (var e : dataset) {
            System.out.println("\nRemove: " + e);
            skipList.remove(e);
            System.out.println(skipList);
        }
        // display the skip-list
    }

    /**
     * Tests SkipList class with randomly generated data set
     */
    public static void test2() {
        Random rand = new Random();
        SkipList<Integer> skipList = new SkipList<>();
        // add randomly generated items into the skip-list
        for (int i = 0; i < SET_SIZE; ++i)  
            skipList.add(rand.nextInt(RAND_MAX));
        // display the skip-list
        System.out.println(skipList);
    }

}
