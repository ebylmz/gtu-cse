package src.test;

import java.util.Random;

import src.skiplist.CustomSkipList;

public class TestCustomSkipList {
    /** Maximum number (exclusive) than can be inside the set */
    private static final int RAND_MAX = 100;
    /** Set size of an data set */
    private static final int SET_SIZE = 32;

    /**
     * Tests CustomSkipList class with specific data set
     */
    public static void test1() {
        Integer[] dataset = {35, 5, 20, 55, 15, 85, 90, 65, 45, 30, 95, 50, 10, 40, 25};
        // Integer[] dataset = {5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70};
        CustomSkipList<Integer> skipList = new CustomSkipList<>();
        // add the dataset items into the skip-list
        for (var e : dataset)
            skipList.add(e);
        // display the skip-list
        System.out.println();
        System.out.println(skipList);

        // search items
        System.out.println();
        Integer[] searchset = {12, 35, 20, 5, 17, 54, 15, 85, 65, 45, 30, 95, 10, 40, 25, 95, 71, 11, 0, 68, 69, 26};
        for (var e : searchset)
            System.out.printf("Find %-10s: %b\n", e, skipList.find(e) != null);
        System.out.println();
        System.out.println(skipList);

        // remove all the items
        for (var e : dataset) {
            System.out.printf("\nRemove: %s\n\n",e);
            skipList.remove(e);
            // display the skip-list
            System.out.println(skipList);
        }
    }

    /**
     * Tests CustomSkipList class with randomly generated data set
     */
    public static void test2() {
        CustomSkipList<Integer> skipList = new CustomSkipList<>();
        Integer[] set = createDataSet(SET_SIZE);
        // add randomly generated items into the skip-list
        for (var e : set) {
            System.out.println("\nAdd " + e + "\n");
            skipList.add(e);
            // display the skip-list
            System.out.println(skipList);
        }
        // display the skip-list
        System.out.println();
        System.out.println(skipList);
        // remove all the items        
        for (var e : set) {
            System.out.printf("\nRemove: %s\n\n",e);
            skipList.remove(e);
            // display the skip-list
            System.out.println(skipList);
        }
        // display the skip-list
        System.out.println(skipList);
    }

    /**
     * Creates an data set which is filled with random integer values
     * @param size Number of item inside the set
     * @return The data set
     */
    private static Integer[] createDataSet(int size) {
        Integer[] dataset = new Integer[size];
        Random rand = new Random();
        for (int i = 0; i < size; ++i)
            dataset[i] = rand.nextInt(RAND_MAX);
        return dataset;
    }
}
