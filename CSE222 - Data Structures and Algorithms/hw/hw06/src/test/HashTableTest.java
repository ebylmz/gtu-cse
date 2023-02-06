package src.test;

import java.util.Random;

import src.hashing.HashTableChaningTree;
import src.hashing.HashTableCoalesced;
import src.hashing.KWHashMap;

public class HashTableTest {
    /** Number of data set */
    public static final int NUM_SETS = 100;
    /** Max value that random number generator can generate */
    private static int MAX_VAL = 1000;
    /** Small data set size */
    public static final int SIZE_SMALL = 100;
    /** Medium data set size */
    public static final int SIZE_MEDIUM = 1000; 
    /** Large data set size */
    public static final int SIZE_LARGE = 10000; 
    /** Default value for table entries */
    public static final String DEFAULT_VALUE = "JAVA";

    public static void test_validation(KWHashMap<Integer, String> table) {
        // int[] keys = {3, 12, 13, 25, 23, 51, 27, 89, 132, 5, 43, 77};
        int[] keys = {3, 12, 13, 25, 23, 51};

        // put the keys to the table
        for (var key : keys) {
            System.out.printf("Put %d\n\n", key);
            table.put(key, "AA");
            System.out.println(table);
        }

        int[] searhKeys = {3, 12, 13, 25, 23, 51, 134, 124, 34, 53};
        // get key-value
        for (var key : searhKeys) {
            System.out.printf("Get key %-5d: ", key);
            System.out.println(table.get(key) != null ? "SUCCESFULL" : "FAIL");
        }

        // remove some of the keys (existing & non-existing)
        int[] removeKeys = {3, 1, 12, 13, 25, 23, 51, 12, 324, -12};
        for (var key: removeKeys) {
            System.out.printf("Remove %d\n\n", key);
            table.remove(key);
            System.out.println(table);
        }
    }
    
    /**
     * Compares runtime performance of HashTableChaningTree, HashTableCoalesced
     */
    public static void test_runtime() {
        // generate the data set for three different set size small, medium, large
        int[][][] dataSet = new int[3][][];
        dataSet[0] = generateRandomDataSet(NUM_SETS, SIZE_SMALL);
        dataSet[1] = generateRandomDataSet(NUM_SETS, SIZE_MEDIUM);
        dataSet[2] = generateRandomDataSet(NUM_SETS, SIZE_LARGE);
        // results are keept in array as
        long[][][] results = new long[2][3][];
        // results[0]: result of HashTableChaningTree  
        // results[1]: result of HashTableCoalesced 
        // measure total execution time of public methods put, 
        // get, remove for small, medium and large set sizes
        for (int i = 0; i < dataSet.length; ++i) {
            results[0][i] = measureHashTable(dataSet[i], new HashTableChaningTree<>());
            results[1][i] = measureHashTable(dataSet[i], new HashTableCoalesced<>());
        }
        // print the results
        String sset = "Small(" + SIZE_SMALL + ")";
        String mset = "Medium(" + SIZE_MEDIUM + ")";
        String lset = "Large(" + SIZE_LARGE + ")";

        for (int i = 0; i < results.length; ++i) {
            System.out.println((i == 0) ? 
                "------------------------ HashTableChaningTree --------------------------------" :
                "------------------------ HashTableCoalesced ----------------------------------");
            System.out.printf("\n%-20s %-20s %-20s %-20s\n\n", "Method", sset, mset, lset);
            System.out.printf("%-20s %-20d %-20d %-20d\n", 
                "Put", results[i][0][0], results[i][1][0], results[i][2][0]);
            System.out.printf("%-20s %-20d %-20d %-20d\n", 
                "Get", results[i][0][1], results[i][1][1], results[i][2][1]);
            System.out.printf("%-20s %-20d %-20d %-20d\n", 
                "Remove", results[i][0][2], results[i][1][2], results[i][2][2]);
            System.out.println("----------------------------------------------------------------------------");
        }        
    }

   /**
     * Measures the total execution time for public methods put, get and remove
     * @param dataSet The data set
     * @return Total execution time for 3 public method
     * runtimes[0] = execution time for put
     * runtimes[1] = execution time for get
     * runtimes[2] = execution time for remove
     */
    private static long[] measureHashTable(int[][] dataSet, KWHashMap<Integer, String> table) {
        // test three public method
        long[] runTimes = new long[3];
        runTimes[0] = test_put(dataSet, table);
        runTimes[1] = test_get(dataSet, table);
        runTimes[2] = test_remove(dataSet, table);
        return runTimes;
    }

    /**
     * Creates a randomly generated data sets
     * @param numSets The number of set
     * @param setSize Size of the each data set
     * @return Set of randomly generated values
     */
    private static int[][] generateRandomDataSet(int numSets, int setSize) {
        int[][] data = new int[numSets][setSize];
        Random r = new Random();
        for (int i = 0; i < numSets; ++i)
            for (int j = 0; j < setSize; ++j)
                data[i][j] = r.nextInt(MAX_VAL);
        return data;
    }

    /**
     * Measures the execution time of KWHashMap interface method put
     * @param dataSet The data set
     * @param table The hash table
     * @return execution time
     */
    private static long test_put(int[][] dataSet, KWHashMap<Integer, String> table) {
        long startTime = System.nanoTime();
        for (var set : dataSet)
            for (var item : set)
                table.put(item, DEFAULT_VALUE);
        return (System.nanoTime() - startTime) / (1000l * NUM_SETS);
    }

    /**
     * Measures the execution time of KWHashMap interface method remove
     * @param dataSet The data set
     * @param table The hash table
     * @return execution time
     */
    private static long test_remove(int[][] dataSet, KWHashMap<Integer, String> table) {
        long startTime = System.nanoTime();
        for (var set : dataSet)
            for (var item : set)
                table.remove(item);
        return (System.nanoTime() - startTime) / (1000l * NUM_SETS);
    }

    /**
     * Measures the execution time of KWHashMap interface method get
     * @param dataSet The data set
     * @param table The hash table
     * @return execution time
     */
    private static long test_get(int[][] dataSet, KWHashMap<Integer, String> table) {
        long startTime = System.nanoTime();
        for (var set : dataSet)
            for (var item : set)
                table.get(item);
        return (System.nanoTime() - startTime) / (1000l * NUM_SETS);
    }
}